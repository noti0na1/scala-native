package scala.scalanative
package linker

import java.nio.file.Files

import org.junit.Assert._
import org.junit.Test

import scala.scalanative.build.LiveExecution
import scala.scalanative.live.Manifest

/** Reachability in host links (phantoms of open types, made-up calls on
 *  callable types, stubs for callable code that does not link) and the
 *  manifest's encoding.
 */
class LiveExecutionSpec extends LinkerSpec {

  private val source =
    """
    trait Plugin { def run(x: Int): Int }
    abstract class Base { def greet(): String = "base"; def describe(): String = greet() }
    final class Impl extends Base { override def greet(): String = "impl" }
    abstract class Closed { def f(): Int }
    final class ClosedImpl extends Closed { def f(): Int = 1 }

    object Test {
      def call(p: Plugin): Int = p.run(1)
      def main(args: Array[String]): Unit = {
        val b: Base = new Impl
        println(b.describe())
        val c: Closed = new ClosedImpl
        println(c.f())
        if (args.length > 5) println(call(null))
      }
    }
    """

  private def host(open: String*) =
    (c: build.NativeConfig) =>
      c.withLiveExecution(LiveExecution.Host(open = open))

  private def info(name: String)(implicit r: ReachabilityAnalysis.Result) =
    r.infos(nir.Global.Top(name)).asInstanceOf[ScopeInfo]

  private def sig(name: String, tys: nir.Type*) =
    nir.Sig.Method(name, tys)

  @Test def noPhantomsWithoutOpenTypes(): Unit =
    link("Test", source) { (_, result) =>
      assertTrue(result.phantoms.isEmpty)
      implicit val r: ReachabilityAnalysis.Result = result
      val greet = sig("greet", nir.Rt.String)
      assertEquals(1, info("Base").targets(greet).size)
    }

  @Test def openClassGetsPhantomTarget(): Unit =
    link("Test", source, host("Base", "Plugin")) { (_, result) =>
      implicit val r: ReachabilityAnalysis.Result = result
      assertEquals(
        Set("Base$$Phantom", "Plugin$$Phantom"),
        result.phantoms.map(_.id).toSet
      )
      val greet = sig("greet", nir.Rt.String)
      val targets = info("Base").targets(greet)
      assertEquals(2, targets.size)
      assertTrue(targets.exists(result.isPhantom))
      // The phantom is never in infos and none of its members is reached
      assertFalse(result.infos.keys.exists(result.isPhantom))
      // What an extension subclass inherits is kept, as an entry
      val describe =
        nir.Global.Top("Base").member(sig("describe", nir.Rt.String))
      assertTrue(result.entries.contains(describe))
    }

  @Test def openTraitWithoutImplementorHasOnlyThePhantom(): Unit =
    link("Test", source, host("Plugin")) { (_, result) =>
      implicit val r: ReachabilityAnalysis.Result = result
      val run = sig("run", nir.Type.Int, nir.Type.Int)
      val targets = info("Plugin").targets(run)
      assertEquals(1, targets.size)
      assertTrue(result.isPhantom(targets.head))
    }

  @Test def closedTypesKeepClosedWorldTargets(): Unit =
    link("Test", source, host("Base", "Plugin")) { (_, result) =>
      implicit val r: ReachabilityAnalysis.Result = result
      val f = sig("f", nir.Type.Int)
      val targets = info("Closed").targets(f)
      assertEquals(1, targets.size)
      assertFalse(targets.exists(result.isPhantom))
    }

  private val callableSource =
    """
    trait Api { def a(): Int; def b(): Int }
    final class One extends Api { def a(): Int = 1; def b(): Int = 10 }
    final class Two extends Api { def a(): Int = 2; def b(): Int = 20 }
    abstract class Base extends Api { def a(): Int = 3 }
    object Clock {
      // javalib has no java.time: callable code that needs it still links
      def now(): Long = java.time.Instant.now().toEpochMilli()
    }
    object Test {
      def main(args: Array[String]): Unit = {
        val x: Api = if (args.length > 0) new One else new Two
        println(x.a())
      }
    }
    """

  private def callableHost(callable: String*) =
    (c: build.NativeConfig) =>
      c.withLiveExecution(
        LiveExecution.Host(
          open = Nil,
          callable = callable,
          stubUnlinkable = true
        )
      )

  @Test def callableTypesDispatchEveryMethodTheyDeclare(): Unit =
    link("Test", callableSource, callableHost("Api", "One", "Two")) {
      (_, result) =>
        implicit val r: ReachabilityAnalysis.Result = result
        // The host calls only a(); b() gets a dispatch table entry too
        val b = sig("b", nir.Type.Int)
        assertTrue(info("Api").calls.contains(b))
        assertEquals(2, info("Api").targets(b).size)
    }

  @Test def madeUpCallsKeepOnlyBoundaryImplementations(): Unit =
    // One is not callable: its b() is never called, so it is not kept
    link("Test", callableSource, callableHost("Api", "Two")) { (_, result) =>
      implicit val r: ReachabilityAnalysis.Result = result
      val b = sig("b", nir.Type.Int)
      assertTrue(info("Api").calls.contains(b))
      val bOf = (cls: String) => nir.Global.Top(cls).member(b)
      assertTrue(result.infos.contains(bOf("Two")))
      assertFalse(result.infos.contains(bOf("One")))
    }

  @Test def madeUpCallsLeaveNoTraitDefaultInPlaceOfAnOverride(): Unit = {
    val source =
      """
      trait Api { def a(): Int; def m(): Int = 0 }
      final class Own extends Api { def a(): Int = 1; override def m(): Int = 1 }
      object Test {
        def main(args: Array[String]): Unit = println((new Own: Api).a())
      }
      """
    link("Test", source, callableHost("Api")) { (_, result) =>
      implicit val r: ReachabilityAnalysis.Result = result
      val m = sig("m", nir.Type.Int)
      assertTrue(info("Api").calls.contains(m))
      // Own is not a boundary class: its override is not kept, and its entry
      // must not fall back to Api's default
      assertEquals(None, info("Own").asInstanceOf[Class].resolve(m))
    }
  }

  @Test def callableCodeThatDoesNotLinkBecomesStubs(): Unit =
    link("Test", callableSource, callableHost("Clock$")) { (_, result) =>
      val now = nir.Global.Top("Clock$").member(sig("now", nir.Type.Long))
      val defn = result.defns.collectFirst {
        case d: nir.Defn.Define if d.name == now => d
      }
      assertTrue("Clock.now is kept", defn.isDefined)
      assertTrue(
        "Clock.now throws",
        defn.get.insts.exists {
          case nir.Inst.Let(_, nir.Op.Call(_, nir.Val.Global(f, _), _), _) =>
            f == codegen.Lower.throwNoSuchMethod
          case _ => false
        }
      )
    }

  private implicit val noPosition: nir.SourcePosition =
    nir.SourcePosition.NoPosition
  private implicit val topLevel: nir.ScopeId = nir.ScopeId.TopLevel

  /** A static method of `owner` that loads `module` as a module and returns
   *  `result`.
   */
  private def moduleLoader(
      owner: nir.Global.Top,
      id: String,
      module: nir.Global.Top
  ): nir.Defn.Define = {
    implicit val fresh: nir.Fresh = nir.Fresh()
    val loaded = nir.Val.Local(fresh(), nir.Type.Ref(module))
    nir.Defn.Define(
      nir.Attrs.None,
      owner.member(
        nir.Sig.Method(id, Seq(nir.Rt.Object), nir.Sig.Scope.PublicStatic)
      ),
      nir.Type.Function(Nil, nir.Rt.Object),
      Seq(
        nir.Inst.Label(fresh(), Nil),
        nir.Inst.Let(loaded.id, nir.Op.Module(module), nir.Next.None),
        nir.Inst.Ret(loaded)
      )
    )
  }

  private def throws(defn: nir.Defn.Define): Boolean =
    defn.insts.exists {
      case nir.Inst.Let(_, nir.Op.Call(_, nir.Val.Global(f, _), _), _) =>
        f == codegen.Lower.throwNoSuchMethod
      case _ => false
    }

  @Test def moduleInstructionsOnClassesBecomeStubs(): Unit = {
    // nscplugin gives the companion class of an object nested in a class
    // lazy-val offset accessors that load the object as a module, which it is
    // not: MUnit 1.2.1's munit.diff.Delta$TYPE.OFFSET$_m_0 loads the class
    // Delta$TYPE$. Nothing calls them, but a host link makes them callable,
    // and code generation lowers `module` to a call of the module's
    // accessor, which only modules have. Scala 3.10 writes no such accessors,
    // so the test adds them to the NIR.
    val source =
      """
      class Outer {
        object Kind { lazy val default: String = "a" }
        def kind: String = Kind.default
      }
      object Test {
        def main(args: Array[String]): Unit = println(new Outer().kind)
      }
      """
    val owner = nir.Global.Top("OuterKindOffsets")
    val kind = nir.Global.Top("Outer$Kind$")
    val offsets = (0 to 2).map(i => moduleLoader(owner, s"OFFSET$$_m_$i", kind))
    val written =
      nir.Defn.Class(nir.Attrs.None, owner, Some(nir.Rt.Object.name), Nil) +:
        offsets
    val host = callableHost("Outer*")
    link("Test", source, host, extraNir = written) { (_, result) =>
      def isModule(name: nir.Global.Top) = result.infos.get(name) match {
        case Some(cls: Class) => cls.isModule
        case _                => false
      }
      assertTrue(result.infos.contains(kind))
      assertFalse(isModule(kind))
      result.defns.foreach {
        case d: nir.Defn.Define =>
          d.insts.foreach {
            case nir.Inst.Let(_, nir.Op.Module(n), _) =>
              assertTrue(s"${d.name.show} loads ${n.show}", isModule(n))
            case _ => ()
          }
        case _ => ()
      }
      offsets.foreach { offset =>
        val defn = result.defns.collectFirst {
          case d: nir.Defn.Define if d.name == offset.name => d
        }
        assertTrue(s"${offset.name.show} is kept", defn.isDefined)
        assertTrue(s"${offset.name.show} throws", throws(defn.get))
      }
    }
  }

  @Test def moduleInstructionsOnClassesDoNotLinkInClosedPrograms(): Unit = {
    // A closed program that reaches such code fails to link, naming the
    // missing accessor, instead of failing in code generation
    val source = "class Plain"
    val main = nir.Global.Top("Main")
    val load = moduleLoader(main, "load", nir.Global.Top("Plain"))
    val written = Seq(
      nir.Defn.Class(nir.Attrs.None, main, Some(nir.Rt.Object.name), Nil), {
        implicit val fresh: nir.Fresh = nir.Fresh()
        val args = nir.Val.Local(fresh(), nir.Type.Array(nir.Rt.String))
        nir.Defn.Define(
          nir.Attrs.None,
          main.member(nir.Rt.ScalaMainSig),
          nir.Type.Function(Seq(args.ty), nir.Type.Unit),
          Seq(
            nir.Inst.Label(fresh(), Seq(args)),
            nir.Inst.Let(
              nir.Op
                .Call(load.ty, nir.Val.Global(load.name, nir.Type.Ptr), Nil),
              nir.Next.None
            ),
            nir.Inst.Ret(nir.Val.Unit)
          )
        )
      },
      load
    )
    doesNotLink("Main", source, extraNir = written) { (_, failure) =>
      val plainLoad = nir.Global.Top("Plain").member(nir.Sig.Generated("load"))
      assertEquals(Seq(plainLoad), failure.unreachable.map(_.name))
      assertEquals(load.name, failure.unreachable.head.backtrace.head.name)
    }
  }

  @Test def leftOutClassesWithStaticInitializersDoNotFailTheLink(): Unit = {
    // A class whose parent is missing is left out of a host link that stubs
    // code that does not link; its static initializer is no entry point then
    // (MUnit's munit.Framework without sbt.testing's definitions)
    implicit val fresh: nir.Fresh = nir.Fresh()
    val orphan = nir.Global.Top("Orphan")
    val written = Seq(
      nir.Defn.Class(
        nir.Attrs.None,
        orphan,
        Some(nir.Global.Top("MissingParent")),
        Nil
      ),
      nir.Defn.Define(
        nir.Attrs.None,
        orphan.member(nir.Sig.Clinit),
        nir.Type.Function(Nil, nir.Type.Unit),
        Seq(nir.Inst.Label(fresh(), Nil), nir.Inst.Ret(nir.Val.Unit))
      )
    )
    val source = "object Test { def main(args: Array[String]): Unit = () }"
    link("Test", source, callableHost("Orphan"), extraNir = written) {
      (config, result) =>
        assertFalse(result.infos.get(orphan).exists(_.isInstanceOf[Class]))
        val list = config.workDir.resolve("stubbed-methods.txt")
        assertTrue(
          new String(Files.readAllBytes(list), "UTF-8")
            .contains("left out\t" + orphan.show)
        )
    }
  }

  private def emptyManifest(classes: Seq[Manifest.ClassEntry]) = Manifest(
    header = Manifest.Header(
      targetTriple = "arm64-apple-darwin",
      sizeOfPtr = 8,
      nirCompat = nir.Versions.compat,
      nirRevision = nir.Versions.revision,
      runtimeAbi = Manifest.RuntimeAbiVersion,
      multithreading = true,
      gc = "immix",
      mode = "debug",
      usesLockWords = true,
      yieldPointTraps = false,
      cxxExceptions = false,
      virtualThreads = false,
      hostBinary = "/tmp/host"
    ),
    classes = classes,
    traits = Nil,
    exports = Nil,
    dynsigs = Nil
  )

  private def classEntry(name: String) = Manifest.ClassEntry(
    name = name,
    parent = "java.lang.Object",
    traits = Nil,
    flags = Manifest.Flags.Open,
    size = 16,
    fields = Nil,
    vtable = Nil,
    direct = Nil
  )

  @Test def extensionsDoNotSeeHostMethodsThatNeedMissingClasses(): Unit = {
    // A host left such a method out (Thread.join of a java.time.Duration):
    // a subclass of the host class must not inherit it
    implicit val fresh: nir.Fresh = nir.Fresh()
    val base = nir.Global.Top("Base")
    val missing = nir.Type.Ref(nir.Global.Top("java.time.Duration"))
    def method(id: String, tys: Seq[nir.Type]) = {
      val self = nir.Val.Local(fresh(), nir.Type.Ref(base))
      nir.Defn.Define(
        nir.Attrs.None,
        base.member(nir.Sig.Method(id, tys :+ nir.Type.Unit)),
        nir.Type.Function(nir.Type.Ref(base) +: tys, nir.Type.Unit),
        Seq(nir.Inst.Label(fresh(), Seq(self)), nir.Inst.Ret(nir.Val.Unit))
      )
    }
    val kept = method("kept", Seq(nir.Type.Int))
    val needsMissing = method("join", Seq(missing))
    val declared = nir.Defn.Declare(
      nir.Attrs.None,
      base.member(nir.Sig.Method("abstractJoin", Seq(missing, nir.Type.Unit))),
      nir.Type.Function(Seq(nir.Type.Ref(base), missing), nir.Type.Unit)
    )
    val underlying = ClassLoader.fromMemory(
      Seq(
        nir.Defn.Class(nir.Attrs.None, base, Some(nir.Rt.Object.name), Nil),
        kept,
        needsMissing,
        declared
      )
    )
    val view = new ClassLoader.HostView(
      underlying,
      emptyManifest(Seq(classEntry("Base")))
    )
    val names = view.load(base).toSeq.flatten.map(_.name).toSet
    assertTrue(names.contains(base))
    assertTrue(names.contains(kept.name))
    assertFalse(names.contains(needsMissing.name))
    assertFalse(names.contains(declared.name))
  }

  @Test def extensionRootsAreTheClassesTheirNirDefines(): Unit = {
    // os-lib's os/package$_home.nir holds only a static accessor of a class
    // that does not exist
    implicit val fresh: nir.Fresh = nir.Fresh()
    val real = nir.Global.Top("Real")
    val orphan = nir.Global.Top("Orphan")
    val accessor = orphan.member(
      nir.Sig
        .Method("OFFSET$_m_0", Seq(nir.Type.Long), nir.Sig.Scope.PublicStatic)
    )
    val dir = Files.createTempDirectory("ext-roots")
    def write(top: nir.Global.Top, defns: Seq[nir.Defn]) = {
      val channel = Files.newByteChannel(
        dir.resolve(top.id + ".nir"),
        java.nio.file.StandardOpenOption.CREATE,
        java.nio.file.StandardOpenOption.WRITE
      )
      try nir.serialization.serializeBinary(defns, channel)
      finally channel.close()
    }
    write(
      real,
      Seq(nir.Defn.Class(nir.Attrs.None, real, Some(nir.Rt.Object.name), Nil))
    )
    write(
      orphan,
      Seq(
        nir.Defn.Define(
          nir.Attrs.None,
          accessor,
          nir.Type.Function(Nil, nir.Type.Long),
          Seq(nir.Inst.Label(fresh(), Nil), nir.Inst.Ret(nir.Val.Long(0L)))
        )
      )
    )
    val manifestFile = Files.createTempFile("host", ".snm")
    emptyManifest(Nil).write(manifestFile)
    val ext =
      LiveExecution.Extension(hostManifest = manifestFile, roots = Seq(dir))
    val config = build.Config.empty.withLogger(build.Logger.nullLogger)
    val entries = scala.scalanative.util.Scope { implicit scope =>
      live.LiveLink.extensionEntries(config, ext)
    }
    assertTrue(entries.contains(real))
    assertFalse(entries.contains(orphan))
    assertFalse(entries.contains(accessor))
  }

  private val structuralSource =
    """
    import scala.reflect.Selectable.reflectiveSelectable
    object Lib {
      def size(x: { def length(): Int }): Int = x.length()
      def plain(): Int = 1
    }
    """

  private def extensionOf(manifest: Manifest, stubUnlinkable: Boolean) = {
    val file = Files.createTempFile("host", ".snm")
    manifest.write(file)
    (c: build.NativeConfig) =>
      c.withLiveExecution(
        LiveExecution.Extension(
          hostManifest = file,
          roots = Nil,
          stubUnlinkable = stubUnlinkable
        )
      )
  }

  @Test def structuralCallsInExtensionsBecomeStubs(): Unit = {
    // Extensions support no structural calls (MUnit's
    // Compat.collectionClassName calls Iterable.collectionClassName so)
    val ext = extensionOf(emptyManifest(Nil), stubUnlinkable = true)
    link("Lib", structuralSource, ext) { (_, result) =>
      def defn(id: String) = result.defns.collectFirst {
        case d: nir.Defn.Define
            if d.name.top.id == "Lib$" && (d.name.sig.unmangled match {
              case nir.Sig.Method(`id`, _, _) => true
              case _                          => false
            }) =>
          d
      }
      assertTrue(defn("size").exists(throws))
      assertTrue(defn("plain").exists(!throws(_)))
      assertFalse(result.defns.exists {
        case d: nir.Defn.Define =>
          d.insts.exists {
            case nir.Inst.Let(_, _: nir.Op.Dynmethod, _) => true
            case _                                       => false
          }
        case _ => false
      })
    }
  }

  @Test def structuralCallsInExtensionsAreUnsupported(): Unit = {
    val ext = extensionOf(emptyManifest(Nil), stubUnlinkable = false)
    doesNotLink("Lib", structuralSource, ext) { (_, failure) =>
      assertEquals(
        Seq(Reach.UnsupportedFeature.StructuralCalls),
        failure.unsupportedFeatures.map(_.kind)
      )
    }
  }

  @Test def extensionsTakeTheHostsCodeGenerationSettings(): Unit = {
    // How generated code talks to the runtime is the host's: threading, the
    // kind of safepoint polls and C++ exceptions
    val manifest = emptyManifest(Nil)
    val host = manifest.copy(header =
      manifest.header.copy(yieldPointTraps = true, cxxExceptions = true)
    )
    val config = build.Config.empty.withCompilerConfig(
      _.withGC(build.GC.immix).withMultithreading(false)
    )
    val ext = build.Build.extensionConfig(config, host)
    assertTrue(ext.compilerConfig.multithreadingSupport)
    assertTrue(ext.useTrapBasedGCYieldPoints)
    assertTrue(ext.usingCppExceptions)
    live.LiveLink.checkExtensionConfig(ext, host)
    val plain = build.Build.extensionConfig(
      config.withCompilerConfig(
        _.withCppOptions(Seq("-fcxx-exceptions"))
          .withGarbageCollectorConfig(_.withTrapBasedYieldPoints(true))
      ),
      manifest
    )
    assertFalse(plain.useTrapBasedGCYieldPoints)
    assertFalse(plain.usingCppExceptions)
  }

  @Test def extensionsTakeTheHostsLinktimePropertiesAndServiceProviders()
      : Unit = {
    // The host's values win; the extension keeps the rest of its own
    val manifest = emptyManifest(Nil)
    val host = manifest.copy(header =
      manifest.header.copy(
        linktimeProperties = Seq(
          Manifest.LinktimeProperty("shared", "I", "1"),
          Manifest.LinktimeProperty("host.only", "T", "host")
        ),
        serviceProviders =
          Seq(Manifest.ServiceEntry("api.Service", Seq("api.A")))
      )
    )
    val config = build.Config.empty.withCompilerConfig(
      _.withLinktimeProperties(Map("shared" -> 2, "ext.only" -> true))
        .withServiceProviders(
          Map("api.Service" -> Seq("ext.B"), "ext.Other" -> Seq("ext.C"))
        )
    )
    val ext = build.Build.extensionConfig(config, host).compilerConfig
    assertEquals(
      Map("shared" -> 1, "host.only" -> "host", "ext.only" -> true),
      ext.linktimeProperties
    )
    assertEquals(
      Map("api.Service" -> Seq("api.A", "ext.B"), "ext.Other" -> Seq("ext.C")),
      ext.serviceProviders.map { case (k, v) => k -> v.toSeq }
    )
  }

  @Test def manifestRoundTrip(): Unit = {
    val m = Manifest(
      header = Manifest.Header(
        targetTriple = "arm64-apple-darwin",
        sizeOfPtr = 8,
        nirCompat = nir.Versions.compat,
        nirRevision = nir.Versions.revision,
        runtimeAbi = Manifest.RuntimeAbiVersion,
        multithreading = true,
        gc = "immix",
        mode = "debug",
        usesLockWords = true,
        yieldPointTraps = false,
        cxxExceptions = false,
        virtualThreads = false,
        hostBinary = "/tmp/host",
        linktimeProperties = Seq(
          Manifest.LinktimeProperty("flag", "Z", "true"),
          Manifest.LinktimeProperty("ratio", "D", "0.5"),
          Manifest.LinktimeProperty("separator", "C", ",")
        ),
        serviceProviders =
          Seq(Manifest.ServiceEntry("api.Service", Seq("api.A", "api.B")))
      ),
      classes = Seq(
        Manifest.ClassEntry(
          name = "api.Base",
          parent = "java.lang.Object",
          traits = Seq("api.Plugin"),
          flags = Manifest.Flags.Open | Manifest.Flags.Abstract,
          size = 16,
          fields = Seq(Manifest.FieldEntry("M8api.BaseF1x", 8, "i")),
          vtable = Seq("D5greetL16java.lang.StringEO"),
          direct = Seq(Manifest.DirectEntry("D1fiEO", ""))
        )
      ),
      traits = Seq(
        Manifest.TraitEntry(
          name = "api.Plugin",
          open = true,
          methods = Seq("D3runiiEO"),
          direct = Nil
        )
      ),
      exports = Seq("M8api.BaseD5greetL16java.lang.StringEO"),
      dynsigs = Nil
    )
    val path = Files.createTempFile("manifest", ".snm")
    m.write(path)
    val read = Manifest.read(path)
    assertEquals(m, read)
    assertEquals(
      Seq(true, 0.5, ','),
      read.header.linktimeProperties.map(_.value)
    )
    // A manifest written again with the same modification time and size is
    // read again
    val mtime = Files.getLastModifiedTime(path)
    val size = Files.size(path)
    val other = m.copy(classes = m.classes.map(_.copy(size = 24)))
    other.write(path)
    Files.setLastModifiedTime(path, mtime)
    assertEquals(size, Files.size(path))
    assertEquals(other, Manifest.read(path))
    m.write(path)
    Files.setLastModifiedTime(path, mtime)
    assertEquals(m, Manifest.read(path))
    assertEquals(m.hash, read.hash)
    // The hash a read manifest takes from its file is that of its encoding
    assertEquals(Manifest.sha1Hex(read.payload), read.hash)
    // A damaged file is refused
    val bytes = Files.readAllBytes(path)
    bytes(bytes.length - 1) = (bytes(bytes.length - 1) ^ 1).toByte
    try {
      Manifest.decode(bytes, "damaged")
      fail("expected a hash mismatch")
    } catch {
      case e: build.BuildException =>
        assertTrue(e.getMessage, e.getMessage.contains("hash mismatch"))
    }
  }
}

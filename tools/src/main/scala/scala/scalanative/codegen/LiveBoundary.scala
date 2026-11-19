package scala.scalanative
package codegen

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

import scala.collection.mutable

import scala.scalanative.build.{BuildException, Config}
import scala.scalanative.linker.Field
import scala.scalanative.live.Manifest

/** The boundary between host and extension in generated code. A host link
 *  writes the list of symbols its binary exports and the manifest that
 *  extensions link against. An extension link checks that the host exports
 *  every host symbol its code uses and declares those symbols; it also writes
 *  its delta (`LiveDelta`).
 */
private[scalanative] object LiveBoundary {
  private implicit val pos: nir.SourcePosition = nir.SourcePosition.NoPosition

  private def extern(id: String): nir.Global.Member =
    nir.Global.Member(nir.Global.Top("__"), nir.Sig.Extern(id))

  /** The host's manifest hash, a C string the loader compares. */
  val HostHashName = extern("__scalanative_ext_host_hash")

  /** The C runtime's functions and variables, which extension code calls and
   *  reads as the host's code does, and the native libraries' C glue
   *  (posixlib's `scalanative_stderr_fileno` and the like), which their extern
   *  bindings call: patterns that the host's export list holds, as extension
   *  code may call a binding the host never calls. The runtime's own symbols
   *  that only its C code uses are hidden and stay out.
   */
  val runtimeExports: Seq[String] = Seq("scalanative_*", "__scalanative_*")

  def afterLowering(config: Config, defns: Seq[nir.Defn])(implicit
      meta: Metadata
  ): Seq[nir.Defn] = meta.live match {
    case host: LiveMetadata.Host     => writeHost(config, host, defns)
    case ext: LiveMetadata.Extension =>
      val out = declareHostSymbols(ext, checkExtension(ext, defns))
      LiveDelta.write(ext, out, extensionOutput(config))
      out
    case LiveMetadata.Disabled | _ => defns
  }

  /** Scala symbols this link defines: methods, RTTI, module accessors, module
   *  slots and constant module instances. Not the generator's own tables, the
   *  stubs of dispatch tables, private constants and the entries of anonymous
   *  sections (the class list), which have no symbol.
   */
  private def scalaSymbols(defns: Seq[nir.Defn])(implicit
      meta: Metadata
  ): Seq[String] =
    defns.iterator
      .collect {
        case defn: nir.Defn.Define => defn.name
        case defn: nir.Defn.Var    => defn.name
        case defn: nir.Defn.Const  => defn.name
      }
      .collect {
        case name @ nir.Global.Member(owner, sig)
            if !sig.isExtern && owner.id != "__" && owner.id != "__const" &&
              owner.id != "__scalanative_metadata" &&
              !meta.globalSections.forGlobal(name).exists(_.anonymous) &&
              owner != LiveMetadata.HostStubOwner =>
          name.mangle
      }
      .toSeq
      .distinct
      .sorted

  /** The bundle (macOS) or shared object an extension link writes. */
  def extensionOutput(config: Config): Path =
    config.baseDir.resolve(
      config.moduleName + (if (config.targetsApple) ".bundle" else ".so")
    )

  def manifestPath(config: Config): Path =
    config.artifactPath.resolveSibling(config.artifactName + ".snm")

  def exportListPath(config: Config): Path =
    config.workDir.resolve("exported-symbols.txt")

  private def writeHost(
      config: Config,
      host: LiveMetadata.Host,
      defns: Seq[nir.Defn]
  )(implicit meta: Metadata): Seq[nir.Defn] = {
    val exports = scalaSymbols(defns)
    val manifest = buildManifest(config, host, exports)
    manifest.write(manifestPath(config))

    // One symbol per line; Mach-O names carry a leading underscore. Scala
    // symbols are `_S` and the NIR mangling, as the code generator names them
    val symbols = exports.map("_S" + _)
    // With patterns, which both linkers accept in these lists. In an ELF
    // dynamic list, GNU ld matches a quoted name literally: names are quoted,
    // as NIR mangling uses characters its lexer does not take, and patterns
    // are not.
    val text =
      if (config.targetsApple)
        (symbols ++ runtimeExports).map("_" + _).mkString("", "\n", "\n")
      else
        (symbols.map(s => "\"" + s + "\";") ++ runtimeExports.map(_ + ";"))
          .mkString("{\n", "\n", "\n};\n")
    Files.createDirectories(config.workDir)
    Files.write(exportListPath(config), text.getBytes(UTF_8))
    config.logger.info(
      s"Live execution host: ${host.openClasses.size} open classes, ${host.openTraits.size} open traits, " +
        s"${symbols.size} exported symbols, manifest ${Files.size(manifestPath(config))} bytes (${manifest.hash.take(12)})"
    )

    defns :+ nir.Defn.Const(
      nir.Attrs.None,
      HostHashName,
      nir.Type.ArrayValue(nir.Type.Byte, manifest.hash.length + 1),
      nir.Val.ByteString(manifest.hash.getBytes(UTF_8))
    )
  }

  private def buildManifest(
      config: Config,
      host: LiveMetadata.Host,
      exports: Seq[String]
  )(implicit meta: Metadata): Manifest = {
    val analysis = meta.analysis
    val platform = meta.platform
    val header = Manifest.Header(
      targetTriple = meta.config.configuredOrDetectedTriple.toString,
      sizeOfPtr = platform.sizeOfPtr,
      nirCompat = nir.Versions.compat,
      nirRevision = nir.Versions.revision,
      runtimeAbi = Manifest.RuntimeAbiVersion,
      multithreading = platform.isMultithreadingEnabled,
      gc = config.gc.name,
      mode = config.mode.name,
      usesLockWords = meta.usesLockWords,
      yieldPointTraps = platform.useGCYieldPointTraps,
      cxxExceptions = platform.useCxxExceptions,
      virtualThreads = platform.isMultithreadingEnabled &&
        Lower.usesVirtualThreads(analysis),
      hostBinary = config.artifactPath.toAbsolutePath().toString,
      linktimeProperties = config.compilerConfig.linktimeProperties.toSeq
        .sortBy(_._1)
        .flatMap {
          case (name, value) =>
            Manifest.LinktimeProperty.of(name, value)
        },
      serviceProviders = config.compilerConfig.serviceProviders.toSeq
        .sortBy(_._1)
        .map {
          case (service, providers) =>
            Manifest.ServiceEntry(service, providers.toSeq.distinct)
        }
    )

    def direct(scope: linker.ScopeInfo, hasSlot: nir.Sig => Boolean) =
      scope.calls.toSeq
        .filter(sig => sig.isVirtual && !hasSlot(sig))
        .sortBy(_.mangle)
        .flatMap { sig =>
          val targets = scope.targets(sig)
          if (targets.size <= 1 && !targets.exists(analysis.isPhantom) &&
              !host.lacksImplementations(scope, sig))
            Some(
              Manifest.DirectEntry(
                sig.mangle,
                targets.headOption.fold("")(_.mangle)
              )
            )
          else None
        }

    val classes = meta.classes.map { cls =>
      val layout = meta.layout(cls)
      val ownFields = cls.members.collect { case f: Field => f }.toSeq
      val fields = ownFields.map { f =>
        val offset = layout.layout.tys(layout.index(f)).offset
        Manifest.FieldEntry(f.name.mangle, offset.toInt, f.ty.mangle)
      }
      val vtable = meta.vtable(cls)
      val flags =
        (if (host.openClasses.contains(cls)) Manifest.Flags.Open else 0) |
          (if (cls.isModule) Manifest.Flags.Module else 0) |
          (if (cls.isModule && cls.isConstantModule(analysis))
             Manifest.Flags.ConstantModule
           else 0) |
          (if (cls.allocated) Manifest.Flags.Allocated else 0) |
          (if (cls.attrs.isAbstract) Manifest.Flags.Abstract else 0)
      Manifest.ClassEntry(
        name = cls.name.id,
        parent = cls.parent.fold("")(_.name.id),
        traits = cls.traits.map(_.name.id),
        flags = flags,
        size = layout.size.toInt,
        fields = fields,
        vtable = vtable.signatures
          .drop(cls.parent.fold(0)(meta.vtable(_).signatures.size))
          .map(_.mangle),
        direct = direct(cls, sig => vtable.index(sig) >= 0)
      )
    }
    val traits = meta.traits.map { trt =>
      Manifest.TraitEntry(
        name = trt.name.id,
        open = host.openTraits.contains(trt),
        methods = trt.methods.map(_.mangle),
        direct = direct(trt, _ => false)
      )
    }
    Manifest(
      header = header,
      classes = classes,
      traits = traits,
      exports = exports,
      dynsigs = analysis.dynsigs.map(_.mangle)
    )
  }

  /** Declarations of the host's RTTI, module instances and accessors the
   *  extension's code refers to: the host defines them.
   */
  private def declareHostSymbols(
      ext: LiveMetadata.Extension,
      defns: Seq[nir.Defn]
  )(implicit meta: Metadata): Seq[nir.Defn] = {
    val defined = defns.iterator.map(_.name).toSet
    val needed = mutable.LinkedHashSet.empty[nir.Global.Member]
    val traverse = new nir.Traverse {
      override def onVal(value: nir.Val): Unit = value match {
        case nir.Val.Global(name @ nir.Global.Member(owner, sig), _)
            if !sig.isExtern && ext.manifest.owns(owner) &&
              !defined.contains(name) =>
          needed += name
        case _ => super.onVal(value)
      }
    }
    defns.foreach {
      case defn: nir.Defn.Define => traverse.onInsts(defn.insts)
      case defn: nir.Defn.Var    => traverse.onVal(defn.rhs)
      case defn: nir.Defn.Const  => traverse.onVal(defn.rhs)
      case _                     => ()
    }
    val external = nir.Attrs.None.withIsExtern(true)
    val declarations = needed.toSeq.sortBy(_.mangle).map { name =>
      name.sig.unmangled match {
        case nir.Sig.Generated("type") =>
          val ty = meta.analysis.infos.get(name.owner) match {
            case Some(info: linker.ScopeInfo) => meta.rtti(info).struct
            case _                            => meta.layouts.Rtti.layout
          }
          nir.Defn.Var(external, name, ty, nir.Val.Zero(ty))
        case nir.Sig.Generated("instance") =>
          val ty = meta.layouts.ObjectHeader.layout
          nir.Defn.Var(external, name, ty, nir.Val.Zero(ty))
        case nir.Sig.Generated("load") =>
          nir.Defn.Declare(
            nir.Attrs.None,
            name,
            nir.Type.Function(Nil, nir.Type.Ref(name.owner))
          )
        case _ =>
          meta.analysis.infos.get(name) match {
            case Some(method: linker.Method) =>
              nir.Defn.Declare(nir.Attrs.None, name, method.ty)
            case _ =>
              // A target the host's manifest names for a call the host
              // devirtualized, in a class this link never loaded
              nir.Defn.Declare(
                nir.Attrs.None,
                name,
                methodType(name).getOrElse(
                  throw new BuildException(
                    s"The extension refers to ${name.show} of its host, of unknown type"
                  )
                )
              )
          }
      }
    }
    defns ++ declarations
  }

  /** The type of a method from its signature: the receiver, unless static, then
   *  the parameters.
   */
  private def methodType(name: nir.Global.Member): Option[nir.Type.Function] =
    name.sig.unmangled match {
      case nir.Sig.Method(_, types, scope) =>
        val params = types.init
        Some(
          if (scope.isStatic) nir.Type.Function(params, types.last)
          else nir.Type.Function(nir.Type.Ref(name.owner) +: params, types.last)
        )
      case nir.Sig.Ctor(types) =>
        Some(
          nir.Type.Function(nir.Type.Ref(name.owner) +: types, nir.Type.Unit)
        )
      case _ => None
    }

  /** Before lowering, in an extension link: calls and table entries that name
   *  host methods the host does not export become stubs that throw
   *  `NoSuchMethodException` naming the method. In a host link: the stubs of
   *  its dispatch tables are added (`LiveMetadata.Host`).
   */
  def beforeLowering(defns: Seq[nir.Defn])(implicit
      meta: Metadata
  ): Seq[nir.Defn] = meta.live match {
    case ext: LiveMetadata.Extension =>
      checkExtension(ext, defns, lowered = false)
    case host: LiveMetadata.Host => defns ++ host.stubDefns
    case _                       => defns
  }

  /** Every host symbol the extension's code refers to must be one the host
   *  exports. A host method that is missing becomes a stub that throws when
   *  called, so that the extension fails only if that code runs; anything else
   *  missing (RTTI, module instances) is an error.
   */
  private def checkExtension(
      ext: LiveMetadata.Extension,
      defns: Seq[nir.Defn],
      lowered: Boolean = true
  )(implicit meta: Metadata): Seq[nir.Defn] = {
    val missing = mutable.SortedSet.empty[String]
    val exports = ext.manifest.exportSet
    // Copies of methods that the host or an earlier extension does not export
    // (`LiveReach.ExtensionContext.copiesMethod`) are defined here
    val defined = defns.iterator.collect {
      case defn: nir.Defn.Define => defn.name
    }.toSet
    def isMissing(name: nir.Global): Boolean = name match {
      case member @ nir.Global.Member(owner, sig) =>
        !sig.isExtern && ext.manifest.owns(owner) &&
          !exports.contains(member.mangle) && !defined.contains(member)
      case _ => false
    }
    val traverse = new nir.Traverse {
      override def onVal(value: nir.Val): Unit = value match {
        case nir.Val.Global(name, _) =>
          if (isMissing(name)) missing += name.mangle
        case _ => super.onVal(value)
      }
    }
    defns.foreach {
      case defn: nir.Defn.Define => traverse.onInsts(defn.insts)
      case defn: nir.Defn.Var    => traverse.onVal(defn.rhs)
      case defn: nir.Defn.Const  => traverse.onVal(defn.rhs)
      case _                     => ()
    }
    if (missing.isEmpty) defns
    else {
      val declared = defns.collect {
        case defn: nir.Defn.Declare => defn.name.mangle -> defn
      }.toMap
      val (methods, others) = missing.toSeq.partition(declared.contains)
      if (others.nonEmpty)
        throw new BuildException(
          s"The extension uses ${others.size} symbols its host does not export " +
            "(add their classes to the host's callable set):\n  " +
            others
              .take(30)
              .map(m => nir.Unmangle.unmangleGlobal(m).show)
              .mkString("\n  ") +
            (if (others.size > 30) "\n  ..." else "")
        )
      meta.buildConfig.logger.warn(
        s"The extension calls ${methods.size} host methods the host does not export; " +
          "they throw if called:\n  " +
          methods
            .take(30)
            .map(m => nir.Unmangle.unmangleGlobal(m).show)
            .mkString("\n  ")
      )
      val stubOwner = nir.Global.Top("__scalanative_ext_stub")
      val stubs = methods.map { m =>
        m -> stubOwner.member(nir.Sig.Generated(m))
      }.toMap
      val rename = new nir.Transform {
        override def onVal(value: nir.Val): nir.Val = value match {
          case nir.Val.Global(name, ty) if stubs.contains(name.mangle) =>
            nir.Val.Global(stubs(name.mangle), ty)
          case _ => super.onVal(value)
        }
      }
      val stubDefns = methods.map { m =>
        val decl = declared(m)
        val nir.Type.Function(argtys, _) = decl.ty
        implicit val fresh: nir.Fresh = nir.Fresh()
        implicit val scopeId: nir.ScopeId = nir.ScopeId.TopLevel
        val params = argtys.map(ty => nir.Val.Local(fresh(), ty))
        val call =
          if (lowered)
            nir.Op.Call(
              Lower.throwUndefinedTy,
              Lower.throwUndefinedVal,
              Seq(nir.Val.Null)
            )
          else
            nir.Op.Call(
              Lower.throwNoSuchMethodStringTy,
              Lower.throwNoSuchMethodVal,
              Seq(
                nir.Val.Null,
                nir.Val.String(
                  "host does not export " + nir.Unmangle.unmangleGlobal(m).show
                )
              )
            )
        nir.Defn.Define(
          nir.Attrs.None,
          stubs(m),
          decl.ty,
          Seq(
            nir.Inst.Label(fresh(), params),
            nir.Inst.Let(call, nir.Next.None),
            nir.Inst.Unreachable(nir.Next.None)
          )
        )
      }
      rename.onDefns(defns.filterNot {
        case defn: nir.Defn.Declare => stubs.contains(defn.name.mangle)
        case _                      => false
      }) ++ stubDefns
    }
  }
}

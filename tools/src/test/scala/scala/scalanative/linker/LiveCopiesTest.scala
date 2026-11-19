package scala.scalanative
package linker

import java.nio.file.{Files, Path}

import org.junit.Assert._
import org.junit.Test

import scala.scalanative.ProgramBuilds._
import scala.scalanative.build.{Config, LiveExecution, Mode}
import scala.scalanative.codegen.LiveBoundary
import scala.scalanative.live.Manifest
import scala.scalanative.util.Scope

/** An extension copies the host methods that its host does not export, which
 *  the host's optimizer inlined everywhere or which nothing in the host calls,
 *  and the methods those copies call. The copies share the host's state and
 *  resolve the link-time properties and service providers that the host's
 *  configuration sets as the host does; code that needs what the host does not
 *  have (a field no host code uses, an allocation of a class the host never
 *  allocates) or that no class path entry has (listed in `missing-classes.txt`)
 *  becomes a stub that throws. A call that the host does not dispatch, on a
 *  class whose subclasses are all the host's or on a trait that only host
 *  classes implement, goes to the implementation of the receiver's class. A
 *  second extension, chained to the first as the REPL chains its lines, makes
 *  copies of its own: the first's are hidden, and its delta exports none of the
 *  host's methods.
 */
class LiveCopiesTest {

  private val sources = Map(
    "api/Api.scala" ->
      """|package api
         |trait Entry {
         |  def run(): String
         |  def areaOf(shape: AnyRef): Int
         |  def perimeterOf(shape: AnyRef): Int
         |}
         |""".stripMargin,
    "host/Host.scala" ->
      """|package host
         |import scala.scalanative.annotation.alwaysinline
         |import scala.scalanative.runtime.Extension
         |import scala.scalanative.livecopies.Settings
         |object State {
         |  var hits = 0
         |  @alwaysinline def hit(): Unit = hits += 1
         |  // The host calls neither: a copy of twice() needs a copy of bump()
         |  @alwaysinline def bump(n: Int): Unit = hits += n
         |  def twice(): Unit = { bump(1); bump(1) }
         |}
         |final class Point(val x: Int, val y: Int) {
         |  @alwaysinline def sum: Int = x + y
         |}
         |final class Counter {
         |  var count: Int = _
         |  var unused: Int = _
         |}
         |final class NeverMade(val v: Int)
         |// The host never calls area() nor perimeter(): the extension dispatches
         |// them itself, on a class and on a trait that only host classes implement
         |abstract class Shape { def area(): Int }
         |trait Figure { def perimeter(): Int }
         |final class Square(side: Int) extends Shape with Figure {
         |  def area(): Int = side * side
         |  def perimeter(): Int = 4 * side
         |}
         |final class Rect(w: Int, h: Int) extends Shape with Figure {
         |  def area(): Int = w * h
         |  def perimeter(): Int = 2 * (w + h)
         |}
         |// A service provider of the host's configuration
         |trait Service { def name: String }
         |final class ProviderA extends Service { def name: String = "a" }
         |object Services {
         |  // The host's code iterates, with the host's dispatch; an extension's
         |  // copy of names() has the call that the linker resolves to providers
         |  @noinline def count(loader: java.util.ServiceLoader[Service]): Int = {
         |    var n = 0
         |    val it = loader.iterator()
         |    while (it.hasNext()) { it.next(); n += 1 }
         |    n
         |  }
         |  @alwaysinline def names(): Int =
         |    count(java.util.ServiceLoader.load(classOf[Service]))
         |}
         |object Host {
         |  def main(args: Array[String]): Unit = {
         |    State.hit()
         |    val c = new Counter
         |    c.count += 1
         |    println(new Point(c.count, 2).sum)
         |    println(args.length > 5 && (args(0): Any).isInstanceOf[NeverMade])
         |    println(Settings.describe() + " " + Services.names())
         |    // Pairs of arguments: a bundle and its entry module, loaded in order
         |    // and globally, as the REPL loads its lines
         |    var i = 0
         |    while (i < args.length) {
         |      val entry = Extension.load(args(i), global = true).module(args(i + 1)).asInstanceOf[api.Entry]
         |      println(entry.run())
         |      println(entry.areaOf(new Square(3)) + " " + entry.areaOf(new Rect(2, 5)) + " " +
         |        entry.perimeterOf(new Square(3)) + " " + entry.perimeterOf(new Rect(2, 5)))
         |      i += 2
         |    }
         |    println(State.hits)
         |  }
         |}
         |""".stripMargin,
    // A link-time property of the host's configuration, in a package of the
    // runtime's, which `resolved` needs
    "host/Settings.scala" ->
      """|package scala.scalanative.livecopies
         |import scala.scalanative.annotation.alwaysinline
         |import scala.scalanative.unsafe.{resolvedAtLinktime, resolved}
         |object Settings {
         |  @resolvedAtLinktime("live.copies.flag") def flag: Boolean = resolved
         |  @alwaysinline def describe(): String = if (flag) "flag on" else "flag off"
         |}
         |""".stripMargin,
    "ext/Ext.scala" ->
      """|package ext
         |object Main extends api.Entry {
         |  @noinline def touchUnused(): String = {
         |    val c = new host.Counter
         |    c.unused = 5
         |    "touched"
         |  }
         |  @noinline def makeNeverMade(): String = new host.NeverMade(1).v.toString
         |  @noinline def useGone(): String = gone.Thing.v.toString
         |  def run(): String = {
         |    host.State.hit()
         |    host.State.twice()
         |    val p = new host.Point(3, 4)
         |    // No lambdas: they implement scala.Function0, which is not open
         |    val touched =
         |      try touchUnused()
         |      catch { case _: NoSuchMethodException => "stub" }
         |    val made =
         |      try makeNeverMade()
         |      catch { case _: NoSuchMethodException => "stub" }
         |    val gone =
         |      try useGone()
         |      catch { case _: NoSuchMethodException => "stub" }
         |    p.sum.toString + " " + touched + " " + made + " " + gone + " " +
         |      scala.scalanative.livecopies.Settings.describe() + " " + host.Services.names()
         |  }
         |  def areaOf(shape: AnyRef): Int = shape.asInstanceOf[host.Shape].area()
         |  def perimeterOf(shape: AnyRef): Int =
         |    shape.asInstanceOf[host.Figure].perimeter()
         |}
         |""".stripMargin,
    // Compiled, then deleted before the extensions link
    "gone/Gone.scala" ->
      """|package gone
         |object Thing { def v: Int = 1 }
         |""".stripMargin,
    "ext2/Ext2.scala" ->
      """|package ext2
         |object Main extends api.Entry {
         |  def run(): String = {
         |    host.State.hit()
         |    host.State.twice()
         |    "second " + scala.scalanative.livecopies.Settings.describe()
         |  }
         |  def areaOf(shape: AnyRef): Int = shape.asInstanceOf[host.Shape].area()
         |  def perimeterOf(shape: AnyRef): Int =
         |    shape.asInstanceOf[host.Figure].perimeter()
         |}
         |""".stripMargin
  )

  private val open = Seq(
    "api.*",
    "java.lang.Object",
    "java.io.Serializable",
    "scala.Product",
    "scala.Equals"
  )

  // Mangled names of host.State's methods
  private val hit = "_SM11host.State$D3hituEO"
  private val twice = "_SM11host.State$D5twiceuEO"
  private val bump = "_SM11host.State$D4bumpiuEO"

  // Without scala.jdk.CollectionConverters, which Scala 2.12 lacks
  private def seq[A](it: java.util.Iterator[A]): Seq[A] = {
    val out = Seq.newBuilder[A]
    while (it.hasNext()) out += it.next()
    out.result()
  }

  private def lines(dir: Path): Seq[String] =
    seq(Files.list(dir).iterator())
      .filter(_.toString.endsWith(".ll"))
      .flatMap(p => seq(Files.readAllLines(p).iterator()))

  private def definesHidden(ir: Seq[String], name: String) =
    ir.exists(l => l.startsWith("define hidden ") && l.contains(name))

  private val expected =
    "3\nfalse\nflag on 1\n7 stub stub stub flag on 1\n9 10 12 14\nsecond flag on\n9 10 12 14\n7\n"

  @Test def anExtensionCopiesWhatItsHostDoesNotExport(): Unit =
    withWorkDir("live-copies") { workDir =>
      Scope { implicit scope =>
        val classes = compile(sources, workDir.resolve("classes"))
        val services = classes.resolve("META-INF").resolve("services")
        Files.createDirectories(services)
        Files.write(
          services.resolve("host.Service"),
          "host.ProviderA\n".getBytes("UTF-8")
        )
        val roots1 = workDir.resolve("roots1")
        val roots2 = workDir.resolve("roots2")
        Files.createDirectories(roots1)
        Files.createDirectories(roots2)
        Files.move(classes.resolve("ext"), roots1.resolve("ext"))
        Files.move(classes.resolve("ext2"), roots2.resolve("ext2"))
        seq(Files.walk(classes.resolve("gone")).iterator()).reverse
          .foreach(Files.delete(_))

        val hostConfig = Config.empty
          .withBaseDir(workDir.resolve("host"))
          .withModuleName("host")
          .withClassPath(classpath(classes))
          .withMainClass(Some("host.Host"))
          .withCompilerConfig(
            nativeConfig
              .withMode(Mode.releaseFast)
              .withLinktimeProperties(Map("live.copies.flag" -> true))
              .withServiceProviders(
                Map("host.Service" -> Seq("host.ProviderA"))
              )
              .withLiveExecution(
                LiveExecution.Host(open = open, callable = Seq("api.*"))
              )
          )
        val host = build(hostConfig)
        val exports = seq(
          Files.readAllLines(LiveBoundary.exportListPath(hostConfig)).iterator()
        )
        assertFalse(
          "the host inlines hit() everywhere",
          exports.exists(_.contains(hit))
        )
        val header = Manifest.read(LiveBoundary.manifestPath(hostConfig)).header
        assertEquals(
          Seq(Manifest.LinktimeProperty("live.copies.flag", "Z", "true")),
          header.linktimeProperties
        )
        assertEquals(
          Seq(Manifest.ServiceEntry("host.Service", Seq("host.ProviderA"))),
          header.serviceProviders
        )

        // The first extension, which writes a delta as a REPL line does, and
        // the second, linked against it
        def extensions(name: String, optimize: Boolean) = {
          def link(
              module: String,
              roots: Path,
              classPath: Seq[Path],
              chain: Seq[Path],
              delta: Option[Path]
          ) = {
            val config = Config.empty
              .withBaseDir(workDir.resolve(name).resolve(module))
              .withModuleName(module)
              .withClassPath(classPath)
              .withCompilerConfig(
                nativeConfig
                  .withOptimize(optimize)
                  .withMode(if (optimize) Mode.releaseFast else Mode.debug)
                  .withLiveExecution(
                    LiveExecution.Extension(
                      hostManifest = LiveBoundary.manifestPath(hostConfig),
                      roots = Seq(roots),
                      hostBinary = Some(host),
                      chain = chain,
                      delta = delta,
                      stubUnlinkable = true
                    )
                  )
              )
            (config, build(config))
          }
          val delta = workDir.resolve(name).resolve("ext.snm")
          val first =
            link("ext", roots1, roots1 +: classpath(classes), Nil, Some(delta))
          val second = link(
            "ext2",
            roots2,
            roots2 +: roots1 +: classpath(classes),
            Seq(delta),
            None
          )
          (first, second, delta)
        }

        val ((plainConfig, plain), (plain2Config, plain2), plainDelta) =
          extensions("plain", optimize = false)
        val stubbed = seq(
          Files
            .readAllLines(plainConfig.workDir.resolve("stubbed-methods.txt"))
            .iterator()
        )
        val ir = lines(plainConfig.workDir.resolve("generated"))
        // The copies are the extension's own, twice()'s with bump()'s, which
        // only the copy calls
        Seq(hit, twice, bump).foreach { name =>
          assertTrue(name, definesHidden(ir, name))
        }
        assertTrue(
          stubbed.mkString("\n"),
          stubbed.exists(l =>
            l.contains("makeNeverMade") && l.contains("never allocates")
          )
        )
        assertTrue(
          stubbed.mkString("\n"),
          stubbed.exists(l =>
            l.contains("unused") && l.contains("does not keep")
          )
        )
        assertTrue(
          stubbed.mkString("\n"),
          stubbed.exists(l => l.contains("useGone") && l.contains("gone.Thing"))
        )
        assertEquals(
          Seq("gone.Thing$"),
          seq(
            Files
              .readAllLines(plainConfig.workDir.resolve("missing-classes.txt"))
              .iterator()
          )
        )
        Seq("hit", "twice", "bump", "area", "perimeter", "describe", "names")
          .foreach { name =>
            assertFalse(
              stubbed.mkString("\n"),
              stubbed.exists(_.contains(name))
            )
          }
        // area() and perimeter() by the receiver's class id: both
        // implementations of each are copied
        Seq(
          "_SM11host.SquareD4areaiEO",
          "_SM9host.RectD4areaiEO",
          "_SM11host.SquareD9perimeteriEO",
          "_SM9host.RectD9perimeteriEO"
        ).foreach(impl => assertTrue(impl, definesHidden(ir, impl)))
        // The first extension's delta exports none of the host's methods, so
        // the second makes copies of its own
        val deltaExports = Manifest.read(plainDelta).exports
        assertFalse(
          deltaExports.mkString("\n"),
          deltaExports.exists(_.contains("host."))
        )
        val ir2 = lines(plain2Config.workDir.resolve("generated"))
        Seq(hit, twice).foreach { name =>
          assertTrue(name, definesHidden(ir2, name))
        }
        assertEquals(
          expected,
          run(host, plain.toString, "ext.Main$", plain2.toString, "ext2.Main$")
        )

        // Optimized, the extensions link their optimized copies again
        val ((_, optimized), (_, optimized2), _) =
          extensions("optimized", optimize = true)
        assertEquals(
          expected,
          run(
            host,
            optimized.toString,
            "ext.Main$",
            optimized2.toString,
            "ext2.Main$"
          )
        )
      }
    }
}

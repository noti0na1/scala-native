package scala.scalanative

import java.io.File
import java.nio.file.{Files, Path, Paths}

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._
import scala.language.implicitConversions

import org.junit.Assert.fail

import scala.scalanative.buildinfo.ScalaNativeBuildInfo
import scala.scalanative.linker.ReachabilityAnalysis
import scala.scalanative.util.unreachable
import scalanative.build.{Config, Discover, Logger, NativeConfig, ScalaNative}
import scalanative.util.Scope

/** Base class to test the linker. */
abstract class LinkerSpec {

  /** Runs the linker using `driver` with `entry` as entry point on `sources`,
   *  and applies `fn` to the definitions.
   *
   *  @param entry
   *    The entry point for the linker.
   *  @param sources
   *    Map from file name to file content representing all the code to compile
   *    and link.
   *  @param fn
   *    A function to apply to the products of the compilation.
   *  @return
   *    The result of applying `fn` to the resulting definitions.
   */
  def link[T](
      entry: String,
      sources: Map[String, String],
      setupConfig: NativeConfig => NativeConfig = identity,
      extraNir: Seq[nir.Defn] = Nil
  )(fn: (Config, ReachabilityAnalysis.Result) => T): T =
    mayLink(entry, sources, setupConfig, extraNir) {
      case (config, result: ReachabilityAnalysis.Result) => fn(config, result)
      case _ => fail("Expected code to link"); unreachable
    }

  def doesNotLink[T](
      entry: String,
      sources: Map[String, String],
      setupConfig: NativeConfig => NativeConfig = identity,
      extraNir: Seq[nir.Defn] = Nil
  )(fn: (Config, ReachabilityAnalysis.Failure) => T): T =
    mayLink(entry, sources, setupConfig, extraNir) {
      case (config, result: ReachabilityAnalysis.Failure) =>
        fn(config, result)
      case _ => fail("Expected code to not link"); unreachable
    }

  /** @param extraNir
   *    definitions written next to the compiled sources' NIR, one file per
   *    top-level name (in the empty package), for code the compiler does not
   *    write
   */
  protected def mayLink[T](
      entry: String,
      sources: Map[String, String],
      setupConfig: NativeConfig => NativeConfig = identity,
      extraNir: Seq[nir.Defn] = Nil
  )(fn: (Config, ReachabilityAnalysis) => T): T = {
    // Deleted once the scope has closed what it opened in the directory
    val outDir = Files.createTempDirectory("native-test-out")
    try
      NIRCompiler.withSourceDir(sources) { sourcesDir =>
        Scope { implicit in =>
          val compiler = NIRCompiler.getCompiler(outDir)
          val files = compiler.compile(sourcesDir)
          extraNir.groupBy(_.name.top).foreach {
            case (top, defns) =>
              val channel = Files.newByteChannel(
                outDir.resolve(top.id + ".nir"),
                java.nio.file.StandardOpenOption.CREATE,
                java.nio.file.StandardOpenOption.WRITE,
                java.nio.file.StandardOpenOption.TRUNCATE_EXISTING
              )
              try nir.serialization.serializeBinary(defns, channel)
              finally channel.close()
          }
          val config = makeConfig(outDir, entry, setupConfig)
          val entries = ScalaNative.entries(config)
          config.compilerConfig.liveExecution match {
            case host: build.LiveExecution.Host if host.stubUnlinkable =>
              fn(config, live.HostStubs.link(config, entries, host))
            case host: build.LiveExecution.Host =>
              fn(config, live.LiveLink.host(config, entries, host))
            case ext: build.LiveExecution.Extension =>
              // An extension of the compiled sources, unless it names its roots
              val withRoots =
                if (ext.roots.nonEmpty) ext else ext.copy(roots = Seq(outDir))
              val extConfig = config
                .withMainClass(None)
                .withCompilerConfig(_.withLiveExecution(withRoots))
              val extEntries = ScalaNative.entries(extConfig) ++
                live.LiveLink.extensionEntries(extConfig, withRoots)
              fn(
                extConfig,
                if (withRoots.stubUnlinkable)
                  live.HostStubs.linkExtension(extConfig, extEntries, withRoots)
                else linker.Link(extConfig, extEntries)
              )
            case _ => fn(config, linker.Link(config, entries))
          }
        }
      }
    finally scalanative.build.IO.deleteRecursive(outDir)
  }

  private def makeClasspath(outDir: Path)(implicit in: Scope) = {
    val parts: Array[Path] =
      ScalaNativeBuildInfo.nativeRuntimeClasspath
        .split(File.pathSeparator)
        .map(Paths.get(_))

    parts :+ outDir
  }

  private def makeConfig(
      outDir: Path,
      entry: String,
      setupNativeConfig: NativeConfig => NativeConfig
  )(implicit in: Scope): Config = {
    val classpath = makeClasspath(outDir)
    Config.empty
      .withBaseDir(outDir)
      .withClassPath(classpath.toSeq)
      .withMainClass(Some(entry))
      .withCompilerConfig(setupNativeConfig.compose(withDefaults))
      .withLogger(Logger.nullLogger)
  }

  private def withDefaults(config: NativeConfig): NativeConfig = {
    config
      .withTargetTriple("x86_64-unknown-unknown")
      .withClang(Discover.clang())
      .withClangPP(Discover.clangpp())

  }

  protected implicit def String2MapStringString(
      code: String
  ): Map[String, String] =
    Map("source.scala" -> code)

}

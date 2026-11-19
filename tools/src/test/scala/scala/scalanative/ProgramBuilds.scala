package scala.scalanative

import java.io.File
import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Path, Paths}

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

import org.junit.Assert._

import scala.scalanative.build._
import scala.scalanative.buildinfo.ScalaNativeBuildInfo
import scala.scalanative.util.Scope

/** Builds programs that tests compile and runs them. */
object ProgramBuilds {

  /** Applies `fn` to a fresh temporary directory, deleted once `fn` returns.
   */
  def withWorkDir[T](prefix: String)(fn: Path => T): T = {
    val dir = Files.createTempDirectory(prefix)
    try fn(dir)
    finally IO.deleteRecursive(dir)
  }

  /** Compiles `sources` into `dir`, which it creates. */
  def compile(sources: Map[String, String], dir: Path): Path = {
    Files.createDirectories(dir)
    NIRCompiler.withSourceDir(sources) { sourceDir =>
      NIRCompiler.getCompiler(dir).compile(sourceDir)
    }
    dir
  }

  /** The runtime's class path and `classes`. */
  def classpath(classes: Path*): Seq[Path] =
    ScalaNativeBuildInfo.nativeRuntimeClasspath
      .split(File.pathSeparator)
      .map(Paths.get(_))
      .toSeq ++ classes

  /** A debug configuration that runs the NIR checker, whose errors are fatal.
   */
  def nativeConfig: NativeConfig = NativeConfig.empty
    .withClang(Discover.clang())
    .withClangPP(Discover.clangpp())
    .withCompileOptions(Discover.compileOptions())
    .withLinkingOptions(Discover.linkingOptions())
    .withMode(Mode.debug)
    .withCheck(true)
    .withCheckFatalWarnings(true)

  /** Builds `config`, whose warnings and errors the assertion shows if it
   *  fails.
   */
  def build(config: Config)(implicit scope: Scope): Path = {
    val log = new StringBuffer
    val logger = new Logger {
      def trace(msg: Throwable): Unit = ()
      def debug(msg: String): Unit = ()
      def info(msg: String): Unit = ()
      def warn(msg: String): Unit = log.append(msg).append('\n')
      def error(msg: String): Unit = log.append(msg).append('\n')
    }
    Files.createDirectories(config.baseDir)
    try Await.result(Build.build(config.withLogger(logger)), Duration.Inf)
    catch {
      case e: Exception =>
        throw new AssertionError(
          s"${config.moduleName} does not build:\n$log",
          e
        )
    }
  }

  /** Runs `binary` with `args`; it must exit with status 0. */
  def run(binary: Path, args: String*): String = {
    val process = new ProcessBuilder((binary.toString +: args): _*)
      .redirectErrorStream(true)
      .start()
    val output =
      new String(process.getInputStream.readAllBytes(), StandardCharsets.UTF_8)
    assertEquals(s"exit code of $binary:\n$output", 0, process.waitFor())
    output
  }
}

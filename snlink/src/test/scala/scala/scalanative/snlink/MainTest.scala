package scala.scalanative.snlink

import java.nio.file.{Path, Paths}

import scala.collection.mutable

import org.junit.Assert._
import org.junit.Test

import scala.scalanative.build._

class MainTest {
  import Main.{LogLevel, Options, UsageError}

  /** Discovery that records what was asked of it instead of running tools. */
  private class FakeDefaults extends Main.Defaults {
    val asked = mutable.ListBuffer.empty[String]
    private def ask[T](name: String)(value: T): T = { asked += name; value }
    def clang(): Path = ask("clang")(Paths.get("/discovered/clang"))
    def clangPP(): Path = ask("clang++")(Paths.get("/discovered/clang++"))
    def compileOptions(): Seq[String] =
      ask("compileOptions")(Seq("-I/discovered/include", "-Qunused-arguments"))
    def linkingOptions(): Seq[String] =
      ask("linkingOptions")(Seq("-L/discovered/lib"))
    def mode(): Mode = ask("mode")(Mode.debug)
    def gc(): GC = ask("gc")(GC.immix)
    def lto(): LTO = ask("lto")(LTO.none)
    def optimize(): Boolean = ask("optimize")(true)
  }

  private def parse(args: String*): Options = Main.parseArgs(args.toList)

  private def usageError(args: String*): String =
    try {
      parse(args: _*)
      fail(s"expected a usage error for ${args.mkString(" ")}")
      ""
    } catch { case UsageError(message) => message }

  private def config(args: String*): (NativeConfig, FakeDefaults) = {
    val defaults = new FakeDefaults
    (Main.nativeConfig(parse(args: _*), defaults), defaults)
  }

  @Test def defaults(): Unit = {
    val options = parse("--main", "Hello", "classes")
    assertEquals(Some("Hello"), options.mainClass)
    assertEquals(
      Vector(Paths.get("classes").toAbsolutePath()),
      options.classPath
    )
    assertEquals(BuildTarget.application, options.buildTarget)
    assertEquals(LogLevel.Info, options.logLevel)

    val (nativeConfig, _) = config("--main", "Hello", "classes")
    assertEquals(Paths.get("/discovered/clang"), nativeConfig.clang)
    assertEquals(Paths.get("/discovered/clang++"), nativeConfig.clangPP)
    assertEquals(
      Seq("-I/discovered/include", "-Qunused-arguments"),
      nativeConfig.compileOptions
    )
    assertEquals(Seq("-L/discovered/lib"), nativeConfig.linkingOptions)
    assertEquals(Nil, nativeConfig.cOptions)
    assertEquals(Nil, nativeConfig.cppOptions)
    assertEquals(BuildTarget.application, nativeConfig.buildTarget)
  }

  @Test def buildTargets(): Unit = {
    for {
      (name, target) <- Seq(
        "application" -> BuildTarget.application,
        "library-dynamic" -> BuildTarget.libraryDynamic,
        "library-static" -> BuildTarget.libraryStatic
      )
    } {
      val (nativeConfig, _) =
        config("--main", "Hello", "--build-target", name, "classes")
      assertEquals(name, target, nativeConfig.buildTarget)
    }
    // Libraries need no main class; applications do
    assertEquals(
      BuildTarget.libraryDynamic,
      parse("--build-target", "library-dynamic", "classes").buildTarget
    )
    assertEquals(
      None,
      parse("--build-target", "library-static", "classes").mainClass
    )
    assertTrue(usageError("classes").contains("--main"))
    assertTrue(
      usageError("--build-target", "application", "classes").contains("--main")
    )
    assertTrue(
      usageError("--build-target", "executable", "classes")
        .contains("unknown build target executable")
    )
    assertTrue(
      usageError("--main", "Hello", "--build-target").contains("needs a value")
    )
  }

  @Test def clangPaths(): Unit = {
    val (nativeConfig, defaults) = config(
      "--main",
      "Hello",
      "--clang",
      "/opt/llvm/bin/clang",
      "--clang-pp",
      "llvm/clang++",
      "classes"
    )
    assertEquals(Paths.get("/opt/llvm/bin/clang"), nativeConfig.clang)
    assertEquals(
      Paths.get("llvm/clang++").toAbsolutePath(),
      nativeConfig.clangPP
    )
    // Given tools are not discovered: discovery fails without clang on the PATH
    assertFalse(defaults.asked.contains("clang"))
    assertFalse(defaults.asked.contains("clang++"))

    val (onlyClang, onlyClangDefaults) =
      config("--main", "Hello", "--clang", "/opt/llvm/bin/clang", "classes")
    assertEquals(Paths.get("/opt/llvm/bin/clang"), onlyClang.clang)
    assertEquals(Paths.get("/discovered/clang++"), onlyClang.clangPP)
    assertEquals(
      List("clang++"),
      onlyClangDefaults.asked.filter(_.startsWith("clang")).toList
    )
  }

  @Test def compileAndLinkingOptions(): Unit = {
    val (nativeConfig, _) = config(
      "--main",
      "Hello",
      "--compile-option",
      "-O1",
      "--c-compile-option",
      "-std=c11",
      "--c-compile-option",
      "-DC_ONLY",
      "--cpp-compile-option",
      "-std=c++17",
      "--linking-option",
      "-lz",
      "classes"
    )
    assertEquals(
      Seq("-I/discovered/include", "-Qunused-arguments", "-O1"),
      nativeConfig.compileOptions
    )
    assertEquals(Seq("-std=c11", "-DC_ONLY"), nativeConfig.cOptions)
    assertEquals(Seq("-std=c++17"), nativeConfig.cppOptions)
    assertEquals(Seq("-L/discovered/lib", "-lz"), nativeConfig.linkingOptions)

    val (noDefaults, defaults) = config(
      "--main",
      "Hello",
      "--no-default-compile-options",
      "--no-default-linking-options",
      "--compile-option",
      "-O1",
      "--linking-option",
      "-lz",
      "classes"
    )
    assertEquals(Seq("-O1"), noDefaults.compileOptions)
    assertEquals(Seq("-lz"), noDefaults.linkingOptions)
    assertFalse(defaults.asked.contains("compileOptions"))
    assertFalse(defaults.asked.contains("linkingOptions"))

    val (onlyCompile, _) =
      config("--main", "Hello", "--no-default-compile-options", "classes")
    assertEquals(Nil, onlyCompile.compileOptions)
    assertEquals(Seq("-L/discovered/lib"), onlyCompile.linkingOptions)
  }

  @Test def logLevels(): Unit = {
    def level(args: String*) = parse(
      ("--main" +: "Hello" +: args :+ "classes"): _*
    ).logLevel
    assertEquals(LogLevel.Info, level())
    assertEquals(LogLevel.Warn, level("-q"))
    assertEquals(LogLevel.Warn, level("--quiet"))
    assertEquals(LogLevel.Debug, level("-v"))
    assertEquals(LogLevel.Debug, level("--verbose"))
    for (l <- LogLevel.all) assertEquals(l, level("--log-level", l.name))
    // The last one wins
    assertEquals(LogLevel.Error, level("-v", "--log-level", "error"))
    assertTrue(
      usageError("--main", "Hello", "--log-level", "loud", "classes")
        .contains("unknown log level loud")
    )
  }

  @Test def loggerFiltersByLevel(): Unit = {
    def logged(level: LogLevel): List[String] = {
      val lines = mutable.ListBuffer.empty[String]
      val recording = Logger(
        traceFn = t => lines += s"trace ${t.getMessage}",
        debugFn = m => lines += s"debug $m",
        infoFn = m => lines += s"info $m",
        warnFn = m => lines += s"warn $m",
        errorFn = m => lines += s"error $m"
      )
      val logger = Main.logger(level, recording)
      logger.trace(new Exception("t"))
      logger.running(Seq("clang", "-c"))
      logger.time("phase")(())
      logger.warn("w")
      logger.error("e")
      lines.toList.map(_.takeWhile(_ != ' '))
    }
    assertEquals(List("error"), logged(LogLevel.Error))
    assertEquals(List("warn", "error"), logged(LogLevel.Warn))
    assertEquals(List("info", "warn", "error"), logged(LogLevel.Info))
    assertEquals(List("debug", "info", "warn", "error"), logged(LogLevel.Debug))
    assertEquals(
      List("trace", "debug", "info", "warn", "error"),
      logged(LogLevel.Trace)
    )
  }

  @Test def unknownOptionsAreErrors(): Unit = {
    assertTrue(
      usageError("--main", "Hello", "--frobnicate", "classes")
        .contains("unknown option --frobnicate")
    )
    assertTrue(
      usageError("--main", "Hello", "-x", "classes").contains(
        "unknown option -x"
      )
    )
    assertTrue(usageError("--main", "Hello").contains("no class path entries"))
  }

  @Test def version(): Unit = {
    assertTrue(parse("--version").version)
    assertFalse(parse("--main", "Hello", "classes").version)
  }

  @Test def help(): Unit = {
    assertTrue(parse("--help").help)
    assertTrue(parse("--main", "Hello", "-h", "--frobnicate").help)
  }
}

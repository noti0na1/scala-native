package scala.scalanative
package build

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

import scala.scalanative.build.IO.RichPath
import scala.scalanative.linker.ReachabilityAnalysis

/** Weak definitions of C functions that a live-execution host cannot link.
 *
 *  A host whose boundary is everything the class path defines
 *  (`live.HostStubs`) reaches C functions that exist only on other platforms,
 *  or behind `@define`s that no ordinary program of this platform sets (epoll
 *  on macOS, for instance). In ordinary programs, link-time conditions leave
 *  out the code that calls them. When the platform linker reports such
 *  functions undefined, each gets a weak definition that aborts naming the
 *  function, and the link runs again. Real definitions, where they exist, take
 *  precedence.
 *
 *  A stub is a function: an undefined symbol that the program declares as an
 *  extern variable gets none, and the link fails naming it. Undefined symbols
 *  that only C code refers to are taken for functions.
 */
private[scalanative] object LiveWeakStubs {

  /** C symbols the linker's output reports undefined. */
  def undefinedSymbols(
      output: Seq[String],
      targetsApple: Boolean
  ): Seq[String] = {
    val mac = """\s*"_([A-Za-z0-9_$.]+)", referenced from:""".r
    val gnu =
      """.*undefined (?:reference to|symbol:) [`']?([A-Za-z0-9_$.]+)'?.*""".r
    output.flatMap {
      case mac(name) if targetsApple => Some(name)
      case gnu(name)                 => Some(name)
      case _                         => None
    }.distinct
  }

  /** The loader's own functions (`ext_loader.c`). They are never stubbed: a
   *  host without them, built with a nativelib older than the toolchain, cannot
   *  load extensions, so the link fails instead.
   */
  def isLoader(symbol: String): Boolean =
    symbol.startsWith("scalanative_ext_") ||
      symbol.startsWith("__scalanative_ext_")

  /** The C variables the program declares (`@extern` fields). */
  def externVariables(analysis: ReachabilityAnalysis.Result): Set[String] =
    analysis.defns.iterator
      .collect {
        case nir.Defn.Var(attrs, nir.Global.Member(_, sig), _, _)
            if attrs.isExtern =>
          sig.unmangled
      }
      .collect { case nir.Sig.Extern(id) => id }
      .toSet

  /** Compiles weak definitions of `symbols` and returns the object file. */
  def compile(config: Config, symbols: Seq[String]): Path = {
    val dir = Files.createDirectories(config.workDir.resolve("live-weak-stubs"))
    val source = dir.resolve("weak-stubs.c")
    val header =
      """#include <stdio.h>
        |#include <stdlib.h>
        |static void scalanative_live_unlinkable(const char *name) {
        |    fprintf(stderr, "%s is not available in this program (live-execution host)\n", name);
        |    abort();
        |}
        |""".stripMargin
    val stubs = symbols.map { name =>
      s"""__attribute__((weak)) void $name(void) { scalanative_live_unlinkable("$name"); }"""
    }
    Files.write(
      source,
      (header + stubs.mkString("", "\n", "\n")).getBytes(UTF_8)
    )
    val obj = dir.resolve("weak-stubs.o")
    val command =
      Seq(config.clang.abs, "-c", source.abs, "-o", obj.abs, "-w") ++
        config.compilerConfig.targetTriple.toSeq.flatMap(t => Seq("-target", t))
    config.logger.running(command)
    val result = Processes.run(
      command,
      config.workDir.toFile(),
      Logger.toProcessLogger(config.logger)
    )
    if (result != 0) throw new BuildException(s"Failed to compile $source")
    config.logger.info(
      s"Live execution: ${symbols.size} C functions this platform does not define get stubs that abort: " +
        symbols.take(10).mkString(", ") + (if (symbols.size > 10) ", ..."
                                           else "")
    )
    obj
  }
}

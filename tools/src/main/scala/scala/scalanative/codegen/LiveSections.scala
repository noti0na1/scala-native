package scala.scalanative
package codegen

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}

import scala.scalanative.build.IO.RichPath
import scala.scalanative.build.{BuildException, Config, Logger, Processes}

/** The static storage of an extension, as its loader needs it.
 *
 *  Generated code puts global objects (module slots and constant module
 *  instances), class objects and string literals in sections of their own
 *  (`GlobalSections`). The host registers its own sections when it starts
 *  (nativelib's `staticSections.c` and `staticRoots.c`): global objects are GC
 *  roots, and the lock word of a class or string literal whose monitor is
 *  inflated becomes one. An extension is another image with sections of its
 *  own: every extension link compiles this C code, which defines
 *  `__scalanative_ext_sections`, the bounds of its global objects, class
 *  objects and string literals, in that order, which its descriptor points to
 *  and the loader registers (`ext_loader.c`).
 */
private[scalanative] object LiveSections {

  private def source(config: Config): String = {
    import GlobalSections.{ClassInfo, GlobalObjects, Strings}
    val sections = Seq(
      "globals" -> GlobalObjects,
      "classinfo" -> ClassInfo,
      "strings" -> Strings
    )
    val declarations = sections.map {
      case (id, section) =>
        s"""SN_EXT_SECTION($id, "${section.name}", "${section.machOName}")"""
    }
    val entries = sections.map {
      case (id, _) => s"    {${id}_start, ${id}_end},"
    }
    s"""|#include <stdint.h>
        |
        |#if defined(__APPLE__)
        |#define SN_EXT_SECTION(id, elf, mach)                                        \\
        |    extern uintptr_t id##_start[] __asm("section$$start$$__DATA$$" mach);     \\
        |    extern uintptr_t id##_end[] __asm("section$$end$$__DATA$$" mach);         \\
        |    __attribute__((section("__DATA," mach), used,                           \\
        |                   aligned(sizeof(uintptr_t)))) static volatile uintptr_t  \\
        |        id##_anchor = 0;
        |#else
        |#define SN_EXT_SECTION(id, elf, mach)                                        \\
        |    extern uintptr_t id##_start[] __asm("__start_" elf)                     \\
        |        __attribute__((visibility("hidden")));                              \\
        |    extern uintptr_t id##_end[] __asm("__stop_" elf)                        \\
        |        __attribute__((visibility("hidden")));                              \\
        |    __attribute__((section(elf), used,                                      \\
        |                   aligned(sizeof(uintptr_t)))) static volatile uintptr_t  \\
        |        id##_anchor = 0;
        |#endif
        |
        |${declarations.mkString("\n")}
        |
        |typedef struct {
        |    void *start;
        |    void *end;
        |} SNStaticSection;
        |
        |__attribute__((visibility("hidden"))) const SNStaticSection
        |    __scalanative_ext_sections[${sections.size}] = {
        |${entries.mkString("\n")}
        |};
        |""".stripMargin
  }

  /** Compiles the sections' bounds for the extension `config` links and returns
   *  the object file.
   */
  def compile(config: Config): Path = {
    val dir = Files.createDirectories(config.workDir.resolve("live"))
    val src = dir.resolve("ext_sections.c")
    Files.write(src, source(config).getBytes(UTF_8))
    val obj = dir.resolve("ext_sections.o")
    val command =
      Seq(config.clang.abs, "-c", src.abs, "-o", obj.abs, "-fPIC", "-O2") ++
        config.compilerConfig.targetTriple.toSeq.flatMap(t => Seq("-target", t))
    config.logger.running(command)
    val result = Processes.run(
      command,
      config.workDir.toFile(),
      Logger.toProcessLogger(config.logger)
    )
    if (result != 0) throw new BuildException(s"Failed to compile $src")
    obj
  }
}

package scala.scalanative
package linker

import java.nio.file.{Files, Path}
import java.util.zip.{ZipEntry, ZipOutputStream}

import org.junit.Assert._
import org.junit.Test

import scala.scalanative.build.Logger
import scala.scalanative.util.Scope

class ClassPathCacheTest {

  private def writeJar(jar: Path, entries: String*): Unit = {
    val out = new ZipOutputStream(Files.newOutputStream(jar))
    try
      entries.foreach { name =>
        out.putNextEntry(new ZipEntry(name))
        out.write(name.getBytes("UTF-8"))
        out.closeEntry()
      }
    finally out.close()
  }

  // Each jar is opened in a private file system (upstream's #5250), which a
  // replaced or closed entry closes with its scope
  @Test def aChangedJarReplacesItsEntry(): Unit = {
    val jar = Files.createTempDirectory("classpath-cache").resolve("a.jar")
    def lookup() = ClassPathCache.lookup(jar, Logger.nullLogger).get
    writeJar(jar, "a.txt")
    Scope { implicit scope =>
      ClassPathCache.enable(scope)
      val first = lookup()
      assertSame(first, lookup())

      writeJar(jar, "a.txt", "b.txt")
      val second = lookup()
      assertNotSame(first, second)
      assertSame(second, lookup())
    }
    // The scope closed the jars it kept, and the cache is off
    assertEquals(None, ClassPathCache.lookup(jar, Logger.nullLogger))
  }
}

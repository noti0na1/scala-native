package scala.scalanative
package io

import java.nio.file.{Files, Path}
import java.util.zip.{ZipEntry, ZipOutputStream}

import org.junit.Assert._
import org.junit.Test

import scala.scalanative.util.Scope

class VirtualDirectoryTest {

  private def withJar(f: Path => Unit): Unit = {
    val dir = Files.createTempDirectory("virtual-directory")
    val jar = dir.resolve("a.jar")
    try {
      val out = new ZipOutputStream(Files.newOutputStream(jar))
      try {
        out.putNextEntry(new ZipEntry("a.txt"))
        out.write("hello".getBytes("UTF-8"))
        out.closeEntry()
      } finally out.close()
      f(jar)
    } finally {
      Files.deleteIfExists(jar)
      Files.deleteIfExists(dir)
    }
  }

  private def contents(dir: VirtualDirectory): String = {
    val buffer = dir.read(dir.files.head)
    val bytes = new Array[Byte](buffer.remaining())
    buffer.get(bytes)
    new String(bytes, "UTF-8")
  }

  @Test def jarOpenedAgainInAnInnerScope(): Unit = withJar { jar =>
    Scope { outer =>
      val dir = VirtualDirectory.jar(jar)(outer)
      Scope { inner =>
        assertEquals("hello", contents(VirtualDirectory.jar(jar)(inner)))
      }
      assertEquals("hello", contents(dir))
    }
  }

  @Test def jarWhoseFirstScopeClosesFirst(): Unit = withJar { jar =>
    val first = Scope.unsafe()
    val second = Scope.unsafe()
    VirtualDirectory.jar(jar)(first)
    val dir = VirtualDirectory.jar(jar)(second)
    first.close()
    try assertEquals("hello", contents(dir))
    finally second.close()
  }
}

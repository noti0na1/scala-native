package scala.scalanative
package codegen

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path, Paths}

import org.junit.Assert._
import org.junit.Test

import scala.scalanative.io.VirtualDirectory

/** `llvm.IRWriter` writes text as `Files.newBufferedWriter` with UTF-8 does,
 *  and unpaired surrogates as `?`.
 */
class IRWriterTest {

  private def written(text: String): (Path, Array[Byte]) = {
    val dir = Files.createTempDirectory("irwriter")
    val path = llvm.IRWriter.write(
      VirtualDirectory.local(dir),
      Paths.get("out.ll")
    ) { w =>
      // Each write method the code generator uses
      val (a, b, c) = (text.length / 4, text.length / 2, 3 * text.length / 4)
      w.append(text.substring(0, a))
      w.write(text, a, b - a)
      w.write(text.substring(b, c).toCharArray, 0, c - b)
      w.append(text.charAt(c))
      w.write(text.charAt(c + 1).toInt)
      w.append(new java.lang.StringBuilder(text.substring(c + 2)))
    }
    (dir, Files.readAllBytes(path))
  }

  @Test def writesTheSameBytesAsABufferedWriter(): Unit = {
    val text = "define i32 @\"_SM3fooD\"() {\n" + ("x" * 70000) +
      " é ∀ 😀 end 😁\n"
    val (dir, bytes) = written(text)
    val expected = dir.resolve("expected.ll")
    val writer = Files.newBufferedWriter(expected, UTF_8)
    try writer.write(text)
    finally writer.close()
    assertArrayEquals(Files.readAllBytes(expected), bytes)
  }

  @Test def writesLoneSurrogatesAsQuestionMarks(): Unit = {
    val (_, bytes) = written("0123456789ab \uD800 cd \uDC00 efghij")
    assertEquals(
      "0123456789ab ? cd ? efghij",
      new String(bytes, UTF_8)
    )
  }
}

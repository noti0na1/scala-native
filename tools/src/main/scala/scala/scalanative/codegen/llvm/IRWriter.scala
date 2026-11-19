package scala.scalanative
package codegen
package llvm

import java.nio.ByteBuffer
import java.nio.channels.FileChannel
import java.nio.file.{Files, Path, StandardOpenOption}

import scala.scalanative.io.VirtualDirectory

/** Writes LLVM IR files. The code generator appends many short strings, and
 *  this writer encodes them to UTF-8 directly into one buffer. A buffered
 *  writer over a charset encoder copies each string into arrays of its own (in
 *  Scala Native's javalib, a new array for every string); in a natively linked
 *  toolchain, that copying takes longer than generating the IR.
 */
private[scalanative] object IRWriter {

  /** Writes `path` in `dir` with what `fn` writes to its writer. */
  def write(dir: VirtualDirectory, path: Path)(
      fn: java.io.Writer => Unit
  ): Path = {
    val fullPath = dir.path.resolve(path)
    Files.createDirectories(fullPath.getParent())
    val channel = FileChannel.open(
      fullPath,
      StandardOpenOption.CREATE,
      StandardOpenOption.WRITE,
      StandardOpenOption.TRUNCATE_EXISTING
    )
    val writer = new Utf8Writer(channel)
    try fn(writer)
    finally writer.close()
    fullPath
  }

  /** A writer of UTF-8 to a channel, through a buffer of its own. It writes an
   *  unpaired surrogate as `?`.
   */
  final class Utf8Writer(channel: FileChannel) extends java.io.Writer {
    private val buffer = new Array[Byte](1 << 16)
    private var pos = 0
    private var high: Char = 0 // a high surrogate waiting for its pair

    private def drain(): Unit = {
      val bb = ByteBuffer.wrap(buffer, 0, pos)
      while (bb.hasRemaining()) channel.write(bb)
      pos = 0
    }

    @inline private def byte(b: Int): Unit = {
      if (pos == buffer.length) drain()
      buffer(pos) = b.toByte
      pos += 1
    }

    private def char(c: Char): Unit =
      if (high != 0) {
        val h = high
        high = 0
        if (Character.isLowSurrogate(c)) codePoint(Character.toCodePoint(h, c))
        else {
          byte('?')
          char(c)
        }
      } else if (c < 0x80) byte(c)
      else if (c < 0x800) {
        byte(0xc0 | (c >> 6))
        byte(0x80 | (c & 0x3f))
      } else if (Character.isHighSurrogate(c)) high = c
      else if (Character.isLowSurrogate(c)) byte('?')
      else {
        byte(0xe0 | (c >> 12))
        byte(0x80 | ((c >> 6) & 0x3f))
        byte(0x80 | (c & 0x3f))
      }

    private def codePoint(cp: Int): Unit = {
      byte(0xf0 | (cp >> 18))
      byte(0x80 | ((cp >> 12) & 0x3f))
      byte(0x80 | ((cp >> 6) & 0x3f))
      byte(0x80 | (cp & 0x3f))
    }

    override def write(c: Int): Unit = char(c.toChar)

    override def write(cbuf: Array[Char], off: Int, len: Int): Unit = {
      var i = off
      val end = off + len
      while (i < end) {
        char(cbuf(i))
        i += 1
      }
    }

    override def write(str: String): Unit = write(str, 0, str.length)

    override def write(str: String, off: Int, len: Int): Unit = {
      var i = off
      val end = off + len
      while (i < end) {
        char(str.charAt(i))
        i += 1
      }
    }

    override def append(csq: CharSequence): java.io.Writer = {
      if (csq == null) write("null")
      else {
        var i = 0
        val end = csq.length()
        while (i < end) {
          char(csq.charAt(i))
          i += 1
        }
      }
      this
    }

    override def append(c: Char): java.io.Writer = {
      char(c)
      this
    }

    override def flush(): Unit = drain()

    override def close(): Unit = {
      if (high != 0) {
        high = 0
        byte('?')
      }
      drain()
      channel.close()
    }
  }
}

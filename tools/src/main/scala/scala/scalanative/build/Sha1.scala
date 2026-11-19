package scala.scalanative
package build

/** SHA-1 as specified by RFC 3174, for build cache keys.
 *
 *  The toolchain also runs as a native program, where `java.security` is not
 *  available, so it carries its own implementation.
 */
private[scalanative] final class Sha1 {
  private var h0 = 0x67452301
  private var h1 = 0xefcdab89
  private var h2 = 0x98badcfe
  private var h3 = 0x10325476
  private var h4 = 0xc3d2e1f0

  private val block = new Array[Byte](64)
  private var blockLength = 0
  private var messageLength = 0L
  private val w = new Array[Int](80)

  def update(bytes: Array[Byte]): Unit = update(bytes, 0, bytes.length)

  def update(bytes: Array[Byte], offset: Int, length: Int): Unit = {
    var i = offset
    val end = offset + length
    messageLength += length
    while (i < end) {
      val n = math.min(64 - blockLength, end - i)
      System.arraycopy(bytes, i, block, blockLength, n)
      blockLength += n
      i += n
      if (blockLength == 64) {
        processBlock()
        blockLength = 0
      }
    }
  }

  /** Completes the hash and returns the 20-byte digest. */
  def digest(): Array[Byte] = {
    val bitLength = messageLength * 8
    block(blockLength) = 0x80.toByte
    blockLength += 1
    if (blockLength > 56) {
      java.util.Arrays.fill(block, blockLength, 64, 0.toByte)
      processBlock()
      blockLength = 0
    }
    java.util.Arrays.fill(block, blockLength, 56, 0.toByte)
    var i = 0
    while (i < 8) {
      block(56 + i) = (bitLength >>> (56 - 8 * i)).toByte
      i += 1
    }
    processBlock()

    val out = new Array[Byte](20)
    Array(h0, h1, h2, h3, h4).zipWithIndex.foreach {
      case (h, j) =>
        out(4 * j) = (h >>> 24).toByte
        out(4 * j + 1) = (h >>> 16).toByte
        out(4 * j + 2) = (h >>> 8).toByte
        out(4 * j + 3) = h.toByte
    }
    out
  }

  private def processBlock(): Unit = {
    var t = 0
    while (t < 16) {
      w(t) = ((block(4 * t) & 0xff) << 24) |
        ((block(4 * t + 1) & 0xff) << 16) |
        ((block(4 * t + 2) & 0xff) << 8) |
        (block(4 * t + 3) & 0xff)
      t += 1
    }
    while (t < 80) {
      w(t) = Integer.rotateLeft(w(t - 3) ^ w(t - 8) ^ w(t - 14) ^ w(t - 16), 1)
      t += 1
    }

    var a = h0
    var b = h1
    var c = h2
    var d = h3
    var e = h4
    t = 0
    while (t < 80) {
      val f =
        if (t < 20) ((b & c) | (~b & d)) + 0x5a827999
        else if (t < 40) (b ^ c ^ d) + 0x6ed9eba1
        else if (t < 60) ((b & c) | (b & d) | (c & d)) + 0x8f1bbcdc
        else (b ^ c ^ d) + 0xca62c1d6
      val temp = Integer.rotateLeft(a, 5) + f + e + w(t)
      e = d
      d = c
      c = Integer.rotateLeft(b, 30)
      b = a
      a = temp
      t += 1
    }
    h0 += a
    h1 += b
    h2 += c
    h3 += d
    h4 += e
  }
}

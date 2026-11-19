package scala.scalanative.build

import java.nio.charset.StandardCharsets.US_ASCII

import org.junit.Assert._
import org.junit.Test

class Sha1Test {

  private def hex(bytes: Array[Byte]): String =
    bytes.map(b => f"${b & 0xff}%02x").mkString

  private def sha1(s: String): String = {
    val digest = new Sha1()
    digest.update(s.getBytes(US_ASCII))
    hex(digest.digest())
  }

  @Test def rfc3174Vectors(): Unit = {
    assertEquals("da39a3ee5e6b4b0d3255bfef95601890afd80709", sha1(""))
    assertEquals("a9993e364706816aba3e25717850c26c9cd0d89d", sha1("abc"))
    assertEquals(
      "84983e441c3bd26ebaae4aa1f95129e5e54670f1",
      sha1("abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")
    )
  }

  @Test def paddingAtBlockBoundaries(): Unit = {
    val expected = List(
      55 -> "c1c8bbdc22796e28c0e15163d20899b65621d65a",
      56 -> "c2db330f6083854c99d4b5bfb6e8f29f201be699",
      63 -> "03f09f5b158a7a8cdad920bddc29b81c18a551f5",
      64 -> "0098ba824b5c16427bd7a1122a5a442a25ec644d",
      65 -> "11655326c708d70319be2610e8a57d9a5b959d3b",
      119 -> "ee971065aaa017e0632a8ca6c77bb3bf8b1dfc56",
      120 -> "f34c1488385346a55709ba056ddd08280dd4c6d6",
      128 -> "ad5b3fdbcb526778c2839d2f151ea753995e26a0"
    )
    expected.foreach {
      case (length, hash) =>
        assertEquals(s"length $length", hash, sha1("a" * length))
    }
  }

  @Test def incrementalUpdatesMatchOneShot(): Unit = {
    val digest = new Sha1()
    val chunk = ("a" * 1000).getBytes(US_ASCII)
    // Odd offsets and lengths cross block boundaries in every position
    for (_ <- 0 until 1000) {
      digest.update(chunk, 0, 7)
      digest.update(chunk, 7, 993)
    }
    assertEquals(
      "34aa973cd4c4daa4f61eeb2bdbad27316534016f",
      hex(digest.digest())
    )
  }
}

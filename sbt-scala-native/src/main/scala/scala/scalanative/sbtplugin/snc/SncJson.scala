package scala.scalanative.sbtplugin.snc

import scala.collection.mutable

/** A small JSON reader for `snc`'s `--about` output and events. Values are
 *  `Map[String, Any]`, `Vector[Any]`, `String`, `BigDecimal`, `Boolean` and
 *  `null`.
 */
private[scalanative] object SncJson {

  def parse(text: String): Any = {
    val reader = new Reader(text)
    val value = reader.value()
    reader.skipSpaces()
    if (!reader.atEnd) reader.fail("trailing characters")
    value
  }

  /** Field access on parsed objects. */
  implicit class Fields(private val value: Any) extends AnyVal {
    def field(name: String): Any = value match {
      case m: Map[_, _] =>
        m.asInstanceOf[Map[String, Any]].getOrElse(name, null)
      case _ => null
    }
    def string(name: String): Option[String] = field(name) match {
      case s: String => Some(s)
      case _         => None
    }
    def int(name: String): Option[Int] = field(name) match {
      case n: BigDecimal => Some(n.toInt)
      case _             => None
    }
    def array(name: String): Vector[Any] = field(name) match {
      case v: Vector[_] => v.asInstanceOf[Vector[Any]]
      case _            => Vector.empty
    }
  }

  private final class Reader(text: String) {
    private var i = 0

    def atEnd: Boolean = i >= text.length

    def fail(message: String): Nothing =
      throw new IllegalArgumentException(s"Invalid JSON at $i: $message")

    def skipSpaces(): Unit =
      while (!atEnd && Character.isWhitespace(text.charAt(i))) i += 1

    def value(): Any = {
      skipSpaces()
      if (atEnd) fail("unexpected end")
      text.charAt(i) match {
        case '{'                                   => obj()
        case '['                                   => arr()
        case '"'                                   => str()
        case 't'                                   => literal("true", true)
        case 'f'                                   => literal("false", false)
        case 'n'                                   => literal("null", null)
        case c if c == '-' || Character.isDigit(c) => num()
        case c                                     => fail(s"unexpected '$c'")
      }
    }

    private def literal(word: String, result: Any): Any =
      if (text.startsWith(word, i)) { i += word.length; result }
      else fail(s"expected $word")

    private def expect(c: Char): Unit = {
      skipSpaces()
      if (atEnd || text.charAt(i) != c) fail(s"expected '$c'")
      i += 1
    }

    private def peek(c: Char): Boolean = {
      skipSpaces()
      !atEnd && text.charAt(i) == c
    }

    private def obj(): Map[String, Any] = {
      expect('{')
      val fields = mutable.LinkedHashMap.empty[String, Any]
      if (peek('}')) i += 1
      else {
        var more = true
        while (more) {
          skipSpaces()
          val key = str()
          expect(':')
          fields(key) = value()
          if (peek(',')) i += 1
          else { expect('}'); more = false }
        }
      }
      fields.toMap
    }

    private def arr(): Vector[Any] = {
      expect('[')
      val values = Vector.newBuilder[Any]
      if (peek(']')) i += 1
      else {
        var more = true
        while (more) {
          values += value()
          if (peek(',')) i += 1
          else { expect(']'); more = false }
        }
      }
      values.result()
    }

    private def str(): String = {
      expect('"')
      val sb = new StringBuilder
      var done = false
      while (!done) {
        if (atEnd) fail("unterminated string")
        val c = text.charAt(i)
        i += 1
        c match {
          case '"'  => done = true
          case '\\' =>
            if (atEnd) fail("unterminated escape")
            val e = text.charAt(i)
            i += 1
            e match {
              case 'n' => sb += '\n'
              case 'r' => sb += '\r'
              case 't' => sb += '\t'
              case 'b' => sb += '\b'
              case 'f' => sb += '\f'
              case 'u' =>
                if (i + 4 > text.length) fail("short unicode escape")
                sb += Integer.parseInt(text.substring(i, i + 4), 16).toChar
                i += 4
              case other => sb += other
            }
          case other => sb += other
        }
      }
      sb.toString
    }

    private def num(): BigDecimal = {
      val start = i
      while (!atEnd && "+-0123456789.eE".indexOf(text.charAt(i).toInt) >= 0)
        i += 1
      BigDecimal(text.substring(start, i))
    }
  }
}

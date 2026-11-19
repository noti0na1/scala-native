package scala.scalanative
package compiler

import org.junit.Assert._
import org.junit.Test

import scala.scalanative.linker.compileAndLoad
import scala.scalanative.nir._

/** Every definition of a class reaches its NIR file. The plugin writes one file
 *  per top-level name, with all the definitions that belong to it. In Scala
 *  3.10.1-RC1 and RC2 (scala/scala3#26688, until scala/scala3#27337), two
 *  `PlainFile`s for the same path were not equal, so grouping the definitions
 *  by output file wrote each of them on its own and each write replaced the
 *  last: classes lost their fields, constructors and methods.
 */
class DefinitionsPerFileTest {

  /** What is defined for `owner`: `owner` for the class or module itself, and
   *  the names of its fields, constructors (`<init>`) and methods.
   */
  private def definedFor(defns: Seq[Defn], owner: String): Set[String] =
    defns.iterator
      .filter(_.name.top == Global.Top(owner))
      .map(_.name match {
        case Global.Member(_, sig) =>
          sig.unmangled match {
            case Sig.Field(id, _)     => id
            case Sig.Ctor(_)          => "<init>"
            case Sig.Method(id, _, _) => id
            case other                => other.toString
          }
        case _ => owner
      })
      .toSet

  @Test def classAndModuleKeepTheirMembers(): Unit = {
    compileAndLoad(
      "Point.scala" ->
        """|class Point(val x: Int, val y: Int) {
           |  def norm: Int = x * x + y * y
           |  def shifted(dx: Int): Point = new Point(x + dx, y)
           |}
           |object Point {
           |  def origin: Point = new Point(0, 0)
           |}
           |""".stripMargin
    ) { defns =>
      val point = definedFor(defns, "Point")
      for (name <- Seq("Point", "x", "y", "<init>", "norm", "shifted"))
        assertTrue(s"Point lacks $name: $point", point.contains(name))
      val module = definedFor(defns, "Point$")
      for (name <- Seq("Point$", "<init>", "origin"))
        assertTrue(s"Point$$ lacks $name: $module", module.contains(name))
    }
  }
}

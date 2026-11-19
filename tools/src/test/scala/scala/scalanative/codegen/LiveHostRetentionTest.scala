package scala.scalanative
package codegen

import org.junit.Assert._
import org.junit.Test

import scala.scalanative.build.{LiveExecution, Mode}
import scala.scalanative.live.Manifest

/** A host's manifest describes its classes as they were before optimization,
 *  which is what its optimized code assumes: a class whose allocations the
 *  optimizer removed is still allocated, with its fields, and a class that only
 *  has methods other classes inherit is not.
 */
class LiveHostRetentionTest extends CodeGenSpec {

  private val source = Map(
    "Test.scala" ->
      """
      class Base { def n(): Int = 1 }
      final class Sub extends Base
      final class Point(val x: Int, val y: Int) { def sum: Int = x + y }
      object Test {
        def main(args: Array[String]): Unit = {
          val b: Base = if (args.length > 3) new Sub else null
          println(new Point(args.length, 2).sum)
          if (b != null) println(b.n())
        }
      }
      """
  )

  @Test def theManifestHasWhatWasAllocatedBeforeOptimization(): Unit =
    codegen(
      "Test",
      source,
      _.withMode(Mode.releaseFast)
        .withLiveExecution(LiveExecution.Host(open = Nil, callable = Nil))
    ) { (config, analysis, _) =>
      val point = nir.Global.Top("Point")
      assertFalse(
        "the optimizer removes Point's allocation",
        analysis.defns.exists {
          case defn: nir.Defn.Define =>
            defn.insts.exists {
              case nir.Inst.Let(_, nir.Op.Classalloc(`point`, _), _) => true
              case _                                                 => false
            }
          case _ => false
        }
      )
      val manifest = Manifest.read(LiveBoundary.manifestPath(config))
      val entry = manifest.classByName("Point")
      assertTrue("Point is allocated", entry.isAllocated)
      assertEquals(2, entry.fields.size)
      assertTrue("Sub is allocated", manifest.classByName("Sub").isAllocated)
      assertFalse(
        "Base is never allocated",
        manifest.classByName("Base").isAllocated
      )
    }
}

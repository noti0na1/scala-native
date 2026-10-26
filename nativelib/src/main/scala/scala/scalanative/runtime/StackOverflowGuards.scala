package scala.scalanative
package runtime

import scala.scalanative.unsafe._

// Not in the runtime package object: its @extern annotation, from unsafe's
// package object, causes a cyclic reference on Scala 2.12
@extern private[runtime] object StackOverflowGuards {
  @name("scalanative_StackOverflowGuards_size")
  def size: Int = extern

  @name("scalanative_StackOverflowGuards_setup")
  def setup(isMainThread: Boolean): Unit = extern

  @name("scalanative_StackOverflowGuards_reset")
  def reset(): Unit = extern

  @name("scalanative_StackOverflowGuards_close")
  def close(): Unit = extern
}

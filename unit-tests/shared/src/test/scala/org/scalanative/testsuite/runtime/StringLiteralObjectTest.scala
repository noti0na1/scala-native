package org.scalanative.testsuite.runtime

import java.util.concurrent.atomic.AtomicInteger

import org.junit.Assert._
import org.junit.Test

// A string literal is an object like any other: its hash can be stored, and
// it can be locked on, also by threads that contend for it
class StringLiteralObjectTest {
  import StringLiteralObjectTest._

  @Test def hashOfALiteralWhoseHashIsZero(): Unit = {
    assertEquals(0, literal("\u0000").hashCode)
    assertEquals(0, literal("\u0000").hashCode)
  }

  @Test def lockingOnALiteral(): Unit = {
    val lock = literal("a lock of a literal")
    val count = new AtomicInteger(0)
    lock.synchronized { count.incrementAndGet() }
    val thread =
      new Thread(() => lock.synchronized { count.incrementAndGet(); () })
    lock.synchronized {
      thread.start()
      // The thread waits for the lock, which inflates it
      Thread.sleep(20)
      count.incrementAndGet()
    }
    thread.join()
    assertEquals(3, count.get())
  }
}

object StringLiteralObjectTest {
  @noinline def literal(s: String): AnyRef = s
}

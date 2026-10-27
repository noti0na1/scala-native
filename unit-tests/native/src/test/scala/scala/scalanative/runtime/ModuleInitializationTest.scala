package scala.scalanative.runtime

import java.lang.Runtime
import java.util.concurrent.{ConcurrentLinkedQueue, CountDownLatch, TimeUnit}

import org.junit.Assert._
import org.junit.Test

import scala.scalanative.junit.utils.AssumesHelper

object TestModule {
  val slowInitField = {
    assertNotNull(this)
    assertNotNull(this.getClass())
    Thread.sleep(1000)
    42
  }
}

// Initialized by one thread while others wait for it, and read again by code
// that runs while they wait (ModuleReadingThread)
object SlowlyInitializedModule {
  val value: Int = {
    ModuleInitializationTest.slowInitializerStarted.countDown()
    Thread.sleep(500)
    42
  }
}

// A thread whose interrupt status reads SlowlyInitializedModule: the backoff
// sleep in a monitor's slow path reads it
class ModuleReadingThread(body: Runnable) extends Thread(body) {
  override def isInterrupted(): Boolean = {
    if (SlowlyInitializedModule.value != 42)
      throw new AssertionError("read a partly initialized module")
    super.isInterrupted()
  }
}

object ModuleInitializationTest {
  val slowInitializerStarted = new CountDownLatch(1)
}

class ModuleInitializationTest {
  @Test def initializeFromMultipleThreads(): Unit = {
    AssumesHelper.assumeMultithreadingIsEnabled()

    val latch = new java.util.concurrent.CountDownLatch(1)
    val threads = Seq.fill(4) {
      new Thread(() => {
        latch.await()
        assertEquals(42, TestModule.slowInitField)
      })
    }
    threads.foreach(_.start())
    Thread.sleep(100)
    latch.countDown()
    threads.foreach(_.join())
    assertEquals(42, TestModule.slowInitField)
  }

  @Test def waitingForAModuleReentersNothing(): Unit = {
    AssumesHelper.assumeMultithreadingIsEnabled()

    val failures = new ConcurrentLinkedQueue[java.lang.Throwable]()
    val owner = new Thread(() => {
      try assertEquals(42, SlowlyInitializedModule.value)
      catch { case t: java.lang.Throwable => failures.add(t); () }
    })
    owner.start()
    assertTrue(
      "the initializer did not start",
      ModuleInitializationTest.slowInitializerStarted
        .await(10, TimeUnit.SECONDS)
    )
    val waiters = Seq.fill(4) {
      new ModuleReadingThread(() => {
        try assertEquals(42, SlowlyInitializedModule.value)
        catch { case t: java.lang.Throwable => failures.add(t); () }
      })
    }
    waiters.foreach(_.start())
    waiters.foreach(_.join(10000))
    owner.join(10000)
    assertTrue(s"failures: $failures", failures.isEmpty())
    assertFalse(waiters.exists(_.isAlive()))
  }
}

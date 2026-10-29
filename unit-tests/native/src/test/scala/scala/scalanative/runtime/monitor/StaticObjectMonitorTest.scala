package scala.scalanative.runtime.monitor

import java.lang.ref.WeakReference

import org.junit.Assert._
import org.junit.Assume._
import org.junit.Test

import scala.scalanative.meta.LinktimeInfo
import scala.scalanative.runtime.Intrinsics._
import scala.scalanative.runtime.MemoryLayout

// Class objects, the instances of constant modules and string literals are
// outside the heap. Once the monitor of such an object is inflated, the
// object's lock word alone keeps the monitor
class StaticObjectMonitorTest {
  import StaticObjectMonitorTest._

  @Test def inflatedMonitorOfAClassSurvivesCollections(): Unit =
    checkMonitorSurvives(classOf[NeverInstantiated])

  @Test def inflatedMonitorOfAConstantModuleSurvivesCollections(): Unit =
    checkMonitorSurvives(StatelessModule)

  @Test def inflatedMonitorOfAStringLiteralSurvivesCollections(): Unit =
    checkMonitorSurvives("a string literal locked by StaticObjectMonitorTest")
}

object StaticObjectMonitorTest {
  private class NeverInstantiated

  private object StatelessModule

  private def checkMonitorSurvives(obj: AnyRef): Unit = {
    assumeTrue(
      "inflated monitors, with a collector",
      LinktimeInfo.isMultithreadingEnabled && !LinktimeInfo.gc.isNone
    )
    // On a thread that ends before the collections, so that neither its
    // stack nor its registers keep the monitor
    var monitor: WeakReference[ObjectMonitor] = null
    val thread = new Thread(() => {
      inflate(obj)
      monitor = monitorOf(obj)
    })
    thread.start()
    thread.join()
    for (_ <- 0 until 5) {
      Seq.fill(1000)(new Array[Byte](64))
      System.gc()
    }
    assertNotNull("the monitor was collected", monitor.get())
    assertSame(monitor.get(), currentMonitor(obj))
    obj.synchronized(obj.notifyAll())
  }

  private def lockWord(obj: AnyRef): LockWord =
    loadRawPtr(
      elemRawPtr(
        castObjectToRawPtr(obj),
        castIntToRawSize(MemoryLayout.Object.LockWordOffset)
      )
    )

  private def currentMonitor(obj: AnyRef): ObjectMonitor = {
    val word = lockWord(obj)
    assertTrue("the monitor is inflated", word.isInflated)
    word.getObjectMonitor
  }

  // Waiting inflates the monitor
  @noinline private def inflate(obj: AnyRef): Unit =
    obj.synchronized(obj.wait(1))

  @noinline private def monitorOf(obj: AnyRef): WeakReference[ObjectMonitor] =
    new WeakReference(currentMonitor(obj))
}

package scala.scalanative.runtime.gc

import org.junit.Assert._
import org.junit.Assume._
import org.junit.Test

import scala.scalanative.meta.LinktimeInfo
import scala.scalanative.unsafe._

class GCThreadSignalsTest {
  import GCThreadSignalsTest._

  // The semaphore wait of the GC threads, as a thread that blocks no signal
  // sees it
  @Test def semaphoreWaitOutlastsSignals(): Unit = {
    assumeTrue(
      "POSIX signals on macOS and Linux",
      LinktimeInfo.isMac || LinktimeInfo.isLinux
    )
    assertEquals(1, Signals.semaphoreWaitWithSignals())
  }
}

object GCThreadSignalsTest {
  @extern object Signals {
    @blocking
    @name("scalanative_test_semaphore_wait_with_signals")
    def semaphoreWaitWithSignals(): CInt = extern
  }
}

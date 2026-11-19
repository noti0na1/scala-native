import scala.scalanative.runtime.Extension

import api._

/** A host implementation of the open types. run.sh drops the lines marked OPEN
 *  for the "closed" variant, where the host has no implementation of `Plugin`
 *  or subclass of `Base` of its own: then the phantoms are the only other
 *  targets of those calls.
 */
final class HostPlugin extends Base with Plugin { // OPEN
  def name: String = "host" // OPEN
  def run(x: Int): Int = x * 2 + 7 // OPEN
  def fail(): Unit = throw new HostError("host plugin failed") // OPEN
  def checksum(): Long = 42L // OPEN
} // OPEN

/** The host: loads the extension and uses it only through the open types.
 *
 *  usage: host <extension> <threads> [<copy of the extension>]
 */
object Host {
  private var junk: AnyRef = null

  private def churn(rounds: Int): Unit = {
    var r = 0
    while (r < rounds) {
      var i = 0
      while (i < 10000) { junk = new Array[Long](64); i += 1 }
      r += 1
    }
    System.gc()
  }

  def main(args: Array[String]): Unit = {
    // Never true: it links the marker copy of the extension into the host
    if (args(0) == "--never") { // OPEN
      Registry.register(new hostcopy.Greeter(0)) // OPEN
      println(hostcopy.Main.entry()) // OPEN
    } // OPEN
    Registry.register(new HostPlugin) // OPEN

    val t0 = System.nanoTime()
    val ext = Extension.load(args(0))
    val t1 = System.nanoTime()
    println(f"load: ${(t1 - t0) / 1e6}%.2f ms, ${ext.classCount} classes")

    val main = ext.module("ext.Main$").asInstanceOf[Entry]
    println(
      s"extension entry (1 = it caught the host's exception): ${main.entry()}"
    )

    val plugins = Registry.all
    val p = plugins.head
    println(
      s"plugins: ${plugins.size}; head: ${p.name}, " +
        s"run(1) = ${p.run(1)} (9 from the extension's code, 1009 from the host's copy)"
    )
    println(
      s"host type tests: Base ${p.isInstanceOf[Base]}, Plugin ${p.isInstanceOf[Plugin]}, " +
        s"HostError ${(p: Any).isInstanceOf[HostError]}, Entry ${(p: Any).isInstanceOf[Entry]}"
    )
    println(s"host getClass: ${p.getClass.getName}")
    println(s"virtual call through Base: ${p.asInstanceOf[Base].describe()}")
    try p.fail()
    catch {
      case e: HostError =>
        println(s"host caught ${e.getClass.getName}: ${e.getMessage}")
        println(
          "  frames: " + e.getStackTrace
            .take(6)
            .map(f => f.getClassName + "." + f.getMethodName)
            .mkString(", ")
        )
    }
    println(s"extension probes a host error: ${main.probe(new HostError("x"))}")
    println(s"extension probes its own object: ${main.probe(p)}")
    println(s"extension probes a host string: ${main.probe("s")}")

    val sums0 = plugins.map(_.checksum()).sum
    val mod0 = main.verify()
    val threads = args(1).toInt
    val t2 = System.nanoTime()
    if (!scala.scalanative.meta.LinktimeInfo.isMultithreadingEnabled) churn(100)
    else if (threads <= 1) churn(100)
    else {
      val ts = (1 to threads).map { k =>
        val t = new Thread(() => {
          var r = 0
          while (r < 20) {
            churn(5)
            plugins.foreach(q =>
              if (q.name != "host" && q.run(k) != 2 * k + 7) {
                println("bad run")
                sys.exit(3)
              }
            )
            r += 1
          }
        })
        t.start(); t
      }
      ts.foreach(_.join())
    }
    val t3 = System.nanoTime()
    val sums1 = plugins.map(_.checksum()).sum
    val mod1 = main.verify()
    println(
      s"after churn on $threads thread(s) (${(t3 - t2) / 1000000} ms): " +
        s"plugin checksums ${if (sums0 == sums1) "same"
          else s"DIFFER $sums0 $sums1"}, " +
        s"extension module ${if (mod0 == mod1) "same"
          else s"DIFFERS $mod0 $mod1"}"
    )
    // Loading again gives the same extension; a second copy is refused
    println(s"reload: ${Extension.load(args(0)).classCount} classes")
    if (args.length > 2) {
      try {
        Extension.load(args(2))
        println("second copy: loaded (wrong)")
      } catch {
        case e: scala.scalanative.runtime.ExtensionLoadError =>
          println(s"second copy refused: ${e.getMessage}")
      }
    }
  }
}

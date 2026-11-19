import scala.scalanative.runtime.Extension

/** The host of the chained-extension test (chain.sh): it knows none of the
 *  lines. It loads each line's extension so that later lines bind to it, then
 *  calls the line's entry, as the REPL's evaluator does.
 *
 *  usage: host (<extension> <entry module>)...
 */
object Host {
  trait Entry { def run(): String }

  def main(args: Array[String]): Unit = {
    var i = 0
    while (i < args.length) {
      val t0 = System.nanoTime()
      val ext = Extension.load(args(i), global = true)
      val t1 = System.nanoTime()
      val entry = ext.module(args(i + 1)).asInstanceOf[Entry]
      println(f"load ${(t1 - t0) / 1e6}%.1f ms: ${entry.run()}")
      i += 2
    }
  }
}

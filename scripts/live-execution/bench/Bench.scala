// Virtual calls on types a host declares open, against the same program
// linked closed (bench.sh). Times are the best of several runs, in ms.
package bench

abstract class Shape { def area(x: Int): Int }
final class Square extends Shape { def area(x: Int): Int = x * x }
final class Triple extends Shape { def area(x: Int): Int = x * 3 }

abstract class Mono { def f(x: Int): Int }
final class OnlyMono extends Mono { def f(x: Int): Int = x + 1 }

abstract class Poly { def h(x: Int): Int }
final class PolyA extends Poly { def h(x: Int): Int = x + 1 }
final class PolyB extends Poly { def h(x: Int): Int = x + 2 }
final class PolyC extends Poly { def h(x: Int): Int = x + 3 }

trait Op { def apply(x: Int): Int }
final class Inc extends Op { def apply(x: Int): Int = x + 1 }
final class Twice extends Op { def apply(x: Int): Int = x * 2 }

// Never open: the control
abstract class Closed { def g(x: Int): Int }
final class ClosedA extends Closed { def g(x: Int): Int = x ^ 1 }
final class ClosedB extends Closed { def g(x: Int): Int = x ^ 2 }

object Bench {
  final val N = 200000000

  def shapes(xs: Array[Shape]): Int = {
    var s = 0; var i = 0
    while (i < N) { s += xs(i & 1023).area(i); i += 1 }
    s
  }
  def monos(xs: Array[Mono]): Int = {
    var s = 0; var i = 0
    while (i < N) { s += xs(i & 1023).f(i); i += 1 }
    s
  }
  def polys(xs: Array[Poly]): Int = {
    var s = 0; var i = 0
    while (i < N) { s += xs(i & 1023).h(i); i += 1 }
    s
  }
  def ops(xs: Array[Op]): Int = {
    var s = 0; var i = 0
    while (i < N) { s += xs(i & 1023).apply(i); i += 1 }
    s
  }
  def closeds(xs: Array[Closed]): Int = {
    var s = 0; var i = 0
    while (i < N) { s += xs(i & 1023).g(i); i += 1 }
    s
  }

  def time(name: String, runs: Int)(body: => Int): Unit = {
    var best = Long.MaxValue
    var result = 0
    var r = 0
    while (r < runs) {
      val t0 = System.nanoTime()
      result += body
      val t = System.nanoTime() - t0
      if (t < best) best = t
      r += 1
    }
    println(f"$name%-8s ${best / 1e6}%8.1f ms ($result)")
  }

  def main(args: Array[String]): Unit = {
    val runs = if (args.nonEmpty) args(0).toInt else 5
    val s = Array.tabulate[Shape](1024)(i =>
      if (i % 3 == 0) new Square else new Triple
    )
    val m = Array.tabulate[Mono](1024)(_ => new OnlyMono)
    val p = Array.tabulate[Poly](1024)(i =>
      if (i % 3 == 0) new PolyA else if (i % 3 == 1) new PolyB else new PolyC
    )
    val o =
      Array.tabulate[Op](1024)(i => if (i % 3 == 0) new Inc else new Twice)
    val c = Array.tabulate[Closed](1024)(i =>
      if (i % 3 == 0) new ClosedA else new ClosedB
    )
    time("shapes", runs)(shapes(s))
    time("mono", runs)(monos(m))
    time("poly3", runs)(polys(p))
    time("ops", runs)(ops(o))
    time("closed", runs)(closeds(c))
  }
}

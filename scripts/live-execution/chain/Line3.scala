object rs$line$3 {
  import rs$line$1._
  import rs$line$2._
  class C extends B { override def f: Int = super.f * 100 }
  val c = new C
  val all: List[A] = List(a, b, c)
}
object rs$runner$3 extends Host.Entry {
  def run(): String = {
    val l = rs$line$3
    val tests = l.all.map(v =>
      (v.isInstanceOf[rs$line$2.B], (v: Any).isInstanceOf[rs$line$1.T])
    )
    s"line3: ${l.all.map(_.f)} ${l.all.map(_.describe)} $tests bump=${rs$line$1.bump()} " +
      s"sum=${(1 to 1000000).foldLeft(0L)(_ + _)}"
  }
}

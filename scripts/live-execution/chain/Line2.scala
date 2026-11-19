object rs$line$2 {
  import rs$line$1._
  class B extends A with T { override def f: Int = 2; def g: Int = 3 }
  val b = new B
  val x: A = b
  val ps = List(P(1, 2), P(3, 4)).map(p => p.copy(y = p.x * 10))
  val t: T = new T { def g = 42 }
}
object rs$runner$2 extends Host.Entry {
  def run(): String = {
    val l = rs$line$2
    val anyX: Any = l.x
    val anyT: Any = l.t
    s"line2: ${l.x.describe} g=${l.b.g} isA=${anyX.isInstanceOf[rs$line$1.A]} isT=${anyX.isInstanceOf[rs$line$1.T]} " +
      s"tIsT=${anyT.isInstanceOf[rs$line$1.T]} tg=${l.t.g} ps=${l.ps} bump=${rs$line$1.bump()} " +
      s"match=${anyX match {
          case b: l.B => "B"; case _: rs$line$1.A => "A"; case _ => "?"
        }}"
  }
}

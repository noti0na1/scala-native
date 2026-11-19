object rs$line$1 {
  class A { def f: Int = 1; def describe: String = s"A.f=$f" }
  trait T { def g: Int }
  case class P(x: Int, y: Int)
  val a = new A
  var counter = 0
  def bump(): Int = { counter += 1; counter }
}
object rs$runner$1 extends Host.Entry {
  def run(): String = {
    val l = rs$line$1
    s"line1: ${l.a.describe} ${l.bump()} ${List(1, 2, 3).map(_ * 2)} ${l.P(1, 2)}"
  }
}

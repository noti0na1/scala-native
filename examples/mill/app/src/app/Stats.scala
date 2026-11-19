package app

object Stats:
  def mean(xs: List[Int]): Double = xs.sum.toDouble / xs.size

  def median(xs: List[Int]): Int = xs.sorted.apply(xs.size / 2)

  def summary(xs: List[Int]): String =
    s"n=${xs.size} mean=${mean(xs)} median=${median(xs)} max=${xs.max}"

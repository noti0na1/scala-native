package app

import lib.Greeting

object Main:
  def main(args: Array[String]): Unit =
    val who = if args.isEmpty then "Scala Native" else args.mkString(" ")
    val file = os.temp.dir() / "greeting.txt"
    os.write(file, Greeting.greet(who))
    println(os.read(file))
    println(Stats.summary(List(3, 1, 4, 1, 5, 9, 2, 6)))

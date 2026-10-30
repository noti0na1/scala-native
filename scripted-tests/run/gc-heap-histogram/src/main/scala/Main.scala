final class Retained(val a: Long, val b: Long, val c: Long, val d: Long)

object Main {
  // The values that project/HistogramCheck.scala checks
  final val RetainedObjects = 1000
  final val RetainedArrays = 10
  final val Collections = 3

  def main(args: Array[String]): Unit = {
    val objects = Array.tabulate(RetainedObjects)(i => new Retained(i, i, i, i))
    val arrays = Array.fill(RetainedArrays)(new Array[Long](1000))
    for (_ <- 1 to Collections) System.gc()
    // Keeps both alive until the last collection has ended
    if (objects.length + arrays.length == 0) println("unreachable")
  }
}

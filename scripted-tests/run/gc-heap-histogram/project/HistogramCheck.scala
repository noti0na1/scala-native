import sbt.io.IO

import java.io.File
import java.util.concurrent.TimeUnit

object HistogramCheck {
  // The values of Main
  val RetainedObjects = 1000
  val RetainedArrays = 10
  val Collections = 3

  case class Line(
      collection: Int,
      bytes: Long,
      objects: Long,
      size: Int,
      name: String
  )

  def run(binary: File, histogram: File, top: Option[Int]): Seq[Line] = {
    IO.delete(histogram)
    val proc = new ProcessBuilder(binary.getAbsolutePath)
    proc.environment().put("GC_HISTOGRAM_FILE", histogram.getAbsolutePath)
    top.foreach(n => proc.environment().put("GC_HISTOGRAM_TOP", n.toString))
    proc.inheritIO()
    val running = proc.start()
    if (!running.waitFor(60, TimeUnit.SECONDS)) {
      running.destroyForcibly()
      sys.error("the program did not finish within 60 s")
    }
    if (running.exitValue() != 0)
      sys.error(s"the program exited with ${running.exitValue()}")
    IO.readLines(histogram).map { line =>
      line.split('\t') match {
        case Array(collection, bytes, objects, size, name) =>
          Line(collection.toInt, bytes.toLong, objects.toLong, size.toInt, name)
        case _ => sys.error(s"unexpected line in the histogram: $line")
      }
    }
  }

  // Each collection lists at most `top` classes by decreasing bytes, then the
  // total, which is no less than the sum of the classes listed
  def checkCollections(lines: Seq[Line], top: Int): Unit = {
    val byCollection = lines.groupBy(_.collection)
    val numbers = byCollection.keys.toList.sorted
    if (numbers != (1 to numbers.size).toList)
      sys.error(s"collections not numbered from 1: $numbers")
    if (numbers.size < Collections)
      sys.error(s"${numbers.size} collections, expected $Collections")
    byCollection.values.foreach { collection =>
      val classes = collection.init
      val total = collection.last
      val bytes = classes.map(_.bytes)
      if (total.name != "*total*")
        sys.error(s"a collection does not end with its total: $total")
      if (classes.size > top)
        sys.error(s"${classes.size} classes listed, more than $top")
      if (bytes != bytes.sorted.reverse)
        sys.error(s"classes not sorted by bytes: $classes")
      if (bytes.sum > total.bytes || classes.map(_.objects).sum > total.objects)
        sys.error(s"the total is less than its classes: $collection")
    }
  }

  // The last collection lists Main's objects and arrays
  def checkRetained(lines: Seq[Line]): Unit = {
    val last = lines.filter(_.collection == lines.map(_.collection).max)
    def find(name: String): Line =
      last.find(_.name == name).getOrElse(sys.error(s"$name not listed: $last"))
    // Each object's bytes are its instance size, rounded up to 16 bytes
    val retained = find("Retained")
    if (retained.objects != RetainedObjects || retained.size <= 0 ||
        retained.bytes != RetainedObjects * ((retained.size + 15) / 16 * 16))
      sys.error(s"unexpected line for Retained: $retained")
    val arrays = find("scala.scalanative.runtime.LongArray")
    if (arrays.objects < RetainedArrays || arrays.size != 0 ||
        arrays.bytes < RetainedArrays * 8000L)
      sys.error(s"unexpected line for LongArray: $arrays")
  }
}

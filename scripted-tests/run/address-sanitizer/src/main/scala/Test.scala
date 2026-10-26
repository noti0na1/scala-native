object Test {
  def main(args: Array[String]): Unit = {
    val threads = (1 to 8).map { i =>
      new Thread(() => {
        var kept: List[Array[Byte]] = Nil
        for (round <- 1 to 50) {
          kept = new Array[Byte](1024 * (round % 7 + 1)) :: kept.take(16)
          if (round % 10 == 0) System.gc()
        }
        assert(kept.nonEmpty, s"thread $i")
      })
    }
    threads.foreach(_.start())
    threads.foreach(_.join())
    System.gc()
    println("done")
  }
}

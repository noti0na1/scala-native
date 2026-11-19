// The extension: linked separately against the host's manifest. It subclasses
// a host class, implements a host trait, has a stateful module, allocates,
// and throws and catches across the boundary. run.sh also compiles a copy of
// it into the host with ORIGIN replaced (the marker test), as package
// `hostcopy`, so output naming the host copy shows host code running in the
// extension's place.
package ext

import api._

class ExtError(msg: String) extends HostError(msg)

class Greeter(id: Int) extends Base with Plugin {
  private val data = Array.tabulate(256)(i => (i * id).toLong)
  override def greet(): String = "hello from ORIGIN greeter " + id
  def name: String = "greeter-" + id
  // 0 in the extension, 1000 in the host's copy
  private def marker: Int = if ("ORIGIN" == "extension") 0 else 1000
  // Called by several threads at once in the multithreaded runs: notifyAll
  // inflates the monitors of the class object and of a string literal, which
  // live in the extension's static storage, while collections run
  def run(x: Int): Int = classOf[Greeter].synchronized {
    classOf[Greeter].notifyAll()
    val lock = "ORIGIN lock"
    lock.synchronized(lock.notifyAll())
    x * 2 + Main.counter + marker
  }
  def fail(): Unit = throw new ExtError("ORIGIN greeter " + id + " failed")
  def checksum(): Long = {
    var s = 0L
    var i = 0
    while (i < data.length) { s += data(i); i += 1 }
    s
  }
}

object Main extends Entry {
  var counter = 0
  private val cache = new Array[Array[Int]](100)

  def entry(): Int = {
    counter = 7
    var i = 1
    while (i <= 1000) { Registry.register(new Greeter(i)); i += 1 }
    var k = 0
    while (k < 100) { cache(k) = Array.fill(1000)(k); k += 1 }
    val caught =
      try { Registry.check(-1); false }
      catch { case e: HostError => e.getMessage == "negative: -1" }
    if (caught) 1 else 0
  }

  def verify(): Long = {
    var s = 0L
    var k = 0
    while (k < cache.length) {
      val a = cache(k)
      var i = 0
      while (i < a.length) { s += a(i); i += 1 }
      k += 1
    }
    s
  }

  def probe(x: Any): String =
    "HostError " + x.isInstanceOf[HostError] +
      ", Base " + x.isInstanceOf[Base] +
      ", Greeter " + x.isInstanceOf[Greeter] +
      ", ExtError " + x.isInstanceOf[ExtError] +
      ", Plugin " + x.isInstanceOf[Plugin] +
      ", class " + x.getClass.getName
}

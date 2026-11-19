// The host's API: the types an extension subclasses, implements and calls.
// run.sh links the host with `api.*` open.
package api

trait Plugin {
  def name: String
  def run(x: Int): Int
  def fail(): Unit
  def checksum(): Long
}

abstract class Base {
  def greet(): String = "base"
  def describe(): String = getClass.getName + ": " + greet()
}

class HostError(msg: String) extends RuntimeException(msg)

/** What an extension's entry module implements. */
trait Entry {

  /** Registers the extension's plugins; 1 if it caught the host's exception. */
  def entry(): Int

  /** A checksum over the extension's own module state. */
  def verify(): Long

  /** The extension's type tests and `getClass` on a host object. */
  def probe(x: Any): String
}

object Registry {
  private var plugins: List[Plugin] = Nil
  def register(p: Plugin): Unit = synchronized { plugins = p :: plugins }
  def all: List[Plugin] = plugins
  def check(x: Int): Int =
    if (x < 0) throw new HostError("negative: " + x) else x
}

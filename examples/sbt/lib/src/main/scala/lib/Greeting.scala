package lib

trait Greeter:
  def greet(name: String): String

object Greeting extends Greeter:
  def greet(name: String): String = s"Hello, $name!"

  def shout(name: String): String = greet(name).toUpperCase

package lib

class GreetingSuite extends munit.FunSuite:
  test("greet") {
    assertEquals(Greeting.greet("snc"), "Hello, snc!")
  }

  test("shout") {
    assertEquals(Greeting.shout("snc"), "HELLO, SNC!")
  }

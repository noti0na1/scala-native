package app

class StatsSuite extends munit.FunSuite:
  test("mean") {
    assertEquals(Stats.mean(List(1, 2, 3, 6)), 3.0)
  }

  test("median") {
    assertEquals(Stats.median(List(5, 1, 3)), 3)
  }

  test("summary uses lib") {
    assert(Stats.summary(List(1)).startsWith("n=1"))
    assertEquals(lib.Greeting.greet("app"), "Hello, app!")
  }

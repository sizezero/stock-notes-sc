package org.kleemann.stocknotes

class TestCurrency extends munit.FunSuite {

  test("zero") {
    assertEquals(Currency(0).toString, "$0.00")
  }

  test("less than a buck") {
    assertEquals(Currency( 1).toString, "$0.01")
    assertEquals(Currency(25).toString, "$0.25")
  }

  test("big numbers") {
    assertEquals(Currency(        1_000_000_00 ).toString,         "$1,000,000.00")
    assertEquals(Currency(1_000_000_000_000_00L).toString, "$1,000,000,000,000.00")
  }

  test("negative numbers") {
    assertEquals(Currency(    -1).toString, "($0.01)")
    assertEquals(Currency( -1_25).toString, "($1.25)")
    assertEquals(Currency(-99_99).toString, "($99.99)")

  }

  test("double") {
    val d = Currency(10_00).toDouble
    // can't really assert equal for binary floating point values
    assert(10.001 > d)
    assert( 9.999 < d)
  }
}

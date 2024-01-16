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

  test("parse simple") {
    assertEquals(Currency.parse("0"), Some(Currency(0)))
    assertEquals(Currency.parse("1"), Some(Currency(100)))
  }

  test("parse big") {
    assertEquals(Currency.parse(        "1000000"),          Some(Currency(1_000_000_00)))
    assertEquals(Currency.parse(        "1000000.99"),       Some(Currency(1_000_000_99L)))
    assertEquals(Currency.parse("123456789012345"),    Some(Currency(123456789012345_00L)))
    assertEquals(Currency.parse("123456789012345.67"), Some(Currency(123456789012345_67L)))
  }

  test("parse dollar prefix") {
    assertEquals(Currency.parse("$1"), Some(Currency(100)))
    assertEquals(Currency.parse("$123.45"), Some(Currency(123_45)))
  }

  test("fail") {
    assertEquals(Currency.parse(""), None)
    assertEquals(Currency.parse("$"), None)
    assertEquals(Currency.parse("foo"), None)
    assertEquals(Currency.parse("111a222"), None)
    assertEquals(Currency.parse("456 456"), None)
    assertEquals(Currency.parse("45.45.45"), None)
  }

  test("sign") {
    assertEquals(Currency.parse("+$1"),    Some(Currency( 1_00)))
    assertEquals(Currency.parse("-$1"),    Some(Currency(-1_00)))
    assertEquals(Currency.parse("+$9.99"), Some(Currency( 9_99)))
    assertEquals(Currency.parse("-$9.99"), Some(Currency(-9_99)))
    assertEquals(Currency.parse( "+1"),    Some(Currency( 1_00)))
    assertEquals(Currency.parse( "-1"),    Some(Currency(-1_00)))
    assertEquals(Currency.parse( "+9.99"), Some(Currency( 9_99)))
    assertEquals(Currency.parse( "-9.99"), Some(Currency(-9_99)))
  }
}

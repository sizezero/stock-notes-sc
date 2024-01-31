package org.kleemann.stocknotes.stock

class TestCurrency extends munit.FunSuite {

  test("zero") {
    assertEquals(Currency(0, 0).toString, "$0.00")
  }

  test("less than a buck") {
    assertEquals(Currency(0,  1).toString, "$0.01")
    assertEquals(Currency(0, 25).toString, "$0.25")
  }

  test("big numbers") {
    assertEquals(Currency(        1_000_000,  0).toString,         "$1,000,000.00")
    assertEquals(Currency(1_000_000_000_000L, 0).toString, "$1,000,000,000,000.00")
  }

  test("negative numbers") {
    assertEquals(Currency( -1, 0,  1).toString, "($0.01)")
    assertEquals(Currency(    -1, 25).toString, "($1.25)")
    assertEquals(Currency(   -99, 99).toString, "($99.99)")
  }

  test("double") {
    val d = Currency(10, 0).toDouble
    // can't really assert equal for binary floating point values
    assert(10.001 > d)
    assert( 9.999 < d)
  }

  test("parse simple") {
    assertEquals(Currency.parse("0"), Some(Currency.zero))
    assertEquals(Currency.parse("1"), Some(Currency(1, 0)))
  }

  test("parse big") {
    assertEquals(Currency.parse(        "1000000"),          Some(Currency(1_000_000L,  0)))
    assertEquals(Currency.parse(        "1000000.99"),       Some(Currency(1_000_000L, 99)))
    assertEquals(Currency.parse("123456789012345"),    Some(Currency(123456789012345L,  0)))
    assertEquals(Currency.parse("123456789012.34"),       Some(Currency(123456789012L, 34)))
  }

  test("parse dollar prefix") {
    assertEquals(Currency.parse(  "$1"),    Some(Currency(  1,  0)))
    assertEquals(Currency.parse("$123.45"), Some(Currency(123, 45)))
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
    assertEquals(Currency.parse( "+$1"),    Some(Currency(  1,  0)))
    assertEquals(Currency.parse( "-$1"),    Some(Currency( -1,  0)))
    assertEquals(Currency.parse( "+$9.99"), Some(Currency(  9, 99)))
    assertEquals(Currency.parse("-$11.11"), Some(Currency(-11, 11)))
    assertEquals(Currency.parse( "+1"),     Some(Currency(  1,  0)))
    assertEquals(Currency.parse( "-1"),     Some(Currency( -1,  0)))
    assertEquals(Currency.parse( "+9.99"),  Some(Currency(  9, 99)))
    assertEquals(Currency.parse("-12.12"),  Some(Currency(-12, 12)))
  }

  test("parse single digit to the right of the decimal") {
    assertEquals(Currency.parse("1.1"), Some(Currency(1, 10)))
    assertEquals(Currency.parse("1.1"), Some(Currency.decimal(1, "1")))
  }

  test("parse zero in the dime spot") {
    assertEquals(Currency.parse("1.01"), Some(Currency(1, 1)))
    assertEquals(Currency.parse("1.05"), Some(Currency(1, 5)))
  }

  test("decimal method") {
    assertEquals(Currency.decimal(0, "0").milliPennies,         0L)
    assertEquals(Currency.decimal(0, "12").milliPennies,    12000L)
    assertEquals(Currency.decimal(0, "123").milliPennies,   12300L)
    assertEquals(Currency.decimal(0, "1234").milliPennies,  12340L)
    assertEquals(Currency.decimal(0, "12345").milliPennies, 12345L)
  }

  test("operators") {
    val one = Currency.decimal(1, "0")
    val two = Currency.decimal(2, "0")
    assertEquals( one + one, two)
    assertEquals( one - one, Currency.zero)
  }

  test("fromDouble") {
    assertEquals(Currency.fromDouble(0.0), Currency.zero)
    assertEquals(Currency.fromDouble(1.0), Currency.decimal(1,"0"))
  }

  test("priceMultipleAdjust") {
    assertEquals(Currency(10,0).priceMultipleAdjust(Fraction.one,  Fraction(2,1)), Currency(5, 0))
    assertEquals(Currency(10,0).priceMultipleAdjust(Fraction(3,1), Fraction(6,1)), Currency(5, 0))
  }

  test("truncate") {
    assertEquals(Currency.decimal(0, "12345").truncate, Currency.decimal(0, "12"))
    val c = Currency.decimal(12, "34")
    assertEquals(c.truncate, c)
  }

  test("compare") {
    assert(Currency.zero < Currency(1,0))
    assert(Currency(1,0) > Currency.zero )
    assert(Currency(-2,0) < Currency(-1,0))
  }
}

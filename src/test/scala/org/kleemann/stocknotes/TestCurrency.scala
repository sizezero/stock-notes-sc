package org.kleemann.stocknotes

class TestCurrency extends munit.FunSuite {

  test("zero") {
    assertEquals(Currency.dollarsCents(0, 0).toString, "$0.00")
  }

  test("less than a buck") {
    assertEquals(Currency.dollarsCents(0,  1).toString, "$0.01")
    assertEquals(Currency.dollarsCents(0, 25).toString, "$0.25")
  }

  test("big numbers") {
    assertEquals(Currency.dollarsCents(        1_000_000,  0).toString,         "$1,000,000.00")
    assertEquals(Currency.dollarsCents(1_000_000_000_000L, 0).toString, "$1,000,000,000,000.00")
  }

  test("negative numbers") {
    assertEquals(Currency.dollarsCents( -1, 0,  1).toString, "($0.01)")
    assertEquals(Currency.dollarsCents(    -1, 25).toString, "($1.25)")
    assertEquals(Currency.dollarsCents(   -99, 99).toString, "($99.99)")
  }

  test("double") {
    val d = Currency.dollarsCents(10, 0).toDouble
    // can't really assert equal for binary floating point values
    assert(10.001 > d)
    assert( 9.999 < d)
  }

  test("parse simple") {
    assertEquals(Currency.parse("0"), Some(Currency.zero))
    assertEquals(Currency.parse("1"), Some(Currency.dollarsCents(1, 0)))
  }

  test("parse big") {
    assertEquals(Currency.parse(        "1000000"),          Some(Currency.dollarsCents(1_000_000L,  0)))
    assertEquals(Currency.parse(        "1000000.99"),       Some(Currency.dollarsCents(1_000_000L, 99)))
    assertEquals(Currency.parse("123456789012345"),    Some(Currency.dollarsCents(123456789012345L,  0)))
    assertEquals(Currency.parse("123456789012.34"),       Some(Currency.dollarsCents(123456789012L, 34)))
  }

  test("parse dollar prefix") {
    assertEquals(Currency.parse(  "$1"),    Some(Currency.dollarsCents(  1,  0)))
    assertEquals(Currency.parse("$123.45"), Some(Currency.dollarsCents(123, 45)))
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
    assertEquals(Currency.parse( "+$1"),    Some(Currency.dollarsCents(  1,  0)))
    assertEquals(Currency.parse( "-$1"),    Some(Currency.dollarsCents( -1,  0)))
    assertEquals(Currency.parse( "+$9.99"), Some(Currency.dollarsCents(  9, 99)))
    assertEquals(Currency.parse("-$11.11"), Some(Currency.dollarsCents(-11, 11)))
    assertEquals(Currency.parse( "+1"),     Some(Currency.dollarsCents(  1,  0)))
    assertEquals(Currency.parse( "-1"),     Some(Currency.dollarsCents( -1,  0)))
    assertEquals(Currency.parse( "+9.99"),  Some(Currency.dollarsCents(  9, 99)))
    assertEquals(Currency.parse("-12.12"),  Some(Currency.dollarsCents(-12, 12)))
  }

  test("parse single digit to the right of the decimal") {
    assertEquals(Currency.parse( "1.1"), Some(Currency.dollarsCents(1, 10)))
    assertEquals(Currency.parse( "1.1"), Some(Currency.decimal(1, 1)))
  }

  test("decimal method") {
    assertEquals(Currency.decimal(0, 0).milliPennies,         0L)
    assertEquals(Currency.decimal(0, 12).milliPennies,    12000L)
    assertEquals(Currency.decimal(0, 123).milliPennies,   12300L)
    assertEquals(Currency.decimal(0, 1234).milliPennies,  12340L)
    assertEquals(Currency.decimal(0, 12345).milliPennies, 12345L)
  }

  test("operators") {
    val one = Currency.decimal(1, 0)
    val two = Currency.decimal(2, 0)
    assertEquals( one + one, two)
    assertEquals( one - one, Currency.zero)
  }

  test("fromDouble") {
    assertEquals(Currency.fromDouble(0.0), Currency.zero)
    assertEquals(Currency.fromDouble(1.0), Currency.decimal(1,0))
  }
}

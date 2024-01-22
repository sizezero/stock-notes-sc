package org.kleemann.stocknotes

class TestDate extends munit.FunSuite {
  test("create good dates") {
    assertNotEquals(Date(2005,1,1), None)
    assertNotEquals(Date(2005,12,31), None)
    assertNotEquals(Date(1968,4,24), None)
  }

  test("fail to create bad years") {
    assertEquals(Date(1,1,1), None)
    assertEquals(Date(0,1,1), None)
    assertEquals(Date(-1,1,1), None)
    assertEquals(Date(1899,-1,1), None)
    assertEquals(Date(3001,-1,1), None)
  }

  test("fail to create bad months") {
    assertEquals(Date(2020,-1,1), None)
    assertEquals(Date(2020,0,1), None)
    assertEquals(Date(2020,13,1), None)
    assertEquals(Date(2020,1000,1), None)
  }

  test("today is somewhat recent") {
    assert(Date.today.year > 2020)
  }

  test("parse good dates") {
    assertNotEquals(Date.parse("Jan 1, 2005"), None)
    assertNotEquals(Date.parse("jan 1, 2005"), None)
    assertNotEquals(Date.parse("jAN 1, 2005"), None)
    assertNotEquals(Date.parse("   Dec    31,   2005    "), None)
    assertNotEquals(Date.parse("Apr 24, 1968"), None)
  }

  test("parse bad dates") {
    assertEquals(Date.parse("Jan 32, 2005"), None)
    assertEquals(Date.parse("stuff before Jan 1, 2005"), None)
    assertEquals(Date.parse("Jan 1, 2005 stuff after"), None)
    assertEquals(Date.parse("January 32, 2005"), None)
  }

  test("date ordering") {
    val o0 = Date(1968,4,24)
    val o1 = Date(2020,1,1)
    val o2 = Date(2020,1,2)
    assertEquals(for d0<-o0; d1<-o1 yield (d0<d1), Option(true))
  }

  test("1 decimalYear") {
    val d1 = Date(2010,1,1).get.decimalYear
    val d2 = Date(2011,1,1).get.decimalYear
    assert(d2 - d1 >= 0.99)
    assert(d2 - d1 <= 1.01)
  }

  test("2 decimalYear") {
    val d1 = Date(2010,1,1).get.decimalYear
    val d2 = Date(2010,12,31).get.decimalYear
    assert(d2 - d1 >= 0.99)
    assert(d2 - d1 <= 1.00)
  }
}

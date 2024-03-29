package org.kleemann.stocknotes.command

class TestBrowseTicker extends munit.FunSuite {
  test("fail on no arguments") {
    val args = Vector[String]()
    assertNotEquals(BrowseTicker.command(args), None)
  }

  // we can't succeed now that the functionality has been implemented

  // test("succeed on ticker") {
  //   val args = Vector("MSFT")
  //   assertEquals(BrowseTicker.command(args), None)
  // }

  // test("succeed on no edit") {
  //   val args = Vector("-n", "MSFT")
  //   assertEquals(BrowseTicker.command(args), None)
  // }

  test("fail on no edit") {
    assertNotEquals(BrowseTicker.command(Vector("foo", "MSFT")), None)
  }

  test("fail with no edit in wrong position") {
    assertNotEquals(BrowseTicker.command(Vector("MSFT", "-n")), None)
  }
}

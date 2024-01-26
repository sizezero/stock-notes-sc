package org.kleemann.stocknotes.command

class TestDownloadQuotes extends munit.FunSuite {
  test("succeed on no arguments") {
    val args = Vector[String]()
    assertEquals(DownloadQuotes.command(args), None)
  }

  test("fail on various single arguments") {
    assertNotEquals(DownloadQuotes.command(Vector("foo")), None)
    assertNotEquals(DownloadQuotes.command(Vector("bar")), None)
    assertNotEquals(DownloadQuotes.command(Vector("baz")), None)
    assertNotEquals(DownloadQuotes.command(Vector("")), None)
  }

  test("fail on various single arguments") {
    assertNotEquals(DownloadQuotes.command(Vector("notify", "notify")), None)
    assertNotEquals(DownloadQuotes.command(Vector("buysell", "notify")), None)
    assertNotEquals(DownloadQuotes.command(Vector("foo", "bar")), None)
    assertNotEquals(DownloadQuotes.command(Vector("notify", "")), None)
  }

}
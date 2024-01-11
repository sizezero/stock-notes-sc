package org.kleemann.stocknotes

class TestDownloadQuotes extends munit.FunSuite {
  test("succeed on no arguments") {
    val args = Vector[String]()
    assertEquals(DownloadQuotes.command(args), None)
  }

  test("succeed on notify argument") {
    assertEquals(DownloadQuotes.command(Vector("notify")), None)
  }

  test("succeed on buysell argument") {
    assertEquals(DownloadQuotes.command(Vector("buysell")), None)
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

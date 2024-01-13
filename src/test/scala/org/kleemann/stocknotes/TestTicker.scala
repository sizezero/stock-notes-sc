package org.kleemann.stocknotes

class TestTicker extends munit.FunSuite {

    test("sorted") {
        assert(Ticker("AAPL") < Ticker("MSFT"))
        assert(Ticker("MSFT") > Ticker("AAPL"))

        val obtained = List(Ticker("GOOG"), Ticker("AAPL"), Ticker("MSFT")).sorted
        val expected = List(Ticker("AAPL"), Ticker("GOOG"), Ticker("MSFT"))
        assertEquals(obtained, expected)
    }
}

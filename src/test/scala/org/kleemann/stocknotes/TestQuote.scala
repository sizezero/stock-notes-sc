package org.kleemann.stocknotes

class TestQuote extends munit.FunSuite {

    test("read a file of quotes") {
        val g: os.Generator[String] = os.Generator.from(Seq(
            "AAPL,185.92,",
            "ABB,0,",
            "AMD,146.56,",
            "APEMY,31.41,"))
        val m: Map[Ticker, Quote] = Map(
            Ticker("AAPL")  -> Quote(Currency(185, 92)),
            Ticker("ABB")   -> Quote(Currency.zero),
            Ticker("AMD")   -> Quote(Currency(146, 56)),
            Ticker("APEMY") -> Quote(Currency(31, 41)))
        assertEquals(Quote.load(g), m)
    }

    test("csv line has too few args") {
        assertEquals(Quote.parseCsvLine("AAPL,185.92"), None)
    }

    test("csv line has bad price") {
        assertEquals(Quote.parseCsvLine("AAPL,expensive,"), None)
    }

}

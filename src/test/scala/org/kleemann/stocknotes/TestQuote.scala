package org.kleemann.stocknotes

class TestQuote extends munit.FunSuite {

    test("read a file of quotes") {
        val g: os.Generator[String] = os.Generator.from(Seq(
            "AAPL,185.92,01/12/2024,",
            "ABB,0,01/12/2024,",
            "AMD,146.56,01/12/2024,",
            "APEMY,31.41,01/12/2024,"))
        val d = Date(2024, 1, 12).get
        val m: Map[Ticker, Quote] = Map(
            Ticker("AAPL")  -> Quote(185.92, d),
            Ticker("ABB")   -> Quote(  0,    d),
            Ticker("AMD")   -> Quote(146.56, d),
            Ticker("APEMY") -> Quote(31.41,  d))
        assertEquals(Quote.load(g), m)
    }

    test("csv line has too few args") {
        assertEquals(Quote.parseCsvLine("AAPL,185.92,01/12/2024"), None)
    }

    test("csv line has bad price") {
        assertEquals(Quote.parseCsvLine("AAPL,expensive,01/12/2024"), None)
    }

    test("csv line has unparsable date") {
        assertEquals(Quote.parseCsvLine("AAPL,185.92,recently"), None)
    }

    test("map merging") {
        val m1 = Map(1->11, 2->12, 3->13, 5->15)
        val m2 = Map(1->11, 2->14, 3->13, 4->14)
        assertEquals(Quote.merge(m1, m2, (i,j) => if (i<j) i else j ),
            Map(1->11, 2->12, 3->13, 4->14, 5->15))
        assertEquals(Quote.merge(m1, m2, (i,j) => if (i>j) i else j ),
            Map(1->11, 2->14, 3->13, 4->14, 5->15))
    }
}

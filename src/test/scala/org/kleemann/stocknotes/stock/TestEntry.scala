package org.kleemann.stocknotes.stock

import org.kleemann.stocknotes.Ticker

class TestEntry extends munit.FunSuite {

    test("ordering") {
        // I'm pretty sure this will just sort based on the composites
        val e1 = Entry(Ticker("MSFT"),Date(2000,1,2).get, List("a"))
        val e2 = Entry(Ticker("MSFT"),Date(2000,1,1).get, List("b"))
        val e3 = Entry(Ticker("MSFT"),Date(2000,1,3).get, List("c"))
        val e4 = Entry(Ticker("AAPL"),Date(2000,1,1).get, List("d"))
        val e5 = Entry(Ticker("MSFT"),Date(1990,1,1).get, List("e"))
        val ls = List(e1,e2,e3,e4,e5)
        assertEquals(ls.sorted, List(e4, e5, e2, e1, e3))
    }

    test("coalesce") {
        val content: List[String | Trade | Watch] = List(
            "one ", "two ", "three",
            BuyWatch.none,
            "four",
            BuyWatch.none,
            "five ", "six",
            BuyWatch.none,
            "seven ", "eight ", "nine"
        )
        Entry(Ticker("foo"), Date.earliest, content) match {
            case Entry(_, _, obtained) => {
                val expected: List[String | Trade | Watch] = List(
                    "one two three",
                    BuyWatch.none,
                    "four",
                    BuyWatch.none,
                    "five six",
                    BuyWatch.none,
                    "seven eight nine"
                )
                assertEquals(obtained, expected)
            }
            case null => assert(false)
        }
    }
}
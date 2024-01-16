package org.kleemann.stocknotes

class TestEntry extends munit.FunSuite {

    test("ordering") {
        // I'm pretty sure this will just sort based on the composites
        val e1 = Entry(Ticker("MSFT"),Date(2000,1,2).get, "a")
        val e2 = Entry(Ticker("MSFT"),Date(2000,1,1).get, "b")
        val e3 = Entry(Ticker("MSFT"),Date(2000,1,3).get, "c")
        val e4 = Entry(Ticker("AAPL"),Date(2000,1,1).get, "d")
        val e5 = Entry(Ticker("MSFT"),Date(1990,1,1).get, "e")
        val ls = List(e1,e2,e3,e4,e5)
        assertEquals(ls.sorted, List(e4, e5, e2, e1, e3))
    }

}
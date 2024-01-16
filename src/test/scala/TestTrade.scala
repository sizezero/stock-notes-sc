package org.kleemann.stocknotes

class TestTrade extends munit.FunSuite {

    test("buy") {
        val d = Date(1900,1,1).get
        val e = Trade.parse("TRADE buy 100@$67.20 balance 100 commission 9.99", d, Fraction.one)
        assertEquals(
            e,
            Right( (Buy(d, Shares(100, Fraction.one), Price(Currency(67_20), Fraction.one), Currency(9_99)), Shares(100, Fraction.one)) )
            )
        // TODO: these signatures are making me think that the buy and sell share price are really Currency types and not Price types. Unlike Watch, 
        // they have nothing to do with changing value over time. They are an absolute value at the time of the transation. Well, maybe they do if you use them
        // to calculate costs...
    }

    test("sell") {
        val d = Date(1968,4,24).get
        val e = Trade.parse("TRADE sell 420@$354 balance 10000 commission 0", d, Fraction.one)
        assertEquals(
            e,
            Right( (Sell(d, Shares(420, Fraction.one), Price(Currency(354_00), Fraction.one), Currency(0)), Shares(10000, Fraction.one)) )
            )
    }

    test("split") {
        val d = Date(1990,7,13).get
        val e = Trade.parse("TRADE split 10:1 balance 10", d, Fraction.one)
        assertEquals(
            e,
            Right( (Split(d, Fraction(10,1)), Shares(10, Fraction(10,1))) ) // note balance multiple is post split
            )
    }

    test("bad trade") {
        val d = Date(1990,7,13).get
        val m = Fraction.one
        assert(Trade.parse("TRADE foobar", d, m).isLeft)
        assert(Trade.parse("TRADE buy 10@0@67.20 balance 100 commission 9.99", d, m).isLeft)
        assert(Trade.parse("TRADE buy 10@0@67.20 balance 100", d, m).isLeft)
        assert(Trade.parse("TRADE sell 100@67.20 balance 100", d, m).isLeft)
        assert(Trade.parse("TRADE split 10 balance 100", d, m).isLeft)
    }
}
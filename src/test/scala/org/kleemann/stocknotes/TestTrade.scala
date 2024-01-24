package org.kleemann.stocknotes

class TestTrade extends munit.FunSuite {

    test("buy") {
        val d = Date.earliest
        val e = Trade.parse("TRADE buy 100@$67.20 balance 100 commission 9.99", d, Fraction.one)
        assertEquals(
            e,
            Right( (Buy(d, Shares(100, Fraction.one), Currency(67, 20), Currency(9, 99)), Shares(100, Fraction.one)) )
            )
    }

    test("sell") {
        val d = Date(1968,4,24).get
        val e = Trade.parse("TRADE sell 420@$354 balance 10000 commission 0", d, Fraction.one)
        assertEquals(
            e,
            Right( (Sell(d, Shares(420, Fraction.one), Currency(354, 0), Currency.zero), Shares(10000, Fraction.one)) )
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
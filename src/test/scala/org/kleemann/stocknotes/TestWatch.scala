package org.kleemann.stocknotes

class TestWatch extends munit.FunSuite {

    test("none buy/sell") {
        assertEquals(
            Watch.parse("BUY none", Fraction.one),
            Right(BuyWatch.none)
        )
        assertEquals(
            Watch.parse("BUY None", Fraction.one),
            Right(BuyWatch.none)
        )
        assertEquals(
            Watch.parse("SELL none", Fraction.one),
            Right(SellWatch.none)
        )
        assertEquals(
            Watch.parse("SELL None", Fraction.one),
            Right(SellWatch.none)
        )
    }

    test("one buy") {
        assertEquals(
            Watch.parse("BUY $20.00", Fraction.one),
            Right(BuyWatch(Some(Currency(20, 0)), None, Fraction.one))
        )
        assertEquals(
            Watch.parse("BUY 100.00", Fraction.one),
            Right(BuyWatch(Some(Currency(100, 0)), None, Fraction.one))
        )
    }

    test("two buy") {
        assertEquals(
            Watch.parse("BUY $40.00 20", Fraction.one),
            Right(BuyWatch(Some(Currency(40, 0)),Some(Currency(20, 0)), Fraction.one))
        )
    }

    test("one sell") {
        assertEquals(
            Watch.parse("SELL $9.99", Fraction.one),
            Right(SellWatch(None, Some(Currency(9, 99)), Fraction.one))
        )
    }

    test("two sell") {
        assertEquals(
            Watch.parse("SELL $40.00 20", Fraction.one),
            Right(SellWatch(Some(Currency(40, 0)),Some(Currency(20, 0)), Fraction.one))
        )
    }

    test("bad parse") {
        assert(Watch.parse("floogle", Fraction.one).isLeft)
        assert(Watch.parse("BUY blah blorg", Fraction.one).isLeft)
        assert(Watch.parse("BUY NONE", Fraction.one).isLeft)
        assert(Watch.parse("SELL a quick brown fox", Fraction.one).isLeft)
    }

    test("multiple") {
        val w = SellWatch(Some(Currency(40, 0)),Some(Currency(20, 0)), Fraction.one)
        val two = Fraction(2,1)
        // doubling the mult is a 2:1 split which should half the price
        assertEquals( w.lowAtMult(two), Some(Currency(20,0)))
        assertEquals(w.highAtMult(two), Some(Currency(10,0)))
    }

    test("complex multiple") {
        // test multiple changes over time
        val m1 = Fraction.one
        val w1 = BuyWatch(Some(Currency(10, 0)), None, m1)
        assertEquals(w1.lowAtMult(m1), Some(Currency(10, 0)))
        // 5:1 split, every share becomes five, multiply shares by five.
        // All things being equal, the price should divide by five
        val m2 = m1*Fraction(5,1)
        assertEquals(w1.lowAtMult(m2), Some(Currency(2, 0)))
        // 2:1 split, shares double, prices are cut in half
        val m3 = m2*Fraction(2,1)
        assertEquals(w1.lowAtMult(m3), Some(Currency(1, 0)))
        // a reverse 1:3 split, every share becomes 3, price multiplies by three
        val m4 = m3*Fraction(1,3)
        assertEquals(w1.lowAtMult(m4), Some(Currency(3, 0)))
    }

    test("canonical none") {
        assertEquals(BuyWatch(None,None,Fraction(1,3)), BuyWatch.none)
        assertEquals(Watch.parse("BUY None",  Fraction(7,1)), Right(BuyWatch.none))
        assertEquals(Watch.parse("SELL none", Fraction(9,1)), Right(SellWatch.none))
    }
}

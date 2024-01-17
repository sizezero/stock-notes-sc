package org.kleemann.stocknotes

class TestWatch extends munit.FunSuite {

    test("none buy/sell") {
        assertEquals(
            Watch.parse("BUY none", Fraction.one),
            Right(BuyWatch(None, None))
        )
        assertEquals(
            Watch.parse("BUY None", Fraction.one),
            Right(BuyWatch(None, None))
        )
        assertEquals(
            Watch.parse("SELL none", Fraction.one),
            Right(SellWatch(None, None))
        )
        assertEquals(
            Watch.parse("SELL None", Fraction.one),
            Right(SellWatch(None, None))
        )
    }

    test("one buy") {
        assertEquals(
            Watch.parse("BUY $20.00", Fraction.one),
            Right(BuyWatch(Some(Price(Currency.dollarsCents(20, 0), Fraction.one)), None))
        )
        assertEquals(
            Watch.parse("BUY 100.00", Fraction.one),
            Right(BuyWatch(Some(Price(Currency.dollarsCents(100, 0), Fraction.one)), None))
        )
    }

    test("two buy") {
        assertEquals(
            Watch.parse("BUY $40.00 20", Fraction.one),
            Right(BuyWatch(Some(Price(Currency.dollarsCents(40, 0), Fraction.one)),Some(Price(Currency.dollarsCents(20, 0), Fraction.one))))
        )
    }

    test("one sell") {
        assertEquals(
            Watch.parse("SELL $9.99", Fraction.one),
            Right(SellWatch(None, Some(Price(Currency.dollarsCents(9, 99), Fraction.one))))
        )
    }

    test("two sell") {
        assertEquals(
            Watch.parse("SELL $40.00 20", Fraction.one),
            Right(SellWatch(Some(Price(Currency.dollarsCents(40, 0), Fraction.one)),Some(Price(Currency.dollarsCents(20, 0), Fraction.one))))
        )
    }

    test("bad parse") {
        assert(Watch.parse("floogle", Fraction.one).isLeft)
        assert(Watch.parse("BUY blah blorg", Fraction.one).isLeft)
        assert(Watch.parse("BUY NONE", Fraction.one).isLeft)
        assert(Watch.parse("SELL a quick brown fox", Fraction.one).isLeft)
    }
}

package org.kleemann.stocknotes

class TestPrice extends munit.FunSuite {

    test("zero") {
        assertEquals(Price.zero, Price(Currency.zero, Fraction(1, 1)))
        assertEquals(Price.zero, Price(Currency.zero, Fraction(2, 2)))
    }

    test("parse success") {
        val m = Fraction.one
        assertEquals(Price.parse("$1", m), Some(Price(Currency(1_00), m)))
        assertEquals(Price.parse("foogle", m), None)
        // parse is just a passthrough to Currency.parse so we don't need to test it more here
    }

    test("multiple") {
        // test multiple changes over time
        val m1 = Fraction.one
        val p1 = Price(Currency(10_00), m1)
        val d1 = p1.atMult(m1)
        assert(d1 >  9.99)
        assert(d1 < 10.01)
        // 5:1 split, every five shares becomes 1, divide by five
        val m2 = m1*Fraction(5,1)
        val d2 = p1.atMult(m2)
        assert(clue(d2) > 1.99)
        assert(d2 < 2.01)
        // 2:1 split, every two shares becomes 1, divide by two
        val m3 = m2*Fraction(2,1)
        val d3 = p1.atMult(m3)
        assert(clue(d3) > 0.99)
        assert(d3 < 1.01)
        // 1:3 split, every share becomes 3, multiplay by three
        val m4 = m3*Fraction(1,3)
        val d4 = p1.atMult(m4)
        assert(clue(d4) > 2.99)
        assert(d4 < 3.01)
    }
}
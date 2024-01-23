package org.kleemann.stocknotes

class TestShares extends munit.FunSuite {

    test("multiple") {
        // test multiple changes on shares over time
        val m1 = Fraction.one
        // buy 10 shares
        val s1 = Shares(10, m1)
        assertEquals(s1.atMult(m1), 10.0)
        // 5:1 split, every share becomes five, multiply shares by five.
        val m2 = m1*Fraction(5,1)
        assertEquals(s1.atMult(m2), 50.0)
        // buy 10 more shares at the current multiple
        val s2 = Shares(10, m2)
        // 2:1 split, every shares becomes 2, multiply shares by two
        val m3 = m2*Fraction(2,1)
        assertEquals(s1.atMult(m3), 100.0)
        assertEquals(s2.atMult(m3), 20.0)
        // 1:3 split, every 3 shares becomes 1, divide by three
        val m4 = m3*Fraction(1,3)
        assertEquals(s1.atMult(m4), 33.333333333333336) // this is lossy and may require you to manually buy or sell a share or two for accuracy
        assertEquals(s2.atMult(m4), 6.666666666666666)

        // now add these shares and normalize them based on the new multiple
        //val s3 = s1.add(s2, m4)
        //assertEquals(s3, Shares(39, Fraction(20, 3)))
    }

    test("compare") {
        // zero
        assertEquals(Shares.zero, Shares.zero)
        assertEquals(Shares.zero, Shares(0, Fraction(2,1)))
        assertEquals(Shares.zero, Shares(0, Fraction(1,2)))
        // equality of simple numbers
        assertEquals(Shares(1, Fraction.one), Shares(1, Fraction.one))
        assertEquals(Shares(1, Fraction.one), Shares(2, Fraction(2,1)))
        // inquality
        assertNotEquals(Shares(1, Fraction.one), Shares(1, Fraction(2,1)))
        assertNotEquals(Shares(1, Fraction.one), Shares(2, Fraction.one))
    }

    test("simple add, source and dest multiples are the same") {
        val s = Shares(1, Fraction.one)
        assertEquals(s.add(s, Fraction.one), Shares(2,Fraction.one))
    }

    test("simple add, source multiples are the same, dest multiple is different") {
        val s = Shares(1, Fraction.one)
        // there has been a two to one split so there should be twice as many shares
        val m = Fraction(2,1)
        val sum = s.add(s, m)
        assertEquals(sum.multiple, m) // sum should have the requested target multiple
        assertEquals(sum.atMult(m), 4.0) // we should have twice as many shares as before
    }

    test("add, both source and dest are different") {
        val m1 = Fraction.one
        val s1 = Shares(10, m1)
        // there has been a ten to one split so there should be twice as many shares
        val m2 = m1*Fraction(10,1)
        val s2 = Shares(10, m2)
        // there is a reverse 1:2 split
        val m3 = m2*Fraction(1,2)
        val sum = s1.add(s2, m3)
        assertEquals(m3, Fraction(5,1))
        assertEquals(sum.multiple, m3)
        assertEquals(sum.atMult(m3), 55.0) // 10 -split.> 100 -buy-> 110 -reverse-split-> 55
    }

    test("subtraction") {
        assertEquals(
            Shares(3, Fraction.one).sub(Shares(1, Fraction.one), Fraction.one),
            Shares(2, Fraction.one))
    }

}

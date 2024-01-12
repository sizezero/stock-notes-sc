package org.kleemann.stocknotes

class TestFraction extends munit.FunSuite {

    // test all three gcd calls
    def gcd(name: String, a: Int, b: Int, result: Int)(implicit loc: munit.Location): Unit = {
        test(name) {
            assertEquals(Fraction.gcdRecursive(a,b), result)
            assertEquals(Fraction.gcdIterative(a,b), result)
            assertEquals(Fraction.gcd(a,b),          result)
        }
    }

    gcd("one", 1, 1, 1)
    gcd("12,15", 12, 15, 3)
    gcd("big", 3000, 2000, 1000)
    // probably need more gcd tests...

    test("normalize") {
        val f = Fraction(10,20)
        assertEquals(f.numerator, 1)
        assertEquals(f.denominator, 2)
    }

    test("toString") {
        assertEquals(Fraction(1,2).toString, "1/2")
        assertEquals(Fraction(-1,2).toString, "-1/2")
        assertEquals(Fraction(1,-2).toString, "-1/2")
    }

    test("toDouble") {
        assertEquals(Fraction(1,1).toDouble, 1.0)
        assertEquals(Fraction(-1,1).toDouble, -1.0)
        assertEquals(Fraction(1,2).toDouble, 0.5)
    }

    test("addition") {
        assertEquals(Fraction(1,2)+Fraction(1,4), Fraction(3,4))
    }

    test("negation") {
        assertEquals(-Fraction(1,2), Fraction(-1,2))
    }

    test("subtraction") {
        assertEquals(Fraction(1,2)-Fraction(1,3), Fraction(1,6))
    }

    test("multiplication") {
        assertEquals(Fraction(1,2)*Fraction(1,3), Fraction(1,6))
    }

    test("division") {
        assertEquals(Fraction(1,2)/Fraction(1,2), Fraction(1,1))
    }

    test("ordering") {
        assert(Fraction(1,2) > Fraction(1,4))
        assert(Fraction(-1,2) < Fraction(1,4))

        val obtained = List(Fraction(2,3), Fraction(1,3), Fraction(3,3)).sorted
        val expected = List(Fraction(1,3), Fraction(2,3), Fraction(3,3))
        assertEquals(obtained, expected)
    }

    test("predefined values") {
        assertEquals(Fraction.zero.toString, "0/1")
        assertEquals(Fraction.one.toString, "1/1")
    }
}

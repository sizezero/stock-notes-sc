package org.kleemann.stocknotes

import scala.annotation.tailrec

/**
  * A simple fraction class that can be used for accurate stock splits.
  */
final case class Fraction private(numerator: Int, denominator: Int) extends Ordered[Fraction] {

    override def toString(): String = s"$numerator/$denominator"

    def toDouble: Double = numerator.toDouble / denominator

    /**
      * I'm not sure if I need this. I should delete it if its not being used.
      *
      * @param that
      * @return
      */
    override def compare(that: Fraction): Int = 
        if (this.numerator==that.numerator && this.denominator==that.denominator) 0
        else if (this.toDouble - that.toDouble > 0.0) 1
        else -1

    /**
      * Reduces the current fraction to it's smallest possible numerators and denominators and makes sure the sign is only in the numerator.
      * Mutable actions within this class will always call this method ensuring that we have the smallest possible values.
      *
      * @return
      */
    private def normalize: Fraction = {
        // only the numerator is signed, denominator is never negative
        val (n, d) =
            if (numerator>=0 && denominator>=0)     ( numerator,  denominator) // both are positive
            else if (numerator<0 && denominator>=0) ( numerator,  denominator) // only numerator is negative
            else if (numerator>=0 && denominator<0) (-numerator, -denominator) // only denominator is negative
            else                                    (-numerator, -denominator) // both are negative

        val divisor = Fraction.gcd(n,d)
        // normalize() is called by apply() so use the new operator to prevent infinite recursion
        new Fraction(n/divisor, d/divisor)
    }

    def +(that: Fraction): Fraction = Fraction(
        numerator*that.denominator + that.numerator*denominator,
        denominator*that.denominator)

    def unary_- = Fraction(-numerator,denominator)

    def -(that: Fraction): Fraction = this + -that

    def *(that: Fraction): Fraction = Fraction(
        this.numerator * that.numerator,
        this.denominator * that.denominator)

    def /(that: Fraction): Fraction = Fraction(
        this.numerator * that.denominator,
        this.denominator * that.numerator)

    def reciprocal: Fraction =
        // I don't think we have a use case for zero in this application so this should never happen
        if (numerator==0) throw new java.lang.ArithmeticException("/ by zero")
        else Fraction(denominator, numerator)
}

object Fraction {

    val one  = Fraction(1,1)

    def apply(numerator: Int, denominator: Int): Fraction = {
        // we could return an Option but this should be a rare case so there's no need to make the caller constantly unpack these
        if (denominator==0) throw new java.lang.ArithmeticException("/ by zero")
        new Fraction(numerator, denominator).normalize
    }

    /** Greatest common denominator. Finds the largest number that can divide each of the two numbers. 
     * The number one divides into everything so a return value is guaranteed.
     * 
     * The gcd function is needed by Fraction.reduce()
     * 
     * @param a one integer
     * @param b another integer
     * @return the largest integer that divides into both a and b
     */
    private[stocknotes] def gcd(a: Int, b: Int): Int = gcdIterative(a.abs,b.abs)

    /**
      * @param a must be positive
      * @param b must be positive
      * @return the largest integer that divides into both a and b
      */
    @tailrec
    private[stocknotes] def gcdRecursive(a: Int, b: Int): Int =
        if (b == 0) a
        else gcdRecursive(b,a%b)

    /**
      * @param a must be positive
      * @param b must be positive
      * @return the largest integer that divides into both a and b
      */
    private[stocknotes] def gcdIterative(a: Int, b: Int): Int = {
        var pair = (a, b)
        while (true) {
            if (pair._2 == 0) return pair._1
            pair = (pair._2 , pair._1 % pair._2)
        }
        throw new RuntimeException("unreachable")
    }

}
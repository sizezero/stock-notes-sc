package org.kleemann.stocknotes.stock

import org.kleemann.stocknotes.Fraction

/**
  * This represents a number of shares that were transacted at a particular multiple.
  * This allows the share count to be represented by an integer even though stock splits and
  * reverse splits in the future may make this no longer the case.
  *
  * @param shares
  * @param multiple
  */
final case class Shares(shares: Int, multiple: Fraction) {

    /**
      * This is a lossy way to calculate the current integer share count.
      *
      * @param currentMultiple The current multiple to convert to.
      * @return the decimal number of shares
      */
    def atMult(currentMultiple: Fraction): Double = (shares * (currentMultiple/multiple).toDouble)

    def toDoubleShares: DoubleShares = DoubleShares(atMult(Fraction.one))

    def toString(multiple: Fraction): String = atMult(multiple).toInt.toString

    /**
      * At a minimum we need this to compare shares against Shares.zero.
      * I think Odersky's Scala book has a bunch of pages on this so it might be useful to read them.
      * I think the fact that this is a final class makes things easier.
      *
      * @param other
      * @return
      */
    override def equals(other: Any): Boolean = other match {
      case that: Shares =>
        (this.shares==0 && that.shares==0)
        || (this.shares/this.multiple.toDouble == that.shares/that.multiple.toDouble)
      case _ => false
    }

    // TODO: there are a whole bunch of comparison and arithmetic operators that I'm going to hold of on implementing.

    /**
      * Adds two share counts to result in a new share count with the target multiple.
      *
      * @param that the second share count to add
      * @param targetMutiple the multiple of the returned share count
      * @return
      */
    def add(that: Shares, targetMultiple: Fraction): Shares =
      // we go through the different clauses in order to do integer addition when possible
      if (this.multiple==targetMultiple && that.multiple==targetMultiple)
        Shares(this.shares + that.shares, targetMultiple) // do integer addition when possible
      else if (this.multiple==targetMultiple) {
        // if we multiply the shares by a number and the fraction by the reciprocal of the number, then the number doesn't change
        val normalizer = (targetMultiple * that.multiple.reciprocal).toDouble
        Shares(Math.round(this.shares + that.shares*normalizer).toInt, targetMultiple)
      } else if (that.multiple==targetMultiple) {
        val normalizer = (targetMultiple * this.multiple.reciprocal).toDouble
        Shares(Math.round(this.shares*normalizer + that.shares).toInt, targetMultiple)
      } else {
        val thisNormalizer = (targetMultiple * this.multiple.reciprocal).toDouble
        val thatNormalizer = (targetMultiple * that.multiple.reciprocal).toDouble
        Shares(Math.round(this.shares*thisNormalizer + that.shares*thatNormalizer).toInt, targetMultiple)
      }

    def sub(that: Shares, targetMultiple: Fraction): Shares =
      add(Shares(-that.shares, that.multiple), targetMultiple)
}

object Shares {
    // I'm going to hold off on implementing the parse fn.
    // It just converts a decimal to a share count and takes a multiple argument.

    val zero = Shares(0, Fraction.one)
 }

/**
  * Reverse splits can cause old share counts to be fractional so, during report calculations it's
  * handy to have a data object that is non-integer.
  * This is usually just needed for reporting that can handle tiny rounding issues so the solution is to treat the
  * shares as a double at multiple Fraction.one.
  *
  * Note: we would like this object to be called Double in the Shares object but we don't want to
  * mask the built-in Double type.
  */
case class DoubleShares private[stock] (d: Double) {

  /**
    * Compares at a small quantum that should be far below any fractional share.
    */
  override def equals(other: Any): Boolean = other match
    case that: DoubleShares => Math.abs(this.d-that.d) < 0.0001
    case _                  => false

  /**
    * Converts to a Share object at the given multiple by rounding the contained double.
    */
  def toShares(toMultiple: Fraction): Shares = Shares((d * toMultiple.toDouble).round.toInt, toMultiple)

  /**
    * Converts to a Share object at the given multiple by truncating the contained double.
    */
  def toSharesTruncate(toMultiple: Fraction) = Shares((d * toMultiple.toDouble).toInt, toMultiple)

  def +(that: DoubleShares): DoubleShares = DoubleShares(this.d + that.d)

  def -(that: DoubleShares): DoubleShares = DoubleShares(this.d - that.d)
}

object DoubleShares {

  val zero = Shares.zero.toDoubleShares

}

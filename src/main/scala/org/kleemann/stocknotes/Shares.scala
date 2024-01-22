package org.kleemann.stocknotes

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
      * @return the rounded number of integer shares left.
      */
    def atMult(currentMultiple: Fraction): Int = (shares * (currentMultiple/multiple).toDouble).toInt

    // TODO: there are a whole bunch of comparison and arithmetic operators that I'm going to hold of on implementing.

    /**
      * Adds two share counts to result in a new share count with the target multiple.
      *
      * @param that the second share count to add
      * @param targetMutiple the multiple of the returned share count
      * @return
      */
    def add(that: Shares, targetMultiple: Fraction): Shares = {
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
    }

    def sub(that: Shares, targetMultiple: Fraction): Shares = {
      add(Shares(-that.shares, that.multiple), targetMultiple)
    }
}

object Shares {
    // I'm going to hold off on implementing the parse fn.
    // It just converts a decimal to a share count and takes a multiple argument.

    val zero = Shares(0, Fraction.one)
}

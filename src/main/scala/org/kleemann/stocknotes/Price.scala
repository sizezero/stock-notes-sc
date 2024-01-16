package org.kleemann.stocknotes

/**
  * Combines the share price with the multiple at the time the share price was significant.
  * E.g., the shares were bought or sold at that multiple.
  * 
  * TODO: This is not really a price it's a SplitAdjustedPrice and not all the clients of this need the functionality.
  * As far as I can tell, it's just Watch. BUY SELL don't seem to care about this value.
  * I'm thinking of refactoring this sooner rather than later but I'm not sure if something actually needs this.
  * Also, if this is _only_ used by Watch then consider merging the atMult() functionality int that class and just 
  * rely on the Currency class.
  *
  * @param price the number of shares at the time of the transaction
  * @param multiple the multiple applied to the shares at the time of the transaction
  */
final case class Price(currency: Currency, multiple: Fraction) {

  /**
    * Multiples change over time. This returns the number of shares at the new multiple.
    * Note: this returns a binary floating point number so there are accuracy limits
    *
    * @param currentMultiple
    * @return a double that represents the number of shares at the current  multiple
    */
    def atMult(currentMultiple: Fraction): Double = currency.toDouble * (multiple/currentMultiple).toDouble
}

object Price {

    val zero = Price(Currency.zero, Fraction.one)

    /**
      * Parses $nnn.nn according to currency parser
      *
      * @param s
      * @param currentMultiple
      * @return
      */
    def parse(s: String, currentMultiple: Fraction): Option[Price] =
        Currency.parse(s).map{ c => Price(c,currentMultiple) }
}

package org.kleemann.stocknotes

sealed trait Watch(low: Option[Currency], high: Option[Currency], multiple: Fraction) {

  /**
    * Multiples change over time. This returns the prices at the new multiple.
    * Note: this returns a binary floating point number so there are accuracy limits
    * 
    * @param currentMultiple
    * @return a double that represents the number of shares at the current  multiple
    */
    def lowAtMult(currentMultiple: Fraction): Option[Currency] = low.map{ c => c * (multiple/currentMultiple).toDouble }

    def highAtMult(currentMultiple: Fraction): Option[Currency] = high.map{ c => c * (multiple/currentMultiple).toDouble }
}

object Watch {
    private val buySellWatch1Pattern = """^(BUY|SELL)\s+(None|none)\s*$""".r
    private val buySellWatch2Pattern = """^(BUY|SELL)\s+([\$\d\.]+)\s*$""".r
    private val buySellWatch3Pattern = """^(BUY|SELL)\s+([\$\d\.]+)\s+([\$\d\.]+)\s*$""".r

    /**
      * Parses lines like the following:
      * (BUY|SELL) $nnn.nnn
      * (BUY|SELL) $nnn.nnn $nnn.nnn
      * (BUY|SELL) (None|none)
      * 
      * @param line the entire line to be parsed
      * @param multiple the current multiple at the time of the entry
      * @return the generated Watch or an error message
      */
    def parse(line: String, multiple: Fraction): Either[String, Watch] = {
        // parse a price and give an optional error message
        def parse(s: String): Either[String, Currency] = {
            val co = Currency.parse(s)
            if (co.isDefined) Right(co.get)
            else Left(s"argument is not a currency value: $s")
        }
        line match {
            case buySellWatch1Pattern("BUY", "None" | "none") => Right(BuyWatch(None, None, multiple))
            case buySellWatch2Pattern("BUY", low) => parse(low).map{ c => BuyWatch(Some(c), None, multiple) }
            case buySellWatch3Pattern("BUY", low, high) =>
                for (
                    lowCurrency <- parse(low);
                    highCurrency <- parse(high))
                yield (BuyWatch(Some(lowCurrency), Some(highCurrency), multiple))
            case buySellWatch1Pattern("SELL", "None" | "none") => Right(SellWatch(None, None, multiple))
            case buySellWatch2Pattern("SELL", low) => parse(low).map{ c => SellWatch(None, Some(c), multiple) }
            case buySellWatch3Pattern("SELL", low, high) =>
                for (
                    lowCurrency <- parse(low);
                    highCurrency <- parse(high))
                yield (SellWatch(Some(lowCurrency), Some(highCurrency), multiple))
            case _ => Left("""buy/sell lines must be one of the following:
                |(BUY|SELL) $nnn.nnn
                |(BUY|SELL) $nnn,nnn $nnn.nnn
                |(BUY|SELL) (None|none)""".stripMargin)
        }
    }

}

/**
  * A note that there is interest at buying a stock and either of the two prices.
  *
  * @param low the lower of the two prices (back in the truck)
  * @param high the higher of the two prices (nibble)
  */
final case class BuyWatch(low: Option[Currency], high: Option[Currency], multiple: Fraction) extends Watch(low, high, multiple)

object BuyWatch {

    val none = BuyWatch(None, None, Fraction.one)

}

/**
  * A note that there is interest at selling a stock and either of the two prices.
  *
  * @param low the lower of the two prices (sell it all)
  * @param high the higher of the two prices (take some profits)
  */
final case class SellWatch(low: Option[Currency], high: Option[Currency], multiple: Fraction) extends Watch(low, high, multiple)

object SellWatch {

    val none = SellWatch(None, None, Fraction.one)

}

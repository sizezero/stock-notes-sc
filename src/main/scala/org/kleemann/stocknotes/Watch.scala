package org.kleemann.stocknotes

sealed trait Watch(low: Option[Price], high: Option[Price])

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
        def parse(s: String): Either[String, Price] = {
            val po = Price.parse(s, multiple)
            if (po.isDefined) Right(po.get)
            else Left(s"argument is not a price value: $s")
        }
        line match {
            case buySellWatch1Pattern("BUY", "None" | "none") => Right(BuyWatch(None, None))
            case buySellWatch2Pattern("BUY", low) => parse(low).map{ p => BuyWatch(Some(p), None) }
            case buySellWatch3Pattern("BUY", low, high) =>
                for (
                    lowPrice <- parse(low);
                    highPrice <- parse(high))
                yield (BuyWatch(Some(lowPrice), Some(highPrice)))
            case buySellWatch1Pattern("SELL", "None" | "none") => Right(SellWatch(None, None))
            case buySellWatch2Pattern("SELL", low) => parse(low).map{ p => SellWatch(None, Some(p)) }
            case buySellWatch3Pattern("SELL", low, high) =>
                for (
                    lowPrice <- parse(low);
                    highPrice <- parse(high))
                yield (SellWatch(Some(lowPrice), Some(highPrice)))
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
final case class BuyWatch(low: Option[Price], high: Option[Price]) extends Watch(low, high)

/**
  * A note that there is interest at selling a stock and either of the two prices.
  *
  * @param low the lower of the two prices (sell it all)
  * @param high the higher of the two prices (take some profits)
  */
final case class SellWatch(low: Option[Price], high: Option[Price]) extends Watch(low, high)

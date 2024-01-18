package org.kleemann.stocknotes

sealed trait Trade(date: Date)

object Trade {

    // There are a lot of limitations to using REs with match/case, this includes no optional groups, no named groups, and no nested groups.
    // In order to get this to work I had to make a worse RE, E.g., a better commision would be (\d?(+\.\d+))
    private val tradeBuySellPattern = """^TRADE\s+(buy|sell)\s+(\d{1,9})@(\S+)\s+balance\s+(\S+)\s+commission\s+(\S+)\s*$""".r
    private val tradeSplitPattern = """^TRADE\s+split\s+(\d{1,9}):(\d{1,9})\s+balance\s+(\S+)\s*$""".r

    /**
      * Parses lines like the following:
      * TRADE buy nnn@nnn.nnn balance nnn commission nnn.nnn
      * TRADE sell nnn@nnn.nnn balance nnn commission nnn.nnn
      * TRADE split nnn:nnn balance nnn
      * 
      * @param line the entire line to be parsed
      * @param date the enclosing date of the entry
      * @param multiple the current multiple at the time of the entry
      * @return (trade, balance) the generated trade and the check for the balance of shares
      */
    def parse(line: String, date: Date, multiple: Fraction): Either[String, (Trade, Shares)] = {
        // tries to convert the price and commission values and produces appropriate errors if necessary
        def parseCurrency(input: String, name: String): Either[String, Currency] =
            Currency.parse(input) match {
                case Some(c) => Right(c)
                case None => Left(s"$name must be decimal floating point: $input")
            }
        line match {
            case tradeBuySellPattern("buy", s, p, b, c) =>
                for {
                    price      <- parseCurrency(p, "price")
                    commission <- parseCurrency(c, "commission")
                } yield (Buy(date, Shares(s.toInt, multiple), price, commission), Shares(b.toInt, multiple))
            case tradeBuySellPattern("sell", s, p, b, c) =>
                for {
                    price      <- parseCurrency(p, "price")
                    commission <- parseCurrency(c, "commission")
                } yield (Sell(date, Shares(s.toInt, multiple), price, commission), Shares(b.toInt, multiple))
            case tradeSplitPattern(numerator, denominator, balance) => {
                // balance is the balance after the new multiple adjustment
                val splitMultiple = Fraction(numerator.toInt, denominator.toInt)
                Right((Split(date, splitMultiple), Shares(balance.toInt, multiple * splitMultiple)))
            }
            case _ => Left("""trade must be one of the following:
                            |TRADE buy nnn@nnn.nnn balance nnn commission nnn.nnn
                            |TRADE sell nnn@nnn.nnn balance nnn commission nnn.nnn
                            |TRADE split nnn:nnn balance nnn""".stripMargin)
        }
    }
}

final case class Buy(date: Date, shares: Shares, price: Currency, commission: Currency) extends Trade(date) {
    // TODO: python has shareAdjust which returns shares plus an integer?
    // TODO: python has returnAdjust which changes both the shares and price by a new multiple and subtracts the commission
    // TODO: python has getters and setters for unsoldShares
}

final case class Sell(date: Date, shares: Shares, price: Currency, commission: Currency) extends Trade(date) {
    // TODO: python has shareAdjust which returns shares plus an integer?
    // TODO: python has returnAdjust which changes both the shares and price by a new multiple and subtracts the commission
}

/**
  * A split that occurs on the number of shares outstanding of a stock.
  * Note wording of splits is usually something like "5 for 1" or in our parser 5:1 which means for each share of stock you own you now have five.
  * Within the split structure this is represented by Fraction(5,1). The result is that, when a split happens, you multiply 
  * shares by the fraction but you divide prices by the fraction.
  *
  * @param date the date of the entry that the split was in
  * @param multiple multiply the pre split shares by this to get the new count of shares.
  */
final case class Split(date: Date, multiple: Fraction) extends Trade(date) {
    // TODO: python has multAdjust that just multiplies a value by multiple and returns it
}

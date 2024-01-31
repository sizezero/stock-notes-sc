package org.kleemann.stocknotes.stock

/**
  * An amount of US dollars. Backed by a Long
  * 
  * We use an integer representation to prevent binary floating point inconsistencies.
  * An Int would have given us a max currency value of around 21 and a half million dollars so I went with a Long.
  * This precisions is used because some stock trades use up to five digits to the right of the decimal e.g.: $9.12345
  *
  * @param milliPennies The number of pennies divided by 1000
  */
final case class Currency private(milliPennies: Long) extends Ordered[Currency] {

    override def compare(that: Currency): Int =
      // can't subtract because we're going from long to int
      if (this.milliPennies == that.milliPennies) 0
      else if (this.milliPennies < that.milliPennies) -1
      else 1

    /**
      * @return the amount in dollars
      */
    def toDouble: Double = milliPennies.toDouble / 100000.0

    def +(that: Currency): Currency = Currency(milliPennies + that.milliPennies)

    def -(that: Currency): Currency = Currency(milliPennies - that.milliPennies)

    /**
      * Displays strings with periods, columns, a dollar sign, and parens for negative values
      * 0 => $0.00
      * 1_234_56 => $1,234.57
      * -99_99 => ($99.99)
      * 
      * Values less than a penny are truncated
      *
      * @return
      */
    override def toString: String = {
        var out = List[Char]()
        val pennies: Long = milliPennies / 1000
        if (pennies < 0) out = ')' :: out
        val pUnpadded = pennies.abs.toString
        // prefix with zeros if less than length 3
        val p = if (pUnpadded.length < 3) "0"*(3-pUnpadded.length) + pUnpadded else pUnpadded
        var i = 0
        for (c: Char <- p.reverse) {
            out = c :: out
            i += 1
            if (i==2) out = '.' :: out
            if (i>2 && i<p.length && (i-2)%3 == 0) out = ',' :: out
        }
        out = '$' :: out
        if (pennies < 0) out = '(' :: out
        out.mkString
    }

    /**
      * Shares have multiples not currencies but sometimes we want to see a price viewed at a different multiple.
      * An example of this is an old purchase price that you want to see what it would be to current shares.
      *
      * @param from the multiple that the currency is associated with
      * @param to the different multiple that we would like to see the price adjusted for
      */
    def priceMultipleAdjust(from: Fraction, to: Fraction): Currency =
      if (from == to) this
      else {
        val m = to / from
        Currency.fromDouble(this.toDouble / m.toDouble)
      }

    /**
      * Gets rid of any sub pennie values.
      *
      * @return millipennies that are a multiple of a thousand. E.g.: whole pennies.
      */
    def truncate: Currency = new Currency(milliPennies - (milliPennies % 1000))
}

object Currency {

    def apply(dollars: Long, cents: Long): Currency = 
      if (dollars<0)
        Currency(-1, -dollars, cents)
      else
        Currency(1, dollars, cents)

    // this variant is needed to produce negative fractions E.g.: Currency(-1, 0, 25)
    def apply(sign: Int, dollars: Long, cents: Long): Currency = {
      if (sign!=1 && sign != -1)      throw new java.lang.ArithmeticException(s"sign must be 1 or -1: $sign")
      else if (cents<0L || cents>99L) throw new java.lang.ArithmeticException(s"cents must be between 0 and 99 inclusive: $cents")
      else if (dollars<0L)            throw new java.lang.ArithmeticException(s"dollars must be non-negative: $dollars")
      else new Currency(sign * (dollars*100_000L + cents*1_000L))
    }

    /**
      * This variant is used for parsing Currency(1, 1) == Currency.decimal(1, "10")
      *
      * @param leftOfDecimal The digits to the left of the decimal point
      * @param rightOfDecimal The digits to the right of the decimal point
      * @return
      */
    private[stock] def decimal(leftOfDecimal: Long, rightOfDecimal: String): Currency =
      if (leftOfDecimal<0)
        decimal(-1, -leftOfDecimal, rightOfDecimal)
      else
        decimal(1, leftOfDecimal, rightOfDecimal)

    private val requiredRightDigits = """^\d{1,5}$""".r

    // this variant is needed to produce negative fractions E.g.: Currency(-1, 0, 25)
    private[stock] def decimal(sign: Int, leftOfDecimal: Long, rightOfDecimal: String): Currency = {
      if (sign != 1 && sign != -1)
        throw new java.lang.ArithmeticException(s"sign must be 1 or -1: $sign")
      else if (!requiredRightDigits.matches(rightOfDecimal))
        throw new java.lang.ArithmeticException(s"rightOfDecimal must be between 0 and 99999 inclusive: $rightOfDecimal")
      else if (leftOfDecimal < 0L)
        throw new java.lang.ArithmeticException(s"leftOfDecimal must be non-negative: $leftOfDecimal")
      else {
        var milliPennies: Long = 0
        var multiplier = 10_000
        rightOfDecimal.foreach{ (digit: Char) =>
          milliPennies = milliPennies + digit.asDigit*multiplier
          multiplier = multiplier / 10
        }
        new Currency(sign * (leftOfDecimal*100_000L + milliPennies))
      }
    }

    /**
      * Obviously lossy but we need it for fractional commission calculations and such
      */
    def fromDouble(d: Double) = new Currency(Math.round(d * 100_000.0))

    val zero = new Currency(0)

    private val numberPattern = """^\$?+(\d{1,17})$""".r
    private val numberPatternWithDecimal = """^\$?+(\d{1,12})\.(\d{1,5})$""".r

      /**
      * Parses [+|-][$][n+]n[.nn]
      * The leading dollar sign is optional
      * If digits exist to the right of the decimal point, there may be one or two.
      * longs can parse up to 19 digits but we only allow up to 17, with decimals 12.
      *
      * @param s
      * @return
      */
    def parse(s: String): Option[Currency] = {
      // scala RE matching doesn like optional matches so we check the sign outside the RE
      val (trimmed, sign) = if (s.length>0) {
        if (s(0)=='+') (s.tail, 1)
        else if (s(0)=='-') (s.tail, -1)
        else (s, 1)
      } else (s, 1)

      trimmed match {
        case numberPattern(n) => Some(new Currency(sign * n.toLong * 100000L))
        case numberPatternWithDecimal(n1, n2) => Some(decimal(sign, n1.toLong, n2))
        case _ => None
      }
    }

}

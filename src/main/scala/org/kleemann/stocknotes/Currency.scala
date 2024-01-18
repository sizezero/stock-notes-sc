package org.kleemann.stocknotes

/**
  * An amount of US dollars. Backed by a Long
  * 
  * We use an integer representation to prevent binary floating point inconsistencies.
  * An Int would have given us a max currency value of around 21 and a half million dollars so I went with a Long.
  * This precisions is used because some stock trades use up to five digits to the right of the decimal e.g.: $9.12345
  *
  * @param milliPennies The number of pennies divided by 1000
  */
final case class Currency private(milliPennies: Long) {

    /**
      * @return the amount in dollars
      */
    def toDouble: Double = milliPennies.toDouble / 100000.0

    /**
      * This is lossy if the multiplier is fractional but is needed by atMult() in higher classes
      *
      * @param that
      * @return
      */
    def *(that: Double): Currency = Currency(math.round(milliPennies * that))

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
}

object Currency {

    def dollarsCents(dollars: Long, cents: Long): Currency = 
      if (dollars<0) dollarsCents(-1, -dollars, cents)
      else dollarsCents(1, dollars, cents)

    // this variant is needed to produce negative fractions
    def dollarsCents(sign: Int, dollars: Long, cents: Long): Currency = {
      if (sign!=1 && sign != -1) throw new java.lang.ArithmeticException(s"sign must be 1 or -1: $sign")
      else if (cents<0L || cents>99L) throw new java.lang.ArithmeticException(s"cents must be between 0 and 99 inclusive: $cents")
      else if (dollars<0L) throw new java.lang.ArithmeticException(s"dollars must be non-negative: $dollars")
      else Currency(sign * ( dollars*100000L + cents*1000L))
    }

    def decimal(leftOfDecimal: Long, rightOfDecimal: Long): Currency =
      if (leftOfDecimal<0) decimal(-1, -leftOfDecimal, rightOfDecimal)
      else decimal(1, leftOfDecimal, rightOfDecimal)

    // this variant is needed to produce negative fractions
    def decimal(sign: Int, leftOfDecimal: Long, rightOfDecimal: Long): Currency = {

      // I need an integer power function
      def pow(base: Int, exp: Int): Int = {
          // this is so ugly
          if (base==0) 1
          else {
              var acc=1
              var count=base
              while(count>0) {
                  acc *= exp
                  count = count - 1
              }
              acc
          }
      }

      if (sign!=1 && sign != -1)
        throw new java.lang.ArithmeticException(s"sign must be 1 or -1: $sign")
      else if (rightOfDecimal<0L || rightOfDecimal>99999L)
        throw new java.lang.ArithmeticException(s"rightOfDecimal must be between 0 and 99999 inclusive: $rightOfDecimal")
      else if (leftOfDecimal<0L)
        throw new java.lang.ArithmeticException(s"leftOfDecimal must be non-negative: $leftOfDecimal")
      else {
        // length has to be at least one; max five
        val l = rightOfDecimal.toString.length
        val mult = pow(5-l, 10)
        Currency(sign * (leftOfDecimal*100000L + rightOfDecimal*mult))
      }
    }

    val zero = Currency(0)

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
        case numberPattern(n) => Some(Currency(sign * n.toLong * 100000L))
        case numberPatternWithDecimal(n1, n2) => Some(decimal(sign, n1.toLong, n2.toLong))
        case _ => None
      }
    }

}

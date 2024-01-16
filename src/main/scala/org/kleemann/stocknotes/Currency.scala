package org.kleemann.stocknotes

/**
  * An amount of US dollars. Backed by a Long
  * 
  * We use an integer representation to prevent binary floating point inconsistencies.
  * An Int would have given us a max currency value of around 21 and a half million dollars so I went with a Long.
  *
  * @param pennies The number of dollars divided by one hundred
  */
final case class Currency(pennies: Long) {

    /**
      * @return the amount in dollars
      */
    def toDouble: Double = pennies.toDouble / 100.0

    /**
      * Displays strings with periods, columns, a dollar sign, and parens for negative values
      * 0 => $0.00
      * 1_234_56 => $1,234.57
      * -99_99 => ($99.99)
      *
      * @return
      */
    override def toString: String = {
        var out = List[Char]()
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

    val zero = Currency(0)

    private val numberPattern = """^\$?+(\d{1,17})$""".r
    private val numberPatternWithPennies = """^\$?+(\d{1,15})\.(\d\d)$""".r

      /**
      * Parses [+|-][$][n+]n[.nn]
      * The leading dollar sign is optional
      * If digits exist to the right of the decimal point, there must be two
      * longs can parse up to 19 digits but we only allow up to 17
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
        case numberPattern(n) => Some(Currency(sign * n.toLong * 100))
        case numberPatternWithPennies(n1, n2) => Some(Currency(sign * (n1+n2).toLong))
        case _ => None
      }
    }

}

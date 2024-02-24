package org.kleemann.stocknotes.calc

import scala.util.matching.Regex

import org.kleemann.stocknotes.Currency


/**
     * Code associated with a single attribute in the attributes class.
     *
     * @param re a regular expression that matches an input line as a candidate for parsing.
     */
private[calc] sealed trait Processor(re: Regex) {

    /**
         * The first token of the parsed line contains the command that is associated with this attribute.
         * If it matches, we should attempt to parse it with this processor.
         */
    def matches(firstToken: String): Boolean = re.matches(firstToken)

    /**
         * Attempt to parse the tokens and return an updated Attributes containing the parsed value.
         * On failure an error message is returned.
         */
    def parse(args: Vector[String], att: Attributes): Either[String, Attributes]

    /**
         * If the attribute wasn't specified on input, then we attempt to generate it.
         * We will likely use the other values in the passed Attribute to generate this value.
         * If the value cannot be generated, then the original Attributes is returned.
         * The method will only overrite the attribute if it is not already defined.
         */
    def generate(att: Attributes): Attributes

    /**
         * Returns the displayable value for the attribute followed by a newline.
         * If the value doesn't exist, then an empty string is returned.
         * If there is a problem with the value, then this string may be an error string followed by a newline.
         * The error string should just be incorporated into the output in place of the value.
         */
    def display(att: Attributes): String
}

private[calc] object Processor {

    // *************** Utility Functions *************** 

    /**
      * If the string ends in M or K then replace the default multiplier with the specified one.
      * Strip the multiple specifier from the resultant string
      */
    private def parsesMegaKilo(s: String, defaultMult: Int): (String, Int) =
        if (s.length() == 0) (s, defaultMult)
        else
            s.last match {
                case 'k' | 'K' => (s.dropRight(1),     1_000)
                case 'm' | 'M' => (s.dropRight(1), 1_000_000)
                case _         => (s,            defaultMult)
           }

    private def parseDollar(s: String, defaultMult: Int): Option[Currency] = {
        val (s2, mult) = parsesMegaKilo(s.trim(), defaultMult)
        val co = Currency.parse(s2.replace(",",""))
        if (mult == 1) co
        else           co.map{ c => Currency.fromDouble(c.toDouble * mult)}
    }

    private def parseSingleArgumentCurrency(attribute: String, args: Vector[String]): Either[String, Currency] = {
        if (args.length != 1) Left(f"$attribute takes a single argument")
        else {
            parseDollar(args(0), 1) match {
                case Some(c) => Right(c)
                case None    => Left(f"can't parse $attribute: ${args(0)}")
            }
        }
    }

    private def parseNumber(s: String, defaultMult: Int): Option[Int] = {
        val (s2, mult) = parsesMegaKilo(s.trim(), defaultMult)
        s2.replace(",","").toIntOption.map{ _ * mult }
    }

    private def parseSingleArgumentDoubleValue(attribute: String, args: Vector[String]): Either[String, Double] =
        if (args.length != 1) Left(f"$attribute takes a single argument")
        else
            args(0).toDoubleOption match {
                case Some(d) => Right(d)
                case None    => Left(f"can't parse $attribute: ${args(0)}")
            }

    private def parseIncomeOrRevenue(args: Vector[String]): Either[String, Currency] = {
        if (args.length > 4) Left("can't have more than four quarters of values")
        else {
            val parsed: Vector[Either[String,Currency]] = args.map{ s => parseDollar(s, 1_000) match {
                case Some(c) => Right(c)
                case None => Left(f"failed to parse quarterly value: $s")
            }}
            if (parsed.find{ _.isLeft }.isDefined) {
                val cumulativeErrors = parsed.collect{ case Left(s) => s }.mkString("\n")
                Left(cumulativeErrors)
            } else {
                val sum = parsed.collect{ case Right(c) => c }.reduce{ _ + _ }
                val annualized: Currency = Currency.fromDouble(sum.toDouble * (4.0/args.length))
                Right(annualized)
            }
        }
    }

    // *************** Attribute Processors *************** 

    private object Income extends Processor("^inc(ome)?$".r) {

        override def parse(args: Vector[String], att: Attributes): Either[String, Attributes] =
            parseIncomeOrRevenue(args).map{ c => att.copy(income = Some(c)) }

        override def generate(att: Attributes): Attributes =
            if (att.income.isDefined) att
            else if (att.eps.isDefined && att.shares.isDefined) {
                val income: Currency = Currency.fromDouble(att.eps.get.toDouble * att.shares.get)
                att.copy(income = Some(income))
            } else att
            
        override def display(att: Attributes): String = att.income match {
            case Some(c) => f"Income ${c.toDouble/1_000_000}%.1fM\n"
            case None    => ""
        }
    }

    private object Revenue extends Processor("^rev(enue)?$".r) {

        override def parse(args: Vector[String], att: Attributes): Either[String, Attributes] =
            parseIncomeOrRevenue(args).map{ c => att.copy(revenue = Some(c)) }

        override def generate(att: Attributes): Attributes =
            if (att.revenue.isDefined) att
            else if (att.income.isDefined && att.margins.isDefined) {
                val revenue: Currency = Currency.fromDouble(att.income.get.toDouble / att.margins.get)
                att.copy(revenue = Some(revenue))
            } else att

        override def display(att: Attributes): String = att.revenue match {
            case Some(c) => f"Revenue ${c.toDouble/1_000_000}%.1fM\n"
            case None    => ""
        }
    }

    private object Eps extends Processor("^eps$".r) {

        override def parse(args: Vector[String], att: Attributes): Either[String, Attributes] = 
            parseSingleArgumentCurrency("eps", args).map{ d => att.copy(eps = Some(d)) }

        override def generate(att: Attributes): Attributes = {
            if (att.eps.isDefined) att
            else if (att.income.isDefined && att.shares.isDefined) {
                val eps: Currency = Currency.fromDouble(att.income.get.toDouble / att.shares.get)
                att.copy(eps = Some(eps))
            } else att
        }

        override def display(att: Attributes): String = att.eps match {
            case Some(c) => f"EPS $c\n"
            case None    => ""
        }
    }

    private object Shares extends Processor("^shares$".r) {

        override def parse(args: Vector[String], att: Attributes): Either[String, Attributes] = {
            if (args.length==1 || (args.length==2 && args(1)=="(diluted)")) {
                val arg = args(0)
                parseNumber(arg, 1) match {
                    case Some(i) => Right(att.copy(shares = Some(i)))
                    case None    => Left(f"can't parse share argument: $arg")
                }
            } else Left("incorrect number of share arguments")
        }

        override def generate(att: Attributes): Attributes =
            if (att.shares.isDefined) att
            else if (att.price.isDefined && att.marketCap.isDefined) {
                val shares: Int = (att.marketCap.get.toDouble / att.price.get.toDouble).toInt
                att.copy(shares = Some(shares))
            } else if (att.income.isDefined && att.eps.isDefined) {
                val shares: Int = (att.income.get.toDouble / att.eps.get.toDouble).toInt
                att.copy(shares = Some(shares))
            } else att


        override def display(att: Attributes): String = att.shares match {
            case Some(i) => if (i < 0) "WTF? negative shares\n"
                            else f"Shares $i\n"
            case None => ""
        }
    }

    private object Pe extends Processor("^pe$".r) {

        override def parse(args: Vector[String], att: Attributes): Either[String, Attributes] = 
            parseSingleArgumentDoubleValue("pe", args).map{ d => att.copy(pe = Some(d)) }

        override def generate(att: Attributes): Attributes = 
            if (att.pe.isDefined) att
            else if (att.price.isDefined && att.eps.isDefined) {
                val d = att.price.get.toDouble / att.eps.get.toDouble
                att.copy(pe = Some(d))
            } else if (att.marketCap.isDefined && att.income.isDefined) {
                val d = att.marketCap.get.toDouble / att.income.get.toDouble
                att.copy(pe = Some(d))
            } else att

        override def display(att: Attributes): String = att.pe match {
            case Some(d) => if (d < 0) "N/A\n"
                            else f"PE $d%.1f\n"
            case None => ""
        }
    }

    private object MarketCap extends Processor("^mc$".r) {

        override def parse(args: Vector[String], att: Attributes): Either[String, Attributes] = 
            parseSingleArgumentCurrency("mc", args).map{ c => att.copy(marketCap = Some(c)) }

        override def generate(att: Attributes): Attributes = 
            if (att.marketCap.isDefined) att
            else if (att.price.isDefined && att.shares.isDefined) {
                val c = Currency.fromDouble(att.price.get.toDouble * att.shares.get)
                att.copy(marketCap = Some(c))
            } else att

        override def display(att: Attributes): String = att.marketCap match {
            case Some(c) => {
                val d = c.toDouble
                if (d < 0.0) "WTF? negative market cap\n"
                else f"MC ${d/1_000_000}%.1fM\n"
            }
            case None => ""
        }
    }

    private object Price extends Processor("^price$".r) {

        override def parse(args: Vector[String], att: Attributes): Either[String, Attributes] =
            parseSingleArgumentCurrency("price", args).map{ c => att.copy(price = Some(c)) }

        override def generate(att: Attributes): Attributes = 
            if (att.price.isDefined) att
            else if (att.marketCap.isDefined && att.shares.isDefined) {
                val c = Currency.fromDouble(att.marketCap.get.toDouble / att.shares.get.toDouble)
                att.copy(price = Some(c))
            } else if (att.pe.isDefined && att.eps.isDefined) {
                val c = Currency.fromDouble(att.pe.get.toDouble * att.eps.get.toDouble)
                att.copy(price = Some(c))
            } else att

        override def display(att: Attributes): String = att.price match {
            case Some(c) => f"Price $c\n"
            case None    => ""
        }

    }

    private object Margins extends Processor("^margins$".r) {

        override def parse(args: Vector[String], att: Attributes): Either[String, Attributes] = 
            parseSingleArgumentDoubleValue("margins", args).map{ d => att.copy(margins = Some(d)) }

        override def generate(att: Attributes): Attributes = 
            if (att.margins.isDefined) att
            else if (att.income.isDefined && att.revenue.isDefined) {
                val i = att.income.get.toDouble
                val r = att.revenue.get.toDouble
                if (i<0.0 || r<0.0) att
                else att.copy(margins = Some(i/r))
            } else att

        override def display(att: Attributes): String = att.margins match {
            case Some(d) => f"Margin ${d*100}%.2f%%\n"
            case None    => ""
        }
    }

    private object Dividend extends Processor("^div$".r) {

        override def parse(args: Vector[String], att: Attributes): Either[String, Attributes] = 
            parseSingleArgumentCurrency("dividend", args).map{ c => att.copy(dividend = Some(c)) }

        override def generate(att: Attributes): Attributes =
            if (att.dividend.isDefined) att
            else if (att.dividendYield.isDefined && att.price.isDefined) {
                val c = Currency.fromDouble(att.dividendYield.get * att.price.get.toDouble)
                att.copy(dividend = Some(c))
            } else att

        override def display(att: Attributes): String = att.dividend match {
            case Some(c) => f"Dividend $c\n"
            case None    => ""
        }
    }

    private object DividendYield extends Processor("^yield$".r) {

        override def parse(args: Vector[String], att: Attributes): Either[String, Attributes] =
            parseSingleArgumentDoubleValue("yield", args).map{ d => att.copy(dividendYield = Some(d)) }

        override def generate(att: Attributes): Attributes =
            if (att.dividendYield.isDefined) att
            else if (att.dividend.isDefined && att.price.isDefined) {
                val d = att.dividend.get.toDouble / att.price.get.toDouble
                att.copy(dividendYield = Some(d))
            } else att

        override def display(att: Attributes): String = att.dividendYield match {
            case Some(d) => f"Dividend Yield ${d*100}%.2f%%\n"
            case None    => ""
        }
    }

    private object PayoutRatio extends Processor("^payout$".r) {

        override def parse(args: Vector[String], att: Attributes): Either[String, Attributes] =
            Left("Payout Ratio is not allowed as input.")

        override def generate(att: Attributes): Attributes =
            if (att.payoutRatio.isDefined) att
            else if (att.dividend.isDefined && att.eps.isDefined) {
                val d = att.dividend.get.toDouble / att.eps.get.toDouble
                att.copy(payoutRatio = Some(d))
            } else att

        override def display(att: Attributes): String = att.payoutRatio match {
            case Some(d) => f"Payout Ratio ${d*100}%.2f%%\n"
            case None    => ""
        }
    }

    private object Help extends Processor("^help$".r) {

        override def parse(args: Vector[String], att: Attributes): Either[String, Attributes] =
            Right(att.copy(help = Some(true)))

        override def generate(att: Attributes): Attributes = att

        override def display(att: Attributes): String =
            if (att.help.isDefined)
                """
                |inc <quarter1>  <quarter2> <quarter3> <quarter4>
                |  annual income
                |  Default values are in thousands but may be modified by a K or M suffix.
                |  If one or more quarters are missing then the annual income is considered the average of the specified quarters.
                |  generated by eps * shares
                |rev <quarter1>  <quarter2> <quarter3> <quarter4>
                |  annual revenue
                |  Default values are in thousands but may be modified by a K or M suffix.
                |  If one or more quarters are missing then the annual income is considered the average of the specified quarters.
                |  not generated
                |eps <currency>
                |  earnings per share
                |  generated by income / shares
                |shares <number>
                |  total shares outstanding
                |  May be modified by a K or M suffix.
                |  not generated
                |pe <double>
                |  price earnings ratio
                |  generated by price / eps
                |  generated by mc / income
                |mc <currency>
                |  total market capitalization
                |  May be modified by a K or M suffix.
                |  generated by price * shares
                |price <currency>
                |  price per share
                |  generated by mc / shares
                |  generated by pe / eps
                |margins
                |  not allowed as input
                |  generated by income / revenue
                |div <currency>
                |  annual dividend per share
                |  generated by nothing
                |yield
                |  annual dividends yield
                |  not allowed as input
                |  generated by div / price
                |payout ratio
                |  not allowed as input
                |  generated by div / eps
                |""".stripMargin
            else
                ""
    }

    /**
      * This is the primary way processors are accessed outside this class.
      */
    private[calc] val all: List[Processor] = List(
        Income,
        Revenue,
        Shares,
        Eps,
        Pe,
        MarketCap,
        Price,
        Margins,
        Dividend,
        DividendYield,
        PayoutRatio,
        Help
    )
}

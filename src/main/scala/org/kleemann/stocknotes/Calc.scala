package org.kleemann.stocknotes

import scala.util.control.Breaks.{break, breakable}
import scala.util.matching.Regex

import org.kleemann.stocknotes.stock.Currency
import scala.collection.mutable

object Calc {

    private sealed trait Processor(re: Regex) {

        /**
          * The first token of the parsed line contains the command that is associated with this attribute.
          * If it matches, we should attempt to parse it with this processor.
          */
        def matches(firstToken: String): Boolean = re.matches(firstToken)

        /**
          * Attempt to parse the tokens and return an updated Attributes with the parsed value.
          * On failure an error message is returned.
          */
        def parse(args: Array[String], att: Attributes): Either[String, Attributes]

        /**
          * If the attribute wasn't specified on input, then we attempt to generate it.
          * We will likely use the other values to generate this value.
          * If the value cannot be generated, then the original Attributes is returned.
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

    /**
      * If the string ends in M or K then replace the default multiplier with the specified one.
      * Strip the multiple specifier from the resultant string
      */
    private def parsesMegaKilo(s: String, defaultMult: Int): (String, Int) =
        if (s.length() == 0) (s, defaultMult)
        else {
            s.last match {
                case 'k' | 'K' => (s.dropRight(1),     1_000)
                case 'm' | 'M' => (s.dropRight(1), 1_000_000)
                case _         => (s,            defaultMult)
           }
        }

    private def parseDollar(s: String, defaultMult: Int): Option[Currency] = {
        val (s2, mult) = parsesMegaKilo(s.strip(), defaultMult)
        val co = Currency.parse(s2)
        if (mult == 1) co
        else           co.map( c => Currency.fromDouble(c.toDouble * mult))
    }

    private object Income extends Processor("^inc(ome)?$".r) {

        override def parse(args: Array[String], att: Attributes): Either[String, Attributes] = {
            if (args.length > 4) Left("income can't have more than four quarters")
            else {
                val parsed: Array[Either[String,Currency]] = args.map{ s => parseDollar(s, 1_000) match {
                    case Some(c) => Right(c)
                    case None => Left(f"failed to parse income number: $s")
                }}
                if (parsed.find{ _.isLeft }.isDefined) {
                    val cumulativeErrors = parsed.collect{ case Left(s) => s }.mkString("\n")
                    Left(cumulativeErrors)
                } else {
                    val sum = parsed.collect{ case Right(c) => c }.reduce{ _ + _ }
                    val annualized: Currency = Currency.fromDouble(sum.toDouble * (4.0/args.length))
                    Right(att.copy(income=Some(annualized)))
                }
            }
        }

        override def generate(ats: Attributes): Attributes = ats
            
        override def display(ats: Attributes): String = ats.income match {
            case Some(income) => income.toString + "\n"
            case None         => ""
        }
    }

    private case class Attributes(

        income: Option[Currency] = None
    )


    private val processors: List[Processor] = List(
        Income
    )

    def calc(it: Iterator[String]): String = {

        // loop through input and parse every line
        // correctly parsed values are accumulated in parsedAttributes
        // parse failures are accumulated in reverseParseErrors
        val ignore = """^(\s*|=+>)$""".r
        val tokenDelimiters = """[\s:=]+""".r
        val (parsedAttributes, reverseParseErrors): (Attributes, List[String]) =
            it.filter{ s => !ignore.matches(s) } // each line is now guaranteed to match at least one token
            .map{ s => tokenDelimiters.split(s.strip()) } // convert lines to scala Array of String tokens
            // from tokens, accumulate Attribute values and error strings
            .foldLeft((Attributes(), List[String]())){ case ((att: Attributes, errors: List[String]), args: Array[String]) => 
                val attribute = args.head // we have at least one token
                val rest = args.tail
                // find the first processor that can handle the args
                processors.find{ _.matches(attribute) } match {
                    case Some(p) => p.parse(rest, att) match {
                        case Right(updatedA) => (updatedA, errors)
                        case Left(e)         => (att,      e :: errors)
                    }
                    case None => (att, "no processor found to parse line" :: errors)
                }
            }

        // if we have any errors we need to stop
        if (!reverseParseErrors.isEmpty) reverseParseErrors.reverse.mkString("\n") + "\n"
        else {

            // conflicts
            // one value in each set has to be unspecified
            // TODO

            // if values are missing then attempt to generate them
            val cumulativeAttributes = processors.foldLeft(parsedAttributes){ case (a, p) => p.generate(a) }

            // display the values we can, there may be errors embedded in the result
            processors.foldLeft(mutable.StringBuilder()){ case (sb, p) => sb ++= p.display(cumulativeAttributes) }.result
        }
    }
}

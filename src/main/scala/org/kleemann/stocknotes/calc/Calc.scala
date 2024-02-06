package org.kleemann.stocknotes.calc

import scala.collection.mutable
import scala.annotation.tailrec

object Calc {

    /**
      * Takes the multi line input and parses it either successfully into an Attributes object that contains
      * both parsed and generated attributes, or returns a list of Errors. This version of calc
      * that returns objects instead of a monolithic String, is good for testing.
      *
      * @param it
      * @return
      */
    private[calc] def inputToAttributes(it: Iterator[String]): Either[List[String], Attributes] = {

        // loop through input and parse every line
        // correctly parsed values are accumulated in parsedAttributes
        // parse failures are accumulated in reverseParseErrors
        val ignore = """^(\s*|=+>)$""".r
        val tokenDelimiters = """[\s:=]+""".r
        val (parsedAtt, reverseParseErrors): (Attributes, List[String]) =
            it.filter{ s => !ignore.matches(s) } // each line is now guaranteed to match at least one Processor
            .map{ s => tokenDelimiters.split(s.trim()).toVector } // convert lines to tokens of strings in a scala Vector
            // from tokens, accumulate Attribute values and error strings
            .foldLeft((Attributes(), List[String]())){ case ((att: Attributes, errors: List[String]), args: Vector[String]) => 
                val attributeToken = args.head // we have at least one token
                val rest = args.tail
                // find the first processor that can handle the args
                Processor.all.find{ _.matches(attributeToken) } match {
                    case Some(proc) => proc.parse(rest, att) match {
                        case Right(updated) => (updated, errors)
                        case Left(e)        => (att,     e :: errors)
                    }
                    case None =>               (att,     f"no processor found to parse line: $attributeToken" :: errors)
                }
            }

        // if we have any parse errors we need to stop
        if (!reverseParseErrors.isEmpty) Left(reverseParseErrors.reverse)
        else {

            // one value in each set has to be unspecified
            // TODO: this is not a foolproof check since it doesn't check transitive stuff
            // e.g. specifying shares, income would produce eps
            // and specifying price and pe would produce eps
            // thus shares, income, price, and pe could be in conflict
            // TODO: this almost seems like a property of processors, not something that should be in this function
            val conflicts: List[(String, List[Option[Any]])] = List(
                ("shares, income, eps",        List(parsedAtt.shares,   parsedAtt.income,        parsedAtt.eps)),
                ("price, eps, pe",             List(parsedAtt.price,    parsedAtt.eps,           parsedAtt.pe)),
                ("shares, price, marketCap",   List(parsedAtt.shares,   parsedAtt.price,         parsedAtt.marketCap)),
                ("income, revenue, margins",   List(parsedAtt.income,   parsedAtt.revenue,       parsedAtt.margins)),
                ("dividend, yield, price",     List(parsedAtt.dividend, parsedAtt.dividendYield, parsedAtt.eps)),
                ("dividend, eps, payoutRatio", List(parsedAtt.dividend, parsedAtt.eps,           parsedAtt.payoutRatio))
            )
            val reverseConflictErrors: List[String] = conflicts.foldLeft( List[String]() ){ case (errors, (msg, conflict)) => 
                if (conflict.count{ _.isDefined } == conflict.length) f"all cannot be specified: $msg" :: errors
                else                                                  errors
            }

            if (!reverseConflictErrors.isEmpty) Left(reverseConflictErrors) // there's no real order to conflicts so leave them reversed
            else {

                // if values are missing then attempt to generate them
                @tailrec
                def generate(prev: Attributes): Attributes = {
                    // run through all the generators
                    val next = Processor.all.foldLeft(prev){ (att, proc) => proc.generate(att) }
                    // if anything changed then run through it again until there are no changes
                    if (next == prev) prev
                    else              generate(next)
                }
                val cumulativeAtt = generate(parsedAtt)

                Right(cumulativeAtt)
            }
        }
    }

    /**
      * Given an attributes object, display it as a string.
      *
      * @param att
      * @return
      */
    private[calc] def attributesToString(att: Attributes): String =
        // display the values we can, there may be errors embedded in the result
        Processor.all.foldLeft(mutable.StringBuilder()){ (sb, proc) => sb ++= proc.display(att) }.result

    /**
      * The main entrypoint of the program.
      * Turns input lines of attributes into a multiline output string of calculated attributes.
      *
      * @param it
      * @return
      */
    def calc(it: Iterator[String]): String = {
        inputToAttributes(it) match {
            case Right(att) => attributesToString(att)
            case Left(errors) => errors.mkString("\n") + "\n"
        }
    }
}

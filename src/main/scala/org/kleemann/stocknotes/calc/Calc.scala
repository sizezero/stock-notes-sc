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

        Attributes.parse(it) match {
            case Left(errors) => Left(errors)
            case Right(att) => {
                // one value in each set has to be unspecified
                // TODO: this is not a foolproof check since it doesn't check transitive stuff
                // e.g. specifying shares, income would produce eps
                // and specifying price and pe would produce eps
                // thus shares, income, price, and pe could be in conflict
                // TODO: this almost seems like a property of processors, not something that should be in this function
                val conflicts: List[(String, List[Option[Any]])] = List(
                    ("shares, income, eps",        List(att.shares,   att.income,        att.eps)),
                    ("price, eps, pe",             List(att.price,    att.eps,           att.pe)),
                    ("shares, price, marketCap",   List(att.shares,   att.price,         att.marketCap)),
                    ("income, revenue, margins",   List(att.income,   att.revenue,       att.margins)),
                    ("dividend, yield, price",     List(att.dividend, att.dividendYield, att.eps)),
                    ("dividend, eps, payoutRatio", List(att.dividend, att.eps,           att.payoutRatio))
                )
                val reverseConflictErrors: List[String] = conflicts.foldLeft( List[String]() ){ case (errors, (msg, conflict)) => 
                    if (conflict.count{ _.isDefined } == conflict.length) f"all cannot be specified: $msg" :: errors
                    else                                                  errors
                }

                if (reverseConflictErrors.isEmpty)
                    Right(att.generate)
                else
                    Left(reverseConflictErrors) // there's no real order to conflicts so leave them reversed
            }
        }
    }

    /**
      * The main entrypoint of the program.
      * Turns input lines of attributes into a multiline output string of calculated attributes.
      *
      * @param it
      * @return
      */
    def calc(it: Iterator[String]): String =
        inputToAttributes(it) match {
            case Right(att) => att.toString()
            case Left(errors) => errors.mkString("\n") + "\n"
        }

}

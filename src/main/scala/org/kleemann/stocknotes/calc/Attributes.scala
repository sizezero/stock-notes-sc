package org.kleemann.stocknotes.calc

import scala.annotation.tailrec
import scala.collection.mutable

import org.kleemann.stocknotes.Currency

/**
 * This is the list of all possible attributes both parsed and generated.
 * Any attribute may be missing.
 *
 * @param income annual income in dollars
 * @param revenue annual revenue in dollars
 * @param shares share count, hopefully fully diluted
 * @param eps earnings per share in dollars
 * @param pe price earnings multiple
 * @param marketCap total market capitalization in dollars
 * @param price price per share in dollars
 * @param margins net margins: income/revenue
 * @param dividend annual dividend per share
 * @param dividendYield annual dividend yield
 * @param payoutRatio dividend / income
 * @param help not really an attribute, if defined, indicates that help should be displayed
 */
private final case class Attributes(
        income:        Option[Currency] = None,
        revenue:       Option[Currency] = None,
        shares:        Option[Int]      = None,
        eps:           Option[Currency] = None,
        pe:            Option[Double]   = None,
        marketCap:     Option[Currency] = None,
        price:         Option[Currency] = None,
        margins:       Option[Double]   = None,
        dividend:      Option[Currency] = None,
        dividendYield: Option[Double]   = None,
        payoutRatio:   Option[Double]   = None,
        help:          Option[Boolean]  = None
    ) {

    /**
         * Attempts to generate None values from other Some values via Processor.generate()
         */
    def generate: Attributes = {
        @tailrec
        def recurse(prev: Attributes): Attributes = {
            // run through all the generators
            val next = Processor.all.foldLeft(prev){ (att, proc) => proc.generate(att) }
            // if anything changed then run through it again until there are no changes
            if (next == prev) prev
            else              recurse(next)
        }
        recurse(this)
    }


    /**
         * Displays all the non-None values via Processor.display().
         * There may be some errors embedded in the result.
         * This is the value we want inserted into the user's Stock document.
         *
         * @return A multi-line String.
         */
    override def toString(): String =
        Processor.all.foldLeft(mutable.StringBuilder()){ (sb, proc) => sb ++= proc.display(this) }.result

}

object Attributes {

    private val ignore = """^(\s*|=+>)$""".r
    private val tokenDelimiters = """[\s:=]+""".r

    /**
      * Parse multiple lines of input text as Attribute values.
      * Returns either the parse attribute or a list of errors.
      *
      * @param it lines of input
      * @return
      */
    def parse(it: Iterator[String]): Either[List[String], Attributes] = {
        val (parsedAtt, reverseParseErrors): (Attributes, List[String]) =
            it.withFilter{ s => !ignore.matches(s) } // each line is now guaranteed to match at least one Processor
            .map{ s => tokenDelimiters.split(s.trim()).toVector } // convert lines to tokens of strings in a scala Vector
            // from tokens, accumulate Attribute values and error strings
            .foldLeft((Attributes(), List[String]())){ case ((att: Attributes, errors: List[String]), args: Vector[String]) => 
                val attributeToken = args.head // due to previous filter we have at least one token
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
        if (reverseParseErrors.isEmpty) Right(parsedAtt)
        else                            Left(reverseParseErrors.reverse)
    }

}
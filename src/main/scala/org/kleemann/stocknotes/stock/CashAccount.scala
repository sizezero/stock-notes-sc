package org.kleemann.stocknotes.stock

import scala.annotation.tailrec
import scala.util.control.Breaks.{break, breakable}

import org.kleemann.stocknotes.{Config, Currency, Date}

/**
  * Represents cash and cash equivalents in an investment account
  *
  * @param accountName the name of the account, very similar to a ticker value
  * @param date the last date in the cash account
  * @param balance the last balance in the cash account, this value is in pennies so it fits in an integer
  */
final case class CashAccount(accountName: String, date: Date, balance: Currency)

object CashAccount {

    /**
      * Loads all cash accounts from standard cash directory
      * Errors in parsing will result in a call to sys.exit(1)
      *
      * @param config the global configuration
      * @return
      */
    def load(config: Config): List[CashAccount] =
      if (!os.isDir(config.cashDir)) Nil
      else {
        os.list(config.cashDir).flatMap { f =>
          if (f.ext != "txt") Nil
          else {
            val accountName = f.baseName
            load(accountName, f.toString(), os.read.lines.stream(f)) match {
              case Right(ca) => List(ca)
              case Left(error) => {
                println(error)
                sys.exit(1)
              }
            }
          }
        }.toList
      }

    private val balancePattern = """^BALANCE:\s*\$([\d,]+)\.(\d{2})$""".r

    /**
      * Loads a cash account from the specified generator. This is testable.
      * Problems with parsing the file are reported in Left[String]
      *
      * @param accountName the name used to construct the possibly returned CashAccount
      * @param cashFile the string representation of the input file used for error reporting
      * @param g the generator that produces all the text lines to parse
      */
    private[stock] def load(accountName: String, cashFile: String, g: os.Generator[String]): Either[String, CashAccount] = {

        def mkError(lineNo: Int, s: String): Either[String, CashAccount] = Left(s"$cashFile($lineNo): $s")

        @tailrec
        def processLine(in: Seq[String], prevLineNo: Int, date: Option[Date], balance: Option[Currency]): Either[String, CashAccount] = {
            if (in.isEmpty) {
              date match {
                case None => mkError(prevLineNo, "no date entered")
                case Some(d) => {
                  balance match {
                    case None => mkError(prevLineNo, "no balance entered")
                    case Some(b) => Right(CashAccount(accountName, d, b))
                  }
                }
              }
            } else {
                val line = in.head
                val lineNo = prevLineNo + 1
                Date.parse(line) match {
                    case Some(d: Date) =>
                      if (date.isEmpty) processLine(in.tail, lineNo, Some(d), balance)
                      else mkError(lineNo, "date entered a second time")
                    case _ => {
                        // continue parsing
                        line match {
                            case balancePattern(dollars, cents) => {
                                if (balance.isDefined) mkError(prevLineNo, "balance defined more than once")
                                else {
                                  val newBalance = Currency(dollars.replace(",","").toLong, cents.toLong)
                                  processLine(in.tail, lineNo, date, Some(newBalance))
                                }
                            }
                            case _ => processLine(in.tail, lineNo, date, balance)
                        }
                    }
                }
            }
        }
        processLine(g.toSeq, 0, None, None)
    }
}
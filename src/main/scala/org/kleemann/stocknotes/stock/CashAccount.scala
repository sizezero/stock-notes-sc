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
      *
      * @param config the global configuration
      * @return
      */
    def load(config: Config): List[CashAccount] =
      os.list(config.cashDir).flatMap { f =>
        if (f.ext == "txt") {
          val accountName = f.baseName
          List(load(accountName, f))
        } else Nil
      }.toList

    /**
      * Loads the specified cash account. Errors in loading will call sys.exit(1)
      *
      * @param dir
      * @return
      */
    private def load(accountName: String, cashFile: os.Path): CashAccount = {
      val e = load(accountName, cashFile.toString, os.read.lines.stream(cashFile))
      e match {
        case Right(ca) => ca
        case Left(error) => {
          println(error)
          sys.exit(1)
        }
      }
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
    private[stock] def load(accountName: String, cashFile: String, g: os.Generator[String]): Either[String, CashAccount] = 
      loadFunctional(accountName, cashFile, g)

    private[stock] def loadImperative(accountName: String, cashFile: String, g: os.Generator[String]): Either[String, CashAccount] = {
      // oh god, this is some old school iteration
      var lineNo = 0
      var curDate: Date = null
      var curBalance: Currency = null
      var error: String = null
      breakable {
        g.foreach { line =>
          lineNo = lineNo + 1
          Date.parse(line) match {
            case Some(d) => {
              if (curDate != null) { 
                error = s"$cashFile($lineNo): date entered a second time"
                break
              }
              else curDate = d
            }
            case None => {
              line match {
                case balancePattern(dollars, cents) => {
                  curBalance = Currency(dollars.replace(",","").toLong, cents.toLong)
                }
                case _ => 
              }
            }
          }
        }
      }
      if (error != null) Left(error)
      else if (curDate == null) Left(s"$cashFile: no date entered")
      else if (curBalance == null) Left(s"$cashFile: no balance entered")
      else Right(CashAccount(accountName, curDate, curBalance))
    }

    private[stock] def loadFunctional(accountName: String, cashFile: String, g: os.Generator[String]): Either[String, CashAccount] = {

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
                                val newBalance = Currency(dollars.replace(",","").toLong, cents.toLong)
                                processLine(in.tail, lineNo, date, Some(newBalance))
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
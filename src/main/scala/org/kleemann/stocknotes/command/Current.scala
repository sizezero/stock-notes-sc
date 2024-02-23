package org.kleemann.stocknotes.command

import org.kleemann.stocknotes.report.{Gain => ReportGain}
import org.kleemann.stocknotes.{Config, Quote, Ticker}
import org.kleemann.stocknotes.stock.{CashAccount, Currency, Date, Stock}
import org.kleemann.stocknotes.report.Gain.StockReport

object Current extends Command {

  val help = s"""current [ <ticker> ]
  |""".stripMargin

  private[command] def parse(args: IndexedSeq[String]): Either[String, Option[Ticker]] =
    if (args.length == 0) Right(None)
    else if (args.length == 1) Right(Some(Ticker(args(0))))
    else Left("extra arguments detected")

  /**
   * The non-functional part of the code.
   */
  private def current(oticker: Option[Ticker]): Unit = {
    val config = Config.load()
    val ss: List[Stock] = Stock.load(config)
    val stocks: Map[Ticker,Stock] = ss.map{ s => s.ticker -> s }.toMap
    val quotes: Map[Ticker, Quote] = Quote.load(config)
    val cash: List[CashAccount] = CashAccount.load(config)

    oticker match {
      case Some(ticker) => {
        if (!stocks.isDefinedAt(ticker)) {
          println(s"specified ticker does not have a log file: $ticker")
          sys.exit(1)
        }
      }
      case _ => ()
    }

    val commission = Currency(30,0)
    val srs: List[ReportGain.StockReport] = ReportGain.createCurrent(oticker, ss, cash, quotes, commission, Date.today)
    print(ReportGain.render(srs))
  }

  override def command(args: IndexedSeq[String]): Option[String] = {
    parse(args) match {
      case Right(oticker) => {
        current(oticker)
        None
      }
      case Left(error) => Option(error)
    }
  }
}
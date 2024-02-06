package org.kleemann.stocknotes.command

import org.kleemann.stocknotes.report.{Gain => ReportGain}
import org.kleemann.stocknotes.{Config, Quote, Ticker}
import org.kleemann.stocknotes.stock.{CashAccount, Currency, Date, Stock}

object Gain extends Command {

  val help = s"""gain [-omit <ticker> ] (<year>[:<year>] | <commission>) [ <ticker> <ticker> ... ]
  |""".stripMargin

  /**
    * The arguments either take a date range OR a commission. If I modelled this with Either, I would lose the ability to name the arguments.
    * Leaving this alone for now.
    *
    * @param start
    * @param end
    * @param commission
    * @param tickers
    * @param omitKeyword
    */
  case class ParseArgs(start: Date, end: Date, commission: Currency, tickers: List[Ticker], omitKeyword: Option[String]) {

    /**
      * There are two modes for the gain report:
      * current value mode: creates fake sells for each stock dates today and simulates the underlying value of all stocks
      * historical mode: takes a date arguments and shows the gains of actual sales in the past
      * 
      * TODO: the current way to distinguish the modes is with the currency set to -100, a trait and two case classes would probably be a better
      * way to do this.
      *
      * @return true if the parse args represents the current value mode
      */
    def isCurrentValueMode: Boolean = commission != Currency(-100, 0)
  }

  /** 
    * 
    *
    * @param args
    * @return
    */
  private[command] def parse(args: IndexedSeq[String]): Either[String, ParseArgs] = {

    val (args2: IndexedSeq[String], omitKeyword: Option[String]) = 
      if (args.length >= 2 && args(0)=="-omit") (args.drop(2), Some(args(1)))
      else (args, None)

    // default sell with a commision of 30
    val args3: IndexedSeq[String] =
      if (args2.length==0) Vector("30")
      else args2

    // TODO: figure out why these can't be dropped in place
    val yearPattern = """^(\d{4})$""".r
    val twoYearPattern = """^(\d{4}):(\d{4})$""".r
    val commissionPattern = """^\d+(\.\d+)?$""".r

    // args3 is now guaranteed to have at least one element
    val (start: Date, end: Date, commission: Currency) =
      if (args3(0)==":") (Date.earliest, Date.latest, Currency(-100, 0))
      else args3(0).match {
        case yearPattern(y)        => (Date.earliest( y.toInt).get, Date.latest( y.toInt).get, Currency(-100, 0))
        case twoYearPattern(y1,y2) => (Date.earliest(y1.toInt).get, Date.latest(y2.toInt).get, Currency(-100, 0))
        case commissionPattern(d)  => (Date.earliest, Date.earliest, Currency.parse(args3(0)).get) // dates don't matter for this case
        case _                     => return Left(help) //this looks non-functional
      }

    val args4 = args3.drop(1)

    val tickers: List[Ticker] = args4.map{ Ticker(_) }.sorted.toList

    Right(ParseArgs(start, end, commission, tickers, omitKeyword))
  }

  /** 
    * For now we are just using the same strange arguments as the original python code.
    * I should be able to make this better in the future.
    * 
    * @param pa
    * @return
    */
  private def gain(pa: ParseArgs): Unit = {
  
    val config = Config.load()
    val ss: List[Stock] = Stock.load(config)
    val stocks: Map[Ticker,Stock] = ss.map{ s => s.ticker -> s }.toMap
    val quotes: Map[Ticker, Quote] = Quote.load(config)
    val cash: List[CashAccount] = CashAccount.load(config)

    // verify that every parsearg ticker is a valid ticker
    pa.tickers.foreach{ t => 
      if (!stocks.isDefinedAt(t)) {
        println(s"specified ticker does not have a log file: $t")
        sys.exit(1)
      }
    }

    val srs: List[ReportGain.StockReport] = ReportGain.create(pa, ss, quotes, Date.today)

    // print the fuckers
    // TODO: this could be moved to a render function that returns a string with no IO
    // this would make for some easy end to end testing

    if (srs.isEmpty) {
      println("No stocks found")
      sys.exit(0)
    }

    def percentString(d: Double): String = f"${d*100}%.1f%%"

    // line items
    val colWidth = "13"
    val itemFmt = "%"+colWidth+"s%"+colWidth+"s%"+colWidth+"s%"+colWidth+"s%"+colWidth+"s%"+colWidth+"s%"+colWidth+"s"
    srs.foreach{ sr =>
      println(sr.stock.ticker)
      println()
      sr.mss.foreach{ ms =>
        val s = ms.sell
        val m = s.shares.multiple
        println(s"${s.date} sell ${s.shares.toString(m)}@${s.price} ${s.gross} commission ${s.commission}")
        println(String.format(itemFmt, "purchase date", "", "share@price", "cost", "buy fee", "sell feel", "annual yield"))
        ms.mbs.foreach{ mb =>
          println(String.format(itemFmt, 
            mb.buy.date.toStringEnglishFixedWidth(), 
            if (mb.ltcg) "(ltcg)" else "", 
            s"${mb.sold.toString(m)}@${mb.price}", 
            mb.proportionalCost, 
            mb.proportionalBuyCommission, 
            mb.proportionalSellCommission, 
            percentString(mb.annualYield)))
        }
        // TODO: move this into matched buy so we can test it
        val totalCost: Double = ms.mbs.foldLeft(0.0){ (c, mb) => c + mb.proportionalCost.toDouble }
        val weightedPrice = Currency.fromDouble(totalCost / s.shares.atMult(m))
        println(String.format(itemFmt, 
          "", 
          "=", 
          s"${s.shares.toString(s.shares.multiple)}@${weightedPrice}", 
          ms.mbs.foldLeft(Currency.zero){_ + _.proportionalCost}, 
          ms.mbs.foldLeft(Currency.zero){_ + _.proportionalBuyCommission},
          ms.mbs.foldLeft(Currency.zero){_ + _.proportionalSellCommission},
          "???"))
        println(f"cap gain ${ms.capitalGain}")
        println()
      }
    }

    // add cash accounts as pseudo StockReports
    val srs2 = if (pa.isCurrentValueMode && srs.length>1) {
      srs ++ cash.map{ c => {
        val s = Stock(Ticker("$"+c.accountName), None, None, Set(), List(), List(), null, null)
        ReportGain.StockReport(s, List(), c.balance, Currency.zero, 1.0)
      }}
    } else srs

    // summary
    println("Summary")
    val itemFmt2 = "%10s%18s%7s%15s%15s"
    println(String.format(itemFmt2, "ticker", "value", "%", "cap gains", "ltcg"))
    val totalNet = srs2.foldLeft(Currency.zero){ (acc, sr) => acc + sr.net }
    val totalCapGains = srs2.foldLeft(Currency.zero){ (acc, sr) => acc + sr.capGains }
    srs2.foreach{ sr =>
      val percentageValue = sr.net.toDouble / totalNet.toDouble
      println(String.format(itemFmt2, sr.stock.ticker, sr.net, percentString(percentageValue), sr.capGains, percentString(sr.ltcgPercentage)))
    }
    println("="*50)
    println(String.format(itemFmt2,"", totalNet, "100%", totalCapGains, ""))
    println()
  }

  override def command(args: IndexedSeq[String]): Option[String] = {
    parse(args) match {
      case Right(parseArgs) => {
        gain(parseArgs)
        None
      }
      case Left(error) => Option(error)
    }
  }
}
package org.kleemann.stocknotes.current

import org.kleemann.stocknotes.{Currency, Date, Fraction, Ticker}
import org.kleemann.stocknotes.stock.{CashAccount, Shares, Stock}
import org.kleemann.stocknotes.stock.{Trade, Buy, Sell, Split}


/**
    * The top level report for the Gain Report is a list of StockReport objects.
    *
    * @param stock The underlying stock data from the log directory.
    * @param mss The set of matched sells that have occurred. For current data, there is only one matched sell.
    * @param net The total value of all sales minus the sell commission
    * @param capGains The capital gains of this sale (gross-cost). This includes buy and sell commissions.
    * @param ltcgPercentage This is the percentage of shares sold that are ltcg.
    */
final case class StockReport(
  stock: Stock,
  mss: List[MatchedSell],
  net: Currency,
  capGains: Currency,
  ltcgPercentage: Double)

object StockReport {

  /**
    * Creates a report by selling all currently owned stocks.
    *
    * @param onlyShow if defined then only show this one ticker, if specified, ticker must appear in stocks
    * @param stocks the loaded stock logs
    * @param cash the loaded cash accounts
    * @param quotes the loaded quotes
    * @param commission the additional cost for each sell block
    * @param today normally set to Date.today, other values are for testing
    * @return a list of Company objects which, with its contained MatchedSells and MatchedBuys
    */
  def createCurrent(onlyShow: Option[Ticker], stocks: List[Stock], cash: List[CashAccount], quotes: Map[Ticker, Currency], commission: Currency, today: Date): List[StockReport] = {

    // if tickers is empty then replace it with the tickers of all companies
    val stocks2: List[Stock] = onlyShow match {
      case Some(t) => List(stocks.find{s => s.ticker == t}.get)
      case None => stocks.filter{ !_.trades.isEmpty } // don't really need the filter optimization
    }

    val srs: List[StockReport] = stocks2.flatMap{ stock => {
      val price =
        if (quotes contains stock.ticker) quotes.get(stock.ticker).get
        else                              Currency.zero
      parseCompanyCurrentValue(stock, price, commission, today)
    }}.sortBy{ _.net }

    if (onlyShow.isDefined) srs
    else
      // add cash accounts as pseudo StockReports
      // we want them at the end of the report
      srs ++ cash.map{ c => {
        val s = Stock(Ticker("$"+c.accountName), None, None, Set(), List(), List(), null, null)
        StockReport(s, List(), c.balance, Currency.zero, 1.0)
      }}
  }

  /**
    * Show the sold stocks in the year range with gains and losses.
    *
    * @param start the first year inclusive
    * @param end the last year inclusive
    * @param stocks the loaded stock logs
    * @return a list of Company objects which, with its contained MatchedSells and MatchedBuys
    */
  def createHistorical(start: Date, end: Date, stocks: List[Stock]): List[StockReport] =
    // ignore stocks that don't have at least one sell in the date range
    stocks.filter{ stock =>
      stock.trades.exists{
        case Sell(date, _, _, _) => start <= date && end >= date
        case _ => false
      }
    }.flatMap{ parseCompanyDateRange(_, start, end) }

  /**
    * A pure function that renders a list of stock reports to text.
    *
    * @param srs the list of stock reports
    * @return a multiline output string of the report, meant to be displayed in fixed width
    */
  def render(srs: List[StockReport]): String = {
    if (srs.isEmpty)
      "No stocks found\n"
    else {

      val sb = collection.mutable.StringBuilder()

      def percentString(d: Double): String = f"${d*100}%.1f%%"

      // line items
      val colWidth = "13"
      val itemFmt = "%"+colWidth+"s%"+colWidth+"s%"+colWidth+"s%"+colWidth+"s%"+colWidth+"s%"+colWidth+"s%"+colWidth+"s"
      srs.foreach{ sr =>
        // cash accounts don't have any sells
        if (!sr.mss.isEmpty) {
          sb ++= sr.stock.ticker.toString()
          sb ++= "\n"
        }
        sr.mss.foreach{ ms =>
          val s = ms.sell
          val m = s.shares.multiple
          sb ++= s"${s.date} sell ${s.shares.toString(m)}@${s.price} ${s.gross} commission ${s.commission}"
          sb ++= "\n"
          sb ++= String.format(itemFmt, "purchase date", "", "share@price", "cost", "buy fee", "sell feel", "annual yield")
          sb ++= "\n"
          ms.mbs.foreach{ mb =>
            sb ++= String.format(itemFmt, 
              mb.buy.date.toStringEnglishFixedWidth(), 
              if (mb.ltcg) "(ltcg)" else "", 
              s"${mb.sold.toString(m)}@${mb.price}", 
              mb.proportionalCost, 
              mb.proportionalBuyCommission, 
              mb.proportionalSellCommission, 
              percentString(mb.annualYield))
            sb ++= "\n"
          }
          // TODO: move this into matched buy so we can test it
          val totalCost: Double = ms.mbs.foldLeft(0.0){ (c, mb) => c + mb.proportionalCost.toDouble }
          val weightedPrice = Currency.fromDouble(totalCost / s.shares.atMult(m))
          sb ++= String.format(itemFmt, 
            "", 
            "=", 
            s"${s.shares.toString(s.shares.multiple)}@${weightedPrice}", 
            ms.mbs.foldLeft(Currency.zero){_ + _.proportionalCost}, 
            ms.mbs.foldLeft(Currency.zero){_ + _.proportionalBuyCommission},
            ms.mbs.foldLeft(Currency.zero){_ + _.proportionalSellCommission},
            "???")
          sb ++= "\n"
          sb ++= f"cap gain ${ms.capitalGain}"
          sb ++= "\n"
          sb ++= "\n"
        }
      }

      // summary
      sb ++= "Summary"
      sb ++= "\n"
      val itemFmt2 = "%10s%18s%7s%15s%15s"
      sb ++= String.format(itemFmt2, "ticker", "value", "%", "cap gains", "ltcg")
      sb ++= "\n"
      val totalNet = srs.foldLeft(Currency.zero){ (acc, sr) => acc + sr.net }
      val totalCapGains = srs.foldLeft(Currency.zero){ (acc, sr) => acc + sr.capGains }
      srs.foreach{ sr =>
        val percentageValue = sr.net.toDouble / totalNet.toDouble
        sb ++= String.format(itemFmt2, sr.stock.ticker, sr.net, percentString(percentageValue), sr.capGains, percentString(sr.ltcgPercentage))
        sb ++= "\n"
      }
      sb ++= ("="*50)
      sb ++= "\n"
      sb ++= String.format(itemFmt2,"", totalNet, "100%", totalCapGains, "")
      sb ++= "\n"
      sb ++= "\n"

      sb.result()
    }
  }

  private[current] def parseCompanyCurrentValue(stock: Stock, price: Currency, commission: Currency, today: Date): Option[StockReport] = {
    // we only want trades that are less than today; this is really just a problem for testing
    val trades = stock.trades.filter{ (t: Trade) => t.getDate() <= today }

    // We need to find all currently owned shares of the company.
    // Walk through each trade, accumulating the share count and multiple.
    // We only keep the share count.
    val (_, currentOwnedShares: Shares) =
      trades.foldLeft( (Fraction.one, Shares.zero) ) { case ((accMultiple, accShares), t) => t match {
        case Buy(_, shares, _, _)  => (accMultiple, accShares.add(shares, accMultiple))
        case Sell(_, shares, _, _) => (accMultiple, accShares.sub(shares, accMultiple))
        case Split(_, multiple)    => {
          val newMultiple = accMultiple * multiple
          // shares need to be adjusted since the final, pretend sell has a price at the new multiple
          val sharesAtNewMultiple = accShares.add(Shares.zero, newMultiple)
          (newMultiple, sharesAtNewMultiple)
        }
    }}

    if (currentOwnedShares == Shares.zero) None // nothing to sell so no report
    else {
      // This pretends we sell all of our remaining shares so the report
      // can show us what the current value of the shares are.
      val pretendSell = Sell(today, currentOwnedShares, price, commission)
      val pretendStock = stock.copy(trades = trades :+ pretendSell)
      parseCompanyDateRange(pretendStock, today, today)
    }
  }

  /**
    * This is the beginning of the common code of both the current report and the historical report.
    * Iterate through sells and match them with buys.
    *
    * @param stock the stock to analyze
    * @param start the earliest sell
    * @param end the latest sell
    * @return
    */
  private[current] def parseCompanyDateRange(stock: Stock, start: Date, end: Date): Option[StockReport] = {

    // When a buy is encountered we add it to brss which is our accumulating list of BuyReadyToSells.
    // When a sell is encountered, the accumulated buy list is passed to the sell parser which may consume some of the buys
    // and a matched sell is returned that we accumulate.
    // We can ignore splits since the share values in the buys and sells already have multiples associated with them.
    val (_, mssReversed: List[MatchedSell]) = 
      stock.trades.foldLeft( (Vector[BuyReadyToSell](), List[MatchedSell]() ) ){ case ((brss, mss), trade) =>
        trade match {
          case buy: Buy  => (brss :+ BuyReadyToSell(buy), mss)
          case sell: Sell => {
            // even if the sell isn't in our date range, we have to process it so it will consume earlier buys
            val (ms: MatchedSell, unconsumed: Vector[BuyReadyToSell]) = MatchedSell.sell2MatchedSell(sell, brss)
            if (sell.date>=start && sell.date<=end) (unconsumed, ms :: mss)
            else                                    (unconsumed,       mss)
          }
          case _: Split => (brss, mss)
        }
      }
    val mss = mssReversed.reverse

    // if there are no matched sells for the company, then there's nothing to report
    if (mss.isEmpty) None
    else {

      // for net sum, only count sell commission, not buy commission or buys cost
      // this is the cash you get when you sell
      val net: Currency = mss.foldLeft(Currency.zero){ (acc, ms) => acc + ms.sell.gross - ms.sell.commission }

      // capital gains is what you actually earned
      // gross of each sell - sell commission - cost - buy commisions proportional to sold shares
      val capGains: Currency = mss.foldLeft(Currency.zero){ (acc: Currency, ms: MatchedSell) =>
        acc + ms.sell.gross - ms.sell.commission - ms.mbs.foldLeft(Currency.zero) { (acc2, mb) =>
          acc2 + mb.proportionalCost + mb.proportionalBuyCommission
        }
      }

      // percentage of shares that are ltcg vs short
      val m = Fraction.one // all calculations need to be at the same multiple but it doesn't matter which one
      val ltcgShares: Double = mss.foldLeft(0.0){ (acc, ms) =>
        acc + ms.mbs.foldLeft(0.0){ (acc, mb) => 
          acc + (if (mb.ltcg) mb.sold.atMult(m) else 0.0)
        } 
      }
      val totalShares: Double = mss.foldLeft(0.0){ (acc, ms) => acc + ms.sell.shares.atMult(m) }
      val ltcgPercentage = ltcgShares / totalShares

      Some(StockReport(stock, mss, net.truncate, capGains.truncate, ltcgPercentage))
    }
  }

}
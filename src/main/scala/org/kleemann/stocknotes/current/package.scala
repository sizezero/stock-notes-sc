package org.kleemann.stocknotes

import org.kleemann.stocknotes.stock.{CashAccount, Shares, Stock}
import org.kleemann.stocknotes.stock.{Trade, Buy, Sell, Split}

/**
  * 
  * These functions primarily match blocks of sells with buys.
  * In addition, some attributes are calculated such as commision, capital gains, and 
  * annual yield.
  *
  * StockReport represents the sale of a particular stock over a time period.
  * The StockReport wraps a stock object and adds more calculated attributes.
  * Similarly MatchedSell wraps Sell and MatchedBuy Wraps Buy.
  * A StockReport can have one or more MatchedSells.
  * A MatchedSell can have one more more MatchedBuys.
  *
  * ┌───────────────┐
  * │  StockReport  │   ┌────────┐
  * │  * $net       ┼───► Stock  │
  * │  * $cap gains │ 1 └────────┘
  * │  * %LTCG      │
  * └───────┬───────┘
  *         │ many
  * ┌───────▼───────┐
  * │  MatchedSell  │   ┌──────┐
  * │  * $net       ┼───► Sell │
  * │  * $cap gains │ 1 └──────┘
  * └───────┬───────┘
  *         │ many
  * ┌───────▼─────────────┐
  * │  MatchedBuy         │   ┌─────┐
  * │  * shares sold      ┼───► Buy │
  * │  * $price           │ 1 └─────┘
  * │  * $cost            │
  * │  * $buy commission  │
  * │  * $sell commission │
  * │  * is LTCG          │
  * │  * %annual yield    │
  * └─────────────────────┘
  */
package object current {

  /**
    * Creates a report by pretending to sell all currently owned stocks and reporting the cash value.
    *
    * @param onlyShow if defined then only show this one ticker, otherwise show all stocks that have unsold shares
    * @param stocks the loaded stock logs
    * @param cash the loaded cash accounts
    * @param quotes the loaded quotes
    * @param commission the additional cost for each sell block
    * @param today normally set to Date.today, other values are for testing
    * @return a list of StockReport objects that can be used for reporting
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
      StockReport.parseCompanyCurrentValue(stock, price, commission, today)
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
    * @param start the first date of possible sales inclusive
    * @param end the last date of possible sales inclusive
    * @param stocks the loaded stock logs
    * @return a list of StockReport objects that can be used for reporting
    */
  def createHistorical(start: Date, end: Date, stocks: List[Stock]): List[StockReport] =
    // ignore stocks that don't have at least one sell in the date range
    stocks.filter{ stock =>
      stock.trades.exists{
        case Sell(date, _, _, _) => start <= date && end >= date
        case _                   => false
      }
    }.flatMap{ StockReport.parseCompanyDateRange(_, start, end) }

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
          val totalCost: Double = ms.mbs.foldLeft(0.0){ _ + _.proportionalCost.toDouble }
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

}

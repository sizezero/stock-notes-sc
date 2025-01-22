package org.kleemann.stocknotes.current

import org.kleemann.stocknotes.{Currency, Date, Fraction, Quote, Ticker}
import org.kleemann.stocknotes.stock.{CashAccount, Shares, Stock}
import org.kleemann.stocknotes.stock.{Trade, Buy, Sell, Split}
import scala.annotation.tailrec

/**
  * This is where all the calculations are performed for the Gain report. All this code is pure functional.
  */
object Gain {
   /**
    * The top level report for the Gain Report is a list of StockReport objects.
    *
    * @param stock The underlying stock data from the log directory.
    * @param mss The set of matched sells that have occurred. For current data, there is only one matched sell.
    * @param net The total value of all sales minus the sell commission
    * @param capGains The capital gains of this sale (gross-cost). This includes buy and sell commissions.
    * @param ltcgPercentage This is the percentage of shares sold that are ltcg.
    */
  case class StockReport(
    stock: Stock,
    mss: List[MatchedSell],
    net: Currency,
    capGains: Currency,
    ltcgPercentage: Double)

  /**
    * This represents a sale of a block of stock.
    * It is matched up with perhaps multiple buy blocks of stock.
    * 
    * @param sell The Trade Sell that is associated with this sale. All the shares of this Sell trade are sold thus they are all matched with buys.
    * @param net gross - sell commission, the cash you get when you sell
    * @param capitalGain net - sell commissions - buy commissions proportional to the shares sold from the buy batch
    * @param mbs A list of Matched buys that add up to this sale from oldest to newest.
    */
  case class MatchedSell(
    sell: Sell,
    net: Currency,
    capitalGain: Currency,
    mbs: List[MatchedBuy])

  /**
    * This represents a block of bought shares that are now the result of a possibly larger sale.
    * Note: not all of the shares of the buy block may have been sold.
    *
    * @param buy the Buy Trade
    * @param sold the number of shares sold from this Buy Trade: sold <= buy.shares. This is in the Sell multiple.
    * @param price this is the price of the shares in the Sell multiple. This may have sub-penny precision.
    * @param proportionalCost the cost proportional to the sold shares. This may have sub-penny precision.
    * @param proportionalBuyCommission the buy commission proportional to the sold shares. This may have sub-penny precision.
    * @param proportionalSellCommission the sell commision proportional to the sold shares. This may have sub-penny precision.
    * @param ltcg true if the shares were purchased at least a year before the sale.
    * @param annualYield The annual yield from cost+proportionalBuyCommission to the gross-proportionalSellCommission
    */
  case class MatchedBuy(
    buy: Buy,
    sold: Shares,
    price: Currency,
    proportionalCost: Currency,
    proportionalBuyCommission: Currency,
    proportionalSellCommission: Currency,
    ltcg: Boolean,
    annualYield: Double)

  /**
    * Creates a report by selling all currently owned stocks.
    *
    * @param onlyShow if defined then only show this one ticker, if specified, ticker must appear in stocks
    * @param stocks the loaded stock logs
    * @param cash the loaded cash accounts
    * @param quotes the loaded quotes
    * @param commission the additional cost for each sell block
    * @param today used for testing
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
        if (quotes contains stock.ticker)
          quotes.get(stock.ticker).get
        else
          Currency.zero
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

  private[stocknotes] def parseCompanyCurrentValue(stock: Stock, price: Currency, commission: Currency, today: Date): Option[StockReport] = {
    // we only want trades that are less than today; this is really just a problem for testing
    var trades = stock.trades.filter{ (t: Trade) => t.getDate() <= today }

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
    * This is a buy that is ready to be matched with sells to take some of the shares.
    *
    * @param buy The buy associated with these shares to sell.
    * @param unsold unsold shares at the multiple Fraction.one
    */
  private[stocknotes] case class BuyReadyToSell(buy: Buy, unsold: Double)

  private[stocknotes] object BuyReadyToSell {
    // default has all shares unsold
    def apply(buy: Buy) = new BuyReadyToSell(buy, buy.shares.atMult(Fraction.one))
  }

  private[stocknotes] def parseCompanyDateRange(stock: Stock, start: Date, end: Date): Option[StockReport] = {

    // When a buy is encountered we add it to brss which is our accumulating list of BuyReadyToSells.
    // When a sell is encountered, the accumulated buy list is passed to the sell parser which may consume some of the buys
    // and a matched sell is returned that we accumulate.
    // We can ignore splits since the share values in the buys and sells already have multiples associated with them.
    val (_, mssReversed: List[MatchedSell]) = 
      stock.trades.foldLeft( (Vector[BuyReadyToSell](), List[MatchedSell]() ) ){ case ((brss, mss), trade) =>
        trade match {
          case buy: Buy  => (brss :+ BuyReadyToSell(buy), mss)
          case sell: Sell => {
            // even if we the sell isn't in our date range, we have to process it so it will consume earlier buys
            val (ms: MatchedSell, unconsumed: Vector[BuyReadyToSell]) = parseMatchedSell(sell, brss)
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

      // the sum of each Matched sell, only count sell commission, not buy commission or buys cost
      // this is the cash you get when you sell
      val net: Currency = mss.foldLeft(Currency.zero){ (acc, ms) => acc + ms.sell.gross - ms.sell.commission }

      // gross of each sell - sell commission - cost - buy commisions proportional to sold shares
      val capGains: Currency = mss.foldLeft(Currency.zero){ (acc: Currency, ms: MatchedSell) =>
        acc + ms.sell.gross - ms.sell.commission - ms.mbs.foldLeft(Currency.zero) { (acc2, mb) =>
          acc2 + mb.proportionalCost + mb.proportionalBuyCommission
        }
      }

      // percentage of shares that are ltcg vs short
      val m = Fraction.one // all calculations need to be at the same multiple but it doesn't matter which one
      val numerator: Double = mss.foldLeft(0.0){ (acc, ms) =>
        acc + ms.mbs.foldLeft(0.0){ (acc, mb) => 
          acc + (if (mb.ltcg) mb.sold.atMult(m) else 0.0)
        } 
      }
      val denominator: Double = mss.foldLeft(0.0){ (acc, ms) => acc + ms.sell.shares.atMult(m) }
      val ltcgPercentage = numerator / denominator
        
      Some(StockReport(stock, mss, net.truncate, capGains.truncate, ltcgPercentage))
    }
  }

  /**
    * Associates all the shares of the given sell with some of the given buys.
    * We consume from the head of the buys since those are the oldest.
    * 
    * @param buys
    * @return The MatchedSell and BuyReadyToSells that have yet to be matched with sells.
    */
  private[stocknotes] def parseMatchedSell(sell: Sell, buys: Vector[BuyReadyToSell]): (MatchedSell, Vector[BuyReadyToSell]) = {

    // Unfortunately it's not possible to do this calculation with integer share values plus multiples.
    // We need to use lossy Doubles. This is highlighted in the test case "parseCompanyDateRange fractional matching"
    // which caused the old code to blow up.

    // this is some tiny share value at multiple one that we are comfortable that share counts will never get below
    // we use this to see if share counts are equal or at zero since we can't use equality on a Double
    val quantum: Double = 0.0001

    // all double calculations and all conversion to Shares are done with the sell multiple
    val m = sell.shares.multiple

    // a tempory structure to build our eventual matched buys
    case class IncompleteMatchedBuy(buy: Buy, sold: Shares) // sold shares are in the Sell multiple

    // Recursively walk through the BuyReadyToSell list consuming buys, and accumulate our incomplete matched buy list
    // until we have satisfied all shares in the sell.
    // Note: both buy and sell shares are converted to Double at multiple Fraction.one.
    @tailrec
    def assignBuys(toBuys: Vector[BuyReadyToSell], toSell: Double, imbs: List[IncompleteMatchedBuy]): (Vector[BuyReadyToSell], List[IncompleteMatchedBuy]) = {
      if (toSell <= quantum) (toBuys, imbs) // shares to sell are zero, we are done
      else {
        val b: BuyReadyToSell = toBuys.head
        if (Math.abs(b.unsold - toSell) < quantum) { // b.unsold == toSell
          // the current buy perfectly completes the sell
          val s = Shares((toSell * m.toDouble).round.toInt, m)
          (toBuys.tail, IncompleteMatchedBuy(b.buy, s) :: imbs)
        } else if (b.unsold < toSell) {
          // we have used up the current buy, but the sell is not done
          val s = Shares((b.unsold * m.toDouble).round.toInt, m)
          assignBuys(toBuys.tail, toSell-b.unsold, IncompleteMatchedBuy(b.buy, s) :: imbs)
        } else { // b.unsold > toSell
          // only some of the buy shares are needed to satisfy the sell
          val s = Shares((toSell * m.toDouble).round.toInt, m)
          val reducedShares: Double = b.unsold - toSell
          // sell is completed and the next buy is only partially completed
          (toBuys.updated(0, BuyReadyToSell(b.buy, reducedShares)), IncompleteMatchedBuy(b.buy, s) :: imbs)
        }
      }
    }
    val (toBuys: Vector[BuyReadyToSell], imbsReversed: List[IncompleteMatchedBuy]) =
      assignBuys(buys, sell.shares.atMult(Fraction.one), Nil)

    val mbs = imbsReversed.map{ imb => completeMatchedBuy(sell, imb.buy, imb.sold) }.reverse
    val net = sell.gross - mbs.foldLeft(Currency.zero){ (acc, mb) => acc + mb.proportionalCost }
    val capitalGain = net - sell.commission - mbs.foldLeft(Currency.zero){ (acc, mb) => acc + mb.proportionalBuyCommission }
    (MatchedSell(sell, net.truncate, capitalGain.truncate, mbs), toBuys)
  }

  /**
    * Now that buys are matched with sells, calculate some values needed by the report.
    *
    * @param sell the Sell Trade
    * @param buy one of the (possibly many) Buy Trades matched with the sell
    * @param sold the number shares from the buy used to satisfy the sell. A single buy may be satisfied with multiple sells.
    * @return
    */
  private[stocknotes] def completeMatchedBuy(sell: Sell, buy: Buy, sold: Shares): MatchedBuy = {

    // we need to convert the buy price to the sell multiple
    val price = buy.price.priceMultipleAdjust(buy.shares.multiple, sell.shares.multiple)

    // the proportion of sold shares in this buy batch vs total shares in this buy batch
    // If all shares in this batch are sold then this is 1.0
    val proportionBuy: Double = sold.atMult(sell.shares.multiple) / buy.shares.atMult(sell.shares.multiple)

    // the proportion of sold shares in this buy batch vs the total shares in the sell batch
    val proportionSell: Double = sold.atMult(sell.shares.multiple) / sell.shares.atMult(sell.shares.multiple)

    val proportionalSellCommission: Currency = Currency.fromDouble(sell.commission.toDouble * proportionSell)
    val proportionalBuyCommission:  Currency = Currency.fromDouble(buy .commission.toDouble * proportionBuy)

    // we can use non-multiple share count since this is the shares at the time of the buy
    val totalBuyCost = buy.shares.shares * buy.price.toDouble
    val proportionalBuyCost: Currency = Currency.fromDouble(totalBuyCost * proportionBuy)

    val diff: Double = sell.date.decimalYear - buy.date.decimalYear
    val ltcg: Boolean = diff >= 1.0

    // include commissions in annual yield calc
    val proportionalSellGross: Currency = Currency.fromDouble(sell.gross.toDouble * proportionSell)
    val ay: Double = annualYield(
        proportionalBuyCost   + proportionalBuyCommission, 
        proportionalSellGross - proportionalSellCommission, 
        diff)

    MatchedBuy(buy, sold, price, proportionalBuyCost, proportionalBuyCommission, proportionalSellCommission, ltcg, ay)
  }

  private[stocknotes] def annualYield(start: Currency, end: Currency, decimalYearsDifference: Double): Double =
    // don't explode if a purchase was made today
    if (end==start || decimalYearsDifference<0.001 || start==Currency.zero) return 0.0
    else Math.pow(end.toDouble / start.toDouble, 1.0 / decimalYearsDifference) - 1.0
}

package org.kleemann.stocknotes

/**
  * This is where all the calculations are performed for the Gain report. All this code is pure functional.
  */
object GainCalc {
   /**
    * I guess we're calling a Company the data needed to display a Company/Stock info on the gain report.
    * This is just the relevant data from the date range or just the current data.
    *
    * @param stock The underlying stock date from the log directory.
    * @param mss The set of matched sells that have occurred. For current data, there is only one matched sell.
    * @param gross The total value of all sales, sale price times shares without commission
    * @param capGains The capital gains of this sale (gross-cost). This includes commissions.
    * @param ltcgPercentage This is the percentage of shares that are ltcg.
    */
  case class Company(stock: Stock, mss: List[MatchedSell], gross: Currency, capGains: Currency, ltcgPercentage: Double)

  /**
    * This represents a sale of a block of stock.
    * It is matched up with perhaps multiple buy blocks of stock.
    *
    * @param sell The Trade Sell that is associated with this sale. All shares of this sell are sold.
    * @param net gross - cost
    * @param capitalGain net - all commissions
    * @param mbs A list of Matched buys that add up to this sale from oldest to newest.
    */
  case class MatchedSell(sell: Sell, net: Currency, capitalGain: Currency, mbs: List[MatchedBuy])

  /**
    * This represents a block of bought shares that are now the result of a possibly larger sale.
    * Note: not all of the shares of the buy block may have been sold.
    *
    * @param buy The buy trade. Not all of the shares may be sold from this trade.
    * @param sold The number of shares sold from this buy trade. Is <= buy.shares
    * @param proportionalCost The cost of the sold shares.
    * @param proportionalBuyCommission The buy commission proportional to the sold shares.
    * @param proportionalSellCommission The sell commision proportional to the sold shares.
    * @param ltcg True if the shares were purchased at least a year before the sale.
    * @param annualYield The annual yield from cost+proportionalBuyCommission to the gross-proportionalSellCommission
    */
  case class MatchedBuy(buy: Buy, sold: Shares, proportionalCost: Currency, proportionalBuyCommission: Currency, proportionalSellCommission: Currency, ltcg: Boolean, annualYield: Double)

  /**
    * This is the main entry point for this object. calc() returns a list of Company objects
    * which is the result of the Gain report.
    *
    * @param pa user specified command line inpute
    * @param stocks the parsed stock files from the log/ directory
    * @param quotes the parsed quotes from the downloaded quote file
    * @param today today's date passed in to make this pure functional and testable
    * @return a list of Company objects for each company that had sales in the indicated period
    */
  def calc(pa: Gain.ParseArgs, stocks: List[Stock], quotes: Map[Ticker, Quote], today: Date): List[Company] = {

    val sm: Map[Ticker, Stock] = stocks.map{ s => s.ticker -> s }.toMap

    // if tickers is empty then replace it with the tickers of all companies
    val tickers: List[Ticker] =
      if (pa.tickers.isEmpty) sm.keys.toList.sorted
      else pa.tickers

    val stocks2: List[Stock] = tickers.map{ sm(_) }.filter{ s =>
      // ignore stocks that have no trades
      // TODO: this may be an efficiency but it is not really necessary
      !s.trades.isEmpty &&
      // ignore the stock if it contains the specified keyword
      (if (pa.omitKeyword.isDefined) !(s.keywords contains pa.omitKeyword.get) else true)
    }
        
    // there are two versions of this, one where we get a year range and one where we get a commision and fake a sale
    if (pa.isCurrentValueMode) {

      stocks2.flatMap{ s => parseCompanyCurrentValue(
        s, 
        if (quotes contains s.ticker) quotes.get(s.ticker).get.price else Currency.zero,
        pa.commission,
        today)}

    } else {

      // ignore stocks that don't have at least one sell (in the date range)
      val stocks3 = stocks2.filter{ s =>
        s.trades.exists{ t => t match {
          case Sell(date, _, _, _) => pa.start <= date && pa.end >= date
          case _ => false
        }}
      }
      stocks3.flatMap{ s => parseCompanyDateRange(s, pa.start, pa.end)}

    }
  }

  private[stocknotes] def parseCompanyCurrentValue(stock: Stock, price: Currency, commission: Currency, today: Date): Option[Company] = {
    // this is really just a problem for testing, we only want trades that are less than today
    var ts = stock.trades.filter{ (t: Trade) => t.getDate() <= today }
    // need to find all outstanding shares of the company
    var currentMultiple = Fraction.one
    var acc = Shares.zero
    ts.foreach{ t => {
      t match {
        case Buy(date, shares, price, commission) => acc = acc.add(shares, currentMultiple)
        case Sell(date, shares, price, commission) => acc = acc.sub(shares, currentMultiple)
        case Split(date, multiple) => currentMultiple = currentMultiple*multiple
      }
    }}
    if (acc == Shares.zero) None
    else {
      // final case class Sell(date: Date, shares: Shares, price: Currency, commission: Currency) extends Trade(date) {
      val sell = Sell(today, acc, price, commission)
      val ts2 = ts :+ sell // inefficient, but what else do we do?
      val s = Stock(
        stock.ticker,
        stock.name,
        stock.cid,
        stock.keywords,
        stock.entries,
        ts2,
        stock.buyWatch,
        stock.sellWatch)
      parseCompanyDateRange(s, Date.earliest, today)
    }
  }

  private[stocknotes] def parseCompanyDateRange(stock: Stock, start: Date, end: Date): Option[Company] = {
    // I'm breaking my head trying to get this to work with foldLeft or other method. Going back to iterative.
    var brss = Vector[BuyReadyToSell]()
    var mss = List[MatchedSell]() // we will be accumulating these in reverse order
    stock.trades.foreach{ t =>
      t match {
        case b: Buy  => brss = brss :+ BuyReadyToSell(b)
        case s: Sell => {
            if (s.date>=start && s.date<=end) {
            val (ms: MatchedSell, brss2: Vector[BuyReadyToSell]) = parseMatchedSell(s, brss)
            brss = brss2
            mss = ms :: mss
          }
        }
        case p: Split => Nil
      }
    }
    // if there are no matched sells for the company, then there's nothing to report
    if (mss.isEmpty) None
    else {
      // the sum of each Matched sell, I think we leave the commissions out of it
      val gross: Currency = mss.foldLeft(Currency.zero){ (acc, ms) => acc + ms.sell.gross }
      // gross of each sell - sell
      val capGains: Currency = mss.foldLeft(Currency.zero){ (acc: Currency, ms: MatchedSell) =>
        acc + ms.sell.gross - ms.sell.commission - ms.mbs.foldLeft(Currency.zero) { (acc2, mb) =>
          acc2 + mb.proportionalCost + mb.proportionalBuyCommission
        }
      }
      // percentage of shares that are ltcg vs short
      // all share counts need to be converted to an arbitrary multiple
      // since they are all at the same multiple, we can just divde their int values
      val m = Fraction.one
      val numerator = mss.foldLeft(0.0){ (acc, ms) =>
        acc + ms.mbs.foldLeft(0.0){ (acc, mb) => 
          acc + (if (mb.ltcg) mb.sold.atMult(m) else 0.0)
        } 
      }
      val denominator = mss.foldLeft(0.0){ (acc, ms) => acc + ms.sell.shares.atMult(m)}
      val ltcgPercentage: Double = numerator / denominator
        
      Some(Company(stock, mss.reverse, gross, capGains, ltcgPercentage))
    }
  }

  /**
    * This is a buy that is ready to be matched with sells to take some of the shares.
    *
    * @param buy The buy associated with these shares to sell.
    * @param unsold unsold shares
    */
  private[stocknotes] case class BuyReadyToSell(buy: Buy, unsold: Shares)

  object BuyReadyToSell {
    // default has all shares unsold
    def apply(buy: Buy) = new BuyReadyToSell(buy, buy.shares)
  }

  /**
    * Associates the given sell with some of the given BuyReadyToSells.
    * 
    * TODO: I think we can refactor this to not require IncompleteMatchedBuy
    * Do this after we have a working version with tests.
    *
    * @param buys
    * @return The MatchedSell and BuyReadyToSells that still have shares.
    */
  private[stocknotes] def parseMatchedSell(sell: Sell, buys: Vector[BuyReadyToSell]): (MatchedSell, Vector[BuyReadyToSell]) = {
    // a tempory structure to build our eventual matched buys
    case class IncompleteMatchedBuy(buy: Buy, sold: Shares)
    // loop through the BuyReadyToSell list until we have satisfied all shares in the sell
    var ready = buys
    var toSell: Shares = sell.shares
    var imbs: List[IncompleteMatchedBuy] = Nil // new ones placed on front
    while (toSell != Shares.zero) {
      val b: BuyReadyToSell = ready.head
      // can't seem to get around losing precision when converting buy shares to the current mult
      // we try to minimize it by doing nothing if the multiples match and propagating the converted values forward
      val availableBuyShares: Shares = if (b.unsold.multiple == toSell.multiple) b.unsold
        else Shares(b.unsold.atMult(toSell.multiple).floor.toInt, toSell.multiple)
      // comparisons from now on will be at the sell multiple
      if (availableBuyShares.shares == toSell.shares) {
        // the current buy perfectly completes the sell
        imbs = IncompleteMatchedBuy(b.buy, toSell) :: imbs
        toSell = Shares.zero
        ready = ready.tail
      } else if (availableBuyShares.shares < toSell.shares) {
        // we have used up the current buy, but the sell is not done
        imbs = IncompleteMatchedBuy(b.buy, availableBuyShares) :: imbs
        toSell = toSell.sub(availableBuyShares, toSell.multiple)
        ready = ready.tail
      } else { // availableBuyShares.shares > toSell.shares
        // only some of the buy shares are needed to satisfy the sell
        imbs = IncompleteMatchedBuy(b.buy, toSell) :: imbs
        // sell is completed and the next buy is only partially completed
        val reducedShares = availableBuyShares.sub(toSell, toSell.multiple)
        ready = ready.updated(0, BuyReadyToSell(b.buy, reducedShares))
        toSell = Shares.zero
      }
    }
    val mbs = imbs.map{ imb => completeMatchedBuy(sell, imb.buy, imb.sold) }.reverse
    val net = sell.gross - mbs.foldLeft(Currency.zero){ (acc, mb) => acc + mb.proportionalCost }
    val capitalGain = net - sell.commission - mbs.foldLeft(Currency.zero){ (acc, mb) => acc + mb.proportionalBuyCommission }
    (MatchedSell(sell, net, capitalGain, mbs), ready)
  }

  private[stocknotes] def completeMatchedBuy(sell: Sell, buy: Buy, sold: Shares): MatchedBuy = {
    // the proportion of sold shares in this buy batch vs total shares in this buy batch
    // If all shares in this batch are sold then this is 1.0
    val proportionBuy: Double = sold.atMult(sell.shares.multiple) / buy.shares.atMult(sell.shares.multiple)
    // the proportion of sold shares in this buy batch vs the total shares in the sell batch
    val proportionSell: Double = sold.atMult(sell.shares.multiple) / sell.shares.atMult(sell.shares.multiple)

    val proportionalSellCommission: Currency = Currency.fromDouble(sell.commission.toDouble * proportionSell)
    val proportionalBuyCommission: Currency = Currency.fromDouble(buy.commission.toDouble * proportionBuy)
    // we can use non-multipe share count since this is the shares at the time of the buy
    val totalBuyCost = buy.shares.shares * buy.price.toDouble
    val proportionalBuyCost: Currency = Currency.fromDouble(totalBuyCost * proportionBuy)
    val diff: Double = sell.date.decimalYear - buy.date.decimalYear
    val ltcg: Boolean = diff >= 1.0
    // include commissions in annual yield calc
    val proportionalSellGross: Currency = Currency.fromDouble(sell.gross.toDouble * proportionSell)
    val ay: Double = annualYield(proportionalBuyCost + proportionalBuyCommission, proportionalSellGross - proportionalSellCommission, diff)
    MatchedBuy(buy, sold, proportionalBuyCost, proportionalBuyCommission, proportionalSellCommission, ltcg, ay)
  }

  private[stocknotes] def annualYield(start: Currency, end: Currency, decimalYears: Double): Double =
    // don't explode if a purchase was made today
    if (end==start || decimalYears<0.001 || start==Currency.zero) return 0.0
    else return Math.pow(end.toDouble/start.toDouble, 1.0/decimalYears) - 1.0
}

// here is the original analysis of the python code

      // let's start by reverse engineering what the old python code does

      // old getArgs
        // if there were no arguments then there was a default commision of 30 (implemented here)
        // if tickers are not specified then get the tickers for all the companies (there must be some filtering later)  (need to implement this)
        // if a list of tickers is specifid then we need to verify that the companies exist in our db (need to implement this)

      // accumulate a list of companies that have our desired trades
      // loop through all specified stocks (companies) {
        // ignore stocks that have no trades
        // ignore the stock if it contains the specified keyword
        // ignore stocks that don't have at least one sell (in the date range)
        // a temporary object for the company is made:
          // headerPrinted
          // value
          // capitalGains
          // gross
          // ltcg
        // push Fraction.one through all splits to get the current multiple
        // accumulate share balance, hasSellLineItem, 
        // trades are looped {
          // buys are given
            // set the unsold sharecount which is just a copy of the trades share count
          // sells are given
            // buy/sell matched in a function
            // if commision < 0 (???) and trade date is within the date range, The sell is printed
            // company is given some other attributes: value+=sell value, capital gains+=sell cap gain, gross+=sell gross, ltcg+=sell ltcg
            // since there is a sell, hasSellLine item = true
        // } end trade
        // after looping through all trades, if commision > 0 and balance is positive
          // a mock trade sell is made to sell the remaining shares.
          // I guess the positive commision tells us that we are looking at the "current time" and pretending we're selling everything
          // guard against no quote for the ticker
          // quote is needed to create a sell trade
          // matchBuysToSells(sell, trades, mult) is called
            // attributes added to sell
              // s.gross: shares sold at current multiple * price
              // s.cost: TODO
                // Not sure why we need multiples of either of these. If we had a multiple for the share count at the time of purchase, price would be enough.
              // s.totalCommissions: TODO
              // s.ltcg: TODO
            // remaining is set to shares sold
            // loop through all buy trades {
              // break if remaining is zero
              // skip buys that have no unsold shares
              // make a new transaction t
              // t.buy = buy
              // subtract the buys shares from the sells shares
                // take into account that either buy or sell may have more shares than the other
                // this should take into account some multiple conversion but doesn't since sell and buy could be at different mults
              // t gets a bunch of attributes: buyCommission, sellCommission, commission (total), cost, net, annualYield, ltcg
                // all of the shares*price calcs are using the same mult which is probably wrong
                // annualyield and ltcg is calculated with decimal years
            // } end trade loop
            // sell.cost is the sum of the buy costs
            // sell.totalCommission is the sum of the buy commissions
              // this is really total buy commission
            // sell.value = sell.gross - sell.commission
            // sell.capitalGain = sell.gross-sell.cost =sell.totalCommission
              // this may miss the sell commission
          // sell is printed
            // prints two row header:
              // e.g.
              // January 18, 2024 sell 2095@30.47 $63834.65 commission $30.00
              // purchase date               share@price         cost      buy fee     sell fee annual yield
            // loops through buys associated with sell
              // prints date, optional "(ltcg)", shares, cost, buycom, sellcom, annual yield
            // prints sell summary
              // special case for tiny sell cost
              // average purchase price, sell cost, buysCommission, sellCommission
                // I think total commission mistakenly removes sell coast
              // sell.capitalGain on next line

        // if at this point, we have no sell line items, we skip to the next company
        // otherwise we add to the list of companies that have desired trades
      // } end companies loop
      // add fake companies for all cash accounts
        // ticker is the file basename with a dollar prefix
        // capitalGains=0, value=gross=file balance, ltcg=1.0 ?
      // print summary
        // print("Summary")
        // fmt="%10s%15s%5s%15s%15s"
        // print(fmt % ("ticker","value","%","cap gains","ltcg"))
        // loop through companies
          // ltcg is c.ltcp/c.gross; gross can't be close to zero
          // print(fmt % (c.ticker, dollar2str(c.value), pcnt2str(c.value/cumValue), dollar2str(c.capitalGains),pcn
        // cumValue and cumCapitalGains is just the sum of the values for all companies
        // print(fmt % ("",dollar2str(cumValue),"100%",dollar2str(cumCapitalGains),""))

      //analysis of the above

      // inputs: tickers (default all), sell commission, start, end, keywords
      // generate companies list

      // only care about companies that:
        // have a sell within the range (companies that have no trades are obviously rejected)
        // don't have one of the given keywords
  
      // I think a good starting point would be to separate the calculation from the display

      // there is a scope of looking at a company, creating tempory objects for that company and returning some permanent state

      // within the company calculation
        // there will be a selection of which sells matter, then an iteration of those sells, to find the matching buys
        // we will need new objects to hold shares counts for these
        // I think it would help to have the multiple at the time of each buy and sell
          // this may be easier to make than having a dateToMultiple function with a data structure backing it
        
      // A function that creates the sell object
      // given a list of buys with multiple and unsold shares
      // returns sell with extra attributes, also returns object with remaining buys with positive values

      // we need a functional gain object that returns these structures
      // a non functional gain object calls the function gain and pretty prints

      // I'm torn with how much to cache for these object. I think I'm going to cache everything that needs to be displayed
      // this will help with testing.

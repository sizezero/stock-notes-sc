package org.kleemann.stocknotes

object Gain extends Command {

  val help = s"""gain [-omit <ticker> ] (<year>[:<year>] | <commission>) [ <ticker> <ticker> ... ]
  |""".stripMargin

  def command(args: IndexedSeq[String]): Option[String] = {
    parse(args) match {
      case Right(parseArgs) => gain(parseArgs)
      case Left(error)      => Option(error)
    }
  }
  
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
  private[stocknotes] case class ParseArgs(start: Date, end: Date, commission: Currency, tickers: List[Ticker], omitKeyword: Option[String]) {

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
    def isCurrentValueMode: Boolean = commission != Currency.dollarsCents(-100, 0)
  }

  /** 
    * 
    *
    * @param args
    * @return
    */
  private[stocknotes] def parse(args: IndexedSeq[String]): Either[String, ParseArgs] = {

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
      if (args3(0)==":") (Date(1900,1,1).get, Date(2100,1,1).get, Currency.dollarsCents(-100, 0))
      else args3(0).match {
        case yearPattern(y)        => (Date(y.toInt,1,1).get, Date(y.toInt,1,1).get, Currency.dollarsCents(-100, 0))
        case twoYearPattern(y1,y2) => (Date(y1.toInt,1,1).get, Date(y2.toInt,1,1).get, Currency.dollarsCents(-100, 0))
        case commissionPattern(d)  => (Date(1900,1,1).get, Date(1900,1,1).get, Currency.parse(args3(0)).get)
        case _                     => return Left(help) //this looks non-functional
      }

    val args4 = args3.drop(1)

    val tickers: List[Ticker] = args4.map{ Ticker(_) }.sorted.toList

    Right(ParseArgs(start, end, commission, tickers, omitKeyword))
  }

  /** For now we are just using the same strange arguments as the original python code.
    * I should be able to make this better in the future.
    * 
    * Does this command have the side effect of printing? Right now our return value consists of only output text in the error case.
    * Maybe we should use Either to enable output in both cases.
    *
    * @param pa
    * @return
    */
  private def gain(pa: ParseArgs): Option[String] = {
    // both config and stock loading blow us out with a sys.exit(1) not sure if that's what I want
    // we're not really returning anything at this point, may as well be Unit
    val config = Config.load()
    val ss: List[Stock] = Stock.load(config)
    val stocks: Map[Ticker,Stock] = ss.map{ s => s.ticker -> s }.toMap
    val quotes: Map[Ticker, Quote] = Quote.load(config)

    // verify that every parsearg ticker is a valid ticker
    pa.tickers.foreach{ t => 
      if (!stocks.isDefinedAt(t)) {
        println(s"specified ticker does not have a log file: $t")
        sys.exit(1)
      }
    }

    val companies: List[Company] = functionalGain(pa, ss, quotes, Date.today)

    // print the fuckers

    if (companies.isEmpty) {
      println("No stocks found")
    }
    companies.foreach{ c =>
      println(c.stock.ticker)
      //println()
    }

    None

  }

  private[stocknotes] def functionalGain(pa: ParseArgs, stocks: List[Stock], quotes: Map[Ticker, Quote], today: Date): List[Company] = {

    val sm: Map[Ticker, Stock] = stocks.map{ s => s.ticker -> s }.toMap

    // if tickers is empty then replace it with the tickers of all companies
    val tickers: List[Ticker] = if (pa.tickers.isEmpty) sm.keys.toList.sorted
    else pa.tickers

    val stocks2: List[Stock] = tickers.map{ sm(_) }.filter{ s =>
      // ignore stocks that have no trades
      !s.trades.isEmpty &&
      // ignore the stock if it contains the specified keyword
      (if (pa.omitKeyword.isDefined) !(s.keywords contains pa.omitKeyword.get) else true)
    }
        
    // there are two versions of this, one where we get a year range and one where we get a commision and fake a sale
    if (pa.isCurrentValueMode) {

      // ignore stocks that don't have at least one sell (in the date range)
      val stocks3 = stocks2.filter{ s =>
        s.trades.exists{ t => t match {
          case _: Sell => true
          case _ => false
        }}
      }
      stocks3.flatMap{ s => parseCompanyCurrentValue(
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

  /**
    * I guess we're calling a Company the data needed to display a Company/Stock info on the gain report.
    *
    * @param stock
    * @param ms
    * @param value
    * @param capGains
    * @param ltcgPercentage
    */
  case class Company(stock: Stock, ms: List[MatchedSell], value: Currency, capGains: Currency, ltcgPercentage: Double)

  def parseCompanyCurrentValue(stock: Stock, price: Currency, commission: Currency, today: Date): Option[Company] = {
    // need to find all outstanding shares of the company
    var currentMultiple = Fraction.one
    var acc = Shares.zero
    stock.trades.foreach{ t => 
      t match {
        case Buy(date, shares, price, commission) => acc = acc.add(shares, currentMultiple)
        case Sell(date, shares, price, commission) => acc = acc.sub(shares, currentMultiple)
        case Split(date, multiple) => currentMultiple = currentMultiple*multiple
      }
    }
    if (acc == Shares.zero) None
    else {
      // final case class Sell(date: Date, shares: Shares, price: Currency, commission: Currency) extends Trade(date) {
      val sell = Sell(today, acc, price, commission)
      val t2 = stock.trades :+ sell // inefficient, but what else do we do?
      val s = Stock(
        stock.ticker,
        stock.name,
        stock.cid,
        stock.keywords,
        stock.entries,
        t2,
        stock.buyWatch,
        stock.sellWatch)
      parseCompanyDateRange(s, Date(1900, 1, 1).get, today)
    }
  }

  def parseCompanyDateRange(stock: Stock, start: Date, end: Date): Option[Company] = {
    // I'm breaking my head trying to get this to work with foldLeft or other method. Going back to iterative.
    var brss = Vector[BuyReadyToSell]()
    var ms = List[MatchedSell]()
    stock.trades.foreach{ t =>
      t match {
        case b: Buy  => brss = brss :+ BuyReadyToSell(b)
        case s: Sell => {
            if (s.date>start && s.date<end) {
            val (m: MatchedSell, brss2: Vector[BuyReadyToSell]) = parseMatchedSell(s, brss)
            brss = brss2
            ms = m :: ms
          }
        }
        case p: Split => Nil
      }
    }
    // if there are no matched sells for the company, then there's nothing to report
    if (ms.isEmpty) None
    else {
      // the sum of each Matched sell, I think we leave the commissions out of it
      val value: Currency = ms.foldLeft(Currency.zero){ (acc, m) => acc + m.sell.gross }
      // gross of each sell - sell
      val capGains: Currency = ms.foldLeft(Currency.zero){ (acc: Currency, m: MatchedSell) =>
        acc + m.sell.gross - m.sell.commission - m.mbs.foldLeft(Currency.zero) { (acc2, mb) =>
          mb.buy.cost + mb.buy.commission
        }
      }
      // percentage of shares that are ltcg vs short
      // all share counts need to be converted to an arbitrary multiple
      // since they are all at the same multiple, we can just divde their int values
      val ltcgPercentage: Double = 
        ms.foldLeft(Shares.zero){ (acc, m) =>
          acc.add(
            m.mbs.foldLeft(acc){ (acc, mb) => 
              acc.add(if (mb.ltcg) mb.sold else Shares.zero, Fraction.one)
            }, 
            Fraction.one)
        }.shares.toDouble
        /
        ms.foldLeft(Shares.zero){ (acc, m) => acc.add(m.sell.shares, Fraction.one )}.shares.toDouble
        
      Some(Company(stock, ms, value, capGains, ltcgPercentage))
    }
  }

  private[stocknotes] case class MatchedSell(sell: Sell, mbs: List[MatchedBuy])

  /**
    * First pass results in this
    *
    * @param buy
    * @param sold
    */
  private[stocknotes] case class IncompleteMatchedBuy(buy: Buy, sold: Shares)

  /**
    * Once we have all the buys and the sell we can generate the complet MatchedBuy
    *
    * @param buy
    * @param sold
    * @param fractionalSellCommission
    * @param ltcg
    * @param annualYield
    */
  private[stocknotes] case class MatchedBuy(buy: Buy, sold: Shares, fractionalSellCommission: Currency, ltcg: Boolean, annualYield: Double)

  /**
    * This is a buy that is ready to be matched with sells to take some of the shares
    *
    * @param buy
    * @param unsold
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
  def parseMatchedSell(sell: Sell, buys: Vector[BuyReadyToSell]): (MatchedSell, Vector[BuyReadyToSell]) = {
    // loop through the BuyReadyToSell list until we have satisfied all shares in the sell
    var ready = buys
    var toSell: Shares = sell.shares
    var is: List[IncompleteMatchedBuy] = Nil // new ones placed on front
    while (toSell != Shares.zero) {
      val b: BuyReadyToSell = ready.head
      val currentBuyShares: Int = b.buy.shares.atMult(sell.shares.multiple)
      // comparisons from now on will be at the sell multiple
      if (currentBuyShares == toSell.shares) {
        // the current buy perfectly completes the sell
        is = IncompleteMatchedBuy(b.buy, toSell) :: is
        toSell = Shares.zero
        ready = ready.tail
      } else if (currentBuyShares < toSell.shares) {
        // we have used up the current buy, but the sell is not done
        val s = Shares(currentBuyShares, toSell.multiple)
        is = IncompleteMatchedBuy(b.buy, s) :: is
        toSell = toSell.sub(s, toSell.multiple)
        ready = ready.tail
      } else { // currentBuyShares > toSell.shares
        // which multiple, the new or the old? In theory it doesn't matter.
        // In practice we may want it in the sell multiple.
        val s = b.unsold.sub(toSell, toSell.multiple)
        is = IncompleteMatchedBuy(b.buy, s) :: is
        // sell is completed and the next buy is only partially completed
        toSell = Shares.zero
        ready = ready.updated(0, BuyReadyToSell(b.buy, s))
      }
    }
    (MatchedSell(sell, completeMatchedBuys(sell, is).reverse), ready)
  }

  def completeMatchedBuys(sell: Sell, ims: List[IncompleteMatchedBuy]): List[MatchedBuy] =
    ims.map { im =>
      val fractionalSellCommission: Currency = Currency.fromDouble(sell.commission.toDouble * (im.buy.shares.atMult(sell.shares.multiple)/sell.shares.shares))
      val diff: Double = sell.date.decimalYear - im.buy.date.decimalYear
      val ltcg: Boolean = diff >= 1.0
      MatchedBuy(im.buy, im.sold, fractionalSellCommission, ltcg, annualYield(im.buy.cost, sell.gross, diff))
    }

  def annualYield(start: Currency, end: Currency, decimalYears: Double): Double =
    // don't explode if a purchase was made today
    // TODO: we can't really compare decimalYears to 0.0
    if (end==start || decimalYears==0.0 || start==Currency.zero) return 0.0
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

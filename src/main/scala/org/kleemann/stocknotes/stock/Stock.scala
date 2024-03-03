package org.kleemann.stocknotes.stock

import scala.collection.mutable
import scala.util.control.Breaks.{break, breakable}

import org.kleemann.stocknotes.{Config, Date, Fraction, Ticker}
import scala.annotation.tailrec

/**
  * The stock class is an extraction of the log/<ticker>.txt file.
  * It contains a list of date separated content entries,
  * a list of trades, various attributes such as name and cid.
  * BUY and SELL watch values are replaced each time they are entred
  * so only the last one counts.
  *
  * @param ticker
  * @param name
  * @param cid
  * @param keywords
  * @param entries
  * @param trades
  * @param buyWatch
  * @param sellWatch
  */
final case class Stock(
    ticker: Ticker, 
    name: Option[String], 
    cid: Option[String], 
    keywords: Set[String], 
    entries: List[Entry], 
    trades: List[Trade], 
    buyWatch: BuyWatch, 
    sellWatch: SellWatch) {

    /**
     * Return the date of the most recent Entries value.
     * Useful for sorting.
     */
    lazy val latestDate: Date = if (entries.isEmpty) Date.earliest else entries.last.date
}

object Stock {

    /**
      * Loads all stock files from the standard log directory.
      * Errors in parsing will result in a call to sys.exit(1)
      *
      * @param config the standard config file
      * @return
      */
    def load(config: Config): List[Stock] = load(config.logDir)

    /**
      * Loads all stock files from the dead log directory.
      * Errors in parsing will result in a call to sys.exit(1)
      *
      * @param config the standard config file
      * @return
      */
    def loadDead(config: Config): List[Stock] = load(config.deadDir)

    private def load(dir: os.Path): List[Stock] = 
        os.list(dir).flatMap{ f =>
            if (f.ext != "txt") Nil
            else {
                load(Ticker(f.baseName), f.toString, os.read.lines.stream(f)) match {
                    case Right(stock) => List(stock)
                    case Left(error) => {
                        println(error)
                        sys.exit(1)
                    }
                }
            }
        }.toList

    private val namePattern = "^NAME:(.*)$".r
    private val cidPattern = "^CID:(.*)$".r
    private val keywordsPattern = """^KEYWORDS:\s(.*)$""".r
    private val keywordPattern = """^([a-z][a-z0-9_]*)$""".r
    private val tradePattern = """^(TRADE)\s.*$""".r
    private val buySellWatchPattern = """^(BUY|SELL)\s.*$""".r

    /**
      * This is where the bulk of the file parsing happens.
      * The signature was designed for testing.
      *
      * @param ticker this usually comes from the filename
      * @param filename the filename of the input file needed for error reporting
      * @param g the contents of the input file
      * @return either an error string or a valid Stock object
      */
    def load(ticker: Ticker, filename: String, g: os.Generator[String]): Either[String, Stock] =
        loadFunctional(ticker, filename, g)

    def loadImperative(ticker: Ticker, filename: String, g: os.Generator[String]): Either[String, Stock] = {

        var lineNo = 0
        var stockName = Option.empty[String]
        var stockCid = Option.empty[String]
        var keywords = Set[String]()
        var entryDate = Date.earliest
        val entryText = scala.collection.mutable.StringBuilder()
        var entryContent = List[String | Trade | Watch]()
        var currentShares = Shares(0, Fraction.one)
        var currentMult = Fraction.one
        var entries = List[Entry]() // we add new entries to the head of the list so reverse before making the final list
        var trades  = List[Trade]() // we add new trades to the head of the list so reverse before making the final list
        var buyWatch = BuyWatch.none
        var sellWatch = SellWatch.none
        var error: String = null
        def addContent(content: Trade | Watch): Unit = {
            // first add any accumulated text content
            val s = entryText.result()
            if (!s.isEmpty) 
                entryContent = s :: entryContent
            entryText.clear()
            if (content != null)
                entryContent = content :: entryContent
        }
        breakable {
            g.foreach { line => { 
                lineNo = lineNo + 1
                // if we match a date, we finish up the current entry and start the next
                Date.parse(line) match {
                    case Some(d: Date) => {
                        if (d <= entryDate) {
                            error = s"date $d is not greater than previous date $entryDate"
                            break
                        } else {
                            addContent(null)
                            entries = Entry(ticker, entryDate, entryContent.reverse) :: entries
                            entryDate = d
                            entryContent = Nil
                        }
                    }
                    case _ => {
                        // continue parsing
                        line match {
                            case namePattern(name) => stockName = Some(name.trim())
                            case cidPattern(cid) => stockCid = Some(cid.trim())
                            case keywordsPattern(ks) => {
                                val kws: Array[String] = ks.split("\\s+")
                                kws.find{ !keywordPattern.matches(_) }.map { k =>
                                    error = s"keywords must be alphanumeric with underscores: $k"
                                    break
                                }
                                keywords = kws.toSet
                            }
                            case tradePattern(_) => // I needed to match something to get this to work
                                Trade.parse(line, entryDate, currentMult) match {
                                    case Right((trade, balance)) => {
                                        trade match {
                                            case Buy (d, shares, price, commission) => currentShares = currentShares.add(shares, currentMult)
                                            case Sell(d, shares, price, commission) => currentShares = currentShares.sub(shares, currentMult)
                                            case Split(_, splitMultiple) => {
                                                currentMult = currentMult * splitMultiple
                                                // bring the current shares up to the current multiple
                                                currentShares = currentShares.add(Shares.zero, currentMult)
                                            }
                                        }
                                        if (currentShares.shares < 0) {
                                            error = s"share count cannot be negative: $currentShares"
                                            break
                                        }
                                        assert(balance.multiple == currentShares.multiple)
                                        if (currentShares != balance) {
                                            error = s"listed balance: $balance does not equal calculated: $currentShares"
                                            break
                                        }
                                        trades = trade :: trades
                                        addContent(trade)
                                    }
                                    case Left(e) => {
                                        error = e
                                        break
                                    }
                                }
                            case buySellWatchPattern(_) =>
                                Watch.parse(line, currentMult) match {
                                    case Right(b: BuyWatch) => {
                                        buyWatch = b
                                        addContent(b)
                                    }
                                    case Right(s: SellWatch) => {
                                        sellWatch = s
                                        addContent(s)
                                    }
                                    case Left(e) => {
                                        error = e
                                        break
                                    }
                                }
                            case _ => {
                                entryText.append(line)
                                entryText.append("\n")
                            }
                        }
                    }
                }
            }}
        }
        if (error != null) Left(s"$filename($lineNo): $error")
        else {
            // add special keywords
            if (currentShares.shares != 0) keywords = keywords + "owned"
            else if (trades.length > 0)
                // no shares but some trades means we once owned this and now have sold it
                keywords = keywords + "sold"
            if (buyWatch != BuyWatch.none || sellWatch != SellWatch.none)
                keywords = keywords + "watching"

            // wrap up the final entry
            addContent(null)
            entries = Entry(ticker, entryDate, entryContent.reverse) :: entries
            // build our return object
            Right(Stock(ticker, stockName, stockCid, keywords, entries.reverse, trades.reverse, buyWatch, sellWatch))
        }
    }

    /**
      * StockBuilder is used internally by the load() command.
      * 
      * The caller is responsible for parsing the stock log file line by line,
      * translating the lines of text into actual objects, handling possible 
      * input errors, and feeding them to this builder via add().
      * Once the log file has been read, the toStock() function should be called 
      * to generate the resulting Stock object.
      * 
      * The constructor should just be called with the ticker argument.
      * 
      * This class came about when I was trying to figure out how to write a pure
      * functional version of the Stock load() function. The result is that this object
      * is re-constructed via copy() after every input line is parsed. This sounds very 
      * inefficient but since it is immutable most of the work is just copying a few 
      * references and the final run still seems really fast. Profiling shows the time
      * of each implementation to be within 5% of each other.
      */
    private case class StockBuilder(
        // attributes from Stock
        ticker: Ticker, 
        name: Option[String] = None, 
        cid: Option[String] = None, 
        keywords: Set[String] = Set(), 
        entries: List[Entry] = Nil, // reverse order
        trades: List[Trade] = Nil,  // reverse order
        buyWatch: BuyWatch = BuyWatch.none, 
        sellWatch: SellWatch= SellWatch.none,

        // attributes needed while iterating
        date: Date = Date.earliest,
        content: List[String | Trade | Watch] = Nil, // reverse order, strings can be doubled up
        multiple: Fraction = Fraction.one,
        shares: Shares = Shares.zero
        ) {

        def add(v: String | Trade | Watch | Date): StockBuilder = v match {
            case newString: String => this.copy(content = newString :: content)
            case buy: Buy => {
                val newShares = shares.add(buy.shares, multiple)
                this.copy(content = buy :: content, trades = buy :: trades, shares = newShares)
            }
            case sell: Sell => {
                val newShares = shares.sub(sell.shares, multiple)
                this.copy(content = sell :: content, trades = sell :: trades, shares = newShares)
            }
            case split: Split => {
                val newMult = multiple * split.multiple
                // bring the current shares up to the current multiple
                val newShares = shares.add(Shares.zero, newMult)
                this.copy(content = split :: content, trades = split :: trades, shares = newShares, multiple = newMult)
            }
            case bw: BuyWatch  => this.copy(content = bw :: content, buyWatch  = bw)
            case sw: SellWatch => this.copy(content = sw :: content, sellWatch = sw)
            case newDate: Date => {
                // a date signifies that the current entry has "finished"
                // a new Entry needs to be created,
                // and the accumulating content needs to be reset.
                val newEntry = Entry(ticker, date, content.reverse)
                this.copy(entries = newEntry :: entries, date = newDate, content = Nil)
            }
        }

        def toStock: Stock = {
            // add special keywords
            val keywords2 = if (shares.shares != 0)
                keywords + "owned"
            else if (trades.length > 0)
                // no shares but some trades means we once owned this and now have sold it
                keywords + "sold"
            else
                keywords

            val keywords3 = 
                if (buyWatch != BuyWatch.none || sellWatch != SellWatch.none)
                    keywords2 + "watching"
                else
                    keywords2

            val sb = add(Date.latest)
            // the only thing changed by addDate() is "entries" but it's probably safest to use everything from the new StockBuilder
            Stock(sb.ticker, sb.name, sb.cid, keywords3, sb.entries.reverse, sb.trades.reverse, sb.buyWatch, sb.sellWatch)
        }
    }

    def loadFunctional(ticker: Ticker, filename: String, g: os.Generator[String]): Either[String, Stock] = {
        
        def mkError(lineNo: Int, msg: String): Either[String, Stock] = Left(s"$filename($lineNo): $msg")

        @tailrec
        def processLine(in: Seq[String], prevLineNo: Int, sb: StockBuilder): Either[String, Stock] = {
            if (in.isEmpty) Right(sb.toStock)
            else {
                val line = in.head
                val lineNo = prevLineNo + 1
                Date.parse(line) match {
                    case Some(d: Date) => {
                        if (d <= sb.date)
                            mkError(lineNo, s"date $d is not greater than previous date ${sb.date}")
                        else
                            processLine(in.tail, lineNo, sb.add(d))
                    }
                    case _ => {
                        // continue parsing
                        line match {
                            case namePattern(name) => processLine(in.tail, lineNo, sb.copy(name = Some(name.trim())))
                            case cidPattern(cid)   => processLine(in.tail, lineNo, sb.copy(cid  = Some( cid.trim())))
                            case keywordsPattern(ks) => {
                                val kws: Array[String] = ks.split("\\s+")
                                kws.find{ !keywordPattern.matches(_) } match {
                                    case Some(badKeyword) => mkError(lineNo, s"keywords must be alphanumeric with underscores: $badKeyword")
                                    case None => processLine(in.tail, lineNo, sb.copy(keywords = kws.toSet))
                                }
                            }
                            case tradePattern(_) => Trade.parse(line, sb.date, sb.multiple) match {
                                case Right((trade, balance)) => {
                                    val newSb = sb.add(trade)
                                    assert(balance.multiple == newSb.multiple)
                                    if (newSb.shares.shares < 0)
                                        mkError(lineNo, s"share count cannot be negative: ${newSb.shares}")
                                    else if (newSb.shares != balance)
                                        mkError(lineNo, s"listed balance: $balance does not equal calculated: ${newSb.shares}")
                                    else
                                        processLine(in.tail, lineNo, newSb)
                                }
                                case Left(e) => mkError(lineNo, e)
                            }
                            case buySellWatchPattern(_) => Watch.parse(line, sb.multiple) match {
                                case Right(w: Watch)  => processLine(in.tail, lineNo, sb.add(w))
                                case Left(e)          => mkError(lineNo, e)
                            }
                            case _ => processLine(in.tail, lineNo, sb.add(line + "\n"))
                        }
                    }
                }
            }
        }
        processLine(g.toSeq, 0, new StockBuilder(ticker))
    }

}

package org.kleemann.stocknotes.stock

import scala.util.control.Breaks.{break, breakable}

import org.kleemann.stocknotes.{Config, Ticker}
import scala.annotation.tailrec
import scala.collection.mutable

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
      *
      * @param config the standard config file
      * @return
      */
    def load(config: Config): List[Stock] = 
        os.list(config.logDir).flatMap{ f =>
                if (f.ext == "txt") List(load(f))
                else                Nil
        }.toList

    /**
      * Loads the specified stock file. Errors in parsing will result in a sys.exit(1)
      *
      * @param f
      * @return
      */
    def load(f: os.Path): Stock = {
        // TODO: maybe the ticker/file conversion should be moved to the inner load()
        val ticker = Ticker(f.baseName)
        val e = load(ticker, f.toString, os.read.lines.stream(f))
        e match {
            case Right(stock) => stock
            case Left(error) => {
                println(error)
                sys.exit(1)
            }
        }
    }

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

    def loadFunctional(ticker: Ticker, filename: String, g: os.Generator[String]): Either[String, Stock] = {
        
        case class StockBuilder(
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
            // TODO: not sure if multiple and shares should be part of this object since the object doesn't use them
            date: Date = Date.earliest,
            content: List[String | Trade | Watch] = Nil, // reverse order, strings can be doubled up
            multiple: Fraction = Fraction.one,
            shares: Shares = Shares.zero
            ) {

            // a date separator has been found in the content
            def addDate(newDate: Date): StockBuilder = {
                // a date signifies that the currentEntry has "finished", needs to be added to the entries list,
                // and a new blank currentEntry needs to be created

                // coalesce adjacent Strings in content into single Strings
                // The external, mutable StringBuilder is not functional but it is contained to this small block of code.
                val sb = mutable.StringBuilder()
                val newContent1: List[String | Trade | Watch] = content.reverse.flatMap{ c => c match {
                    case s: String => {
                        sb.append(s)
                        List()
                    }
                    case other: (Trade | Watch) => {
                        if (sb.isEmpty) List(other)
                        else {
                            val combinedString = sb.result()
                            sb.clear()
                            List(combinedString, other)
                        }
                    }
                }}
                val newContent2: List[String | Trade | Watch] = if (sb.isEmpty) newContent1 else newContent1 :+ sb.toString()

                val newEntry = Entry(ticker, date, newContent2)
                this.copy(entries = newEntry :: entries, date = newDate, content = Nil)
            }

            def addContent(v: String | Trade | Watch): StockBuilder = v match {
                case newString: String => this.copy(content = newString :: content)
                case buy: Buy => {
                    val newShares = shares.add(buy.shares, multiple)
                    this.copy(content = buy :: content, trades = buy :: trades, shares = newShares)
                }
                case sell: Sell => {
                    val newShares = shares.sub(sell.shares, multiple)
                    this.copy(content = sell :: content, trades = sell  :: trades, shares = newShares)
                }
                case split: Split => {
                    val newMult = multiple * split.multiple
                    // bring the current shares up to the current multiple
                    val newShares = shares.add(Shares.zero, newMult)
                    this.copy(content = split :: content, trades = split :: trades, shares = newShares, multiple = newMult)
                }
                case bw: BuyWatch  => this.copy(content = bw :: content, buyWatch = bw)
                case sw: SellWatch => this.copy(content = sw :: content, sellWatch = sw)
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

                val sb = addDate(Date.latest)
                // the only thing changed is "entries" but it's probably safest to use everything from the new StockBuilder
                Stock(sb.ticker, sb.name, sb.cid, keywords3, sb.entries.reverse, sb.trades.reverse, sb.buyWatch, sb.sellWatch)
            }
        }

        def mkError(lineNo: Int, s: String): Either[String, Stock] = Left(s"$filename($lineNo): $s")

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
                            processLine(in.tail, lineNo, sb.addDate(d))
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
                                    val newSb = sb.addContent(trade)
                                    assert(balance.multiple == newSb.multiple)
                                    if (newSb.shares.shares < 0) mkError(lineNo, s"share count cannot be negative: ${newSb.shares}")
                                    else if (newSb.shares != balance) mkError(lineNo, s"listed balance: $balance does not equal calculated: ${newSb.shares}")
                                    else processLine(in.tail, lineNo, newSb)
                                }
                                case Left(e) => mkError(lineNo, e)
                            }
                            case buySellWatchPattern(_) => Watch.parse(line, sb.multiple) match {
                                case Right(b: BuyWatch)  => processLine(in.tail, lineNo, sb.addContent(b))
                                case Right(s: SellWatch) => processLine(in.tail, lineNo, sb.addContent(s))
                                case Left(e) => mkError(lineNo, e)
                            }
                            case _ => processLine(in.tail, lineNo, sb.addContent(line + "\n"))
                        }
                    }
                }
            }
        }
        processLine(g.toSeq, 0, new StockBuilder(ticker))
    }

}

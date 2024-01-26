package org.kleemann.stocknotes.stock

import scala.util.control.Breaks.{break, breakable}

import org.kleemann.stocknotes.{Config, Ticker}

/**
  * The stock class is an extraction of the log/<ticker>.txt file.
  * It contains a list of date separated text entries,
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
    sellWatch: SellWatch)

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
      * @param filename
      * @param g
      * @return
      */
    def load(ticker: Ticker, filename: String, g: os.Generator[String]): Either[String, Stock] = {

        var lineNo = 0
        var stockName = Option.empty[String]
        var stockCid = Option.empty[String]
        var keywords = Set[String]()
        var entryDate = Date.earliest
        val entryText = scala.collection.mutable.StringBuilder()
        var currentShares = Shares(0, Fraction.one)
        var currentMult = Fraction.one
        var entries = List[Entry]() // we add new entries to the head of the list so reverse before making the final list
        var trades  = List[Trade]() // we add new trades to the head of the list so reverse before making the final list
        var buyWatch = BuyWatch.none
        var sellWatch = SellWatch.none
        var error: String = null
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
                            entries = Entry(ticker, entryDate, entryText.result()) :: entries
                            entryDate = d
                            entryText.clear()
                        }
                    }
                    case _ => {
                        // continue parsing
                        line match {
                            case namePattern(name) => stockName = Some(name.strip())
                            case cidPattern(cid) => stockCid = Some(cid.strip())
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
                                    }
                                    case Left(e) => {
                                        error = e
                                        break
                                    }
                                }
                            case buySellWatchPattern(_) =>
                                Watch.parse(line, currentMult) match {
                                    case Right(b: BuyWatch) => buyWatch = b
                                    case Right(s: SellWatch) => sellWatch = s
                                    case Left(e) => {
                                        error = e
                                        break
                                    }
                                }
                            case _ =>
                        }
                        // all of the above special text is also placed in the entry text
                        entryText.append(line)
                        entryText.append("\n")
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
            entries = Entry(ticker, entryDate, entryText.result()) :: entries
            // build our return object
            Right(Stock(ticker, stockName, stockCid, keywords, entries.reverse, trades.reverse, buyWatch, sellWatch))
        }
    }

}

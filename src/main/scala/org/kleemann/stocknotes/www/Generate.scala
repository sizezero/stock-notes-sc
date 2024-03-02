package org.kleemann.stocknotes.www

import org.kleemann.stocknotes.{Config, Currency, Date, Fraction, Quote, Ticker}
import org.kleemann.stocknotes.stock.{Shares, Stock}
import org.kleemann.stocknotes.stock.{Trade, Buy, Sell, Split}
import org.kleemann.stocknotes.stock.{BuyWatch, SellWatch, Watch}

object Generate {

    // If we want to test this, we need to separate the parsing and html rendering

    /**
      * Converts a Stock log to viewable html.
      *
      * @param stock
      * @return the complete html file as a String
      */
    private def stockToHtml(stock: Stock): String = {
        val content = {
            import scalatags.Text.all._
            import scalatags.Text.TypedTag

            def dispMult(multiple: Fraction): String = 
                multiple.toString().replace("/",":")

            def dispShares(shares: Shares): String = 
                f"""${shares.shares}(${dispMult(shares.multiple)})"""

            def dispOptionCurrency(oc: Option[Currency]): String =
                if (oc.isDefined) oc.get.toString else "none"

            def dispTrade(t: Trade): TypedTag[String] = t match {
                case buy: Buy => div(
                    span(color := "LightGreen")("BUY"), " ",
                    dispShares(buy.shares), "@", buy.price.toString, " ",
                    "commission", " ",
                    buy.commission.toString,
                    "\n"
                )
                case sell: Sell => div(
                    span(color := "IndianRed")("SELL"), " ",
                    dispShares(sell.shares), "@", sell.price.toString, " ",
                    "commission", " ",
                    sell.commission.toString,
                    "\n"
                )
                case split: Split => div(
                    span(color := "LightSkyBlue")("SPLIT"), " ",
                    dispMult(split.multiple)
                )
            }

            def dispWatch(w: Watch): TypedTag[String] = w match {
                case buy: BuyWatch => div(
                    span(color := "LightGreen")("BUY WATCH"), " ",
                    dispOptionCurrency(buy.low), " ",
                    dispOptionCurrency(buy.high), " ",
                    dispMult(buy.multiple)
                )
                case sell: SellWatch => div(
                    span(color := "IndianRed")("SELL WATCH"), " ",
                    dispOptionCurrency(sell.low), " ",
                    dispOptionCurrency(sell.high), " ",
                    dispMult(sell.multiple)
                )
            }

            html (
                head(
                    link(rel := "stylesheet", href := "https://www.w3.org/StyleSheets/Core/Midnight", `type` := "text/css")
                ), "\n",
                body(
                    h1(stock.ticker.name),
                    if (stock.name.isDefined) div(stock.name.get, br(), "\n") else "",
                    if (stock.cid.isDefined) div("CIK: ",stock.cid.get, br(), "\n")  else "",
                    if (!stock.keywords.isEmpty) div(stock.keywords.toList.sorted.mkString(", "), br(), "\n") else "",

                    for (
                        entry <- stock.entries
                    ) yield div( // can't figure out how not to put a div here
                        hr(),
                        if (entry.date != Date.earliest) h5(entry.date.toString()) else "",
                        "\n",
                        for (c <- entry.content) yield div (
                            c match {
                                case s: String => for (line <- s.split("\n")) yield div(
                                    line, br(), "\n"
                                )
                                case t: Trade => div( dispTrade(t), "\n" )
                                case w: Watch => div( dispWatch(w), "\n" )
                            }
                        )
                    )
                )
            )
        }
        content.toString
    }

    // case class to make paramater passing easier to type check.
    // We don't want a whole bunch of strings in a row.
    private case class Files(all: String, dead: String, buySell: String)

    /**
      * Displays the list of all Stock log files. Four versions of these files are generated, each sorted by:
      * name, reverse name, last entry date, reverse last entry date.
      *
      * @param stocks all log files as Stock objects; sorted in displayable order; may be active or dead
      * @param files The three core index files: all, dead, buySell, with default sorting.
      * @param otherTicker the version of this index file with the opposite ticker sorting
      * @param otherDate the version of this index file with the opposite date sorting
      * @param refDir either "log" or "dead"
      * @return the complete html file as a String
      */
    private def generateAll(stocks: List[Stock], files: Files, otherTicker: String, otherDate: String, refDir: String): String = {
        val content = {
            import scalatags.Text.all._
            import scalatags.Text.TypedTag
            html (
                head(
                    link(rel := "stylesheet", href := "https://www.w3.org/StyleSheets/Core/Midnight", `type` := "text/css")
                ), "\n",
                body (
                    table (
                        tr(
                            //td(border := "20px solid black")(h1( "All")),
                            td(h1(a(href := files.all)    ("All"))),
                            td(h1(a(href := files.buySell)("Watch"))),
                            td(h1(a(href := files.dead)   ("Dead")))
                        ),
                        tr(
                            td(h3(a(href := otherTicker)("Ticker"))), "\n",
                            td(h3(a(href := otherDate)("Last note"))), "\n"
                        ), "\n",
                        for (
                            s <- stocks
                        ) yield tr(
                            td(a(href := f"http://finance.yahoo.com/q?s=${s.ticker}")(s.ticker.name)), "\n",
                            td(a(href := f"${refDir}/${s.ticker.name.toLowerCase}.txt.html")(s.latestDate.toString())), "\n"
                        ), "\n"
                    )
                )
            );
        }
        content.toString
    }

    /**
      * Displays the list of all Watched stocks. Four versions of these files are generated, each sorted by:
      * name, reverse name, last entry date, reverse last entry date.
      *
      * @param stocks all Stocks that have active Watches; sorted in displayable order
      * @param stockQuotes current prices for the above stocks
      * @param files The three core index files: all, dead, buySell, with default sorting.
      * @param otherTicker the version of this index file with the opposite ticker sorting
      * @param otherDate the version of this index file with the opposite date sorting
      * @return the complete html file as a String
      */
    private def generateBuySell(stocks: List[Stock], stockQuotes: Map[Ticker, Currency], files: Files, otherTicker: String, otherDate: String): String = {
        val bd = "10px solid black"
        val content = {
            import scalatags.Text.all._
            import scalatags.Text.TypedTag

            def dispTd(oc: Option[Currency], stockQuote: Currency, bgColor: String, colorIfLessThan: (Currency, Currency) => Boolean): TypedTag[String] = {
                val (content, styleText) = oc match {
                    case Some(c) => (
                        c.toString,
                        if (colorIfLessThan(stockQuote, c)) f"background-color: $bgColor; color: White" else ""
                    )
                    case None => ("", "")
                }
                td(style := styleText)(content)
            }

            html (
                head(
                    link(rel := "stylesheet", href := "https://www.w3.org/StyleSheets/Core/Midnight", `type` := "text/css")
                ), "\n",
                body (
                    table (
                        tr(
                            td(colspan := "3")(h1(a(href := files.all)("All"))),
                            td(colspan := "3")(h1("Watch")),
                            td(colspan := "3")(h1(a(href := files.dead)("Dead")))
                        ),
                        tr(
                            td(border := bd)(h3(a(href := otherTicker)("Ticker"))), "\n",
                            td(border := bd)(h3("quote")), "\n",
                            td(border := bd)(h3("buy")), td(border := bd)(h3("buy")), td(border := bd)(h3("sell")), td(border := bd)(h3("sell")), "\n",
                            td(border := bd)(h3(a(href := otherDate)("Last Note"))), "\n"
                        ), "\n",
                        for (
                            s <- stocks;
                            // no way to put vals after this so put it here even though it's not a generator
                            price = stockQuotes.get(s.ticker).get
                        ) yield tr(
                            td(a(href := f"http://finance.yahoo.com/q?s=${s.ticker}")(s.ticker.name)), "\n",
                            td(price.toString), "\n",
                            dispTd(s.buyWatch.low,   price, "DarkGreen", _ < _), "\n",
                            dispTd(s.buyWatch.high,  price, "DarkGreen", _ < _), "\n",
                            dispTd(s.sellWatch.low,  price, "DarkRed",   _ > _), "\n",
                            dispTd(s.sellWatch.high, price, "DarkRed",   _ > _), "\n",
                            td(a(href := f"log/${s.ticker.name.toLowerCase}.txt.html")(s.latestDate.toString())), "\n"
                        ), "\n"
                    )
                )
            )
        }
        content.toString
    }

    /**
      * Wipe the ~/.stockquotes/www/ dir and regenerate it with files.
      * Side effects include reading config file, log and dead stock files,
      * and writing out the html files.
      *
      * www/
      *   index.html : redirect to buysell-ticker.html
      *   all-ticker.html
      *   all-ticker-reverse.html
      *   all-date.html
      *   all-date-reverse.html
      *   dead-ticker.html
      *   dead-ticker-reverse.html
      *   dead-date.html
      *   dead-date-reverse.html
      *   buysell-ticker.html
      *   buysell-ticker-reverse.html
      *   buysell-date.html
      *   buysell-date-reverse.html
      *   log/
      *     <ticker1>.txt.html
      *     <ticker2>.txt.html
      *     ...
      */
    def refreshWwwDir(): Unit = {
        val config = Config.load()
        val stocks = Stock.load(config)
        val dead = Stock.loadDead(config)
        val quotes = Quote.load(config)

        // clean files out of www
        os.remove.all(config.wwwDir)
        os.makeDir(   config.wwwDir)

        os.write(config.wwwDir/"index.html", """<meta HTTP-EQUIV="REFRESH" content="0; url=http://www.kleemann.org/stocks/buysell-ticker.html">\n\n""")

        // generate all log files
        val logDstDir = config.wwwDir/"log"
        os.makeDir(logDstDir)
        stocks.foreach{ s =>
            val outFile = logDstDir / (s.ticker.name.toLowerCase() + ".txt.html")
            os.write(outFile, stockToHtml(s))
        }

        // generate all dead files
        val deadDstDir = config.wwwDir/"dead"
        os.makeDir(deadDstDir)
        dead.foreach{ s =>
            val outFile = deadDstDir / (s.ticker.name.toLowerCase() + ".txt.html")
            os.write(outFile, stockToHtml(s))
        }

        // name the files for brevity
        val at  = "all-ticker.html"
        val atr = "all-ticker-reverse.html"
        val ad  = "all-date.html"
        val adr = "all-date-reverse.html"
        val dt  = "dead-ticker.html"
        val dtr = "dead-ticker-reverse.html"
        val dd  = "dead-date.html"
        val ddr = "dead-date-reverse.html"
        val bt  = "buysell-ticker.html"
        val btr = "buysell-ticker-reverse.html"
        val bd  = "buysell-date.html"
        val bdr = "buysell-date-reverse.html"
        val files = Files(all=at, dead=dt, buySell=bt) // default index files

        // generate files with all tickers
        os.write(config.wwwDir/at,
            generateAll(
                stocks.sortWith{ _.ticker.name < _.ticker.name },
                files, atr, ad, "log"))
        os.write(config.wwwDir/atr,
            generateAll(
                stocks.sortWith{ _.ticker.name >= _.ticker.name },
                files, at, ad, "log"))
        os.write(config.wwwDir/ad,
            generateAll(
                stocks.sortWith{ _.latestDate < _.latestDate },
                files, at, adr, "log"))
        os.write(config.wwwDir/adr,
            generateAll(
                stocks.sortWith{ _.latestDate >= _.latestDate },
                files, at, ad, "log"))

        // generate files with dead tickers
        os.write(config.wwwDir/dt,
            generateAll(
                dead.sortWith{ _.ticker.name < _.ticker.name },
                files, dtr, dd, "dead"))
        os.write(config.wwwDir/dtr,
            generateAll(
                dead.sortWith{ _.ticker.name >= _.ticker.name },
                files, dt, dd, "dead"))
        os.write(config.wwwDir/dd,
            generateAll(
                dead.sortWith{ _.latestDate < _.latestDate },
                files, dt, ddr, "dead"))
        os.write(config.wwwDir/ddr,
            generateAll(
                dead.sortWith{ _.latestDate >= _.latestDate },
                files, dt, dd, "dead"))

        // generate files with just buysell tickers
        val watched = stocks.filter{ s => s.buyWatch!=BuyWatch.none || s.sellWatch!=SellWatch.none }
        os.write(config.wwwDir/bt,
            generateBuySell(
                watched.sortWith{ _.ticker.name < _.ticker.name }, quotes,
                files, btr, bd))
        os.write(config.wwwDir/btr,
            generateBuySell(
                watched.sortWith{ _.ticker.name >= _.ticker.name }, quotes,
                files, bt, bd))
        os.write(config.wwwDir/bd,
            generateBuySell(
                watched.sortWith{ _.latestDate < _.latestDate }, quotes,
                files, bt, bdr))
        os.write(config.wwwDir/bdr,
            generateBuySell(
                watched.sortWith{ _.latestDate >= _.latestDate }, quotes,
                files, bt, bd))
    }
}

package org.kleemann.stocknotes.command.www

import org.kleemann.stocknotes.{Config, Quote, Ticker}
import org.kleemann.stocknotes.stock.{Currency, Stock}
import org.kleemann.stocknotes.stock.{BuyWatch, SellWatch}

object Generate {

    // If we want to test this, we need to separate the parsing and html rendering

    private def stockToHtml(stock: Stock): String = {
        val content = {
            import scalatags.Text.all._
            html (
                head(),
                body(
                    for (
                        entry <- stock.entries
                    ) yield div( // can't figure out how not to put a div here
                        hr(),
                        entry.date.toString(), br(), "\n",
                        for (line <- entry.text.split("\n")) yield div(line, br(), "\n")
                    )
                )
            )
        }
        content.toString
    }

    private def generateAll(stocks: List[Stock], buySellFile: String, otherTicker: String, otherDate: String): String = {
        val bd = "1px solid black"
        val content = {
            import scalatags.Text.all._
            import scalatags.Text.TypedTag
            html (
                head(
                    h1( "All Tickers", " ", a(href := buySellFile)("BuySell Tickers") )
                ), "\n",
                body (
                    table(border := bd ) (
                        tr(
                            td(border := bd)(a(href := otherTicker)("ticker")), "\n",
                            td(border := bd)(a(href := otherDate)("last note")), "\n"
                        ), "\n",
                        for (
                            s <- stocks
                        ) yield tr(
                            td(border := bd)(a(href := f"http://finance.yahoo.com/q?s=${s.ticker}")(s.ticker.ticker)), "\n",
                            td(border := bd)(a(href := f"log/${s.ticker.ticker.toLowerCase}.txt.html")(s.latestDate.toString())), "\n"
                        ), "\n"
                    )
                )
            )
        }
        content.toString
    }

    private def generateBuySell(stocks: List[Stock], stockQuotes: Map[Ticker, Quote], allFile: String, otherTicker: String, otherDate: String): String = {
        val bd = "1px solid black"
        val content = {
            import scalatags.Text.all._
            import scalatags.Text.TypedTag

            def dispTd(oc: Option[Currency], stockQuote: Currency, bgColor: String, colorIfLessThan: (Currency, Currency) => Boolean): TypedTag[String] = {
                // it seems I can do vals and other code within a function but not within a content tag
                val bow = "background-color: White; color: Black"
                val (content, styleText) = oc match {
                    case Some(c) => (
                        c.toString,
                        if (colorIfLessThan(stockQuote, c)) f"background-color: $bgColor; color: White" else bow
                    )
                    case None => ("", bow)
                }
                td(border := bd, style := styleText)(content)
            }

            html (
                head(
                    h1( a(href := allFile)("All Tickers"), " ", "BuySell Tickers" )
                ), "\n",
                body (
                    table(border := bd ) (
                        tr(
                            td(border := bd)(a(href := otherTicker)("ticker")), "\n",
                            td(border := bd)("quote"), "\n",
                            td(border := bd)("buy"), td(border := bd)("buy"), td(border := bd)("sell"), td(border := bd)("sell"), "\n",
                            td(border := bd)(a(href := otherDate)("last note")), "\n"
                        ), "\n",
                        for (
                            s <- stocks;
                            // no way to put vals after this so put it here even though it's not a generator
                            price = stockQuotes.get(s.ticker).get.price
                        ) yield tr(
                            td(border := bd)(a(href := f"http://finance.yahoo.com/q?s=${s.ticker}")(s.ticker.ticker)), "\n",
                            td(border := bd, backgroundColor := "LightCyan")(price.toString), "\n",
                            dispTd(s.buyWatch.low,   price, "DarkGreen", _ < _), "\n",
                            dispTd(s.buyWatch.high,  price, "DarkGreen", _ < _), "\n",
                            dispTd(s.sellWatch.low,  price, "DarkRed",   _ > _), "\n",
                            dispTd(s.sellWatch.high, price, "DarkRed",   _ > _), "\n",
                            td(border := bd)(a(href := f"log/${s.ticker.ticker.toLowerCase}.txt.html")(s.latestDate.toString())), "\n"
                        ), "\n"
                    )
                )
            )
        }
        content.toString
    }

    def refreshWwwDir(): Unit = {
        val config = Config.load()
        val stocks = Stock.load(config)
        val quotes = Quote.load(config)

        // clean files out of www
        os.remove.all(config.wwwDir)
        os.makeDir(   config.wwwDir)

        os.write(config.wwwDir/"index.html", "<p>Hello World\n")

        // generate all log files
        val logDstDir = config.wwwDir/"log"
        os.makeDir(logDstDir)
        stocks.foreach{ s =>
            val outFile = logDstDir / (s.ticker.ticker.toLowerCase() + ".txt.html")
            os.write(outFile, stockToHtml(s))
        }

        // name the files for brevity
        val bt  = "buysell-ticker.html"
        val btr = "buysell-ticker-reverse.html"
        val bd  = "buysell-date.html"
        val bdr = "buysell-date-reverse.html"
        val at  = "all-ticker.html"
        val atr = "all-ticker-reverse.html"
        val ad  = "all-date.html"
        val adr = "all-date-reverse.html"

        // generate files with all tickers
        os.write(config.wwwDir/ at,
            generateAll(
                stocks.sortWith{ _.ticker.ticker < _.ticker.ticker },
                bt, atr, ad))
        os.write(config.wwwDir/atr,
            generateAll(
                stocks.sortWith{ _.ticker.ticker >= _.ticker.ticker },
                bt, at, ad))
        os.write(config.wwwDir/ ad,
            generateAll(
                stocks.sortWith{ _.latestDate < _.latestDate },
                bt, at, adr))
        os.write(config.wwwDir/ adr,
            generateAll(
                stocks.sortWith{ _.latestDate >= _.latestDate },
                bt, at, ad))

        // generate files with just buysell tickers
        val watched = stocks.filter{ s => s.buyWatch!=BuyWatch.none || s.sellWatch!=SellWatch.none }
        os.write(config.wwwDir/ bt,
            generateBuySell(
                watched.sortWith{ _.ticker.ticker < _.ticker.ticker }, quotes,
                at, btr, bd))
        os.write(config.wwwDir/btr,
            generateBuySell(
                watched.sortWith{ _.ticker.ticker >= _.ticker.ticker }, quotes,
                at, bt, bd))
        os.write(config.wwwDir/ bd,
            generateBuySell(
                watched.sortWith{ _.latestDate < _.latestDate }, quotes,
                at, bt, bdr))
        os.write(config.wwwDir/ bdr,
            generateBuySell(
                watched.sortWith{ _.latestDate >= _.latestDate }, quotes,
                at, bt, bd))
    }
}

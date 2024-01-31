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
                        e <- stock.entries
                    ) yield div( // can't figure out how not to put a div here
                        hr(),
                        e.date.toString(), br(), "\n",
                        for (line <- e.text.split("\n")) yield div(line, br(), "\n")
                    )
                )
            )
        }
        content.toString
    }

    private def generateBuySell(stocks: List[Stock], stockQuotes: Map[Ticker, Quote], otherTicker: String, otherDate: String): String = {
        val bd = "1px solid black"
        def currencyString(oc: Option[Currency]): String = oc match {
            case Some(c) => c.toString()
            case None    => ""
        }
        val content = {
            import scalatags.Text.all._
            html (
                head(),
                body (
                    table(border := bd ) (
                        tr(
                            td(border := bd)(a(href := otherTicker)("ticker")),
                            td(border := bd)("quote"),
                            td(border := bd)("buy"), td(border := bd)("buy"), td(border := bd)("sell"), td(border := bd)("sell"),
                            td(border := bd)(a(href := otherDate)("last note"))
                        ),
                        for (s <- stocks) yield tr(
                            td(border := bd)(a(href := f"http://finance.yahoo.com/q?s=${s.ticker}")(s.ticker.ticker)),
                            td(border := bd)(stockQuotes(s.ticker).price.toString),
                            td(border := bd)(currencyString(s.buyWatch.low)),
                            td(border := bd)(currencyString(s.buyWatch.high)),
                            td(border := bd)(currencyString(s.sellWatch.low)),
                            td(border := bd)(currencyString(s.sellWatch.high)),
                            td(border := bd)(a(href := f"log/${s.ticker.ticker.toLowerCase}.txt.html")(s.latestDate.toString()))
                        )
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

        // generate buysell files
        def write(ss: List[Stock], out: os.Path, otherTicker: String, otherDate: String): Unit = {
            val content = generateBuySell(ss, quotes, otherTicker, otherDate)
            os.write(out, content)
        }
        val watched = stocks.filter{ s => s.buyWatch!=BuyWatch.none || s.sellWatch!=SellWatch.none }
        write(watched.sortWith{ (s1, s2) => s1.ticker.ticker <  s2.ticker.ticker },
            config.wwwDir/"buysell-ticker.html",         "buysell-ticker-reverse.html", "buysell-date.html")
        write(watched.sortWith{ (s1, s2) => s1.ticker.ticker >= s2.ticker.ticker },
            config.wwwDir/"buysell-ticker-reverse.html", "buysell-ticker.html",         "buysell-date.html")
        write(watched.sortWith{ (s1, s2) => s1.latestDate    <  s2.latestDate },
            config.wwwDir/"buysell-date.html",           "buysell-ticker.html",         "buysell-date-reverse.html")
        write(watched.sortWith{ (s1, s2) => s1.latestDate    >=  s2.latestDate },
            config.wwwDir/"buysell-date-reverse.html",   "buysell-ticker.html",         "buysell-date.html")
    }
}

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

    private def generateBuySell(stocks: List[Stock], stockQuotes: Map[Ticker, Quote], otherTicker: String, otherDate: String): String = {
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
                head(), "\n",
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

        // generate buysell files
        def write(ss: List[Stock], out: os.Path, otherTicker: String, otherDate: String): Unit = {
            val content = generateBuySell(ss, quotes, otherTicker, otherDate)
            os.write(out, content)
        }
        val watched = stocks.filter{ s => s.buyWatch!=BuyWatch.none || s.sellWatch!=SellWatch.none }
        write(watched.sortWith{ (s1, s2) => s1.ticker.ticker <  s2.ticker.ticker },
            config.wwwDir/"buysell-ticker.html",
            "buysell-ticker-reverse.html", "buysell-date.html")
        write(watched.sortWith{ (s1, s2) => s1.ticker.ticker >= s2.ticker.ticker },
            config.wwwDir/"buysell-ticker-reverse.html",
            "buysell-ticker.html",         "buysell-date.html")
        write(watched.sortWith{ (s1, s2) => s1.latestDate    <  s2.latestDate },
            config.wwwDir/"buysell-date.html",
            "buysell-ticker.html",         "buysell-date-reverse.html")
        write(watched.sortWith{ (s1, s2) => s1.latestDate    >=  s2.latestDate },
            config.wwwDir/"buysell-date-reverse.html",
            "buysell-ticker.html",         "buysell-date.html")
    }
}

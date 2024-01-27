package org.kleemann.stocknotes.command

import sttp.client4.quick.*

import org.kleemann.stocknotes.{Config, Ticker}
import org.kleemann.stocknotes.stock.{Date, Stock}
import org.kleemann.stocknotes.stock.{BuyWatch, SellWatch}
import sttp.model.StatusCode

object DownloadQuotes extends Command {

  private val delayInSeconds = 2

  private def downloadQuotes(): Unit = {

    val config = Config.load()

    // we need quotes for stocks we either own or that have active watches
    val tickers = Stock.load(config).filter{ s =>
      (s.keywords contains "owned") || s.buyWatch != BuyWatch.none || s.sellWatch != SellWatch.none
    }.map{ _.ticker }

    val delay: Double = (delayInSeconds/60.0) * tickers.length
    println(f"The download is estimated to take ${delay}%2.2f minutes.")

    os.write.over(config.quotesFile, quotesFileContent(tickers, config.finnhubAccessKey))

    println("Download Complete")
  }

  private def quotesFileContent(tickers: List[Ticker], finnhubAccessKey: String): String = {
  
    // our quotes file format uses this legacy date format
    val today: String = Date.today match {
      case Date(year, month, day) => f"$month%02d/$day%02d/$year%04d"
    }

    // TODO: I can't find good docs on os-lib so I'm just going to collect the quotes in memory and write the file all at once.
    tickers.map{ t =>
      downloadSingleQuote(t, finnhubAccessKey) match {
        case Left(e)  => f"${t.ticker},0.0,${today},${e}\n"
        case Right(q) => f"${t.ticker},${q},${today},\n" 
      }
    }.foldLeft(""){ _ + _ }  // TODO: consider replacing with some efficient string accumulator like StringBuilder
  }

  /**
    * We either return an error or a decimal stock price.
    * We could return it as something besides a string but since we're just going 
    * to write it to a file there's no value in making a complicated type
    *
    * @param ticker
    * @return
    */
  private def downloadSingleQuote(ticker: Ticker, finnhubAccessKey: String): Either[String, String] = {

    // finnhub doesn't allows us to spam their service so we need to slow it down a bit
    Thread.sleep(delayInSeconds * 1000L)

    val queryParams = Map("symbol" -> ticker.ticker,  "token" -> finnhubAccessKey)
    val u = uri"https://finnhub.io/api/v1/quote?${queryParams}"
    val request = quickRequest.get(u)
    val response = request.send()
    if (response.code == StatusCode.Ok) {
      val json = ujson.read(response.body)
      // ex
      // {"c":4.93,"d":-0.08,"dp":-1.5968,"h":5.08,"l":4.91,"o":4.93,"pc":5.01,"t":1706302801}
      val m: Map[String,String] = upickle.default.read[Map[String,String]](json)
      m.get("c") match {
        case Some(price) => Right(price)
        case None        => Left(f"key 'c' not found in json")
      }
    } else
      Left(f"Ticker service failed with response code: ${response.code}")
  }

  val help = Some(s"""
    |stock-notes download-quotes
  """.stripMargin)

  override def command(args: IndexedSeq[String]): Option[String] =
    if (args.length==0) {
      downloadQuotes()
      None
    }
    else help

}

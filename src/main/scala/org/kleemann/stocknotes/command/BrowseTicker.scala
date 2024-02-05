package org.kleemann.stocknotes.command

import sttp.client4.quick.*
import sttp.model.Uri

import org.kleemann.stocknotes.stock.{Stock}
import org.kleemann.stocknotes.{Config, Ticker}
import os.Shellable

// some things this needs
// - its all about side effects; I probably need some pure functional fns in here to make sure this works.
// - I could pass configuration into this that has all of the read values
// - I could also pass an IO object in here that has implementations for openBrowser, openEditor, and others.
// - before I implementing this I need a fully populated "companies" singleton.
// - I don't think I want to go overboard on adding test configurations to this. Just get it to work and if tests need help, refactor.
// - To start lets just make each of these command that correctly parse their arguments.

object BrowseTicker extends Command {

  private def browseTicker(ticker: Ticker, edit: Boolean): Unit = {
  
    val easyUrls: List[String] = List(
      f"https://finance.yahoo.com/quote/${ticker.ticker}/profile",
      f"https://finance.yahoo.com/quote/${ticker.ticker}/financials",
      f"https://seekingalpha.com/symbol/${ticker.ticker}",
      f"https://www.morningstar.com/stocks/xnas/${ticker.ticker.toLowerCase()}/quote"
    )

    val config = Config.load()
    val stocks = Stock.load(config)
    val cik: Option[String] = stocks.find{ _.ticker == ticker}.flatMap{ _.cid }
    val secUrls: List[String] = cik match {
      case Some(cik) => List(f"https://www.sec.gov/edgar/browse/?CIK=${cik}")
      case None => List(
        f"http://sec.gov/edgar/searchedgar/companysearch.html",
        f"http://sec.gov/cgi-bin/browse-edgar?company=&CIK=${ticker.ticker}&filenum=&State=&SIC=&owner=include&action=getcompany",
        config.noCikUrl.toString
      )
    }

    val urls = easyUrls ++ secUrls

    val cmd = Shellable("open_as_chrome_tabs" :: urls)
    println(f"running command: ${cmd.toString}")
    os.proc(cmd).spawn() // unlike run(), spawn() allows this program to terminate

    if (edit) {
      val editor = os.root / "usr" / "bin" / "gedit"
      val logFile = config.logDir / f"${ticker.ticker.toLowerCase()}.txt"
      println(f"running command: ${editor.toString} ${logFile.toString}")
      os.proc(editor, logFile.toString).spawn()
    }
  }

  val help = Some(s"""
    |stock-notes browse-ticker [-n] <ticker>
    |    -n: do not open an editor
  """.stripMargin)

  override def command(args: IndexedSeq[String]): Option[String] = {
    if (args.length == 1) {
      browseTicker(Ticker(args(0)), true)
      None
    }
    else if (args.length == 2) {
        val opt = args(0)
        val ticker = Ticker(args(1))
        if (opt != "-n") help
        else {
          browseTicker(ticker, false)
          None
        }
    } else help
  }
}

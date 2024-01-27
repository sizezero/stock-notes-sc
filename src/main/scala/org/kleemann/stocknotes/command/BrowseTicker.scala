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

  private def browseTicker(ticker: Ticker, edit: Boolean): Option[String] = {
  
    val yahooUrls: List[String] = 
      List(("pr",""), ("is","\\&annual"), ("is",""), ("h",""), ("bc",""), ("ae",""))
      .map{ case (type1, type2) =>
        f"http://finance.yahoo.com/q/${type1}?s=${ticker.ticker}${type2}"
      }

    val morningstarUrls: List[String] =
      List(f"http://www.morningstar.com/content/morningstarcom/en_us/stocks/xnas/${ticker.ticker}/quote.html")

    val config = Config.load()
    val stocks = Stock.load(config).map{ s => (s.ticker -> s) }.toMap
    val cik: Option[String] = stocks.get(ticker).flatMap{ _.cid }
    val secUrls: List[String] = cik match {
      case Some(cik) => List(f"'http://sec.gov/cgi-bin/browse-edgar?company=&CIK=${cik}&filenum=&State=&SIC=&owner=include&action=getcompany'")
      case None => List(
        f"http://sec.gov/edgar/searchedgar/companysearch.html",
        f"'http://sec.gov/cgi-bin/browse-edgar?company=&CIK=${ticker.ticker}&filenum=&State=&SIC=&owner=include&action=getcompany'",
        config.noCikUrl.toString
      )
    }

    val urls = yahooUrls ++ morningstarUrls ++ secUrls

    //println(urls.mkString("\n"))
    val cmd = Shellable("open_as_chrome_tabs" :: urls)
    println(f"running command: ${cmd.toString}")
    os.proc(cmd).call(check=false)

    if (edit) {
      val editor = os.root / "usr" / "bin" / "gedit"
      val logFile = config.logDir / (ticker.ticker.toLowerCase()+".txt")
      println(f"running command: ${editor.toString} ${logFile.toString}")
      os.proc(editor, logFile.toString).call(check=false)
    }

    None
  }

  val help = Some(s"""
    |stock-notes browse-ticker [-n] <ticker>
    |    -n: do not open an editor
  """.stripMargin)

  override def command(args: IndexedSeq[String]): Option[String] = {
    if (args.length == 1) browseTicker(Ticker(args(0)), true)
    else if (args.length == 2) {
        val opt = args(0)
        val ticker = Ticker(args(1))
        if (opt != "-n") help
        else browseTicker(ticker, false)
    } else help
  }
}

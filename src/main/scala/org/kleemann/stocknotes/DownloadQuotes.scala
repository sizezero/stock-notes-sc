package org.kleemann.stocknotes

object DownloadQuotes extends Command {

  val help = Some(s"""
    |stock-notes download-quotes [ notify | buysell ]
    |  the default value is notify
  """.stripMargin)

  def command(args: IndexedSeq[String]): Option[String] =
    if (args.length==0) downloadQuotesNotify()
    else if (args.length==1) {
      val a = args(0)
      if (a=="notify") downloadQuotesNotify()
      else if (a=="buysell") downloadQuotesBuySell()
      else help
    } else help

  def downloadQuotesNotify(): Option[String] = {
    None
  }

  def downloadQuotesBuySell(): Option[String] = {
    None
  }
  
}

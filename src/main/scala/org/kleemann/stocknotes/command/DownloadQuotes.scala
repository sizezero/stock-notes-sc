package org.kleemann.stocknotes.command

object DownloadQuotes extends Command {

  val help = Some(s"""
    |stock-notes download-quotes
  """.stripMargin)

  override def command(args: IndexedSeq[String]): Option[String] =
    if (args.length==0) {
      downloadQuotes()
      None
    }
    else help

  private def downloadQuotes(): Unit = {
  }
}

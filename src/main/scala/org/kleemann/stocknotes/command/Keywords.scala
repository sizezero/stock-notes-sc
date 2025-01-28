package org.kleemann.stocknotes.command

import org.kleemann.stocknotes.{Config}
import org.kleemann.stocknotes.stock.{Stock}

object Keywords extends Command {

  val help = Some(s"""
   |stock-notes keywords
   |  displays all keywords used in all stock files
   """.stripMargin)

  private def allKeywords(stocks: List[Stock]): String = {
    val allKeywords: Set[String] = stocks.foldLeft(Set[String]()){ (acc, stock) => { acc ++ stock.keywords }}
    allKeywords.toList.sorted.mkString("\n")
  }

  override def command(args: IndexedSeq[String]): Option[String] =
    if (args.length==0) {
      val config = Config.load()
      val stocks = Stock.load(config)
      println(allKeywords(stocks))
      None
    }
    else help

}

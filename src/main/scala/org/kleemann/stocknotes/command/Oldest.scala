package org.kleemann.stocknotes.command

import org.kleemann.stocknotes.{Config}
import org.kleemann.stocknotes.stock.{Date, Stock}

object Oldest extends Command {

  val help = s"""oldest [-r] [-t] [-k <keyword>]
  |  -r reverse the order
  |  -t display ticker only
  |  -k restrict the list to the specified keyword""".stripMargin

  private[stocknotes] case class ParseArgs(reverse: Boolean, tickerOnly: Boolean, keyword: Option[String])

  private[stocknotes] def parse(args: IndexedSeq[String]): Either[String,ParseArgs] = {
    // iterative seems easier in this case since one argument needs to peek at the next argument
    var a = scala.collection.mutable.ArrayBuffer[String]()
    a.appendAll(args)
    var r: Boolean = false
    var t: Boolean = false
    var k: Option[String] = Option.empty[String]
    while (!a.isEmpty) {
      val token = a.remove(0)
      token.match {
        case "-r" => r=true
        case "-t" => t=true
        case "-k" => {
          if (a.isEmpty) return Left(s"""error: -k keyword argument not specified\n$help""")
          else {
            k = Some(a(0))
            a.remove(0)
          }
        }
        case opt: String => return Left(s"""error: unknown option: $opt\n$help""")
      }
    }
    Right(ParseArgs(r,t,k))
  }

  private def display(pa: ParseArgs): Unit = {
    // both config and stock loading blow us out with a sys.exit(1) not sure if that's what I want
    // we're not really returning anything at this point, may as well be Unit
    val config = Config.load()
    val ss1: List[Stock] = Stock.load(config)

    val ss2: List[Stock] = ss1.sortWith{ (s1,s2) => s1.latestDate < s2.latestDate }

    val ss3: List[Stock] = if (pa.reverse) ss2.reverse else ss2

    val ss4: List[Stock] =
      if (pa.keyword.isDefined) {
        ss3.filter{ _.keywords contains pa.keyword.get }
      } else ss3

    if (pa.tickerOnly)
      ss4.foreach{ s =>
        println(s.ticker)
      }
    else
      // TODO: I'm not sure if it's possible for a Stock to have no entries but doing this anyway
      val noDate = "NODATE".padTo(Date.earliest.toStringEnglishFixedWidth().length() , " ")
      ss4.foreach{ s => {
        val d = if (s.entries.length > 0) s.entries.last.date.toStringEnglishFixedWidth() else "NODATE"
        val n = s.name.getOrElse("NONAME")
        println(s"$d ${s.ticker} $n")
      }}

    None
  }

  def command(args: IndexedSeq[String]): Option[String] = {
    parse(args) match {
      case Right(parseArgs) => {
        display(parseArgs)
        None
      }
      case Left(error)      => Option(error)
    }
  }
}

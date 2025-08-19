package org.kleemann.stocknotes.command

import org.kleemann.stocknotes.{Config, Date, Ticker}
import org.kleemann.stocknotes.stock.{Stock}
import org.kleemann.stocknotes.current.StockReport

object Historical extends Command {

  val help = s"""historical ( all | <year> | <year1>:<year2> )
                |    ${Date.earliest.year} <= <year> <= ${Date.latest.year}
                |""".stripMargin

  private[command] def parse(args: IndexedSeq[String]): Either[String, (Date, Date)] = {
    // TODO: figure out why these can't be dropped in place
    val yearPattern = """^(\d{4})$""".r
    val twoYearPattern = """^(\d{4}):(\d{4})$""".r

    def unwrapDates(od1: Option[Date], od2: Option[Date]): Either[String, (Date, Date)] =
      if (od1.isEmpty || od2.isEmpty) Left(help)
      else                            Right((od1.get, od2.get))

    if (args.length != 1) Left(help)
    else {
      val arg: String = args(0)
      if (arg=="all") Right((Date.earliest, Date.latest))
      else arg.match {
        case yearPattern(y)        => unwrapDates(Date.earliest( y.toInt), Date.latest( y.toInt))
        case twoYearPattern(y1,y2) => unwrapDates(Date.earliest(y1.toInt), Date.latest(y2.toInt))
        case _                     => Left(help)
      }
    }
  }

  /**
   * The non-functional part of the code.
   */
  private def historical(start: Date, end: Date): Unit = {
    val config = Config.load()
    val ss: List[Stock] = Stock.load(config)

    val srs: List[StockReport] = StockReport.createHistorical(start, end, ss)
    print(StockReport.render(srs))
  }

  override def command(args: IndexedSeq[String]): Option[String] = {
    parse(args) match {
      case Right((start, end)) => {
        historical(start, end)
        None
      }
      case Left(error) => Some(error)
    }
  }
}

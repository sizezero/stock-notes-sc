package org.kleemann.stocknotes.command

import org.kleemann.stocknotes.current.{Gain => ReportGain}
import org.kleemann.stocknotes.{Config, Date, Ticker}
import org.kleemann.stocknotes.stock.{Stock}

object Historical extends Command {

  val help = s"""gain <year>[:<year>]
  |""".stripMargin

  private[command] def parse(args: IndexedSeq[String]): Either[String, (Date, Date)] = {
    // TODO: figure out why these can't be dropped in place
    val yearPattern = """^(\d{4})$""".r
    val twoYearPattern = """^(\d{4}):(\d{4})$""".r

    if (args.length != 1) Left(help)
    else {
      val arg: String = args(0)
      if (args(0)==":") Right((Date.earliest, Date.latest))
      else arg.match {
        case yearPattern(y)        => Right((Date.earliest( y.toInt).get, Date.latest( y.toInt).get))
        case twoYearPattern(y1,y2) => Right((Date.earliest(y1.toInt).get, Date.latest(y2.toInt).get))
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

    val srs: List[ReportGain.StockReport] = ReportGain.createHistorical(start, end, ss)
    print(ReportGain.render(srs))
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

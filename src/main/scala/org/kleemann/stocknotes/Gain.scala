package org.kleemann.stocknotes

object Gain extends Command {

  val help = s"""gain [-omit <ticker> ] (<year>[:<year>] | <commission>) [ <ticker> <ticker> ... ]
  |""".stripMargin

  def command(args: IndexedSeq[String]): Option[String] = {
    parse(args) match {
      case Right(parseArgs) => gain(parseArgs)
      case Left(error)      => Option(error)
    }
  }
  
  private[stocknotes] case class ParseArgs(start: Date, end: Date, commission: Double, tickers: List[Ticker], omitKeyword: Option[String])

  /** 
    * 
    *
    * @param args
    * @return
    */
  private[stocknotes] def parse(args: IndexedSeq[String]): Either[String, ParseArgs] = {

    val (args2: IndexedSeq[String], omitKeyword: Option[String]) = 
      if (args.length >= 2 && args(0)=="-omit") (args.drop(2), Some(args(1)))
      else (args, None)

    // default sell with a commision of 30
    val args3: IndexedSeq[String] =
      if (args2.length==0) Vector("30")
      else args2

    // TODO: figure out why these can't be dropped in place
    val yearPattern = """^(\d{4})$""".r
    val twoYearPattern = """^(\d{4}):(\d{4})$""".r
    val commissionPattern = """^\d+(\.\d+)?$""".r

    // args3 is now guaranteed to have at least one element
    val (start: Date, end: Date, commission: Double) =
      if (args3(0)==":") (Date(1900,1,1).get, Date(2100,1,1).get, -100.0)
      else args3(0).match {
        case yearPattern(y)        => (Date(y.toInt,1,1).get, Date(y.toInt,1,1).get, -100.0)
        case twoYearPattern(y1,y2) => (Date(y1.toInt,1,1).get, Date(y2.toInt,1,1).get, -100.0)
        case commissionPattern(d)  => (Date(1900,1,1).get, Date(1900,1,1).get, args3(0).toDouble)
        case _                     => return Left(help) //this looks non-functional
      }

    val args4 = args3.drop(1)

    val tickers: List[Ticker] = args4.map{ Ticker(_) }.sorted.toList

    Right(ParseArgs(start, end, commission, tickers, omitKeyword))
  }

  /** For now we are just using the same strange arguments as the original python code.
    * I should be able to make this better in the future.
    * 
    * Does this command have the side effect of printing? Right now our return value consists of only output text in the error case.
    * Maybe we should use Either to enable output in both cases.
    *
    * @param pa
    * @return
    */
  private def gain(pa: ParseArgs): Option[String] = {
    // both config and stock loading blow us out with a sys.exit(1) not sure if that's what I want
    // we're not really returning anything at this point, may as well be Unit
    val ss: List[Stock] = Stock.load(Config.load())
    println("all the stocks loaded!")
    None
  }
}

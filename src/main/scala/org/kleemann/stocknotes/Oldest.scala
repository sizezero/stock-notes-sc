package org.kleemann.stocknotes

object Oldest extends Command {

  val help = s"""oldest [-r] [-t] [-k <keyword>]
  |  -r reverse the order
  |  -t display ticker only
  |  -k restrict the list to the specified keyword""".stripMargin

  def command(args: IndexedSeq[String]): Option[String] = {
    parse(args) match {
      case Right(parseArgs) => display(parseArgs)
      case Left(error)      => Option(error)
    }
  }

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

  def display(args: ParseArgs): Option[String] = {
    None
  }
}

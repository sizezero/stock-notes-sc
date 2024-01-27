import org.kleemann.stocknotes.*
import org.kleemann.stocknotes.command.*

/** a functional main that is easier to test
 *  
 * @param args all command line arguments
 * @return on failure, display the following text and exit
 */
def funcMain(args: Seq[String]): Option[String] = {
  val help = Some(s"""
                |stocknotes <command> [ <args> ... ]
                |    where <command> is one of: ${Command.all.keys.mkString(" ")}
  """.stripMargin)

  if (args.isEmpty) help
  else {
    val commandText = args.head
    val rest        = args.tail
    Command.all.get(commandText) match {
      case Some(c) => c.command(rest.toVector)
      case None => help
    }
  }
}


/** A non functional main with all the IO and side effects
  * 
  * @param args all command line args
  */
@main def main(args: String*): Unit = {
  
  // make sure the config file exists and load it
  val config = Config.load()

  funcMain(args) match {
    case Some(out) => {
      println(out)
      sys.exit(1)
    }
    case None => sys.exit(0)
  }
}

package org.kleemann.stocknotes

import scala.collection.mutable
import scala.annotation.tailrec

/**
  * Provides a map of Tickers to stock quotes in Currency.
  * Internet quotes are downloaded explicitly and cached to a CSV file in our config dir.
  */
object Quote {

    /**
      * Download the specified tickers and overwrite the quotes file.
      * 
      * This implementation keeps the format of the quote file internal to this class
      * but leaves the service call external.
      *
      * @param tickers the list of tickers we want quotes for
      * @param config the config file
      * @param downloadSingleQuote downloads a ticker from a service and either return the price or an error
      * @return The multi-line text that is the content of a CSV file
      */
    def save(tickers: List[Ticker], config: Config, downloadSingleQuote: Ticker => Either[String, Currency]): Unit = {

        val content: String = tickers.map{ t =>
            // call the webservice to get a single quote
            downloadSingleQuote(t) match {
                // format the result to a CSV line
                case Left(error)  => f"${t.name},0.0,${error}\n"
                case Right(price) => f"${t.name},${price.toStringBare},\n" 
            }
        }.foldLeft(mutable.StringBuilder()){ _ ++= _ }.toString

        // TODO: I can't find good docs on os-lib so I'm just going to collect the quotes in memory and write the file all at once.
        os.write.over(config.quotesFile, content)
    }

    /**
      * Reads the quote file. Blows out with an IOException or sys.exit if there is any problem.
      *
      * This is the dirty, side effect version of the function.
      * 
      * @param f
      * @return
      */
    def load(config: Config): Map[Ticker, Currency] =
        load(os.read.lines.stream(config.quotesFile), config.quotesFile.toString) match {
          case Left(error) => {
            println(error)
            sys.exit(1)
          }
          case Right(quotes) => quotes
        }

    /** A functional, testable version of the load command.
      * 
      * If the entry fails to parse for any reason, it is silently omitted from the returned list.
      * 
      * @param g the input source to parse
      * @param filename the name of the source file for error reporting
      * @return a map of quotes on success, an error string (with no trailing newline) on error
      */
    private[stocknotes] def load(g: os.Generator[String], filename: String): Either[String, Map[Ticker, Currency]] = {
      @tailrec
      def loop(in: Seq[String], errors: String, quotes: List[(Ticker, Currency)]): Either[String, Map[Ticker, Currency]] =
        if (in.isEmpty) {
          if (errors.isEmpty) Right(quotes.toMap)
          else                Left(errors)
        } else
          parseCsvLine(in.head) match {
            case Left(error) => loop(in.tail, s"${errors}\n${error}", quotes)
            case Right(tup)  => loop(in.tail, errors, tup :: quotes)
          }
      loop(g.toSeq, "", Nil)
    }

    private val datePattern = """^(\d{2})/(\d{2})/(\d{4})$""".r

    /**
      * Parses a line of our dowloaded quote CSV file.
      * 
      * The format of the lines are:
      * AAPL,185.92,optional error
      *
      * @param line
      * @return
      */
    private[stocknotes] def parseCsvLine(line: String): Either[String, (Ticker, Currency)] = {
        val a = line.split(",", 3)
        if (a.length != 3) Left(s"line does not have three elements: ${line}")
        else {
            val ticker = Ticker(a(0))
            Currency.parse(a(1)) match {
                case None => Left(s"can't parse currency: ${a(1)}") // price string can't be parsed
                case Some(price) => Right((ticker, price))
            }
        }
    }

}

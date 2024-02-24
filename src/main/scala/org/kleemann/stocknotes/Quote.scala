package org.kleemann.stocknotes

import scala.collection.mutable

/**
  * Represets a quote downloaded from some authoritative source on the internet.
  *
  * @param price The price of the stock around the time it was downloaded
  * @param date  The the day the quote was from
  */
final case class Quote(price: Currency, date: Date)

object Quote {

    /**
      * Download the specified tickers and write the quotes file.
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

        // Our python quotes file format uses this legacy date format.
        // When that's retired we can change it.
        val today: String = Date.today match {
            case Date(year, month, day) => f"$month%02d/$day%02d/$year%04d"
        }

        val content: String = tickers.map{ t =>
            // call the webservice to get a single quote
            downloadSingleQuote(t) match {
                // format the result to a CSV line
                case Left(e)  => f"${t.ticker},0.0,${today},${e}\n"
                case Right(curr) => f"${t.ticker},${curr.toStringBare},${today},\n" 
            }
        }.foldLeft(mutable.StringBuilder()){ _ ++= _ }.toString

        // TODO: I can't find good docs on os-lib so I'm just going to collect the quotes in memory and write the file all at once.
        os.write.over(config.quotesFile, content)
    }

    def load(config: Config): Map[Ticker, Quote] =
        if (os.exists(config.quotesFile)) load(config.quotesFile)
        else {
            val m1 = if (os.exists(config.notifyQuotesFile))  load(config.notifyQuotesFile)  else Map()
            val m2 = if (os.exists(config.buySellQuotesFile)) load(config.buySellQuotesFile) else Map()
            // combine the two quote maps with just the newest quotes
            merge(m1, m2, (q1,q2) => if (q1.date > q2.date) q1 else q2 )
        }

    /**
      * Reads the quote file. Blows out with an IOException if there is any problem.
      *
      * This is the dirty, side effect version of the function.
      * 
      * @param f
      * @return
      */
    private def load(f: os.Path): Map[Ticker, Quote] = load(os.read.lines.stream(f))

    /** A functional, testable version of the load command.
      * 
      * If the entry fails to parse for any reason, it is silently omitted from the returned list.
      * 
      * @param g
      * @return
      */
    private[stocknotes] def load(g: os.Generator[String]): Map[Ticker, Quote] =
        g.flatMap{ parseCsvLine(_) }.toSeq.toMap

    private val datePattern = """^(\d{2})/(\d{2})/(\d{4})$""".r

    /**
      * Parses a line of our dowloaded quote CSV file.
      * 
      * The format of the lines are:
      * AAPL,185.92,01/12/2024,optional error
      *
      * @param line
      * @return
      */
    private[stocknotes] def parseCsvLine(line: String): Option[(Ticker, Quote)] = {
        val a = line.split(",", 4)
        if (a.length == 4) {
            val ticker = Ticker(a(0))
            Currency.parse(a(1)) match {
                case Some(price) => {
                    val dateText = a(2)
                    val errorText = a(3)
                    dateText match {
                        case datePattern(month,day,year) => {
                            Date(year.toInt, month.toInt, day.toInt) match {
                                case Some(d) => Some((ticker, Quote(price, d)))
                                case None => None // date integer is out of range
                            }
                        }
                        case _ => None // date string does not parse
                    }
                }
                case None => None // price string can't be parsed
            }
        } else None // length != 4
    }

    /**
      * Merges two maps. For duplicate values, a function chooses which value to take.
      * 
      * There is probably a way to do this that takes advantage of generic collections methods. I don't know if it's cleaner.
      *
      * @param m1 first map
      * @param m2 second map
      * @param func I function that, given two duplicate values, returns the one to use in the resultant Map
      * @return A new merged map.
      */
    private[stocknotes] def merge[K,V](m1: Map[K,V], m2: Map[K,V], func: (V,V) => V): Map[K,V] = {
        val keys = m1.keySet.union(m2.keySet)
        keys.map{ k =>
            if (m1.contains(k) && m2.contains(k))
                // we have a conflict so let the caller decide
                k -> func(m1(k), m2(k))
            else
                // either m1 or m2 is guaranteed to have a value for k
                k -> m1.getOrElse(k, m2(k))
        }.toMap
    }

}

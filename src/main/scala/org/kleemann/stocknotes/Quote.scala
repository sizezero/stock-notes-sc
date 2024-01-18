package org.kleemann.stocknotes

/**
  * Represets a quote downloaded from some authoritative source on the internet.
  *
  * @param price The price of the stock around the time it was downloaded
  * @param date  The the day the quote was from
  */
final case class Quote(price: Currency, date: Date)

object Quote {

    def load(config: Config): Map[Ticker, Quote] =
        if (os.exists(config.quotesFile)) load(config.quotesFile)
        else {
            val m1 = if (os.exists(config.notifyQuotesFile)) load(config.notifyQuotesFile) else Map()
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
      * If the entry fails to parse for any reason, it is omitted from the returned list.
      * 
      * @param g
      * @return
      */
    private[kleemann] def load(g: os.Generator[String]): Map[Ticker, Quote] =
        g.flatMap{ line => {
            parseCsvLine(line) match {
                case None => None
                case some => some // guaranteed to be Some[(Ticker,Quote)]
            }
        }}.toSeq.toMap

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
    private[kleemann] def parseCsvLine(line: String): Option[(Ticker, Quote)] = {
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
      * A non functional method that connects to the website and downloads the quotes
      *
      * Right now we have two sets of tickers, one for notify (gain) and one for buysell (the web page).
      * It should make sense to combine these into a single list. I don't yet have the info of the list of tickers.
      * 
      * @param config
      */
    def download(config: Config): Unit = ???

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
    private[kleemann] def merge[K,V](m1: Map[K,V], m2: Map[K,V], func: (V,V) => V): Map[K,V] = {
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

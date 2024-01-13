package org.kleemann.stocknotes

/**
  * Represets a quote downloaded from some authoritative source on the internet.
  *
  * @param price The price of the stock around the time it was downloaded
  * @param date  The the day the quote was from
  */
final case class Quote(price: Double, date: Date)

object Quote {

    def load(config: Config): Map[String, Quote] =
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
      * I wonder if it would make sense for this to return Option[Quote] instead of Quote?
      * 
      * @param f
      * @return
      */
    private[kleemann] def load(f: os.Path): Map[String, Quote] = {
        var i = 1
        var out: List[(String,Quote)] = List()
        val q = Quote(0.0, Date(1900,1,1).get)
        os.read.lines.stream(f).foreach{ line => {
            println(line)
            val a = line.split(",", 4)
            if (a.length >= 4) {
                val ticker = a(0)
                val price = a(1).toDoubleOption.getOrElse(0.0)
                val dateText = a(2)
                val errorText = a(3)
                out = (ticker, Quote(price.toDouble, parseCsvDate(dateText).get)) :: out
            }
            i += 1
        }}
        out.toMap
    }

    private val datePattern = """^(\d{2})/(\d{2})/(\d{4})$""".r
    private def parseCsvDate(s: String): Option[Date] = {
        s match {
            case datePattern(month,day,year) => Date(year.toInt, month.toInt, day.toInt)
            case _ => Date(1900,1,1)
        }
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
      * There is probably a much more functional way to do this. I don't know if it's cleaner.
      *
      * @param m1 first map
      * @param m2 second map
      * @param func I function that, given two duplicate values, returns the one to use in the resultant Map
      * @return A new merged map.
      */
    private def merge[K,V](m1: Map[K,V], m2: Map[K,V], func: (V,V) => V): Map[K,V] = {
        val keys = m1.keySet.union(m2.keySet)
        keys.flatMap{ k =>
            m1.get(k) match {
                case None => {
                    m2.get(k) match {
                         // neight map has the key, this should never happen
                        case None => None
                        // m1 does not have the key but m2 does, take the m2 value
                        case Some(v2) => Some(k -> v2)
                    }
                }
                case Some(v1) => {
                    m2.get(k) match {
                        // m1 has the key but m2 doesn't, take the m1 value
                        case None => Some(k -> v1)
                        // both m1 and m2 have a value, ask the caller to decide
                        case Some(v2) => Some(k -> func(v1, v2))
                    }
                }
            }
        }.toMap
    }

}

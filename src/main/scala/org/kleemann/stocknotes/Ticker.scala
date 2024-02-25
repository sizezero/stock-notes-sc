package org.kleemann.stocknotes

/**
  * A proper class for our stock tickers E.g.: AAPL, MSFT, GOOG
  * 
  * The official source of this is either what Yahoo calls them (for browse ticker)
  * or what finnhubb calls them to download quotes. I'm not sure what to do if there is a confict.
  *
  * @param name the upper case string that represents the stock ticker
  */
final case class Ticker private(name: String) extends Ordered[Ticker] {
    override def compare(that: Ticker): Int = name.compare(that.name)

    override def toString(): String = name
}

object Ticker {
    def apply(s: String): Ticker = new Ticker(s.toUpperCase())
}

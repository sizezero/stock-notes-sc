package org.kleemann.stocknotes

/**
  * A proper class for our stock tickers E.g.: AAPL, MSFT, GOOG
  * 
  * The official source of this is either what Yahoo calls them (for browse ticker)
  * or what finnhubb calls them to download quotes. I'm not sure what to do if there is a confict.
  *
  * @param ticker
  */
final case class Ticker private(ticker: String) extends Ordered[Ticker] {
    override def compare(that: Ticker): Int = ticker.compare(that.ticker)

    override def toString(): String = ticker
}

object Ticker {
    def apply(s: String): Ticker = new Ticker(s.toUpperCase())
}

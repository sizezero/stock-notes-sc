package org.kleemann.stocknotes.stock

import org.kleemann.stocknotes.Ticker

/**
  * Stocks are written about in a single file with a date separating different sections.
  * An entry represents one dated section.
  * 
  * The content of an entry is an order list of text, Trades, and Watches interspersed in any order.
  *
  * @param ticker
  * @param date
  * @param content
  */
final case class Entry(ticker: Ticker, date: Date, content: List[String | Trade | Watch]) extends Ordered[Entry] {

      override def compare(that: Entry): Int = 
        if (this.ticker != that.ticker) ticker.ticker.compare(that.ticker.ticker)
        else if (this.date != that.date) date.compare(that.date)
        else 0

}

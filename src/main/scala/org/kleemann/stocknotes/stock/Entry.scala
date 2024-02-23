package org.kleemann.stocknotes.stock

import scala.collection.mutable

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
final case class Entry private(ticker: Ticker, date: Date, content: List[String | Trade | Watch]) extends Ordered[Entry] {

      override def compare(that: Entry): Int = 
        if (this.ticker != that.ticker) ticker.ticker.compare(that.ticker.ticker)
        else if (this.date != that.date) date.compare(that.date)
        else 0

}

object Entry {

  def apply(ticker: Ticker, date: Date, content: List[String | Trade | Watch]): Entry =
    new Entry(ticker, date, coalesce(content))

  /**
    * Combine adjacent strings into a single element.
    * 
    * @param content a list of content that may have repeated Strings
    * @return Each String in the list will have a non-String immediately before and after it.
    */
  private def coalesce(content: List[String | Trade | Watch]): List[String | Trade | Watch] = {
    // The external, mutable StringBuilder is not functional but it is contained to this small block of code.
    val sb = mutable.StringBuilder()
    // This implementation relies on List.flatMap() traversing each element of the list in order.
    // I'm not sure if the specification guarantees this even though the current implementation
    // does traverse the list in order.
    val newContent1: List[String | Trade | Watch] = content.flatMap{ c => c match {
      case s: String => {
        sb.append(s)
        List()
      }
      case other: (Trade | Watch) => {
        if (sb.isEmpty) List(other)
        else {
          val combinedString = sb.result()
          sb.clear()
          List(combinedString, other)
        }
      }
    }}
    if (sb.isEmpty) newContent1 else newContent1 :+ sb.toString()
  }

}

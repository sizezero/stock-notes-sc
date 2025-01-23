package org.kleemann.stocknotes.current

import _root_.org.kleemann.stocknotes.{Currency, Date, Fraction, Quote, Ticker}
import _root_.org.kleemann.stocknotes.stock.{Shares, Stock}
import _root_.org.kleemann.stocknotes.stock.{Trade, Buy, Sell, Split}
import _root_.org.kleemann.stocknotes.current.{StockReport, MatchedSell, MatchedBuy, BuyReadyToSell}

class TestMatchedBuy extends munit.FunSuite {

  test("annualYield") {
    // one dollar to two dollars in a year, 100% annual
    val d1 = MatchedBuy.annualYield(Currency(1, 0), Currency(2, 0), 1.0)
    assert(clue(d1) > 0.9999)
    assert(clue(d1) < 1.0001)
    // four times in two years should be 100% annual
    val d2 = MatchedBuy.annualYield(Currency(1, 0), Currency(4, 0), 2.0)
    assert(clue(d2) > 0.9999)
    assert(clue(d2) < 1.0001)
  }

}
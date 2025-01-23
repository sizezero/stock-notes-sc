package org.kleemann.stocknotes.current

import _root_.org.kleemann.stocknotes.{Currency, Date, Fraction}
import _root_.org.kleemann.stocknotes.stock.{Shares}
import _root_.org.kleemann.stocknotes.stock.{Trade, Buy, Sell, Split}

class TestMatchedSell extends munit.FunSuite {

  test("sell2MatchedSell") {
    // test all three cases of sell equals buy, and each exceeds

    // def parseMatchedSell(sell: Sell, buys: Vector[BuyReadyToSell]): (MatchedSell, Vector[BuyReadyToSell]) = {
    // date: Date, shares: Shares, price: Currency, commission: Currency
    val com = Currency.zero // all commissions are zero to make things easy
    val b1 = Buy(Date(2015,1,1).get, Shares(2,Fraction.one), Currency(2, 0), com) // cost $4
    val b2 = Buy(Date(2016,1,1).get, Shares(2,Fraction.one), Currency(3, 0), com) // cost $6
    val m = Fraction(2,1) // the new multiple
    val sp1 = Split(Date(2017,1,1).get, m)
    val b3 = Buy(Date(2018,2,1).get, Shares(2,m), Currency(2, 0), com) // shares 1, price 4 pre-split, cost $4
    // shares 5, price 6 pre split. This should empty out the bought shares, gross $30, cost $14 net $16
    val s1 = Sell(Date(2019,1,1).get, Shares(10,m), Currency(3,0), com)
    val b4 = Buy(Date(2020,1,1).get, Shares(2,m), Currency(4, 0), com) // shares 1, price 8 pre split

    // TODO: right now we blow up if we try to sell something for which there are no buys. I think
    // to do things right, we should bubble up the failed sell. Ugh. Maybe that is caught in parsing 
    // and we don't have to worry? It could just be a test thing

    val trades = List[Trade](b1, b2, sp1, b3, s1, b4)

    val brss: Vector[BuyReadyToSell] = trades.flatMap{ t => t match {
      case b: Buy => List(BuyReadyToSell(b))
      case _ => Nil
    }}.toVector

    val (ms, brss2) = MatchedSell.sell2MatchedSell(s1, brss)

    // I didn't test the annual yield, just copied it from the output
    val ms2 = List[MatchedBuy](
      MatchedBuy(b1, Shares(4,m), b1.price.priceMultipleAdjust(Fraction.one,m), Currency(4, 0), com, com, true, 0.3160740129524924),
      MatchedBuy(b2, Shares(4,m), b2.price.priceMultipleAdjust(Fraction.one,m), Currency(6, 0), com, com, true, 0.2599210498948732),
      MatchedBuy(b3, Shares(2,m), b3.price.priceMultipleAdjust(m,m),            Currency(4, 0), com, com, false, 0.5575251156760133)
    )

    assertEquals(ms, MatchedSell(s1, Currency(16, 0), Currency(16, 0), ms2))

    val brss3 = Vector[BuyReadyToSell](
      BuyReadyToSell(b4, Shares(2,m).atMult(Fraction.one))  // none of the shares were sold from this, still at two
    )

    assertEquals(brss2, brss3)
  }

}
package org.kleemann.stocknotes.report

import _root_.org.kleemann.stocknotes.{Currency, Date, Fraction, Quote, Ticker}
import _root_.org.kleemann.stocknotes.stock.{Shares, Stock}
import _root_.org.kleemann.stocknotes.stock.{Trade, Buy, Sell, Split}

class TestGain extends munit.FunSuite {

  test("calc") {
    // two companies, test both sell immediate and sell in years range

    val g1: os.Generator[String] = os.Generator.from(
        """
        |Jan 1, 1990
        |TRADE buy 10@$5.00 balance 10 commission 9.99
        |Jan 1, 1991
        |TRADE buy 5@$6.00 balance 15 commission 9.99
        |Jan 1, 1992
        |TRADE split 2:1 balance 30
        |Jan 1, 1993
        |TRADE buy 10@$4.00 balance 40 commission 9.99
        |Jan 1, 1994
        |TRADE sell 40@4 balance 0 commission 0
        |Jan 1, 1995
        |TRADE buy 20@3 balance 20 commission 0
        |Jan 1, 1996
        |TRADE sell 10@7 balance 10 commission 0
        |""".stripMargin.split("\n")
    )
    val t1 = Ticker("MSFT")
    val e1 = Stock.load(t1, "filename", g1)
    assert(e1.isRight)
    val stock1: Stock = e1.right.get

    val g2: os.Generator[String] = os.Generator.from(
        """
        |Jan 1, 1990
        |TRADE buy 10@$5.00 balance 10 commission 9.99
        |Jan 1, 1991
        |TRADE buy 5@$6.00 balance 15 commission 9.99
        |Jan 1, 1992
        |TRADE split 2:1 balance 30
        |Jan 1, 1993
        |TRADE buy 10@$4.00 balance 40 commission 9.99
        |Jan 1, 1994
        |TRADE sell 40@4 balance 0 commission 0
        |Jan 1, 1995
        |TRADE buy 20@3 balance 20 commission 0
        |Jan 1, 1996
        |TRADE sell 10@7 balance 10 commission 0
        |""".stripMargin.split("\n")
    )
    val t2 = Ticker("AAPL")
    val e2 = Stock.load(t2, "filename", g2)
    assert(e2.isRight)
    val stock2: Stock = e2.right.get

    val today = Date(1997, 1, 1).get
    val commission = Currency(30, 0)
    val stocks = List(stock1, stock2)
    val quotes = Map( t1 -> Quote(Currency(8,0)), t2 ->  Quote(Currency(9,0)))

    val srs = Gain.createCurrent(None, stocks, List(), quotes, commission, today)

    assertEquals(srs.length, 2)
  }

  test("gain aapl") {
    // I'm running into problems testing the appl numbers, might as well make a test case out of it
    val g: os.Generator[String] = os.Generator.from(
        """
        |Jan 28, 2013
        |TRADE buy 22@445.00 balance 22 commission 9.99
        |Apr 18, 2013
        |TRADE buy 25@399.00 balance 47 commission 9.99
        |Jun 24, 2013
        |TRADE buy 25@395.00 balance 72 commission 9.99
        |Jun 9, 2014
        |TRADE split 7:1 balance 504
        |Aug 29, 2014
        |TRADE sell 252@102.55 balance 252 commission 9.99
        |Nov 20, 2014
        |TRADE sell 252@116.40 balance 0 commission 9.99
        |""".stripMargin.split("\n"))

    val e = Stock.load(Ticker("AAPL"), "filename", g)
    assert(e.isRight)
    val stock: Stock = e.right.get

    val com = Currency(9, 99)
    val m1 = Fraction.one
    val b1 = Buy(Date(2013,1,28).get, Shares(22,m1), Currency(445,0), com) // 154 shares post split
    val b2 = Buy(Date(2013,4,18).get, Shares(25,m1), Currency(399,0), com) // 175 shares post split
    val b3 = Buy(Date(2013,6,24).get, Shares(25,m1), Currency(395,0), com) // 175 shares post split
    val m2 = Fraction(7,1)
    val sp1 = Split(Date(2014,6,9).get, m2)
    val s1 = Sell(Date(2014, 8,29).get, Shares(252,m2), Currency(102, 55), com)
    val s2 = Sell(Date(2014,11,20).get, Shares(252,m2), Currency(116, 40), com)
    val ts = List[Trade](b1, b2, b3, sp1, s1, s2)

    // TODO: just copying annualYield from output for now

    // attempting to sell 252 shares at 102
    // sell all 154 shares of b1, proportion values are 100%
    val mb1 = Gain.MatchedBuy(
      b1, 
      Shares(154,m2),
      b1.price.priceMultipleAdjust(m1,m2),
      Currency(22*445, 0), 
      com, // full buy commision
      Currency.fromDouble(com.toDouble*(154.0/252)),
      true, 
      0.3513183447677446)
    // sell needs 98 more shares, less that b2 has
    // 98 of these are sold, 77 remain, cost and commission are proportional
    val mb2 = Gain.MatchedBuy(
      b2, 
      Shares(98,m2), 
      b2.price.priceMultipleAdjust(m1,m2),
      Currency(14*399, 0), 
      Currency.fromDouble(com.toDouble*(98.0/175)), 
      Currency.fromDouble(com.toDouble*(98.0/252)), 
      true, 
      0.5363864184585441)
    val net1 = s1.gross - (mb1.proportionalCost + mb2.proportionalCost)
    val capGains1 = net1 - com - mb1.proportionalBuyCommission - mb2.proportionalBuyCommission
    val ms1 = Gain.MatchedSell(s1, net1.truncate, capGains1.truncate, List(mb1,mb2))

    // sell remaining 77 shares from b2
    val mb3 = Gain.MatchedBuy(
      b2, 
      Shares(77,m2), 
      b2.price.priceMultipleAdjust(m1,m2),
      Currency(11*399, 0), 
      Currency.fromDouble(com.toDouble*(77.0/175)), 
      Currency.fromDouble(com.toDouble*(77.0/252)), 
      true, 
      0.5647085818990794)
    // all 175 shares of b3 are sold
    val mb4 = Gain.MatchedBuy(
      b3, 
      Shares(175,m2), 
      b3.price.priceMultipleAdjust(m1,m2),
      Currency(25*395, 0), // original shares and price
      com, // full buy commission
      Currency.fromDouble(com.toDouble*(175.0/252)), 
      true, 
      0.6706370504190582)
    val net2 = s2.gross - (mb3.proportionalCost + mb4.proportionalCost)
    val capGains2 = net2 - com - mb3.proportionalBuyCommission - mb4.proportionalBuyCommission
    val ms2 = Gain.MatchedSell(s2, net2.truncate, capGains2.truncate, List(mb3,mb4))

    val value = s1.gross - s1.commission + s2.gross - s1.commission
    val capGainsTotal1 = s1.gross + s2.gross
      - Currency.fromDouble(b1.shares.shares * b1.price.toDouble) // all buys in this example are at multiple 1
      - Currency.fromDouble(b2.shares.shares * b2.price.toDouble) // all buys in this example are at multiple 1
      - Currency.fromDouble(b3.shares.shares * b3.price.toDouble) // all buys in this example are at multiple 1
      - s1.commission - s2.commission
      - b1.commission - b2.commission - b3.commission
    val capGainsTotal2 = s1.gross + s2.gross
      - s1.commission - s2.commission
      - mb1.proportionalCost - mb1.proportionalBuyCommission
      - mb2.proportionalCost - mb2.proportionalBuyCommission
      - mb3.proportionalCost - mb3.proportionalBuyCommission
      - mb4.proportionalCost - mb4.proportionalBuyCommission
    // this is taken from the code
    val capGainsTotal3: Currency = List[Gain.MatchedSell](ms1,ms2).foldLeft(Currency.zero){ (acc: Currency, ms: Gain.MatchedSell) =>
        acc + ms.sell.gross - ms.sell.commission - ms.mbs.foldLeft(Currency.zero) { (acc2, mb) =>
          acc2 + mb.proportionalCost + mb.proportionalBuyCommission
        }
      }
    // There are multiple ways to check cap gains
    assertEquals(capGainsTotal1, capGainsTotal2)
    assertEquals(capGainsTotal1, capGainsTotal3)

    val stockReport = Gain.StockReport(stock, List(ms1, ms2), value, capGainsTotal1, 1.0)

    val o = Gain.parseCompanyDateRange(stock, Date(2010, 1, 1).get, Date(2015, 2, 1).get)
    o match {
      case Some(sr: Gain.StockReport) => {
        // company is a big object that doesn't display diffs well so we break it up into multiple assertions
        assertEquals(sr.stock, stockReport.stock)
        assert(sr.mss.length == 2)
        assertEquals(sr.mss.head, ms1)
        assertEquals(sr.mss.tail.head, ms2)
        assertEquals(sr.net, stockReport.net)
        assertEquals(sr.capGains, stockReport.capGains)
        assertEquals(sr.ltcgPercentage, stockReport.ltcgPercentage)
      }
      case None => assert(false)
    }
  }

  test("parseCompany") {
    // test both sell immediate and sell in years range, do a split, multiple sells

    val g: os.Generator[String] = os.Generator.from(
        """
        |Jan 1, 1990
        |TRADE buy 10@$5.00 balance 10 commission 9.99
        |Jan 1, 1991
        |TRADE buy 5@$6.00 balance 15 commission 9.99
        |Jan 1, 1992
        |TRADE split 2:1 balance 30
        |Jan 1, 1993
        |TRADE buy 10@$4.00 balance 40 commission 9.99
        |Jan 1, 1994
        |TRADE sell 30@4 balance 10 commission 0
        |Jan 1, 1995
        |TRADE buy 10@3 balance 20 commission 0
        |Jan 1, 1996
        |TRADE sell 20@7 balance 0 commission 0
        |""".stripMargin.split("\n")
    )
    val e = Stock.load(Ticker("MSFT"), "filename", g)
    assert(e.isRight)
    val stock: Stock = e.right.get

    val today = Date(1993, 12, 31).get
    val o1 = Gain.parseCompanyCurrentValue(stock, Currency(8,0), Currency.zero, today)
    o1 match {
      case Some(c: Gain.StockReport) => {
        assertEquals(c.mss.length, 1) // one sell
        val cost = Currency(10*5, 0) + Currency(5*6, 0) + Currency(10*4, 0)
        val capGains = Currency(40*8, 0) - cost - Currency(29,97)
        assertEquals(c.capGains, capGains)
      }
      case None => assert(false)
    }
    
    val o2 = Gain.parseCompanyDateRange(stock, Date(1992, 1, 1).get, Date(1997, 2, 1).get)
    o2 match {
      case Some(sr: Gain.StockReport) => assertEquals(sr.mss.length, 2) // two sells due to date range
      case None => assert(false)
    }
  }

  test("parseCompanyDateRange fractional matching") {
    // running "gain 2014 dgly" caused parseMatchSell to blow up due to fractional shares not getting correctly match
    // this required me to rewrite the code using doubles instead of ints with multiples
    // the test replicates the symptom

    val ticker = Ticker("DGLY")
    val g: os.Generator[String] = os.Generator.from(
      """
      |Sep 24, 2010
      |TRADE buy 1225@1.60 balance 1225 commission 7.00
      |Nov 11, 2010
      |TRADE buy 1875@1.60 balance 3100 commission 7.00
      |Jan 11, 2011
      |TRADE buy 2923@1.71 balance 6023 commission 7.00
      |Aug 24, 2012
      |TRADE sell 7@1.0 balance 6016 commission 0.0
      |TRADE split 1:8 balance 752
      |Aug 28, 2014
      |TRADE sell 752@17.32 balance 0  commission 7.29 
      |""".stripMargin.split("\n")
    )
    val e = Stock.load(ticker, "filename", g)
    assert(e.isRight)
    val stock = e.right.get
    assert(stock.trades.length == 6)

    val o = Gain.parseCompanyDateRange(stock, Date.earliest(2014).get, Date.latest(2014).get)
    assert(o.isDefined)
    val sr = o.get
    // we're really testing that the above didn't blow up; not sure what else to look at
  }

  test("parsedMatchedSell") {
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

    val brss: Vector[Gain.BuyReadyToSell] = trades.flatMap{ t => t match {
      case b: Buy => List(Gain.BuyReadyToSell(b))
      case _ => Nil
    }}.toVector

    val (ms, brss2) = Gain.parseMatchedSell(s1, brss)

    // I didn't test the annual yield, just copied it from the output
    val ms2 = List[Gain.MatchedBuy](
      Gain.MatchedBuy(b1, Shares(4,m), b1.price.priceMultipleAdjust(Fraction.one,m), Currency(4, 0), com, com, true, 0.3160740129524924),
      Gain.MatchedBuy(b2, Shares(4,m), b2.price.priceMultipleAdjust(Fraction.one,m), Currency(6, 0), com, com, true, 0.2599210498948732),
      Gain.MatchedBuy(b3, Shares(2,m), b3.price.priceMultipleAdjust(m,m),            Currency(4, 0), com, com, false, 0.5575251156760133)
    )

    assertEquals(ms, Gain.MatchedSell(s1, Currency(16, 0), Currency(16, 0), ms2))

    val brss3 = Vector[Gain.BuyReadyToSell](
      Gain.BuyReadyToSell(b4, Shares(2,m).atMult(Fraction.one))  // none of the shares were sold from this, still at two
    )

    assertEquals(brss2, brss3)
  }

  test("annualYield") {
    // one dollar to two dollars in a year, 100% annual
    val d1 = Gain.annualYield(Currency(1, 0), Currency(2, 0), 1.0)
    assert(clue(d1) > 0.9999)
    assert(clue(d1) < 1.0001)
    // four times in two years should be 100% annual
    val d2 = Gain.annualYield(Currency(1, 0), Currency(4, 0), 2.0)
    assert(clue(d2) > 0.9999)
    assert(clue(d2) < 1.0001)
  }

}
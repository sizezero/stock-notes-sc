package org.kleemann.stocknotes

class TestGain extends munit.FunSuite {

  // parsing

  test("succeed parse commission") {
    val ret = Right(Gain.ParseArgs(Date(1900,1,1).get, Date(1900,1,1).get, Currency.dollarsCents(30, 0), List[Ticker](), Option.empty[String]))
    assertEquals(Gain.parse(Vector[String]()), ret)
    assertEquals(Gain.parse(Vector[String]("30.0")), ret)
  }

  test("succeed single year") {
    val y = Date(1970,1,1).get
    assertEquals(
        Gain.parse(Vector("1970")), 
        Right(Gain.ParseArgs(y,y,Currency.dollarsCents(-100, 0),List[Ticker](), Option.empty[String])))
  }

  test("succeed double year") {
    assertEquals(
        Gain.parse(Vector("1970:1980")), 
        Right(Gain.ParseArgs(Date(1970,1,1).get,Date(1980,1,1).get,Currency.dollarsCents(-100,0),List[Ticker](), Option.empty[String])))
  }

  test("succeed tickers") {
    val y = Date(1970,1,1).get
    assertEquals(
        Gain.parse(Vector("1970", "msFt", "gooG", "cScO")), 
        Right(Gain.ParseArgs(y,y,Currency.dollarsCents(-100,0),List(Ticker("CSCO"),Ticker("GOOG"),Ticker("MSFT")), Option.empty[String])))
  }

  test("succeed omit") {
    assertEquals(
        Gain.parse(Vector("-omit","BigCompany")),
        Right(Gain.ParseArgs(Date(1900,1,1).get, Date(1900,1,1).get, Currency.dollarsCents(30, 0), List[Ticker](), Option("BigCompany"))))
    assertEquals(
        Gain.parse(Vector("-omit","BigCompany", "30", "a", "b", "c")),
        Right(Gain.ParseArgs(Date(1900,1,1).get, Date(1900,1,1).get, Currency.dollarsCents(30, 0), List(Ticker("A"),Ticker("B"),Ticker("C")), Option("BigCompany"))))
  }

  test("fail bad date") {
      assert(Gain.parse(Vector("1000000:2000000")).isLeft)
  }

  // functionality

  test("functionalGain") {
    // TODO three companies, test both sell immediate and sell in years range
    assert(true)
  }

  test("parseCompany") {
    // test both sell immediate and sell in years range, do a split
    assert(true)
  }

  test("parsedMatchedSell") {
    // test all three cases of sell equals buy, and each exceeds

    // def parseMatchedSell(sell: Sell, buys: Vector[BuyReadyToSell]): (MatchedSell, Vector[BuyReadyToSell]) = {
    // date: Date, shares: Shares, price: Currency, commission: Currency
    val com = Currency.zero // all commissions are zero to make things easy
    val b1 = Buy(Date(2015,1,1).get, Shares(2,Fraction.one), Currency.dollarsCents(2, 0), com)
    val b2 = Buy(Date(2016,1,1).get, Shares(2,Fraction.one), Currency.dollarsCents(3, 0), com)
    val sp1 = Split(Date(2017,1,1).get, Fraction(2,1))
    val m = Fraction(2,1) // the new multiple
    val b3 = Buy(Date(2018,2,1).get, Shares(2,m), Currency.dollarsCents(2, 0), com) // shares 1, price 4 pre-split
    // shares 5, price 6 pre split. This should empty out the bought shares
    val s1 = Sell(Date(2019,1,1).get, Shares(10,m), Currency.dollarsCents(3,0), com)
    val b4 = Buy(Date(2020,1,1).get, Shares(2,m), Currency.dollarsCents(4, 0), com) // shares 1, price 8 pre split

    // TODO: right now we blow up if we try to sell something for which there are no buys. I think
    // to do things right, we should bubble up the failed sell. Ugh. Maybe that is caught in parsing 
    // and we don't have to worry? It could just be a test thing

    val trades = List[Trade](b1, b2, sp1, b3, s1, b4)

    val brss: Vector[Gain.BuyReadyToSell] = trades.flatMap{ t => t match {
      case b: Buy => List(Gain.BuyReadyToSell(b))
      case _ => Nil
    }}.toVector

    val (ms, brss2) = Gain.parseMatchedSell(s1, brss)

    val ms2 = List[Gain.MatchedBuy](
      Gain.MatchedBuy(b1, Shares(4,m), com, true, 0.6548754598234365),
      Gain.MatchedBuy(b2, Shares(4,m), com, true, 0.7099759466766968),
      Gain.MatchedBuy(b3, Shares(2,m), com, false, 8.042312756132256)
    )

    assertEquals(ms, Gain.MatchedSell(s1, ms2))

    val brss3 = Vector[Gain.BuyReadyToSell](
      Gain.BuyReadyToSell(b4, Shares(2,m))  // none of the shares were sold from this, still at two
    )

    assertEquals(brss2, brss3)
  }

  test("annualYield") {
    // one dollar to two dollars in a year, 100% annual
    val d1 = Gain.annualYield(Currency.dollarsCents(1, 0), Currency.dollarsCents(2, 0), 1.0)
    assert(clue(d1) > 0.9999)
    assert(clue(d1) < 1.0001)
  }

}

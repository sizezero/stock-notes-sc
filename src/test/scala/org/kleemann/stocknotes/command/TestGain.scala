package org.kleemann.stocknotes.command

import org.kleemann.stocknotes.{Ticker}
import org.kleemann.stocknotes.stock.{Currency, Date}

class TestGain extends munit.FunSuite {

  // parsing

  test("succeed parse commission") {
    val ret = Right(Gain.ParseArgs(Date.earliest, Date.earliest, Currency(30, 0), List[Ticker](), Option.empty[String]))
    assertEquals(Gain.parse(Vector[String]()), ret)
    assertEquals(Gain.parse(Vector[String]("30.0")), ret)
  }

  test("succeed single year") {
    val y1 = Date(1970,1,1).get
    val y2 = Date(y1.year,12,31).get
    assertEquals(
        Gain.parse(Vector("1970")), 
        Right(Gain.ParseArgs(y1,y2,Currency(-100, 0),List[Ticker](), Option.empty[String])))
  }

  test("succeed double year") {
    assertEquals(
        Gain.parse(Vector("1970:1980")), 
        Right(Gain.ParseArgs(Date(1970,1,1).get,Date(1980,12,31).get,Currency(-100,0),List[Ticker](), Option.empty[String])))
  }

  test("succeed tickers") {
    val y1 = Date(1970,1,1).get
    val y2 = Date(y1.year,12,31).get
    assertEquals(
        Gain.parse(Vector("1970", "msFt", "gooG", "cScO")), 
        Right(Gain.ParseArgs(y1,y2,Currency(-100,0),List(Ticker("CSCO"),Ticker("GOOG"),Ticker("MSFT")), Option.empty[String])))
  }

  test("succeed omit") {
    assertEquals(
        Gain.parse(Vector("-omit","BigCompany")),
        Right(Gain.ParseArgs(Date.earliest, Date.earliest, Currency(30, 0), List[Ticker](), Option("BigCompany"))))
    assertEquals(
        Gain.parse(Vector("-omit","BigCompany", "30", "a", "b", "c")),
        Right(Gain.ParseArgs(Date.earliest, Date.earliest, Currency(30, 0), List(Ticker("A"),Ticker("B"),Ticker("C")), Option("BigCompany"))))
  }

  test("fail bad date") {
      assert(Gain.parse(Vector("1000000:2000000")).isLeft)
  }

}
package org.kleemann.stocknotes

import stock.Currency

class TestCalc extends munit.FunSuite {

    test("parse income four quarters") {
        val it: Iterator[String] =
            """
            |inc 100 200 300 400
            |""".stripMargin.split("\n").iterator
        Calc.inputToAttributes(it) match {
            case Right(att) => {
                assertEquals(att.income, Some(Currency(1_000_000, 0)))
            }
            case Left(_) => assert(false)
        }
    }

    test("parse income m") {
        val it: Iterator[String] =
            """
            |inc 100m 0 0 0
            |""".stripMargin.split("\n").iterator
        Calc.inputToAttributes(it) match {
            case Right(att) => {
                assertEquals(att.income, Some(Currency(100_000_000, 0)))
            }
            case Left(_) => assert(false)
        }
    }

    test("parse revenue k") {
        val it: Iterator[String] =
            """
            |inc 100k 100k 100k
            |""".stripMargin.split("\n").iterator
        Calc.inputToAttributes(it) match {
            case Right(att) => {
                assertEquals(att.income, Some(Currency(400_000, 0)))
            }
            case Left(_) => assert(false)
        }
    }

    test("income from eps and shares") {
        val it: Iterator[String] =
            """
            |eps $3.00
            |shares 100k
            |""".stripMargin.split("\n").iterator
        Calc.inputToAttributes(it) match {
            case Right(att) => {
                assertEquals(att.income, Some(Currency(300_000, 0)))
            }
            case Left(_) => assert(false)
        }
    }

    test("calc eps") {
        val it: Iterator[String] =
            """
            |inc 100k
            |shares 400k
            |""".stripMargin.split("\n").iterator
        Calc.inputToAttributes(it) match {
            case Right(att) => {
                assertEquals(att.eps, Some(Currency(1, 0)))
            }
            case Left(_) => assert(false)
        }
    }

    test("calc pe") {
        val it: Iterator[String] =
            """
            |price $20.00
            |eps $2.00
            |""".stripMargin.split("\n").iterator
        Calc.inputToAttributes(it) match {
            case Right(att) => {
                att.pe match {
                    case Some(pe) => {
                        assert(pe > 9.9999)
                        assert(pe < 10.0001)
                    }
                    case None => assert(false)
                }
            }
            case Left(_) => assert(false)
        }
    }

    test("calc pe via marketcap") {
        val it: Iterator[String] =
            """
            |mc $1,000,000.00
            |income 100k 0 0 0
            |""".stripMargin.split("\n").iterator
        Calc.inputToAttributes(it) match {
            case Right(att) => {
                att.pe match {
                    case Some(pe) => {
                        assert(pe > 9.9999)
                        assert(pe < 10.0001)
                    }
                    case None => assert(false)
                }
            }
            case Left(_) => assert(false)
        }
    }

    test("marketcap from price and shares") {
        val it: Iterator[String] =
            """
            |price $10
            |shares 20
            |""".stripMargin.split("\n").iterator
        Calc.inputToAttributes(it) match {
            case Right(att) => {
                assertEquals(att.marketCap, Some(Currency(200, 0)))
            }
            case Left(_) => assert(false)
        }
    }

    test("price from mc and shares") {
        val it: Iterator[String] =
            """
            |mc $100
            |shares 20
            |""".stripMargin.split("\n").iterator
        Calc.inputToAttributes(it) match {
            case Right(att) => {
                assertEquals(att.price, Some(Currency(5, 0)))
            }
            case Left(_) => assert(false)
        }
    }

    test("dividend yield from dividend and price") {
        val it: Iterator[String] =
            """
            |div 0.50
            |price $2.50
            |""".stripMargin.split("\n").iterator
        Calc.inputToAttributes(it) match {
            case Right(att) => {
                att.dividendYield match {
                    case Some(dy) => {
                        assert(dy > 0.1999999)
                        assert(dy < 0.2000001)
                    }
                    case None => assert(false)
                }
            }
            case Left(e) => assert(false, clue(e))
        }
    }

    test("payout ratio from dividend and eps") {
        val it: Iterator[String] =
            """
            |div 0.50
            |eps $2.50
            |""".stripMargin.split("\n").iterator
        Calc.inputToAttributes(it) match {
            case Right(att) => {
                att.payoutRatio match {
                    case Some(pr) => {
                        assert(pr > 0.1999999)
                        assert(pr < 0.2000001)
                    }
                    case None => assert(false)
                }
            }
            case Left(e) => assert(false, clue(e))
        }
    }

    test("conflict") {
        val it: Iterator[String] =
            """
            |income 0.50
            |eps $2.50
            |shares 20
            |""".stripMargin.split("\n").iterator
        Calc.inputToAttributes(it) match {
            case Right(att) => assert(false)
            case Left(e)    => assert(true)
        }
    }
}

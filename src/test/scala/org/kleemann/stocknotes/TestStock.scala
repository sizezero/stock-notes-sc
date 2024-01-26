package org.kleemann.stocknotes

class TestStock extends munit.FunSuite {

    test("empty file") {
        val ticker = Ticker("MSFT")
        val g: os.Generator[String] = os.Generator.from(Seq())
        val obtained = Stock.load(ticker, "filename", g)
        val required = Stock(
            ticker,
            None,
            None,
            Set(),
            List(Entry(ticker, Date.earliest, "")),
            List[Trade](),
            BuyWatch.none,
            SellWatch.none
        )
        assertEquals(obtained, Right(required))
    }

    test("simple single entry") {
        val ticker = Ticker("MSFT")
        val g: os.Generator[String] = os.Generator.from(
            """
            |NAME: Microsoft Corp
            |CID: 0000789019
            |KEYWORDS: megacap2008 maybe_great
            |
            |Jan 1, 1992
            |foobar
            |""".stripMargin.split("\n")
        )
        val obtained = Stock.load(ticker, "filename", g)
        val entries = List(
            Entry(ticker, Date.earliest, """
                |NAME: Microsoft Corp
                |CID: 0000789019
                |KEYWORDS: megacap2008 maybe_great
                |
                |""".stripMargin),
            Entry(ticker, Date(1992,1,1).get, "foobar\n")
        )
        val required = Stock(
            ticker,
            Some("Microsoft Corp"),
            Some("0000789019"),
            Set("megacap2008", "maybe_great"),
            entries,
            List[Trade](),
            BuyWatch.none,
            SellWatch.none
        )
        assertEquals(obtained, Right(required))
    }

    test("bad date order") {
        val ticker = Ticker("MSFT")
        val g: os.Generator[String] = os.Generator.from(
            """
            |Jan 1, 1992
            |foobar
            |Jan 1, 1980
            |barfoo
            |""".stripMargin.split("\n")
        )
        val obtained = Stock.load(ticker, "filename", g)
        assertEquals(obtained, Left("filename(4): date Jan  1, 1980 is not greater than previous date Jan  1, 1992"))
    }

    test("bad keywords") {
        val ticker = Ticker("MSFT")
        val g: os.Generator[String] = os.Generator.from(
            """
            |KEYWORDS: good1 bad1!@#$ good2 bad2%%&*
            |""".stripMargin.split("\n")
        )
        val obtained = Stock.load(ticker, "filename", g)
        assertEquals(obtained, Left("filename(2): keywords must be alphanumeric with underscores: bad1!@#$"))
    }

    test("trades") {
        val ticker = Ticker("MSFT")
        val g: os.Generator[String] = os.Generator.from(
            """
            |Jan 1, 1990
            |TRADE buy 10@$5.00 balance 10 commission 9.99
            |Jan 1, 1991
            |TRADE buy 10@$6.00 balance 20 commission 9.99
            |Jan 1, 1992
            |TRADE split 2:1 balance 40
            |Jan 1, 1993
            |TRADE sell 30@4 balance 10 commission 0
            |""".stripMargin.split("\n")
        )
        Stock.load(ticker, "filename", g) match {
            case Left(msg) => assert(false, clue(msg))
            case Right(s) => {
                assertEquals(s.trades,
                    List(
                        Buy(Date(1990,1,1).get, Shares(10,Fraction.one), Currency(5, 0), Currency(9, 99)),
                        Buy(Date(1991,1,1).get, Shares(10,Fraction.one), Currency(6, 0), Currency(9, 99)),
                        Split(Date(1992,1,1).get, Fraction(2,1)),
                        Sell(Date(1993,1,1).get, Shares(30,Fraction(2,1)), Currency(4, 0), Currency.zero)

                    )
                )
            }
        }
    }

    test("error on negative balance") {
        val ticker = Ticker("MSFT")
        val g: os.Generator[String] = os.Generator.from(
            """
            |Jan 1, 1990
            |TRADE buy 10@$5.00 balance 10 commission 9.99
            |Jan 1, 1991
            |TRADE sell 20@$6.00 balance 20 commission 9.99
            |""".stripMargin.split("\n")
        )
        val obtained = Stock.load(ticker, "filename", g);
        assertEquals(obtained, Left("filename(5): share count cannot be negative: Shares(-10,1/1)"))
    }

    test("watch") {
        val ticker = Ticker("MSFT")
        val g: os.Generator[String] = os.Generator.from(
            """
            |BUY 12
            |BUY 12 34
            |SELL 45 67
            |SELL None
            |""".stripMargin.split("\n")
        )
        Stock.load(ticker, "filename", g) match {
            case Left(msg) => assert(false, clue(msg))
            case Right(Stock(_, _, _, _, _, _, b, s)) => {
                assertEquals(b, BuyWatch(Some(Currency(12, 0)), Some(Currency(34, 0)), Fraction.one))
                assertEquals(s, SellWatch.none)
            }
        }
    }
}

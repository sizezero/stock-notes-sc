package org.kleemann.stocknotes.stock

import org.kleemann.stocknotes.{Currency, Date}

class TestCashAccount extends munit.FunSuite {

  test("good 1") {
    val accountName = "name"
    val cashFile = "foo" // this is just for error reporting
    val in: os.Generator[String] = os.Generator.from(
        """line one
        |line two
        |Apr 24, 1968
        |Some more text
        |BALANCE: $1,000,000.00
        |last line""".stripMargin.split("\\n").toSeq)
    val obtained = CashAccount.load(accountName, cashFile, in)
    val expected = CashAccount(accountName, Date(1968,4,24).get, Currency(1_000_000, 0))
    assertEquals(obtained, Right(expected))
  }

  test("good 2") {
    val accountName = "name"
    val cashFile = "foo" // this is just for error reporting
    val in: os.Generator[String] = os.Generator.from(
        """line one
        |line two
        |BALANCE: $5.00
        |Jan 1, 2030
        |Some more text
        |last line""".stripMargin.split("\\n").toSeq)
    val obtained = CashAccount.load(accountName, cashFile, in)
    val expected = CashAccount(accountName, Date(2030,1,1).get, Currency(5, 0))
    assertEquals(obtained, Right(expected))
  }

  test("two dates") {
    val accountName = "name"
    val cashFile = "foo" // this is just for error reporting
    val in: os.Generator[String] = os.Generator.from(
        """line one
        |line two
        |BALANCE: $5.00
        |Jan 1, 2030
        |Jan 2, 2030
        |Some more text
        |last line""".stripMargin.split("\\n").toSeq)
    val obtained = CashAccount.load(accountName, cashFile, in)
    assertEquals(obtained, Left("foo(5): date entered a second time"))
  }

  test("no date") {
    val accountName = "name"
    val cashFile = "foo" // this is just for error reporting
    val in: os.Generator[String] = os.Generator.from(
        """line one
        |line two
        |BALANCE: $5.00
        |Some more text
        |last line""".stripMargin.split("\\n").toSeq)
    val obtained = CashAccount.load(accountName, cashFile, in)
    assertEquals(obtained, Left("foo(5): no date entered"))
  }

  test("no balance") {
    val accountName = "name"
    val cashFile = "foo" // this is just for error reporting
    val in: os.Generator[String] = os.Generator.from(
        """line one
        |line two
        |Jan 2, 2030
        |Some more text
        |last line""".stripMargin.split("\\n").toSeq)
    val obtained = CashAccount.load(accountName, cashFile, in)
    assertEquals(obtained, Left("foo(5): no balance entered"))
  }

}
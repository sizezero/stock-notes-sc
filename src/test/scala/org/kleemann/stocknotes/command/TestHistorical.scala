package org.kleemann.stocknotes.command

import org.kleemann.stocknotes.Date

class TestHistorical extends munit.FunSuite {

  test("succeed at simple option all") {
    Historical.parse(Vector("all")) match {
      case Right(d1, d2) => {
        assertEquals(d1, Date.earliest)
        assertEquals(d2, Date.latest)
      }
      case Left(error) => assert(false)
    }
  }

  test("succeed at simple option one year") {
    Historical.parse(Vector("1980")) match {
      case Right(d1, d2) => {
        assertEquals(d1, Date.earliest(1980).get)
        assertEquals(d2, Date.latest(1980).get)
      }
      case Left(error) => assert(false)
    }
  }

  test("succeed at simple option two years") {
    Historical.parse(Vector("1980:1985")) match {
      case Right(d1, d2) => {
        assertEquals(d1, Date.earliest(1980).get)
        assertEquals(d2, Date.latest(1985).get)
      }
      case Left(error) => assert(false)
    }
  }

  test("date out of range") {
    Historical.parse(Vector("7000")) match {
      case Right(d1, d2) => assert(false)
      case Left(error)   => assert(true)
    }
  }

}

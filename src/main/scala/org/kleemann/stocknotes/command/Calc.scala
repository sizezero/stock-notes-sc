package org.kleemann.stocknotes.command

import scala.io.Source

object Calc extends Command {

  val help = Some(s"""
    |stock-notes calc
    |    <stdin> stock data
    |    <stdout> augmented stock data
  """.stripMargin)


  override def command(args: IndexedSeq[String]): Option[String] = {
    if (args.length == 0) {
        // read stdin
        val it: Iterator[String] = Source.stdin.getLines()
        val out = org.kleemann.stocknotes.calc.Calc.calc(it)
        // write stdout
        print(out) // no println because we don't want an extra newline
        None
    } else help
  }
}

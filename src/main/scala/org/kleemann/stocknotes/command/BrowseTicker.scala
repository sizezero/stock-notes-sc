package org.kleemann.stocknotes.command

import org.kleemann.stocknotes.Ticker

// some things this needs
// - its all about side effects; I probably need some pure functional fns in here to make sure this works.
// - I could pass configuration into this that has all of the read values
// - I could also pass an IO object in here that has implementations for openBrowser, openEditor, and others.
// - before I implementing this I need a fully populated "companies" singleton.
// - I don't think I want to go overboard on adding test configurations to this. Just get it to work and if tests need help, refactor.
// - To start lets just make each of these command that correctly parse their arguments.

object BrowseTicker extends Command {

  val help = Some(s"""
    |stock-notes browse-ticker [-n] <ticker>
        -n: do not open an editor
  """.stripMargin)

  override def command(args: IndexedSeq[String]): Option[String] = {
    if (args.length == 1) browseTicker(Ticker(args(0)), true)
    else if (args.length == 2) {
        val opt = args(0)
        val ticker = Ticker(args(1))
        if (opt != "-n") help
        else browseTicker(ticker, false)
    } else help
  }

  def browseTicker(ticker: Ticker, edit: Boolean): Option[String] = None
}

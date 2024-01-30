package org.kleemann.stocknotes.command

import org.kleemann.stocknotes.Config
import org.kleemann.stocknotes.stock.{Stock}

object GenerateWww extends Command {

  private def generateWww(): Unit = {

    val config = Config.load()
    val stock = Stock.load(config)

    // clean files out of www
    os.remove.all(config.wwwDir)
    os.makeDir(   config.wwwDir)

    // TODO: we may want to delegate to functional code to generate the actual files

    val indexFile = config.wwwDir / "index.html"
    os.write(indexFile, "<p>Hello World\n")

  }

  val help = Some(s"""
    |stock-notes generate-www
  """.stripMargin)

  override def command(args: IndexedSeq[String]): Option[String] =
    if (args.length==0) {
      generateWww()
      None
    }
    else help

}
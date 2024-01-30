package org.kleemann.stocknotes.command

import org.kleemann.stocknotes.Config
import org.kleemann.stocknotes.stock.{Stock}

object GenerateWww extends Command {

    // TODO: good canadidate for functional testing
    // especially if we add more features
    private def toHtml(it: IndexedSeq[String]): String = {
        val content = {
            import scalatags.Text.all._
            html {
                head()
                body(
                    for (s <- it) yield div(s, br(), "\n")
                )
            }
        }
        content.toString
    }

    private def generateWww(): Unit = {

        val config = Config.load()
        val stocks = Stock.load(config)

        // clean files out of www
        os.remove.all(config.wwwDir)
        os.makeDir(   config.wwwDir)

        // TODO: we may want to delegate to functional code to generate the actual files

        os.write(config.wwwDir/"index.html", "<p>Hello World\n")

        // generate all log files
        val logDstDir = config.wwwDir/"log"
        os.makeDir(logDstDir)
        stocks.foreach{ s =>
            val baseName: String = s.ticker.ticker.toLowerCase() + ".txt"
            val inFile = config.logDir / baseName
            val outFile = logDstDir / (baseName + ".html")
            val content = toHtml(os.read.lines(inFile))
            os.write(outFile, content)
        }
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
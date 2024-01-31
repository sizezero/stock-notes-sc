package org.kleemann.stocknotes.command

object GenerateWww extends Command {

    val help = Some(s"""
        |stock-notes generate-www
    """.stripMargin)

    override def command(args: IndexedSeq[String]): Option[String] =
        if (args.length==0) {
            www.Generate.refreshWwwDir()
            None
        }
        else help

}
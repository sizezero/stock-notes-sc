package org.kleemann.stocknotes.command

import org.kleemann.stocknotes.Date

object CalcTemplate extends Command {

    val help = Some(s"""
        |stock-notes calc-template
    """.stripMargin)

    override def command(args: IndexedSeq[String]): Option[String] = {
        if (args.length == 0) {
            println(s"""
                |${Date.today.toStringEnglishFixedWidth()}
                |rev 
                |inc 
                |shares 
                |price 
                |div 
                |==>
                |""".stripMargin)
            None
        }
        else help
    }

}

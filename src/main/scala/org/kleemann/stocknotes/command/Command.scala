package org.kleemann.stocknotes.command

protected trait Command {
  /**
    * A command defined by the first argument of the command line program.
    * This command is assumed to possibly perform side effects such as 
    * printing to the screenm, loading files, calling sys.exit(), etc.
    *
    * @param args All remaining command line arguments except for the first.
    * @return None if successful, an error string if there was a problem
    */
  def command(args: IndexedSeq[String]): Option[String]

  // TODO: consider adding a method that can return the help, this way the caller can reconstruct the full help text for all commands.
}

object Command {

  val all: Map[String, Command] = Map(
    "current"         -> Current,
    "historical"      -> Historical,
    "browse-ticker"   -> BrowseTicker,
    "download-quotes" -> DownloadQuotes,
    "calc"            -> Calc,
    "gain"            -> Gain,
    "generate-www"    -> GenerateWww,
    "oldest"          -> Oldest
  )

}

package org.kleemann.stocknotes

trait Command {
    /**
      * A command defined by the first argument of the command line program
      *
      * @param args All remaining command line arguments except for the first.
      * 
      */
  def command(args: IndexedSeq[String]): Option[String]

  // TODO: consider adding a method that can return the help, this way the caller can reconstruct the full help text for all commands.
}

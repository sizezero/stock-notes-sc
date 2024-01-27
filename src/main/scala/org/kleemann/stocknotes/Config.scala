package org.kleemann.stocknotes

import sttp.client4.quick.*
import sttp.model.Uri
import upickle.default.*

/** An object that encapsulates the user configuration for this program.
  *
  * Note: os.Path objects cannot be serialized/deserialized with upickle so we we String instead
  * 
  * @param shared The path of checked out stock-notes-data project in the users home dir
  * @param projectDir The path of the checked out stock-notes-sc project (this project) in the user's home dir
  * @param finnhubAccessKey The key to access the finnhub.io site.
  */
final case class Config (shared: String, projectDir: String, finnhubAccessKey: String) derives ReadWriter {

    /**
      * The directory that contains all edited stock notes of the form <ticker>.txt
      */
    def logDir: os.Path = os.home/ shared /"log"

    /**
      * The directory where cash accounts are stored.
      */
    def cashDir: os.Path = os.home/ shared /"cash"

    /**
      * This is a legacy file used for gain. The scala system reads from it but doesn't write.
      */
    def notifyQuotesFile: os.Path = os.home/ shared /"download"/"quotes_notify.csv"

    /**
      * This is a legacy file used for the buysell html page. The scala system reads from it but doesn't write.
      */
    def buySellQuotesFile: os.Path = os.home/ shared /"download"/"quotes_buysell.csv"

    /**
      * This file that the scala system reads from and writes to.
      * 
      * On reading, if it doesn't exist, then the two legacy files are read and the newest dated quotes are kept.
      *
      * @return
      */
    def quotesFile: os.Path = os.home/ shared /"download"/"quotes.csv"


    /**
      * I'm not sure what parts of the program need a temp directory
      */
    def tempFile: os.Path = os.home/ shared /"download"/"temp"

    /**
      * Used by browse_ticker to display the SEC page for an unknown company.
      */
    def noCikUrl: String = "file://" + (os.home / projectDir / "src" / "res" / "nocik.html").toString
}

object Config {

    /**
      * If the config file exists then load it an return it.
      * If not, write out a temp version of the file, inform the user to edit it, and exit the program.
      *
      * @return
      */
    def load(f: os.Path = os.home/".stocknotes.json"): Config = {
        if (os.exists(f)) read[Config](os.read(f))
        else {
            // case class with default values does not deserialize properly with upickle so we have to have default values here
            val c = org.kleemann.stocknotes.Config("stock-notes-data", "stock-notes-sc", "********")
            os.write(f, write(c))
            println("No configuration file was found.")
            println("A new configuration file was written as: "+f.toString)
            println("Edit the configuration file and rerun this program.")
            sys.exit(1)
        }
    }
}

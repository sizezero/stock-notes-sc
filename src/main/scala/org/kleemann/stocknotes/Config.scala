package org.kleemann.stocknotes

import sttp.client4.quick.*
import sttp.model.Uri
import upickle.default.*

/** An object that encapsulates the user configuration for this program.
  *
  * Note: os.Path objects cannot be serialized/deserialized with upickle so we use String instead
  * 
  * @param sharedDir The path of checked out stock-notes-data project in the users home dir
  * @param projectDir The path of the checked out stock-notes-sc project (this project) in the user's home dir
  * @param finnhubAccessKey The key to access the finnhub.io site.
  */
final case class Config (sharedDir: String, projectDir: String, finnhubAccessKey: String) derives ReadWriter {

  /**
    * The directory that contains all edited stock notes of the form <ticker>.txt
    * TODO: there is probably a better name for this. notes/ stock/ tickers/
    */
  def logDir: os.Path = os.home/ sharedDir /"log"

  /**
    * The directory where cash accounts are stored.
    */
  def cashDir: os.Path = os.home/ sharedDir /"cash"

  /**
    * This is the quotes file that the scala system reads from and writes to.
    *
    * Even though this doesn't depend on variable data and could exist in the Config object,
    * we place it here to insure that the parent directories have been created.
    */
  def quotesFile: os.Path = Config.configDir/"quotes.csv"

  /**
    * The output of all generated web files.
    *
    * Even though this doesn't depend on variable data and could exist in the Config object,
    * we place it here to insure that the parent directories have been created.
    */
  def wwwDir: os.Path = Config.wwwDir

  /**
    * Used by browse_ticker to display the SEC page for an unknown company.
    */
  def noCikUrl: String = "file://" + (os.home/ projectDir /"src"/"res"/"nocik.html").toString
}

object Config {

  private val configDir:  os.Path = os.home/".stocknotes"
  private val configFile: os.Path = configDir/"config.json"
  private val wwwDir:     os.Path = configDir/"www"

  /**
    * The config dir consists of:
    * ~/.stocknotes/
    *   config.json
    *   www/
    *
    * If any of the directories don't exist they are created.
    * If the config file exists this object is returned, otherwise write out a template version of the file,
    * inform the user to edit it, and exit the program.
    *
    * @return
    */
  def load(): Config = {

    if (!os.exists(configDir)) os.makeDir(configDir)

    if (!os.exists(wwwDir)) os.makeDir(wwwDir)

    if (!os.exists(configFile)) {
      val c = org.kleemann.stocknotes.Config("stock-notes-data", "stock-notes-sc", "********")
      os.write(configFile, write(c))
      println("No configuration file was found.")
      println("A new configuration file was written as: "+configFile.toString)
      println("Edit the configuration file and rerun this program.")
      sys.exit(1)
    } else
      read[Config](os.read(configFile))
  }
}

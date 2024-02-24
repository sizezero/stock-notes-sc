package org.kleemann.stocknotes.command

import scala.collection.mutable
import sttp.client4.quick.*
import sttp.model.StatusCode
import sttp.model.Uri

import org.kleemann.stocknotes.{Config, Date, Ticker, Quote}
import org.kleemann.stocknotes.stock.{Stock}
import org.kleemann.stocknotes.stock.{BuyWatch, SellWatch}

/**
  * Downloads needed quotes from an online quote service into the file config.quotesFile
  * 
  * Almost everything in this file has side effects so there are no tests for this.
  */
object DownloadQuotes extends Command {

  private val delayInSeconds = 2

  private def downloadQuotes(): Unit = {

    val config = Config.load()

    // we need quotes for stocks we either own or that have active watches
    val tickers = Stock.load(config).filter{ s =>
      (s.keywords contains "owned") || s.buyWatch != BuyWatch.none || s.sellWatch != SellWatch.none
    }.map{ _.ticker }

    val delay: Double = (delayInSeconds/60.0) * tickers.length
    println(f"The download is estimated to take ${delay}%2.2f minutes.")

    Quote.save(tickers, config, downloadSingleQuote(config.finnhubAccessKey))

    println("Download Complete")
  }

  /**
    * This implementation of http get uses the sttp library.
    * It depends on libraries from Java 11 and cannot run under java 8.
    */
  private def getJsonViaSttp(baseUrl: String, options: Map[String, String]): Option[String] = {
    // uri"http:foo.com/dir?$options" is prettier but the below is required when the root url is in a variable.
    // couldn't get sttp to not encode the baseUrl so using java Uri instead
    val jUri: java.net.URI = java.net.URI(baseUrl)
    val uBase: Uri = Uri(jUri)
    val u: Uri = uBase.addParams(options)
    val request = quickRequest.get(u)
    val response = request.send()
    if (response.code == StatusCode.Ok)
      Some(response.body)
    else
      None
  }

  // this is the original ancient java 8 code found on the internet
/* 
public static String executePost(String targetURL, String urlParameters) {
  HttpURLConnection connection = null;

  try {
    //Create connection
    URL url = new URL(targetURL);
    connection = (HttpURLConnection) url.openConnection();
    connection.setRequestMethod("POST");
    connection.setRequestProperty("Content-Type", 
        "application/x-www-form-urlencoded");

    connection.setRequestProperty("Content-Length", 
        Integer.toString(urlParameters.getBytes().length));
    connection.setRequestProperty("Content-Language", "en-US");  

    connection.setUseCaches(false);
    connection.setDoOutput(true);

    //Send request
    DataOutputStream wr = new DataOutputStream (
        connection.getOutputStream());
    wr.writeBytes(urlParameters);
    wr.close();

    //Get Response  
    InputStream is = connection.getInputStream();
    BufferedReader rd = new BufferedReader(new InputStreamReader(is));
    StringBuilder response = new StringBuilder(); // or StringBuffer if Java version 5+
    String line;
    while ((line = rd.readLine()) != null) {
      response.append(line);
      response.append('\r');
    }
    rd.close();
    return response.toString();
  } catch (Exception e) {
    e.printStackTrace();
    return null;
  } finally {
    if (connection != null) {
      connection.disconnect();
    }
  }
}
 */

  /**
    * I just got this working. I'm not sure if all the the api calls are necessary to get this to work.
    *
    * @param baseUrl
    * @param queryParams
    * @return
    */
  private def getJsonViaJava8(baseUrl: String, queryParams: Map[String, String]): Option[String] = {
    var connection: java.net.HttpURLConnection  = null
    //Create connection
    try {
      val urlParameters: String = queryParams.map{ (key, value) => f"$key=$value"}.mkString("&")
      val urlString = baseUrl + urlParameters
      //println(urlString)
      val url: java.net.URL = new java.net.URL(urlString);
      connection = url.openConnection().asInstanceOf[java.net.HttpURLConnection]; // cast to (HttpURLConnection)
      connection.setRequestProperty("Content-Type", 
        "application/x-www-form-urlencoded");
      connection.setRequestProperty("Content-Type",
                                       "application/json")
      connection.setRequestProperty("Accept",
                                       "application/json")
      connection.setUseCaches(false)
      connection.setDoOutput(true)

      //Get Response  
      val is: java.io.InputStream = connection.getInputStream()
      val rd: java.io.BufferedReader = new java.io.BufferedReader(new java.io.InputStreamReader(is))
      var response: StringBuilder = new StringBuilder() // or StringBuffer if Java version 5+
      var line: String = ""
      while (line != null) {
        line = rd.readLine()
        if (line != null) {
          response.append(line)
          response.append('\r')
        }
      }
      rd.close();
      val returnString = response.toString()
      Some(returnString)
    } catch {
      case e: Exception => {
        e.printStackTrace()
        None
      }
    } finally {
      if (connection != null) {
        connection.disconnect()
      }
    }
  }

  /**
    * We either return an error or a decimal stock price.
    * We could return it as something besides a string but since we're just going 
    * to write it to a file there's no advantage in making a complicated type
    *
    * @param ticker
    * @return
    */
  private def downloadSingleQuote(finnhubAccessKey: String)(ticker: Ticker): Either[String, String] = {

    // finnhub doesn't allows us to spam their service so we need to slow it down a bit
    Thread.sleep(delayInSeconds * 1_000L)

    val baseUrl = "https://finnhub.io/api/v1/quote?"
    val queryParams = Map("symbol" -> ticker.ticker, "token" -> finnhubAccessKey)
    getJsonViaJava8(baseUrl, queryParams) match {
      case Some(body) => {
        val json = ujson.read(body)
        // ex
        // {"c":4.93,"d":-0.08,"dp":-1.5968,"h":5.08,"l":4.91,"o":4.93,"pc":5.01,"t":1706302801}
        val m: Map[String,String] = upickle.default.read[Map[String,String]](json)
        m.get("c") match {
          case Some(price) => Right(price)
          case None        => Left(f"key 'c' not found in json")
        }
      }
      case None => Left(f"Ticker service failed with non 200 response code")
    }
  }

  val help = Some(s"""
    |stock-notes download-quotes
  """.stripMargin)

  override def command(args: IndexedSeq[String]): Option[String] =
    if (args.length==0) {
      downloadQuotes()
      None
    }
    else help

}

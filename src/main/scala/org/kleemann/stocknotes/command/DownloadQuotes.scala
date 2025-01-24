package org.kleemann.stocknotes.command

import scala.collection.mutable
import sttp.client4.quick.*
import sttp.model.StatusCode
import sttp.model.Uri

import org.kleemann.stocknotes.{Config, Currency, Date, Ticker, Quote}
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
    val tickers: List[Ticker] =
      for (stock <- Stock.load(config)
        if (stock.keywords contains "owned") || (stock.keywords contains "watching"))
      yield stock.ticker

    val delay: Double = (delayInSeconds/60.0) * tickers.length
    println(f"The download is estimated to take ${delay}%2.2f minutes.")

    // we use the crusty Java 8 httpGet because we want this program to run
    // on dreamhost which only supports Java 8
    Quote.save(tickers, config, downloadSingleQuoteFromFinnhub(config.finnhubAccessKey, httpGetViaJava8))

    println("Download Complete")
  }

  /**
    * GET the given url and return the body.
    * 
    * This implementation uses the modern sttp library from Scalas toolkit. It requires Java 11.
    *
    * @param baseUrl The full URL of the call. Does not include trailing slash or ?
    * @param options query parameters
    * @return The response body if status code was 200 or None if there was an error.
    */
  private def httpGetViaSttp(baseUrl: String, options: Map[String, String]): Option[String] = {
    // uri"http:foo.com/dir?$options" is prettier but the below is required when the root url is in a variable.
    // couldn't get sttp to not encode the baseUrl so using java Uri instead
    val jUri: java.net.URI = java.net.URI(baseUrl + "?")
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
    * GET the given url and return the body.
    * 
    * This implementation uses ancient java 8 libraries.
    * It's not very clean. I just hacked it until it worked so some of the settings/code
    * may not be necessary.
    *
    * @param baseUrl The full URL of the call. Does not include trailing slash or ?
    * @param options query parameters
    * @return The response body if status code was 200 or None if there was an error.
    */
  private def httpGetViaJava8(baseUrl: String, queryParams: Map[String, String]): Option[String] = {
    var connection: java.net.HttpURLConnection  = null
    //Create connection
    try {
      val urlString = baseUrl + "?" + queryParams.map{ (key, value) => f"$key=$value" }.mkString("&")
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
    * The purpose of this function is use the remote service finnhub to lookup the price of a ticker symbol.
    * 
    * The function requires both finnhub authentication as well as an httpGet implementation so currying is used
    * to provide this functionality and create a simpler function that just needs a Ticker argument.
    *
    * @param finnhubAccessKey The access key that allows us to auth agains finnhub web services.
    * @param httpGet An implementation that lets us connect to a webservice. Takes a base url and query paramaters and returns the body on success.
    * @param ticker The ticker/stock we wish to get a quote of.
    * @return The stock price on success or an error message on failure.
    */
  private def downloadSingleQuoteFromFinnhub
    (
      finnhubAccessKey: String, 
      httpGet: (String, Map[String, String]) => Option[String]
    )
    (
      ticker: Ticker
    ): Either[String, Currency] = {

    // finnhub doesn't allows us to spam their service so we need to slow it down a bit
    Thread.sleep(delayInSeconds * 1_000L)

    val baseUrl = "https://finnhub.io/api/v1/quote"
    val queryParams = Map("symbol" -> ticker.name, "token" -> finnhubAccessKey)
    httpGet(baseUrl, queryParams) match {
      case None => Left(f"Ticker service failed with non 200 response code")
      case Some(body) => {
        val json = ujson.read(body)
        // example:
        // {"c":4.93,"d":-0.08,"dp":-1.5968,"h":5.08,"l":4.91,"o":4.93,"pc":5.01,"t":1706302801}
        val m: Map[String,String] = upickle.default.read[Map[String,String]](json)
        m.get("c") match {
          case None => Left(f"key 'c' not found in downloaded json: $body")
          case Some(price) => {
            Currency.parse(price) match {
              case None => Left(f"can't parse as Currency: ${price}")
              case Some(currency) => Right(currency)
            }
          }
        }
      }
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

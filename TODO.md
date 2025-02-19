- entry point with various placeholder commands
  - ~~browse-ticker~~
  - ~~download-quotes~~
  - ~~gain~~
  - ~~next-research~~
    - ~~note: this is actually a shell script that calls stock-notes oldest. Let's keep the functionality the same for now and refactor later if we want to.~~
  - calc
    - this is a completely independent program that should probably exists separately from stock notes
    - rewrite this sometime in the future if I want to deprecate this
    - at a minimum I should make sure that this functionality still works in my stock note editor of choice
  - keywords
    - might be useful but let that one wait for later
- ~~command line options are parsed and help is displayed~~
  - ~~I looked at various scala command line libraries. They seem to provide complex DSLs to solve the taks. I'm going to start just by implementing it myself in raw scala. It's possible this will be the clearest solution.~~
- ~~configuration file has necessary data~~
  - ~~log dir, cash dir, notify quotes file, buy sell quotes file, temp file (???), no cik url, finnhub access key~~
  - ~~make this a high level object, maybe we can just read JSON from a dot file in the home directory~~
- ~~IO object~~
  - I'm not sure if we need this. It would be created at a hight level and passed down.
  - The alterative is to have each command handle whatever IO it needs to do and build the functions in a way that allows easy testing.
  - I think most IO results in a failure of the command so it may not matter.
- ~~parse everything~~
  - ~~date~~
  - notify (maybe don't need this)
  - ~~fraction~~
  - shares
  - stocks
  - quotes
  - price
  - cash
  - buysell
  - trade
- ~~Hierarchy / Typeing~~
  - ~~Quote(price: Double, date: Date)~~
    - need special parser for CSVs; makes sense for this to be close to the quote download file not date
    - object method to load the existing cache of tickers
    - object method to download a list to directory (may need a list of quotes) I wonder how we generate our list for this
      - I can punt on this if I need the other objects first
  - ~~Quotes(List[Quote])~~
    - I'm not sure if we need a structure for this, just a CSV parser; it should probably be part of the CSV Download package
  - ~~I think it's time to refactor the ticker String into a case class~~
  - ~~CashAccount(date: Date, balance: Int)~~
    - balance is stored as dollars*100 (pennies)
    - object method to load a list from the dir
  - ~~price(price: Double, mult:Fraction)~~
    - toStr does not seem to honor the multple. Is this intentional?
    - I didn't fully implement this; I have some questions so let's see how it is used first
  - ~~Shares(shares: Int, Mult: Fraction)~~
    - I'm goint to call this Stock, nope, left it as shares
  - ~~Entry(ticker: Ticker, date: Date, text: String)~~
    - this is every dated entry of every company
    - I don't think they're in chronological order so I'm not sure what they're used for
    - I'm going to try to put this in the case class for Stock (was Company) and see what will happen. Why do that? Leave it as it's own class.
  - ~~Trade abstract base class, I guess a trait~~
    - Buy(date: Date, shares: Shares, price: Price, commission: Double)
    - Sell(date: Date, shares: Shares, price: Price, commission: Double)
    - strangely parsing is returning the balance but it's not part of the data structure
      - I think it's just an assertion for the parsing
    - Split(date: Date, mult: Fraction)
    - Dividend has not been used at all so leave it out
  - ~~Buysell~~
    - These are the four ranges to put stocks in. I really hate the name of this. I'm goint to change this to Alert.
    - Watch(buyLow: Price, buyHigh: Price, sellLow: Price)
  - ~~Company(ticker: String, name: String, cid: Option[String], entries: List[Entry], trades: List[Trade], keywords: Set[String])~~
    - This could be called Stock. Don't change it for now. Change it!
    - there is a global list of Companies as well as a global list of entries which is used by oldest, html_index, and bydate
  - Data
    - I don't know what to call this; something besides data; All, 
    - I this this should contain (companies: Map[String, Company], cashAccounts: Map[String, CashAccount], quotes: Map[String, Quote])
    - might as well throw configuration into it since that is what builds it
- gain
  - ~~currency precision~~
    - ~~mini disaster: it turns out the many buy prices have more than two digits to the right of the decimal point~~
      ~~- this makes it a bad choice for currency. I can't think of why I wouldn't just make it a Typed version of Double~~
      ~~- I still need price for Watch, but it makes even more sense to incorporate Price Watch at that point~~
      ~~- alternatively I could change currency to allow for 5 digits on the right, this sounds a little strange~~
      ~~- I'm not sure what that would do to intuitive constructors. I think we would need a dollar one and a dollar and thousandCents?~~
      ~~- the common case is dollars and cents, maybe factories would be better than constructors~~
      ~~- I've updated all the code, tests, and non-conformant data, and all the stock files now load!~~
  - ~~before I start this I should really get rid of Price. move the mult stuff to Watch where it is actually used.~~
  - I'd like to clean this up a bit before going on
    - ~~can Currency.dollarsCents be made shorter?~~
      - ~~If I use this all over the place we could make the default apply() do this functionality~~
      - ~~I'm not sure if I'd confuse it with decimal.~~
      - ~~Within the Currency code I'd just have to make sure that I use the new keyword.~~
      - ~~while doing this I found and fixed a bug with parsing "1.01"~~
    - ~~move all the Gain calculation into it's own file; GainCalc, GailFunc,~~
    - ~~clean up the code, see if it makes sense to get rid of some of the iteration~~
    - ~~see if the final company value should be gross, gross-sell commisions, net, or something else. Using value as gross-sell commission makes the most sense.~~
    - ~~add cash accounts~~
    - ~~something is wrong with value calculations. look at GOOG.~~
      - ~~It turns the fake sell we create needs shares at the current multiple because the price is at the current multiple~~
    - ~~there'a a problem with the display of pre split purchase prices and shares~~
    - ~~gain 2006 produces no stocks~~
    - ~~2012 dgly~~
      - ~~blows up in python, strange values in scala, it looks like scala is doing the right thing~~
    - ~~2013 MSFT is fucked up in scala~~
      - ~~      MSFT     $30963.81  3.5%      $9255.99          100.0%~~
      - ~~      MSFT        $30,963.81  11.5%     $29,247.81         100.0%~~
      - ~~the trade is being paired with a 1991 buy that should have been long ago sold~~
      - ~~gain 1995:2014 gives similar results in p and s~~
      - ~~it turns out that we incorrectly filtering out the sells too early and not processing them so they consume the necessary buys~~
    - ~~gain 2014 dgly blows up~~
      - ~~I think the problem is that we are trying to maintain integer share counts which is not possible~~
      - ~~we may need to convert all of these to doubles in the sell multiples, do all calcs, and then convert back~~
      - ~~converted to doubles, capital gains seem way worse, some sales are now off in value by a lot~~
        - ~~BSV has a value problem because quantum is too high, I guess it needs a lot of digits~~
        - ~~the bug seems to be that the current report is showing previous sales, fixed~~
    - ~~The annual yield difference is bugging me. I should verify that the scala version is correct.~~
      - ~~It looks like python code does not include commissions in the annual yield~~
      - ~~It is still off but now it's a rounding error~~
      - ~~gain dgly 2014 shows python with an incorrect cost post split~~
    - "gain 2016" shows two stocks with the sum of their capital gains off by a penny. This is due to sub penny values of the cap gains.
      - ~~do we want to truncate some of these currency values after creating them?~~
      - ~~added truncate on Currency and used it on some "final" values~~
    - ~~play around with it a bit and see if I can break it~~
- next research (oldest)
  - ~~implemented simple version of oldest~~
  - next research is mostly within a bash script, figure out how to deploy a scala cl app
    - ~~went through a roller coaster to see how an sbt scala jar can be run outside of sbt. blogged about it~~
    - ~~next research is all handled by a bash script, copying that to the scala project~~
    - ~~need to implement the watching keyword~~
    - ~~I think some stocks without dates are now showing up when they weren't before. I'm not sure what behavior I want.~~
      - ~~MDSO has a mistyped date entry "Oct, 2, 2017" so it has no entries~~
      - ~~in python the file parses as a that has no entries~~
      - ~~oldest then treats no entry as no date and excludes it from the list~~
      - ~~this seems just wrong, not parse error by silently never see it~~
      - ~~the scala behavior of no date entry results in a date that is the oldest seems correct. The result will be me seeing it and adding a correct date.~~
      - ~~no changes need to be made~~
    - ~~AAPL is showing up in the scala watch but not the python~~
      - ~~the stock was watched and then set to none so it has values of none,none,7/1 which doesn't equal the none values~~
      - ~~I think the best solution is to get rid of the None type and have the parse return option None instead of an actual object. This way it won't have a    multiple associated with it which is what I want.~~
      - ~~Actually this doesn't work. Watches are complicated as each of the low and high values are options. I think having a singular none type is the best bet.~~
    - ~~Change dates to have multiple tostring formats. oldest uses fixed length~~
- ~~download quotes~~
  - ~~lets start by moving some classes to packages~~
    - ~~command: BrowseTicker, Command, DownloadQuotes, Gain, Oldest, ~~
    - ~~stock: CashAccount, Currency, Date, Entry, Fraction, Shares, Stock, Trade, Watch~~
    - ~~stocknotes: Config, GainCalc, Quote, Ticker~~
- ~~browse ticker~~
  - ~~mostly done~~
  - ~~sec page is not showing anything~~
- ~~calc~~
  - ~~start buy testing the existing calc, I think it was broken some time ago~~
    - ~~it seems to work fine now~~
  - ~~I have the basic framework setup and have implemented income~~
  - ~~use Vector for args instead of Array~~
  - ~~add unit tests~~
  - ~~run generate until att doesn't change, maybe recursive fn~~
  - ~~conflicts~~
  - ~~add extra way to generate income~~
  - ~~run from gedit, play around~~
  - ~~add help command~~
  - ~~add more generators for attributes~~
- www pages
  - ~~update config to get www creation location~~
    - ~~.stocknotes/~~
    - ~~ config.json~~
    - ~~ www/~~
  - ~~ https ://com-lihaoyi.github.io/scalatags/ ~~
  - www/
    - ~~index.html~~
    - ~~log/<ticker>.txt.html~~
    - ~~buysell.html~~
      - ~~buysell-ticker.html~~
      - ~~buysell-ticker-reverse.html~~
      - ~~buysell-date.html~~
      - ~~buysell-date-reverse.html~~
    - ~~all~~
      - ~~all-ticker.html~~
      - ~~all-ticker-reverse.html~~
      - ~~all-date.html~~
      - ~~all-date-reverse.html~~
  - run on dreamhost
    - ~~hand deploy the jar~~
    - ~~setup symlink(s)~~
    - ~~cron to download quotes followed by generating www~~
    - ~~see if www can be referenced by symlink or has to be copied; maybe need bash script instead of cron~~
    - ~~blog strangeness on dreamhost~~
    - ~~cleanup and checkin change~~
    - ~~change dreamhost cron to run once a day~~
  - ~~dividends on calc are messed up~~
    - ~~entered price 46.24 div 1.28 shows yield and payout ratio of zero~~
    - ~~issue with display~~
- ~~browse-ticker should exit after spawning browser and gedit~~
- ~~review browse-ticker web pages~~
  - ~~definitely add seeking alpha~~
  - ~~current urls~~
    - ~~profile: https://finance.yahoo.com/quote/MSFT/profile?ltr=1 ~~
    - ~~yahoo not hitting anything~~
    - ~~annual income statement: https://finance.yahoo.com/quote/MSFT/financials?ltr=1 ~~
    - ~~news: https://finance.yahoo.com/quote/MSFT/news?ltr=1 ~~
    - ~~chart: https://finance.yahoo.com/chart/MSFT?ltr=1#eyJsYXlvdXQiOnsiaW50ZXJ2YWwiOiJ3ZWVrIiwicGVyaW9kaWNpdHkiOjEsInRpbWVVbml0IjpudWxsLCJjYW5kbGVXaWR0aCI6My41MTM0MDk5NjE2ODU4MjM2LCJmbGlwcGVkIjpmYWxzZSwidm9sdW1lVW5kZXJsYXkiOnRydWUsImFkaiI6dHJ1ZSwiY3Jvc3NoYWlyIjp0cnVlLCJjaGFydFR5cGUiOiJsaW5lIiwiZXh0ZW5kZWQiOmZhbHNlLCJtYXJrZXRTZXNzaW9ucyI6e30sImFnZ3JlZ2F0aW9uVHlwZSI6Im9obGMiLCJjaGFydFNjYWxlIjoibGluZWFyIiwicGFuZWxzIjp7ImNoYXJ0Ijp7InBlcmNlbnQiOjEsImRpc3BsYXkiOiJNU0ZUIiwiY2hhcnROYW1lIjoiY2hhcnQiLCJpbmRleCI6MCwieUF4aXMiOnsibmFtZSI6ImNoYXJ0IiwicG9zaXRpb24iOm51bGx9LCJ5YXhpc0xIUyI6W10sInlheGlzUkhTIjpbImNoYXJ0Iiwi4oCMdm9sIHVuZHLigIwiXX19LCJzZXRTcGFuIjp7Im11bHRpcGxpZXIiOjUsImJhc2UiOiJ5ZWFyIiwicGVyaW9kaWNpdHkiOnsicGVyaW9kIjoxLCJ0aW1lVW5pdCI6IndlZWsifSwiZm9yY2VMb2FkIjp0cnVlfSwib3V0bGllcnMiOmZhbHNlLCJhbmltYXRpb24iOnRydWUsImhlYWRzVXAiOnsic3RhdGljIjp0cnVlLCJkeW5hbWljIjpmYWxzZSwiZmxvYXRpbmciOmZhbHNlfSwibGluZVdpZHRoIjoyLCJmdWxsU2NyZWVuIjp0cnVlLCJzdHJpcGVkQmFja2dyb3VuZCI6dHJ1ZSwiY29sb3IiOiIjMDA4MWYyIiwiZXZlbnRzIjp0cnVlLCJzdHJpcGVkQmFja2dyb3VkIjp0cnVlLCJldmVudE1hcCI6eyJjb3Jwb3JhdGUiOnsiZGl2cyI6dHJ1ZSwic3BsaXRzIjp0cnVlfSwic2lnRGV2Ijp7fX0sInN5bWJvbHMiOlt7InN5bWJvbCI6Ik1TRlQiLCJzeW1ib2xPYmplY3QiOnsic3ltYm9sIjoiTVNGVCIsImV4Y2hhbmdlVGltZVpvbmUiOiJBbWVyaWNhL05ld19Zb3JrIiwicXVvdGVUeXBlIjoiRVFVSVRZIiwibWFya2V0IjoidXNfbWFya2V0In0sInBlcmlvZGljaXR5IjoxLCJpbnRlcnZhbCI6IndlZWsiLCJ0aW1lVW5pdCI6bnVsbCwic2V0U3BhbiI6eyJtdWx0aXBsaWVyIjo1LCJiYXNlIjoieWVhciIsInBlcmlvZGljaXR5Ijp7InBlcmlvZCI6MSwidGltZVVuaXQiOiJ3ZWVrIn0sImZvcmNlTG9hZCI6dHJ1ZX19XSwiY3VzdG9tUmFuZ2UiOm51bGwsInN0dWRpZXMiOnsi4oCMdm9sIHVuZHLigIwiOnsidHlwZSI6InZvbCB1bmRyIiwiaW5wdXRzIjp7IlNlcmllcyI6InNlcmllcyIsImlkIjoi4oCMdm9sIHVuZHLigIwiLCJkaXNwbGF5Ijoi4oCMdm9sIHVuZHLigIwifSwib3V0cHV0cyI6eyJVcCBWb2x1bWUiOiIjMGRiZDZlZWUiLCJEb3duIFZvbHVtZSI6IiNmZjU1NDdlZSJ9LCJwYW5lbCI6ImNoYXJ0IiwicGFyYW1ldGVycyI6eyJjaGFydE5hbWUiOiJjaGFydCIsImVkaXRNb2RlIjp0cnVlLCJwYW5lbE5hbWUiOiJjaGFydCJ9LCJkaXNhYmxlZCI6ZmFsc2V9fX0sImV2ZW50cyI6eyJkaXZzIjp0cnVlLCJzcGxpdHMiOnRydWUsInRyYWRpbmdIb3Jpem9uIjoibm9uZSIsInNpZ0RldkV2ZW50cyI6W119LCJwcmVmZXJlbmNlcyI6e319 ~~
    - ~~analysis: https://finance.yahoo.com/quote/MSFT/analysis?ltr=1 ~~
    - ~~no page found on morningstar: https://www.morningstar.com/content/morningstarcom/en_us/stocks/xnas/MSFT/quote.html ~~
    - ~~edgar: https://www.sec.gov/edgar/browse/?CIK=0000789019&owner=include ~~
  - ~~new urls~~
    - ~~yahoo profile~~
      - ~~ https://finance.yahoo.com/quote/MSFT/profile ~~
    - ~~yahoo financials~~
      - ~~ https://finance.yahoo.com/quote/MSFT/financials ~~
    - ~~seeking alpha~~
      - ~~ https://seekingalpha.com/symbol/MSFT ~~
    - ~~morningstar~~
      - ~~ https://www.morningstar.com/stocks/xnas/msft/quote ~~
    - ~~edgar~~
      - ~~good~~
        - ~~ https://www.sec.gov/edgar/browse/?CIK=0000789019 ~~
      - ~~bad~~
        - ~~ https://www.sec.gov/edgar/searchedgar/companysearch ~~
        - ~~same~~
- ~~can Gain.parseargs be made more intelligent?~~
  - ~~reverse engineering the command line~~
    - -~~omit doesn't work; it's supposed to show keywords but would I ever use this? I think it's worth getting rid of~~
      - ~~I only use keywords on the full stock list~~
    - ~~current~~
      - ~~current report simulates the sale of all stocks at today's date~~
      - ~~previously sold stocks are not shown~~
      - ~~commission must be provided, default 30~~
      - ~~"gain" current report, commision 30~~
      - ~~"gain 30" current report, commission 30~~
    - ~~historical~~
      - ~~show past sales of a given year~~
      - ~~commission is not provided, actual commission are used~~
    - ~~"gain 2012"~~
      - ~~historical report of sales that occurred in 2012~~
    - ~~"gain 2012:2014"~~
      - ~~historical report over year range~~
  - ~~new design~~
    - ~~stock-notes current [ <single ticker> ]~~
      - ~~I don't think there is a use case for multiple tickers~~
      - ~~I also don't think there is a case for different commissions~~
    - ~~stock-notes historical <year1>[:<year2>]~~
  - ~~refactor create() to createCurrent() createHitorical() with correct params~~
- ~~add Dead stocks~~
  - ~~allow stock to load dead~~
  - ~~www generation~~
    - ~~dead log files~~
    - ~~four new dead "all" listing pages~~
- break up Gain report code
  - ~~package name report -> current~~
  - break out into separate class
    - StockReport: createHistorical, createCurrent, render, parseCompanyCurrentValue, parseCompanyDateRange
    - MatchedSell: BuyReadyToSell, parseMatchedSell, 
    - MatchedBuy: completeMatchedBuy, annualYield

# Requirements

Since this is a re-implementation of a previous project I already know what I need to do. Still, this will help me focus on the most important things.

- The program offers a number of command line user entrypoints
  - browse_ticker
  - gain
  - download_quotes
  - next_research
- the format of stock-notes-should be unchanged for this initial port
  - The format of log/<ticker>.txt files
  - The format of cash/<acct>.txt files
  - The format of download/quotes_buysell.csv
  - The format of download/quotes_notify.csv
- html will change for cgi to a pre-rendered directory
  - the use case is to download the quotes followed by rendering the directory
- for all code
  - there will be a test harness
  - I'm not sure what coverage I need to shoot for
  - The code will follow a functional style when appropriate
  - Add decent scaladocs and figure out how to generate them.
- Configuration is needed to point to the stock-notes-data dir as well as tokens.
# RShiny App

`av_runShiny()` runs an interactive RShiny app with a professional
command line interface to download, manage, and visualize data from the
[Alpha Vantage](https://www.alphavantage.co/documentation/) data
service. The app treats equities, ETFs, indices, and FX equally, so
users do not need to know specific API calls to integrate data.
Downloaded data is cached to reduce API calls, all data can be saved for
external use, and both external data and external commands (that call
user functions) can be added to the app. Sets of securities can be
easily added and managed.

## Usage

``` r
av_runShiny()
```

## Value

Nothing

A `ShinyAppHandle` object.

## Details

Invocation starts Shiny application. See vignettes for full details.
**On first use**, In the `AVOPTS` tab, fill in the following fields, and
hit the "Set Opts" button.

- **AV API Key**: API key obtained from [Alpha
  Vantage](https://www.alphavantage.co/documentation/)

- **Entitlement** Entitlement status (either `delayed` or `realtime`)
  Other options that can be set are given below.

**To get data and run analyses:** Enter in the top yellowed line a
semicolon delimited set of securities (Equity, ETF, FX, Crypto, indices)
followed by a command to run analytics on those securities. Commands
without securities are always prefixed by \`"AV.". The results are shown
as tables, plots, or dygraphs below the command line, and possibly in an
additional tab to the right of the main tab. The secondary tab will
change names as approprriate, and the focus may be changed off the MAIN
tab.

Commands are not case sensitive, and may refer to a counterasset in the
second yellowed line. A few examples include:

|  |  |
|----|----|
| Command | Description |
| `AV.H` | List all available functions |
| `AV.INV` | Inventory of all available data |
| `AV.TICKERS` | Inventory of indices and crypto pairs available from [Alpha Vantage](https://www.alphavantage.co/documentation/) |
| `SPACE S` | Search for all tickers with `SPACE` in their names |
| `IBM;QQQ;NDX;USD/MXN GPI` | Produce a rebased time series graph of the securities |
| `IBM;QQQ GV` | Time series graph of rolling volatiliies |
| `ibm;qqq rv` | Relative Value /Active Return Analysis |
| `IBM;AAPL;USD/MXN Q` | Table of latest prices for each instrument |
| `USD/BRL;USD/MXN SCATI` | Scatterplot of rebased levels and returns |
| `IBM;AAPL EA` | Table of Earnings data for each instrument |
| `IBM;AAPL CN` | Table of recent news items for each asset, with links |
| `IBM;AAPL RV` | Graph of excess returns over counterasset (`SPY` by default) |
| `IBM OS F,M,P,otm` | Search for (e.g.) Out of the money front month puts |

If an analysis requires price, dividend or earnings data that is not in
the app's internal store, then it will download it as necessary from
[Alpha Vantage](https://www.alphavantage.co/documentation/). Other data
(e.g. News, options) are downloaded freshly each time. One of the key
contributions of this app is to abstract out the asset-specific
functions calls that are necessary to do cross-asset analyses.

**Other niceties**:

- Groups of assets can be named, saved, and recalled easily. The saved
  name can then be used in lieu of the original list. See [Basic
  Usage](https://derekholmes0.github.io/alphavantagepf/articles/ShinyApp_1_setup_and_usage.html)
  vignette

- Internal data can be stored and used (as `data.table()`s) in a
  directory of the users' choosing. See
  [Options](https://derekholmes0.github.io/alphavantagepf/articles/ShinyApp_2_Graphs_and_Options.html)
  vignette to set the directory, and the
  [Data](https://derekholmes0.github.io/alphavantagepf/articles/ShinyApp_3_Data_Management.html)
  vignette for details on how the data is structured.

- Events and other plotting options can be specified in the app. See
  functions vignette and e.g.
  [FinanceGraphs](https://derekholmes0.github.io/FinanceGraphs/reference/index.html)

- Data within each produced graph or table can be copied to the
  clipboard (option in `AVOPTS` page), See
  [Setup](https://derekholmes0.github.io/alphavantagepf/articles/ShinyApp_1_setup_and_usage.html)
  vignette to see where to turn this on.

- Raw downloaded data can be saved into a "dump directory" (option in
  `AVOPTS` page)

- A current inventory list is always kept in a separate tab, avoiding
  having to run `AV.INV` repeatedly.

- User price data, earnings data, and asset groups can be added using
  helper functions
  [`av_add_px()`](https://derekholmes0.github.io/alphavantagepf/reference/av_add_px.md),
  [`av_add_earn()`](https://derekholmes0.github.io/alphavantagepf/reference/av_add_earn.md),
  [`av_add_assetgroups()`](https://derekholmes0.github.io/alphavantagepf/reference/av_add_assetgroups.md).
  See
  [data](https://derekholmes0.github.io/alphavantagepf/articles/ShinyApp_3_Data_Management.html)
  vignette. Adding data from the console immediately adds the data to
  the app.

- User analytics can be added using
  [`av_add_analytic()`](https://derekholmes0.github.io/alphavantagepf/reference/av_add_analytic.md).
  See examples in the
  [Extensions](https://derekholmes0.github.io/alphavantagepf/articles/ShinyApp_4_New_Functionality.html)
  vignette

## Examples

``` r
if (FALSE) { # \dontrun{
av_runShiny()
} # }
```

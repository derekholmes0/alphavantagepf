# RShiny App

`av_runShiny()` runs an interactive RShiny app with a professional
command line iterface to download, manage, and visualize data from the
[Alpha Vantage](https://www.alphavantage.co/documentation/) data
service. The app treats equities, ETFs, indices, and FX equally, so
users do not need to know specific API calls to integrate data.
Downloaded data is cached to reduce API calls, all data can be saved for
external use, and external data can be added to the app. Sets of
securities can be easily added and managed.

## Usage

``` r
av_runShiny()
```

## Value

Nothing

A `ShinyAppHandle` object.

## Details

Invocation starts Shiny application. See vignette for full details. **On
first use**, click on `AVOPTS` tab, fill in the following fields, and
hit the "Set Opts" button.

- **AV API Key**: API key obtained from [Alpha
  Vantage](https://www.alphavantage.co/documentation/)

- **Entitlement** Entitlement status (either `delayed` or `realtime`)
  Other options that can be set are given below.

**To run analysis**, Enter a semicolon delimited set of securities (e.g.
`SPY;QQQ;DIA`), select a runtime option from the adjacent dropdown, and
press the \`RUN button. Options include:

|  |  |
|----|----|
| Option | Description |
| `Inventory` | Table of what asset lists and price data is available |
| `LivePx` | Live prices of all tickers |
| `NameSearch` | Search for tickers |
| `PriceTS` | Time Series of prices or Total Return indices |
| `ActiveTS` | Active returns of tickers in top row relative to first ticker in bottom row |
| `HistVolTS` | Historic vol and rolling correlations of tickers in top row. |
| `DES`,`News` | Descriptions and News for tickers in top row. |
| `DivEarn` | Dividends, earnings, and earnings estimates for tickers in top row. |
| `OptSearch` | Search for options for tickers in top row. (`OPTS` tab) |

**Options that can be set**. click on `AVOPTS` tab, fill in the
following fields, and hit the "Set Opts" button.

- **fgts colorset**: A named list of colors which can be set using the
  [`FinanceGraphs::fg_update_aes()`](https://derekholmes0.github.io/FinanceGraphs/reference/set_constants.html)
  function.

- **Regr Significance**: p-level to highlight significant regressions.
  (Used in the `HistVolTS` function above.)

- **Cache Directory**: A directory where internal App data can be stored
  (and added to). If not filled in, a temporary directory is created and
  used.

The app allows all downloaded data to be saved. If used, data is saved
in one file, and is stored as a named (by API call function) list of
`data.table`s. Options to control this behavior are

- **AV dump Directory**: A directory where individual API requests can
  be cached. Data is saved in one file as a named (by API call function)
  list of `data.table`s.

- **Capture AV Data**: Save Price data, non-price data, all or none.

- **Update or Cumulative**: If `update` is selected, data is keyed
  relevantly, so for example there will only be one set of dividend data
  stored. If `cumulative` then all API data (including a timestamp) is
  saved sequentially. The latter will lead to much larger files.

- **Other options**:

&nbsp;

- `CleanOnStart` : cleans out the cache file every time the app is
  started.

- `SaveEveryAVCall` : Saves cache file after every call.

- `SaveNow` : Save cache file when this is selected.

## Examples

``` r
if (FALSE) { # \dontrun{
av_runShiny()
} # }
```

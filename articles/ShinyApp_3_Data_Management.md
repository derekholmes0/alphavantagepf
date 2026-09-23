# ShinyApp Data Management

## Data storage overview

One of the key contributions of the
[av_runShiny()](https://derekholmes0.github.io/alphavantagepf/reference/av_runShiny.html)
app is to hide the details of Alphavantage asset-specific calling
conventions. TO the degree possible, the app also caches locally that
data, both to speed up retrieval and to minimize traffic to the API. To
generalize the analyses beyind that Alphavantage data, The app also
allows other **user data** to be added. Economic or sentiment data may
be added, or rates or other company financial data.

The minimal set of data needed consists of a **time series** dataset and
(for equities) **earnings** datasets. For scalability and speed, those
files are kept in [fst](https://www.fstpackage.org/) format. They can
accessed directly (even when the app is running), or via helper
functions described in the next section. The files kept are

| Filename | Location | Description |
|:--:|:--:|:---|
| `avpf_px.fst` | Cache Directory | Raw and adjusted (total rtn) prices, and cash flows |
| `avpf_earn.fst` | Cache Directory | Historical earnings |
| `avpf_earnest.fst` | Cache Directory | Earnings forecasts |
| `avpf_inv.RD` | Cache Directory | Inventory (dates and latest values) file |

In addition, there is a constants file that is always kept in a
system-assigned cache directory. This file (summarized by
[dump_state()](https://derekholmes0.github.io/alphavantagepf/reference/dump_state.html))
contains pointers to the other files as well as downloaded ticker lists
and cached state values.

## Adding new data

Each dataset described above has its own minimal set of columns and
columns that may be zero for many cases. To ease the burden of
determining that, three helpful user functions are included. These can
be used in two ways, either to add new data or to download data from
[AlphaVantage](https://www.alphavantage.co/). Below is a list of data
available from the API and which is downloadable via the
[av_runShiny()](https://derekholmes0.github.io/alphavantagepf/reference/av_runShiny.html)
app and the interface functions described in subsequent sections

| Data Item | Stored in App | Helper Function | Notes |
|:---|:--:|:--:|:---|
| Equity,ETF prices | Y | [av_add_px](https://derekholmes0.github.io/alphavantagepf/reference/av_runShiny_userthings.html) |  |
| Equity Option Prices | N |  | Available per ticker using `OS` function |
| Equity,ETF dividends | Y | [av_add_px](https://derekholmes0.github.io/alphavantagepf/reference/av_runShiny_userthings.html) |  |
| Currency, Crypto prices | Y | [av_add_px](https://derekholmes0.github.io/alphavantagepf/reference/av_runShiny_userthings.html) | Not all Crypto pairs available |
| Equity related Indices | Y | [av_add_px](https://derekholmes0.github.io/alphavantagepf/reference/av_runShiny_userthings.html) | Run `AV.TICKERS` to get list[^1] |
| Equity Earnings | Y | [av_add_earn](https://derekholmes0.github.io/alphavantagepf/reference/av_runShiny_userthings.html) | Kept in `avpf_earn.fst` |
| Equity Earnings Estimates | Y | [av_add_earn](https://derekholmes0.github.io/alphavantagepf/reference/av_runShiny_userthings.html) | Kept in `avpf_earnest.fst` |
| Earnings Call Transcripts | N |  | Available per ticker using `EA` function |
| Company News | N |  | Available per ticker using `CN` function |
| Equity Financials | N |  | Planned for a future release |
| Insider Transactions | N |  | Planned for a future release |
| Commodities | N |  | Planned for a future release |

Any other data you may need can be added as generic (i.e. without
further description) price series.

### Adding Prices and Indices

The function
[av_add_px](https://derekholmes0.github.io/alphavantagepf/reference/av_runShiny_userthings.html)
can add user time series or price data from symbols (via
[av_get_pf()](https://derekholmes0.github.io/alphavantagepf/reference/av_get_pf.html))
would would normally be downloaded from the app.

The function requires at a minimum one of two items:

- An input
  [`data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
  with at least three columns `c(symbol,timestamp,close)` containing the
  series identifier, a date, and a value. \*\*The user has the
  responsibility for ensuring that symbols are unique\*. To avoid
  conflicts, consider decorating your data’s symbols, e.g. `I_CL`
  instead of `CL` for Crude futures. Optionally, other data (usually
  provided automatically from AlphaVantge) associated with intraday
  moves and total return calculations could be added.

| Data types | required? | Column names |
|:--:|:--:|:---|
| Time Series | Y | `c(symbol,timestamp,close)` |
| Intraday | N | `c(open,high,low,volume)` |
| Total Return | N | `c(adjusted_close,dividend_amount,split_coefficient)` |

Suppose we wish to download Natural Gas data from Alphavantage (via
[FRED](https://www.quantmod.com/documentation/getSymbols.FRED.html) )
and give it our own ticker `HH_GAS`. First we download the price series
and get the columns we need. Then we add some basic description,
including most critically the asset type, so the app knows where to get
data going forward.

``` r

require(data.table)
ng_dta <- av_get_pf("","NATURAL_GAS")[,.(symbol="GAS_HH",timestamp,close=value)]
asset_df <- data.frame(symbol=c("GAS_HH"),type=c("user"),currency=c("USD"), name=c("Henry Hub Gas Spot"))
av_add_px(ng_dta, assettypes=asset_df)
```

We can source data anywhere, really. As an example of getting data
directly from [quantmod](https://www.quantmod.com), let’s add FEDFUNDS
as its own ticker:

``` r

suppressMessages(require(quantmod))
ffdta <- as.data.table(quantmod::getSymbols("FEDFUNDS",src="FRED",auto.assign=FALSE))
ffdta <- ffdta[,.(DT_ENTRY=index,close=FEDFUNDS,symbol="FEDFUNDS")]
av_add_px(ffdta)
```

In this case where the `assettypes` argument is not used, the source
(`user`) and symbol (`symbol`) are inferred from the input data.

- A list of Equity, ETF, currency, crypto[^2] or available index[^3]
  symbols. For example,

``` r

av_add_px(equitylist=c("IBM","GS","JPM"))
```

will determine the asset type, download, and inventory the data as would
be done if the data were requested by a command.

### Earnings

Earnings and Earnings estimates are not strictly necessary for many of
the commands, and are kept in separate files. Like the
[`av_add_px()`](https://derekholmes0.github.io/alphavantagepf/reference/av_add_px.md)
function above, either user data can be added or a list of tickers can
be given. However, please note that **price data must always be
downloaded or added before any earnings or estimates data**.

Any of the following will work:

``` r

av_add_earn(equitylist=c("IBM","GS"))

tmp_earn <- av_get_pf("JPM","EARNINGS") |> av_extract_df("quarterlyEarnings")
tmp_earnf<- av_get_pf("JPM","EARNINGS_ESTIMATES") |> av_extract_df("estimates")
av_add_earn(substitute_earn=tmp_earn)
av_add_earn(substitute_earnest=tmp_earnf)


tmp_earn <- av_get_pf("MU","EARNINGS") |> av_extract_df("quarterlyEarnings")
tmp_earnf<- av_get_pf("MU","EARNINGS_ESTIMATES") |> av_extract_df("estimates")
av_add_earn(substitute_earn=tmp_earn, substitute_earnest=tmp_earnf)
```

The advantage of such generality is that you can source price data
anywhere, but not necessarily earnings data.  
Likewise, you may want to do analyses with your own forecasts, instead
of consensus forecasts.

### Asset Groups

Saving sets of asset groups via the app (see
[Usage](https://derekholmes0.github.io/alphavantagepf/articles/ShinyApp_1_setup_and_usage.html)
is to be sure a tedious task. To shortcut that effort, use
[av_add_assetgroups()](https://derekholmes0.github.io/alphavantagepf/reference/av_add_assetgroups.html)
as in the following example:

``` r

newtickers <- c("QQQ","QQQE","NDX")
newweights <- c(0.5,0.2,0.3)
newasset_dt <- data.table(ticker=newtickers,listnm=rep("nasdaq",length(newtickers)), weight=newweights)
av_add_assetgroups(newasset_dt)
dump_assetgroups()
```

If no column `weight` is given weights are assumed equal. This
information is saved for future use with the idea that user defined
indices (as opposed to asset groups) may be useful.

## Data Inventory and retrieval.

Whenever data is added, as inventory information after the addition is
collected. There are three ways to see what is currently in inventory:

- Run `AV.INV` to get all tickers with data downloaded, including
  indices and user data
- Run `AV.EQINV` to get just Equity and ETF tickers.
- Run
  [dump_inv()](https://derekholmes0.github.io/alphavantagepf/reference/av_state_interface.html)
  from the R console.

Also a separate tab INVENTORY is populated on application startup. The
idea is to always have a dictionary what what you have on hand, without
going back and forth between (e.g.) `AV.INV` and your train of thought.

### API call dumping

As described in the options vignette, the app has the ability to save
the results of every API call into the “dump directory” set in the
AVOPTS tab. If a valid directory is named and saved in that page, the
app will append the results of every API call to a file called
`av_download.RD`

This file consists of a list of named (by API call function)
data.tables, each of which contains the results of that call. This is
best illustrated by the following code:

    > load("c:\\t\\av_dump\\av_download.RD",verbose=TRUE)
    Loading objects:
      av_download
      
    > names(av_download)
    [1] "HISTORICAL_OPTIONS"         "TIME_SERIES_DAILY_ADJUSTED" "EARNINGS"                   "EARNINGS_ESTIMATES"        
    > av_download[["EARNINGS"]]
     symbol          variable     ltype            value_df value_str value_num             load_ts
     <char>            <char>    <char>              <list>    <char>     <num>              <POSc>
        BAC    annualEarnings      list  <data.frame[31x2]>      NULL        NA 2026-08-26 14:59:00
        BAC quarterlyEarnings      list <data.frame[122x7]>      NULL        NA 2026-08-26 14:59:00
        BAC            symbol character              [NULL]       BAC        NA 2026-08-26 14:59:00
         GS    annualEarnings      list  <data.frame[27x2]>      NULL        NA 2026-08-26 14:59:01
         GS quarterlyEarnings      list <data.frame[109x7]>      NULL        NA 2026-08-26 14:59:01
         GS            symbol character              [NULL]        GS        NA 2026-08-26 14:59:01
        JPM    annualEarnings      list  <data.frame[31x2]>      NULL        NA 2026-08-26 14:59:02
        JPM quarterlyEarnings      list <data.frame[122x7]>      NULL        NA 2026-08-26 14:59:02
        JPM            symbol character              [NULL]       JPM        NA 2026-08-26 14:59:02

Data stored in the file can either be **cumulative**, which will save
every call with a new timestamp, or as a keyed
[`data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html)
where new results are updated by a relevant key (usually `symbol`) as
necessary. **This file can grow to be quite large** (and hence slow the
app considerably), so consider also enabling the `CleanOnStart` option.
The user may want to periodically remove that file, but that would be
outside the scope of this app.

## Options prices and implied volatilities

Sadly, Alphavantage does not provide any implied volatility time series
data. They do provide a full snapshot of option implieds for a given
ticker and a given date. This amounts to a *lot* of data, but once it’s
downloaded it can be summarized and analyzed at will. Since your ability
to download that data will depend on your data subscription level,
collecting this data will take longer than is appropriate for an
interactive tool. For example, at 75 requests/minute (the starter level
for paid subscriptions), downloading 10 years of daily options data for
a single ticker will take 10\*252/75 = 33.6 minutes (and in the case of
`QQQ`, adds up to 700 MB). The key implications of this are twofold:

- **Downloading and summarizing that data for interactive use must be
  done outside the app.**
- **Large sets options and derived term structures will add greately to
  the space required and the time to load internal data.**

Included in the app is a helper function
[av_add_options](https://derekholmes0.github.io/alphavantagepf/reference/av_add_options.html)
to download and manage the data. That much data is a stretch for a
single `.fst` file, so the app/function stores the data in a **partioned
parquet** format within a subdirectoy of the main cache directory. The
function also summarizes the term structure of the data into a separate
`.fst` file for quick retrieval and interactive use.

Options data can be downloaded for an arbitrary list of tickers and a
given date range, and (unless specified) will only download data it
doesn’t already have. To save on time and space, weekly or monthly data
can be downloaded instead of daily. Even so, it will take close to 6
hours to download 10 year of weekly data for 50 tickers.

If the data and implied vol summaries are there, the app will refer to
them as necessary. If that data hasn’t been downloaded, the app will
politely decline to work and return a message as such. To reiterate,
other than the `OS` function, the app **will not download any data
live**. If you want live data, use the
[av_add_options](https://derekholmes0.github.io/alphavantagepf/reference/av_add_options.html)
via an external call. Note that this can still be done when the Shiny
app is still running.

### Getting started

Assuming the app is up and working with valid API keys, the data can be
downloaded using, e.g.

\`\`\` blah=sapply(c(“IBIT”,“IBM”,“CSCO”,“ORCL”), (x)
av_add_options(“update”,dtstr=“-10y::”,symbols=x,freq=“w”) ) Option data
to get:IBIT from 2016-09-09 to 2026-07-31 (476 days) AV Options for IBIT
\[———-\] 4% \[ 6s\] vs 6.61 mins maxav_get_pf: Pacing 0.83 second(s). AV
Options for IBIT \[———-\] 4% \[ 8s\] vs 6.61 mins maxav_get_pf: Pacing
0.83 second(s). … No data for symbol IBIT on date 2024-11-15. Please
specify a valid combination of symbol and trading day. AV_optchain( IBIT
/ 2024-11-15 ) err: No data for symbol IBIT on date 2024-11-15. Please
specify a valid combination of symbol and trading day. AV Options for
IBIT \[\>———\] 11% \[35s\] vs 6.61 mins maxNo data for symbol IBIT on
date 2024-11-08. Please specify a valid combination of symbol and
trading day. AV_optchain( IBIT / 2024-11-08 ) err: No data for symbol
IBIT on date 2024-11-08. Please specify a valid combination of symbol
and trading day. No data for symbol IBIT on date 2024-11-01. Please
specify a valid combination of symbol and trading day. AV_optchain( IBIT
/ 2024-11-01 ) err: No data for symbol IBIT on date 2024-11-01. Please
specify a valid combination of symbol and trading day. AV Options for
IBIT \[\>———\] 11% \[36s\] vs 6.61 mins maxNo data for symbol IBIT on
date 2024-10-25. Please specify a valid combination of symbol and
trading day. AV_optchain( IBIT / 2024-10-25 ) err: No data for symbol
IBIT on date 2024-10-25. Please specify a valid combination of symbol
and trading day. AV Options for IBIT \[\>———\] 11% \[37s\] vs 6.61 mins
maxmange_optdb_arrow: IBIT has 4 conseq days with no options, skipping
the rest Option Symbol: IBIT gathered in :37.87 Option update: Adding
114396 rows to partitioned parquet set Returned 114396 new options,
refreshing inventory, took :1.17 …

Note that

- The function gives a lot of information about times taken and ETAs. It
  utilizes progress bars and publishes a message when API pacing starts
  or is in effect.
- The function downloads backwards in time, and if more than 4
  consequtive empty days are detected, it stops downloaded that ticker.
- The function gives updates on the sizes of data downloaded.
- The function recalculates term structures at the end.

### Data structures created

Within a subdirectory of the cache directory called `eqopt`, a
partitioned parquet data structure is kept with the results of all
option downloads. Each symbol will have its own subdirectory,
(e.g. `symbol=SPY`) with a parquet file with the following columns

| Column Set | Description | Columns |
|:--:|:--:|:---|
| Keys | `symbol,ts,contractid` | Closing option values for option `contractid` on symbol `symbol` for day `ts` |
| Contract details | `expiration,type,strike,dtoexp` | Each contract valuation details |
| Helpful details | `expcode,spot,ITM` | Derived and added valution data. Expcodes are of the form (e.g.) `mo_2` for 2nd monthly contract |
| Pricing Data | `last,mark,bid,bid_size,ask,ask_size,volume,open_interest` | Market quotes |
| Derived Data | `iv,delta,gamma,vega,theta,rho` | Black Scholes derived data |

From that data, another
[`data.table()`](https://rdrr.io/pkg/data.table/man/data.table.html) is
created for Shiny app use with quotes from above narrowed to a select
set of strikes. For each symbol an day, the table `eqopt_iv` will have
quotes for both calls and puts closest to 5, 10, 25, 50, 77 and 90
deltas. (Those codes are added as the variable `moneyn`.)

[^1]: Alphavantage has a small select list of CBOE, VIX and equity
    futures indices available as historical data, listed by running
    `AV.TICKERS`

[^2]: Alphavantage has a small select list of CBOE, VIX and equity
    futures indices available as historical data, listed by running
    `AV.TICKERS`

[^3]: Alphavantage has a small select list of CBOE, VIX and equity
    futures indices available as historical data, listed by running
    `AV.TICKERS`

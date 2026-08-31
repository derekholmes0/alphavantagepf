# Add or download Price or Time Series Data

Adds price data to
[`av_runShiny()`](https://derekholmes0.github.io/alphavantagepf/reference/av_runShiny.md)
internal data.

## Usage

``` r
av_add_px(
  indta = NULL,
  assettypes = NULL,
  equitylist = NULL,
  dtstr = "-30y::",
  delay = 0
)
```

## Arguments

- indta:

  (default: NULL) A data.frame with the following minimal columns:
  `c(symbol,timestamp,close)`. Other variables added could be
  `c(adjusted_close,open,high,low,volume,dividend_amount,split_coefficient)`
  If `adjusted_close` is not in the dataset, it will be set to `close`

- assettypes:

  (default NULL) An optional data.frame with minimal columns
  `c(symbol,type,currency,name)` with descriptive data for the assets
  given in `indta`. If not specified, a call to
  `av_get_pf(.,"SYMBOL_SEARCH")` is necessary to determine the asset
  type (one of `c("Equity","ETF","FX","Index","Crypto")`) for subsequent
  calls to
  [`av_get_pf()`](https://derekholmes0.github.io/alphavantagepf/reference/av_get_pf.md)

- equitylist:

  (default NULL) If specified, function will get equity prices from
  `av_get_pf`. `indta` can be null or is otherwise ignored.

- dtstr:

  (default `"-30y::"`). Date range to download if applicable.

- delay:

  (default 0) Seconds to delay calls to determine asset type for future
  AV downloads. This is unused if `assettypes` is given.

## Value

Nothing

## Details

Add Price or Time Series Data

Entire set of columns from
[`av_get_pf()`](https://derekholmes0.github.io/alphavantagepf/reference/av_get_pf.md)
can be added. First date column renamed to `timestamp` internally.

## See also

[`av_runShiny()`](https://derekholmes0.github.io/alphavantagepf/reference/av_runShiny.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# To add known symbols outside the app
av_load_shinydata()  # Make sure most recent data is loaded
av_add_px(equitylist=c("IBM","GS","JPM"))

# To add ad-hoc data from Alphavantage (e.g. Natgas spot at Henry Hub)
# Note that "symbol" in indta must match same in assettypes
asset_df <- data.frame(symbol=c("GAS_HH"),type=c("user"),currency=c("USD"), name=c("GAS_HH"))
ng_data <- av_get_pf("","NATURAL_GAS")[,.(symbol="GAS_HH",timestamp,close=value)]
av_add_px(ng_data, assettypes=asset_df)

# To data from other sources
suppressMessages(require(quantmod))
ffdta <- as.data.table(quantmod::getSymbols("FEDFUNDS",src="FRED",auto.assign=FALSE))
ffdta <- ffdta[,.(DT_ENTRY=index,close=FEDFUNDS,adjusted_close=FEDFUNDS,symbol="FEDFUNDS")]
av_add_px(ffdta)
} # }
```

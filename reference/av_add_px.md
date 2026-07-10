# App database functions

Adds price data to
[`av_runShiny()`](https://derekholmes0.github.io/alphavantagepf/reference/av_runShiny.md)
internal data.

## Usage

``` r
av_add_px(indta, assettypes = NULL, delay = 0)
```

## Arguments

- indta:

  A data.frame with the following minimal columns:
  `c(symbol,timestamp,close,adjusted_close)`. Other variables added
  could be `c(open,high,low,volume,dividend_amount,split_coefficient)`

- assettypes:

  (default NULL) An optional data.frame with minimal columns
  `c(symbol,type,currency,name)` with descriptive data for the assets
  given in `indta`. If not specified, a call to
  `av_get_pf(.,"SYMBOL_SEARCH")` is necessary to determine the asset
  type (one of `c("Equity","ETF","FX","Index","Crypto")`) for subsequent
  calls to
  [`av_get_pf()`](https://derekholmes0.github.io/alphavantagepf/reference/av_get_pf.md)

- delay:

  (default 0) Seconds to delay calls to determine asset type for future
  AV downloads. This is unused if `assettypes` is given.

## Value

Nothing

## Details

Entire set of columns from
[`av_get_pf()`](https://derekholmes0.github.io/alphavantagepf/reference/av_get_pf.md)
can be added. First date column renamed to `timestamp`

## See also

[`av_runShiny()`](https://derekholmes0.github.io/alphavantagepf/reference/av_runShiny.md)

## Examples

``` r
if (FALSE) { # \dontrun{
av_add_px(av_get_pf("IBM","TIME_SERIES_DAILY_ADJUSTED"))
asset_df <- data.frame(symbol=c("HYG"),type=c("ETF"),currency=c("USD"), name=c("HY ETF"))
av_add_px(av_get_pf("HYG","TIME_SERIES_DAILY_ADJUSTED"), assettypes=asset_df)

suppressMessages(require(quantmod))
ffdta <- as.data.table(quantmod::getSymbols("FEDFUNDS",src="FRED",auto.assign=FALSE))
ffdta <- ffdta[,.(DT_ENTRY=index,close=FEDFUNDS,adjusted_close=FEDFUNDS,symbol="FEDFUNDS")]
av_add_px(ffdta)
} # }
```

# Load cached options data previously downloaded using av_add_options()

Returns a data.table of options data given a data.table of keys desired
or a set of parameters.

## Usage

``` r
av_load_cached_opts(optsDT = NULL, symbols = NULL, dtstr = "")
```

## Arguments

- optsDT:

  (default: NULL) A data.frame of symbols and optional keys (ts,
  expiration, expcode, type) to join with data.

- symbols:

  (default: NULL) Ticker or list of tickers for which option data is
  returned.

- dtstr:

  (default: "") Dates for which to return options. Works with both
  optsDT and symbols specfication.

## Value

data.table with full option data.

## Details

Load Options data from parquet files

## See also

[`av_runShiny()`](https://derekholmes0.github.io/alphavantagepf/reference/av_runShiny.md)

## Examples

``` r
if (FALSE) { # \dontrun{
opttest1=av_load_cached_opts(symbols="JPM;GS",dtstr="-2m::")
opttest2=av_load_cached_opts(optsDT=CJ(symbol=c("JPM","GS"), expcode=c("MO_1","MO_2"), type=c("call")), dtstr="-1y::")
opttest2[,.(min(ts),max(ts)), by=.(expcode,symbol,type)]
} # }
```

# Get financial data from the Alpha Vantage API

Interface to alphavantage API.

## Usage

``` r
av_get_pf(
  symbol,
  av_fun,
  symbolvarnm = "symbol",
  dfonerror = TRUE,
  melted = "default",
  delay = 0,
  verbose = FALSE,
  ...
)
```

## Arguments

- symbol:

  A character string of an appropriate stock, fund, or currency See
  parameter "symbol" in [Alpha Vantage API
  documentation](https://www.alphavantage.co/documentation/).

- av_fun:

  A character string matching an appropriate Alpha Vantage "function".
  See parameter "function" in [Alpha Vantage API
  documentation](https://www.alphavantage.co/documentation/).

- symbolvarnm:

  (default: `symbol`) Variable name which has the `symbol` requested.
  Set to a blank string if not wanted.

- dfonerror:

  (default: TRUE) Return an empty data.table when any error occurs

- melted:

  (default: "default") String specifying when to melt, "default" is
  chosen by the package, "TRUE\|always" always melt, "FALSE\|never"
  never melts

- delay:

  (default: 0) Delay in seconds after API call, used to embed within
  large single-symbol calls.

- verbose:

  (default: FALSE) Print debug information helpful for errors. Also
  copies full url to clipboard.

- ...:

  Additional parameters or overrides passed to the Alpha Vantage API.
  For a list of parameters, visit the [Alpha Vantage API
  documentation](https://www.alphavantage.co/documentation/).

## Value

Returns a data.table with results dependent on the function called.
Mixed data is returned as a melted data.table, possibly with nested
data.frames. Time series are returned as data.tables.

## Details

**The `av_fun` argument replaces the API parameter “function”** because
function is a reserved name in R. All other arguments match the Alpha
Vantage API parameters.

**There is no need to specify the `apikey`, `datatype`, `outputsize` or
`entitlement` parameters** as arguments to av_get_pf(). Before using,
you must set the API key using av_api_key("YOUR_API_KEY"). `outputsize`
defaults to "full" unless overridden with "compact in `...`."

**Required parameters other than `symbol`** must be passed as named
arguments via `...`.

**Optional parameters have defaults** which can be obtained by calling
[`av_funhelp()`](https://derekholmes0.github.io/alphavantagepf/reference/av_funhelp.md)
and overridden via `...`.

**There is no need to specify the datatype parameter** as an argument to
`av_get_pf()`. The function will return a data.table.

**ForEx "FROM/TO" symbol details.** FOREX symbols in the `av_get_pf()`
function are supplied in `"FROM/TO"` format, which are then parsed in
the Alpha Vantage API into `from_currency` and `to_currency` API
parameters. Usage example: `av_get_pf("USD/BRL", "FX_DAILY")`

## See also

[`av_api_key()`](https://derekholmes0.github.io/alphavantagepf/reference/av_api_key.md),
[`av_extract_df()`](https://derekholmes0.github.io/alphavantagepf/reference/av_extract_df.md),
[`av_extract_fx()`](https://derekholmes0.github.io/alphavantagepf/reference/av_extract_df.md),
[`av_grep_opts()`](https://derekholmes0.github.io/alphavantagepf/reference/av_grep_opts.md),[`av_funhelp()`](https://derekholmes0.github.io/alphavantagepf/reference/av_funhelp.md)

## Examples

``` r
if (FALSE) { # \dontrun{
av_api_key("YOUR_API_KEY")
av_api_key("YOUR_API_KEY","delayed") # if you have such access

# example code

# ---- 1.0 SINGLE NAME EQUITY SUMMARY INFORMATION AND SEARCH ----

av_get_pf("IBM","OVERVIEW") |> str()
av_get_pf("EWZ","ETF_PROFILE")
av_get_pf("EWZ","ETF_PROFILE") |> av_extract_df("holdings")
av_get_pf("","SYMBOL_SEARCH",keywords="COMMERCE")

# ---- 2.0 MARKET QUOTES  ----

av_get_pf("IBM","GLOBAL_QUOTE")
av_get_pf("USD/BRL","CURRENCY_EXCHANGE_RATE") |> av_extract_fx()
av_get_pf(c("ORCL","IBM","EWZ","ARGT"),"REALTIME_BULK_QUOTES",melt=FALSE)
# Note you need advanced permissioning for REALTIME_BULK_QUOTES

# ---- 3.0 SINGLE NAME HISTORICAL DATA  ----

av_get_pf("IBM","TIME_SERIES_DAILY")
av_get_pf("IBM","TIME_SERIES_INTRADAY")

# ---- 4.0 MARKET PRICING DATA  ----

av_get_pf("","MARKET_STATUS")  |> av_extract_df()
av_get_pf("","TOP_GAINERS_LOSERS") |> av_extract_df("top_losers")
av_get_pf("","TREASURY_YIELD",maturity='7year')

# ---- 4.0 SINGLE NAME NON-PRICING DATA  ----

av_get_pf("IBM","DIVIDENDS")
av_get_pf("IBM","EARNINGS")  |> av_extract_df("quarter",melt=TRUE)
av_get_pf("IBM","NEWS_SENTIMENT") |> av_extract_df("feed")

av_get_pf("IBM","EARNINGS_CALL_TRANSCRIPT",quarter="2024Q3")  |> av_extract_df("transcript")
 # Note that quarter is a required parameter, not specifying will throw an error

 # ---- 5.0 SINGLE NAME OPTION PRICING DATA  ----

av_get_pf("IBM","HISTORICAL_OPTIONS") |> av_grep_opts("F,M,put",mindays=2)

# ---- 6.0 TECHNICAL INDICATORS  ----

av_funhelp("SMA")  # Shows parameters and defaults chosen by this package.
av_get_pf("IBM","SMA",time_period=20)

# ---- 7.0 WINDOW ANALYTICS  ----

av_get_pf(c("ORCL","IBM","EWZ","ARGT"),"ANALYTICS_FIXED_WINDOW",verbose=TRUE) |>
            av_extract_analytics(separate_vars=TRUE)
} # }
```

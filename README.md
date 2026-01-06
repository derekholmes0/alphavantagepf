
<!-- README.md is generated from README.Rmd. Please edit that file -->

# alphavantagepf

<!-- badges: start -->

<!-- badges: end -->

A [data.table](https://github.com/Rdatatable/data.table) centric R
interface to the Alpha Vantage API, geared towards personal finance
applications. Data is typically returned in “melted” or normalized forms

## Installation

You can install the development version of alphavantagepf from

``` r
# install.packages("pak")
pak::pak("derekholmes0/alphavantagepf")
```

Load the package.

``` r
library(alphavantagepf)
```

Set your API key obtained from [Alpha
Vantage](https://www.alphavantage.co/).

``` r
av_api_key("YOUR_API_KEY")
print(av_api_key())
#> [1] "YOUR_API_KEY" NA
```

If you have paid access, include an additional argument with your
entitlement status, e.g.

``` r
av_api_key("YOUR_API_KEY","delayed")
print(av_api_key())
#> [1] "YOUR_API_KEY" "delayed"
```

## Finding Functions and their defaults

To find parameters and defaults provided by the `alphavantagepf`
package, use `av_funhelp()` and refer to the Alphavantage API calling
conventions in [Alphavantage API
documentation](https://www.alphavantage.co/documentation/)

``` r
av_funhelp("SERIES_INTRADAY")
#> Function: TIME_SERIES_INTRADAY
#> Category: equity
#> 
#> Parameters:
#> R> symbol
#> R> interval (default: 15min)
#> O> adjusted
#> O> extended_hours
#> O> month
#> O> outputsize (default: compact)
#> O> entitlement (default: {entitlement})
#> NULL
```

Required parameters are listed with “R” and optional parameters (and any
default provided by this package) are listed with “O”

## Getting Data from Alpha Vantage

Once the API key has been set, use the function `av_get_pf()` which
requires at minimum two arguments, a `symbol` (put first to facilitate
usage in pipes) and an Alphavantage “function” `av_fun`.

``` r
av_get_pf("IBM","TIME_SERIES_INTRADAY") |> head()

   symbol           timestamp  open  high   low close volume
   <char>              <POSc> <num> <num> <num> <num>  <int>
1:    IBM 2026-01-02 11:00:00   292   293   291   292 141979
2:    IBM 2026-01-02 11:15:00   293   293   292   292 117760
3:    IBM 2026-01-02 11:30:00   292   292   291   291  67820
4:    IBM 2026-01-02 11:45:00   291   291   291   291  92434
5:    IBM 2026-01-02 12:00:00   291   291   290   291 128306
6:    IBM 2026-01-02 12:15:00   291   292   291   291 185174
```

Note that data is returned in a `data.table`, which can be cast as
tibbles as necessary.

## More complex data and helpful data extractors.

Some API calls return more complex data, i.e. data with strings,
numbers, and nested data.frames collected together. By defauly,
`av_get_pf()` returns everything the API returns, but in a long form
with columns for returned variables and each data type. For example, the
`TOP_GAINERS_LOSERS` function returns separate data.frames for each
category.

``` r
av_get_pf("","TOP_GAINERS_LOSERS")

Key: <variable>
               symbol             variable     ltype           value_df                                value_str value_num
               <char>               <char>    <char>             <list>                                   <char>     <num>
1: TOP_GAINERS_LOSERS         last_updated   numeric             [NULL]           2026-01-05 16:15:59 US/Eastern      2026
2: TOP_GAINERS_LOSERS             metadata character             [NULL] Top gainers, losers, and most actively t        NA
3: TOP_GAINERS_LOSERS most_actively_traded      list <data.frame[20x5]>                                     NULL        NA
4: TOP_GAINERS_LOSERS          top_gainers      list <data.frame[20x5]>                                     NULL        NA
5: TOP_GAINERS_LOSERS           top_losers      list <data.frame[20x5]>                                     NULL        NA
```

- Note that symbol for `TOP_GAINERS_LOSERS` isn’t used, but still needs
  to be specified.
- Note that the data returned is in long form, and always has at least
  three columns, `symbol` (which cna be changed in `av_get_pf()`
  optional parameters). `variable` which is the name of the data item
  returned, and (e.g.) `value_str` or `value_df` with appropriate data
  components. The data is separated out by type so further delisting
  doesn’t have to be done after the call.

If you want to just get, e.g. the top losers, the returned data can be
piped into the `av_extract_df()` function:

``` r
av_get_pf("","TOP_GAINERS_LOSERS") |> av_extract_df("top_losers")

    <char>   <num>         <num>            <char>     <num>             <char>
 1:    OCG  0.0378       -0.0654         -63.3721% 216078762 TOP_GAINERS_LOSERS
 2:   ZBIO 16.6100      -17.8900         -51.8551%   8034469 TOP_GAINERS_LOSERS
 3:    SGN  0.4627       -0.4873         -51.2947%   2115079 TOP_GAINERS_LOSERS
 4:  HYT^#  0.0186       -0.0164         -46.8571%    126059 TOP_GAINERS_LOSERS
 5:  LVROW  0.0122       -0.0079         -39.3035%     10967 TOP_GAINERS_LOSERS
```

## Other extracting helper examples

### Foreign exchange

The returned data from the `CURRENCY_EXCHANGE_RATE` is a bit complex,
and can be simplified with

``` r
# REAL-TIME QUOTE
av_get_pf("USD/BRL","CURRENCY_EXCHANGE_RATE") |> av_extract_fx()

Key: <symbol>
    symbol   Ask   Bid      QuoteTimestamp   Mid
    <char> <num> <num>              <POSc> <num>
1: USD/BRL  5.37  5.37 2026-01-06 15:47:46  5.37
```

## Options

The `HISTORICAL_OPTIONS` function returns a large set of options for any
given ticker, many of which are long dated or have no opent interest.
The `av_grep_opts()` helper can be used to narrow those down using a
comma-separated string specifying

1.  How far out maturities should be returned, e.g. Front Month (F) or
    Back month (B) or all (A)

2.  What expiration schedules should be used, e.g. (Q) for Quarterly
    or (M) for monthlies.

3.  “Call”, “Put” or “all”

So, for example, to get the closest monthly puts with at least 2 days to
maturity, use the tring

``` r
av_get_pf("IBM","HISTORICAL_OPTIONS") |> av_grep_opts("F,M,put",mindays=2)

   symbol         contractID expiration strike   type  last  mark   bid bid_size   ask ask_size volume open_interest       date implied_volatility  delta  gamma  theta   vega     rho
   <char>             <char>     <IDat>  <num> <char> <num> <num> <num>    <int> <num>    <int>  <int>         <int>     <IDat>              <num>  <num>  <num>  <num>  <num>   <num>
1:    IBM IBM260116P00277500 2026-01-16    278    put  0.00  0.80  0.67      158  0.94      254      0            33 2026-01-05              0.298 -0.110 0.0123 -0.127 0.0961 -0.0100
2:    IBM IBM260116P00280000 2026-01-16    280    put  0.98  0.96  0.90      180  1.02       10    111          1443 2026-01-05              0.278 -0.130 0.0149 -0.133 0.1085 -0.0119
3:    IBM IBM260116P00282500 2026-01-16    282    put  1.32  1.29  1.17      213  1.41      158     14           272 2026-01-05              0.269 -0.165 0.0181 -0.150 0.1272 -0.0150
```

## Important Notes: av_get_pf()

1.  Three parameters `apikey`, `datatype` and `outputsize` are filled
    into the API call from the package. `outputsize` defaults to the
    full dataset, and can be overridden as a named parameter to the
    `av_get_pf()` call.

2.  An additional parameter `entitlement` is added to the url if
    specified in the `av_api_key()` call and relevant.

3.  `symbol` is always returned in the output dataset, and defaults to
    the name of the `av_fun` call if no symbol is relevant.

4.  There is no need to specify the `datatype` parameter as an argument
    to `av_get()`. The function will return a data.table

5.  Multiple symbols in one API call are currently unsupported.

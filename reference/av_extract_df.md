# Extract data from Alpha Vantage returned data

`av_extract_df()` pulls out nested data.frames from mixed data returned
by
[`av_get_pf()`](https://derekholmes0.github.io/alphavantagepf/reference/av_get_pf.md)
`av_extract_fx()` returns a simplified FX quote in data.table formfrom
[`av_get_pf()`](https://derekholmes0.github.io/alphavantagepf/reference/av_get_pf.md)
calls. `av_extract_analytics()` returns melted data.table from calls to
`av_get_pf("ANALYTICS_FIXED_WINDOW")` or
`av_get_pf("ANALYTICS_SLIDING_WINDOW")` `av_extract_divs_or_splits()`
returns melted data.table from calls to `av_get_pf("DIVIDENDS")` or
`av_get_pf("SPLITS")`

## Usage

``` r
av_extract_df(indta, grepstring = "", melt = FALSE, empty_dt_onerror = TRUE)

av_extract_fx(indta, outputform = "common", cols = "")

av_extract_analytics(indta, separate_vars = FALSE)

av_extract_divs_or_splits(indta)
```

## Arguments

- indta:

  A data.table as returned by av_get()

- grepstring:

  select which variable (data item) to unnest in data.table returned
  from av_get_pf

- melt:

  Return data in melted/normalized form

- empty_dt_onerror:

  (default : TRUE): Return gracefully an empty data.table if requested
  item is not present.

- outputform:

  (default : `common`, `av_extract_fx()` only): Use common names from
  `REALTIME_BULK_QUOTES`

- cols:

  (default : all columns: `av_extract_fx()` only): String or List of
  columns to return \`

- separate_vars:

  (default : FALSE) separate out multiple levels of variable names into
  new keys

## Value

Extracted data.tables for nested data returned from
[`av_get_pf()`](https://derekholmes0.github.io/alphavantagepf/reference/av_get_pf.md),
If `grepstring` is not specified, first nested table is returned.
`av_extract_fx()` returns a shortened data.table with FX quotes.

## Details

[`av_get_pf()`](https://derekholmes0.github.io/alphavantagepf/reference/av_get_pf.md)
frequently returns a nested data.table, or a structure with nested
data.frames. These are utilities functions to extract, filter and
summarize returned values.

## See also

[`av_get_pf()`](https://derekholmes0.github.io/alphavantagepf/reference/av_get_pf.md),
[`av_grep_opts()`](https://derekholmes0.github.io/alphavantagepf/reference/av_grep_opts.md)

## Examples

``` r
if (FALSE) { # \dontrun{
av_get_pf("","MARKET_STATUS")  |> av_extract_df()
av_get_pf("","TOP_GAINERS_LOSERS") |> av_extract_df("top_losers")
av_get_pf("USD/BRL","CURRENCY_EXCHANGE_RATE") |> av_extract_fx()
av_get_pf(c("ORCL","IBM"),"ANALYTICS_FIXED_WINDOW") |> av_extract_analytics(separate_vars=TRUE)
} # }
```

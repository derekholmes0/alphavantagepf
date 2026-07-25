# App database functions: Earnings

Adds earnings data to
[`av_runShiny()`](https://derekholmes0.github.io/alphavantagepf/reference/av_runShiny.md)
internal data, either by download or with user data

## Usage

``` r
av_add_earn(
  substitute_earn = NULL,
  substitute_earnest = NULL,
  assettypes = NULL,
  delay = 0
)
```

## Arguments

- substitute_earn:

  A (default NULL) data.frame with past earnings

- substitute_earnest:

  (default NULL) A data.frame with earnings estimates

- assettypes:

  (default NULL) An optional data.frame with minimal columns
  `c(symbol,type,currency,name)` with descriptive data for the assets
  given in `indta`. If not specified, a call to
  `av_get_pf(.,"SYMBOL_SEARCH")` is necessary to determine the asset
  type (one of `c("Equity","ETF","FX","Index","Crypto")`) for subsequent
  calls to
  [`av_get_pf()`](https://derekholmes0.github.io/alphavantagepf/reference/av_get_pf.md)
  NOTE THAT assettypes cannot be NULL if substitute_earn and
  substitute_earnest are NULL.

- delay:

  (default 0) Seconds to delay calls to determine asset type for future
  AV downloads. This is unused if `assettypes` is given.

## Value

Data.table with downloaded or added earnings

## Details

Entire set of columns from
[`av_get_pf()`](https://derekholmes0.github.io/alphavantagepf/reference/av_get_pf.md)
can be added. First date column renamed to `timestamp`. If just
assetypes is given, the function downloads earnings as needed
(respecting maximum age parameters defined in the app's `AVOPTS` tab.)

## See also

[`av_runShiny()`](https://derekholmes0.github.io/alphavantagepf/reference/av_runShiny.md)

## Examples

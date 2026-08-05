# av_add_earn

Adds earnings data to
[`av_runShiny()`](https://derekholmes0.github.io/alphavantagepf/reference/av_runShiny.md)
internal data, either by download or with user data

## Usage

``` r
av_add_earn(
  substitute_earn = NULL,
  substitute_earnest = NULL,
  equitylist = NULL,
  delay = 0
)
```

## Arguments

- substitute_earn:

  A (default NULL) data.frame with past earnings

- substitute_earnest:

  (default NULL) A data.frame with earnings estimates

- equitylist:

  (default NULL) A list with tickers for which to retrieve earnings
  (from AlphaVantage)

- delay:

  (default 0) A numeric value specifying delay between Alphavantage
  calls (in seconds)

## Value

Data.table with summary of downloaded or added earnings

## Details

Entire set of columns from
[`av_get_pf()`](https://derekholmes0.github.io/alphavantagepf/reference/av_get_pf.md)
can be added. First date column renamed to `timestamp`. If just
assetypes is given, the function downloads earnings as needed
(respecting maximum age parameters defined in the app's `AVOPTS` tab.)
**Note that price data must always be added first**

## See also

[`av_runShiny()`](https://derekholmes0.github.io/alphavantagepf/reference/av_runShiny.md)

## Examples

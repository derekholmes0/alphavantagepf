# Extract data from Alpha Vantage retuned data

Extract data from Alpha Vantage retuned data

## Usage

``` r
av_funhelp(av_fun_grep = "", verbose = TRUE)
```

## Arguments

- av_fun_grep:

  A Alpha Vantage function name or portions of one. For a list of
  parameters, visit the [Alpha Vantage API
  documentation](https://www.alphavantage.co/documentation/).

- verbose:

  (Default: TRUE) Prints the help string

## Value

Help Text and default parameters.

## Details

Returns defaults and parameter lists for Alphavantage functions

## See also

[`av_get_pf()`](https://derekholmes0.github.io/alphavantagepf/reference/av_get_pf.md)

## Examples

``` r
av_funhelp("GLOBAL_QUOTE")
#> Function: GLOBAL_QUOTE
#> Category: equity
#> 
#> Parameters:
#> R> symbol
#> O> entitlement (default: {entitlement})
#> [1] "Function: GLOBAL_QUOTE\nCategory: equity\n\nParameters:\nR> symbol\nO> entitlement (default: {entitlement})\n"
```

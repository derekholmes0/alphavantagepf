# Extract data from Alpha Vantage retuned data

Extract data from Alpha Vantage retuned data

## Usage

``` r
av_reset_defaults(fileopts = TRUE, keep_apikeys = FALSE)
```

## Arguments

- fileopts:

  (default: TRUE) If TRUE, then remove all files and subdirectories

## Value

No return

## Details

Resets
[`av_runShiny()`](https://derekholmes0.github.io/alphavantagepf/reference/av_runShiny.md)
defaults to original (newly installed) state

## See also

[`av_runShiny()`](https://derekholmes0.github.io/alphavantagepf/reference/av_runShiny.md)

## Examples

``` r
if (FALSE) { # \dontrun{
av_reset_defaults()
} # }
```

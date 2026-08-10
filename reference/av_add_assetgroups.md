# Add asset lists

Adds asset lists to
[`av_runShiny()`](https://derekholmes0.github.io/alphavantagepf/reference/av_runShiny.md)
internal data.

## Usage

``` r
av_add_assetgroups(indta)
```

## Arguments

- indta:

  A data.frame with a minimum of two columns `c("listnm","ticker")` with
  one or more lines for each `"listnm"` and possibly a column `weight`
  for weightings

## Value

Nothing

## Details

Add Asset Groups

Lists are specified in normalized form. Duplicate list names with those
currently in use are replaced.

## See also

[`av_runShiny()`](https://derekholmes0.github.io/alphavantagepf/reference/av_runShiny.md)

## Examples

``` r
if (FALSE) { # \dontrun{
newtickers <- c("QQQ","QQQE","NDX")
av_add_assetgroups(data.table(listnm=rep("nasdaq",length(newtickers)),ticker=newtickers))
# To remove an asset list, just use an empty string for the ticker
av_add_assetgroups(data.table(listnm=c("new"),ticker=c("")))
} # }
```

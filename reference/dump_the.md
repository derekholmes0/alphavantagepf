# Extract internal state

Prints internal data state of
[`av_runShiny()`](https://derekholmes0.github.io/alphavantagepf/reference/av_runShiny.md)
`dump_the(typegrep="*")` `dump_inv()` `dump_assetgroups()`
`dump_captured()`

## Usage

``` r
dump_the(typegrep = "*")

dump_inv()

dump_assetgroups(returngt = TRUE)

dump_captured(todo = "byfunction")
```

## Arguments

- typegrep:

  : Grep string for internal state parameters

- returngt:

  : Return GT table

- todo:

  : One of c("byfunction","pxhist",any av function name)

## Value

data.table with desired data.

## See also

[`av_runShiny()`](https://derekholmes0.github.io/alphavantagepf/reference/av_runShiny.md)

## Examples

``` r
if (FALSE) { # \dontrun{
`dump_the()`
`dump_inv()`
`dump_assetgroups(returngt=TRUE)`
`dump_captured(todo="byfunction")`
} # }
```

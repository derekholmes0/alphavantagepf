# Exported Internal internal state functions

Prints internal data state of
[`av_runShiny()`](https://derekholmes0.github.io/alphavantagepf/reference/av_runShiny.md)
`dump_state(typegrep="*")` `dump_inv()` `dump_assetgroups()`
`dump_captured()`

## Usage

``` r
dump_state(typegrep = "*")

inv_rv(rv)

dump_inv()

dump_assetgroups(returngt = TRUE)

dump_av_funcs()

dump_captured(todo = "byfunction")
```

## Arguments

- typegrep:

  : Grep string for internal state parameters

- rv:

  : An isolated list of av_shiny parameters

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
`dump_state()`
`dump_inv()`
`dump_av_funcs()`
`dump_assetgroups(returngt=TRUE)`
`dump_captured(todo="byfunction")`
`inv_rv(rv)`
} # }
```

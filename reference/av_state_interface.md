# av_state_interface

retrieves internal data state of
[`av_runShiny()`](https://derekholmes0.github.io/alphavantagepf/reference/av_runShiny.md)
`dump_state(typegrep="*")` `dump_inv(invgrep="*")` `dump_assetgroups()`
`dump_captured(todo="byfunction")` `av_shiny_px()`

## Usage

``` r
dump_state(typegrep = "*")

dump_inv(invgrep = "*")

dump_assetgroups()

dump_av_funcs()

dump_captured(todo = "byfunction")
```

## Arguments

- typegrep:

  : Grep string for internal state parameters

- invgrep:

  : A regular expression string

- todo:

  : One of c("byfunction","pxhist",any av function name)

## Value

data.table with desired data.

## Details

Return av_runShiny data and states

## See also

[`av_runShiny()`](https://derekholmes0.github.io/alphavantagepf/reference/av_runShiny.md)

## Examples

``` r
if (FALSE) { # \dontrun{
`dump_state()`
`dump_inv()`
`dump_av_funcs()`
`dump_assetgroups()`
`dump_captured(todo="byfunction")`
} # }
```

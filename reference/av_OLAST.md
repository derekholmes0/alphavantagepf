# Summary of most recent term structure

Gives a tqble of most recent option term structures

## Usage

``` r
av_OLAST(todo, rv)
```

## Arguments

- todo:

  Data structure containing command and assets

- rv:

  Copy of internal av_runShiny() state

## Value

List of output items displayed within to
[`av_runShiny()`](https://derekholmes0.github.io/alphavantagepf/reference/av_runShiny.md)

## Details

Uses precomputed data from av_add_options("iv_nearest"). See data
vignette for managing the options data stream.

## See also

[`av_runShiny_addFunctions()`](https://derekholmes0.github.io/alphavantagepf/reference/av_runShiny_addFunctions.md)

## Examples

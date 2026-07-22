# App database functions

Loads internal data (prices, earnings, etc.

## Usage

``` r
av_shinydata(item = NULL)
```

## Arguments

- item:

  Any data name as seen by running
  [`dump_state()`](https://derekholmes0.github.io/alphavantagepf/reference/dump_state.md).
  **If blank, loads database**

## Value

Data item specified by `item` or a nothing (but a message) if left blank

## See also

[`av_runShiny()`](https://derekholmes0.github.io/alphavantagepf/reference/av_runShiny.md)

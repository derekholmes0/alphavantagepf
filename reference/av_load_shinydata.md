# av_load_shinydata

Loads internal data (prices, earnings, etc.

## Usage

``` r
av_load_shinydata(item = NULL, verbose = TRUE)
```

## Arguments

- item:

  Any data name as seen by running
  [`dump_state()`](https://derekholmes0.github.io/alphavantagepf/reference/av_state_interface.md).
  **If blank, loads entire database**

- verbose:

  (default TRUE) write a status message to console

## Value

Data item specified by `item` or a nothing (but a message) if left blank

## See also

[`av_runShiny()`](https://derekholmes0.github.io/alphavantagepf/reference/av_runShiny.md)

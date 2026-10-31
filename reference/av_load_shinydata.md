# Load av_runShiny() internal data.

Loads internal data (prices, earnings, etc.

## Usage

``` r
av_load_shinydata(item = NULL, verbose = TRUE, addfuncs = TRUE)
```

## Arguments

- item:

  Any data name as seen by running
  [`dump_state()`](https://derekholmes0.github.io/alphavantagepf/reference/av_state_interface.md).
  **If blank, loads entire database**

- verbose:

  (default TRUE) write a status message to console

- addfuncs:

  (default TRUE) Readd and re-source any user defined functions

- restore_lazy:

  (default FALSE) Also restore all lazy data

## Value

Data item specified by `item` or a nothing (but a message) if left blank

## Details

Load ShinyApp data from Cache

## See also

[`av_runShiny()`](https://derekholmes0.github.io/alphavantagepf/reference/av_runShiny.md)

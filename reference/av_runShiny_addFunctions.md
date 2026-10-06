# Add experimental functions from a given directory

Adds analytics in code taken from single directory.

## Usage

``` r
av_runShiny_addFunctions(
  fun_dir = "c:/d/src/R/avpfShinyFuncs/avpfshinyFuncs/R"
)
```

## Arguments

- fun_dir:

  Directory containing functions with input signatures to add

## Value

Nothing

## Details

Add a set of analytics from a code direcory

Each file will be read in the specified directory and any function with
a valid signature will be added to the list of available functions in
the shiny app. The function signature is a call to the function with a
single argument "signature" which returns a list of three items: 1) a
short name for the function, 2) the function name, and 3) a help string
for the function. If those conditions obtain, the function will be added
to
[`av_runShiny()`](https://derekholmes0.github.io/alphavantagepf/reference/av_runShiny.md)

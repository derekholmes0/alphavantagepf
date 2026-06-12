# App formatting functions

Adds default elements to
[`gt()`](https://gt.rstudio.com/reference/gt.html) table

## Usage

``` r
gt.basetheme(
  x,
  gtopts = "all",
  sizepct = 100,
  style = 4,
  digits = 2,
  seps = FALSE,
  na_format = "-",
  size = "",
  interactive = FALSE
)
```

## Arguments

- x:

  Input data or `gt` table

- gtopts:

  Which elements to add

- sizepct:

  (default: 70) How big to make tables

- style:

  (default: 4) Style number (see
  [`gt()`](https://gt.rstudio.com/reference/gt.html))

- digits:

  (default: 2) Number formatting

- seps:

  (default: FALSE) Number formatting thousands separators

- na_format:

  (default: "-") WHat to show for NAs

- size:

  (default: "") Size of text, either `""` or `"small"`

- interactive:

  (default: `FALSE`) Add interactive elements

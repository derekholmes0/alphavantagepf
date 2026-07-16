# App database functions

Adds a user-defined function to the av Shiny app

## Usage

``` r
av_add_analytic(
  runcode,
  func_name,
  helpstr = "user function",
  focus = "MAIN",
  category = "USER"
)
```

## Arguments

- runcode:

  Code string user must run to call the function.

- func_name:

  Name of function run when analytic is called. If an empty string is
  supplied, the runcode will be de-registered.

- focus:

  (default: "MAIN") String with tab name to set focus to when command is
  run

- category:

  (default "USER") A string with a category used to sort function when
  help is called.

- help_str:

  (default: "user function"): A string comment to ad to the av.h (help)
  command

## Value

String message with success or failure of function addition.

## Details

When the
[`av_runShiny()`](https://derekholmes0.github.io/alphavantagepf/reference/av_runShiny.md)
app is run, users can call functions to provide analytics based on asset
strings in the command line. This function allows users to add their own
analytics by registering a function which takes, as inputs

1.  `todo`: The command line and any subsequent parameters as a space
    delimited string

2.  `rv`: Reactive values supplied by the Shiny app. In particular the
    parameter `rv$istr1` contains the semicolon delimited set of assets
    prior to the command invocation. The registered function should
    return a (possibly named, see vignette) list containing one or more
    `gt()` tables, `dygraphs()`, or `ggplots()` to be displayed when the
    command is run. See vignette for specfic details

- The function specified must be available (i.e in `.GlobalENv()`) to
  the Shiny app when the command is run. Otherwise an error message will
  be displayed.

- If the specified command has already been registered, a message will
  be given and the internal data will be overridden.

## See also

[`av_runShiny()`](https://derekholmes0.github.io/alphavantagepf/reference/av_runShiny.md)

## Examples

``` r
if (FALSE) { # \dontrun{
my_testfunc <- function(todo,rv) {
  message("todO: ",todo," with asset string ",rv$istr1)
  n_to_return <- c(strsplit(todo," "),"3")[[2]] |> as.numeric()
  table1 <- head(mtcars,n_to_return) |> gt()
  table2 <- data.table(asset=strsplit(rv$istr1,";")) |> gt()
  plot1 <- ggplot(mtcars,aes(mpg,disp)) + geom_point()
  return(list(table1, table2, plot1))
}
av_add_analytic("TEST","my_testfunc",helpstr="a test func")
# From the app; run "QQQ;SPY test 5"
# From the app: run "av.h"
} # }
```

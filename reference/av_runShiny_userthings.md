# App database functions: Price

Adds price data to
[`av_runShiny()`](https://derekholmes0.github.io/alphavantagepf/reference/av_runShiny.md)
internal data.

Adds earnings data to
[`av_runShiny()`](https://derekholmes0.github.io/alphavantagepf/reference/av_runShiny.md)
internal data, either by download or with user data

Adds asset lists to
[`av_runShiny()`](https://derekholmes0.github.io/alphavantagepf/reference/av_runShiny.md)
internal data.

Adds a user-defined function to the av Shiny app

Loads internal data (prices, earnings, etc.

## Usage

``` r
av_add_px(indta, assettypes = NULL, delay = 0)

av_add_earn(
  substitute_earn = NULL,
  substitute_earnest = NULL,
  assettypes = NULL,
  delay = 0
)

av_add_assetgroups(indta)

av_add_analytic(
  runcode,
  func_name,
  helpstr = "user function",
  focus = "MAIN",
  category = "USER"
)

av_shinydata(item = NULL)
```

## Arguments

- indta:

  A data.frame with two columns `c("listnm","ticker")` with one or more
  lines for each `"listnm"`

- assettypes:

  (default NULL) An optional data.frame with minimal columns
  `c(symbol,type,currency,name)` with descriptive data for the assets
  given in `indta`. If not specified, a call to
  `av_get_pf(.,"SYMBOL_SEARCH")` is necessary to determine the asset
  type (one of `c("Equity","ETF","FX","Index","Crypto")`) for subsequent
  calls to
  [`av_get_pf()`](https://derekholmes0.github.io/alphavantagepf/reference/av_get_pf.md)
  NOTE THAT assettypes cannot be NULL if substitute_earn and
  substitute_earnest are NULL.

- delay:

  (default 0) Seconds to delay calls to determine asset type for future
  AV downloads. This is unused if `assettypes` is given.

- substitute_earn:

  A (default NULL) data.frame with past earnings

- substitute_earnest:

  (default NULL) A data.frame with earnings estimates

- runcode:

  Code string user must run to call the function.

- func_name:

  Name of function run when analytic is called. If an empty string is
  supplied, the runcode will be de-registered.

- helpstr:

  (default: "user function"): A string comment to ad to the av.h (help)
  command

- focus:

  (default: "MAIN") String with tab name to set focus to when command is
  run

- category:

  (default "USER") A string with a category used to sort function when
  help is called.

- item:

  Any data name as seen by running
  [`dump_state()`](https://derekholmes0.github.io/alphavantagepf/reference/dump_state.md).
  **If blank, loads database**

## Value

Nothing

Data.table with downloaded or added earnings

Nothing

String message with success or failure of function addition.

Data item specified by `item` or a nothing (but a message) if left blank

## Details

Entire set of columns from
[`av_get_pf()`](https://derekholmes0.github.io/alphavantagepf/reference/av_get_pf.md)
can be added. First date column renamed to `timestamp`

Entire set of columns from
[`av_get_pf()`](https://derekholmes0.github.io/alphavantagepf/reference/av_get_pf.md)
can be added. First date column renamed to `timestamp`. If just
assetypes is given, the function downloads earnings as needed
(respecting maximum age parameters defined in the app's `AVOPTS` tab.)

Lists are specified in normalized form. Duplicate list names with those
currently in use are replaced.

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

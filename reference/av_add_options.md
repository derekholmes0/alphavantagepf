# av_add_options

Manages option database and creates vol surfaces

## Usage

``` r
av_add_options(
  todo,
  dtstr = "-1w::",
  symbols = NULL,
  freq = "d",
  replace_data = FALSE,
  baddates_limits = 3,
  verbosity = "basic",
  external_path = NULL
)
```

## Arguments

- todo:

  What to do to add options. Choices are in the table below

  |  |  |
  |----|----|
  | `todo` | Description |
  | `update` | Download and add or update full option runs by symbol over periods in `dtstr` |
  | `inventory` | Create/update inventory of options that have been downloaded |
  | `reconstruct_inventory` | Reconstruct inventory of all options that have been downloaded |
  | `iv` | Updates internal data.table (`eqopt_iv` of Implied term structures by symbol and date |
  | `copy_external` | Copies identical directory structures from `external_path` to siny app internal data |

- dtstr:

  (default `"-1w::"`) How far back to load options. ONly data needed
  within timeframe is downloaded, unless `replace_data=TRUE`

- symbols:

  (default: NULL) List of symbols to download

- freq:

  (Default `"d"`) Granularity of data downloaded ("d" for daily, "w" for
  weekly, "m" for monthly)

- replace_data:

  (Default `FALSE`) Update or replace data

- baddates_limits:

  (default 3): Consecutive business days of null data beyond which
  options are assumed not to exist.

- verbosity:

  (default `"time,basic"`) What to display as data is added.

- external_path:

  (default NULL) Path from which to replace existing data

## Value

nothing

## Details

Manage option data

**This function can take a long time to run**. Full option quotes are
stored by symbol in arrow/parquet format by symbol day, and contract.

- Unless `replace_data==TRUE`, only dates and symbols that need to be
  downloaded will.

- Except the `OS S` function, all analytics run in the app require
  options data to be downloaded using this function.

## See also

[`av_runShiny()`](https://derekholmes0.github.io/alphavantagepf/reference/av_runShiny.md)

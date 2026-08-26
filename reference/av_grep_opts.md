# Filter options from returned AV data

Filter options from returned AV data

## Usage

``` r
av_grep_opts(
  indta,
  grepstring = "F,M,C,otm",
  spot = NULL,
  mindays = 3,
  startdt = Sys.Date(),
  mindelta = 0.05,
  dropsymbol = FALSE
)
```

## Arguments

- indta:

  A data.table as returned by
  [`av_get_pf()`](https://derekholmes0.github.io/alphavantagepf/reference/av_get_pf.md)

- grepstring:

  (default `F,M,call,otm,act`) Three to five item string to select
  specific maturities, option types, strike ranges, and open interest
  categories from option sets returned by
  `av_get_pf(.,"HISTORICAL_OPTIONS")`. Each item in the list is an
  abbreviated code for what to select. The items are not case sensitive
  and in order are

  |  |  |  |
  |----|----|----|
  | Item | Values | description |
  | Expiration Limit | `F`\|`B`\|`A` | `F` is first maturity, `B` is second maturity, `A` (pr blank) are all |
  | Maturity Type | `M`\|`Q`\|`A` | `M` for monthly contracts, `Q` for quarterly, `A` (pr blank) are all |
  | Option Type | `C`\|`P`\|`A` | Calls, puts, `all` (or blank) for all |
  | Moneyness | `otm`\|`itm`\|`A` | Out of the money, in the money or both |
  | Activity | `act`\|`A` | If `A`, only pass contracts with positive open interest |

- spot:

  (default `NULL`) Spot to be used to determine itm/otm, If null then it
  is inferred from most out of the money call or put Note: This
  parameter only applies if there is one symbol in `indta`. If there is
  more than one ticker in `indta` a column `spot` must be in `indta` to
  get correct results.

- mindays:

  (default 3). Minimum number of days to expiration to be passed through
  from `startdt`

- startdt:

  (default [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html)). Date
  from which expirations will be considered.

- mindelta:

  (default `0.05`) delta limit on both moneyness sides, i.e. pass only
  options with deltas in range c(`mindelta`,1-`mindelta`)

- dropsymbol:

  (default `FALSE`). Drop symbol from returned data table.

## Value

A reduced set of options obtained from
[`av_get_pf()`](https://derekholmes0.github.io/alphavantagepf/reference/av_get_pf.md)
using Alphavantage `HISTORICAL_OPTIONS` function.

## Details

Alphavantage doesn't always return quarterly options

[`av_get_pf()`](https://derekholmes0.github.io/alphavantagepf/reference/av_get_pf.md)
returns a large list of options. This function helps to narrow down the
list by maturity and moneyness.

## See also

[`av_get_pf()`](https://derekholmes0.github.io/alphavantagepf/reference/av_get_pf.md)

## Examples

``` r
if (FALSE) { # \dontrun{
av_get_pf("IBM","HISTORICAL_OPTIONS") |> av_grep_opts("F,M,put",mindays=2)
} # }
```

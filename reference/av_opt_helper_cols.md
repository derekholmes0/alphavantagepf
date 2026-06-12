# Add additional data to returned option sets

Add additional data to returned option sets

## Usage

``` r
av_opt_helper_cols(indta, scaling = NULL, spot = NULL)
```

## Arguments

- indta:

  An option data.table as returned by
  [`av_get_pf()`](https://derekholmes0.github.io/alphavantagepf/reference/av_get_pf.md)

- scaling:

  (default `NULL`) Scaling factor for marks, last values, and greeks.
  Options are

  - `NULL` or `"none"` : No Scaling

  - `"10contracts"` : 10 contracts converted into market value

  - `"10kMV"` : 10,000 USD converted into equivalent market value

  - `number` : Any numeric value in thousands of USD into equivalent
    market value

- spot:

  (default `NULL`) Spot to be used to determine itm/otm, If null then it
  is inferred from most out of the money call or put Note: This
  parameter only applies if there is one symbol in `indta`. If there is
  more than one ticker in `indta` a column `spot` must be in `indta` to
  get correct results.

## Value

An option `data.table` with extra columns helpful for further analysis

## Details

Adds columns including

|                |                                    |
|----------------|------------------------------------|
| Column         | Definition                         |
| `daysExp`      | Days to Expiration                 |
| `bo_pct`       | Bid offer in percent of option mid |
| `bid_size_poi` | Bid size percent of open interest  |
| `ncak`         | Notional number of contracts       |

## See also

[`av_grep_opts()`](https://derekholmes0.github.io/alphavantagepf/reference/av_grep_opts.md)

## Examples

``` r
if (FALSE) { # \dontrun{
av_get_pf("IBM","HISTORICAL_OPTIONS") |> av_grep_opts("F,M,put",mindays=2) |> av_opt_helper_cols()
} # }
```

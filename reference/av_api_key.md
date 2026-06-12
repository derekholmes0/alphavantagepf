# Set the Alpha Vantage API Key

`av_api_key()` sets Alphavantage API key and entitlement code

## Usage

``` r
av_api_key(api_key, entitlement = NULL)
```

## Arguments

- api_key:

  A character string with your Alpha Vantage API Key.

- entitlement:

  A character string with your Alpha Vantage entitlement status. If not
  "delayed" or "realtime" entitlement not added to API string.

## Value

Invisibly returns two item list with API key and entitlement string once
set). Use print method to view.

## Details

The Alpha Vantage API key must be set prior to using
[`av_get_pf()`](https://derekholmes0.github.io/alphavantagepf/reference/av_get_pf.md).
You can obtain an API key at the [Alpha Vantage
Website](https://www.alphavantage.co/).

## See also

[`av_get_pf()`](https://derekholmes0.github.io/alphavantagepf/reference/av_get_pf.md)

## Examples

``` r
if (FALSE) { # \dontrun{
av_api_key("YOUR_API_KEY",entitlement="delayed")
av_get_pf("IBM", "TIME_SERIES_INTRADAY")
} # }
```

#' alphavantagepf: A lightweight R interface to the Alpha Vantage API
#'
#' @description
#' An R interface to the Alpha Vantage API which emphasizes normalized data nd data.table conventions
#'
#'
#' @details
#' The `alphavantagepf` package provides a lightweight interface to the
#' Alpha Vantage API. Alpha Vantage is a free source for financial data
#' that in many cases is more accurate than Yahoo Finance and Google Finance.
#' Get a free API KEY at https://www.alphavantage.co. Then use the R interface
#' functions `av_api_key("YOUR_KEY")` to set the API key and the `av_get_pf()`
#' function to get financial data.
#'
#'
#' @name alphavantagepf
#'
#' @importFrom dplyr %>%
## usethis namespace: start
#' @importFrom data.table :=
#' @importFrom data.table .N
#' @importFrom data.table let
## usethis namespace: end
NULL

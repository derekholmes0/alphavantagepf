#' Set the Alpha Vantage API Key
#'
#' @name av_api_key
#' @description
#' `av_api_key()` sets Alphavantage API key
#' `av_validkey()` returns TRUE if Alphavantage API key appears valid.
#'
#' @param api_key A character string with your Alpha Vantage API Key.
#'
#' @returns Invisibly returns API key once set. Use print method to view.
#'
#' @details
#' The Alpha Vantage API key must be set prior to using [av_get_pf()]. You can obtain
#' an API key at the [Alpha Vantage Website](https://www.alphavantage.co/).
#'
#' @seealso [av_get_pf()]
#'
#' @examples
#' \dontrun{
#' av_api_key("YOUR_API_KEY")
#' av_get_pf("IBM", "TIME_SERIES_INTRADAY")
#' }
#'
#' @rdname av_api_key
#' @export
av_api_key <- function(api_key) {
    if (!missing(api_key)) {
        options(av_api_key = api_key)
    }
    invisible(getOption('av_api_key'))
}

#' @rdname av_api_key
#' @export
av_validkey <- function() { nchar(getOption('av_api_key'))>10 }

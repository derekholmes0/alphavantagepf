#' Set the Alpha Vantage API Key
#'
#' @name avpf_api_key
#' @description
#' `avpf_api_key()` sets Alphavantage API key and entitlement code
#'
#' @param api_key A character string with your Alpha Vantage API Key.
#' @param entitlement An optional character string with your Alpha Vantage entitlement status.  If not "delayed" or "realtime" entitlement not added to API string.
#' @returns Invisibly returns two item list with API key and entitlement string once set). Use print method to view.
#'
#' @details
#' The Alpha Vantage API key must be set prior to using [av_get_pf()]. You can obtain
#' an API key at the [Alpha Vantage Website](https://www.alphavantage.co/).
#'
#' @seealso [av_get_pf()]
#'
#' @examples
#' \dontrun{
#' avpf_api_key("YOUR_API_KEY",entitlement="delayed")
#' av_get_pf("IBM", "TIME_SERIES_INTRADAY")
#' }
#'
#' @rdname avpf_api_key
#' @export
avpf_api_key <- function(api_key,entitlement=NULL) {
    if (!missing(api_key)) {
        options(av_api_key = api_key)
        the_av$avapikey = api_key
        av_set_defaults()
    }
  if (!is.null(entitlement)) {
    if( tolower(entitlement) %in% c("delayed","realtime")) {
        options(av_api_entitlement = entitlement)
        the_av$avapientitlement = entitlement
      av_set_defaults()
    }
  }
  invisible(c(getOption('av_api_key'),getOption('av_api_entitlement',default=NA_character_)))
}


#' Set the Alpha Vantage max requests per minute
#'
#' @name avpf_set_request_pace
#' @description
#' `avpf_set_request_pace()` sets Alphavantage API key and entitlement code
#' @param max_requests_per_min (default 60) Maximum requests per minute
#' @returns Nothing
#' @details
#' Change this if you have a better plan with AlphaVantage
#' @seealso [avpf_api_key()]
#' @export
avpf_set_request_pace <- function(max_requests_per_min=60) {
  message_if_red(TRUE,"Setting Max requests per minute to ",max_requests_per_min)
  av_set_defaults("max_requests_per_min",max_requests_per_min,savetoconstants=TRUE)
  return()
}

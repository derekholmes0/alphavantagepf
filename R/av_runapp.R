#' RShiny App
#'
#' @name av_runShiny
#' @description
#' `av_runShiny()` runs an interactive RShiny app with a professional command line  iterface to download, manage, and visualize data from the [Alpha Vantage](https://www.alphavantage.co/documentation/) data service.
#' The app treats equities, ETFs, indices, and FX equally, so users do not need to know specific API calls to integrate data.  Downloaded data is cached to reduce
#' API calls, all data can be saved for external use, and external data can be added to the app.  Sets of securities can be easily added and managed.
#'
#' @returns Nothing
#'
#' @details
#' Invocation starts Shiny application.  See vignette for full details.
#' **On first use**, click on `AVOPTS` tab, fill in the following fields, and  hit the "Set Opts" button.
#' * **AV API Key**: API key obtained from  [Alpha Vantage](https://www.alphavantage.co/documentation/)
#' * **Entitlement** Entitlement status (either `delayed` or `realtime`)
#' Other options that can be set are given below.
#'
#' **To run analysis**, Enter a semicolon delimited set of securities (e.g. `SPY;QQQ;DIA`), select a runtime option from
#' the adjacent dropdown, and press the `RUN button.  Options include:
#' |Option|Description|
#' |:-------:|:-------------------------------------|
#' |`Inventory`|Table of what asset lists and price data is available|
#' |`LivePx`|Live prices of all tickers|
#' |`NameSearch`|Search for tickers|
#' |`PriceTS`|Time Series of prices or Total Return indices|
#' |`ActiveTS`|Active returns of tickers in top row relative to first ticker in bottom row|
#' |`HistVolTS`|Historic vol and rolling correlations of tickers in top row.|
#' |`DES`,`News`|Descriptions and News for tickers in top row.|
#' |`DivEarn`|Dividends, earnings, and earnings estimates for tickers in top row.|
#' |`OptSearch`|Search for options for tickers in top row. (`OPTS` tab)|
#'
#' **Options that can be set**. click on `AVOPTS` tab, fill in the following fields, and  hit the "Set Opts" button.
#'
#' * __fgts colorset__: A named list of colors which can be set using the  [FinanceGraphs::fg_update_aes()] function.
#' * __Regr Significance__: p-level to highlight significant regressions. (Used in the `HistVolTS` function above.)
#' * __Cache Directory__: A directory where internal App data can be stored (and added to). If not filled in, a temporary
#' directory is created and used.
#'
#' The app allows all downloaded data to be saved.  If used, data is saved in one file, and is stored as a named (by API call function)
#' list of `data.table`s.  Options to control this behavior are
#'
#' * __AV dump Directory__: A directory where individual API requests can be cached.  Data is saved in one file as a named
#' (by API call function) list of `data.table`s.
#' * __Capture AV Data__: Save Price data, non-price data, all or none.
#' * __Update or Cumulative__: If `update` is selected, data is keyed relevantly, so for example there will only be one set of dividend data stored.
#' If `cumulative` then all API data (including a timestamp) is saved sequentially.  The latter will lead to much larger files.
#' * __Other options__:
#'  - `CleanOnStart` : cleans out the cache file every time the app is started.
#'  - `SaveEveryAVCall` : Saves cache file after every call.
#'  - `SaveNow` : Save cache file when this is selected.
#' @returns
#' A `ShinyAppHandle` object.
#' @examples
#' \dontrun{
#' av_runShiny()
#' }
#' @rdname av_runShiny
#' @export
av_runShiny <- function() {
  if(file.exists(the_av$constants_fn)) {
    restore_avs_state(msg="Startup")
    the_av$outcopy<-list()
    options(av_api_key = the_av$avapikey)
    options(av_api_entitlement = the_av$avapientitlement)
  }
  else {
    av_reset_defaults(fileopts=FALSE) # Only use true if reinstalling entire package
    the_av$avsh_funcs <- copy(avsd$def_avsh_funcs)
    save_avs_state("all",msg="I N I T")
  }
  the_av$do_on_start <- TRUE
  return(startApp(shinyApp(ui=av_make_ui(), server=av_make_server(), options=list(width=1400,height=800,"launch.browser"))))
}

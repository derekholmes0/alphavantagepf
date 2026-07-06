# =======================================================================================================
#' App database functions
#'
#' @importFrom fst read_fst write_fst
#' @importFrom lubridate is.instant
#' @import data.table
#' @importFrom stats setNames
#'
#' @name av_add_px
#' @description Adds price data to [av_runShiny()] internal data.
#' @param indta A data.frame with the following minimal columns: `c(symbol,timestamp,close,adjusted_close)`.
#' Other variables added could be `c(open,high,low,volume,dividend_amount,split_coefficient)`
#' @param assettypes (default NULL)  An optional data.frame with minimal columns `c(symbol,type,currency,name)` with
#' descriptive data for the assets given in `indta`.  If not specified, a call to `av_get_pf(.,"SYMBOL_SEARCH")`
#' is necessary to determine the asset type (one of `c("Equity","ETF","FX","Index","Crypto")`) for subsequent
#' calls to [av_get_pf()]
#' @param delay (default 0) Seconds to delay calls to determine asset type for future AV downloads. This is
#' unused if `assettypes` is given.
#' @returns Nothing
#' @seealso [av_runShiny()]
#' @details Entire set of columns from [av_get_pf()] can be added. First date column renamed to `timestamp`
#' @examples
#' \dontrun{
#' av_add_px(av_get_pf("IBM","TIME_SERIES_DAILY_ADJUSTED"))
#' asset_df <- data.frame(symbol=c("HYG"),type=c("ETF"),currency=c("USD"), name=c("HY ETF"))
#' av_add_px(av_get_pf("HYG","TIME_SERIES_DAILY_ADJUSTED"), assettypes=asset_df)
#'
#' suppressMessages(require(quantmod))
#' ffdta <- as.data.table(quantmod::getSymbols("FEDFUNDS",src="FRED",auto.assign=FALSE))
#' ffdta <- ffdta[,.(DT_ENTRY=index,close=FEDFUNDS,adjusted_close=FEDFUNDS,symbol="FEDFUNDS")]
#' av_add_px(ffdta)
#' }
#'
#' @export
av_add_px <- function(indta,assettypes=NULL,delay=0) {
  restore_avs_state("all")
  firstdate <- find_col_bytype(indta,lubridate::is.instant)
  if (is.null(firstdate)) {
    stop("av_add_data: Need a timestamp column")
  }
  indta <- data.table(indta)
  setnames(indta,firstdate,"timestamp")
  check_min_colset(indta,s("symbol;timestamp;close;adjusted_close"))
  #manage_epx(unique(indta$symbol),"-30y::",substitute_data=indta,substitute_symset=assettypes,force=TRUE,delay=delay)
  manage_px(unique(indta$symbol),"-30y::",substitute_data=indta,substitute_symset=assettypes,delay=delay)
  # need (symbol=TICKER,type="user",currency="USD",name=TICKER)
  newinv <- get_inv(unique(indta$symbol),override_symset=assettypes)
  the_av$pxinv <- DTUpsert(the_av$pxinv, newinv, c("symbol"),fill=TRUE)
  save_avs_state("px",msg="av_add_px")
}

#' @name av_add_earn
#' @description Adds price data to [av_runShiny()] internal data.
#' @param substitute_earn A (default NULL)  data.frame with past earnings
#' @param substitute_earnest  (default NULL)  A data.frame with  earnings estimates
#' @param assettypes (default NULL)  An optional data.frame with minimal columns `c(symbol,type,currency,name)` with
#' descriptive data for the assets given in `indta`.  If not specified, a call to `av_get_pf(.,"SYMBOL_SEARCH")`
#' is necessary to determine the asset type (one of `c("Equity","ETF","FX","Index","Crypto")`) for subsequent
#' calls to [av_get_pf()]
#' NOTE THAT assettypes cannot be NULL if substitute_earn and substitute_earnest are NULL.
#' @param delay (default 0) Seconds to delay calls to determine asset type for future AV downloads. This is
#' unused if `assettypes` is given.
#' @
#' @returns Nothing
#' @seealso [av_runShiny()]
#' @details Entire set of columns from [av_get_pf()] can be added. First date column renamed to `timestamp`
#' @examples
#' \dontrun{
#' # To add earnings for a set of tickers
#' av_add_earn(assettypes=data.table(symbol=c("AAPL","QQQ"))
#' }
#'
av_add_earn <- function(substitute_earn=NULL,substitute_earnest=NULL,assettypes=NULL,delay=0,maxage=0) {
  if(nrow(the_av$earn)>0 & as.numeric(Sys.Date()-max(the_av$earn$ts))<=maxage) {
    message_if_red(TRUE,"av_add_earn skipping earning addition with maxage ",maxage," at ",Sys.time())
  }
  else {
    symset = if (is.null(substitute_earn)) unique(assettypes$symbol) else unique(substitute_earn$symbol)
    rtnpx <- the_av$pxinv[data.table(symbol=symset),on=.(symbol)][,.(symbol,type)]
    manage_earn(rtnpx,substitute_earn=substitute_earn,substitute_earnest=substitute_earnest,delay=delay)
    the_av$pxinv <- DTUpsert(the_av$pxinv, get_inv(symset), c("symbol"),fill=TRUE)
    save_avs_state("px",msg="av_add_earn")
  }
}

# =======================================================================================================
#' App database functions
#'
#' @name av_add_assetgroups
#' @description Adds asset lists to [av_runShiny()] internal data.
#' @param indta A data.frame with two columns `c("listnm","ticker")` with one or more lines for each `"listnm"`
#' @returns Nothing
#' @seealso [av_runShiny()]
#' @details Lists are specified in normalized form.  Duplicate list names with those currently in use are replaced.
#' @examples
#' \dontrun{
#' newtickers <- c("QQQ","QQQE","NDX")
#' av_add_assetgroups(data.table(listnm=rep("nasdaq",length(newtickers)),ticker=newtickers))
#' # To remove an asset list, just use an empty string for the ticker
#' av_add_assetgroups(data.table(listnm=c("new"),ticker=c("")))
#' }
#'
#' @export
av_add_assetgroups <- function(indta) {
  indta <- as.data.table(indta)
  check_min_colset(indta,s("listnm;ticker"))
  restore_avs_state("constants")
  the_av$assetgroups <- DTUpsert(the_av$assetgroups,indta,c("listnm"))
  the_av$assetgroups <- the_av$assetgroups[nchar(ticker)>0,]
  save_avs_state("all",msg="add_assetgroups")
}


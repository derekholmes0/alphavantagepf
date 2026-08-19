#' Add Price or Time Series Data
#'
#' @name av_add_px
#' @title Add or download Price or Time Series Data
#' @description Adds price data to [av_runShiny()] internal data.
#' @param indta A data.frame with the following minimal columns: `c(symbol,timestamp,close)`.
#' Other variables added could be `c(adjusted_close,open,high,low,volume,dividend_amount,split_coefficient)`
#' If `adjusted_close` is not in the dataset, it will be set to `close`
#' @param assettypes (default NULL)  An optional data.frame with minimal columns `c(symbol,type,currency,name)` with
#' descriptive data for the assets given in `indta`.  If not specified, a call to `av_get_pf(.,"SYMBOL_SEARCH")`
#' is necessary to determine the asset type (one of `c("Equity","ETF","FX","Index","Crypto")`) for subsequent
#' calls to [av_get_pf()]
#' @param equitylist (default NULL) If specified, function will get equity prices from `av_get_pf`.  `indta` can be
#' null or is otherwise ignored.
#' @param dtstr (default `"-30y::"`). Date range to download if applicable.
#' @param delay (default 0) Seconds to delay calls to determine asset type for future AV downloads. This is
#' unused if `assettypes` is given.
#' @returns Nothing
#' @seealso [av_runShiny()]
#' @details Entire set of columns from [av_get_pf()] can be added. First date column renamed to `timestamp` internally.
#' @examples
#' \dontrun{
#' # To add known symbols outside the app
#' av_load_shinydata()  # Make sure most recent data is loaded
#' av_add_px(equitylist=c("IBM","GS","JPM"))
#'
#' # To add ad-hoc data from Alphavantage (e.g. Natgas spot at Henry Hub)
#' # Note that "symbol" in indta must match same in assettypes
#' asset_df <- data.frame(symbol=c("GAS_HH"),type=c("user"),currency=c("USD"), name=c("GAS_HH"))
#' ng_data <- av_get_pf("","NATURAL_GAS")[,.(symbol="GAS_HH",timestamp,close=value)]
#' av_add_px(ng_data, assettypes=asset_df)
#'
#' # To data from other sources
#' suppressMessages(require(quantmod))
#' ffdta <- as.data.table(quantmod::getSymbols("FEDFUNDS",src="FRED",auto.assign=FALSE))
#' ffdta <- ffdta[,.(DT_ENTRY=index,close=FEDFUNDS,adjusted_close=FEDFUNDS,symbol="FEDFUNDS")]
#' av_add_px(ffdta)
#' }
#' @importFrom fst read_fst write_fst
#' @importFrom lubridate is.instant
#' @import data.table
#' @importFrom stats setNames
#' @export
av_add_px <- function(indta=NULL,assettypes=NULL,equitylist=NULL,dtstr="-30y::",delay=0) {
  av_load_shinydata(verbose=FALSE)
  if(!is.null(assettypes)) { assettypes <- as.data.table(assettypes) }
  if(!is.null(indta)) {
    indta <- as.data.table(indta)
    firstdate <- find_col_bytype(indta,lubridate::is.instant)
    if (is.null(firstdate)) {
      stop("av_add_data: Need a timestamp column")
    }
    indta <- data.table(indta)[,symbol:=toupper(symbol)]
    setnames(indta,firstdate,"timestamp")
    check_min_colset(indta,s("symbol;timestamp;close"))
    if(!"adjusted_close" %in% names(indta)) {
      indta <- indta[,adjusted_close:=close][]
    }
    symbolset <- unique(indta$symbol)
    dtstr <- paste0(min(indta$timestamp),"::")
    manage_px(symbolset,dtstr,substitute_data=indta,substitute_symset=assettypes,delay=delay)
  }
  else if (is.vector(s(equitylist))) {
    symbolset <- lapply(s(equitylist),\(x) manage_px(x,dtstr,delay=delay))
    symbolset <- s(equitylist)
  }
  else {
    message_if_red(TRUE,"av_add_px: without any data or an equitylist, have nothing to do")
    return()
  }
  # need (symbol=TICKER,type="user",currency="USD",name=TICKER)
  newinv <- get_inv(symbolset,override_symset=assettypes)
  the_av$pxinv <- DTUpsert(the_av$pxinv, newinv, c("symbol"),fill=TRUE)
  save_avs_state("px",msg="av_add_px",ts_update=FALSE)
}

#' Add Earnings Data
#'
#' @name av_add_earn
#' @title Add or download Earnings Data
#' @description Adds earnings data to [av_runShiny()] internal data, either by download or with user data
#' @param substitute_earn A (default NULL)  data.frame with past earnings
#' @param substitute_earnest  (default NULL)  A data.frame with  earnings estimates
#' @param equitylist (default NULL)  A list with tickers for which to retrieve earnings (from AlphaVantage)
#' @param delay (default 0)  A numeric value specifying delay between Alphavantage calls (in seconds)
#' @returns Data.table with summary of downloaded or added earnings
#' @seealso [av_runShiny()]
#' @details Entire set of columns from [av_get_pf()] can be added. First date column renamed to `timestamp`.
#' If just assetypes is given, the function downloads earnings as needed (respecting maximum age parameters defined
#' in the app's `AVOPTS` tab.)
#' **Note that price data must always be added first**
#' @examples
#' \dontrun{
#' # To add earnings for a set of tickers
#' av_load_shinydata()  # Make sure most recent data is loaded
#' av_add_earn(equitylist=data.table(symbol=c("IBM","GS"))
#' }
#' @export
av_add_earn <- function(substitute_earn=NULL,substitute_earnest=NULL,equitylist=NULL,delay=0) {
  # Age taken care of by manage_earn
  av_load_shinydata(verbose=FALSE)
  symset <- list()
  if(!is.null(substitute_earn) && length(symset)<=0) { symset <- unique(substitute_earn$symbol) }
  if(!is.null(substitute_earnest) && length(symset)<=0) { symset <- unique(substitute_earnest$symbol) }
  if(!is.null(equitylist) && length(symset)<=0) { symset <- unique(s(equitylist)) }
  if(length(symset)<=0) {
    message_if_red(TRUE,"av_add_earn cannot find any symbols")
    return(NULL)()
  }
  rtnpx <- the_av$pxinv[data.table(symbol=symset),on=.(symbol)][,.(symbol,type)][type=="Equity",]
  rtniv <- manage_earn(rtnpx,substitute_earn=substitute_earn,substitute_earnest=substitute_earnest,delay=delay)
  the_av$pxinv <- DTUpsert(the_av$pxinv, get_inv(symset), c("symbol"),fill=TRUE)
  save_avs_state("px",msg="av_add_earn",ts_update=FALSE)
  return(rtniv)
}

#' Load ShinyApp data from Cache
#'
#' @name av_load_shinydata
#' @title Load av_runShiny() internal data.
#' @description Loads internal data (prices, earnings, etc.
#' @param item Any data name as seen by running [dump_state()].  **If blank, loads entire database**
#' @param verbose (default TRUE) write a status message to console
#' @returns Data item specified by `item` or a nothing (but a message) if left blank
#' @seealso [av_runShiny()]
#' @export
av_load_shinydata <- function(item=NULL,verbose=TRUE) {
  if(is.null(item)) {
    restore_avs_state("all");
    the_av$outcopy<-list()
    options(av_api_key = the_av$avapikey)
    options(av_api_entitlement = the_av$avapientitlement)
    message_if(the_av$verbose && verbose,"Loading avShiny Internal data.  Use dump_state() to see what's available")
  }
  else {
    return(get(item,envir=the_av))
  }
}

#' Add Asset Groups
#'
#' @name av_add_assetgroups
#' @title Add asset lists
#' @description Adds asset lists to [av_runShiny()] internal data.
#' @param indta A data.frame with a minimum of two columns `c("listnm","ticker")` with one or more lines for each `"listnm"` and possibly a column `weight` for weightings
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
#' @export
av_add_assetgroups <- function(indta) {
  av_load_shinydata(verbose=FALSE)
  indta <- as.data.table(indta)
  check_min_colset(indta,s("listnm;ticker"))
  if(!("weight" %in% colnames(indta))) { indta[, weight:=1/.N, by=.(listnm)]  }
  restore_avs_state("constants")
  the_av$assetgroups <- DTUpsert(the_av$assetgroups,indta,c("listnm"))
  the_av$assetgroups <- the_av$assetgroups[nchar(ticker)>0,]
  save_avs_state("all",msg="add_assetgroups")
}

#' Adds or removes a new command to the av_runShiny app
#'
#' @title av_add_analytic
#' @description Adds a user-defined function to the av Shiny app
#' @param runcode Code string user must run to call the function.
#' @param func_name Name of function run when analytic is called.  **If an empty string is supplied, the runcode will be de-registered.**
#' @param helpstr (default: "user function"): A string comment to ad to the av.h (help) command
#' @param focus (default: "MAIN")  String with tab name to set focus to when command is run
#' @returns String message with success or failure of function addition.
#' @seealso [av_runShiny()]
#' @details When the [av_runShiny()] app is run, users can call functions to provide analytics based on asset strings in the command line.
#' This function allows users to add their own analytics by registering a function which takes, as inputs
#'  1. `todo`: The command line and any subsequent parameters as a space delimited string
#'  2. `rv`: Reactive values supplied by the Shiny app.  In particular the parameter `rv$istr1` contains the semicolon delimited set of assets
#'  prior to the command invocation.
#' The registered function should return a (possibly named, see vignette)
#' list containing one or more `gt()` tables, `dygraphs()`, or `ggplots()` to be displayed when the command is run.  See vignette for specfic details
#' * The function specified must be available (i.e in `.GlobalENv()`) to the Shiny app when the command is run.  Otherwise an error message will be displayed.
#' * If the specified command has already been registered, a message will be given and the internal data will be overridden.
#' @examples
#' \dontrun{
#' my_testfunc <- function(todo,rv) {
#'   message("todO: ",todo," with asset string ",rv$istr1)
#'   n_to_return <- c(strsplit(todo," "),"3")[[2]] |> as.numeric()
#'   table1 <- head(mtcars,n_to_return) |> gt()
#'   table2 <- data.table(asset=strsplit(rv$istr1,";")) |> gt()
#'   plot1 <- ggplot(mtcars,aes(mpg,disp)) + geom_point()
#'   return(list(table1, table2, plot1))
#' }
#' av_add_analytic("TEST","my_testfunc",helpstr="a test func")
#' # From the app; run "QQQ;SPY test 5"
#' # From the app: run "av.h"
#' }
#' @export
av_add_analytic <- function(runcode,func_name,helpstr="user function",focus="MAIN") {
  runcode=toupper(runcode)
  av_load_shinydata(verbose=FALSE)
  if( toupper(runcode) %in% the_av$avsh_funcs$runcode) {
    if( nchar(func_name)<=0) {
      message_if_red(TRUE,"av_add_analytic: ",runcode, " will be removed from function list")
      the_av$avsh_funcs <- the_av$avsh_funcs[!runcode==runcode,]
      save_avs_state("all",msg="Remove function")
    }
    else {
      message_if_red(TRUE,"av_add_analytic: ",runcode, " already registered, data will be replaced")
    }
  }
  if( nchar(func_name)<=0) {
    message_if_red(TRUE,"Invalid function name; skipping operation")
    return()
  }
  new_analytics <- data.table(category="user",runcode=runcode, func_src="user", func_name=func_name, focus=focus, helpstr=helpstr)
  the_av$avsh_funcs <- DTUpsert(the_av$avsh_funcs,new_analytics,keys=c("runcode"),fill=TRUE)
  save_avs_state("all",msg=paste0("Add FUnction ",runcode))
}


#' Display a user message in the av_runShiny app
#'
#' @title avsh_quick_message
#' @description Displays a message underneath an input box
#' @param this_message (default "")  A text message to  be used. If empty string, the current message is cleared.
#' @param eval (default TRUE) OPtional parameter to suppress execution.
#' @param color Optional text color
#' @returns logical value of `eval`
#' @export
avsh_quick_message <- function(this_message,eval=TRUE,color="#1f78b4") {
  if(eval) {  the_av$user_feedback <- this_message }
}

quick_message <- function(this_message="",eval=TRUE,color="#1f78b4",wh="istr1", session = shiny::getDefaultReactiveDomain()) {
  shinyFeedback::hideFeedback(inputId=wh, session=session)
  if(nchar(this_message)>0 & eval==TRUE) {
    this_message <- paste0("<small>",this_message,"</small>")
    shinyFeedback::showFeedback(inputId=wh, text=this_message,color=color)
  }
  return(eval)
}

#' Copy data to clipboard
#'
#' @title avsh_clipboard
#' @description Copies a data.frame to the clipboard, with a status message if relevant
#' @param x A `data.frame` or equivalent.
#' @param title String to add to a message printed if relevant
#' @returns Nothing
#' @import clipr
#' @export
avsh_clipboard <- function(x,title="") {
  if(the_av$autocopy) {
    write_clip(as.data.frame(x))
    message_if_green(the_av$verbose,"to Clipboard: ",title," w/ ",nrow(x)," rows")
    quick_message("Data copied to Clipboad")
  }
}

#' Set an av_runShiny Tab Title
#'
#' @title avsh_set_tabtitle
#' @description Sets the title for the Details tab
#' @param newtext (default"DETAIL") What to name the tab as
#' @param tabnm (default "detail") inputID of relevant tab
#' @param makefocus (default: TRUE) Upon setting the tab title, select the tab.
#' @returns Nothing
#' @importFrom shinyjs runjs
#' @export
avsh_set_tabtitle <- function(newtext="DETAIL",tabnm="detail",makefocus=TRUE) {
  shpf <- sprintf('$(\'#inTabset li a[data-value="%s"]\').text("%s");',tabnm,newtext)
  if(makefocus==TRUE) av_set_defaults("starttab",tabnm)
  shinyjs::runjs(shpf)
}

#' Return av_runShiny data and states
#'
#' @name av_state_interface
#' @title av_state_interface
#' @description retrieves internal data state of [av_runShiny()]
#' `dump_state(typegrep="*")`
#' `dump_inv(invgrep="*")`
#' `dump_assetgroups()`
#' `dump_captured(todo="byfunction")`
#' `av_shiny_px()`
#' @param typegrep : Grep string for internal state parameters
#' @param todo : One of c("byfunction","pxhist",any av function name)
#' @param invgrep : A regular expression string
#' @returns data.table with desired data.
#' @seealso [av_runShiny()]
#' @examples
#' \dontrun{
#' `dump_state()`
#' `dump_inv()`
#' `dump_av_funcs()`
#' `dump_assetgroups()`
#' `dump_captured(todo="byfunction")`
#' }
#' @export
dump_state <- function(typegrep="*") {
  classtype=nm=NULL
  outdump<-data.table()
  for (x in ls(envir=the_av)) {
    toget <- get(x,envir=the_av)
    type <- class(toget)
    if(any(grepl(typegrep,type))) {
      if("data.frame" %in% type) {
        toget<-paste0("<<data.frame>> with ",nrow(toget), " rows")
      }
      if("list" %in% type) {
        toget<-paste0("<<list>> with ",length(toget), " items")
      }
      if("POSIXct" %in% type) { # KILLER
        toget<-as.character(toget)
      }
      outdump<-rbindlist(list(outdump,data.table(nm=x,classtype=type[1], toget=toget)),ignore.attr=TRUE,fill=TRUE)
    }
  }
  # Comment out after creating vignettes
  #outdump[nm=="avapikey",]$toget<-"Hidden"
  #-------------------
  return(outdump[order(classtype,nm)])
}

#'
#' @rdname av_state_interface
#' @export
dump_inv <- function(invgrep="*") {
  return(the_av$pxinv[grepl(invgrep,symbol,ignore.case=TRUE),])
}

#'
#' @rdname av_state_interface
#' @export
dump_assetgroups <- function() {
  return(the_av$assetgroups[,.(tickers=paste0(.SD$ticker,collapse=" ")), by=.(listnm)])
}

#'
#' @rdname av_state_interface
#' @export
dump_av_funcs <- function() {
  return(the_av$avsh_funcs)
}

#'
#' @rdname av_state_interface
#' @export
dump_captured <- function(todo="byfunction") {
  nr=fn=load_ts=NULL
  if(is.null(the_av$av_download)) { return("No Data downloaded")}
  if(todo=="byfunction") {
    rtn <- data.table(fn=names(the_av$av_download))[,nr:=nrow(the_av$av_download[[fn]]), by=.I][]
  }
  if(todo=="pxhist" & "TIME_SERIES_DAILY_ADJUSTED" %in% names(the_av$av_download)) {
    rtn <- the_av$av_download[["TIME_SERIES_DAILY_ADJUSTED"]][,
                                                              .(lastpx=last(close), lastts=max(load_ts), mindate=min(timestamp), maxdate=max(timestamp)), by=.(symbol)]
  }
  if(todo %in% names(the_av$av_download)) {
    tkeys <- setdiff(key(the_av$av_download[[todo]]),s("contractID;timestamp;timestamp"))
    rtn <- the_av$av_download[[todo]][,.(n=.N,lastts=max(load_ts)),by=tkeys]
  }
  return( rtn )
}

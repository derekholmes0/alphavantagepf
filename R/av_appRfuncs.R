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


# =======================================================================================================
#' App database functions
#'
#' @name av_add_analytic
#' @description Adds a user-defined function to the av Shiny app
#' @param runcode Code string user must run to call the function.
#' @param func_name Name of function run when analytic is called.  If an empty string is supplied, the runcode will be de-registered.
#' @param helpstr (default: "user function"): A string comment to ad to the av.h (help) command
#' @param focus (default: "MAIN")  String with tab name to set focus to when command is run
#' @param category (default "USER") A string with a category used to sort function when help is called.
#' @returns String message with success or failure of function addition.
#' @seealso [av_runShiny()]
#' @details When the [av_runShiny()] app is run, users can call functions to provide analytics based on asset strings in the command line.
#' This function allows users to add their own analytics by registering a function which takes, as inputs
#'  1. `todo`: The command line and any subsequent parameters as a space delimited string
#'  2. `rv`: Reactive values supplied by the Shiny app.  In particular the parameter `rv$istr1` contains the semicolon delimited set of assets
#'  prior to the command invocation.
#' The registered function should return a (possibly named, see vignette)
#' list containing one or more `gt()` tables, `dygraphs()`, or `ggplots()` to be displayed when the command is run.  See vignette for specfic details
#'
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
#'
#' @export
av_add_analytic <- function(runcode,func_name,helpstr="user function",focus="MAIN",category="USER") {
  runcode=toupper(runcode)
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
  new_analytics <- data.table(category=category,runcode=runcode, func_src="user", func_name=func_name, focus=focus, helpstr=helpstr)
  the_av$avsh_funcs <- DTUpsert(the_av$avsh_funcs,new_analytics,keys=c("runcode"),fill=TRUE)
  save_avs_state("all",msg=paste0("Add FUnction ",runcode))
}

# =======================================================================================================
# ----------------------- Exported Shiny Functions
#' @name quick_message
#' @description Displays a message underneath an input box
#' @param wh inputID for shiny element to put a message underneath of.  See Documentation and/or Code
#' @param this_message (default "")  A text message to  be used. If empty string, the current message is cleared.
#' @param eval (default TRUE) OPtional parameter to suppress execution.
#' @param color Optional text color
#' @returns logical value of `eval`
quick_message <- function(wh,this_message="",eval=TRUE,color="#1f78b4") {
  shinyFeedback::hideFeedback(inputId=wh)
  if(nchar(this_message)>0 & eval==TRUE) {
    this_message <- paste0("<small>",this_message,"</small>")
    shinyFeedback::showFeedback(inputId=wh, text=this_message,color=color)
  }
  return(eval)
}

#' @name avsh_clipboard
#' @description Copies a data.frame to the clipboard, with a status message if relevant
#' @param x A `data.frame` or equivalent.
#' @param title String to add to a message printed if relevant
#' @returns Nothing
#' @import clipr
avsh_clipboard <- function(x,title="") {
  if(the_av$autocopy) {
    write_clip(as.data.frame(x))
    message_if_green(the_av$verbose,"to Clipboard: ",title," w/ ",nrow(x)," rows")
    quick_message("istr1","Data copied to Clipboad")
  }
}

#' @name avsh_set_tabtitle
#' @description Sets the title for the Details tab
#' @param newtext (default"DETAIL") What to name the tab as
#' @param tabnm (default "detail") inputID of relevant tab
#' @returns Nothing
#' @importFrom shinyjs runjs
avsh_set_tabtitle <- function(newtext="DETAIL",tabnm="detail",makefocus=TRUE) {
  shpf <- sprintf('$(\'#inTabset li a[data-value="%s"]\').text("%s");',tabnm,newtext)
  if(makefocus==TRUE) av_set_defaults("starttab",tabnm)
  shinyjs::runjs(shpf)
}
# ====================================================================================================
#' Exported Internal  internal state functions
#'
#' @name dump_state
#' @description Prints internal data state of [av_runShiny()]
#' `dump_state(typegrep="*")`
#' `dump_inv()`
#' `dump_assetgroups()`
#' `dump_captured()`
#' @param typegrep : Grep string for internal state parameters
#' @param returngt : Return GT table
#' @param todo : One of c("byfunction","pxhist",any av function name)
#' @param rv : An isolated list of av_shiny parameters
#' @returns data.table with desired data.
#' @seealso [av_runShiny()]
#' @examples
#' \dontrun{
#' `dump_state()`
#' `dump_inv()`
#' `dump_av_funcs()`
#' `dump_assetgroups(returngt=TRUE)`
#' `dump_captured(todo="byfunction")`
#' `inv_rv(rv)`
#' }
#'
#' @rdname dump_state
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
      outdump<-rbindlist(list(outdump,data.table(nm=x,classtype=type[1], toget=toget)),ignore.attr=TRUE,fill=TRUE)
    }
  }
  # Comment out after creating vignettes
  #outdump[nm=="avapikey",]$toget<-"Hidden"
  #-------------------
  return(outdump[order(classtype,nm)])
}

#' @rdname dump_state
#' @export
inv_rv <- function(rv) {
  tnames <- names(rv)
  tclass <- sapply(tnames, \(x) paste0(class(rv[[x]]),sep=" "))
  tres <- rv[tnames]
  return(data.table(inputId=tnames, type=tclass, res=tres))
}


#' @rdname dump_state
#' @export
dump_inv <- function() {
  return(the_av$pxinv)
}

#' @rdname dump_state
#' @export
dump_assetgroups <- function(returngt=TRUE) {
  return(the_av$assetgroups[,.(tickers=paste0(.SD$ticker,collapse=" ")), by=.(listnm)])
}

#' @rdname dump_state
#' @export
dump_av_funcs <- function() {
  return(the_av$avsh_funcs)
}

#' @rdname dump_state
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

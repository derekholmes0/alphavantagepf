#' Add Price or Time Series Data
#'
#' @name av_add_px
#' @title Add or download Price or Time Series Data
#' @description Adds price data to [av_runShiny()] internal data.
#' @param indta (default: NULL) A data.frame with the following minimal columns: `c(symbol,timestamp,close)`.
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
  if(!is.null(assettypes)) {
    assettypes <- as.data.table(assettypes)[,symbol:=toupper(symbol)] }
  if(!is.null(indta)) {
    indta <- as.data.table(indta)
    firstdate <- find_col_bytype(indta,lubridate::is.instant)
    if (is.null(firstdate)) {
      stop("av_add_data: Need a timestamp column")
    }
    indta <- data.table(indta)[,symbol:=toupper(symbol)][]
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
#' av_add_earn(equitylist=data.table(symbol=c("IBM","GS")))
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
    message_if(verbosity() && verbose,"Loading avShiny Internal data.  Use dump_state() to see what's available")
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
#' @param delay_save_state (default: FALSE) Do not save state to cache files.  Used for speed optimization
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
av_add_analytic <- function(runcode,func_name,helpstr="user function",focus="MAIN",delay_save_state=FALSE) {
  runcode=toupper(runcode)
  msg <- paste0(" function ",func_name," to Command Line functions as code ",runcode)
  if(!exists("avsh_funcs",envir=the_av)) {  av_load_shinydata(verbose=FALSE) }
  if( toupper(runcode) %in% the_av$avsh_funcs$runcode) {
    if( nchar(func_name)<=0) {
      msg <- paste0(runcode, " removed from function list")
      the_av$avsh_funcs <- the_av$avsh_funcs[!runcode==runcode,]
      save_avs_state("all",msg="Remove function")
    }
    else {
      msg <- paste0(runcode, " already registered, Function code replaced")
    }
  }
  if( nchar(func_name)<=0) {
    message_if_red(TRUE,"Invalid function name; skipping operation")
    return()
  }
  new_analytics <- data.table(category="user",runcode=runcode, func_src="user", func_name=func_name, focus=focus, helpstr=helpstr)
  the_av$avsh_funcs <- DTUpsert(the_av$avsh_funcs,new_analytics,keys=c("runcode"),fill=TRUE)
  if(!delay_save_state) {   save_avs_state("all",msg=paste0("Add FUnction ",runcode)) }
  return(paste0("av_add_analytic: ",msg," at ",format(Sys.time(),"%d-%H:%M%:S")))
}

#' Add a set of analytics from a code direcory
#'
#' @name av_runShiny_addFunctions
#' @title Add experimental functions from a given directory
#' @description Adds analytics in code taken from single directory.
#' @param fun_dir Directory containing functions with input signatures to add
#' @returns Nothing
#' @details  Each file will be read in the specified directory and any function with a valid signature will be added to the list of available functions in the shiny app.
#' The function signature is a call to the function with a single argument "signature" which returns a list of three items: 1) a short name for the function, 2)
#' the function name, and 3) a help string for the function.  If those conditions obtain, the function will be added to [av_runShiny()]
#' @export
av_runShiny_addFunctions <- function(fun_dir="c:/d/src/R/avpfShinyFuncs/avpfshinyFuncs/R") {
  message("av_runShiny_addFunctions v 0.2 ")
  allfiles <-list.files(fun_dir,pattern="*\\.r",ignore.case=TRUE,full.names=TRUE)
  is_fn_def_with_params <- function(e, params = c("todo", "rv")) {
    if (!(is.call(e) &&
          as.character(e[[1]]) %in% c("<-", "=", "<<-") &&
          is.call(e[[3]]) &&
          identical(e[[3]][[1]], as.name("function")))) {
      return(FALSE)
    }
    fn_formals <- e[[3]][[2]]          # pairlist of formal args
    identical(names(fn_formals), params)
  }

  check_fn <- function(thisfn) {
    exprs <- parse(thisfn)
    matching_exprs <- Filter(is_fn_def_with_params, as.list(exprs))
    fn_res <- invisible(sapply(matching_exprs, eval, envir = environment()))
    fn_names <- sapply(matching_exprs, function(e) as.character(e[[2]]))
    nadded<- sapply(fn_names, \(nm) {
      toadd <- do.call(nm,list("signature"))
      if( length(toadd) != 3 ) { # Add if signature is returned
        message_if_red(TRUE,paste0("Function ",nm," does not have a valid signature"))
        return(0)
      } else {
        message( av_add_analytic(toadd[[1]],toadd[[2]],helpstr=toadd[[3]], delay_save_state=TRUE) )
        return(1)
      }
    })
    return(nadded)
  }
  noverall <- sapply(allfiles, \(thisfn) check_fn(thisfn))
  save_avs_state("the",msg="Added FUnction(s) using av_runShiny_addFunctions")
  return(paste("Added ",sum(unlist(noverall))," functions from",fun_dir))
}

# ==========================================================================================================
# OPTIONS
# ==========================================================================================================


#' Manage option data
#'
#' @title av_add_options
#' @description Manages option database and creates vol surfaces
#' @param todo What to do to add options.  Choices are in the table below
#' |`todo`|Description|
#' |:----:|:-------------------|
#' |`update`|Download and add or update full option runs by symbol over periods in `dtstr`|
#' |`inventory`|Create/update inventory of options that have been downloaded|
#' |`reconstruct_inventory`|Reconstruct inventory of all options that have been downloaded|
#' |`iv`|Updates internal data.table (`eqopt_iv` of Implied term structures by symbol and date|
#' |`copy_external`|Copies identical directory structures from `external_path` to siny app internal data|
#' @param dtstr (default `"-1w::"`)  How far back to load options. ONly data needed within timeframe is downloaded, unless `replace_data=TRUE`
#' @param symbols (default: NULL) List of symbols to download
#' @param freq (Default `"d"`) Granularity of data downloaded ("d" for daily, "w" for weekly, "m" for monthly)
#' @param replace_data (Default `FALSE`) Update or replace data
#' @param baddates_limits (default 3): Consecutive business days of null data beyond which options are assumed not to exist.
#' @param verbosity (default `"basic"`) What to display as data is added. Options may be comma delimited and are
#' |`verbosity`|Description|
#' |:----:|:-------------------|
#' |`basic`|Most basic information|
#' |`timing`|Timing information|
#' |`iv`|Implied volatility surface calculation progress|
#' |`ivprobs`|Implied volatility surface calculation errors (e.g. not enough data, etc.)|
#' @param external_path (default NULL) Path from which to replace existing data
#' @returns nothing
#' @seealso [av_runShiny()]
#' @details **This function can take a long time to run**.  Full option quotes are stored by symbol in arrow/parquet format by symbol
#' day, and contract.
#' * Unless `replace_data==TRUE`, only dates and symbols that need to be downloaded will.
#' * Except the `OS S` function, all analytics run in the app require options data to be downloaded using this function.
#' @importFrom progress progress_bar
#' @importFrom dplyr filter
#' @importFrom fs dir_copy
#' @export
av_add_options <- function(todo, dtstr="-1w::", symbols=NULL, freq="d", replace_data=FALSE, baddates_limits=3, verbosity="basic",
                           external_path=NULL) {
  moneyn=yrwk=tfreq=ts=expcode=ov=open_interest=ask=bid=mark=iv=NULL
  eo_path <- paste0(the_av$cachedir,"/eqopt")
  closest_deltamap <- data.table(dcat=s("(-0.9,-0.75];(-0.75,-0.5];(-0.5,-0.25];(-0.25,-0.1];(-0.1,-0.05];(-0.05,0];(0,0.05];(0.05,0.1];(0.1,0.25];(0.25,0.5];(0.5,0.75];(0.75,0.9];NA"),
                         moneyn=s("P75;P50;P25;P10;P5;P0;C0;C5;C10;C25;C50;C75;NA"),
                         cutlevel=c(-0.9,-0.75,-0.5,-0.25,-0.1,-0.05,0,0.05,0.1,0.25,0.5,0.75,0.9))
  deltamap <- data.table(moneyn=s("P90;P75;P50;P45;P40;P35;P25;P15;P10;P5;C5;C10;C15;C25;C35;C45;C50;C75;C90;NA"),
                         cutlevel=c(-0.9,-0.75,-0.5,-0.45,-0.4,-0.35,-0.25,-0.15,-0.1,-0.05,0.05,0.1,0.15,0.25,0.35,0.45,0.5,0.75,0.9,NA))
  dtoexpmap <- data.table(dtoexp=c(7, 14, 30,60,90), expcode=s("CWK_1;CWK_2;CMO_1;CMO_2;CMO_3"))
  if(!dir.exists(eo_path)) {
    stop("Please Create ",eo_path," first.  CRAN would prefer the package does not.")
  }
  if(!exists("eqoptinv",envir=the_av)) {
    tsymbols <- symbols
    if(is.null(symbols)) {
      message_if_red(TRUE,"No symbols specified and we're just starting out, so adding SPY only")
      tsymbols <- c("SPY")
    }
    eqoptinv <- list("datelist"=data.table(),"inv"=data.table(),"last"=data.table())
    ds <- NULL
  }
  else {
    eqoptinv <- the_av$eqoptinv
    tsymbols <- symbols %||%  eqoptinv[["inv"]]$symbol
    ds <- arrow::open_dataset(eo_path,partitioning=c("symbol"))
  }
  eqoptdb_keys=s("symbol;expcode;contractid;ts")
  freqmap <- data.table(tfreq=s("d;w;m"),freqcol=s("DT_ENTRY;yrwk;yrmo"))
  t_dates <- dtmap[isbday==TRUE & DT_ENTRY<Sys.Date(),.(DT_ENTRY,yrwk,yrmo)] |> narrowbydtstr(dtstr)
  t_dates<- t_dates[,.SD[.N], by=get(freqmap[tfreq==freq,]$freqcol)][order(-DT_ENTRY)]
  dt_todo_all <- CJ(symbol=tsymbols,ts=t_dates$DT_ENTRY,sorted=FALSE)
  dt_todo_raw <- copy(dt_todo_all)
  if(nrow(eqoptinv[["datelist"]])>0) {
    dt_todo_raw <- eqoptinv[["datelist"]][dt_todo_all,on=.(symbol,ts)][is.na(n) | replace_data==TRUE]
  }
  timelist <- list()
  setTimeStamp <- function(x) {  timelist[[x]] = Sys.time()}
  if(todo=="todo") {
    return(dt_todo_raw)
  }
  setTimeStamp("start")
  if(todo=="inventory") {
    eqoptinv <- optdb_inventory(ds)
    the_av$eqoptinv <- copy(eqoptinv)
    return("Inventory created")
  }
  if(todo=="iv") { # Takes over an hour for small set
    eqopt_iv <- the_av$iv %||% data.table()
    interp_cols <- s("iv;strike;delta;theta")
    note_probs <- grepl("ivprobs",verbosity)
    # thissymbol="IBM"; dtset <-c(as.Date("2026-09-24"),as.Date("2026-09-22"), as.Date("2026-09-21"))
    one_symbol <- function(thissymbol,dtset, minobs=5) {
      dtoexp=iv=ivar=cutlevel=NULL
      message_if_green(grepl("all",verbosity),"Implied Vols:",thissymbol," from ",as.Date(min(dtset))," to ",as.Date(max(dtset)), " (",length(dtset)," days)")
      u1 <- ds |> dplyr::inner_join(data.table(ts=dtset)[,symbol:=thissymbol][], by=c("symbol","ts")) |> dplyr::collect() |> as.data.table()
      one_iv <- function(indt) {
        # Approach 3: loess
        if( nrow(indt)<=minobs ) {
          message_if_red(note_probs,"iv: Need more than ",minobs, "obs for e.g. ",indt[[1,"contractid"]])
          return(data.table()) }
        oo <- tryCatch( {
          cjset = CJ(dtoexp=dtoexpmap$dtoexp, delta=deltamap$cutlevel)
          if(max(indt$dtoexp)<=30) { return(data.table()) } # Not enough Expirations
          iv.lo <- stats::loess(ivar ~ dtoexp + delta, data=indt)
          k.lo <- stats::loess(strike ~ dtoexp + delta, data=indt)
          oo <- cbindlist(list(cjset, data.table(ivvar=stats::predict(iv.lo,cjset), strike=stats::predict(k.lo,cjset))))
          oo <- dtoexpmap[oo,on=.(dtoexp)]
          oo <- deltamap[oo, on=.(cutlevel=delta)]
          oo$moneyn=as.factor(oo$moneyn)
          oo <- oo[,iv:=sqrt(ivar)][,ivar:=NULL]
          oo <- oo[!is.na(iv)][,cutlevel:=NULL][]
        }, error = function(e) { data.table()}, finally=data.table()
        )
        return(oo)
      }
      #return(u1[,one_iv(.SD), by=.(symbol,ts,expcode,dtoexp,expiration)])
      return(u1[,one_iv(.SD), by=.(symbol,ts)])
    }
    todo_iv <- dt_todo_all
    if(replace_data==FALSE & nrow(eqopt_iv)>0) {
      todo_iv <- dt_todo_all[!eqopt_iv[,.N,by=.(symbol,ts)], on=.(symbol,ts)]
    }
    pb <- progress::progress_bar$new(format = paste0("AV IV  [:bar] :percent [:elapsed]"), total=length(tsymbols), clear = FALSE, width= 60)
    lastdt_iv <- lapply(tsymbols, \(x) {
      pb$tick();
      message_if_green("basic" %in% verbosity, "Calculating IMplied Vol surfaces for ",x);
      one_symbol(x,todo_iv[symbol==x,]$ts)
      })
    the_av$iv <-DTUpsert(eqopt_iv,rbindlist(lastdt_iv,fill=TRUE),s("symbol;ts;expcode;moneyn"),fill=TRUE)
    save_avs_state("px",msg="Implied surfaces created")
  }
  if(todo=="iv_nearest") {
    eqopt_ivn <- the_av$iv_nearest %||% data.table()
    deltaset <- closest_deltamap$cutlevel
    deltalabels <- closest_deltamap[!(moneyn=="NA"),]$moneyn

    # thissymbol="IBM"; dtset <-c(as.Date("2026-09-24"),as.Date("2026-09-22"), as.Date("2026-09-21"))
    one_symbol_n <- function(thissymbol,dtset) {
      message_if_green(grepl("all",verbosity),"Implied Vols:",thissymbol," from ",as.Date(min(dtset))," to ",as.Date(max(dtset)), " (",length(dtset)," days)")
      u1 <- ds |> dplyr::inner_join(data.table(ts=dtset)[,symbol:=thissymbol][], by=c("symbol","ts")) |> dplyr::collect() |> as.data.table()
      # Original approach: Just find option that is closest to the delta
      u1 <- u1[,moneyn:=cut(delta,deltaset,labels=deltalabels)][!is.na(moneyn)]
      thisiv_call <- u1[type=="call",][,.SD[.N], by=.(symbol,ts,expcode,type,moneyn)]
      thisiv_put <- u1[type=="put",][,.SD[1], by=.(symbol,ts,expcode,type,moneyn)]
      return(rbindlist(list(thisiv_call,thisiv_put)))
      }
    todo_iv <- dt_todo_all
    if(replace_data==FALSE & nrow(eqopt_ivn)>0) {
      todo_iv <- dt_todo_all[!eqopt_ivn[,.N,by=.(symbol,ts)], on=.(symbol,ts)]
    }
    pb <- progress::progress_bar$new(format = paste0("AV IV NEAR  [:bar] :percent [:elapsed]"), total=length(tsymbols), clear = FALSE, width= 60)
    lastdt_iv <- lapply(tsymbols, \(x) { pb$tick(); one_symbol_n(x,todo_iv[symbol==x,]$ts) } )
    eqopt_ivn <- DTUpsert(eqopt_ivn,rbindlist(lastdt_iv),s("symbol;ts;expcode;moneyn;type"),fill=TRUE)
    the_av$iv_nearest <- eqopt_ivn
    save_avs_state("px",msg="Implied nearest options created")
  }
  if(todo=="update") {
    dtall <- data.table()
    t_max_requests_per_min<- as.numeric(the_av$max_requests_per_min)
    est_end_time <- Sys.time()+60*nrow(dt_todo_raw)/t_max_requests_per_min
    message_if_red(TRUE," Estimated end time data collection from AV: ", format(est_end_time,"%H:%M"))
    for(s in tsymbols) {
      setTimeStamp("start_sym")
      indates <- dt_todo_raw[symbol==s,]$ts
      if(length(indates)>0) {
        message_if_red(grepl("basic|tim",verbosity),"Option data to get:",s," from ",as.Date(min(indates))," to ",as.Date(max(indates)), " (",length(indates)," days)",
                      " (",round(est_end_time-Sys.time(),0)," mins to go)")
        max_time <-  round(length(indates)/t_max_requests_per_min,2)
        pb <- progress::progress_bar$new(format = paste0("AV Options for ",s," [:bar] :percent [:elapsed] vs ",max_time," mins max"),
                               total = length(indates), clear = FALSE, width= 60)
        dtnew <- data.table() #much as I love lapply here, need to cancel early if options didn't trade
        nconseq_nulls <- 0
        for(dt in indates) {
          opt_for_one_date <- getData.optchain_all(s,indate=as.Date(dt),verbose=FALSE)
          pb$tick()
          nconseq_nulls <- nconseq_nulls + fifelse(nrow(opt_for_one_date)<=0,1,0)
          if(nconseq_nulls>baddates_limits) {
            message_if_red(TRUE,"mange_optdb_arrow: ",s," has ",nconseq_nulls," conseq days with no options, skipping the rest")
            break
          }
          dtnew <- rbindlist(list(dtnew,opt_for_one_date))
        }
        dtall <- rbindlist(list(dtall,dtnew),fill=TRUE)
        setTimeStamp("end_sym")
        message_if(grepl("tim",verbosity),"Option Symbol: ",s," gathered in ",print_time(timelist,"start_sym","end_sym"))
      }
    }
    if(nrow(dtall)<=0) {
      message_if_red(TRUE," Symbols ",paste(tsymbols,collapse=","),": NOTHING TO UPDATE .. ")
      return()
    }
    setTimeStamp("start_upsert")
    message_if_red(grepl("basic",verbosity),"Option update: Adding ",nrow(dtall)," rows to partitioned parquet set")
    addedopts <- upsert_DT_arrow( dtall, eo_path, dst=ds, partition_keys="symbol",dt_keys=c("ts","contractid"))
    setTimeStamp("end_upsert")
    message_if_green(grepl("tim",verbosity),"Returned ",nrow(addedopts)," new options, refreshing inventory, took ",print_time(timelist,"start_upsert","end_upsert"))
  }
  # Create invntory structures
  if(todo=="update" | todo=="inventory" | todo=="reconstruct_inventory") {
    setTimeStamp("end_upsert")
    ds <- arrow::open_dataset(eo_path,partitioning=c("symbol")) # REopen dataset
    if(todo=="reconstruct_inventory") {
      eqoptinv <- optdb_inventory(ds)
    }
    else {
      eqoptinv_toadd <- optdb_inventory(ds, tsymbols)
      eqoptinv[["datelist"]] <- DTUpsert(eqoptinv[["datelist"]], eqoptinv_toadd[["datelist"]], c("symbol","ts"),fill=TRUE)
      eqoptinv[["inv"]] <- DTUpsert(eqoptinv[["inv"]], eqoptinv_toadd[["inv"]], c("symbol"),fill=TRUE)
      eqoptinv[["last"]] <- DTUpsert(eqoptinv[["last"]], eqoptinv_toadd[["last"]], c("symbol"),fill=TRUE)
    }
    the_av$eqoptinv <- copy(eqoptinv)
    save_avs_state("all",msg="Implied surfaces created")
    setTimeStamp("end_inventory")
    message_if_green(grepl("tim",verbosity),"Updated option inventory in ",print_time(timelist,"end_upsert","end_inventory"))
  }
  # Get a summarized data.table of inventory
  if(todo=="getinv") {
    inv1 <- the_av$eqoptinv$last[expcode=="MO_1" & type=="call",][,.SD[which.min(abs(delta-0.5))], by=.(symbol)][,
                  .(symbol,iv_1mo50d=iv,oi_1mo50d=open_interest,bopct=round(100*(ask-bid)/mark,0))]
    inv2 <- the_av$eqoptinv$inv
    inv3 <- the_av$iv[expcode=="MO_1"][,.(niv=.N,mindt_iv=min(ts),maxdt_iv=max(ts)), by=.(symbol)]
    inv4 <- inv3[inv2[inv1,on=.(symbol)],on=.(symbol)]
    setcolorder(inv4,unique(c(names(inv1),names(inv2),names(inv3))))
    return(inv4)
  }
  if(todo=="copy_external" && dir.exists(external_path)) {
    fs::dir_copy(path=external_path,new_path=eo_path,overwrite=TRUE)
    av_add_options("inventory",dtstr=dtstr)
    av_add_options("iv",dtstr=dtstr)
  }
  return()
}

#[active][otm|itm][[front][mo|qtr]]
# optchain = getData.optchain("QQQ",opttypegrep="put",grepstring="activeotmfrontmo",pctlimit=0.25)

#' @noRd
getData.optchain_all <- function(ticker,spot=NULL,expiration=NULL,rtn="",indate=NULL,verbose=FALSE,delay=0) {
  iv=ts=strike=mark=NULL
  timelist = list()
  setTimeStamp <- function(x) {  timelist[[x]] = Sys.time()}
  chaindt = data.table()
  chaindt_bad = list()
  setTimeStamp("optchainstart")
  chaindta = alphavantagepf::av_get_pf(ticker,"HISTORICAL_OPTIONS",date=indate,verbose=FALSE,delay=delay)
  if( message_if(nrow(chaindta)<=0, paste0("AV_optchain(",ticker,"/",indate,") empty return")) ) {  return(data.table()) }
  if( message_if("value_str" %in% colnames(chaindta),
                 paste("AV_optchain(",ticker,"/",indate,") err:", chaindta[variable %in% c("message","Information"),]$value_str) )) {  return(data.table()) }
  setnames(chaindta,colnames(chaindta),tolower(colnames(chaindta)),skip_absent=TRUE)
  setnames(chaindta,c("implied_volatility","date"),c("iv","ts"),skip_absent=TRUE)
  # Now get last date and spot
  new_indate <- max(chaindta$ts)
  if(!is.numeric(spot)) {
    spot <- av_load_shinydata("pxd")[symbol==ticker & timestamp == new_indate,]$close
  }
  chaindt = chaindta[,let('iv'=100*as.numeric(iv),'dtoexp'=as.numeric(difftime(expiration,as.Date(ts),units="days"),'spot'=spot))]
  setTimeStamp("optchainend")

  # Add expiration codes
  expmap <- opt_expmap(indate,alldates=sort(unique(chaindt$expiration)))
  chaindt <- expmap[chaindt,on=.(expiration)][,let(ITM=fifelse(type=="call",strike>=mark,strike<=mark))]

  setTimeStamp("end")
  message_if(verbose,"optchain(",ticker,"/",new_indate,") returns ",length(unique(chaindt$expiration)), " expirations for ",nrow(chaindt)," options: ",print_time(timelist,"start","end"," total"),
             print_time(timelist,"optchainstart","optchainend","API"),
             print_time(timelist,"greeks_start","greeks_end","Greeks"))
  return(chaindt)
}

#' @noRd
opt_expmap <- function(indate,alldates=NULL,maxdate=NULL) {
  optexpstr=NULL
  maxdate <- maxdate %||% indate+10*365
  chaindt0 = dtmap[between(DT_ENTRY,indate,maxdate) & nchar(optexp)>0,.(expiration=DT_ENTRY,optexp)]
  if(is.data.frame(alldates)) {
    chaindt0 <- chaindt0[alldates,on=.(expiration)][expiration<=max(alldates$expiration)]
  }
  chaindt0 <- chaindt0[order(optexp,expiration)][,.(expiration,optexpstr=paste0(optexp,"_",.I-min(.I)+1)),by=.(optexp)]
  return(chaindt0[,.(expiration,expcode=toupper(optexpstr))])
}

#' @noRd
#' @importFrom dplyr group_by inner_join count
optdb_inventory <- function(ds,symbolset=NULL) {
  ts=cnt=nYrs=max_DT_ENTRY=NULL
  if(is.null(symbolset)) {
    dsa <- ds
  }
  else {
    dsa <-ds |> dplyr::inner_join(data.frame(symbol=symbolset))
  }
  eoinv3 <- dsa |> dplyr::group_by(symbol,ts) |> dplyr::count() |> as.data.table()
  eoinv1 <- eoinv3[,.(min_DT_ENTRY=min(ts),max_DT_ENTRY=max(ts), cnt=.N, nYrs=as.numeric(max(ts)-min(ts))/365), by=.(symbol)]
  eoinv1 <- eoinv1[,let(nperYr=cnt/nYrs)][]
  eoinv2 <- ds |> dplyr::inner_join(eoinv1[,.(symbol,ts=max_DT_ENTRY)],.by=c("symbol","ts")) |> as.data.table()
  return(list("inv" =  eoinv1, "last"=eoinv2, "datelist"=eoinv3))
}

fix_optchains<- function() {
  eo_path <- paste0(the_av$cachedir,"/eqopt")
  ds <- arrow::open_dataset(eo_path,partitioning=c("symbol"))
  tsymbols <- the_av$eqoptinv[["inv"]]$symbol
  for(tsym in tsymbols) {
    message("... sym :",tsym, "start")
    optset <- ds |> filter(symbol==tsym) |> dplyr::collect() |> as.data.table()
    optset$expcode <- toupper(optset$expcode)
    addedopts <- upsert_DT_arrow( optset, eo_path, dst=ds, partition_keys="symbol",dt_keys=c("ts","contractid"))
    message("... sym :",tsym, "end")
  }
}


# ==========================================================================================================
# APP functions
# ==========================================================================================================

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
    message_if_green(verbosity(),"to Clipboard: ",title," w/ ",nrow(x)," rows")
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
#' @param trunc_length : (default: 35)  Maximumlength of character values returned.
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
dump_state <- function(typegrep="*", trunc_length=35) {
  classtype=nm=NULL
  outdump<-data.table()
  for (x in ls(envir=the_av)) {
    toget <- get(x,envir=the_av)
    type <- class(toget)
    if(any(grepl(typegrep,type))) {
      if("data.frame" %in% type) {
        toget<-paste0("<<data.table>> with ",sprintf("%8d",nrow(toget)), " rows")
      }
      if("list" %in% type) {
        toget<-paste0("<<list>> with ",length(toget), " items")
      }
      if("POSIXct" %in% type) { # KILLER
        toget<-as.character(toget)
      }
      if("character" %in% type) {
        toget <- paste0(substr(toget,1,trunc_length),fifelse(nchar(toget)>trunc_length,"...",""))
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

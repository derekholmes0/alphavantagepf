# =======================================================================================================
#' App database functions
#'
#' @importFrom fst read_fst write_fst
#' @importFrom lubridate is.instant
#' @import data.table
#'
#' @name av_add_data
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
#' av_add_data(av_get_pf("IBM","TIME_SERIES_DAILY_ADJUSTED"))
#' asset_df <- data.frame(symbol=c("HYG"),type=c("ETF"),currency=c("USD"), name=c("HY ETF"))
#' av_add_data(av_get_pf("HYG","TIME_SERIES_DAILY_ADJUSTED"), assettypes=asset_df)
#'
#' suppressMessages(require(quantmod))
#' ffdta <- as.data.table(quantmod::getSymbols("FEDFUNDS",src="FRED",auto.assign=FALSE))
#' ffdta <- ffdta[,.(DT_ENTRY=index,close=FEDFUNDS,adjusted_close=FEDFUNDS,symbol="FEDFUNDS")]
#' av_add_data(ffdta)
#' }
#'
#' @export
av_add_data <- function(indta,assettypes=NULL,delay=0) {
  restore_avs_state("all")
  firstdate <- find_col_bytype(indta,lubridate::is.instant)
  if (is.null(firstdate)) {
    stop("av_add_data: Need a timestamp column")
  }
  indta <- data.table(indta)
  setnames(indta,firstdate,"timestamp")
  check_min_colset(indta,s("symbol;timestamp;close;adjusted_close"))
  manage_epx(unique(indta$symbol),"-30y::",substitute_data=indta,substitute_symset=assettypes,force=TRUE,delay=delay)
  save_avs_state("all")
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
  save_avs_state("all")
}

#' @noRd
update_tickerlists <- function(reallydoingthis=TRUE,reset=FALSE) {
  from_currency=to_currency=list_ts=NULL
  if(reallydoingthis==FALSE) { return() }
  if(reset==TRUE) {
    the_av$tickerlist <- data.table()
    message_if_red(the_av$verbose,"Resetting ticker lists  at ",Sys.time())
    }
  indexlist <- av_get_pf("","INDEX_CATALOG")[,type:="Index"][]
  cryptolist <- avsd$crypto_list[,.(symbol=paste0(from_currency,"/",to_currency),type="Crypto")][,name:=symbol]
  indexlist <- rbindlist(list(indexlist,cryptolist),use.names=TRUE,fill=TRUE)[,list_ts:=Sys.Date()][]
  the_av$tickerlist <- DTUpsert(the_av$tickerlist,indexlist,c("symbol"))
  message_if_red(the_av$verbose,"Reconstructed index and crypto lists at ",Sys.time())
  save_avs_state("all") # must use all with any inventory data
}

# inv_fn/pxinv is a list of all user-level data.
# Currently contains: assetgroups (ex assetlist), tickerlist (ex indexlist), pxinv
# ---------------------------------------------

#' @noRd
restore_avs_state <- function(todo="all",skip=FALSE,msg="") {
  pxinv=NULL
  if(skip) { return() }
  # Filledin dfaults before
  if(grepl("all|constants",todo) & file.exists(the_av$constants_fn)) {
    load(the_av$constants_fn, envir=the_av)
  }
  if(grepl("all|inv",todo) & file.exists(the_av$inv_fn)) {
    load(the_av$inv_fn)
    lapply(names(pxinv),\(x) assign(x,pxinv[[x]],envir=the_av))
  }
  if(grepl("all|px",todo) & file.exists(the_av$pxd_fn)) {
      the_av$pxd <- fst::read_fst(the_av$pxd_fn, as.data.table=TRUE)
  }
  if(nchar(the_av$av_dump_dir)>0) {
    avdatafn <- paste0(the_av$av_dump_dir,"/av_download.RD")
    if(grepl("all|capture",todo) & file.exists(avdatafn)) {
      message_if_green(the_av$verbose,"Loading cumulative capture data from ",avdatafn)
      load(avdatafn,envir=the_av)
    }
  }
  message_if_green(TRUE,"Restored state (",todo,") from ",the_av$cachedir, " ",msg)
}

#' @importFrom stats setNames
save_avs_state <- function(todo="all") {
  classtype=NULL
  exception_names <- s("pxd;pxinv")
  shortmsg <- ""
  if(grepl("all|px",todo)) {
    nonpx_names <-  dump_the()[classtype=="data.table"& !(nm %in% c("pxd")),]$nm
    pxinv <- setNames(lapply(nonpx_names,\(x) get(x,envir=the_av)), nonpx_names)
    save(pxinv,file=the_av$inv_fn)
    fst::write_fst(the_av$pxd,the_av$pxd_fn,compress=20) # pxd to fst
    shortmsg <- paste(shortmsg,"px,inv")
  }
  if(grepl("all|the",todo)) {
    #unames=names(the_av)[sapply(the_av, class) %in% c("logical","character","numeric","difftime")]
    unames <- setdiff(names(the_av),exception_names)
    save(list=unames,envir=the_av,file=the_av$constants_fn)
    shortmsg <- paste(shortmsg,"const")
  }
  message_if_green(the_av$verbose,"Save State (",todo,") or (",shortmsg," at ",Sys.time())
}

# epx_get_avfn : Which function to call given type
# --------------------------------------------------
epx_get_avfn <- function(intype,live=FALSE) {
  av_live=av_hist=NULL
  return(data.table(type=s("Equity;ETF;Index;FX;Crypto"),
                       av_hist=s("TIME_SERIES_DAILY_ADJUSTED;TIME_SERIES_DAILY_ADJUSTED;INDEX_DATA;FX_DAILY;DIGITAL_CURRENCY_DAILY"),
                       av_live=s("GLOBAL_QUOTE;GLOBAL_QUOTE;NOTAVAIL;FX_INTRADAY;CRYPTO_INTRADAY"))[type==intype,.(avf=fifelse(live,av_live,av_hist))]$avf)

}

# epx_fmt_to_hist : Convert quotes to same schema as historical data
# --------------------------------------------------
epx_fmt_to_hist <- function(inquote,intype,live=FALSE) {
  latestDay=high=low=volume=NULL
  if(live==FALSE & (intype=="Equity" | intype=="ETF")) {
    tortn <- inquote
  }
  else if(live==FALSE & intype=="Index") {
    tortn <- inquote[,.(symbol,timestamp=date,open,high,low,close,adjusted_close=close,volume=0,dividend_amount=0,split_coefficient=1)]
  }
  else if(live==FALSE & (intype=="FX" | intype=="Crypto")) {
    tortn <- inquote[,.(symbol,timestamp,open,high,low,close,adjusted_close=close,volume=0,dividend_amount=0,split_coefficient=1)]
  }
  else if(live==TRUE & (intype=="Equity" | intype=="ETF")) {
    tortn <- inquote[,.(symbol,timestamp=latestDay,open,high,low,close,adjusted_close=close,volume,dividend_amount=0,split_coefficient=1)]
  }
  else if(live==TRUE & (intype=="Index")) {
    tortn <- data.table()
  }
  else if(live==TRUE & (intype=="FX")) {
    tortn <- inquote[,.SD[.N]][,.(symbol,timestamp=as.Date(timestamp),open,high,low,close,adjusted_close=close)]
  }
  else {
    message_if_red(TRUE,"epx_fmt_to_hist invalid input combinations (",live,intype)
  }
  return(tortn)
}


# form_symset Finds or downloads the asset type for a given ticker.  Sucks that Alphavantage can't unify these
# form_symset(c("JBL","EEMA","NDX","USD/BRL","BTC/USD","FEDFUNDS"))

#' @importFrom stringr str_extract
form_symset <- function(tickers, force=FALSE, delay=0) {
  symbol=name=matchScore=list_ts=NULL
  alltickers=s(toupper(tickers))
  if(force==TRUE || nrow(the_av$pxinv)<=0) {
    newtickers <- alltickers
    symset <- data.table()
  }
  else {
    # Symbols we already  have
    symset<- data.table(symbol=s(tickers))[the_av$pxinv,on=.(symbol),nomatch=NULL]
    symset<- symset[,.(symbol,type,currency,name,matchScore,list_ts)]
    newtickers <- setdiff(alltickers,symset$symbol)
  }
  # -----------------------------------------Downloadable assets: KNown from ticker
  # Known Indices
  symnew_ix <- the_av$tickerlist[type=="Index",][data.table(symbol=newtickers),on=.(symbol),nomatch=NULL]
  if(nrow(symnew_ix)>0) {
    symnew_ix <- symnew_ix[,.(symbol,type,currency="USD",name,matchScore=1,list_ts)]
  }
  # New Crypto
  possible_fxcr <- grepv("[A-Z]/[A-Z]",newtickers)
  symnew_cryp <- the_av$tickerlist[type=="Crypto",][data.table(symbol=possible_fxcr),on=.(symbol),nomatch=NULL]
  if(nrow(symnew_cryp)>0) {
    symnew_cryp <- symnew_cryp[,.(symbol,type,currency=stringr::str_extract(symbol,"([A-Z]*)/",group=1),name,matchScore=1)]
    possible_fxcr <- setdiff(possible_fxcr,symnew_cryp$symbol)
  }
  # New currency Pairs
  possible_fxcr <- grepv("([A-Z]{3})/([A-Z]{3})",possible_fxcr,ignore.case=TRUE)
  symnew_fx <- data.table(symbol=possible_fxcr)[,.(symbol,type="FX",currency=substr(symbol,1,3),name=symbol,matchScore=1)]
  symnew_inferable <-  rbindlist(list(symnew_ix,symnew_fx,symnew_cryp),fill=TRUE,use.names=TRUE)
  newtickers=setdiff(newtickers,symnew_inferable$symbol)
  # -----------------------------------------Downloadable assets: Need a search
  # New equities/ETS
  symnew_eq <- rbindlist(lapply(newtickers, \(x) {
    z1=av_get_pf("","SYMBOL_SEARCH",keywords=x,delay=delay)
    if(nrow(z1)<=0) {
      message_if_red(TRUE,"Alphavantage cannot find  ",x,": May be a user data series")
      return(data.table())
    }
    else {
      return(z1[matchScore>=0.99,.(symbol=x,type,currency,name,matchScore,list_ts=Sys.Date())])
    }
  }))
  newtickers=setdiff(newtickers,symnew_eq$symbol)
  # -----------------------------------------Un Downloadable assets/ User Data
  symnew_user <- data.table()
  if( length(newtickers)>0 ) {
    symnew_user <- data.table(symbol=newtickers,type="user",currency="USD",matchScore=1,list_ts=Sys.Date())
  }
  # Collect all together
  symset <- rbindlist(list(symset,symnew_inferable,symnew_eq,symnew_user),fill=TRUE,use.names=TRUE)
  return(symset[])
}

# manage_epx only accepts more than one ticker if called with substitute_data
# mange_eps will download repeatedly before market opens, no real way to avoid it without time of day logic

#' @importFrom stats median
manage_epx <- function(inticker, dtstr, substitute_data=NULL, substitute_symset=NULL, addlive=FALSE, force=FALSE, delay=0.1) {
  symbol=beg_dt=medgap=NULL
  dtstoget <- gendtstr(dtstr,rtn="list") # Dates to get
  if(nrow(the_av$pxinv)>0 & is.null(substitute_data) & is.null(substitute_symset)) {
    edates <- the_av$pxinv[data.table(symbol=s(inticker)),on=.(symbol),nomatch=NULL]
    if(nrow(edates)>0) {
      earlystarts <- edates[beg_dt>dtstoget[1],]
      if(nrow(earlystarts)>0) {
        earlystarts <- paste0(earlystarts$symbol,collapse=" ")
        message_if(the_av$verbose,"av_one_px(",earlystarts,"): Start Date requested earlier than series start ")
      }
      dtstoget[1] <- min(edates$end_dt)
    }
  }
  # If exists, then check if data is up to date
  #   if it doesn't exist or is too old, use full download
  # Note that downloads will occur anyway if narket has not opened yet
  nbdays = nrow(dtmap[between(DT_ENTRY,dtstoget[1],dtstoget[2])])
  if(nbdays<=1 & !force & !addlive) {
    src <- "Cached"
  }
  else {
    if(is.data.table(substitute_data)) {
      dta <- data.table::copy(substitute_data)
      src <- "Added"
      tickers <- unique(substitute_data$symbol)
      if(is.data.table(substitute_symset)) {
        check_min_colset(substitute_symset,s("symbol;type;currency;name"))
        symset <- copy(substitute_symset)
      }
      else {
        symset <- form_symset(tickers,force=force,delay=delay)
      }
      symset = symset[data.table(symbol=unique(dta$symbol)), on=.(symbol)] # If subst is a superset
      symset[,let(loadts=Sys.time())]
    }
    else { # DOwnloadable, but one at a time
      symset <- form_symset(inticker,force=force)[,let(loadts=Sys.time())]
      tickertype <- symset[1,]$type
      if(tickertype=="user") {
        message_if(the_av$verbose,"av_one_px(",inticker,") is User data and must be updated outside of ShinyApp")
        return()
      }
      else {
        avfun <- epx_get_avfn(tickertype,live=FALSE)
        dta <- av_get_pf(inticker,avfun,outputsize=fifelse(nbdays<=20 & !force,"compact","full"),verbose=FALSE)
        if(nrow(dta)<=0) {
          tortn <-paste0("ERROR: ",inticker," returns no price data")
          message_if_red(TRUE,tortn)
          return(tortn)
        }
        dta <- dta |> save_av_data(avfun)
        dta <- epx_fmt_to_hist(dta,tickertype,live=FALSE)
        src <- "downloaded"
        if(addlive==TRUE) {
          avfun_live <- epx_get_avfn(tickertype,live=TRUE)
          livedta <- av_get_pf(inticker,avfun_live,outputsize="compact",verbose=FALSE)
          livedta <- epx_fmt_to_hist(livedta,tickertype,live=TRUE)
          message_if_green(the_av$verbose,"manage_epx: Adding ",nrow(livedta)," live prices (",livedta[,1]$close,
                           ") to ",inticker," at ",Sys.time())
          dta <- rbindlist(list(dta,livedta),use.names=TRUE,fill=TRUE)
          src <- "downloaded+live"
        }
      }
      tickers <- c(inticker)
    }
    dta <- dta[,let(ts=Sys.time())]
    the_av$pxd <- DTUpsert(the_av$pxd,dta,c("symbol","timestamp"),fill=TRUE)
    # Get asset type and update inventory
    thisinv <- the_av$pxd[symset[,.(symbol)],on=.(symbol)]
    thisinv <- thisinv[,.(beg_dt=min(timestamp),end_dt=max(timestamp),
                          medgap=median(diff(as.numeric(timestamp)))),by=.(symbol)]
    thisinv <- symset[thisinv,on=.(symbol)][,':='(age=Sys.Date()-end_dt)]
    setcolorder(thisinv,"loadts",after="end_dt")
    the_av$pxinv  <- DTUpsert(the_av$pxinv, thisinv, c("symbol"),fill=TRUE)
    dtrg <- lapply(range(dta$timestamp),\(x) format(x,"%Y-%m-%d"))
    message_if(the_av$verbose,"av_one_px(",tickers[1],fifelse(length(tickers)>1,"... ,",","),src,") ",nrow(dta)," rows with range ",dtrg[1],"::",dtrg[2],
            " filling gap of ",nbdays," days from ",dtstoget[1], " to ",dtstoget[2])
  }
  return("")
}

redownload_all <- function() {
  u1=lapply(the_av$pxinv$symbol,\(x) manage_epx(x,"-30y::",force=TRUE))
  save_avs_state("px")
  save_avs_state("asset")
}

# save_av_data:  Capture all outputs from alphavantage calls, possibly keyed appropriately
# capture_av_what
# cumulative: Add to data
# May need ot use fst if this gets too big

#   selectInput(inputId="capture_av_what",label="CaptureAVData",c("none","pricesonly","noprices","all"),multiple=FALSE),
#   selectInput(inputId="capture_av_update",label="Update or Cumulative",c("update","cum"),multiple=FALSE),
#   checkboxInput(inputId="cleanonstart","Clean Capture files on startup",value=the_av$cleanonstart)


save_av_data <- function(indta, in_av_fun) {
  av_download=skipreason=NULL
  avdatafn <- paste0(the_av$av_dump_dir,"/av_download.RD")
  dtakeys <- s(av_funcmap[av_fn==in_av_fun,.SD[1]]$savekey)
  # REDRUM capture files no matter what
  if(in_av_fun=="KILL") {
    if(file.exists(avdatafn)) {
      if(exists("av_download",envir=the_av)) { the_av$av_download<-list() }
      suppressWarnings(file.remove(avdatafn))
      message_if_red(TRUE,"save_av_data: Removing  capture file", avdatafn)
    }
    return()
  }
  # Do we need to do this?
  skipreason <- fcase(is.null(the_av$av_dump_dir) || the_av$av_dump_dir=="", "no Dump Directory",
                      the_av$capture_av_what=="none", "captured turned off",
                      nrow(indta)<=0, "no data to save",
                      length(dtakeys)<=0, "No save keys specified",
                      default=""
                      )
  if(nchar(skipreason)>0 & !(skipreason=="none")) {
    message_if(the_av$verbose,"save_av_data(",in_av_fun,") : Skipping save data (",skipreason,")")
    return(indta)
  }
  # Special events
  # Is Valid FUnciton
  if(!(in_av_fun %in% av_funcmap$av_fn || in_av_fun=="savenow")) {
    message_if_red(TRUE,"save_av_data: Invalid function name: ",in_av_fun, " must be valid AV call")
    return(indta)
  }
  is_price_data <-  grepl("TIME_SERIES|FX_DAILY|DIGITAL_CURRENCY",in_av_fun)
  # No technical analysis
  if(av_funcmap[av_fn==in_av_fun,.SD[1]]$category=="ta") {
    message_if_red(the_av$verbose,"save_av_data: Technical analysis data",in_av_fun, " not saved")
    return(indta)
  }
  cpy_indta <- copy(indta)[,let(load_ts=Sys.time())]  # Need to copy in case colnames are changed susequent to call
  # Determine if we're saving
  savingcode <-
    fcase(the_av$capture_av_what %chin% c("pricesonly") & is_price_data==TRUE, "timeseries",
          the_av$capture_av_what %chin% c("noprices") & is_price_data==FALSE, "other",
          the_av$capture_av_what %chin% c("all"), "all",
          default=""
    )

  if(nchar(savingcode)>0 & nrow(cpy_indta)>0) {
    if(!exists("av_download",envir=the_av) & file.exists(avdatafn)) {
      message_if_green(the_av$verbose,"Loading cumulative capture data from ",avdatafn)
      load(avdatafn,envir=the_av)
    }
    the_av$av_download[[in_av_fun]] <- the_av$av_download[[in_av_fun]] %||% data.table()
    if(the_av$capture_av_update=="cum") {
      the_av$av_download[[in_av_fun]] <- rbindlist(list(the_av$av_download[[in_av_fun]], cpy_indta),fill=TRUE)
      message_if_green(the_av$verbose,"ADD ",nrow(cpy_indta), " ", savingcode, " rows to ",avdatafn)
    }
    else {  # Update
      the_av$av_download[[in_av_fun]] <- DTUpsert(the_av$av_download[[in_av_fun]], cpy_indta, dtakeys)
      message_if_green(the_av$verbose,"UPSERT ",nrow(cpy_indta), " rows ", savingcode, " to ",avdatafn)
    }
  }

  if ("SaveEveryAVCall" %in% the_av$capture_av_save || "SaveNowOnOptUpdate" %in% the_av$capture_av_save) {
    save(av_download,file=avdatafn,envir=the_av)
    message_if_green(the_av$verbose,"Saving results of ",in_av_fun," call  to ",avdatafn, " now at ",
                     file.info(avdatafn)$size/1000, "kB")
    if("SaveNowOnOptUpdate" %in% the_av$capture_av_save) {
      the_av$capture_av_save <- setdiff(the_av$capture_av_save,"SaveNowOnOptUpdate")
    }
  }
  return(indta)
}

# Database helpers

check_min_colset <- function(indta,colsneeded) {
  if( length(intersect(colsneeded,names(indta))) <length(colsneeded) ) {
    stop(paste0("ERROR: Need at minimum columns ",paste0(colsneeded,collapse=" "), " to continue"))
  }
}

kill_symbol <- function(inticker) {
  the_av$pxd <- the_av$pxd[!(symbol==inticker),]
  the_av$pxinv <- the_av$pxinv[!(symbol==inticker),]
  message_if_red(TRUE,"Removed ",inticker," from price database")
  save_avs_state()
}


#' Extract internal state
#'
#' @name dump_the
#' @description Prints internal data state of [av_runShiny()]
#' `dump_the(typegrep="*")`
#' `dump_inv()`
#' `dump_assetgroups()`
#' `dump_captured()`
#' @param typegrep : Grep string for internal state parameters
#' @param returngt : Return GT table
#' @param todo : One of c("byfunction","pxhist",any av function name)
#' @returns data.table with desired data.
#' @seealso [av_runShiny()]
#' @examples
#' \dontrun{
#' `dump_the()`
#' `dump_inv()`
#' `dump_assetgroups(returngt=TRUE)`
#' `dump_captured(todo="byfunction")`
#' }
#'
#' @rdname dump_the
#' @export
dump_the <- function(typegrep="*") {
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

#' @rdname dump_the
#' @export
dump_inv <- function() {
  return(the_av$pxinv)
}

#' @rdname dump_the
#' @export
dump_assetgroups <- function(returngt=TRUE) {
  return(the_av$assetgroups[,.(tickers=paste0(.SD$ticker,collapse=" ")), by=.(listnm)])
}

#' @rdname dump_the
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

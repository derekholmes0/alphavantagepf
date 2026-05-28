# =======================================================================================================
#' App database functions
#'
#' @importFrom fst read_fst write_fst
#' @import data.table
#'
#' @name av_add_data
#' @description Add price data to [av_runShiny()] internal data
#'
#' @param indta A data.frame with the following minimal columns: `c(symbol,timestamp,close,adjusted_close)`
#' @returns Nothing
#'
#' @seealso [av_runShiny()]
#'
#' @details Entire set of columns from [av_get_pf()] can be added. First date column renamed to `timestamp`
#'
#' @examples
#' \dontrun{
#' av_add_data(av_get_pf("IBM","TIME_SERIES_DAILY_ADJUSTED"))
#' }
#'
#' @importFrom lubridate is.instant
#' @export
av_add_data <- function(indta) {
  firstdate <- find_col_bytype(indta,lubridate::is.instant)
  if (is.null(firstdate)) {
    stop("av_add_data: Need a timestamp column")
  }
  indta <- data.table(indta)
  setnames(indta,firstdate,"timestamp")
  colsneeded <- s("symbol;timestamp;close;adjusted_close")
  if( length(intersect(colsneeded,names(indta))) <length(colsneeded) ) {
    stop(paste0("av_add_data: Need at minimum columns ",paste0(colsneeded,collapse=" ")))
  }
  manage_epx(unique(indta$symbol),"-30y::",substitute_data=indta,force=TRUE)
  save_avs_state("all")
}

#' @noRd
restore_avs_state <- function(todo="all",skip=FALSE,msg="") {
  pxinv=NULL
  if(skip) { return() }
  if( !exists("inv_fn",envir=the) ) {   # FIll In defaults
    for(i in seq(1,nrow(avsd$defaults))) {
      ivartype <- avsd$defaults[i,]$vartype
      ivarnm <- avsd$defaults[i,]$var
      if( ivartype=="cache" ) { ivarval <- paste0(the$cachedir,"/", avsd$defaults[i,get("value_str")]) }
      else { ivarval <- avsd$defaults[i,get(paste0("value_",ivartype))] }
      assign(ivarnm, ivarval, envir=the)
    }
    message_if_red(TRUE,"Filling in app defaults")
  }
  if(grepl("all|constants",todo) & file.exists(the$constants_fn)) {
    load(the$constants_fn, envir=the)
  }
  if(grepl("all|inv",todo) & file.exists(the$inv_fn)) {
    load(the$inv_fn)
    lapply(names(pxinv),\(x) assign(x,pxinv[[x]],envir=the))
  }
  if(grepl("all|px",todo) & file.exists(the$pxd_fn)) {
      the$pxd <- fst::read_fst(the$pxd_fn, as.data.table=TRUE)
  }
  if(nchar(the$av_dump_dir)>0) {
    avdatafn <- paste0(the$av_dump_dir,"/av_download.RD")
    if(grepl("all|capture",todo) & file.exists(avdatafn)) {
      message_if_green(the$verbose,"Loading cumulative capture data from ",avdatafn)
      load(avdatafn,envir=the)
    }
  }
  message_if_green(TRUE,"Restored state (",todo,") from ",the$cachedir, " ",msg)
}

#' @importFrom stats setNames
save_avs_state <- function(todo="all") {
  classtype=NULL
  if(grepl("all|px",todo)) {
    nonpx_names <-  dump_the()[classtype=="data.table"& !(nm %in% c("pxd")),]$nm
    pxinv <- setNames(lapply(nonpx_names,\(x) get(x,envir=the)), nonpx_names)
    save(pxinv,file=the$inv_fn)
    # pxd to fst
    fst::write_fst(the$pxd,the$pxd_fn,compress=20)
    message_if_green(the$verbose,"Wrote inventories to ",the$inv_fn, ",price data to ",the$pxd_fn)
  }
  if(grepl("all|the",todo)) {
    unames=names(the)[sapply(the, class) %in% c("logical","character","numeric","difftime")]
    save(list=unames,envir=the,file= the$constants_fn)
    message_if_green(the$verbose,"Wrote constants state to ",the$constants_fn)
  }
}

# epx_get_avfn : Whichg function to call given type
# --------------------------------------------------
epx_get_avfn <- function(intype,live=FALSE) {
  av_live=av_hist=NULL
  return(data.table(type=s("Equity;ETF;Index;FX"),
                       av_hist=s("TIME_SERIES_DAILY_ADJUSTED;TIME_SERIES_DAILY_ADJUSTED;INDEX_DATA;FX_DAILY"),
                       av_live=s("GLOBAL_QUOTE;GLOBAL_QUOTE;NOTAVAIL;FX_INTRADAY"))[type==intype,.(avf=fifelse(live,av_live,av_hist))]$avf)

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
  else if(live==FALSE & intype=="FX") {
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
# form_symset(c("JBL","EEMA","NDX","USD/BRL"))
form_symset <- function(tickers, force=FALSE) {
  symbol=name=matchScore=NULL
  if(force==TRUE || nrow(the$pxinv)<=0) {
    newtickers <- s(tickers)
    symset <- data.table()
  }
  else {
    # Symbols we already  have
    symset<- data.table(symbol=s(tickers))[the$pxinv,on=.(symbol),nomatch=NULL][,.(symbol,type,currency,name,matchScore)]
    newtickers <- setdiff(s(tickers),symset$symbol)
  }
  # New equities/ETS
  symnew_eq <- rbindlist(lapply(newtickers, \(x)
    av_get_pf("","SYMBOL_SEARCH",keywords=x)[matchScore>=0.999,.(symbol=x,type,currency,name,matchScore)]
  ))
  # New Indices
  symnew_ix <- data.table(symbol=newtickers)
  symnew_ix <- the$indexlist[symnew_ix,on=.(symbol),nomatch=NULL]
  if(nrow(symnew_ix)>0) {
    symnew_ix <- symnew_ix[,.(symbol,type="Index",currency="USD",name,matchScore=1)]
  }
  # New currency Pairs
  symnew_fx <- data.table(symbol=grepv("([A-Z]{3})/([A-Z]{3})",newtickers,ignore.case=TRUE))[,
                                      .(symbol,type="FX",currency=substr(symbol,1,3),name=symbol,matchScore=1)]
  symset <- rbindlist(list(symset,symnew_eq,symnew_ix,symnew_fx),fill=TRUE,use.names=TRUE)
  return(symset[])
}

# manage_epx only accepts more than one ticker if called with substitute_data
# mange_eps will download repeatedly before market opens, no real way to avoid it without time of day logic


manage_epx <- function(inticker, dtstr, substitute_data=NULL, addlive=FALSE, force=FALSE) {
  symbol=beg_dt=NULL
  dtstoget <- gendtstr(dtstr,rtn="list") # Dates to get
  if(nrow(the$pxinv)>0) {
    edates <- the$pxinv[data.table(symbol=s(inticker)),on=.(symbol),nomatch=NULL]
    if(nrow(edates)>0) {
      earlystarts <- edates[beg_dt>dtstoget[1],]
      if(nrow(earlystarts)>0) {
        earlystarts <- paste0(earlystarts$symbol,collapse=" ")
        message_if(the$verbose,"av_one_px(",earlystarts,"): Start Date requested earlier than series start ")
      }
      dtstoget[1] <- min(edates$end_dt)
    }
  }
  # If exists, then check if data is up to date
  #   if it doesn't exist or is too old, use full download
  # Note that downloads will occur anyway if narket has not opened yet
  nbdays = nrow(dtmap[between(DT_ENTRY,dtstoget[1],dtstoget[2])])
  if(nbdays<=1 & !force) {
    message_if(the$verbose,"av_one_px(",inticker,"): No need to download, last date in DB: ",format(dtstoget[2]))
  }
  else {
    if(is.data.table(substitute_data)) {
      dta <- data.table::copy(substitute_data)
      src <- "Added"
      tickers <- unique(substitute_data$symbol)
      symset <- form_symset(tickers,force=force)[,let(loadts=Sys.time())]
    }
    else {
      datasize <- fifelse(nbdays<=20 & !force,"compact","full")
      symset <- form_symset(inticker,force=force)[,let(loadts=Sys.time())]
      tickertype <- symset[1,]$type
      avfun <- epx_get_avfn(tickertype,live=FALSE)
      dta <- av_get_pf(inticker,avfun,outputsize=datasize,verbose=FALSE)
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
        message_if_green(the$verbose,"manage_epx: Adding Live price to ",inticker," at ",Sys.time())
        dta <- rbindlist(list(dta,livedta),use.names=TRUE,fill=TRUE)
        src <- "downloaded+live"
      }
      tickers <- c(inticker)
    }
    dta <- dta[,let(ts=Sys.time())]
    the$pxd <- DTUpsert(the$pxd,dta,c("symbol","timestamp"),fill=TRUE)
    # Get asset type and update inventory
    thisinv <- the$pxd[symset[,.(symbol)],on=.(symbol)]
    thisinv <- thisinv[,.(beg_dt=min(timestamp),end_dt=max(timestamp)),by=.(symbol)]
    thisinv <- symset[thisinv,on=.(symbol)][,':='(age=Sys.Date()-end_dt)]
    setcolorder(thisinv,"loadts",after="end_dt")
    the$pxinv  <- DTUpsert(the$pxinv, thisinv, c("symbol"),fill=TRUE)
    dtrg <- lapply(range(dta$timestamp),\(x) format(x,"%Y-%m-%d"))
    message_if_green(the$verbose,"av_one_px(",tickers[1],"): ",src," ",nrow(dta)," rows with range ",dtrg[1],"::",dtrg[2],
            " filling gap of ",nbdays," days from ",dtstoget[1], " to ",dtstoget[2])
  }
  return("")
}

redownload_all <- function() {
  u1=lapply(the$pxinv$symbol,\(x) manage_epx(x,"-30y::",force=TRUE))
  save_avs_state("px")
  save_avs_state("asset")
}

# save_av_data:  Capture all outputs from alphavantage calls, possibly keyed appropriately
# capture_av_what
# cumulative: Add to data
# May need ot use fst if this gets too big

#   selectInput(inputId="capture_av_what",label="CaptureAVData",c("none","pricesonly","noprices","all"),multiple=FALSE),
#   selectInput(inputId="capture_av_update",label="Update or Cumulative",c("update","cum"),multiple=FALSE),
#   checkboxInput(inputId="cleanonstart","Clean Capture files on startup",value=the$cleanonstart)


save_av_data <- function(indta, in_av_fun) {
  av_download=skipreason=NULL
  avdatafn <- paste0(the$av_dump_dir,"/av_download.RD")
  dtakeys <- s(av_funcmap[av_fn==in_av_fun,.SD[1]]$savekey)
  # REDRUM capture files no matter what
  if(in_av_fun=="KILL") {
    if(file.exists(avdatafn)) {
      if(exists("av_download",envir=the)) { the$av_download<-list() }
      suppressWarnings(file.remove(avdatafn))
      message_if_red(TRUE,"save_av_data: Removing  capture file", avdatafn)
    }
    return()
  }
  # Do we need to do this?
  skipreason <- fcase(is.null(the$av_dump_dir) || the$av_dump_dir=="", "no Dump Directory",
                      the$capture_av_what=="none", "captured turned off",
                      nrow(indta)<=0, "no data to save",
                      length(dtakeys)<=0, "No save keys specified",
                      default=""
                      )
  if(nchar(skipreason)>0 & !(skipreason=="none")) {
    message_if(the$verbose,"save_av_data(",in_av_fun,") : Skipping save data (",skipreason,")")
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
    message_if_red(the$verbose,"save_av_data: Technical analysis data",in_av_fun, " not saved")
    return(indta)
  }
  cpy_indta <- copy(indta)[,let(load_ts=Sys.time())]  # Need to copy in case colnames are changed susequent to call
  # Determine if we're saving
  savingcode <-
    fcase(the$capture_av_what %chin% c("pricesonly") & is_price_data==TRUE, "timeseries",
          the$capture_av_what %chin% c("noprices") & is_price_data==FALSE, "other",
          the$capture_av_what %chin% c("all"), "all",
          default=""
    )

  if(nchar(savingcode)>0 & nrow(cpy_indta)>0) {
    if(!exists("av_download",envir=the) & file.exists(avdatafn)) {
      message_if_green(the$verbose,"Loading cumulative capture data from ",avdatafn)
      load(avdatafn,envir=the)
    }
    the$av_download[[in_av_fun]] <- the$av_download[[in_av_fun]] %||% data.table()
    if(the$capture_av_update=="cum") {
      the$av_download[[in_av_fun]] <- rbindlist(list(the$av_download[[in_av_fun]], cpy_indta),fill=TRUE)
      message_if_green(the$verbose,"ADD ",nrow(cpy_indta), " ", savingcode, " rows to ",avdatafn)
    }
    else {  # Update
      the$av_download[[in_av_fun]] <- DTUpsert(the$av_download[[in_av_fun]], cpy_indta, dtakeys)
      message_if_green(the$verbose,"UPSERT ",nrow(cpy_indta), " rows ", savingcode, " to ",avdatafn)
    }
  }

  if ("SaveEveryAVCall" %in% the$capture_av_save || "SaveNowOnOptUpdate" %in% the$capture_av_save) {
    save(av_download,file=avdatafn,envir=the)
    message_if_green(the$verbose,"Saving results of ",in_av_fun," call  to ",avdatafn, " now at ",
                     file.info(avdatafn)$size/1000, "kB")
    if("SaveNowOnOptUpdate" %in% the$capture_av_save) {
      the$capture_av_save <- setdiff(the$capture_av_save,"SaveNowOnOptUpdate")
    }
  }
  return(indta)
}

# Database helpers

kill_symbol <- function(inticker) {
  the$pxd <- the$pxd[!(symbol==inticker),]
  the$pxinv <- the$pxinv[!(symbol==inticker),]
  message_if_red(TRUE,"Removed ",inticker," from price database")
  save_avs_state()
}


# Checks on internal data structures
# dump_the : Returns internal state
# dump_inv : REturns prie inventory
# dump_assetlist : Returns current set of assets
# dump_captured : Returns summary of captured data

dump_the <- function(typegrep="*") {
  classtype=nm=NULL
  outdump<-data.table()
  for (x in ls(envir=the)) {
    toget <- get(x,envir=the)
    type <- class(toget)
    if(any(grepl(typegrep,type))) {
      if("data.frame" %in% type) {
        toget<-paste0("<<data.frame>> with ",nrow(toget), " rows")
      }
      if("list" %in% type) {
        toget<-paste0("<<list>> with ",length(toget), " items")
      }
      outdump<-rbindlist(list(outdump,data.table(nm=x,classtype=type[1], toget=toget)),ignore.attr=TRUE)
    }
  }
  return(outdump[order(classtype,nm)])
}

dump_inv <- function() {
  return(the$pxinv)
}
dump_assetlist <- function(returngt=TRUE) {
  return(the$assetlist[,.(tickers=paste0(.SD$ticker,collapse=" ")), by=.(listnm)])
}
dump_captured <- function(todo="byfunction") {
  nr=fn=load_ts=NULL
  if(is.null(the$av_download)) { return("No Data downloaded")}
  if(todo=="byfunction") {
    rtn <- data.table(fn=names(the$av_download))[,nr:=nrow(the$av_download[[fn]]), by=.I][]
  }
  if(todo=="pxhist" & "TIME_SERIES_DAILY_ADJUSTED" %in% names(the$av_download)) {
    rtn <- the$av_download[["TIME_SERIES_DAILY_ADJUSTED"]][,
              .(lastpx=last(close), lastts=max(load_ts), mindate=min(timestamp), maxdate=max(timestamp)), by=.(symbol)]
  }
  if(todo %in% names(the$av_download)) {
    tkeys <- setdiff(key(the$av_download[[todo]]),s("contractID;timestamp;timestamp"))
    rtn <- the$av_download[[todo]][,.(n=.N,lastts=max(load_ts)),by=tkeys]
  }
  return( rtn )
}

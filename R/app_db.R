# =======================================================================================================
#' App database functions
#'
#' @importFrom fst read_fst write_fst
#' @import data.table
#'
#' @name av_add_data
#' @description Add price data to [av_runShiny()] internal data
#'
#' @param indta A data.frame with the following minimal columns: `c(symbol,date,close,adjusted_close)`
#' @returns Nothing
#'
#' @seealso [av_runShiny()]
#'
#' @details Entire set of columns from [av_get_pf()] can be added. First date column renamed to `date`
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
    stop("av_add_data: Need a date column")
  }
  indta <- data.table(indta)
  setnames(indta,firstdate,"date")
  colsneeded <- s("symbol;date;close;adjusted_close")
  if( length(intersect(colsneeded,names(indta))) <length(colsneeded) ) {
    stop(paste0("av_add_data: Need at minimum columns ",paste0(colsneeded,collapse=" ")))
  }
  manage_epx(unique(indta$symbol),"-30y::",substitute_data=indta,force=TRUE)
  save_avs_state("all")
}

#' @noRd
restore_avs_state <- function(todo="all",skip=FALSE) {
  assetlist=pxinv=NULL
  if(skip) { return() }
  if(grepl("all|constants",todo) & file.exists(the$constants_fn)) {
    load(the$constants_fn, envir=the)
  }
  if(grepl("all|asset",todo) & file.exists(the$assetlist_fn)) {
    load(the$assetlist_fn)
    the$assetlist <- assetlist
  }
  if(grepl("all|px",todo) & file.exists(the$pxd_fn)) {
    the$pxd <- fst::read_fst(the$pxd_fn, as.data.table=TRUE)
  }
  if(grepl("all|px",todo) & file.exists(the$inv_fn)) {
    load(the$inv_fn)
    the$pxinv <- pxinv
  }
  message_if_green(the$verbose,"Restored state (",todo,") from ",the$cachedir)
}

save_avs_state <- function(todo="all") {
  if(grepl("all|asset",todo)) {
    assetlist <- the$assetlist
    save(assetlist,file=the$assetlist_fn)
    message_if_green(the$verbose,"Wrote asset lists to ",the$cachedir)
  }
  if(grepl("all|px",todo)) {
    pxinv <- the$pxinv
    save(pxinv,file=the$inv_fn)
    message_if_green(the$verbose,"Wrote inventory to ",the$inv_fn)
    fst::write_fst(the$pxd,the$pxd_fn,compress=20)
    message_if_green(the$verbose,"Wrote price data to ",the$pxd_fn)
  }
  if(grepl("all|the",todo)) {
    unames=setdiff(ls(envir=the),s("pxd;pxinv;assetlist"))
    save(list=unames,envir=the,file= the$constants_fn)
    message_if_green(the$verbose,"Wrote constants state to ",the$constants_fn)
  }
}


form_symset <- function(tickers, force=FALSE) {
  symbol=name=matchScore=NULL
  if(force==TRUE || nrow(the$pxinv)<=0) {
    newtickers <- s(tickers)
    symset <- data.table()
  }
  else {
    symset<- data.table(symbol=s(tickers))[the$pxinv,on=.(symbol),nomatch=NULL][,.(symbol,type,currency,name,matchScore)]
    newtickers <- setdiff(s(tickers),symset$symbol)
  }
  symnew <- rbindlist(lapply(newtickers, \(x)
    av_get_pf("","SYMBOL_SEARCH",keywords=x)[matchScore>=0.999,][1,.(symbol=x,type,currency,name,matchScore)]
  ))
  symset = rbindlist(list(symset,symnew),fill=TRUE,use.names=TRUE)
  return(symset[])
}

# manage_epx only accepts more than one ticker if called with substitute_data

manage_epx <- function(inticker, dtstr, substitute_data=NULL, force=FALSE) {
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
    }
    else {
      datasize <- fifelse(nbdays<=20 & !force,"compact","full")
      avfun <- fifelse( stringr::str_detect(inticker,"[A-Z]/[A-Z]"),"FX_DAILY","TIME_SERIES_DAILY_ADJUSTED")
      dta <- av_get_pf(inticker,avfun,outputsize=datasize,verbose=FALSE)
      if(nrow(dta)<=0) {
        tortn <-paste0("ERROR: ",inticker," returns no price data")
        message_if_red(TRUE,tortn)
        return(tortn)
      }
      setnames(dta,"timestamp","date")
      src <- "downloaded"
      tickers <- c(inticker)
    }
    setkeyv(dta,c("symbol","date"))
    if(the$save_data & the$save_prices & is.null(substitute_data)) {
      dta <- dta |> save_av_data(avfun)
    }
    the$pxd <- DTUpsert(the$pxd,dta,c("symbol","date"),fill=TRUE,verbose=TRUE)
    # Get asset type and update inventory
    symset <- form_symset(tickers,force=force)[,let(loadts=Sys.time())]
    thisinv <- the$pxd[symset[,.(symbol)],on=.(symbol)]
    thisinv <- thisinv[,.(beg_dt=min(date),end_dt=max(date)),by=.(symbol)]
    thisinv <- symset[thisinv,on=.(symbol)]
    setcolorder(thisinv,"loadts",after="end_dt")
    the$pxinv  <- DTUpsert(the$pxinv, thisinv, c("symbol"),fill=TRUE)
    dtrg <- lapply(range(dta$date),\(x) format(x,"%Y-%m-%d"))
    message_if_green(the$verbose,"av_one_px(",tickers[1],"): ",src," ",nrow(dta)," rows with range ",dtrg,
            " filling gap of ",nbdays," days from ",dtstoget[1], " to ",dtstoget[2])
  }
  return("")
}

redownload_all <- function() {
  u1=lapply(the$pxinv$symbol,\(x) manage_epx(x,"-30y::",force=TRUE))
  save_avs_state("px")
  save_avs_state("asset")
}

# Just pass data if filenm is null
# Additional option to save price data as well
# cumulative: Add to data
# May need ot use fst if this gets too big
save_av_data <- function(indta, intype) {
  av_download=NULL
  if(is.null(the$save_dir) || the$save_dir=="") {
    message_if_green(the$verbose,"Skipping save data")
    return(indta)
  }
  avdatafn <- paste0(the$save_dir,"/av_download.RD")
  if(intype=="KILL") {
    if(file.exists(avdatafn)) {
      if(exists("av_download",envir=the)) { the$av_download<-list() }
      suppressWarnings(file.remove(avdatafn))
      message_if_red(TRUE,"save_av_data: Removing  capture file", avdatafn)
    }
    return()
  }
  # Is Valid FUnciton
  if(!intype %in% av_funcmap$av_fn) {
    message_if_red(TRUE,"save_av_data: Invalid function name: ",intype, " must be valid AV call")
    return(indta)
  }
  # No technical analysis
  if(av_funcmap[av_fn==intype,.SD[1]]$category=="ta") {
    message_if_red(TRUE,"save_av_data: Technical analysis data",intype, " not saved")
    return(indta)
  }
  # Skip if price
  if(the$save_prices==FALSE & grepl("TIME_SERIES|FX_DAILY",intype)) {
    message_if_red(TRUE,"save_av_data: Not saving price series by choice")
    return(indta)
  }
  if(the$save_cum & !exists("av_download",envir=the) & file.exists(avdatafn)) {
    message_if_green(TRUE,"Loading cumulative data from ",avdatafn)
    load(avdatafn,envir=the)
  }
  if(the$save_ts) {
    indta <- indta[,let(ts=Sys.time())]
  }
  if(the$save_cum==TRUE & exists("av_download",envir=the)) {
    the$av_download[[intype]] <- rbindlist(list(the$av_download[[intype]], indta))
  }
  else {
    the$av_download[[intype]] <- indta
  }
  save(av_download,file=avdatafn,envir=the)
  message_if_green(the$verbose,"Saving results of ",intype," call  to ",avdatafn, "now at ",
                   file.info(avdatafn)$size/1000, "kB")
  return(indta)
}

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
      outdump<-rbindlist(list(outdump,data.table(nm=x,classtype=type[1], toget=toget)))
    }
  }
  return(outdump[order(classtype,nm)])
}

dump_inv <- function() {
  return(the$pxinv)
}
dump_assetlist <- function(returngt=TRUE) {
  outdump <- the$assetlist[,.(tickers=paste0(.SD$ticker,collapse=" ")), by=.(listnm)]
  if(returngt==TRUE) {
    outdump <- outdump |>  gt.avtheme(themeset="assetlist")
  }
  return(outdump)
}


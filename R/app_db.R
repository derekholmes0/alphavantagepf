# =======================================================================================================
#' App database functions
#' Update any lists from Alphavnatage.
#' @noRd
update_tickerlists <- function(reallydoingthis=TRUE,reset=FALSE) {
  from_currency=to_currency=list_ts=NULL
  if(reallydoingthis==FALSE) { return() }
  if(reset==TRUE) {
    the_av$tickerlist <- data.table()
    message_if_red(the_av$verbose,"Resetting ticker lists  at ",Sys.time())
  }
  # Tickers
  indexlist <- av_get_pf("","INDEX_CATALOG",delay=1)[,type:="Index"][]
  cryptolist <- avsd$crypto_list[,.(symbol=paste0(from_currency,"/",to_currency),type="Crypto")][,name:=symbol]
  indexlist <- rbindlist(list(indexlist,cryptolist),use.names=TRUE,fill=TRUE)[,list_ts:=Sys.Date()][]
  the_av$tickerlist <- DTUpsert(the_av$tickerlist,indexlist,c("symbol"))
  # Names
  listings <- av_get_pf("","LISTING_STATUS")[,list_ts:=Sys.Date()]
  setkeyv(listings,c("symbol"))
  the_av$listings <- listings
  message_if_red(the_av$verbose,"Reconstructed index (",nrow(indexlist),"), crypto (",nrow(cryptolist),
            "), and listing status (",nrow(the_av$listings),") lists at ",Sys.time())
  save_avs_state("all",msg="updatetickers") # must use all with any inventory data
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
    tortn <- inquote[,.(symbol,timestamp=latestDay,open,high,low,close=price,adjusted_close=price,volume,dividend_amount=0,split_coefficient=1)]
  }
  else if(live==TRUE & (intype=="Index" | intype=="user")) {
    tortn <- data.table()
  }
  else if(live==TRUE & (intype=="FX" | intype=="Crypto")) {
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
  symbol=name=matchScore=list_ts=assetType=NULL
  alltickers=s(toupper(tickers))
  if(force==TRUE || nrow(the_av$pxinv)<=0) {
    newtickers <- alltickers
    symset <- data.table()
  }
  else {
    # Symbols we already  have
    symset<- data.table(symbol=alltickers)[the_av$pxinv,on=.(symbol),nomatch=NULL]
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
  symnew_eq_inlistings <- the_av$listings[data.table(symbol=sort(newtickers)),on=.(symbol)][,
                                  .(symbol,type=fifelse(assetType=="Stock","Equity",assetType),name,currency="USD",matchScore=1,list_ts)]
  newtickers <-setdiff(newtickers,symnew_eq_inlistings$symbol)
  symnew_eq <- rbindlist(lapply(newtickers, \(x) {
    message("NOTE: Trying symbolsearch")
    z1 <- av_get_pf("","SYMBOL_SEARCH",keywords=x,delay=delay)
    if(nrow(z1)<=0) {
      #message_if_red(TRUE,"Alphavantage cannot find  ",x,": May be a user data series")
      return(data.table(symbol=x,matchScore=0))
    }
    else {
      return(z1[matchScore>=0.99,.(symbol=x,type,currency,name,matchScore,list_ts=Sys.Date())])
    }
  }))
  if(nrow(symnew_eq)>0)  {
      newtickers <- setdiff(newtickers,symnew_eq[matchScore>=0.5,]$symbol)
      symnew_eq <- symnew_eq[matchScore>=0.5,]
  }
  # -----------------------------------------Un Downloadable assets/ User Data
  symnew_user <- data.table()
  if( length(newtickers)>0 ) {
    symnew_user <- data.table(symbol=newtickers,type="user",currency="USD",matchScore=1,list_ts=Sys.Date())
    the_av$tickerlist <- DTUpsert(the_av$tickerlist,symnew_user[,.(symbol,name=symbol,type,list_ts)],c("symbol"))
    #todo: Figure out how to get a proper name in user data.
  }
  # Collect all together
  symset <- rbindlist(list(symset,symnew_inferable,symnew_eq,symnew_eq_inlistings,symnew_user),fill=TRUE,use.names=TRUE)
  return(symset[])
}

# manage_epx only accepts more than one ticker if called with substitute_data
# mange_eps will download repeatedly before market opens, no real way to avoid it without time of day logic
# 260703:  Splitting price and earnings, and keeping manage_epx as wrapper
# If sub data exists, but sub_earn is null, this will download it automatically
manage_epx <- function(inticker, dtstr,
                       substitute_data=NULL, substitute_symset=NULL, substitute_earn=NULL, substitute_earnest=NULL,
                       addlive=FALSE, force=FALSE, delay=0.1) {
  if(nrow(the_av$tickerlist)<=0) { update_tickerlists() }
  # Get data
  # rtnpx returns list (messge,dta_downloaded)
  # rtnpx <- manage_px(inticker,dtstr); rtn_earn<-manage_earn(rtnpx)
  rtnpx   <- manage_px(inticker,dtstr,substitute_data=substitute_data,substitute_symset=substitute_symset,addlive=addlive,force=force,delay=delay)
  if(nrow(rtnpx)<=0) {
    message_if_red(the_av$verbose,"Error: ",inticker, " is invalid ticker")
    return()
  }
  rtnearn <- manage_earn(rtnpx,substitute_earn=substitute_earn,substitute_earnest=substitute_earnest,delay=delay)
  thisinv <- get_inv(inticker)
  the_av$pxinv <- DTUpsert(the_av$pxinv, thisinv, c("symbol"),fill=TRUE)
  save_avs_state("px")
  message_if_green(the_av$verbose,"mange_epx(",inticker,"): px:",rtnpx," earn:",rtnearn)
}

# ------------------------------------------------------------------ INventories
# symset must be given to update names etc of user data in pxinv
get_inv <- function(tickerlist=NULL,override_symset=NULL) {
  matchScore=list_ts=dividend_amount=reportedEPS=horizon=ts=eps_estimate_average=NULL
  if(nrow(the_av$pxinv)<=0) {
    tickerlist <-unique(the_av$pxd$symbol)
  }
  if(is.null(tickerlist)) {
    rtnpx = the_av$pxinv[data.table(type=s("Equity;ETF")),on=.(type),nomatch=NULL][,.(symbol,type,currency,name,matchScore,list_ts)]
  }
  else if (is.data.frame(override_symset)) {
    rtnpx = coalesce_DT(override_symset,data.table(type="user",currency="USD",matchScore=1,list_ts=Sys.Date()))
  }
  else {
    rtnpx = form_symset(s(tickerlist),force=TRUE)[,.(symbol,type,currency,name,matchScore,list_ts)]
  }
  thisinv_dta <- the_av$pxd[rtnpx,on=.(symbol)]
  thisinv_div<- thisinv_dta[abs(dividend_amount)>0,.SD[which.max(timestamp),.(div_lastdt=timestamp,div_lastval=dividend_amount)],by=.(symbol)]
  thisinv_px <- thisinv_dta[,.(beg_dt=min(timestamp),end_dt=max(timestamp),age=Sys.Date()-max(timestamp),lastpx=last(adjusted_close),
                           medgap=median(diff(as.numeric(timestamp)))),by=.(symbol,type)]

  earn_past <- data.table(symbol=tickerlist)[,let(lastearn_dt=Sys.Date(), lastearn_eps=NA_real_)]
  if(nrow(the_av$earn)>0) {
    earn_past <- the_av$earn[rtnpx,on=.(symbol)][,.SD[which.max(reportedDate)],by=.(symbol)][,.(symbol,lastearn_dt=reportedDate,lastearn_eps=reportedEPS)]
  }
  earn_fwd <- data.table(symbol=tickerlist)[,let(earnf_ts=Sys.Date(),earnf_nextdt=Sys.Date(),earnf_next=NA_real_)]
  if(nrow(the_av$earnest)>0) {
    earn_fwd <- the_av$earnest[rtnpx,on=.(symbol)][horizon=="fiscal quarter",.SD[which.max(date)],by=.(symbol)][,                                                                .(symbol,earnf_ts=ts,earnf_nextdt=date,earnf_next=eps_estimate_average)]
  }
  thisinv_id <- rtnpx[,.(symbol,currency,name,matchScore,list_ts)] # Tricky
  thisinv <- Reduce(function(x,y) merge(x,y,by="symbol",all=TRUE),list(thisinv_div,thisinv_px,earn_past,earn_fwd,thisinv_id))
  setcolorder(thisinv,s("symbol;end_dt;lastearn_dt;earnf_nextdt;earnf_ts;div_lastdt;lastpx;earnf_next;div_lastval"))
  return(thisinv)
}

# ------------------------------------------------------------------ PRICES
#' @importFrom stats median
manage_px <- function(inticker, dtstr, substitute_data=NULL, substitute_symset=NULL, addlive=FALSE, force=FALSE, delay=0.1) {
  symbol=beg_dt=medgap=i.enddt=NULL
  # Determine dates needed
  dtstoget <- gendtstr(dtstr,rtn="list") # Dates to get
  if(nrow(the_av$pxinv)>0 & is.null(substitute_data) & is.null(substitute_symset)) {
    #TO do: IUmplement max age and integrate market hours
    edates <- the_av$pxinv[data.table(symbol=s(inticker)),on=.(symbol),nomatch=NULL]
    if(nrow(edates)>0) {
      earlystarts <- edates[beg_dt>dtstoget[1],]
      if(nrow(earlystarts)>0) {
        force <- TRUE
        message_if(the_av$verbose,"av_one_px(",paste0(earlystarts$symbol,collapse=" "),"): Start Date requested earlier than series start, redownloading ")
      }
      dtstoget[1] <- min(edates$end_dt)
    }
  }
  # If exists, then check if data is up to date
  #   if it doesn't exist or is too old, use full download
  # Note that downloads will occur anyway if narket has not opened yet
  symset <- form_symset(inticker,force=force)[,let(loadts=Sys.time())][!is.na(type),]
  if( nrow(symset)<=0 ) { return(data.table())}
  tortn <- symset[,.(symbol,type)]
  nbdays = nrow(dtmap[between(DT_ENTRY,dtstoget[1],dtstoget[2])])
  if(nbdays<=(the_av$maxage_px_days+1) & !force) { # Always have today (last date) in set
    src <- "Cached"
    tortn <- the_av$pxinv[,.(symbol,minaddt=end_dt,maxadddt=end_dt)][tortn,on=.(symbol)]
    dta <- data.table()
    outmsg <- paste0("Using cached data for: ",dtstoget[1], "::",dtstoget[2])
  }
  else {  # Time Series Externally given or downloaded
    if(is.data.table(substitute_data)) {
      dta <- data.table::copy(substitute_data)
      if("low" %notin% colnames(dta)) {   dta <- dta[,let(open=close,high=close,low=close)]  }
      src <- "userdata"
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
      if(nrow(symset)<=0) {
          message_if_red(the_av$verbose,"av_one_px(",inticker,") Not Found Anywhere")
          return("ERROR: cannot find ticker")
      }
      tickertype <- symset[1,]$type
      if(tickertype=="user") {
        message_if(the_av$verbose,"avs_update(",inticker,") is User data w/ last day ",the_av$pxinv[symbol==inticker,]$end_dt,
                        "and must be updated outside of ShinyApp")
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
      }
      tickers <- c(inticker)
      dta <- dta[,let(ts=Sys.time())]
      dtrg <- lapply(range(dta$timestamp),\(x) format(x,"%Y-%m-%d"))
      tortn = tortn[dta[,.(minadddt=min(timestamp),maxadddt=max(timestamp)),by=.(symbol)],on=.(symbol)]
      outmsg <- paste0(nrow(dta)," rows w/ range ",dtrg[1],"::",dtrg[2]," filling gap of ",nbdays," days (",dtstoget[1], "::",dtstoget[2],")")

    } # Downloaded
  } # DOwnloaded or external
  # Add Live if requested
  if(addlive==TRUE) {
    for(ttype in unique(tortn$type)) {
      if(!(avfun_live <- epx_get_avfn(ttype,live=TRUE)) =="NOTAVAIL") {
        intickers <- tortn[type==ttype,]
        livedta <- lapply(intickers$symbol, \(x) {
            av_get_pf(x,avfun_live,outputsize="compact",verbose=FALSE) |> epx_fmt_to_hist(ttype,live=TRUE) })
        livedta <- rbindlist(livedta,fill=TRUE)[,let(ts=Sys.time())]
        src <- paste0(src,"+live")
        outmsg <- paste0(outmsg, " w/ Live px @ ",Sys.time())
        dta <- DTUpsert(dta,livedta,c("symbol","timestamp"),fill=TRUE)
      }
    }
  }
  if(nrow(dta)>0) {
    the_av$pxd <- DTUpsert(the_av$pxd,dta,c("symbol","timestamp"),fill=TRUE)
    the_av$pxinv[dta[,.(symbol,enddt=max(timestamp)),by=.(symbol)],end_dt:=i.enddt,on=.(symbol)]
  }
  message_if(the_av$verbose,"av_one_px(",paste_trunc(tortn$symbol)," @ ",src,") ", outmsg)
  return(tortn)
}

# ------------------------------------------------------------------ EARNINGS
# Assumes price data already downloaded and ticker is in pxinv
#   -- ao call this second!
# todo:  only download earnings when you think you might need to
#' @importFrom purrr map
manage_earn <- function(tickerdt, substitute_earn=NULL, substitute_earnest=NULL, delay=0.05) {
  ts=horizon=eps_estimate_average=assetType=NULL
  src<-""; rtniv<-data.table()
  earntickers <- the_av$listings[tickerdt,on=.(symbol),nomatch=NULL][assetType=="Stock",]
  if(nrow(the_av$earn)>0) {
    age <- as.numeric(Sys.Date()-max(the_av$earn$ts))
    if(age<=the_av$maxage_earn_days) {
      message_if_red(TRUE,paste0("Earnings data age of ",age," less than ",the_av$maxage_earn_days," maxage, skipping"))
      return()
    }
  }
  if( length( badtickers <- setdiff(tickerdt$symbol,earntickers$symbol))>0) {
    message_if_red(TRUE,"Earnings skipping invalid tickers: ",paste_trunc(badtickers))
  }
  if( nrow(earntickers)>0) {
    if(is.data.table(substitute_earn)) {
      src<-"subs earnings"
      earn_past <- copy(substitute_earn)
    }
    if(is.data.table(substitute_earnest)) {
      src<-paste(src, "subs estimates")
      earn_fwd <- copy(substitute_earnest)
    }
    if(src=="") {
      src <- "Downloaded"
      earn_past <- purrr::map(earntickers$symbol, \(x) av_get_pf(x,"EARNINGS",delay=delay) |> av_extract_df("quarterlyEarnings"),.progress="Previous Earnings")
      earn_past <- rbindlist(earn_past,fill=TRUE)
      earn_fwd <- purrr::map(earntickers$symbol, \(x) av_get_pf(x,"EARNINGS_ESTIMATES",delay=delay) |> av_extract_df("estimates"), .progress="Forecast Earnings")
      earn_fwd <- rbindlist(earn_fwd,fill=TRUE)
    }
    if(nrow(earn_past)>0) {
      setkeyv(earn_past,s("symbol;reportedDate;fiscalDateEnding"))
      earn_past <- earn_past[,ts:=Sys.Date()]
      rtninv_past = earn_past[,.(lastearndt=max(reportedDate)),by=.(symbol)]
      the_av$earn <- DTUpsert(the_av$earn,earn_past,key(earn_past))
    }
    if(nrow(earn_fwd)>0) {
      earn_fwd <- earn_fwd[,ts:=Sys.Date()]
      setkeyv(earn_fwd,s("symbol;date;horizon;ts"))  # Possibly want evolution.
      the_av$earnest <- DTUpsert(the_av$earnest,earn_fwd,key(earn_fwd))
      rtninv_fwd <- earn_fwd[horizon=="fiscal quarter",.SD[which.max(date)],by=.(symbol)][,
                            .(symbol,earnf_ts=ts,earnf_nextdt=date,earnf_next=eps_estimate_average)]
      rtniv =  rtninv_fwd[rtninv_past,on=.(symbol)]
    }
    message_if_green(the_av$verbose,"earnings(",paste_trunc(earntickers$symbol),") from ",src," adds ",nrow(earn_past), " past and ",nrow(earn_fwd), " fwd earnings")
    message_if_red(src=="","manage_earn: No tickers to update.  Have they been priced?")
  }
  return(rtniv)
}

redownload_all <- function() {
  u1=lapply(the_av$pxinv$symbol,\(x) manage_epx(x,"-30y::",force=TRUE))
  save_avs_state("px",msg="redownload_px")
  save_avs_state("asset",msg="redownload_asset")
}


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
  if(grepl("all|px",todo)) {
    px_names <- s("pxd;earn;earnest")
    rtn <- lapply(px_names, \(x) {
      thisfn = get(paste0(x,"_fn"),envir=the_av)
      assign(x, fst::read_fst(thisfn, as.data.table=TRUE), envir=the_av) # pxd, earn to fst
    })
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

# =========================================================
# save_avs_state:
# in separate fst: price, eanings, earnings fcst
# in inv.Rd : All other data.tables.  Some along for the ride
# in constants: All non-data.tables in the_av
# =========================================================

#' @importFrom stats setNames
save_avs_state <- function(todo="all",msg="") {
  classtype=rtn=NULL
  shortmsg <- ""
  # Price and earnings in one fst file, everythign else in inventory file
  px_names <- s("pxd;earn;earnest")
  nonpx_names <-  dump_state()[classtype=="data.table" & !(nm %in% px_names),]$nm
  if(grepl("all|px",todo)) {
    pxinv <- setNames(lapply(nonpx_names,\(x) get(x,envir=the_av)), nonpx_names) # So we save a few extra things
    save(pxinv,file=the_av$inv_fn)
    rtn <- lapply(px_names, \(x) {
      thisfn = get(paste0(x,"_fn"),envir=the_av)
      fst::write_fst(get(x,envir=the_av),thisfn,compress=20) # pxd, earn to fst
    })
    shortmsg <- paste(shortmsg,"data.tables")
  }
  if(grepl("all|the",todo)) {
    unames <- setdiff(names(the_av),union(px_names,nonpx_names))
    save(list=unames,envir=the_av,file=the_av$constants_fn)
    shortmsg <- paste(shortmsg,"const")
  }
  message_if_green(the_av$verbose & the_av$dbglvl>=1,"Save State (",todo,") or (",shortmsg,") from '",msg,"' at ",Sys.time())
}


# =========================================================
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
    # debug>> message_if(the_av$verbose,"save_av_data(",in_av_fun,") : Skipping save data (",skipreason,")")
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
  save_avs_state(,msg=" Ttticker RRREdrum")
}

av_dbgmode <- function() {
  source("c:\\d\\src\\R\\ut_package.R");
  av_set_defaults("dbglvl",5)
}


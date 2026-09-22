#' Extract data from Alpha Vantage retuned data
#'
#' @name av_reset_defaults
#' @param fileopts (default: TRUE)  If TRUE, then remove all files and subdirectories
#' @param keep_apikeys (default: FALSE) Keep whatever API keys are stored
#' @param resetgrep (default "*") Only reset grepped default variables
#' @returns No return
#'
#' @details Resets [av_runShiny()] defaults to original (newly installed) state
#' @seealso [av_runShiny()]
#'
#' @examples
#' \dontrun{
#' av_reset_defaults()
#' }
#'
#' @export
av_reset_defaults <- function(fileopts=TRUE,keep_apikeys=FALSE,resetgrep="*") {
  var=NULL
  defaultdf <-avsd$defaults[grepl(resetgrep,var)]
  if(keep_apikeys) {
    # also keep cachedirs
    defaultdf <- defaultdf[!data.table(var=s("avapikey;avapientitlement")),on=.(var)]
  }
  else {
    the_av$cachedir <- the_av$defaultcachedir
    if(!dir.exists(the_av$defaultcachedir)) {
      newd <- dir.create(the_av$defaultcachedir)
      message("Creating ",the_av$constants_fn)
    }
  }
  for(i in seq(1,nrow(defaultdf))) {
    ivartype <- defaultdf[i,]$vartype
    ivarnm <- defaultdf[i,]$var
    if(!(ivarnm=="cachedir")) {
      if( ivartype=="cache" ) { ivarval <- paste0(the_av$cachedir,"/", defaultdf[i,get("value_str")]) }
      else { ivarval <- defaultdf[i,get(paste0("value_",ivartype))] }
      assign(ivarnm, ivarval, envir=the_av)
    }
  }
  # Tables that will get modified by users
  the_av$assetgroups <- data.table(listnm=c(rep("defaultIdx",3)),ticker=c("SPY","QQQ","DIA"), weight=c(0.34,0.33,0.33))
  the_av$avsh_funcs <- copy(avsd$def_avsh_funcs)
  # Data to keep track of
  lapply(s("pxd;pxinv;earn;earnest;tickerlist;cmdhist"), \(x) assign(x, data.table(), envir=the_av))
  # Move files if you need
  if(fileopts==TRUE) {
    file.remove(the_av$constants_fn)
    unlink(the_av$defaultcachedir, force=TRUE,recursive=TRUE)
    unlink(the_av$cachedir, force=TRUE,recursive=TRUE)
  }
  message_if_red(TRUE,"Filling in app defaults")
}

# One time or many
av_set_defaults <- function(optnm=NULL,optval=NULL,savetoconstants=FALSE) {
  if(!is.null(optnm) & !is.null(optval)) {
    #message_if(the_av$verbose,"av_set_defaults> ",optnm,"<-",optval) # tooo verbose
    assign(optnm,optval,envir=the_av) # For some reason, pasing NA into optval destroys any previous changes to the_av
  }
  if(savetoconstants==TRUE) {
    unames=ls(envir=the_av)
    save(list=unames,envir=the_av,file= the_av$constants_fn)
    message("Saving to ",the_av$constants_fn)
  }
  return(optval)
}

av_set_default_set <- function(setset,rv,save="all") {
  persistset=NULL
  toset <- avsd$defaults[persistset==setset,]$var
  lapply(toset, \(x) av_set_defaults(x,rv[[x]]))
  save_avs_state(save,msg=paste0("av_set_default_set: ",setset))
}

av_set_caching_directories <- function() {
  vartype=NULL
  toset <- avsd$defaults[vartype=="cache",]
  for(i in seq(1,nrow(toset))) {
    assign(toset[i,]$var, paste0(the_av$cachedir,"/", toset[i,get("value_str")]), envir=the_av)
  }
}


#' Extract data from Alpha Vantage retuned data
#'
#' @name av_make_funcmap
#'
#' @returns function map dat.table, also written to data directory
#'
#' @details Creates a data.table with API signatures and default values for Alphavantage API calls
#'
#' @seealso [av_get_pf()]
#'
#' @examples
#' \dontrun{
#' av_make_funcmap()
#' }
#'
#' @import data.table
#' @export
av_make_funcmap <- function() {
  # Notes:
  # Addinging symbol t dataset ma be neeed even in functions without parameters; using symbolooverrde
    symboloverride=savekey=NULL
    av_funcmap_all <- data.table::fread("./inst/extdata/av_funcmap.csv") |>
        data.table::melt(id.vars=c("av_fn","category","outform","symboloverride","savekey"))
    av_funclist <- av_funcmap_all[,.(paramname="placeholder"),by=.(category,av_fn,outform,symboloverride,savekey)]
    av_funcmap <- av_funcmap_all[nchar(value)>0,] |>
        tidyr::separate(value,c("ro","paramname"),sep=":",fill="left") |>
        tidyr::separate(paramname,c("paramname","def_value"),sep="=",fill="right",extra="merge") |>
        data.table::data.table(key=c("av_fn","paramname")) |> dplyr::arrange(av_fn,variable)
    # hasssymbol needed to add function name to returned data.
    symbolsanyway <- c("CURRENCY_EXCHANGE_RATE","CRYPTO_INTRADAY","FX_INTRADAY","FX_DAILY","FX_WEEKLY","FX_MONTHLY",
                        "NEWS_SENTIMENT","SYMBOL_SEARCH","INDEX_CATALOG")
    # w/ params
    f_w_symbols <- av_funcmap[paramname=="symbol",.(hassymbol=(.N>0)),by=.(av_fn)]
    av_funcmap <- f_w_symbols[av_funcmap,on=.(av_fn)]
    # w/o params
    av_nofunc <- av_funclist[!av_funcmap[,.N,by=.(category,av_fn,outform)],on=.(av_fn,outform)]
    av_funcmap <- data.table::rbindlist(list(av_funcmap,av_nofunc),use.names=TRUE,fill=TRUE)
    # Combine and set hassymbol
    av_funcmap <- av_funcmap[,':='(hassymbol=fcoalesce(symboloverride,hassymbol,FALSE),outform=fcoalesce(outform,""))][,
                                    ':='(symboloverride=NULL)]
    #save(av_funcmap,file="../data/av_funcmap.rda",compress="xz")
    return(av_funcmap)
}

.addseasonaldates<- function(x,dtname="DT_ENTRY",toadd="all",freqvarname="") {
  # Really slow
  toaggdt<-function(x,to="yrwk") {
    convstr=list('yrwk'="%Y%V","yrweek"="%Y%V","yrmo"="%Y%m","dt"="%Y%m%d","wk"="%V","filedt"="%y%m%d")
    as.numeric(strftime(x,convstr[[to]])) }
  if(!is.data.frame(x)) {
    if(x=="vars") { return("doy|yr|qtr|doq|yrwk|week") }
    else { message("addseasonaldates(x,toadd =(all|yr|qtr|doq|ywk|week),dtname)")} }
  if (sum(grepl(dtname,colnames(x)))<=0 ) { xdt=as.Date(rownames(x)) }
  else { xdt=as.Date(x[[dtname]],use.names=FALSE) }
  if(grepl("doy|all",toadd)) { x$doy<-as.numeric(format(xdt,"%j")) }
  if(grepl("^(yr|all)$",toadd)) { x$yr<-as.numeric(format(xdt,"%Y")) }
  if(grepl("doq|all",toadd)) { x$doq<-as.numeric(xdt-as.Date(paste0( as.character(floor( (lubridate::month(xdt)-1)/3)*3+1),"/1/",x$yr),"%m/%d/%Y")) }
  if(grepl("(qtr)|yrqt|all|dfagg",toadd)) { x$yrqtr<-as.numeric(format(xdt,"%Y") )*10+as.numeric(substr(quarters(xdt),2,2)) }
  if(grepl("yrwk|all|dfagg",toadd)) { x$yrwk<-toaggdt(xdt) }
  if(grepl("yrmo|all|dfagg",toadd)) { x$yrmo<-toaggdt(xdt,to="yrmo") }
  if(grepl("week|all",toadd)) { x$wk<-toaggdt(xdt,to="wk")  }
  if(nchar(freqvarname)>0) { x[,freqvarname] = x[[toadd]] }
  return(x)
}

# Make datemap, very helpuful for narrowing dates.
#
#' @importFrom qlcal getHolidays
av_make_dtmap <- function(yrs_ahead=10,begDate=as.Date("1970-01-01")) {
  # All Dates
  `.`<-DT_ENTRY<-isbday<-rolldt<-yr<-yrmo<-frino<-yrqtr<-optexp<-xoptexp<-isweek<-ismo<-isqtr<-isyr<-yrwk<-NULL
  ishol_nyse<-ishol_bond<-NULL
  dtmap <- data.table::data.table(DT_ENTRY=seq(from =begDate, to = Sys.Date()+yrs_ahead*365, by = "day")) |> .addseasonaldates()
  allhols <- rbindlist(list(
    data.table(DT_ENTRY=qlcal::getHolidays(begDate, Sys.Date()+yrs_ahead*365,xp=qlcal::getCalendar("UnitedStates/NYSE")),calnm="ishol_nyse",ishol=TRUE),
    data.table(DT_ENTRY=qlcal::getHolidays(begDate, Sys.Date()+yrs_ahead*365,xp=qlcal::getCalendar("UnitedStates/GovernmentBond")),calnm="ishol_bond",ishol=TRUE),
    data.table(DT_ENTRY=qlcal::getHolidays(begDate, Sys.Date()+yrs_ahead*365,xp=qlcal::getCalendar( "UnitedKingdom/Exchange")),calnm="ishol_uk",ishol=TRUE)
  ))
  allholstr<- dcast(allhols,DT_ENTRY ~ calnm, value.var="ishol",fill=FALSE)
  dtmap_hol<-allholstr[dtmap,on=.(DT_ENTRY)][,lapply(.SD,\(x) fcoalesce(x, FALSE)), .SDcols=patterns("ishol*")]
  dtmap<- cbind(dtmap,dtmap_hol)
  cdsendpoints <-c(base::as.Date("1970-03-20"), base::as.Date(paste0(max(dtmap$yr),"-03-20")))
  u2dts <- data.table::data.table(DT_ENTRY=seq(cdsendpoints[1],cdsendpoints[2], by = '6 month'))[,':='('rolldt'=DT_ENTRY)]
  dtmap <- u2dts[dtmap,on=.(DT_ENTRY)]
  data.table::setnafill(dtmap,"locf",cols=c('rolldt'))
  data.table::setkeyv(dtmap,c("DT_ENTRY"))
  # Business days and end periods
  dtmap <- dtmap[,'isbday':=data.table::between(data.table::wday(DT_ENTRY),2,6) & !(ishol_nyse | ishol_bond)] # weekdays
  dtmapc <- data.table::copy(dtmap)
  dtmapc <- dtmapc[isbday==TRUE,]
  dtmapc <- dtmapc[,'isweek':=(DT_ENTRY==max(DT_ENTRY)),by="yrwk"]
  dtmapc <- dtmapc[,'ismo':=(DT_ENTRY==max(DT_ENTRY)),by="yrmo"]
  dtmapc <- dtmapc[,'isqtr':=(DT_ENTRY==max(DT_ENTRY)),by="yrqtr"]
  dtmapc <- dtmapc[,'isyr':=(DT_ENTRY==max(DT_ENTRY)),by="yr"]
  dtmapc[,'daysfromroll':=.I-min(.I),by='rolldt'][,'rollpd':=format(rolldt,"%Y%m")]
  dtmapc <- dtmapc[,':='('bdoy'=cumsum(isbday==TRUE)), by=.(yr)]
  # Roll Dates (CDS)
  dtmap <- dtmapc[,c('DT_ENTRY','isweek','ismo','isqtr','isyr','daysfromroll','rollpd','bdoy')][dtmap,on=.(DT_ENTRY)]
  data.table::setnafill(dtmap,"locf",cols=c("daysfromroll"))
  dtmap <- dtmap |> tidyr::fill('rollpd') # tidyr bc of character
  # Option Expirations (Equities)
  moexp <- dtmap[ishol_nyse==FALSE & isbday==TRUE,][,.SD[.N],by=.(yrwk)]
  moexp <-  moexp[,':='('frino'=.I-min(.I)),by=.(yrmo)][frino==2,][,.(DT_ENTRY,prio=2,optexp="mo")]
  qexp <- dtmap[ishol_nyse==FALSE & isbday==TRUE,][,.SD[.N],by=.(yrqtr)][,.(DT_ENTRY,prio=1,optexp="qtr")]
  wkexp <- dtmap[ishol_nyse==FALSE & isbday==TRUE,][,.SD[.N],by=.(yrwk)][,.(DT_ENTRY,prio=3,optexp="wk")]
  optexp <-rbindlist(list(wkexp,moexp,qexp))[order(DT_ENTRY,prio)][,.SD[1],by=.(DT_ENTRY)][,prio:=NULL]
  dtmap <- optexp[dtmap,on=.(DT_ENTRY)][,':='(optexp=data.table::fcoalesce(optexp,fifelse(isbday==TRUE,"dly","")))]
  # Fill in the rest
  dtmap <- dtmap[,':='('isweek'=data.table::fcoalesce(isweek,FALSE),'ismo'=data.table::fcoalesce(ismo,FALSE),
                       'isqtr'=data.table::fcoalesce(isqtr,FALSE),'isyr'=data.table::fcoalesce(isyr,FALSE))][]
  return(dtmap)
}

#' @importFrom usethis use_data
av_shiny_constants <- function() {
  tinputformstyle <- I("{font-size:12px; font-weight:bold; background-color: #ffff99; padding-top:0px; padding-bottom: 0px}")
  yellowed_inputs <- s("#dtstr_hist;#events;#ochains")
  avsd <- list(
    "defaults"=data.table::fread("./inst/extdata/av_defaults.csv"),
    "generalhelp"=data.table::fread("./inst/extdata/help_docs.csv"),
    "crypto_list"=data.table::fread("./inst/extdata/cryptocurrency_list.csv"),
    "table_aes"= data.table::fread("./inst/extdata/table_aes.csv"), # columns widths, extra HTML
    "def_avsh_funcs"= data.table::fread("./inst/extdata/avsh_funcs.csv"), # Functions and options
    "avsh_elements"= data.table::fread("./inst/extdata/avsh_elements.csv"), # Functions and options
    "overviewlist"=data.table::fread("./inst/extdata/overview_map.csv"),  # Formatting of description tables
    "abbreviations"=data.table::fread("./inst/extdata/name_abbreviations.csv"),  # Abbreviations
    "edit_tableoptions"=list('editable1'='row','editable2'='row','pagelen1'=80,'pagelen2'=80,'digits'=3),
    "selectizeoptions" =I("selectize-input: 12px; background-color:red"),
    "inputcss_side" = paste(lapply(yellowed_inputs,(\(x) paste(x,tinputformstyle))),collapse=" "),
    #"inputcss_top" =paste(lapply(s("#istr1;#istr2"),\(x) paste(x,tinputformstyle)),collapse=" "),
    #"labelcss" =I("font-size:11pt color:red font-weight:bold"),
    "othercss2"=I("gropts {font-size:10px; background-color: #ddfcd9}"),
    "av_mcp_base_json"=readLines("./inst/extdata/mcp_config.json")
  )
  av_funcmap <- av_make_funcmap()
  dtmap  <- av_make_dtmap()
  usethis::use_data(av_funcmap, dtmap, avsd, internal = TRUE,overwrite=TRUE)
  return("Made default data into avsd")
}


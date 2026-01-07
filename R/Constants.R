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
#' @export
av_make_funcmap <- function() {
    av_funcmap_all <- data.table::fread("./inst/extdata/av_funcmap.csv") |>
        data.table::melt(id.vars=c("av_fn","category","outform"))
    av_funclist <- av_funcmap_all[,.(paramname="placeholder"),by=.(category,av_fn,outform)]
    av_funcmap <- av_funcmap_all[nchar(value)>0,] |>
        tidyr::separate(value,c("ro","paramname"),sep=":",fill="left") |>
        tidyr::separate(paramname,c("paramname","def_value"),sep="=",fill="right",extra="merge") |>
        data.table::data.table(key=c("av_fn","paramname")) |> dplyr::arrange(av_fn,variable)
    # hasssymbol needed to add function name to returned data.
    symbolsanyway <- c("CURRENCY_EXCHANGE_RATE","CRYPTO_INTRADAY","FX_INTRADAY","FX_DAILY","FX_WEEKLY","FX_MONTHLY")
    f_w_symbols <- av_funcmap[paramname=="symbol",.(hassymbol=(.N>0)),by=.(av_fn)]
    av_funcmap <- f_w_symbols[av_funcmap,on=.(av_fn)]
    av_funcmap <- av_funcmap[,':='(hassymbol=data.table::fcoalesce(hassymbol,data.table::fifelse(av_fn %in% symbolsanyway,TRUE,FALSE)),
                                   outform=data.table::fcoalesce(outform,""))][]
    av_nofunc <- av_funclist[!av_funcmap[,.N,by=.(category,av_fn,outform)],on=.(av_fn,outform)][,':='(hassymbol=FALSE)]
    av_funcmap <- data.table::rbindlist(list(av_funcmap,av_nofunc),use.names=TRUE,fill=TRUE)
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
    else { print("addseasonaldates(x,toadd =(all|yr|qtr|doq|ywk|week),dtname)")} }
  if (sum(grepl(dtname,colnames(x)))<=0 ) { xdt=as.Date(rownames(x)) }
  else { xdt=as.Date(x[[dtname]],use.names=F) }
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
av_make_dtmap <- function(yrs_ahead=5) {
  # All Dates
  dtmap <- data.table::data.table(DT_ENTRY=seq(from =as.Date("1970-03-20"), to = Sys.Date()+yrs_ahead*365, by = "day")) |> .addseasonaldates()
  dtmap <- dtmap[,':='(isholiday=!timeDate::isBizday(timeDate::as.timeDate(DT_ENTRY), holidays =  timeDate::holidayNYSE(1970:2060), wday = 1:5))]
  cdsendpoints <-c(lubridate::ymd('1970-03-20'),lubridate::ymd(paste0(lubridate::year(Sys.Date())+yrs_ahead,'-09-20')))
  u2dts <-data.table::data.table(DT_ENTRY=seq(cdsendpoints[1],cdsendpoints[2], by = '6 month'))[,':='('rolldt'=DT_ENTRY)]
  dtmap <- u2dts[dtmap,on=.(DT_ENTRY)]
  data.table::setnafill(dtmap,"locf",cols=c('rolldt'))
  data.table::setkeyv(dtmap,c("DT_ENTRY"))
  # Business days and end periods
  dtmap <- dtmap[,'isday':=data.table::between(lubridate::wday(DT_ENTRY),2,6)]
  dtmapc <- data.table::copy(dtmap)
  dtmapc <- dtmapc[isday==TRUE,]
  dtmapc <- dtmapc[,'isweek':=(DT_ENTRY==max(DT_ENTRY)),by="yrwk"]
  dtmapc <- dtmapc[,'ismo':=(DT_ENTRY==max(DT_ENTRY)),by="yrmo"]
  dtmapc <- dtmapc[,'isqtr':=(DT_ENTRY==max(DT_ENTRY)),by="yrqtr"]
  dtmapc <- dtmapc[,'isyr':=(DT_ENTRY==max(DT_ENTRY)),by="yr"]
  dtmapc[,'daysfromroll':=.I-min(.I),by='rolldt'][,'rollpd':=format(rolldt,"%Y%m")]
  dtmapc <- dtmapc[,':='('bdoy'=cumsum(isholiday==FALSE)), by=.(yr)]
  # Roll Dates (CDS)
  dtmap <- dtmapc[,c('DT_ENTRY','isweek','ismo','isqtr','isyr','daysfromroll','rollpd','bdoy')][dtmap,on=.(DT_ENTRY)]
  data.table::setnafill(dtmap,"locf",cols=c("daysfromroll"))
  dtmap <- dtmap |> tidyr::fill('rollpd') # tidyr bc of character
  # Option Expirations (Equities)
  moexp <- dtmap[lubridate::wday(DT_ENTRY)==6,][,':='('frino'=.I-min(.I)),by=.(yrmo)][frino==2,][,.(DT_ENTRY,optexp="mo")]
  qexp <- dtmap[isholiday==FALSE & isday==TRUE,][,.SD[.N],by=.(yrqtr)][,.(DT_ENTRY,xoptexp="qtr")]
  dtmap <- moexp[dtmap,on=.(DT_ENTRY)][,':='(optexp=data.table::fcoalesce(optexp,""))]
  dtmap <- qexp[dtmap,on=.(DT_ENTRY)][,':='(optexp=paste0(optexp,data.table::fcoalesce(xoptexp,"")))][,':='(xoptexp=NULL)]
  dtmap <- dtmap[,':='('isweek'=data.table::fcoalesce(isweek,FALSE),'ismo'=data.table::fcoalesce(ismo,FALSE),
                        'isqtr'=data.table::fcoalesce(isqtr,FALSE),'isyr'=data.table::fcoalesce(isyr,FALSE))][]
  return(dtmap)
}


# av_funcmap <- av_make_funcmap()
# dtmap  <- av_make_dtmap()
# usethis::use_data(av_funcmap, dtmap, internal = TRUE,overwrite=TRUE)

# =======================================================================================================
#' App Support functions
#'
#' @noRd
symbol_grep_by_type <- function(eqlist,grepstr="Equity", rtn="list") {
  symbol=NULL
  tickerset <- the$pxinv[data.table(symbol=eqlist),on=.(symbol)][,.(symbol,type,currency)]
  tickerset <- tickerset[grepl(grepstr,type,ignore.case=TRUE),]
  if(rtn=="list") {
    return(tickerset$symbol)
  }
  else {
    return(tickerset)
  }
}

get_one_ts <- function(assetlist,rebase,datestring,dtstartfrac) {
  symbol=NULL
  toplot <- the$pxd[data.table(symbol=assetlist),on=.(symbol)] |> narrowbydtstr(datestring)
  rebasedt <- fcase(rebase=="none","",
                    rebase=="start",paste0(format(toplot[1,]$timestamp,"%Y-%m-%d"),",100"),
                    rebase=="focus",paste0(format(toplot[,.N,by=.(timestamp)][,.SD[floor(.N*dtstartfrac/100)]]$timestamp,"%Y-%m-%d"),",100"))
  message_if_green(the$verbose,"get_one_ts(",paste0(assetlist,collapse=" "),") gets ",nrow(toplot), " rows to  ",as.Date(max(toplot$timestamp)))
  return(list(toplot,rebasedt))
}

# Earnings Data

oneticker_earns <- function(thisticker,fwddts,datestring) {
  symbol=horizon=eps_estimate_average=eps_estimate_high=eps_estimate_low=eps_estimate_average_30_days_ago=NULL
  eps_estimate_analyst_count=eps_estimate_average_90_days_ago=reportTime=fiscalDateEnding=EPpct=estimatedEPS=NULL
  inspot <- av_get_pf(thisticker,"GLOBAL_QUOTE")$price
  earnb <- av_get_pf(thisticker,"EARNINGS") |> save_av_data("EARNINGS")
  earnb <- earnb |> av_extract_df("quarterlyEarnings") |> narrowbydtstr(datestring)
  earnf <- data.table()
  if(fwddts[2]>Sys.Date()) {
    earnf <- av_get_pf(thisticker,"EARNINGS_ESTIMATES") |> save_av_data("EARNINGS_ESTIMATES")
    earnf <- earnf |> av_extract_df("estimates") |> narrowbydtstr(paste0(fwddts,collapse="::"))
    earnf <- earnf[,.(symbol,fiscalDateEnding=date,reportTime=horizon,
                      estimatedEPS= eps_estimate_average, est_high=eps_estimate_high, est_low=eps_estimate_low,
                      est_30dpchg=(eps_estimate_average/eps_estimate_average_30_days_ago-1),
                      est_90dpchg=(eps_estimate_average/eps_estimate_average_90_days_ago-1),
                      est_n=eps_estimate_analyst_count)]
    earnf <- earnf[order(symbol,reportTime,fiscalDateEnding)]
  }
  earna <- rbindlist(list(earnb,earnf),fill=TRUE,use.names=TRUE)
  earna <- earna[,EPpct:=100*estimatedEPS/inspot]
  setcolorder(earna,s("symbol;fiscalDateEnding;estimatedEPS;reportedEPS;EPpct;reportTime;reportedDate"))
  return(earna[])
}

# Divs Data

oneticker_divs <- function(thisticker,datestring) {
  payment_date=divdays=annDiv=amount=ex_dividend_date=symbol=NULL
  divs_1 <-  av_get_pf(thisticker,"DIVIDENDS") |> save_av_data("DIVIDENDS") |> av_extract_divs_or_splits()
  divs_2 <-  av_get_pf(thisticker,"SPLITS") |> save_av_data("SPLITS")
  alldivs<- list()
  if(nrow(divs_1)>0) {
    divs_1 <- divs_1[order(payment_date)][,divdays:=as.numeric(c(NA_integer_,diff(payment_date,1)))]
    alldivs<- append(alldivs, list(divs_1[,annDiv:=amount*365/divdays][]))
  }
  if(nrow(divs_2)>0) {
    divs_2 <- divs_2 |> av_extract_divs_or_splits() |> setnames("effective_date","ex_dividend_date")
    alldivs<- append(alldivs, list(divs_2))
  }
  if(length(alldivs)==0) { return(data.table()) }
  divs <- rbindlist(alldivs,fill=TRUE,use.names=TRUE)
  if(nrow(divs)>0) {
    divs <- divs[order(-ex_dividend_date)][,symbol:=thisticker][]
    setcolorder(divs,"symbol")
    divs <- divs  |> narrowbydtstr(datestring)
  }
  return(divs[])
}

one_px_ts <- function(toplot,rv,title="Prices",extra_anno="",events=NULL,dtstartfrac=NULL) {
  symbol=low=high=NULL
  seriesnm <- fifelse(rv$totrtn,"adjusted_close","close")
  if(is.data.table(toplot[[1]])) {
    trebase <- toplot[[2]]
    fgdt <- toplot[[1]][,.(timestamp,variable=symbol,value=get(seriesnm))]
    if("hilow" %in% rv$gropts) {
      fgdt <- rbindlist(list(fgdt,
                             toplot[[1]][,.(timestamp,variable=paste0(symbol,".lo"),value=get(seriesnm) + (low-close))],
                             toplot[[1]][,.(timestamp,variable=paste0(symbol,".hi"),value=get(seriesnm) + (high-close))]
      )) }
  }
  else {
    fgdt<-toplot
    trebase<-""
  }
  tanno <- fcase(
    "lastlabel" %in% rv$gropts, "last,line",
    "last" %in% rv$gropts, "last,linevalue",
    default = "")
  outdyg <- fgts_dygraph(fgdt,title=title,events=events, dtstartfrac=dtstartfrac/100,
                         annotations=paste0(c(tanno,extra_anno),collapse=";"), colorset=the$ts_colorset,
                         splitcols=("splitts" %in% rv$gropts),roller=1,
                         hilightcols=fifelse("hilightfirst" %in% rv$gropts,fgdt[,.SD[1]]$variable,""),
                         rebase=trebase)
  return(outdyg)
}

getNews<-function(x,nArticles=50,minabssent=0,newsfilter=list(),maxage=+Inf) {
  news0=news1=artno=ticker=overall_sentiment_score=time_published=title=NULL
  news0 <- av_get_pf(x,"NEWS_SENTIMENT",limit=floor(nArticles)) |>  save_av_data("NEWS_SENTIMENT")
  news1 <- news0 |> av_extract_df("feed")
  news1[,age:=difftime(Sys.time(),time_published)]
  if("tickerOnly"%in% newsfilter) {
    tickermentions<-lapply(1:nrow(news1), \(i) data.table(news1[i,]$ticker_sentiment[[1]])[,artno:=i])
    tickermentions<-rbindlist(tickermentions)[ticker==x,]
    news1<-news1[tickermentions$artno,]
  }
  news1<-news1[abs(overall_sentiment_score)>=fifelse("useMinSentiment" %in% newsfilter,minabssent,0),]
  news1<-news1[age<=fifelse("maxDays" %in% newsfilter, maxage,+Inf),]
  return(news1[,.(symbol=x,age,time_published,sntmt=overall_sentiment_score,nlink=paste0("[",title,"](",url,")"),source)])
}

fillin_defaults <- function() {
  if(!exists("avapikey",envir=the)) {
    for(i in seq(1,nrow(avsd$defaults))) {
      ivartype <- avsd$defaults[i,]$vartype
      ivarnm <- avsd$defaults[i,]$var
      if( ivartype=="cache" ) { assign(ivarnm, paste0(the$cachedir,"/", avsd$defaults[i,]$value_str), envir=the) }
      if( ivartype=="str" ) { assign(ivarnm,  avsd$defaults[i,]$value_str, envir=the) }
      if( ivartype=="log" ) { assign(ivarnm,  avsd$defaults[i,]$value_log, envir=the) }
      if( ivartype=="num" ) { assign(ivarnm,  avsd$defaults[i,]$value_num, envir=the) }
    }
    message_if_red(TRUE,"Filling in app defaults")
  }
}

fillin_defaults <- function() {
  if(!exists("avapikey",envir=the)) {
    for(i in seq(1,nrow(avsd$defaults))) {
      ivartype <- avsd$defaults[i,]$vartype
      ivarnm <- avsd$defaults[i,]$var
      if( ivartype=="cache" ) {
        ivarval <- paste0(the$cachedir,"/", avsd$defaults[i,get("value_str")])
      }
      else {
        ivarval <- avsd$defaults[i,get(paste0("value_",ivartype))]
      }
      assign(ivarnm, ivarval, envir=the)
    }
    message_if_red(TRUE,"Filling in app defaults")
  }
}


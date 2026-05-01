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
                    rebase=="start",paste0(format(toplot[1,]$date,"%Y-%m-%d"),",100"),
                    rebase=="focus",paste0(format(toplot[,.N,by=.(date)][,.SD[floor(.N*dtstartfrac/100)]]$date,"%Y-%m-%d"),",100"))
  message_if_green(the$verbose,"get_one_ts(",paste0(assetlist,collapse=" "),") gets ",nrow(toplot), " rows to  ",as.Date(max(toplot$date)))
  return(list(toplot,rebasedt))
}

# Earnings Data

oneticker_earns <- function(thisticker,fwddts,datestring) {
  symbol=horizon=eps_estimate_average=eps_estimate_high=eps_estimate_low=eps_estimate_average_30_days_ago=NULL
  eps_estimate_analyst_count=eps_estimate_average_90_days_ago=reportTime=fiscalDateEnding=EPpct=estimatedEPS=NULL
  inspot <- av_get_pf(thisticker,"GLOBAL_QUOTE")[variable=="price",]$value_num
  earnb <- av_get_pf(thisticker,"EARNINGS") |> av_extract_df("quarterlyEarnings") |> narrowbydtstr(datestring)
  earnf <- data.table()
  if(fwddts[2]>Sys.Date()) {
    message("heer")
    earnf <- av_get_pf(thisticker,"EARNINGS_ESTIMATES") |> av_extract_df("estimates") |> narrowbydtstr(paste0(fwddts,collapse="::"))
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
  return(earna)
}

# Divs Data

oneticker_divs <- function(thisticker,datestring) {
  payment_date=divdays=annDiv=amount=ex_dividend_date=symbol=NULL
  divs_1 <-  av_get_pf(thisticker,"DIVIDENDS") |> av_extract_divs_or_splits()
  divs_2 <-  av_get_pf(thisticker,"SPLITS")
  alldivs<- list()
  if(nrow(divs_1)>0) {
    divs_1 <- divs_1[order(payment_date)][,divdays:=as.numeric(c(NA_integer_,diff(payment_date,1)))]
    alldivs<- list(alldivs, divs_1[,annDiv:=amount*365/divdays])
  }
  if(nrow(divs_2)>0) {
    divs_2 <- divs_2 |> av_extract_divs_or_splits() |> setnames("effective_date","ex_dividend_date")
    alldivs<- list(alldivs, divs_2)
  }
  divs <- rbindlist(alldivs,fill=TRUE,use.names=TRUE)
  if(nrow(divs)>0) {
    divs <- divs[order(-ex_dividend_date)][,symbol:=thisticker][]
    setcolorder(divs,"symbol")
  }
  return(divs  |> narrowbydtstr(datestring))
}

one_px_ts <- function(toplot,rv,title="Prices",extra_anno="",events=NULL,dtstartfrac=NULL) {
  symbol=low=high=NULL
  seriesnm <- fifelse(rv$totrtn,"adjusted_close","close")
  if(is.data.table(toplot[[1]])) {
    trebase <- toplot[[2]]
    fgdt <- toplot[[1]][,.(date,variable=symbol,value=get(seriesnm))]
    if("hilow" %in% rv$gropts) {
      fgdt <- rbindlist(list(fgdt,
                             toplot[[1]][,.(date,variable=paste0(symbol,".lo"),value=get(seriesnm) + (low-close))],
                             toplot[[1]][,.(date,variable=paste0(symbol,".hi"),value=get(seriesnm) + (high-close))]
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
                         splitcols=("splitts" %in% rv$gropts),
                         hilightcols=fifelse("hilightfirst" %in% rv$gropts,fgdt[,.SD[1]]$variable,""),
                         rebase=trebase)
  return(outdyg)
}

getNews<-function(x,nArticles=50,newsfilter="") {
  news0=news1=artno=ticker=overall_sentiment_score=minabssent=time_published=title=NULL
  news0 <- av_get_pf(x,"NEWS_SENTIMENT",limit=floor(nArticles)) |>  save_av_data("NEWS_SENTIMENT")
  news1 <- news0 |> av_extract_df("feed")
  if(newsfilter=="tickerOnly") {
    tickermentions<-lapply(1:nrow(news1), \(i) data.table(news1[i,]$ticker_sentiment[[1]])[,artno:=i])
    tickermentions<-rbindlist(tickermentions)[ticker==x,]
    news1<-news1[tickermentions$artno,]
  }
  if(newsfilter=="opinion") {
    news1<-news1[abs(overall_sentiment_score)>=minabssent,]
  }
  return(news1[,.(symbol=x,time_published,sntmt=overall_sentiment_score,nlink=paste0("[",title,"](",url,")"),source)])
}

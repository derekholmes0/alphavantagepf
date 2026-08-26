

# =======================================================================================================
#' Unexported App Support functions" Data and interface
#'
# form_symset Finds or downloads the asset type for a given ticker or list.  Sucks that Alphavantage can't unify these

# form_symset(c("JBL","EEMA","NDX","USD/BRL","BTC/USD","FEDFUNDS"))
#'
#' @noRd
#' @importFrom stringr str_extract
form_symset <- function(tickers, force=FALSE, typegrep="*",delay=0) {
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
  # New equities/ETFs (US)
  symnew_eq_inlistings <- the_av$listings[data.table(symbol=sort(newtickers)),on=.(symbol),nomatch=NULL][,
                                                                                                         .(symbol,type=fifelse(assetType=="Stock","Equity",assetType),name,currency="USD",matchScore=1,list_ts)]
  newtickers <-setdiff(newtickers,symnew_eq_inlistings$symbol)
  symnew_eq_nonus <- rbindlist(lapply(grepv("\\.([A-Z]{3})$",newtickers), \(x) {
    message_if_green(the_av$verbose, "Checking for international equities: trying av('SYMBOL_SEARCH')")
    if(nrow(z1 <- av_get_pf("","SYMBOL_SEARCH",keywords=x,delay=delay))<=0) {
      message_if_red(TRUE,"Alphavantage cannot find  ",x,", so assuming to be a user series.  Please reconsider renaming")
      return(data.table(symbol=x,matchScore=0))
    }
    else {
      return(z1[matchScore>=0.95,.(symbol=x,type,currency,name,matchScore,list_ts=Sys.Date())])
    }
  }))
  if(nrow(symnew_eq_nonus)>0)  {
    newtickers <- setdiff(newtickers,symnew_eq_nonus$symbol)
  }
  # -----------------------------------------Un Downloadable assets/ User Data
  symnew_user <- data.table()
  if( length(newtickers)>0 ) {
    symnew_user <- data.table(symbol=newtickers,type="user",currency="USD",matchScore=1,list_ts=Sys.Date())
    the_av$tickerlist <- DTUpsert(the_av$tickerlist,symnew_user[,.(symbol,name=symbol,type,list_ts)],c("symbol"))
  }
  # Collect all together, inferable = ix,crpt,fx
  symset <- rbindlist(list(symset,symnew_inferable,symnew_eq_inlistings,symnew_eq_nonus,symnew_user),fill=TRUE,use.names=TRUE)
  return(symset[grepl(typegrep,type),][])
}

# REturns command, then assets
#' @importFrom utils tail
parse_inpline <- function(istr1,envir=parent.frame()) {
  istrs <- s(toupper(istr1),sep="[ ]+",pad=2)
  tcmd <- targs <- tasset <- ""
  if(length(istrs)>0) {
    if(grepl("^AV.",toupper(istrs)[[1]]) | length(istrs)==1) {  # No asset list or some sort of error
      tcmd <- stringr::str_squish(istrs[[1]])
      targs <- stringr::str_squish(paste(tail(istrs,-1),collapse=" "))
    }
    else {
      tasset <- stringr::str_squish(istrs[[1]])
      tcmd <- stringr::str_squish(istrs[[2]])
      targs <- stringr::str_squish(paste(tail(istrs,-2),collapse=" "))
      if(!grepl(";",tasset)) {  # Silently expand
        if( nchar(newassets <- paste0( the_av$assetgroups[grepl(tasset,listnm,ignore.case=TRUE),]$ticker,collapse=";"))>0 ) {
          tasset <- newassets
        }
      }
    }
  }
  outlist = list("todo"=stringr::str_squish(paste(tcmd,targs)),"todofunc"=tcmd,"todoargs"=targs,"assetline"=tasset)
  if(is.environment(envir)) { list2env(outlist,envir=envir)  }
  else{   return(outlist)  }
}



#' @noRd
av_validate_directory <- function(newdir,src_inputID) {
  outdir<-""
  if(nchar(newdir)>0) {
    newdir <- gsub("\\","/",newdir,fixed=TRUE)
    if(!dir.exists(newdir)) {
      quick_message("Invalid Directory.....",wh=src_inputID)
    }
    else {
      quick_message("",wh=src_inputID)
      outdir<-newdir
    }
  }
  message("av_validate_directory(",newdir,") : ",outdir)
  return(outdir)
}

av_determine_output_locs <- function(inlist,location="MAIN") {
  Location=noin_class=inclass=ninlist=NULL
  telements <- avsd$avsh_elements
  if(length(names(inlist))>0) {
    if(length(  invalid_ids <- setdiff(names(inlist),telements$outname) )>0 ) {
      message_if_red(TRUE,"Invalid output elements: ",invalid_ids, " are being dropped")
    }
    return(intersect(names(inlist),telements$outname))
  }
  else {
    outlocs <- telements[Location==location,][,noin_class:=(.I-min(.I)), by=.(inclass)]
    prefix_grep <- paste0(unique(telements$inclass),collapse="|")
    listclasses <- data.table(inclass= sapply(inlist,\(x) grepv(prefix_grep,class(x))))[,ninlist:=.I]
    listclasses <- listclasses[order(inclass)][,noin_class:=(.I-min(.I)), by=.(inclass)]
    listclasses <- outlocs[listclasses,on=.(inclass,noin_class)]
    return(listclasses[order(ninlist)]$outname)
  }
}

find_arg <- function(x,argnm,altno=2) { # altno<0 : don't use
  sepx <- s(x," ")
  argnm1 <- s(grepv(paste0(argnm,"="),sepx,ignore.case=T),"=")[[2]]
  argnm2 <- NULL
  if(length(sepx)>=altno && altno>=2) { # A little hacky
    if( !grepl("=",sepx[altno]) ) argnm2 <- sepx[altno]
  }
  return(argnm1 %||% argnm2)
}

#' @importFrom stringr str_sub
find_rebasecode <- function(todo,default_window=the_av$dtstr_hist) {
  # SCAT, GP <- Need to figure out how better to generalize
  todolist <- grepv("\\w+|\\d+",s(toupper(todo),"[ ]+"))
  actual_func <- gsub("\\d$","",todolist[[1]])
  lastchar <- stringr::str_sub(actual_func, -1)
  ts_rebase <- switch(lastchar, "I"="start","D"="focus") %||% "none"
  dtstr_window <- default_window
  if(ts_rebase=="focus") {
    if(length(todolist)<2) { message_if_red(the_av$verbose,"Rebasing date not specified, dedaulting to start") }
    else { dtstr_window<- todolist[[2]]  }
  }
  dtstr_window <- find_arg(todo,"w") %||% dtstr_window
  ts_title <- switch(lastchar, "I"="Index","D"=paste("Index centered at",dtstr_window),"R"="LogReturns (bp)") %||% "Prices"
  #message_if_green(the_av$verbose,paste("rebase",ts_rebase,"rebase_window",dtstr_window,"func",actual_func))
  return(list("rebase"=ts_rebase,"rebase_window"=dtstr_window,"func"=actual_func,"grtitle"=ts_title))
}

set_list <- function(listtodo,tlist,instr,session) {
  assetline=todo=NULL
  rtnmsg <- ""
  parse_inpline(instr) # todo, todofunc, todoargs,assetline
  tlistgrep <- paste0("^",tlist,"$")
  parse_inpline(instr) # Makes todo; excpanded assetline
  if(listtodo=="Save") {
    if(nchar(instr)<=0 | nchar(tlist)<=0) {
      rtnmsg <- "Cannot Save blank assetgroups Name"
    }
    else {
      newassets <- data.table(ticker=s(assetline))[,listnm:=tlist][,weight:=1/.N][]
      the_av$assetgroups <- DTUpsert(the_av$assetgroups,newassets,c("listnm"),replaceifbempty=the_av$assetgroups[!(listnm==tlist),])
      updateSelectizeInput(session,"assetgp_list", choices=sort(unique(the_av$assetgroups$listnm)))
      rtnmsg <-paste0("Set saved as ",tlist, " w/ Equal Weights")
      av_set_defaults("inpline1", paste0(tlist," ",todo))
      updateTextInput(session,"istr1", value= the_av[["inpline1"]])
    }
  }
  if(listtodo=="Expand") {
    av_set_defaults("inpline1", paste0(assetline," ",todo))
    updateTextInput(session,"istr1", value= the_av[["inpline1"]])
  }
  if(listtodo=="Delete") {
    the_av$assetgroups <- the_av$assetgroups[!grepl(tlistgrep,listnm),]
    updateSelectizeInput(session,"assetgp_list", choices=sort(unique(the_av$assetgroups$listnm)))
    rtnmsg <-paste0("Deleted Asset List: ",tlist)
  }
  save_avs_state("all",msg=paste0("set_list:",listtodo))
  Sys.sleep(0.2)
  updateSelectInput(session,"ag_state",selected="--")
  return(rtnmsg)
}

# =====================-==============================================================================
# =====================-==============================================================================
# Specific function helpers
# =====================-==============================================================================


data_from_list <-function(inlist,datestring,ts_rebase,dtstr_window,msg_inputID="istr1",copytable=TRUE) {
  inlist=unique(inlist)
  # Make sure we have data
  toplot <- sapply(inlist, \(x) manage_epx(x,datestring,addlive=the_av$uselive))
  if(length(badtickers <- names(toplot)[grep("ERROR",toplot)])>0) {
      quick_message(paste("Invalid tickers:", paste(badtickers)),wh=msg_inputID)
  }
  inlist <- inlist[!grepl("ERROR",toplot)]
  # Now retreive it
  toplot <- get_one_ts(inlist,ts_rebase,datestring,dtstr_window) # REturns list(data.table,"") if nothing valid
  if(copytable) { avsh_clipboard(toplot[[1]],"px") }
  return(toplot)
}

get_one_ts <- function(assets,rebase,datestring,dtstr_window) {
  symbol=NULL
  toplot <- the_av$pxd[data.table(symbol=assets),on=.(symbol)] |> narrowbydtstr(datestring)
  rebasedt <- fcase(rebase=="start",paste0(format(toplot[1,]$timestamp,"%Y-%m-%d"),",100"),
                    rebase=="focus",paste0(format(max(toplot[1,]$timestamp,gendtstr(dtstr_window,rtn="first")),"%Y-%m-%d"),",100"),
                    default="")
  #message_if_green(the_av$verbose,"get_one_ts(",paste0(assets,collapse=" "),") retrieves ",nrow(toplot), " rows up to ",as.Date(max(toplot$timestamp)))
  return(list(toplot,rebasedt))
}

# Earnings Data
oneticker_earns <- function(thisticker,fwddts,datestring) {
  symbol=horizon=eps_estimate_average=eps_estimate_high=eps_estimate_low=eps_estimate_average_30_days_ago=NULL
  eps_estimate_analyst_count=eps_estimate_average_90_days_ago=reportTime=fiscalDateEnding=EPpct=estimatedEPS=NULL
  inspot <- av_get_pf(thisticker,"GLOBAL_QUOTE")$price
  earnb <- av_get_pf(thisticker,"EARNINGS") |> save_av_data("EARNINGS")
  quick_message("Earnings not available for ",thisticker, eval=nrow(earnb)<=0) && return();
  earnb <- earnb |> av_extract_df("quarterlyEarnings") |> narrowbydtstr(datestring)
  earnf <- data.table()
  if(fwddts[2]>Sys.Date()) {
    earnf <- av_get_pf(thisticker,"EARNINGS_ESTIMATES") |> save_av_data("EARNINGS_ESTIMATES")
    earnf <- earnf |> av_extract_df("estimates")
    if(nrow(earnf)>0) {
      earnf <- earnf |> narrowbydtstr(paste0(fwddts,collapse="::"))
      earnf <- earnf[,.(symbol,fiscalDateEnding=date,reportTime=horizon,
                      estimatedEPS= eps_estimate_average, est_high=eps_estimate_high, est_low=eps_estimate_low,
                      est_30dpchg=(eps_estimate_average/eps_estimate_average_30_days_ago-1),
                      est_90dpchg=(eps_estimate_average/eps_estimate_average_90_days_ago-1),
                      est_n=eps_estimate_analyst_count)]
    earnf <- earnf[order(symbol,reportTime,fiscalDateEnding)]
    }
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

one_px_ts <- function(toplot,rv,title="Prices",extra_anno="",events=NULL,dt_window=NULL) {
  symbol=low=high=medgap=reportedEPS=surprise=lpx=dividend_amount=surprisePercentage=NULL
  seriesnm <- the_av$seriesnm
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
  # NarrowtoBUsinessDays
  if(!grepl("AllDaysOnGraph",the_av$logopts)) {
    fgdt <- dtmap[isholiday==FALSE,.(timestamp=DT_ENTRY)][fgdt,on=.(timestamp),nomatch=NULL]
  }
  # Annotations
  tanno <- fcase(
    "lastlabel" %in% rv$gropts, "last,line",
    "last" %in% rv$gropts, "last,linevalue",
    default = "")
  # What to step
  xstepcols = the_av$pxinv[data.table(symbol=unique(fgdt$variable)),on=.(symbol)][fcoalesce(as.numeric(medgap),1)>4,]
  if(nrow(xstepcols)>0) { stepcols=xstepcols$symbol } else { stepcols<- FALSE }
  # Eartnings or dividends
  eventset <- data.table()
  eventlist <- s(tolower(events))
  symb_dt <- data.table(symbol=unique(fgdt$variable))
  eventdtrange <- paste(range(fgdt$timestamp),collapse="::")
  if("earn" %in% eventlist) {
    teventset <- the_av$earn[symb_dt,on=.(symbol)] |> narrowbydtstr(eventdtrange)
    teventset <- teventset[,.(timestamp =reportedDate,color="darkgreen",text=paste0("EPS(",symbol,") ",format(reportedEPS,digits=3)),loc="top")]
    eventset <- rbindlist(list(eventset, teventset))
  }
  if("surp" %in% eventlist) {
    teventset <- the_av$earn[symb_dt,on=.(symbol)] |> narrowbydtstr(eventdtrange)
    teventset <- teventset[,.(timestamp =reportedDate,color=fifelse(surprisePercentage>0,"blue","red"),
                              text=paste0("Surp(",symbol,") ",format(surprisePercentage,digits=1)),loc="bottom")]
    eventset <- rbindlist(list(eventset, teventset))
  }
  if("div" %in% eventlist | "divpct" %in% eventlist) {
    teventset <- the_av$pxd[symb_dt,on=.(symbol)][,lpx:=shift(close,1,0,"lag"),by=.(symbol)][ abs(dividend_amount)>0,] |> narrowbydtstr(eventdtrange)
    if("divpct" %in% eventlist) {
      teventset <- teventset[,.(timestamp,color="gray40",text=paste0("Div%(",symbol,") ",format(100*dividend_amount/lpx,digits=2)))]
    } else {
      teventset <- teventset[,.(timestamp,color="gray40",text=paste0("Div(",symbol,") ",format(dividend_amount,digits=3)))]
    }
    eventset <- rbindlist(list(eventset, teventset),fill=TRUE,use.names=TRUE)
  }
  outdyg <- fgts_dygraph(fgdt,title=title,events=events, dtwindow=dt_window,
                         annotations=paste0(c(tanno,extra_anno),collapse=";"), colorset=the_av$ts_colorset,
                         splitcols=("splitts" %in% rv$gropts),roller=1,
                         stepcols=stepcols,event_ds=eventset,
                         hilightcols=fifelse("hilightfirst" %in% rv$gropts,fgdt[,.SD[1]]$variable,""),
                         rebase=trebase)
  return(outdyg)
}


get_allNews <- function(eqlist,rv) {
  symbol=sntmt=time_published=NULL
  allnews=data.table()
  eqset <- form_symset(eqlist)[grepl("Equity|ETF",type),]$symbol
  allnews <- rbindlist(lapply(eqset,\(x)
                              getNews(x,nArticles=rv$nArticles,minabssent=rv$minabssent,newsfilter=rv$newsfilter,
                                      newsagrep=rv$newsgrep,maxage=rv$maxagedays)))
  if(rv$newssort=="sentiment") { allnews<- allnews[order(symbol,-sntmt)] }
  if(rv$newssort=="time,symbol") { allnews<- allnews[order(-time_published,symbol)] }
  if(rv$newssort=="symbol,time") { allnews<- allnews[order(symbol, -time_published)] }
  return(allnews)
}

getNews<-function(x,nArticles=50,minabssent=0,newsfilter=list(),newsagrep="",maxage=+Inf) {
  news0=news1=artno=ticker=overall_sentiment_score=time_published=title=NULL
  news0 <- av_get_pf(x,"NEWS_SENTIMENT",limit=floor(nArticles)) |>  save_av_data("NEWS_SENTIMENT")
  news1 <- news0 |> av_extract_df("feed",empty_dt_onerror=TRUE)
  if(nrow(news1)<=1) {
    message_if_red(the_av$verbose,"getNews: No News for ",x)
    return(data.table())
  }
  if(nchar(newsagrep)>0) {
      keepitems <-!grepl(newsagrep,news1$title,ignore.case = TRUE) & !grepl(newsagrep,news1$source,ignore.case = TRUE)
      message_if_red(the_av$verbose,"News filtered out ",nrow(news1)-sum(keepitems), " Stories")
      news1 <- news1[keepitems]
  }
  news1[,age:=difftime(Sys.time(),time_published)]
  if("tickerOnly"%in% newsfilter) {
    tickermentions<-lapply(1:nrow(news1), \(i) data.table(news1[i,]$ticker_sentiment[[1]])[,artno:=i])
    tickermentions<-rbindlist(tickermentions)[ticker==x,]
    news1<-news1[tickermentions$artno,]
  }
  news1<-news1[abs(overall_sentiment_score)>=fifelse("useMinSentiment" %in% newsfilter,minabssent,0),]
  news1<-news1[age<=fifelse("maxDays" %in% newsfilter, maxage,+Inf),]
  return(news1[,.(symbol=x,age,time_published,sntmt=overall_sentiment_score,source,title,url)])
}

opt_explation <- function(incode) {
  codeset <- c(s(tolower(incode),sep=","),rep("",5))
  outstr <- stringr::str_squish(paste(
    switch(codeset[[1]], "f"="First","b"="2nd","allMats"),
    switch(codeset[[2]], "m"="Monthly","q"="qtrly","wkly"),
    switch(codeset[[4]], "otm"="OTM","itm"="ITM",""),
    switch(codeset[[3]], "c"="calls","p"="puts","calls+puts"),
    switch(codeset[[5]], "act"="(active)","a"="(active)","")
    ))
  return(outstr)
}


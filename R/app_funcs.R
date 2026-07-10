# -----------------------------------------------------------------------
# FOr the following functions: AV.INV AV.EQINV
# Good
av_inventory <- function(todo,rv) {
  if(grepl("eqinv",todo,ignore.case=TRUE)) {
    outcols <- s("symbol;name;type;currency;end_dt;lastearn_dt;div_lastdt;age;lastpx;lastearn_eps;div_lastval")
    invout <- the_av$pxinv[grepl("ETF|Equity",type),][,age:=Sys.Date()-end_dt][,.SD,.SDcols=outcols]
    gtout <-invout |> gt() |> gt.basetheme(interactive="all") |> add_colwidths("pxinv")
  }
  else {
    invout <- the_av$pxinv[,age:=Sys.Date()-end_dt]
    gtout <- invout |> gt() |>  gt.basetheme(interactive="all") |> add_colwidths("pxinv")
  }
  out<-list("GT1"=gtout ,"DGT1"=the_av$assetgroups |> gt() |> gt.basetheme(interactive="filter"))
  avsh_set_tabtitle("Groups",makefocus=FALSE)
  return(out)
}

# -----------------------------------------------------------------------
# FOr the following functions: AV.H
# Good
av_help <- function(todo,rv) {
  func_reqinput=func_opts=helpstr=helpexample=func_src=NULL
  grepstr <-  s(c(todo,"*")," ")[[2]]
  tortn <- the_av$avsh_funcs[,.(category,runcode,func_reqinput,func_opts,helpstr,helpexample,func_src)][order(category,runcode)][!grepl("tblhelp",category)]
  tortn <- tortn[grepl(grepstr,category,ignore.case=TRUE) | grepl(grepstr,runcode,ignore.case=TRUE)]
  tortn <- tortn |> gt() |> gt.basetheme()
  return(list("GT1"=tortn,"MSG"="More help here...... "))
}
# For the following functions: GP GPI GPD GPI2 GPD2
# Good
#' @importFrom stringr str_detect
av_gp <- function(todo,rv) {
  todolist <- c(s(toupper(todo)," ")," ")
  func_details <- the_av$avsh_funcs[runcode==todolist[[1]],]
  wherefrom <- 1 # Still keep open possibility of line 2 direct
  inpstr <- rv[[paste0("istr",wherefrom)]]
  wheretoput <- fifelse(stringr::str_detect(todolist[[1]],"2$"), "TS2" , "TS1")
  rb <- find_rebasecode(todolist,rv$dtstr_hist)
  toplot <- data_from_list(s(inpstr),rv$dtstr_hist,rb$rebase,rb$rebase_window,msg_inputID=paste0("istr",wherefrom))
  out=list()
  if( nrow(toplot[[1]])>0) {
    out[[wheretoput]] <- one_px_ts(toplot,rv,events=rv$ts_events,dt_window=rb$rebase_window,title=rb$grtitle)
  }
  return(out)
}

# For the following functions: SCAT SCATI SCATD
# Good
av_scat <- function(todo,rv) {
  x_close=y_close=NULL
  todolist <- c(s(toupper(todo)," ")," ")
  func_details <- the_av$avsh_funcs[runcode==todolist[[1]],]
  rb <- find_rebasecode(todolist[[1]],rv$dtstr_hist)
  toplot1<-data_from_list(s(rv$istr1),rv$dtstr_hist,rb$rebase,rb$rebase_window,msg_inputID="istr1",copytable=FALSE)
  toplot2<-data_from_list(s(rv$istr2),rv$dtstr_hist,rb$rebase,rb$rebase_window,copytable=FALSE)
  tp1 <- toplot1[[1]][,.(symbol,timestamp,x_close=adjusted_close)]
  tp2 <- toplot2[[1]][symbol==first(symbol),.(timestamp,y_close=adjusted_close)]
  combdta <- tp1[tp2, on=.(timestamp)][,let(x_close=nafill(x_close,type="locf"),y_close=nafill(y_close,type="locf")), by=.(symbol)]
  if(grepl("I$",todolist[[1]], ignore.case=TRUE)) {
    combdta<-combdta[,let(x_close=100*x_close/first(x_close), y_close=100*y_close/first(y_close)), by=.(symbol)]
  }
  #etnafill(combdta,type="locf",cols=s("x_close;y_close"),by=.(symbol))
  combdta <- combdta[,let(x_logrtn=c(0,diff(log(x_close),1)),y_logrtn=c(0,diff(log(y_close),1))),by=.(symbol)]
  outscat1<- fg_scatplot(combdta,"y_close ~ x_close + color:symbol + doi:recent + point:label",
                         type="lmnoeqn",tsize=5,axislabels=paste0("PX ",s(rv$istr2)[[1]],";PX (Line 1)"),
                         title="Px vs Px")
  outscat2<- fg_scatplot(combdta,"y_logrtn ~ x_logrtn + color:symbol + doi:recent + point:label",
                         type="lm",tsize=5,axislabels=paste0("rtn ",s(rv$istr2)[[1]],";rtn (Line 1)"),
                         title="rtn vs rtn")
  out=list("DSCAT1"=patchwork::wrap_plots( outscat1,outscat2,ncol=2))
  avsh_set_tabtitle("Scatter")
  return(out)
}

# For the following functions: GV
# Good
av_vol <-function(todo,rv) {
  out<- list()
  todolist <- c(s(toupper(todo)," ")," ")
  func_details <- the_av$avsh_funcs[runcode==todolist[[1]],]
  rb <- find_rebasecode(gsub("GV","GP",todolist[[1]]),rv$dtstr_hist)
  toplot<-data_from_list(s(rv$istr1),rv$dtstr_hist,rb$rebase,rb$rebase_window,msg_inputID="istr1",copytable=FALSE)
  if( nrow(toplot[[1]])>0) {
    toplot2 <- ts_vol(toplot,rv$ts_volparams);
    avsh_clipboard(toplot2,"HistVol")
    out[["TS1"]] <- one_px_ts(toplot2,rv,title=paste("Volatility (pct) using ",rv$ts_volparams),events=rv$ts_events,dt_window=rb$rebase_window)
    out[["TS2"]] <- one_px_ts(toplot,rv,events=rv$ts_events,dt_window=rb$rebase_window)
  }
  return(out)
}

# For the following functions: AV.LIVE Q
# Good
av_livepx <- function(todo,rv) {
  inlist=NULL
  todolist <- s(toupper(todo)," ")
  assetlist <- s(rv$istr1)
  df_live <- data.table()
  tmp_syms  <-symbol_grep_by_type(NULL,"Equity|ETF")
  fxsymbols <-symbol_grep_by_type(NULL,"FX")
  if(tmp_syms[[1]]=="NOPXINV") {
      quick_message("istr1","Run some Price History first..")
      return()
  }
  if( todolist[[1]]=="Q") {
    tmp_syms <- intersect(tmp_syms,assetlist)
    fxsymbols <- intersect(fxsymbols,assetlist)
  }
  if( length(tmp_syms)>0) {
    df_live <- av_get_pf(tmp_syms,"REALTIME_BULK_QUOTES",melted=FALSE)
    df_live <- data.table(symbol=assetlist)[,inlist:=TRUE][df_live,on=.(symbol)][order(change_percent)]
  }
  if( length( fxsymbols)>0 ) {
    required_numcols <- s("previous_close;change;change_percent;extended_hours_quote;extended_hours_change;extended_hours_change_percent")
    df_live_fx <- lapply(fxsymbols, \(x) av_get_pf(x,"CURRENCY_EXCHANGE_RATE",melted=FALSE) |> av_extract_fx(cols="symbol;timestamp;close") )
    df_live_fx <- rbindlist(df_live_fx)[,let(open=close,low=close,high=close,volume=NA_integer_)]
    df_live_fx[, (required_numcols):=NA_real_]
    df_live <- rbindlist(list(df_live,df_live_fx),use.names=TRUE,fill=TRUE)
    }
  avsh_clipboard(df_live,"liveprice")
  if(nrow(df_live)<=0) {
    quick_message("istr1","Need to make sure all tickers are in inventory by having history retrieved")
    return()
  }
  return(list(df_live |>  gt.avtheme(themeset="live")))
}

# For the following functions: DES
# good
av_des <- function(todo,rv) {
  imp=NULL
  eqlist1 <- s(rv$istr1)
  out<-list()
  #toplot<-data_from_list(eqlist1,dtstr_hist,ts_rebase,dtstr_window,msg_inputID="istr1") # Just to get the asset type.
  #tickerset = the_av$pxinv[data.table(symbol=eqlist1),on=.(symbol)][,.(symbol,type,currency)]
  if( length(eqset <- symbol_grep_by_type(eqlist1,"Equity"))>0 ) {
    eqdt <- rbindlist(lapply(eqset, \(x) av_get_pf(x,"OVERVIEW")))
    eqdt <- eqdt |> save_av_data("OVERVIEW")
    olist <- avsd$overviewlist[,variable:=EquityName][]
    eqdta <- olist[eqdt,on=.(variable)][source=="av",]
    eqdta <- eqdta[order(catprio,prio)][,.(category,symbol,catprio,prio,variable,ltype,value_str,format ,value_num)]
    toplot <- dcast(eqdta[order(catprio,prio)], catprio+prio+category + variable+format ~ symbol, value.var="value_str")
    toplot <- toplot[,imp:=fifelse(grepl("green|yellow|bold",format),"imp","")]
    setcolorder(toplot,"imp",after="category")
    out[["GT1"]] <-  toplot |> gt.avtheme(themeset="eqdesc1")
  }
  # tab_style(eval(parse(text=fm31)),eval(parse(text=fm32)))
  if( length(eqset <- symbol_grep_by_type(eqlist1,"ETF"))>0 ) {
    eqdt <- rbindlist(lapply(eqset, \(x) av_get_pf(x,"ETF_PROFILE")))
    eqdt <- eqdt |> save_av_data("ETF_PROFILE")
    olist <- avsd$overviewlist[,variable:=ETFName][]
    eqdta <- olist[eqdt,on=.(variable)][source=="av",]
    toplot <- dcast(eqdta[order(catprio,prio)], catprio+prio+category + variable+format ~ symbol, value.var="value_str")
    sectorset <- eqdt |> av_extract_df("sectors")
    if("weight" %in% colnames(sectorset)) {
      sectorset <- dcast(sectorset[!is.na(sector),][,let(weight=100*weight)], sector ~ symbol,value.var="weight")
      sectorset <- sectorset[,let(category="sects",catprio=max(toplot$catprio)+1,prio=.I,format="")]
      setnames(sectorset,"sector","variable")
      toplot <- rbindlist(list(toplot, sectorset),use.names=TRUE,fill=TRUE)
    }
    out[["DGT1"]] <-  toplot |> gt.avtheme(themeset="eqdescsec")
    holdset <- eqdt |> av_extract_df("holdings")
    if("weight" %in% colnames(holdset)) {
      holdset <- holdset[,.SD[order(-weight)][,let(n=.I-min(.I)+1, weight=100*weight)], by=.(symbol)]
      holdset <- dcast(holdset[n<=50,],n ~ symbol,value.var=c("description","weight"))
      holdsetcn <- data.table(nm=colnames(holdset)[-1])[,let(i=.I+1,symbol=s(nm,"_")[2]),by=.I][order(symbol,nm)]
      setcolorder(holdset, c(1,holdsetcn$i))
      out[["DGT2"]] <- holdset |> gt.avtheme(themeset="etfholdings")
    }
    avsh_set_tabtitle("ETF",makefocus=FALSE)
  }
  return(out)
}

# For the following functions: RV RVI
# Good
av_active <- function(todo,rv) {
  todolist <- c(s(toupper(todo)," ")," ")
  rb <- find_rebasecode(gsub("RV","GP",todolist[[1]]),rv$dtstr_hist)
  eqlist1 <- s(rv$istr1)
  eqlist2 <- s(rv$istr2)
  out<- list()
  is_in_list <- eqlist2[1] %in% the_av$pxinv$symbol
  shinyFeedback::feedbackDanger("istr2", !is_in_list, "2. Need a previously downloaded hedge/index")
  req(is_in_list, cancelOutput = TRUE)
  toplot<-data_from_list(c(s(rv$istr),s(rv$istr2)),rv$dtstr_hist,rb$rebase,rb$rebase_window,msg_inputID="istr1")
  if( nrow(toplot[[1]])>0) {
    t_toget <- data.table(symbol=c(eqlist2[1],eqlist1),catg=c("idx",rep("act",length(eqlist1))))
    t_toget <- t_toget[,.SD[1],by=.(symbol)] # Weed out duplicates
    toplot <- the_av$pxd[t_toget,on=.(symbol)]  |> narrowbydtstr(rv$dtstr_hist)
    toplot <- toplot[,.(timestamp,adjusted_close,cumrtn=log(adjusted_close)-log(first(adjusted_close))),by=.(catg,symbol)]
    toplot <- toplot[,let(rtn=c(NA_real_,diff(cumrtn,1))), by=.(catg,symbol)]
    toplot_idx <- toplot[catg=="idx",.(timestamp,idxpx=adjusted_close,mktrtn=rtn,cummktrtn=cumrtn)]
    toplot_idx <- toplot_idx[toplot[catg=="act",],on=.(timestamp)]
    toplot_tridx <- toplot_idx[,.(timestamp,variable=symbol,value=100*exp(cumrtn-cummktrtn))]
    avsh_clipboard(toplot_tridx,todo)
    rv$gropts <- setdiff(rv$gropts,"splitts")  # Takee out splitts
    out[["TS1"]] <-  one_px_ts(toplot_tridx,rv,title=paste0("Excess Returns over ",eqlist2[1]),extra_anno="hline,100",
                               events=rv$ts_events,dt_window=rb$rebase_window)
    toplot_idx <- toplot_idx[,let(rtn=100*rtn,mktrtn=100*mktrtn)][!is.na(mktrtn)]
    volp_n <- as.integer(s(rv$ts_volparams)[[2]])
    toplot_corr <- toplot_idx[,rcor:=frollapply(.SD,volp_n,\(x) 100*cor(x$mktrtn,x$rtn,method="kendall",
                                                                        use="complete.obs"),by.column=FALSE), by=.(symbol)]
    out[["TS2"]] <- one_px_ts(toplot_corr[,.(timestamp,variable=symbol,value=rcor)],rv,
                              title=paste0("Rolling ",volp_n," day kendall correlation"),extra_anno="hline,100",
                              events=rv$ts_events,dt_window=rb$rebase_window)
    ffor = "y~x+0"
    if("tailhedge" %in% rv$scatopts) {
      knots <- round(quantile(toplot_idx[symbol==first(symbol),]$mktrtn,c(0.2,0.8),na.rm=T),2)
      ffor  <- paste0("y~splines::bs(x,knots=c(",paste(knots,collapse=","),"),degree=1)+0")
      message_if_red(TRUE,"ActiveTS: Using Splineset: ",ffor)
    }
    rtnscatall <- fg_scatplot(toplot_idx,"rtn ~ mktrtn + color:symbol +  point:label", "lm",datecuts=c(7),
                              tformula=formula(ffor),n_hex_switch=260*4,
                              title=paste0("Asset Daily returns vs ",eqlist2[1], "Daily rtn"),
                              subtitle="Assumes zero intercept",
                              axislabels=paste0("Asset TR;",eqlist2[1]," TR"),returnregresults=TRUE)
    out[["GT1"]]<- rtnscatall[[2]] |> gt.avtheme(themeset="activeregression",eqlist2[1], rv$sigpct)
    toplot_idx <- toplot_idx[,let(rtnidx=100*exp(cumrtn), mktrtnidx=100*exp(cummktrtn))]
    o2 <- fg_scatplot(toplot_idx,"rtnidx ~ mktrtnidx + color:symbol + point:label", "lm",datecuts=c(7),
                      title=paste0("TR Level vs Level ",rv$dtstr_hist),axislabels="Asset TR Index;Index TR Index")
    out[["SCAT1"]] <- patchwork::wrap_plots( rtnscatall[[1]],o2,ncol=2)
  }
  return(out)
}

# For the following functions: DIV
# Good
av_divs <- function(todo,rv) {
  out=list()
  if( length(eqset <- symbol_grep_by_type(s(rv$istr1),"Equity|ETF"))>0 ) {
    alldivs <- rbindlist(lapply(eqset, \(x) oneticker_divs(x,rv$dtstr_hist)),fill=TRUE,use.names=TRUE)
    out<- list(alldivs |> gt.avtheme(themeset="dividends"))
  }
  else {
    quick_message("istr1","No relevant tickers")
  }
  return(out)
}

# For the following functions: DIV
# Good
av_earn <- function(todo,rv) {
  out<-list()
  fwddts <- extenddtstr(rv$dtstr_hist,rtn="list",endchg=2*360)
  if(length(alleqs <- symbol_grep_by_type(s(rv$istr1),"Equity"))>0) {
    allearn <- rbindlist(lapply(alleqs,\(x) oneticker_earns(x,fwddts,rv$dtstr_hist)))
    lastqtr <- max(allearn[symbol==alleqs[[1]] & !is.na(reportedDate)]$fiscalDateEnding)
    lastqtr <- paste0(lubridate::year(lastqtr),"Q",lubridate::quarter(lastqtr))
    avsh_clipboard(allearn,"earnings")
    out[["GT1"]]<- allearn |> gt.avtheme(themeset="earnings")
    if(nrow( xout<-av_get_pf(alleqs[[1]],"EARNINGS_CALL_TRANSCRIPT",quarter=lastqtr) |> av_extract_df("transcript"))>0) {
      xout <- xout[,title:=fcase(grepl("Chief Executive|CEO",title),"CEO",grepl("Chief Financial|CFO",title),"CFO",grepl("Investor Relations",title),"InvRel",default=title)]
      xout <- xout |> gt.avtheme(themeset="earningstranscript",paste0(alleqs[[1]]," ",lastqtr))
      out[["DGT1"]]<- xout
      avsh_set_tabtitle("Transcript",makefocus=FALSE)
    }
  }
  else {
    quick_message("istr1","No relevant tickers")
  }
  return(out)
}

# For the following functions: CN
# Good
av_news <- function(todo,rv) {
  av_set_default_set("news",rv)
  out<-list("NEWSGT"=get_allNews(s(rv$istr1),rv) |> gt.avtheme(themeset="news",rv$istr1))
  av_set_defaults("NEWSGT",out[["NEWSGT"]])
  av_set_defaults("starttab","NEWS")
  return(out)
}


# For the following functions: MOV
# Good
av_movers <- function(todo,rv) {
  toplot=NULL
  out<-list()
  tdta <- av_get_pf("","TOP_GAINERS_LOSERS") |> save_av_data("TOP_GAINERS_LOSERS")
  tab1 <- rbindlist(list(
    av_extract_df(tdta,"top_losers")[,.(symbol=ticker,price,pctchg=as.numeric(gsub("%","",change_percentage)),item="losers",n=.I)],
    av_extract_df(tdta,"top_gainers")[,.(symbol=ticker,price,pctchg=as.numeric(gsub("%","",change_percentage)),item="winners",n=.I)],
    av_extract_df(tdta,"most_actively_traded")[,.(symbol=ticker,price,pctchg=as.numeric(gsub("%","",change_percentage)),item="traded",n=.I)]
  ))
  tab1 <-  the_av$listings[,.(symbol,name)][tab1,on=.(symbol)]
  out <- list(tab1 |> gt.avtheme(themeset="Gen:Movers",tdta[variable=="last_updated",]$value_str) |> cols_move_to_start(s("item;n")))
  avsh_clipboard(toplot,"Movers")
  return(out)
}

# For the following functions: SEARCH
#
av_search <- function(todo,rv) {
  eqdta <- av_get_pf("","SYMBOL_SEARCH",keywords=rv$istr1) |> save_av_data("SYMBOL_SEARCH")
  eqdta[,let(format=fcase(type=="Equity" & region=="United States","bold",default=""))]
  setcolorder(eqdta,neworder="matchScore")
  idxsearch <- the_av$tickerlist[grepl(istr1,name,ignore.case=TRUE) | grepl(istr1,symbol,ignore.case=TRUE),][,.(symbol,name,type="Index")]
  eqdta <- rbindlist(list(idxsearch[,format:="green"],eqdta),fill=TRUE,use.names=TRUE)
  avsh_clipboard(eqdta,"eq search")
  out <- list( eqdta |> gt.avtheme(themeset="namesearch",istr1) )
  return(out)
}

# For the following functions: OS
# GOod
av_optsearch <- function(todo,rv) {
  out<-list()
  av_set_default_set("optsearch",rv)
  allmsg <- ""
  indta <- data.table()
  eqlist1 <- s(rv$istr1)
  ochains <- find_arg(todo,"f") %||% rv$ochains
  mindelta <- find_arg(todo,"d") %||% "0"
  message_if_red(TRUE," av_optsearch ochains:: ",ochains)
  for (x in eqlist1) {
    theseopts <- av_get_pf(x,"HISTORICAL_OPTIONS")
    if("variable" %in% names(theseopts)) {
      allmsg <- paste(allmsg, x) }
    else {
      indta <- rbindlist(list(indta,theseopts), fill=TRUE)
    }
  }
  indta <- indta |> save_av_data("HISTORICAL_OPTIONS")
  if(nrow(indta)>0) {
    inspots <- rbindlist(lapply(eqlist1,\(x) av_get_pf(x,"GLOBAL_QUOTE",melted=TRUE)))
    inspots <- inspots[variable=="price",.(symbol,spot=value_num)]
    indta <- inspots[indta,on=.(symbol)][,ncak:=1]
    filteredopts <- indta |> av_grep_opts(grepstring=ochains,mindelta=as.numeric(mindelta)/100)
    filteredopts <- filteredopts |> av_opt_helper_cols(scaling=rv$oscaling)
    quick_message("istr1",paste(nrow(indta),"rows ", fifelse(nchar(allmsg)>0, paste0(allmsg, " missing"),""), " and narrowing to ",nrow(filteredopts), " using ",ochains))
    colstoshow <- data.table(showset=c("reduced","trading","all"),
                             colstring=c("symbol;ncak;strike;type;daysExp;moneyn;mat_be;mat_bepct;IV;mark;last;bo_pct;delta;vega;theta;contractID",
                                         "symbol;ncak;strike;daysExp;volume;open_interest;IV;delta;last;mark;bo_pct;bid;ask;bid_size_poi;ask_size_poi;contractID",
                                         paste0(names(filteredopts),collapse=";")))
    atmopts = indta[type=="call" & expiration<=Sys.Date()+60,][,.SD[which.min(abs(delta-0.5))],by=.(symbol,expiration)] |> av_opt_helper_cols(scaling="none")
    out[["OPTPLOT1"]] <- fg_scatplot(atmopts,"IV ~ daysExp + color:symbol",type="loessnofill",psize=3,title="ATM Term Structure")
    filteredopts[,type:=fifelse(type=="call","C","P")]
    filteredopts<- filteredopts[,.SD,.SDcols=s(colstoshow[showset==rv$otodisplay,]$colstring)]
    filteredopts<- filteredopts[,symbol:=sprintf("%s %3dd %s",symbol,daysExp,type)]
    avsh_clipboard(filteredopts,"opts")
    out[["OPT1GT"]] <- filteredopts |> gt.avtheme(themeset="filteredopts", rv$istr1, rv$otodisplay)
  }
  else {
    quick_message("istr1"," ... No options found")
  }
  return(out)
}






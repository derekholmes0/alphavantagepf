# -----------------------------------------------------------------------
# FOr the following functions: AV.INV AV.EQINV
#
av_inventory <- function(todo,rv) {
  ntickers=todoargs=NULL
  if(nrow(the_av$pxd)<=0) {
    quick_message("istr1","No Inventory, Run a GP graph to start",color="red")
    return()
  }
  grepstr <-  s(c(todo,"*"),"[ ]+",rtn=2)
  if(grepl("eqinv",todo,ignore.case=TRUE)) {
    outcols <- s("symbol;name;type;currency;end_dt;lastearn_dt;div_lastdt;age;lastpx;lastearn_eps;div_lastval")
    invout <- the_av$pxinv[grepl("ETF|Equity",type),][,age:=Sys.Date()-end_dt][,.SD,.SDcols=outcols]
    invout <- invout[grepl(grepstr,name),]
    gtout <-invout |> gt() |> gt.basetheme(interactive="all") |> add_colwidths("pxinv")
  }
  else if(grepl("tickers",todo,ignore.case=TRUE)) {
    toprint <- data.table(type=s("Crypto;Index"),prio=c(2,1))[the_av$tickerlist,on=.(type)]
    toprint <- toprint[,symbol:=s(symbol,"/")[[1]], by=.I][,ntickers:=.N,by=.(symbol)]
    toprint <- toprint[,.SD[1][,name:=paste0(fifelse(ntickers>1,"e.g.",""),name)],by=.(symbol)]
    toprint <- toprint[grepl(grepstr,name),]
    gtout <- toprint[order(prio,symbol)] |> gt() |>  gt.basetheme(interactive="all",sizepct=85) |> cols_hide(columns=c(prio)) |>
        cols_move_to_start(columns=c(symbol,name)) |>
                  fmt_number(columns=c(ntickers),decimals=0) |> add_colwidths("tickers")

  }
  else {
    outcols <- s("symbol;name;type;currency;lastpx;end_dt;beg_dt;list_ts")
    invout <- the_av$pxinv[,.SD,.SDcols=outcols][,age:=Sys.Date()-end_dt]
    invout <- invout[grepl(grepstr,name),]
    gtout <- invout |>  gt.avtheme(themeset="pxinv",sizepct=90)

  }
  out<-list("GT1"=gtout ,"DGT1"=the_av$assetgroups |> gt() |> gt.basetheme(interactive="filter"))
  avsh_set_tabtitle(newtext="Groups",makefocus=FALSE)
  return(out)
}

# -----------------------------------------------------------------------
# FOr the following functions: AV.H
#
av_help <- function(todo,rv) {
  func_reqinput=func_opts=helpstr=helpexample=func_src=HelpComment=runcode2=NULL
  grepstr <-  s(c(todo,"*"),"[ ]+",rtn=2)
  tortn <- the_av$avsh_funcs[,.(category,runcode,func_reqinput,func_opts,helpstr,helpexample,func_src)][order(category,runcode)][!grepl("tblhelp",category)]
  tortn <- tortn[grepl(grepstr,category,ignore.case=TRUE) | grepl(grepstr,runcode,ignore.case=TRUE)]
  tortn <- tortn[!grepl("tblhlp",category)]
  avail_on_2 <- tortn[grepl("2$",runcode ),.(runcode2=paste0(" (",runcode,")") ,runcode =gsub("2$","",runcode))]
  tortn <- avail_on_2[tortn,on=.(runcode)][!grepl("2$",runcode)][, runcode:=paste0(runcode,fcoalesce(runcode2,"")), by=.I]
  # does this take too long?
  thistm <- system.time({
    rtnlist <- list(
      tortn |> gt(groupname_col="category",row_group_as_column = TRUE) |> gt.basetheme(interactive="filter") |> cols_move_to_start(columns=c(runcode,helpstr)) |>
                    cols_merge(columns=c(func_reqinput,func_opts), pattern = "{1}, {2}") |> cols_hide(columns=c(runcode2)) |>
                  add_colwidths("avh")
      )
  })["elapsed"]
  if(grepl("showGeneralHelp",the_av$logopts)) {
    helptable <- avsd$generalhelp |> gt() |> gt.basetheme(sizepct=90) |> decorate_table() |>
                  tab_style(style=cell_text(font="Courier"),locations=cells_body(columns=c("Example/Choice"))) |>
                  fmt_url(columns=HelpComment,rows=grepl("http",HelpComment),label="FinanceGraphs Parameters and Events",color="blue")
    rtnlist=c(list(helptable),rtnlist)
  }
  return(rtnlist)
}

# -----------------------------------------------------------------------
# FOr the following functions: AV.INPUTS
#
av_dumprv <- function(todo,rv) {
  inputId<-NULL
  tnames <- names(rv)
  tortn <- data.table(inputId=tnames, type=sapply(tnames, \(x) paste0(class(rv[[x]]))), current_value=rv[tnames])
  tortn[inputId=="avapikey",]$current_value<-"<<redacted>>"
  tortn<- tortn[!grepl("shinyAction",type),]
  return(list(tortn |> gt() |> gt.basetheme(sizepct=70)))
}

# -----------------------------------------------------------------------
# FOr the following functions: av.hist
#
#' @importFrom utils tail
av_misc <- function(todo,rv) {
  ts=N=NULL
  todolist <- c(s(toupper(todo),"[ ]+"),"1")
  cmdhistlist <- tail(the_av$cmdhist,40)[order(-ts)][,let(N=.I)]
  setcolorder(cmdhistlist,s("N;cmd;ts"))
  if(grepl("^RM",todo,ignore.case=TRUE)) {
    ts <- lapply(s(rv$assetline), \(x) kill_symbol(x))
    message_if_red(TRUE,"Removed tickers ",rv$assetline," from price history")
    return( list() )
  }
  if(grepl("AV.HIST",todo,ignore.case=TRUE)) {
    tortn <- cmdhistlist |> gt() |> gt.basetheme(sizepct=80) |>
        fmt_datetime(columns=c(ts),date_style="Md",time_style="iso-short") |>
        fmt_number(columns=c(N),decimals=0)
    return(list(tortn))
  }
  if(grepl("AV.R",todo)) {
    ntoget <- as.numeric(todolist[[2]])
    return(list("CMD"=paste0("toinput:",cmdhistlist[N==ntoget,]$cmd)))
  }
  if(grepl("AV.CLS",todo)) {
    ntoget <- as.numeric(todolist[[2]])
    return(list("CMD"="clear:"))
  }
}

# For the following functions: GP GPI GPD GPI2 GPD2
# Good
#' @importFrom stringr str_detect
av_gp <- function(todo,rv) {
  todolist <- s(toupper(todo),"[ ]+",pad=1)
  func_details <- the_av$avsh_funcs[runcode==todolist[[1]],]
  wheretoput <- fifelse(stringr::str_detect(todolist[[1]],"2$"), "TS2" , "TS1")
  rb <- find_rebasecode(todolist,rv$dtstr_hist)
  toplot <- data_from_list(s(rv$assetline),rv$dtstr_hist,rb$rebase,rb$rebase_window,msg_inputID="istr1")
  # Adjust if returns
  if(grepl("LR",todolist[1])) {
    #message_if_green(the_av$verbose,"PLotting Log Returns")
    tseriesnm <- the_av$seriesnm
    toplot[[1]] <- toplot[[1]][,(tseriesnm):=10000*c(NA_real_,diff(log(get(tseriesnm)),1)), by=.(symbol)]
  }
  out=list()
  if( nrow(toplot[[1]])>0) {
    out[[wheretoput]] <- one_px_ts(toplot,rv,events=rv$ts_events,dt_window=rb$rebase_window,title=rb$grtitle)
  }
  return(out)
}


# For the following functions: GP GPI GPD GPI2 GPD2
# Good
#' @importFrom stringr str_detect
av_gearn <- function(todo,rv) {
  horizon=i.enddt=labs=lpx=estimatedEPS=ra_estimatedEPS=ra_reportedEPS=reportedEPS=eps_estimate_average=c_code=NULL
  todolist <- s(toupper(todo),"[ ]+",pad=1)
  func_details <- the_av$avsh_funcs[runcode==todolist[[1]],]
  eqset <- form_symset(s(rv$assetline),typegrep="Equity")$symbol
  if(length(eqset)<=0) { quick_message("No equities in set"); return() }
  wheretoput <- fifelse(stringr::str_detect(todolist[[1]],"2$"), "TS2" , "TS1")
  calccode <- toupper(substr(todolist[[1]],1,3))
  bigdtstr <- extenddtstr(rv$dtstr_hist,begchg=-365)
  toplot <- data_from_list(eqset,bigdtstr ,"none",bigdtstr ,msg_inputID="istr1")
  tdtmap <- narrowbydtstr(dtmap[,.(timestamp=DT_ENTRY,isday)],bigdtstr)
  earnset <- the_av$earn[data.table(symbol=eqset),on=.(symbol)][,.(symbol,timestamp=reportedDate,reportedEPS,estimatedEPS)]
  # Use next estimate for dates between last estimate and now
  next_earnfwd <- the_av$earnest[data.table(symbol=eqset,horizon="fiscal quarter"),on=.(symbol,horizon)][date>=Sys.Date(),.SD[1],by=.(symbol)]
  next_earnfwd <- next_earnfwd[,.(symbol,timestamp=max(toplot[[1]]$timestamp),reportedEPS=eps_estimate_average,estimatedEPS=eps_estimate_average)]
  earnset <- rbindlist(list(earnset,next_earnfwd))[order(symbol,timestamp)]
  withearn <- earnset[toplot[[1]][,.(symbol,timestamp,close=get(the_av$seriesnm))],on=.(symbol,timestamp)][,
                      let(reportedEPS=nafill(reportedEPS,type="nocb"), estimatedEPS=nafill(estimatedEPS,type="nocb")), by=.(symbol)]
  withearn <- withearn[,let(ra_reportedEPS=4*frollmean(reportedEPS,252,na.rm=T,align="right"),
                             ra_estimatedEPS=4*frollmean(estimatedEPS,252,na.rm=T,align="right"),
                            c_code=calccode
                             ),by=.(symbol)]
  toplot_x <- withearn[,.(timestamp,symbol,value=fcase(c_code=="GPE",close/ra_reportedEPS, c_code=="GEP",100*ra_reportedEPS/close,
                                      c_code=="GPF",close/ra_estimatedEPS, c_code=="GFP",100*ra_estimatedEPS/close))]
  toplot_x <- toplot_x |> narrowbydtstr(rv$dtstr_hist)
  titlelist <- list("GPE"="Price/Earnings","GEP"="100*Earnings/Price","GPF"="Px/FcstEarnings","GFP"="100*FctEarn/Px")
  out=list()
  if( nrow(toplot_x)>0) {
    toplot[[1]] <- setnames(toplot_x,"value",the_av$seriesnm)
    out[[wheretoput]] <- one_px_ts(toplot,rv,events=rv$ts_events,dt_window=rv$dtstr_hist,title=titlelist[[calccode]])
  }
  return(out)
}

# For the following functions: GP GPI GPD GPI2 GPD2
# todo="GPEE"; rv<-list(istr1="IBM;GS",dtstr_hist="-2y::")
#' @importFrom stringr str_detect
#' @importFrom FinanceGraphs fg_current_theme
#' @importFrom ggplot2 ggplot aes geom_errorbar geom_line geom_segment scale_color_manual geom_crossbar labs theme
#' @importFrom ggplot2 geom_vline
av_earnest <- function(todo,rv) {
  date1=date2=eps_est=eps_est.hi=eps_est.lo=eps_est_30d=eps_est_90d=eps_estimate_analyst_count=ts=horizon=NULL
  eps_estimate_revision_down_trailing_30_days=eps_estimate_revision_up_trailing_30_days=epse1=epse2=estimatedEPS=NULL
  todolist <- c(s(toupper(todo),"[ ]+"),"invalidate")
  this_dthist <- rv$dtstr_hist
  begdt <- gendtstr(rv$dtstr_hist,rtn="list")[[1]]
  if(!grepl("NA",gendtstr(todolist[[2]]))) { this_dtstr <- gendtstr(todolist[[2]]) }
  earnset <- data.table(symbol=s(rv$assetline))[,horizon:="fiscal quarter"]
  earnset <- the_av$earnest[earnset,on=.(symbol,horizon)][,.SD[ts==max(ts)], by=.(symbol)][date>=begdt,]
  earnset <- earnset[,let(analystdisp30d=100*(fcoalesce(eps_estimate_revision_up_trailing_30_days,0)-fcoalesce(eps_estimate_revision_down_trailing_30_days,0))
                          /eps_estimate_analyst_count)]
  colstokeep <- s("eps_est;eps_est.lo;eps_est.hi;eps_est_30d;eps_est_90d")
  setnames(earnset,s("symbol;eps_estimate_average;eps_estimate_low;eps_estimate_high;eps_estimate_average_30_days_ago;eps_estimate_average_90_days_ago"),
                   c("variable",colstokeep))
  toplot <- earnset[,.SD,.SDcols=c("date","variable",colstokeep)]
  toplot2a <- toplot[,.(variable,date1=date-30,date2=date,epse1=eps_est_30d,epse2=eps_est )]
  toplot2b <- toplot[,.(variable,date1=date-90,date2=date-30,epse1=eps_est_90d,epse2=eps_est_30d )]
  # Sometimes just do it the old fashioned way
  g1 <- ggplot2::ggplot(toplot,aes(x=date,color=variable))+geom_line(aes(y=eps_est),linewidth=2)
  g1 <- g1+ geom_crossbar(aes(y=eps_est,ymin=eps_est.lo,ymax=eps_est.hi),width=10)
  g1 <- g1 + labs(x="Date",y="EPS",title="Earnings Estimates",caption="Red Line: Today")
  g1 <- g1 + scale_color_manual(values=fg_get_aesstring("lines")) + fg_current_theme() + theme(legend.position = "top",legend.justification = "left")
  g1 <- g1 + geom_segment(aes(x=date1,y=epse1,xend=date2,yend=epse2,color=variable),linewidth=1,data=toplot2a)
  g1 <- g1 + geom_segment(aes(x=date1,y=epse1,xend=date2,yend=epse2,color=variable),linewidth=1,data=toplot2b)
  g1 <- g1 + ggplot2::geom_vline(xintercept=Sys.Date(), color="red")
  return(list(g1))
}

# For the following functions: SCAT SCATI SCATD
# Good
av_scat <- function(todo,rv) {
  x_close=y_close=NULL
  todolist <- s(toupper(todo),"[ ]+",pad=1)
  func_details <- the_av$avsh_funcs[runcode==todolist[[1]],]
  rb <- find_rebasecode(todolist[[1]],rv$dtstr_hist)
  toplot1<-data_from_list(s(rv$assetline),rv$dtstr_hist,rb$rebase,rb$rebase_window,msg_inputID="istr1",copytable=FALSE)
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
  x=NULL
  out<- list()
  todolist <- s(toupper(todo),"[ ]+",pad=1)
  func_details <- the_av$avsh_funcs[runcode==todolist[[1]],]
  rb <- find_rebasecode(gsub("GV","GP",todolist[[1]]),rv$dtstr_hist)
  toplot<-data_from_list(s(rv$assetline),rv$dtstr_hist,rb$rebase,rb$rebase_window,msg_inputID="istr1",copytable=FALSE)
  if( nrow(toplot[[1]])>0) {
    volp <- s(rv$ts_volparams)
    one_ts_vol <- function(sb) {
        tdta <- toplot[[1]][symbol==sb,]
        xdta <- tdta[,lapply(.SD,\(x) x+(get(the_av$seriesnm)-close)), .SDcols=s("open;high;low;close")]
        xdta <- tdta[,lapply(.SD,\(x) fcoalesce(x,close)),  .SDcols=s("open;high;low;close")]
        setnafill(xdta,"locf")
        data.table(timestamp=tdta$timestamp,variable=x,value=100*TTR::volatility(xdta, calc=volp[[1]],n=as.integer(volp[[2]]), N=as.integer(volp[[3]])))
    }
    toplot2 <- rbindlist(lapply(unique(toplot[[1]]$symbol), one_ts_vol))
    avsh_clipboard(toplot2,"HistVol")
    out[["TS1"]] <- one_px_ts(toplot2,rv,title=paste("Volatility (pct) using ",rv$ts_volparams),events=rv$ts_events,dt_window=rb$rebase_window)
    toplot[[2]]<-"start"
    out[["TS2"]] <- one_px_ts(toplot,rv,events=rv$ts_events,dt_window=rb$rebase_window)
  }
  return(out)
}

# For the following functions: AV.LIVE Q
# Good
av_livepx <- function(todo,rv) {
  inlist=NULL
  todolist <- s(toupper(todo),"[ ]+")
  assetlist <- s(rv$assetline)
  df_live <- data.table()
  if(nrow(the_av$pxinv)<=0) {
    quick_message("No inventory to price")
    return()
  }
  tmp_syms  <- the_av$pxinv[grepl("Equity|ETF",type),]$symbol
  fxsymbols <- the_av$pxinv[grepl("FX",type),]$symbol
  if(tmp_syms[[1]]=="NOPXINV") {
      quick_message("Run some Price History first..")
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
    quick_message("Need to make sure all tickers are in inventory by having history retrieved")
    return()
  }
  return(list(df_live |>  gt.avtheme(themeset="live")))
}

# For the following functions: DES
# good
av_des <- function(todo,rv) {
  imp=NULL
  out<-list()
  this_symset <- form_symset(s(rv$assetline))
  if( length(eqset <- this_symset[type=="Equity",]$symbol)>0 ) {
    eqdt <- rbindlist(lapply(eqset, \(x) av_get_pf(x,"OVERVIEW")))  |> setnames("field","variable",skip_absent=TRUE)
    eqdt <- eqdt |> save_av_data("OVERVIEW")
    olist <- avsd$overviewlist[,variable:=EquityName][]
    eqdta <- olist[eqdt,on=.(variable)][source=="av",]
    eqdta <- eqdta[order(catprio,prio)][,.(category,symbol,catprio,prio,variable,value,format,value_num)]
    eqdta_2 <- copy(eqdta)[variable=="Description",value:="See Below"]
    toplot <- dcast(eqdta_2[order(catprio,prio)], catprio+prio+category + variable+format ~ symbol, value.var="value")
    toplot <- toplot[,imp:=fifelse(grepl("green|yellow|bold",format),"imp","")]
    setcolorder(toplot,"imp",after="category")
    out[["GT1"]] <-  toplot |> gt.avtheme(themeset="eqdesc1")
    out[["GT2"]] <-  eqdta[variable=="Description",.(symbol,desc=value)] |> gt() |> gt.basetheme(sizepct=70)
  }
  # tab_style(eval(parse(text=fm31)),eval(parse(text=fm32)))
  if( length(eqset <- this_symset[type=="ETF",]$symbol)>0 ) {
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
  artn=NULL
  todolist <- s(toupper(todo),"[ ]+",pad=1)
  rb <- find_rebasecode(gsub("RV","GP",todolist[[1]]),rv$dtstr_hist)
  combassetlist <- c(s(rv$istr2)[1],s(rv$assetline))
  out<- list()
  is_in_list <- combassetlist[1] %in% the_av$pxinv$symbol
  shinyFeedback::feedbackDanger("istr2", !is_in_list, "2. Need a previously downloaded hedge/index")
  req(is_in_list, cancelOutput = TRUE)
  toplot<-data_from_list(c(s(rv$assetline),s(rv$istr2)),rv$dtstr_hist,rb$rebase,rb$rebase_window,msg_inputID="istr1")
  if( nrow(toplot[[1]])>0) {
    t_toget <- data.table(symbol=combassetlist,catg=c("idx",rep("act",length(combassetlist)-1)))
    t_toget <- t_toget[,.SD[1],by=.(symbol)] # Weed out duplicates
    toplot <- the_av$pxd[t_toget,on=.(symbol)]  |> narrowbydtstr(rv$dtstr_hist)
    toplot <- toplot[,.(timestamp,adjusted_close,cumrtn=log(adjusted_close)-log(first(adjusted_close))),by=.(catg,symbol)]
    toplot <- toplot[,let(rtn=c(NA_real_,diff(cumrtn,1))), by=.(catg,symbol)]
    toplot_idx <- toplot[catg=="idx",.(timestamp,idxpx=adjusted_close,mktrtn=rtn,cummktrtn=cumrtn)]
    toplot_idx <- toplot_idx[toplot[catg=="act",],on=.(timestamp)]
    toplot_tridx <- toplot_idx[,.(timestamp,variable=symbol,value=100*exp(cumrtn-cummktrtn))]
    avsh_clipboard(toplot_tridx,todo)
    rv$gropts <- setdiff(rv$gropts,"splitts")  # Takee out splitts
    out[["TS1"]] <-  one_px_ts(toplot_tridx,rv,title=paste0("Excess Returns over ",combassetlist[1]),extra_anno="hline,100",
                               events=rv$ts_events,dt_window=rb$rebase_window)
    toplot_idx <- toplot_idx[,let(rtn=100*rtn,mktrtn=100*mktrtn)][!is.na(mktrtn)]
    volp_n <- as.integer(s(rv$ts_volparams)[[2]])
    toplot_corr <- toplot_idx[,rcor:=frollapply(.SD,volp_n,\(x) 100*cor(x$mktrtn,x$rtn,method="kendall",
                                                                        use="complete.obs"),by.column=FALSE), by=.(symbol)]
    out[["TS2"]] <- one_px_ts(toplot_corr[,.(timestamp,variable=symbol,value=rcor)],rv,
                              title=paste0("Rolling ",volp_n," day correlation"),extra_anno="hline,100",
                              events=rv$ts_events,dt_window=rb$rebase_window)
    ffor = "y~x+0"
    if("tailhedge" %in% rv$scatopts) {
      knots <- round(quantile(toplot_idx[symbol==first(symbol),]$mktrtn,c(0.2,0.8),na.rm=T),2)
      ffor  <- paste0("y~splines::bs(x,knots=c(",paste(knots,collapse=","),"),degree=1)+0")
      message_if_red(TRUE,"ActiveTS: Using Splineset: ",ffor)
    }
    rtnscatall <- fg_scatplot(toplot_idx,"rtn ~ mktrtn + color:symbol +  point:label", "lmnoeqn",
                              tformula=formula(ffor),n_hex_switch=260*4,
                              title=paste0("Asset Daily returns vs ",combassetlist[1], "Daily rtn"),
                              subtitle="Assumes zero intercept",n_color_switch=10,
                              axislabels=paste0("Asset TR;",combassetlist[1]," TR"),returnregresults=TRUE)
    out[["GT1"]]<- rtnscatall[[2]] |> gt.avtheme(themeset="activeregression",combassetlist[1], rv$sigpct)
    toplot_idx <- toplot_idx[,let(rtnidx=100*exp(cumrtn), mktrtnidx=100*exp(cummktrtn))]
    o2 <- fg_scatplot(toplot_idx,"rtnidx ~ mktrtnidx + color:symbol + point:label", "lmnoeqn",n_color_switch=10,
                      title=paste0("TR Level vs Level ",rv$dtstr_hist),axislabels="Asset TR Index;Index TR Index")
    out[["SCAT1"]] <- patchwork::wrap_plots( rtnscatall[[1]],o2,ncol=2)

    anames <- s(rv$assetline)
    corr_idx = toplot_idx[,.(value=cor(rtn,mktrtn,method="kendall")), by=.(symbol)][,let(variable="coridx")]
    corr_tr <- dcast(toplot_idx[,artn:=rtn-mktrtn],timestamp ~ symbol, value.var="rtn")
    corr_raw <- data.table(cor(corr_tr[,.SD,.SDcols=!("timestamp")]),keep.rownames=TRUE) |> setnames("rn","symbol")
    corr_tr <- dcast(toplot_idx[,artn:=rtn-mktrtn],timestamp ~ symbol, value.var="artn")
    corr_act <- data.table(cor(corr_tr[,.SD,.SDcols=!("timestamp")]),keep.rownames=TRUE) |> setnames("rn","symbol")
    corr_all <- rbindlist(list(dcast(corr_idx,variable~symbol)[,symbol:="coridx"],corr_raw[,variable:="returns"], corr_act[,variable:="actrtn"]),use.names=TRUE)
    corr_all <- corr_all[,(anames):=lapply(.SD,\(x) 100*fifelse(x>0.99,NA_real_,x)), .SDcols=anames]
    corr_all <- the_av$pxinv[,.(symbol,name)][corr_all,on=.(symbol)]
    out[["DGT1"]]  = corr_all |> gt( groupname_col = "variable",row_group_as_column = TRUE) |> gt.basetheme() |>  cols_move_to_start(s("variable;name;symbol")) |>
      fmt_number(decimals=0) |>
      data_color( columns = where(is.numeric),palette = c("#2166AC", "white", "#B2182B"),domain = c(-1, 1)) |>
      tab_style_body(  fn = is.na,  style = list(cell_fill(color = "black"),cell_text(color="black")))
    avsh_set_tabtitle("Corrs",makefocus=FALSE)
  }
  return(out)
}

# For the following functions: DIV
# Good
av_divs <- function(todo,rv) {
  asset<-NULL
  out=list()
  this_symset <- form_symset(s(rv$assetline))
  if( length(eqset <- this_symset[grepl("Equity|ETF",type),]$symbol)>0 ) {
    alldivs <- rbindlist(lapply(eqset, \(x) oneticker_divs(x,rv$dtstr_hist)),fill=TRUE,use.names=TRUE)
    out<- list(alldivs |> gt.avtheme(themeset="dividends"))
  }
  else {
    quick_message("No relevant tickers")
  }
  return(out)
}

# For the following functions: DIV
# Good
av_earn <- function(todo,rv) {
  out<-list()
  fwddts <- extenddtstr(rv$dtstr_hist,rtn="list",endchg=2*360)
  this_symset <- form_symset(s(rv$assetline))
  if(length(alleqs <- this_symset[type=="Equity",]$symbol)>0) {
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
    quick_message("No relevant tickers")
  }
  return(out)
}

# For the following functions: CN
# Good
av_news <- function(todo,rv) {
  av_set_default_set("news",rv)
  out<-list("NEWSGT"=get_allNews(s(rv$assetline),rv) |> gt.avtheme(themeset="news",rv$istr1))
  av_set_defaults("NEWSGT",out[["NEWSGT"]])
  av_set_defaults("starttab","news")
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
  assetType=exchange=src=matchScore=NULL
  # options: checkav
  # options: type=<greptype>
  src_str <- rv[["assetline"]]
  src_list <- the_av$listings[grepl(src_str,symbol,ignore.case=TRUE) | grepl(src_str,name,ignore.case=TRUE)][,.(src="Listings",symbol,name,assetType,exchange,currency="USD")]
  src_tickerlist <- the_av$tickerlist[grepl(src_str,symbol,ignore.case=TRUE) | grepl(src_str,name,ignore.case=TRUE)][,.(src="IdxCryp",symbol,name,assetType=type,currency="N/A")]
  src_inv <-  the_av$pxinv[grepl(src_str,symbol,ignore.case=TRUE) | grepl(src_str,name,ignore.case=TRUE)][,.(src="Inv",symbol,name,assetType=type,currency)]
  src_results<-rbindlist(list(src_list,src_tickerlist,src_inv),fill=TRUE,use.names=TRUE)
  if(nrow(src_results)<=0 | grepl("checkav",todo,ignore.case=TRUE)) {
    message_if_red(TRUE,"Search for '",src_str,"' found nothing yet, going to Av SYMBOL_SEARCG")
    src_av <- av_get_pf("","SYMBOL_SEARCH",keywords=src_str) |> save_av_data("SYMBOL_SEARCH")
    src_av <- src_av[,.(symbol,name,type,region,currency,matchScore)]
    src_results<-rbindlist(list(src_results,src_results),fill=TRUE,use.names=TRUE)
  }
  subsearches <- c(stringr::str_extract(todo,"TYPE=([A-Za-z]*)",group=1), stringr::str_extract(todo,"NAME=([A-Za-z]*)",group=1))
  if( !is.na(subsearches[[1]]) )  { src_results <- src_results[ grepl(subsearches[[1]],assetType,ignore.case=TRUE),] }
  if( !is.na(subsearches[[2]]) )  { src_results <- src_results[ grepl(subsearches[[2]],name,ignore.case=TRUE),] }
  avsh_clipboard(src_results,"eq search")
  out <- list( src_results  |> gt(groupname_col="src") |> gt.basetheme() )
  return(out)
}

# For the following functions: OS
# GOod
av_optsearch <- function(todo,rv) {
  out<-list()
  av_set_default_set("optsearch",rv)
  allmsg <- ""
  indta <- data.table()
  eqlist1 <- s(rv$assetline)
  ochains <- find_arg(todo,"f") %||% rv$ochains
  mindelta <- find_arg(todo,"d",altno=-1) %||% rv$omindelta
  message_if_red(the_av$verbose," av_optsearch ochains:: ",ochains, " mindelta: ",mindelta)
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
    quick_message(paste(nrow(indta),"rows ", fifelse(nchar(allmsg)>0, paste0(allmsg, " missing"),""), " and narrowing to ",nrow(filteredopts), " using ",ochains))
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
    out[["OPT1GT"]] <- filteredopts |> gt.avtheme(themeset="filteredopts", rv$assetline, rv$otodisplay)
    quick_message("Option set comes from HISTORICAL_OPTIONS, not REALTIME")
  }
  else {
    quick_message(" ... No options found")
  }
  return(out)
}


av_seasonality <- function(todo,rv) {
  todofunc=ex_dividend_date=thiseq=surprise=todoargs=NULL
  this_symset <- form_symset(s(rv$assetline))
  firsteq <- this_symset[grepl("Equity|ETF",type),][,.SD[1]]$symbol
  firstticker <- this_symset[,.SD[1]]$symbol
  events <- data.table()
  seastype <- ""
  out<-list()
  if(todofunc=="SEASDIV" && !is.na(firsteq)) {
    gtitle <- paste0(firsteq," Dividends Seasonality")
    toplot <- data_from_list(firsteq,rv$dtstr_hist ,"none",rv$dtstr_hist,msg_inputID="istr1")[[1]][,.(timestamp,close=get(the_av$seriesnm))]
    events <- oneticker_divs(firsteq,rv$dtstr_hist)[,.(date=ex_dividend_date ,label=format(ex_dividend_date ,"%y%m") )]
  }
  else if(todofunc=="SEASEA" && !is.na(firsteq)) {
    gtitle <- paste0(firsteq," Earnings Seasonality")
    toplot <- data_from_list(firsteq,rv$dtstr_hist ,"none",rv$dtstr_hist,msg_inputID="istr1")[[1]][,.(timestamp,close=get(the_av$seriesnm))]
    events <- oneticker_earns(thiseq,extenddtstr(rv$dtstr_hist,rtn="list"),rv$dtstr_hist)
    events <- events[, .(date=reportedDate,label=paste0(fifelse(surprise<0,"MISS!",""),format(reportedDate,"%Y%m")))]
  }
  else if (tolower(todoargs)  %in% c("yr","qtr","mo","wk","IMMroll","optmo","optqtr")) {
    gtitle <- paste0(firstticker," ",todoargs," Seasonality")
    toplot <- data_from_list(firstticker,rv$dtstr_hist ,"none",rv$dtstr_hist,msg_inputID="istr1")[[1]][,.(timestamp,close=get(the_av$seriesnm))]
    seastype = tolower(todoargs)
  }
  else {
    quick_message("Invalid Seasonality type for given asset")
    return(out)
  }
  out[["SCAT1"]]<- fg_seasonalstudy(toplot,yvar="close",seasonaldateset = events,seasonaltype<-seastype, normalize="index",projectfwd="mean",title=gtitle)
  return(out)
}

#' @importFrom TTR volatility
#' @import gt
#' @import gtExtras
#' @importFrom hash hash .set
#' @import data.table
#' @importFrom dygraphs dygraphOutput renderDygraph
#' @import shiny
#' @import shinyFeedback
#' @import FinanceGraphs

#require(shiny)
#require(shinyFeedback)
#require(dygraphs)
#require(gt)
#require(gtExtras)
#require(hash)
#require(TTR)
#require(data.table)
#require(FinanceGraphs)
source("./R/utilities.R")

tversion <-paste("1.45",Sys.getpid())
# 1.45: APpearance changes: Plumbed list management out of options, but still need to code
# 1.4: Passed Check, add new data
# 1.3: Most of list done; check opts for invalid tickers
# Save temp data, many parameters added to "the"

emptyhash<-hash::hash(key=s("TS1;TS2;TS3;TABLE1GT;TABLE2GT;TABLE3GT;TABLE4GT;DET1GT;DET2GT;SCAT1;SCAT2;HTML;OPT1GT;OPT2GT;OPTSCAT1"))
.set(emptyhash,keys=s("TABLE1GT;TABLE2GT;TABLE3GT;TABLE4GT;DET1GT;DET2GT;OPT1GT;OPT2GT;NEWSGT"),values=rep(data.frame(),9))

tabstyle=HTML("
      #output_text {
        position: absolute;
        top: 0;
        left: 0;
        z-index: 1;
      }
    ")

#source("c:\\d\\src\\R\\ut_package.R")

av_make_ui <- function() {
  order1=order2=NULL
  restore_avs_state()
  selectizeoptions <- avsd$selectizeoptions
  curr_assetlist <- sort(unique(the$assetlist$listnm))
  av_ui<- fluidPage(
    shinyFeedback::useShinyFeedback(),
     tags$head(
        tags$style(type='text/css',avsd$inputcss_side),
        tags$style(type='text/css',avsd$inputcss_top),
        tags$style(type='text/css',"#msg {font-size:9px; background-color: #ddfcd9; width:330px}"),
        tags$style(type='text/css',"#gropts {font-size:9px; background-color: #ddfcd9}"),
        tags$style(HTML(".radio-inline {  font-size: 10px;  }")),
        tags$style(HTML(".shiny-input-select { font-size: 10px; background-color: #f0f0f0 }")),
        tags$style(HTML(".shiny-bound-input { font-size: 10px}")),
        tags$style(HTML(".item { font-size: 10px; }"))
     ),
     fluidRow(
       column(1,
            actionButton("RUN",paste0(tversion),width='100%',class = "btn btn-primary"),
            selectizeInput("gropts","ts_opts",
                           c("last","lastlabel","hilightfirst","splitts","hilow"),
                           selected=c("last"),
                           multiple=TRUE,options(list(maxOptions=5,maxItems=1,selectizeoptions))),
            textInput(inputId="events", label="Events", value = "tp,5"),
            textInput(inputId="datestring", label="dts", value="-1y::"),
            textInput(inputId="volparams", label="Histvolparams", value="gk.yz;20;252"),
            sliderInput(inputId="dtstartfrac","FocusPct",min=0,max=100,value=0),
            radioButtons(inputId="rebase","Rebase",choices=c("none","start","focus"),selected="none"),
            checkboxInput(inputId="totrtn","TotalRtn",value=TRUE),
            checkboxInput(inputId="useLive","useLive",value=FALSE)
       ),

       column(11,
          fluidRow(
            column(width=2,selectInput("anopt1","",avsd$deflist[!is.na(order1)]$runcode,multiple=FALSE,width='100%')),
            column(width=6,textInput("istr1", "", the$inpline1,width='100%')),
            column(width=2,radioButtons("managelist1","",choices=c("<-","get","save"),selected ="<-",inline=TRUE)),
            column(width=2,selectizeInput("list1","",c("List"="", c("",sort(unique(the$assetlist$listnm)))),
                                          size="70%",options=list(create=TRUE)))
          ),
          fluidRow(
            column(width=2,selectInput("anopt2","",avsd$deflist[!is.na(order2)]$runcode,multiple=FALSE,width='100%')),
            column(width=6,textInput("istr2", "", the$inpline2,width='100%')),
            column(width=2,radioButtons("managelist2","",choices=c("<-","get","save"),selected ="<-",inline=TRUE)),
            column(width=2,selectizeInput("list2","",c("List"="", c("",sort(unique(the$assetlist$listnm)))),
                                          size="50%",options=list(create=TRUE)))
          ),
          fluidRow(
            tabsetPanel(id="inTabset",
                tabPanel("MAIN",
                        gt_output(outputId = "t1gt"),
                        splitLayout(cellWidths = c("35%", "65%"),
                                    gt_output(outputId = "t3l_gt"), gt_output(outputId = "t3r_gt")),
                        gt_output(outputId = "t2gt"),
                        dygraphOutput("view"),
                        dygraphOutput("view2")
                ),
                tabPanel("DETAIL", gt_output(outputId = "det1gt"), gt_output(outputId = "det2gt"),
                                  imageOutput("plot1"), imageOutput("plot2"),),
                tabPanel("OPTIONS",
                    fluidRow(
                      column(width=2,span(textInput(inputId="ochains", label="optChains", value="F,M,C,otm"),style=avsd$labelcss)),
                      column(width=2,numericInput(inputId="mindelta", label="mindelta", value=5,min=0,max=100)),
                      column(width=2,selectInput(inputId="otodisplay", label="Output", c("reduced","trading","all"),selected="reduced",multiple=FALSE)),
                      column(width=2,selectInput(inputId="oscaling", label="Scaling", c("None","10contracts","10kMV"),selected="None",multiple=FALSE))
                    ),
                    fluidRow(
                      gt_output(outputId="opt1gt"),
                      imageOutput(outputId="optplot1")
                    )
                ),
                tabPanel("NEWS",
                    fluidRow(
                    column(width=2,
                       numericInput(inputId="nArticles", label="nArticles", value=50,min=20,max=300),
                       numericInput(inputId="minabssent", label="minabssent", value=0.1,min=0,max=1),
                       selectInput(inputId="newssort",label="SortOn",c("time","sentiment","time,symbol","symbol,time"),multiple=FALSE),
                       selectInput(inputId="newsfilter",label="Filter",c("none","tickerOnly","opinion"),multiple=FALSE)
                     ),
                    column(width=8,
                       gt_output(outputId = "newsgt")
                     )
                  )
                ),
                tabPanel("AVOPTS",
                  column(width=3,
                    actionButton("SetOpts","Set Opts",width='50%',class = "btn btn-primary"),
                    span(textInput(inputId="avapikey", label="av api key", value=the$avapikey),style=avsd$labelcss),
                    span(textInput(inputId="avapientitlement", label="av entitlement", value=the$avapientitlement),style=avsd$labelcss),
                    span(textInput(inputId="cachedir", label="Cache Data Directory", value=the$cachedir),style=avsd$labelcss),
                    span(textInput(inputId="save_dir", label="AV dump Directory", value=the$save_dir),style=avsd$labelcss),
                    span(textInput(inputId="extracalc_file", label="extracalc csv", value=the$extracalc_file),style=avsd$labelcss),
                    span(textInput(inputId="ts_colorset", label="fgts colorset", value=the$ts_colorset),style=avsd$labelcss),
                    selectInput(inputId="sigpct","Regr Significance", c("0.05","0.025","0.1"),selected=c("0.025"),multiple=FALSE),
                    checkboxInput(inputId="save_data","SaveData",value=the$save_data),
                    checkboxInput(inputId="save_prices","Saveprices",value=the$save_data),
                    checkboxInput(inputId="save_cum","Save cumulatively",value=the$save_data),
                    checkboxInput(inputId="save_ts","Add Timestamp to data",value=the$save_ts),
                    checkboxInput(inputId="cleanonstart","Clean files on startup",value=the$cleanonstart)
                  ),
                  column(width=5,gt_output(outputId = "dumpthe"))
                  )
            )
          )
      )
     )
   )
  return(av_ui)
}

#' @importFrom stats cor
av_make_server <- function() {
#  adjusted_close=anopt1=anopt2=catg=catprio=change_percentage=cummktrtn=cumrtn=datestring=dtstartfrac=EquityName=ETFName=NULL
#  events=istr1=istr2=list1=list2=listnm=mindelta=mktrtn=ncak=newssort=ochains=oscaling=otodisplay=NULL
#  pctchg=price=prio=rcor=rebase=region=rtn=runcode=sector=showset=sigpct=sntmt=symbol=ticker=NULL
#  time_published=value_num=value_str=volparams=weight=n=nm=NULL

  av_server<-function(input, output,session) {

    message(" TOP ----------------------- in datasetinputs aa------- ")
    #restore_avs_state()
    curr_assetlist <- sort(unique(the$assetlist$listnm))
    FinanceGraphs::fg_sync_group("avshiny")
    if(the$cleanonstart==TRUE) {
      save_av_data(data.table(),"KILL")
    }
    reset_opts <- reactive({
      tmp1 <- paste0(input$anopt1,",",input$anopt2,",",input$list1,",",input$list2)
      updateSelectInput(session,"anopt1",selected="SelectOption")
      updateSelectInput(session,"anopt2",selected="SelectOption")
      return("reset opts at ",Sys.time())
    })

   # height_from_obs <- reactive({ the$out1h })

    need_index_asset <- reactive({
      is_in_list <- s(input$istr2)[1] %in% the$pxinv$symbol
      shinyFeedback::feedbackWarning("is_in_list", !is_in_list, "(1) Need an asset in inventory to compare against")
    })

    quick_message <- function(wh,this_message) {
      shinyFeedback::hideFeedback(inputId=wh)
      if(nchar(this_message)>0) {
        this_message <- paste0("<small>",this_message,"</small>")
        shinyFeedback::showFeedback(inputId=wh, text=this_message,color="#1f78b4")
      }
    }

    set_list <- function(todo,tlist,instr,no) {
      message_if_green(TRUE,"set_list(",todo,",",tlist," no ",no)
      rtnmsg <- ""
      if(todo=="save") {
        if(nchar(instr)<=0) {
          rtnmsg <- "Cannot Save blank Assetlist Name"
        }
        else {
          newassets <- data.table(ticker=s(instr))[,listnm:=tlist][]
          the$assetlist <- DTUpsert(the$assetlist,newassets,c("listnm"),replaceifbempty=the$assetlist[!(listnm==tlist),])
          save_avs_state("asset;the")
          updateRadioButtons(session,paste0("managelist",no),selected="<-")
          rtnmsg <-paste0("Asset set saved as ",tlist)
        }
      }
      if(todo=="get") {
        av_set_defaults(paste0("inpline",no),
                        paste0( the$assetlist[listnm==tlist,]$ticker,collapse=";") )
        save_avs_state("the")
        updateTextInput(session,paste0("istr",no), value= the[[paste0("inpline",no)]])
        updateRadioButtons(session,paste0("managelist",no),selected="<-")
      }
      return(rtnmsg)
    }

    observeEvent(input$managelist1, {
      req(input$managelist1)
      message(" list1: ",input$managelist1)
      thismsg <- set_list(input$managelist1,input$list1,input$istr1,1)
      quick_message("istr1",thismsg)
      #output$t1gt <-  render_gt(expr=dump_assetlist(returngt=TRUE))
    })

    observeEvent(input$managelist2, {
      req(input$managelist2)
      message(" list2: ",input$managelist2)
      thismsg <- set_list(input$managelist2,input$list2,input$istr2,2)
      quick_message("istr2",thismsg)
      #output$t1gt <-render_gt(expr=dump_assetlist(returngt=TRUE))
    })

    observeEvent(input$anopt1, {
      req(input$anopt1)
      shinyFeedback::hideFeedback(inputId="anopt1")
      if(nchar(thishelp <- avsd$deflist[runcode==input$anopt1,]$help)>0) {
        thishelp <- paste0("<small>",thishelp,"</small>")
        shinyFeedback::showFeedback(inputId="anopt1", text=thishelp,color="#2ca35f") }
    })

    observeEvent(input$anopt2, {
      req(input$anopt2)
      shinyFeedback::hideFeedback(inputId="anopt2")
      if(nchar(thishelp <- avsd$deflist[runcode==input$anopt2,]$help)>0) {
        shinyFeedback::showFeedback(inputId="anopt2", text=thishelp,color="#2ca35f") }
    })

    observeEvent(input$SetOpts, {
      old=toget=NULL
      rv <- isolate(reactiveValuesToList(input))
      th1<- dump_the()
      av_api_key(rv$avapikey,rv$avapientitlement)
      u1<-lapply(s("cachedir;save_dir;save_data;save_prices;save_cum;save_ts;cleanonstart;ts_colorset"),
                    \(x) av_set_defaults(x,rv[[x]]))
      av_set_defaults("assetlist_fn",paste0(rv$cachedir,"/avpf_assetlist.RD"))
      av_set_defaults("pxd_fn",paste0(rv$cachedir,"/avpf_px.fst"))
      av_set_defaults("inv_fn",paste0(rv$cachedir,"/avpf_inv.RD"))
      save_avs_state("the")
      th1 <- th1[,.(nm,old=toget)][dump_the(),on=.(nm)][,format:=fifelse(old==toget,"","yellow")][]
      setcolorder(th1,s("classtype;nm;toget;format"))
      output$dumpthe <- render_gt(expr=th1 |> gt() |> gt.basetheme() |> decorate_table())
    })

    observeEvent(input$save_data, {
      req(input$save_data)
      feedtxt<- fifelse(input$save_data,
                    paste0("AV data will be captured to ",input$save_dir,"/av_download.RD"),
                    "No Data Capture")
      shinyFeedback::showFeedback(inputId="save_dir", text=feedtxt,color="#2ca35f")
    })

    observeEvent(input$RUN, {
      rv <- isolate(reactiveValuesToList(input))
      #lineAssign(rv) just does not work, not sure why
      #toplot<-lapply(names(rv),\(x) { assign(x,rv[[x]],pos=1)})
      #cAssign("rv")
      toplot<-lapply(names(rv),\(x) { assign(x,rv[[x]],pos=1)})
      out<-hash::copy(emptyhash)
      message("H1 >>>>>>>>>>   AA input(",anopt1,"/",anopt2,") sid1(", istr1, ") sid2(", istr2, ") sz:",length(out))
      lapply(s("istr1;istr2"),\(x) quick_message(x,""))
      eqlist1 <- s(istr1)
      eqlist2 <- s(istr2)
      #cAssign("rv;istr1;istr2;eqlist1;eqlist2;rv",silent=TRUE)
      seriesnm <- fifelse(rv$totrtn,"adjusted_close","close")
      restore_avs_state(nrow(the$pxinv)>1 || substr(anopt1,1,2)=="TS" || substr(anopt2,1,2)=="TS")
      if(anopt1=="Gen:Inventory") {
        out[["TABLE4GT"]]<- dump_assetlist(returngt=TRUE)
        out[["TABLE3GT"]]<- the$pxinv |> gt.avtheme(themeset="pxinv")
        out[["TABLE2GT"]]<- av_get_pf("","MARKET_STATUS")  |> av_extract_df()  |>  gt.avtheme(themeset="mktstatus")
      }
      if(anopt1=="Gen:LivePx") {
        allgps <- the$assetlist[,.(tickers=paste0(.SD$ticker,collapse=" ")), by=.(listnm)]
        out[["TABLE3GT"]]<- dump_assetlist(returngt=TRUE)
        out[["TABLE4GT"]]<- the$pxinv |> gt.avtheme(themeset="pxinv")
        out[["TABLE2GT"]]<- av_get_pf("","MARKET_STATUS")  |> av_extract_df()  |>  gt.avtheme(themeset="mktstatus")
      }
      if(anopt1=="TS:PriceTS") {
        toplot <- lapply(eqlist1, \(x) manage_epx(x,datestring))
        quick_message("istr1",paste(toplot,collapse=""))
        toplot <- get_one_ts(eqlist1,rebase,datestring,dtstartfrac)
        out[["TS1"]] <- one_px_ts(toplot,rv,events=events,dtstartfrac=dtstartfrac)
        save_avs_state("px")
      }
      if(anopt2=="TS:PriceTS") {
        toplot <- lapply(eqlist2, \(x) manage_epx(x,datestring))
        quick_message("istr2",paste(toplot,collapse=""))
        toplot <- get_one_ts(eqlist2,rebase,datestring,dtstartfrac)
        out[["TS2"]] <- one_px_ts(toplot,rv,events=events,dtstartfrac=dtstartfrac)
        save_avs_state("px")
      }
      if(anopt1=="TS:ActiveTS") {
        is_in_list <- s(istr2)[1] %in% the$pxinv$symbol
        shinyFeedback::feedbackDanger("istr2", !is_in_list, "2. Need a hedge/index")
        req(is_in_list, cancelOutput = TRUE)

        toplot <- lapply(c(eqlist1,eqlist2), \(x) manage_epx(x,datestring))
        quick_message("istr1",paste(toplot,collapse=""))

        t_toget <- data.table(symbol=c(eqlist2[1],eqlist1),catg=c("idx",rep("act",length(eqlist1))))
        toplot <- the$pxd[t_toget,on=.(symbol)]  |> narrowbydtstr(datestring)
        toplot <- toplot[,.(date,adjusted_close,cumrtn=log(adjusted_close)-log(first(adjusted_close))),by=.(catg,symbol)]
        toplot <- toplot[,let(rtn=c(NA_real_,diff(cumrtn,1))), by=.(catg,symbol)]
        toplot_idx <- toplot[catg=="idx",.(date,idxpx=adjusted_close,mktrtn=rtn,cummktrtn=cumrtn)]
        toplot_idx <- toplot_idx[toplot[catg=="act",],on=.(date)]
        toplot_tridx <- toplot_idx[,.(date,variable=symbol,value=100*exp(cumrtn-cummktrtn))]
        out[["TS1"]] <-  one_px_ts(toplot_tridx,rv,title=paste0("Excess Returns over ",eqlist2[1]),extra_anno="hline,100",
                                   events=events,dtstartfrac=dtstartfrac)
        toplot_idx <- toplot_idx[,let(rtn=100*rtn,mktrtn=100*mktrtn)]
        volp_n <- as.integer(s(volparams)[[2]])
        toplot_corr <- toplot_idx[,rcor:=frollapply(.SD,volp_n,\(x) 100*cor(x$mktrtn,x$rtn,method="kendall"),by.column=FALSE), by=.(symbol)]
        out[["TS2"]] <- one_px_ts(toplot_corr[,.(date,variable=symbol,value=rcor)],rv,
                  title=paste0("Rolling ",volp_n," day kendall correlation"),extra_anno="hline,100",
                  events=events,dtstartfrac=dtstartfrac)
        rtnscatall <- fg_scatplot(toplot_idx,"rtn ~ mktrtn + color:symbol + doi:recent", "lm",datecuts=c(7),
                                      title=paste0("Asset Daily returns vs ",eqlist2[1], "Daily rtn"),
                                      axislabels=paste0("Asset TR;",eqlist2[1]," TR"),returnregresults=TRUE)
        out[["SCAT1"]] <- rtnscatall[[1]]
        out[["TABLE1GT"]]<- rtnscatall[[2]] |> gt.avtheme(themeset="activeregression", s(istr2)[1], sigpct)
        toplot_idx <- toplot_idx[,let(rtnidx=100*exp(cumrtn), mktrtnidx=100*exp(cummktrtn))]
        out[["SCAT2"]] <- fg_scatplot(toplot_idx,"rtnidx ~ mktrtnidx + color:symbol + point:label", "lm",datecuts=c(7),
                                      title=paste0("TR Level vs Level ",datestring),axislabels="Asset TR Index;Index TR Index")
        # TO do, other statistics (PCA?)
      }
      if(anopt1=="TS:HistVolTS") {
        toplot <- lapply(eqlist1, \(x) manage_epx(x,datestring))
        quick_message("istr1",paste(toplot,collapse=""))

        toplot <- get_one_ts(eqlist1,rebase,datestring,dtstartfrac)
        volp <- s(volparams)
        onevol <- function(x) {
          tdta <- toplot[[1]][symbol==x,]
          xdta <- tdta[,lapply(.SD,\(x) x+(get(seriesnm)-close)), .SDcols=s("open;high;low;close")]
          data.table(date=tdta$date,variable=x,
                      value=100*TTR::volatility(xdta, calc=volp[[1]],n=as.integer(volp[[2]]), N=as.integer(volp[[3]]))) }
        toplot2 <- rbindlist(lapply(eqlist1, onevol))
        out[["TS1"]] <- one_px_ts(toplot2,rv,title=paste("Volatility (pct) using ",volparams),events=events,dtstartfrac=dtstartfrac)
        out[["TS2"]] <- one_px_ts(toplot,rv,events=events,dtstartfrac=dtstartfrac)
      }
      if(anopt1=="EQ:DES") {
        tickerset = the$pxinv[data.table(symbol=eqlist1),on=.(symbol)][,.(symbol,type,currency)]
        if( length(eqset <- symbol_grep_by_type(eqlist1,"Equity"))>0 ) {
          eqdt <- rbindlist(lapply(eqset, \(x) av_get_pf(x,"OVERVIEW")))
          eqdt <- eqdt |> save_av_data("OVERVIEW")
          olist <- avsd$overviewlist[,variable:=EquityName][]
          eqdta <- olist[eqdt,on=.(variable)][source=="av",]
          eqdta <- eqdta[order(catprio,prio)][,.(category,symbol,catprio,prio,variable,ltype,value_str,format ,value_num)]
          toplot <- dcast(eqdta[order(catprio,prio)], catprio+prio+category + variable+format ~ symbol, value.var="value_str")
          tbl_loc <- fifelse(length(eqset)>3, "TABLE1GT","TABLE4GT")
          out[[tbl_loc]] <-  toplot |> gt(groupname_col="category",row_group_as_column=TRUE) |> gt.avtheme(themeset="eqdesc1")
        }
        # tab_style(eval(parse(text=fm31)),eval(parse(text=fm32)))
        if( length(eqset <- symbol_grep_by_type(eqlist1,"ETF"))>0 ) {
          eqdt <- rbindlist(lapply(eqset, \(x) av_get_pf(x,"ETF_PROFILE")))
          eqdt <- eqdt |> save_av_data("ETF_PROFILE")
          olist <- avsd$overviewlist[,variable:=ETFName][]
          eqdta <- olist[eqdt,on=.(variable)][source=="av",]
          toplot <- dcast(eqdta[order(catprio,prio)], catprio+prio+category + variable+format ~ symbol, value.var="value_str")

          sectorset <- eqdt |> av_extract_df("sectors")
          sectorset <- dcast(sectorset[!is.na(sector),][,let(weight=100*weight)], sector ~ symbol,value.var="weight")
          sectorset <- sectorset[,let(category="sects",catprio=max(toplot$catprio)+1,prio=.I,format="")]
          setnames(sectorset,"sector","variable")
          toplot <- rbindlist(list(toplot, sectorset),use.names=TRUE,fill=TRUE)
          tbl_loc <- fifelse(length(eqset)>3, "TABLE2GT","TABLE3GT")
          out[[tbl_loc]] <-  toplot |> gt(groupname_col="category",row_group_as_column=TRUE) |> gt.avtheme(themeset="eqdescsec")

          holdset <- eqdt |> av_extract_df("holdings")
          holdset <- holdset[,.SD[order(-weight)][,let(n=.I-min(.I)+1, weight=100*weight)], by=.(symbol)]
          holdset <- dcast(holdset[n<=50,],n ~ symbol,value.var=c("description","weight"))
          holdsetcn <- data.table(nm=colnames(holdset)[-1])[,let(i=.I+1,symbol=s(nm,"_")[2]),by=.I][order(symbol,nm)]
          setcolorder(holdset, c(1,holdsetcn$i))
          out[["DET1GT"]] <- holdset |> gt.avtheme(themeset="etfholdings")
          quick_message("istr1","ETF holdings in DETAILS tab")
        }
      }
      if(anopt1=="EQ:DivEarn") {
        fwddts <- extenddtstr(datestring,rtn="list",endchg=2*360)
        allearn <- rbindlist(lapply(symbol_grep_by_type(eqlist1,"Equity"),\(x) oneticker_earns(x,fwddts,datestring)))
        out[["TABLE1GT"]]<- allearn |> gt.avtheme(themeset="earnings")
        alldivs <- rbindlist(lapply(eqlist1, \(x) oneticker_divs(x,datestring)),fill=TRUE,use.names=TRUE)
        out[["TABLE2GT"]]<- alldivs |> gt.avtheme(themeset="dividends")
      }
      if(anopt1=="Gen:Movers") {
        tdta <- av_get_pf("","TOP_GAINERS_LOSERS") |> save_av_data("TOP_GAINERS_LOSERS")
        tab1 <- rbindlist(list(
          av_extract_df(tdta,"top_losers")[,.(ticker,price,pctchg=as.numeric(gsub("%","",change_percentage)),item="losers",n=.I)],
          av_extract_df(tdta,"top_gainers")[,.(ticker,price,pctchg=as.numeric(gsub("%","",change_percentage)),item="winners",n=.I)],
          av_extract_df(tdta,"most_actively_traded")[,.(ticker,price,pctchg=as.numeric(gsub("%","",change_percentage)),item="traded",n=.I)]
        ))
        toplot = tab1 |> tidyr::pivot_wider(names_from="item",id_cols="n",values_from=c(ticker,price,pctchg),names_vary="slowest")
        out[["TABLE1GT"]]<- toplot |> gt.avtheme(themeset="Gen:Movers",tdta[variable=="last_updated",]$value_str)
      }
      if(anopt1=="Gen:NameSearch") {
        eqdta <- av_get_pf("","SYMBOL_SEARCH",keywords=istr1) |> save_av_data("SYMBOL_SEARCH")
        eqdta[,let(format=fcase(type=="Equity" & region=="United States","bold",default=""))]
        setcolorder(eqdta,neworder="matchScore")
        out[["TABLE1GT"]] <- eqdta |> gt.avtheme(themeset="etfholdings",istr1)
      }
      if(anopt1=="EQ:OptSearch") {
        allmsg <- ""
        indta <- data.table()
        for (x in eqlist1) {
          theseopts <- av_get_pf(x,"HISTORICAL_OPTIONS")
          if("variable" %in% names(theseopts)) {
            allmsg <- paste(allmsg, x) }
          else {
            indta <- rbindlist(list(indta,theseopts), fill=TRUE)
          }
        }
        indta <- indta |> save_av_data("HISTORICAL_OPTIONS")
        quick_message("istr1",paste(nrow(indta),"rows ", fifelse(nchar(allmsg)>0, paste0(allmsg, " missing"),"")))
        if(nrow(indta)>0) {
          inspots <- rbindlist(lapply(eqlist1,\(x) av_get_pf(x,"GLOBAL_QUOTE")))
          inspots <- inspots[variable=="price",.(symbol,spot=value_num)]
          indta <- inspots[indta,on=.(symbol)][,ncak:=1]
          filteredopts <- indta |> av_grep_opts(grepstring=ochains,mindelta=as.numeric(mindelta)/100)
          filteredopts <- filteredopts |> av_opt_helper_cols(scaling=oscaling)
          colstoshow <- data.table(showset=c("reduced","trading","all"),
                                   colstring=c("symbol;ncak;strike;type;daysExp;moneyn;IV;mark;last;bo_pct;delta;vega;theta;contractID",
                                               "symbol;ncak;strike;daysExp;volume;open_interest;IV;delta;last;mark;bo_pct;bid;ask;bid_size_poi;ask_size_poi;contractID",
                                               paste0(names(filteredopts),collapse=";")))
          out[["SCAT1"]] <- fg_scatplot(filteredopts,"IV ~ strikepctspot + color:symbol + symbol:type + xline:0")
          #cAssign("filteredopts;colstoshow;otodisplay")
          filteredopts<- filteredopts[,.SD,.SDcols=s(colstoshow[showset==otodisplay,]$colstring)]
          out[["OPT1GT"]] <- filteredopts |> gt.avtheme(themeset="filteredopts", istr1, otodisplay)
        }
      }
      if(anopt1=="EQ:News") {
        allnews <- rbindlist(lapply(eqlist1,getNews))
        if(newssort=="sentiment") { allnews<- allnews[order(symbol,-sntmt)] }
        if(newssort=="time,symbol") { allnews<- allnews[order(-time_published,symbol)] }
        if(newssort=="symbol,time") { allnews<- allnews[order(symbol, -time_published)] }
        out[["NEWSGT"]] <-allnews |> gt.avtheme(themeset="news",istr1)
      }
    if(length(out)<=1) {
      if(anopt1=="SelectOption" || anopt2=="SelectOption") {
        quick_message("anopt1","Select an Action")
      }
      else {
        quick_message("anopt1",paste0("NOT IMPLEMENTED YET:", anopt1, " or ",anopt2))
      }
    }
    # =============================================================================================================================================
    the$dyg1h <- fifelse( "dygraphs" %in% class(out[["TS1"]]), "600px","auto")
    the$dyg2h <- fifelse( "dygraphs" %in% class(out[["TS2"]]), "600px","auto")

    #u1=lapply(hash::keys(out), \(x) { message("x>",x,">",class(out[[x]]))})
    # Always render if you want to replace old items from RUn to RUn
    output$t1gt <- render_gt(expr=out[["TABLE1GT"]])
    output$t2gt <- render_gt(expr=out[["TABLE2GT"]])
    output$t3l_gt <- render_gt(expr=out[["TABLE3GT"]])
    output$t3r_gt <- render_gt(expr=out[["TABLE4GT"]])
    output$det1gt <- render_gt(expr=out[["DET1GT"]])
    output$det2gt <- render_gt(expr=out[["DET2GT"]])
    output$opt1gt <-  render_gt(expr=out[["OPT1GT"]])
    output$newsgt <- render_gt({ out[["NEWSGT"]] })
    output$view   <- renderDygraph({ out[["TS1"]] })
    output$view2  <- renderDygraph({ out[["TS2"]] })
    output$plot1 <- renderPlot({ out[["SCAT1"]] })
    output$plot2 <- renderPlot({ out[["SCAT2"]] })
    output$optplot1<- renderPlot({ out[["SCAT1"]] })
    output$msg <- renderPrint({  print(out[["MSG"]]) })

    tabfocus = avsd$deflist[runcode==anopt1,]$focus
    updateTabsetPanel(session,"inTabset",selected=tabfocus)

   }) # obsARUnn
  } # Server
  return(av_server)
}



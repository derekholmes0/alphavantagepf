#' @importFrom TTR volatility
#' @import gt
#' @import gtExtras
#' @import data.table
#' @importFrom dygraphs dygraphOutput renderDygraph
#' @import shiny
#' @import shinyFeedback
#' @import FinanceGraphs

source("./R/utilities.R")

# 1.8: News, ability to keep output from one run to the next
# 1.6: Bug checks and check()
# 1.45: APpearance changes: Plumbed list management out of options, but still need to code
# 1.4: Passed Check, add new data
# 1.3: Most of list done; check opts for invalid tickers
# Save temp data, many parameters added to "the"
#av_set_defaults("ochains","F,M,C,otm"); save_avs_state("the")

av_make_ui <- function() {
  order1=order2=NULL
  #restore_avs_state(msg="OnStartup")
  curr_assetgroups <- sort(unique(the_av$assetgroups$listnm))
  runlist1 <- avsd$deflist[!is.na(order1)]$runcode
  runlist2 <- avsd$deflist[!is.na(order2)]$runcode
  av_ui<- fluidPage(
    shinyFeedback::useShinyFeedback(),
     tags$head(
        tags$style(type='text/css',avsd$inputcss_side),
        tags$style(type='text/css',avsd$inputcss_top),
        tags$style(type='text/css',"#msg {font-size:10px; background-color: #ddfcd9; width:330px}"),
        tags$style(type='text/css',"#gropts {font-size:10px; background-color: #ddfcd9}"),
        tags$style(HTML(".radio-inline {  font-size: 11px;  }")),
        tags$style(HTML(".shiny-input-select { font-size: 11px; background-color: #f0f0f0 }")),
        tags$style(HTML(".shiny-bound-input { font-size: 11px}")),
        tags$style(HTML(".item { font-size: 11px; }")),
        tags$style(HTML(".no-gap-row { display: flex; gap: 0;align-items: flex-start; }")),
        tags$style(HTML(".no-gap-row .table-pane { overflow-x: auto; flex: 0 0 auto;}")),
        tags$style(HTML(".selectize-dropdown-content {  max-height: 500px;  }"))
     ),
     fluidRow(
       column(1,
            actionButton("RUN","RUN",width='100%',class = "btn btn-primary"),
            selectizeInput("gropts","TSGraphopts",
                           c("last","lastlabel","hilightfirst","splitts","hilow"),
                           selected=c("last"),
                           multiple=TRUE,options(list(maxOptions=5,maxItems=1,avsd$selectizeoptions))),
            textInput(inputId="ts_events", label="Events", value = the_av$ts_events),
            textInput(inputId="datestring", label="dts", value=the_av$datestring),
            textInput(inputId="ts_volparams", label="Histvolparams", value=the_av$ts_volparams),
            sliderInput(inputId="dtstartfrac","FocusPct",min=0,max=100,value=0),
            radioButtons(inputId="ts_rebase","Rebase",choices=c("none","start","focus"),selected=the_av$ts_rebase),
            checkboxInput(inputId="totrtn","TotalRtn",value=TRUE),
            checkboxInput(inputId="useLive","useLive",value=FALSE),
            checkboxInput(inputId="verbose","Status Msgs",value=the_av$verbose),
            checkboxInput(inputId="autocopy","copyTable",value=the_av$autocopy)
       ),

       column(10,  # Was 11
          fluidRow(
            column(width=2,selectInput("anopt1","",runlist1,multiple=FALSE,width='100%')),
            column(width=6,textInput("istr1", "", the_av$inpline1,width='100%')),
            column(width=2,selectizeInput("list1","",c("List"="", c("",sort(unique(the_av$assetgroups$listnm)))),
                                          size="80%",options=list(create=TRUE))),
            column(width=2,radioButtons("managelist1","",choices=c("get","save","delete"),selected =character(0),width="60%",inline=TRUE))
          ),
          fluidRow(
            column(width=2,selectInput("anopt2","",runlist2,multiple=FALSE,width='100%')),
            column(width=6,textInput("istr2", "", the_av$inpline2,width='100%')),
            column(width=2,selectizeInput("list2","",c("List"="", c("",sort(unique(the_av$assetgroups$listnm)))),
                                          size="80%",options=list(create=TRUE))),
            column(width=2,radioButtons("managelist2","",choices=c("get","save","delete"),selected =character(0),width="60%",inline=TRUE))
          ),
          fluidRow(
            tabsetPanel(id="inTabset",
                tabPanel("MAIN",
                        gt_output(outputId = "t1gt"),
                        #splitLayout(gt_output(outputId = "t3l_gt"), gt_output(outputId = "t3r_gt"),cellWidths=c("60%","40%")),
                        fillPage(div(class = "no-gap-row",
                                     div(class = "table-pane", gt_output("t3l_gt")),
                                     div(class = "table-pane", gt_output("t3r_gt")) )),
                        gt_output(outputId = "t2gt"),
                        dygraphOutput("view"),
                        dygraphOutput("view2")
                ),
                tabPanel("DETAIL", gt_output(outputId = "det1gt"), gt_output(outputId = "det2gt"),
                                  imageOutput("plot1"), imageOutput("plot2")),
                tabPanel("OPTIONS",
                    fluidRow(
                      column(width=2,span(textInput(inputId="ochains", label="Chains",value=the_av$ochains),style=avsd$labelcss)),
                      column(width=2,numericInput(inputId="mindelta", label="mindelta", value=5,min=0,max=100)),
                      column(width=2,selectInput(inputId="otodisplay", label="Output",
                                                 c("reduced","trading","all"),selected=the_av$otodisplay,multiple=FALSE)),
                      column(width=2,selectInput(inputId="oscaling", label="Scaling",
                                                 c("None","10contracts","10kMV"),selected=the_av$oscaling,multiple=FALSE))
                    ),
                    fluidRow(
                      gt_output(outputId="opt1gt"),
                      imageOutput(outputId="optplot1")
                    )
                ),
                tabPanel("NEWS",
                    fluidRow(
                    column(width=2,
                       numericInput(inputId="nArticles", label="nArticles", value=the_av$nArticles,min=20,max=300),
                       selectInput(inputId="newssort",label="SortOn",c("time","sentiment","time,symbol","symbol,time"),selected="time",
                                   multiple=FALSE),
                       selectInput(inputId="newsfilter",label="Filter on:",c("none","tickerOnly","useMinSentiment","maxDays"),
                                   selected="none",multiple=TRUE),
                       span(textInput(inputId="newsgrep", label="Terms to filter out", value=the_av$newsgrep),style=avsd$labelcss),
                       numericInput(inputId="minabssent", label="MinSentiment", value=the_av$minabssent,min=0,max=1),
                       numericInput(inputId="maxagedays", label="Maximum Age (Days)", value=the_av$maxagedays,min=0),
                    ),
                    column(width=8,
                       gt_output(outputId = "newsgt")
                     )
                  )
                ),
                tabPanel("AVOPTS",
                  column(width=3,
                    actionButton("SetOpts","Set Opts",width='50%',class = "btn btn-primary"),
                    span(passwordInput(inputId="avapikey", label="av api key", value=the_av$avapikey),style=avsd$labelcss),
                    span(textInput(inputId="avapientitlement", label="av entitlement", value=the_av$avapientitlement),style=avsd$labelcss),
                    span(textInput(inputId="cachedir", label="Cache Data Directory", value=the_av$cachedir),style=avsd$labelcss),
                    #span(textInput(inputId="extracalc_file", label="extracalc csv", value=the_av$extracalc_file),style=avsd$labelcss), ## <<--- TODO
                    span(textInput(inputId="ts_colorset", label="fgts colorset", value=the_av$ts_colorset),style=avsd$labelcss),
                    selectInput(inputId="sigpct","Regr Significance", c("0.05","0.025","0.1"),selected=c("0.025"),multiple=FALSE),
                    span(textInput(inputId="av_dump_dir", label="AV dump Directory", value=the_av$av_dump_dir),style=avsd$labelcss),
                    selectInput(inputId="capture_av_what",label="Capture AV Data",c("none","pricesonly","noprices","all"),
                                selected=s(the_av$capture_av_what), multiple=FALSE),
                    selectInput(inputId="capture_av_update",label="Update or Cumulative",c("update","cum"),
                                selected=s(the_av$capture_av_update), multiple=FALSE),
                    selectInput(inputId="capture_av_save",label="Data Saving Options",c("none","CleanOnStart","SaveEveryAVCall","SaveNowOnOptUpdate"),
                                selected=the_av$capture_av_save, multiple=TRUE)
                  ),
                  column(width=7,gt_output(outputId = "dumpthe"))
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
#  ts_events=istr1=istr2=list1=list2=listnm=mindelta=mktrtn=ncak=newssort=ochains=oscaling=otodisplay=NULL
#  pctchg=price=prio=rcor=ts_rebase=region=rtn=runcode=sector=showset=sigpct=sntmt=symbol=ticker=NULL
#  time_published=value_num=value_str=ts_volparams=weight=n=nm=NULL
  ts_rebase=ts_events=ts_volparams=NULL
  out <- list()
  av_server<-function(input, output,session) {
    inlist=list_ts=NULL
    quick_message <- function(wh,this_message,eval=TRUE) {
      shinyFeedback::hideFeedback(inputId=wh)
      if(nchar(this_message)>0 & eval==TRUE) {
        this_message <- paste0("<small>",this_message,"</small>")
        shinyFeedback::showFeedback(inputId=wh, text=this_message,color="#1f78b4")
      }
    }
    curr_assetgroups <- sort(unique(the_av$assetgroups$listnm))
    quick_message("ochains","[F(ront)|B(ack)],[M(onth)|Q(tr)],[C(all)|P(ut)],[itm|otm|all]")
    # On Startup download current index list if not there
    update_tickerlists( is.null(the_av$tickerlist) | nrow(the_av$tickerlist)<=0 | (min(the_av$tickerlist$list_ts)<=Sys.Date()-7) )
    FinanceGraphs::fg_sync_group("avshiny")
    if("CleanOnStart" %in% the_av$capture_av_save) {  save_av_data(data.table(),"KILL") }
    # ----
    reset_opts <- reactive({
      tmp1 <- paste0(input$anopt1,",",input$anopt2,",",input$list1,",",input$list2)
      updateSelectInput(session,"anopt1",selected="SelectOption")
      updateSelectInput(session,"anopt2",selected="SelectOption")
      return("reset opts at ",Sys.time())
    })

   # height_from_obs <- reactive({ the_av$out1h })
    need_index_asset <- reactive({
      is_in_list <- s(input$istr2)[1] %in% the_av$pxinv$symbol
      shinyFeedback::feedbackWarning("is_in_list", !is_in_list, "(1) Need an asset in inventory to compare against")
    })
    set_list <- function(todo,tlist,instr,no) {
      rtnmsg <- ""
      if(todo=="save") {
        if(nchar(instr)<=0 | nchar(tlist)<=0) {
          rtnmsg <- "Cannot Save blank assetgroups Name"
        }
        else {
          newassets <- data.table(ticker=s(instr))[,listnm:=tlist][]
          the_av$assetgroups <- DTUpsert(the_av$assetgroups,newassets,c("listnm"),replaceifbempty=the_av$assetgroups[!(listnm==tlist),])
          save_avs_state("all")
          updateSelectizeInput(session,"list1", choices=sort(unique(the_av$assetgroups$listnm)))
          updateSelectizeInput(session,"list2", choices=sort(unique(the_av$assetgroups$listnm)))
          rtnmsg <-paste0("Asset set saved as ",tlist)
        }
      }
      if(todo=="get") {
        av_set_defaults(paste0("inpline",no),paste0( the_av$assetgroups[listnm==tlist,]$ticker,collapse=";") )
        save_avs_state("all")
        updateTextInput(session,paste0("istr",no), value= the_av[[paste0("inpline",no)]])
      }
      if(todo=="delete") {
        the_av$assetgroups <- the_av$assetgroups[!listnm==tlist,]
        updateSelectizeInput(session,"list1", choices=sort(unique(the_av$assetgroups$listnm)))
        updateSelectizeInput(session,"list2", choices=sort(unique(the_av$assetgroups$listnm)))
        rtnmsg <-paste0("Deleted Asset List: ",tlist)
        save_avs_state("all")
      }
      Sys.sleep(0.3)
      updateRadioButtons(session,paste0("managelist",no),selected=character(0))
      return(rtnmsg)
    }

    observeEvent(input$managelist1, {
      req(input$managelist1)
      quick_message("istr1",set_list(input$managelist1,input$list1,input$istr1,1))
    })

    observeEvent(input$managelist2, {
      req(input$managelist2)
      quick_message("istr2",set_list(input$managelist2,input$list2,input$istr2,2))
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
        thishelp <- paste0("<small>",thishelp,"</small>")
        shinyFeedback::showFeedback(inputId="anopt2", text=thishelp,color="#2ca35f") }
    })

    observeEvent(input$SetOpts, {
      old=toget=NULL
      rv <- isolate(reactiveValuesToList(input))
      th1<- dump_the()
      oldcache <- th1[nm=="cachedir",]$toget
      av_api_key(rv$avapikey,rv$avapientitlement)
      u1<-lapply(s("cachedir;av_dump_dir;capture_av_what;capture_av_update;capture_av_save;ts_colorset;avapikey;avapientitlement"),
                    \(x) av_set_defaults(x,rv[[x]]) )
      if(!(rv$cachedir==oldcache)) {
        message_if_red(TRUE,"Cache directory moved; cleaning up old price/inventory data")
        file.remove(paste0(oldcache,"/avpf_px.fst"))
        file.remove(paste0(oldcache,"/avpf_inv.RD"))
      }
      # constants_fn always has to be in tmp directory: av_set_defaults("constants_fn",paste0(rv$cachedir,"/avpf_constants.RD"))
      av_set_caching_directories()
      av_set_defaults("starttab","usedefault")
      th1 <- th1[,.(nm,old=toget)][dump_the(),on=.(nm)][,format:=fifelse(old==toget,"","yellow")][]
      th1 <- th1[,.SD,.SDcols=s("classtype;nm;toget;format")]
      save_avs_state("all")
      output$dumpthe <- render_gt(expr=th1 |> gt() |> gt.basetheme() |> decorate_table())
    })

    observeEvent(input$capture_av_what, {
      req(input$capture_av_what)
      feedtxt<- fcase(input$capture_av_what=="none","No Data Capture",
                      input$capture_av_what=="pricesonly","Only prices captured",
                      input$capture_av_what=="noprices","Only non price data captured",
                      input$capture_av_what=="all","All Data captured",
                      default="Denmark call home")
      if("capture" %chin% feedtxt) {
        feedtxt <- paste0(feedtxt," to ",input$av_dump_dir,"/av_download.RD")
      }
      shinyFeedback::showFeedback(inputId="av_dump_dir", text=feedtxt,color="#2ca35f")
    })

    observeEvent(input$capture_av_save, {
      req(input$capture_av_save)
      if("SaveNowOnOptUpdate" %in% input$capture_av_save) {
        save_av_data(data.table(),"SaveNowOnOptUpdate")
        updateSelectInput(session,"capture_av_save",selected=the_av$capture_av_save)
      }
    })

    observeEvent(input$verbose, {
      av_set_defaults("verbose",input$verbose)
      save_avs_state("all")
      })

    observeEvent(input$autocopy, {
      av_set_defaults("autocopy",input$autocopy)
      save_avs_state("all")
    })


    observeEvent(input$RUN, {
      rv <- isolate(reactiveValuesToList(input))
      thisenv <- environment()
      if(the_av$avapikey=="NOT_SET") {
        quick_message("anopt1","SET Alphavantage API key")
        quick_message("anopt2","SET Alphavantage API key")
        return()
      }
      # Make variables out of captured input values
      lapply(names(rv),\(x) { assign(x,rv[[x]],envir=thisenv)})
      # Save plotting data
      lapply(grepv("^ts",names(rv)), \(x) av_set_defaults(x,rv[[x]]))
      save_avs_state("the")
      # Out gets destroyed on end of routine.  Need to keep it in the the environment.
      message("H1 >>>>>>>>>>   AA input(",anopt1,"/",anopt2,") sid1(", istr1, ") sid2(", istr2, ") sz:",length(out))
      if( nrow(savedgtnames <- dump_the()[classtype=="gt_tbl",])>0) {
        for(x in savedgtnames$nm) out[[x]]<- get(x,envir=the_av)
        message_if_green(the_av$verbose,"Restoring previous items ",savedgtnames$nm)
      }
      lapply(s("istr1;istr2"),\(x) quick_message(x,""))
      eqlist1 <- s(istr1)
      eqlist2 <- s(istr2)
      seriesnm <- fifelse(rv$totrtn,"adjusted_close","close")
      #restore_avs_state(nrow(the_av$pxinv)>1 || substr(anopt1,1,2)=="TS" || substr(anopt2,1,2)=="TS",msg="always")
      if(anopt1=="Gen:Inventory") {
        if(nrow(the_av$pxinv)<=0) {  quick_message("istr1","Create Data by running a Time Series Graph") }
        else {
          out[["TABLE4GT"]]<- dump_assetgroups(returngt=TRUE) |>  gt.avtheme(themeset="assetgroups")
          out[["TABLE3GT"]]<- the_av$pxinv[,age:=Sys.Date()-end_dt] |> gt.avtheme(themeset="pxinv")
          out[["DET1GT"]]<- av_get_pf("","MARKET_STATUS")  |> av_extract_df()  |>  gt.avtheme(themeset="mktstatus")
        }
      }
      if(anopt1=="Gen:LivePx") {
        #message(" get live:  entitlement: ",  options("av_api_entitlement")$av_api_entitlement, " > ")
        # Adding FX:  At this point, I'm almost done with this..
        # Non FX
        toplot <- av_get_pf(symbol_grep_by_type(NULL,"Equity|ETF"),"REALTIME_BULK_QUOTES",melted=FALSE)
        if( length( fxsymbols <-symbol_grep_by_type(NULL,"FX") )>0 ) {
          toplot_fx <- lapply(fxsymbols, \(x) av_get_pf(x,"CURRENCY_EXCHANGE_RATE",melted=FALSE) |> av_extract_fx(cols="symbol;timestamp;close") )
          toplot <- rbindlist(list(toplot,rbindlist(toplot_fx)),use.names=TRUE,fill=TRUE)
        }
        toplot <- data.table(symbol=eqlist1)[,inlist:=TRUE][toplot,on=.(symbol)][order(change_percent)]
        avsh_clipboard(toplot,"liveprice")
        out[["TABLE1GT"]]<- toplot |>  gt.avtheme(themeset="live")
        # Save latest to history, but how to ensure no gaps?
      }
      if(anopt1=="TS:PriceTS") {
        toplot <- lapply(eqlist1, \(x) manage_epx(x,datestring,addlive=input$useLive))
        quick_message("istr1",paste(toplot,collapse=""))
        toplot <- get_one_ts(eqlist1,ts_rebase,datestring,dtstartfrac)
        avsh_clipboard(toplot,anopt1)
        out[["TS1"]] <- one_px_ts(toplot,rv,events=ts_events,dtstartfrac=dtstartfrac)
        save_avs_state("px")
      }
      if(anopt2=="TS:PriceTS") {
        toplot <- lapply(eqlist2, \(x) manage_epx(x,datestring,addlive=input$useLive))
        quick_message("istr2",paste(toplot,collapse=""))
        toplot <- get_one_ts(eqlist2,ts_rebase,datestring,dtstartfrac)
        out[["TS2"]] <- one_px_ts(toplot,rv,events=ts_events,dtstartfrac=dtstartfrac)
        save_avs_state("px")
      }
      if(anopt1=="TS:ActiveTS") {
        is_in_list <- s(istr2)[1] %in% the_av$pxinv$symbol
        shinyFeedback::feedbackDanger("istr2", !is_in_list, "2. Need a hedge/index")
        req(is_in_list, cancelOutput = TRUE)

        toplot <- lapply(c(eqlist1,eqlist2), \(x) manage_epx(x,datestring,addlive=input$useLive))
        quick_message("istr1",paste(toplot,collapse=""))

        t_toget <- data.table(symbol=c(eqlist2[1],eqlist1),catg=c("idx",rep("act",length(eqlist1))))
        t_toget <- t_toget[,.SD[1],by=.(symbol)] # Weed out duplicates
        toplot <- the_av$pxd[t_toget,on=.(symbol)]  |> narrowbydtstr(datestring)
        toplot <- toplot[,.(timestamp,adjusted_close,cumrtn=log(adjusted_close)-log(first(adjusted_close))),by=.(catg,symbol)]
        toplot <- toplot[,let(rtn=c(NA_real_,diff(cumrtn,1))), by=.(catg,symbol)]
        toplot_idx <- toplot[catg=="idx",.(timestamp,idxpx=adjusted_close,mktrtn=rtn,cummktrtn=cumrtn)]
        toplot_idx <- toplot_idx[toplot[catg=="act",],on=.(timestamp)]
        toplot_tridx <- toplot_idx[,.(timestamp,variable=symbol,value=100*exp(cumrtn-cummktrtn))]
        avsh_clipboard(toplot_tridx,anopt1)
        out[["TS1"]] <-  one_px_ts(toplot_tridx,rv,title=paste0("Excess Returns over ",eqlist2[1]),extra_anno="hline,100",
                                   events=ts_events,dtstartfrac=dtstartfrac)
        toplot_idx <- toplot_idx[,let(rtn=100*rtn,mktrtn=100*mktrtn)]
        volp_n <- as.integer(s(ts_volparams)[[2]])
        toplot_corr <- toplot_idx[,rcor:=frollapply(.SD,volp_n,\(x) 100*cor(x$mktrtn,x$rtn,method="kendall",
                                                        use="complete.obs"),by.column=FALSE), by=.(symbol)]
        out[["TS2"]] <- one_px_ts(toplot_corr[,.(timestamp,variable=symbol,value=rcor)],rv,
                  title=paste0("Rolling ",volp_n," day kendall correlation"),extra_anno="hline,100",
                  events=ts_events,dtstartfrac=dtstartfrac)
        rtnscatall <- fg_scatplot(toplot_idx,"rtn ~ mktrtn + color:symbol +  point:label", "lm",datecuts=c(7),
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
        toplot <- lapply(eqlist1, \(x) manage_epx(x,datestring,addlive=input$useLive))
        quick_message("istr1",paste(toplot,collapse=""))
        toplot <- get_one_ts(eqlist1,ts_rebase,datestring,dtstartfrac)
        volp <- s(ts_volparams)
        onevol <- function(x) {
          tdta <- toplot[[1]][symbol==x,]
          xdta <- tdta[,lapply(.SD,\(x) x+(get(seriesnm)-close)), .SDcols=s("open;high;low;close")]
          data.table(timestamp=tdta$timestamp,variable=x,
                      value=100*TTR::volatility(xdta, calc=volp[[1]],n=as.integer(volp[[2]]), N=as.integer(volp[[3]]))) }
        toplot2 <- rbindlist(lapply(eqlist1, onevol))
        avsh_clipboard(toplot2,"HistVol")
        out[["TS1"]] <- one_px_ts(toplot2,rv,title=paste("Volatility (pct) using ",ts_volparams),events=ts_events,dtstartfrac=dtstartfrac)
        out[["TS2"]] <- one_px_ts(toplot,rv,events=ts_events,dtstartfrac=dtstartfrac)
      }
      if(anopt1=="EQ:DES") {
        tickerset = the_av$pxinv[data.table(symbol=eqlist1),on=.(symbol)][,.(symbol,type,currency)]
        if( length(eqset <- symbol_grep_by_type(eqlist1,"Equity"))>0 ) {
          eqdt <- rbindlist(lapply(eqset, \(x) av_get_pf(x,"OVERVIEW")))
          eqdt <- eqdt |> save_av_data("OVERVIEW")
          olist <- avsd$overviewlist[,variable:=EquityName][]
          eqdta <- olist[eqdt,on=.(variable)][source=="av",]
          eqdta <- eqdta[order(catprio,prio)][,.(category,symbol,catprio,prio,variable,ltype,value_str,format ,value_num)]
          toplot <- dcast(eqdta[order(catprio,prio)], catprio+prio+category + variable+format ~ symbol, value.var="value_str")
          tbl_loc <- fifelse(length(eqset)>3, "TABLE1GT","TABLE4GT")
          out[[tbl_loc]] <-  toplot |> gt.avtheme(themeset="eqdesc1")
        }
        # tab_style(eval(parse(text=fm31)),eval(parse(text=fm32)))
        if( length(eqset <- symbol_grep_by_type(eqlist1,"ETF"))>0 ) {
          eqdt <- rbindlist(lapply(eqset, \(x) av_get_pf(x,"ETF_PROFILE")))
          eqdt <- eqdt |> save_av_data("ETF_PROFILE")
          olist <- avsd$overviewlist[,variable:=ETFName][]
          eqdta <- olist[eqdt,on=.(variable)][source=="av",]
          toplot <- dcast(eqdta[order(catprio,prio)], catprio+prio+category + variable+format ~ symbol, value.var="value_str")
          sectorset <- eqdt |> av_extract_df("sectors")
          if("weight" %in% sectorset) {
            sectorset <- dcast(sectorset[!is.na(sector),][,let(weight=100*weight)], sector ~ symbol,value.var="weight")
            sectorset <- sectorset[,let(category="sects",catprio=max(toplot$catprio)+1,prio=.I,format="")]
            setnames(sectorset,"sector","variable")
            toplot <- rbindlist(list(toplot, sectorset),use.names=TRUE,fill=TRUE)
          }
          tbl_loc <- fifelse(length(eqset)>3, "TABLE2GT","TABLE3GT")
          out[[tbl_loc]] <-  toplot |> gt.avtheme(themeset="eqdescsec")

          holdset <- eqdt |> av_extract_df("holdings")
          if("weight" %in% holdset) {
            holdset <- holdset[,.SD[order(-weight)][,let(n=.I-min(.I)+1, weight=100*weight)], by=.(symbol)]
            holdset <- dcast(holdset[n<=50,],n ~ symbol,value.var=c("description","weight"))
            holdsetcn <- data.table(nm=colnames(holdset)[-1])[,let(i=.I+1,symbol=s(nm,"_")[2]),by=.I][order(symbol,nm)]
            setcolorder(holdset, c(1,holdsetcn$i))
            out[["DET1GT"]] <- holdset |> gt.avtheme(themeset="etfholdings")
            quick_message("istr1","ETF holdings in DETAILS tab")
          }
        }
      }
      if(anopt1=="EQ:DivEarn") {
        fwddts <- extenddtstr(datestring,rtn="list",endchg=2*360)
        alleqs <- symbol_grep_by_type(eqlist1,"Equity")
        if(length(alleqs)>0) {
          allearn <- rbindlist(lapply(alleqs,\(x) oneticker_earns(x,fwddts,datestring)))
          lastqtr <- max(allearn[symbol==alleqs[[1]] & !is.na(reportedDate)]$fiscalDateEnding)
          lastqtr <- paste0(lubridate::year(lastqtr),"Q",lubridate::quarter(lastqtr))
          out[["TABLE1GT"]]<- allearn |> gt.avtheme(themeset="earnings")
          xout <- av_get_pf(alleqs[[1]],"EARNINGS_CALL_TRANSCRIPT",quarter=lastqtr) |>
                                av_extract_df("transcript")
          xout <- xout[,title:=fcase(grepl("Chief Executive|CEO",title),"CEO",grepl("Chief Financial|CFO",title),"CFO",grepl("Investor Relations",title),"InvRel",default=title)]
          xout <- xout |> gt.avtheme(themeset="earningstranscript",paste0(alleqs[[1]]," ",lastqtr))
          out[["DET1GT"]]<- xout
        }
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
        idxsearch <- the_av$tickerlist[grepl(istr1,name,ignore.case=TRUE) | grepl(istr1,symbol,ignore.case=TRUE),][,.(symbol,name,type="Index")]
        eqdta <- rbindlist(list(idxsearch[,format:="green"],eqdta),fill=TRUE,use.names=TRUE)
        out[["TABLE1GT"]] <- eqdta |> gt.avtheme(themeset="namesearch",istr1)
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
          inspots <- rbindlist(lapply(eqlist1,\(x) av_get_pf(x,"GLOBAL_QUOTE",melted=TRUE)))
          inspots <- inspots[variable=="price",.(symbol,spot=value_num)]
          indta <- inspots[indta,on=.(symbol)][,ncak:=1]
          filteredopts <- indta |> av_grep_opts(grepstring=ochains,mindelta=as.numeric(mindelta)/100)
          filteredopts <- filteredopts |> av_opt_helper_cols(scaling=oscaling)
          colstoshow <- data.table(showset=c("reduced","trading","all"),
                                   colstring=c("symbol;ncak;strike;type;daysExp;moneyn;mat_be;mat_be_pct;IV;mark;last;bo_pct;delta;vega;theta;contractID",
                                               "symbol;ncak;strike;daysExp;volume;open_interest;IV;delta;last;mark;bo_pct;bid;ask;bid_size_poi;ask_size_poi;contractID",
                                               paste0(names(filteredopts),collapse=";")))
          atmopts = indta[type=="call" & expiration<=Sys.Date()+60,][,.SD[which.min(abs(delta-0.5))],by=.(symbol,expiration)] |>
                  av_opt_helper_cols(scaling="none")

          out[["SCAT1"]] <- fg_scatplot(atmopts,"IV ~ daysExp + color:symbol",type="loessnofill",psize=3,title="ATM Term Structure")
          filteredopts[,type:=fifelse(type=="call","C","P")]
          filteredopts<- filteredopts[,.SD,.SDcols=s(colstoshow[showset==otodisplay,]$colstring)]
          out[["OPT1GT"]] <- filteredopts |> gt.avtheme(themeset="filteredopts", istr1, otodisplay)
        }
      }
      if(anopt1=="EQ:News") {
        u1<-lapply(s("newsgrep;minabssent;maxagedays;nArticles"),\(x) av_set_defaults(x,rv[[x]]))
        save_avs_state("all")
        allnews <- rbindlist(lapply(eqlist1,\(x)
                    getNews(x,nArticles=input$nArticles,minabssent=input$minabssent,newsfilter=input$newsfilter,
                              newsagrep=input$newsgrep,maxage=input$maxagedays)))
        if(newssort=="sentiment") { allnews<- allnews[order(symbol,-sntmt)] }
        if(newssort=="time,symbol") { allnews<- allnews[order(-time_published,symbol)] }
        if(newssort=="symbol,time") { allnews<- allnews[order(symbol, -time_published)] }
        out[["NEWSGT"]] <- allnews |> gt.avtheme(themeset="news",istr1)
        av_set_defaults("NEWSGT",out[["NEWSGT"]])
      }
    if(length(out)<=1) {
      quick_message("anopt1","Select an Action",eval=(anopt1=="SelectOption" || anopt2=="SelectOption"))
      quick_message("anopt1",paste0("NOT IMPLEMENTED YET:", anopt1, " or ",anopt2),eval=!(anopt1=="SelectOption" || anopt2=="SelectOption"))
    }
    # =============================================================================================================================================
    the_av$dyg1h <- fifelse( "dygraphs" %in% class(out[["TS1"]]), "600px","auto")
    the_av$dyg2h <- fifelse( "dygraphs" %in% class(out[["TS2"]]), "600px","auto")
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
    starttab <- fifelse(the_av$starttab %in% unique(avsd$deflist$focus), the_av$starttab, avsd$deflist[runcode==anopt1,]$focus )
    updateTabsetPanel(session,"inTabset",selected=starttab)
   }) # obsARUnn
  } # Server
  return(av_server)
}



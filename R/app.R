source("./R/utilities.R")
tver<-"0.8.201"

# 201: Generalize render, JS to only run on enter
# 20: WOrking copy
# 16: Fix scatter plotting, refactor ui names
# 145: Command line interface
# 14: Saving earnings and estimates: LOtsa plumbing, redid symset
# 135: Separate out inventory tab, start Plumbing for new functions

#' @importFrom TTR volatility
#' @import gt
#' @import gtExtras
#' @import data.table
#' @importFrom dygraphs dygraphOutput renderDygraph
#' @import shiny
#' @import shinyFeedback
#' @import FinanceGraphs
av_make_ui <- function() {
  order1=order2=aesnm=var=NULL
  curr_assetgroups <- sort(unique(the_av$assetgroups$listnm))
  av_ui<- fluidPage(
    shinyFeedback::useShinyFeedback(),
    shinyjs::useShinyjs(),
     tags$head(
       tags$script(HTML("
          $(document).on('keyup', '.enter-submit input', function(e) {
          if (e.key === 'Enter') {
              Shiny.setInputValue(this.id + '_enter', this.value, {priority: 'event'});
          }
        });
        ")),
        lapply(grepv("css",names(avsd)), \(x) tags$style(type='text/css',avsd[[x]])),
        lapply(avsd$table_aes[aesnm=="HTML"]$val_str, \(x) tags$style(HTML(x))),
     ),
     fluidRow(
       column(1,
            #actionButton("RUNLN","RUNLN",width='100%',class = "btn btn-primary"),
            selectizeInput("gropts","TS opts",
                           c("last","lastlabel","hilightfirst","splitts","hilow"),
                           selected=s(the_av$gropts),
                           multiple=TRUE,options(list(maxOptions=5,maxItems=1,avsd$selectizeoptions))),
            selectizeInput("scatopts","Scat opts",
                           c("last","tailhedge"),
                           selected=s(the_av$gropts),
                           multiple=TRUE,options(list(maxOptions=5,maxItems=1,avsd$selectizeoptions))),
            textInput(inputId="ts_events", label="Events", value = the_av$ts_events),
            textInput(inputId="dtstr_hist", label="HistDates", value=the_av$dtstr_hist)
       ),
       column(10,  # Was 11
          fluidRow(
            column(width=6,div(class = "enter-submit", textInput("istr1", "AVShiny Command", the_av$inpline1,width='100%'))),
            column(width=2,selectizeInput("list1","",c("AssetListnm"="", c("",sort(unique(the_av$assetgroups$listnm)))),size="80%",options=list(create=TRUE))),
            column(width=2,radioButtons("managelist1","",choices=c("get","save","delete"),selected =character(0),width="90%",inline=TRUE)),
            column(width=2,div(class = "enter-submit", textInput("istr2", "CounterAsset", the_av$inpline2,width='100%')))
          ),
          fluidRow(
            tabsetPanel(id="inTabset",selected=the_av$starttab,
              # Generic tab 1: Main: t1gt | t3l_gt + t3r_gt | t2gt | (dy)view1 | (dy)view2
              tabPanel("MAIN", value="main",
                  fluidRow(
                    textOutput("msg"),
                    gt_output(outputId = "t1gt"),
                    gt_output(outputId = "t2gt"),
                    div(class = "no-gap-row",
                        div(class = "table-pane", style="flex: 1", gt_output("t3lgt")),
                        div(class = "table-pane", style="flex: 1", gt_output("t3rgt")) ),
                    dygraphOutput("dy1"),
                    dygraphOutput("dy2"),
                    plotOutput(outputId="plot1"),
                    plotOutput(outputId="plot2"),
                  )
                  ),
              # Generic tab 2: Detail: DETGT1 | DETGT2 | plot1 | plot2
              tabPanel("DETAIL", value="detail",
                  fluidRow(
                    gt_output(outputId="d_t1gt"),
                    gt_output(outputId="d_t2gt"),
                    plotOutput(outputId="d_plot1"),
                    plotOutput(outputId="d_plot2"),
                  )
              ),
              # Other tabs
              tabPanel("INVENTORY",value="inventory",
                  actionButton("RefreshInv","RefreshInv",width='30%',class = "btn btn-primary"),
                  tabsetPanel(id="inv_tabset",selected="inv1",
                    tabPanel("Assets",value="inv1", gt_output(outputId="inv1") ),
                    tabPanel("AssetList",value="inv2", gt_output(outputId="inv2") )
                  )
              ),
              tabPanel("OPTIONS",value="options",
                fluidRow(
                  column(width=2,textInput(inputId="ochains", label="Default Chains",value=the_av$ochains)),
                  column(width=2,numericInput(inputId="mindelta", label="mindelta", value=5,min=0,max=100)),
                  column(width=2,selectInput(inputId="otodisplay", label="Output",
                                                 c("reduced","trading","all"),selected=the_av$otodisplay,multiple=FALSE)),
                  column(width=2,selectInput(inputId="oscaling", label="Scaling",
                                                 c("None","10contracts","10kMV"),selected=the_av$oscaling,multiple=FALSE))
                  ),
                  fluidRow(
                    gt_output(outputId="opt_t1gt"),
                    plotOutput(outputId="opt_plot1")
                  )
                ),
              tabPanel("NEWS",value="news",
                fluidRow(
                  column(width=2,
                    numericInput(inputId="nArticles", label="nArticles", value=the_av$nArticles,min=20,max=300),
                    selectInput(inputId="newssort",label="SortOn",c("time","sentiment","time,symbol","symbol,time"),selected=the_av$newssort,
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
              tabPanel("AVOPTS",value="avopts",
                  column(width=3,
                    actionButton("SetOpts","Set Opts",width='50%',class = "btn btn-primary"),
                    span(passwordInput(inputId="avapikey", label="av api key", value=the_av$avapikey),style=avsd$labelcss),
                    span(textInput(inputId="avapientitlement", label="av entitlement", value=the_av$avapientitlement),style=avsd$labelcss),
                    span(textInput(inputId="cachedir", label="Cache Data Directory", value=the_av$cachedir),style=avsd$labelcss),
                    #span(textInput(inputId="extracalc_file", label="extracalc csv", value=the_av$extracalc_file),style=avsd$labelcss), ## <<--- TODO
                    span(textInput(inputId="ts_colorset", label="fgts colorset", value=the_av$ts_colorset),style=avsd$labelcss),
                    textInput(inputId="ts_volparams", label="Histvolparams", value=the_av$ts_volparams),
                    selectInput(inputId="sigpct","Regr Significance", c("0.05","0.025","0.1"),selected=c("0.025"),multiple=FALSE),
                    span(textInput(inputId="av_dump_dir", label="AV dump Directory", value=the_av$av_dump_dir),style=avsd$labelcss),
                    selectInput(inputId="capture_av_what",label="Capture AV Data",c("none","pricesonly","noprices","all"),
                                selected=s(the_av$capture_av_what), multiple=FALSE),
                    selectInput(inputId="capture_av_update",label="Update or Cumulative",c("update","cum"),
                                selected=s(the_av$capture_av_update), multiple=FALSE),
                    selectInput(inputId="capture_av_save",label="Data Saving Options",c("none","CleanOnStart","SaveEveryAVCall","SaveNowOnOptUpdate"),
                                selected=the_av$capture_av_save, multiple=TRUE),
                    checkboxGroupInput(inputId="logopts",label="Options",choices=s(avsd$defaults[var=="avsh_logopts",]$value_str),
                                selected=s(the_av$logopts))
                  ),
                  column(width=7,gt_output(outputId = "dumpthe"))
                  )
            )
          )
      )
     ) #column
   ) #fluid ROw
  return(av_ui)
}

#' @importFrom stats cor
#' @importFrom shinyjs runjs
#' @importFrom splines bs
#' @importFrom patchwork wrap_plots
#' @importFrom stats quantile formula

av_make_server <- function() {
  ts_rebase=ts_events=ts_volparams=imp=x_close=y_close=NULL
  out <- list()
  av_server<-function(input, output,session) {
    inlist=list_ts=vartype=todofunc=todo=assetline=NULL
    curr_assetgroups <- sort(unique(the_av$assetgroups$listnm))
    quick_message("ochains","[F(ront)|B(ack)],[M(onth)|Q(tr)],[C(all)|P(ut)],[itm|otm|all]")
    # On Startup download current index list if not there
    update_tickerlists( is.null(the_av$tickerlist) || nrow(the_av$tickerlist)<=0 ||
            (min(the_av$tickerlist$list_ts)<=Sys.Date()-7) )
    FinanceGraphs::fg_sync_group("avshiny")
    if("CleanOnStart" %in% the_av$capture_av_save) {  save_av_data(data.table(),"KILL") }

   # height_from_obs <- reactive({ the_av$out1h })
    need_index_asset <- reactive({
      is_in_list <- s(input$istr2)[1] %in% the_av$pxinv$symbol
      shinyFeedback::feedbackWarning("is_in_list", !is_in_list, "(1) Need an asset in inventory to compare against")
    })
    set_list <- function(listtodo,tlist,instr,no) {
      assetline=todo=NULL
      rtnmsg <- ""
      parse_inpline(instr) # Makes todo;assetline
      if(listtodo=="save") {
        if(nchar(instr)<=0 | nchar(tlist)<=0) {
          rtnmsg <- "Cannot Save blank assetgroups Name"
        }
        else {
          newassets <- data.table(ticker=s(assetline))[,listnm:=tlist][]
          the_av$assetgroups <- DTUpsert(the_av$assetgroups,newassets,c("listnm"),replaceifbempty=the_av$assetgroups[!(listnm==tlist),])
          save_avs_state("all",msg="saveassets")
          updateSelectizeInput(session,"list1", choices=sort(unique(the_av$assetgroups$listnm)))
          updateSelectizeInput(session,"list2", choices=sort(unique(the_av$assetgroups$listnm)))
          rtnmsg <-paste0("Asset set saved as ",tlist)
        }
      }
      if(listtodo=="get") {
        newassets <- paste0( the_av$assetgroups[listnm==tlist,]$ticker,collapse=";")
        av_set_defaults(paste0("inpline",no), paste0(newassets," ",todo))
        save_avs_state("all",msg="getassets")
        updateTextInput(session,paste0("istr",no), value= the_av[[paste0("inpline",no)]])
      }
      if(listtodo=="delete") {
        the_av$assetgroups <- the_av$assetgroups[!listnm==tlist,]
        updateSelectizeInput(session,"list1", choices=sort(unique(the_av$assetgroups$listnm)))
        updateSelectizeInput(session,"list2", choices=sort(unique(the_av$assetgroups$listnm)))
        rtnmsg <-paste0("Deleted Asset List: ",tlist)
        save_avs_state("all",msg="deleteassets")
      }
      Sys.sleep(0.2)
      updateRadioButtons(session,paste0("managelist",no),selected=character(0))
      return(rtnmsg)
    }

    observeEvent(input$gropts, {
      req(input$gropts)
      av_set_defaults("gropts",paste0(input$gropts,sep=";"))
    })

    observeEvent(input$managelist1, {
      req(input$managelist1)
      quick_message("istr1",set_list(input$managelist1,input$list1,input$istr1,1))
    })

    observeEvent(input$managelist2, {
      req(input$managelist2)
      quick_message("istr2",set_list(input$managelist2,input$list2,input$istr2,2))
    })

    observeEvent(input$SetOpts, {
      old=toget=NULL
      rv <- isolate(reactiveValuesToList(input))
      th1<- dump_the()
      oldcache <- th1[nm=="cachedir",]$toget
      av_api_key(rv$avapikey,rv$avapientitlement)
      av_set_default_set("setopts",rv)
      if( nchar(newcache<-av_validate_directory(rv$cachedir,"cachedir"))>0 ) {
        if(!(newcache==oldcache)) {
          message_if_red(TRUE,"Cache directory moved; cleaning up old price/inventory data from ",oldcache)
          lapply(avsd$defaults[vartype=="cache",]$value_str, \(x) unlink(paste0(oldcache,"/",x),force = TRUE))
          av_set_defaults("cachedir",newcache)
        }
        oldcache <- newcache
      }
      av_set_defaults("cachedir",oldcache)
      av_set_caching_directories() # constants_fn always has to be in tmp directory:
      if( nchar(newcache<- av_validate_directory(rv$av_dump_dir,"av_dump_dir"))>0 ) {
          av_set_defaults("av_dump_dir",newcache)
      }
      av_set_defaults("starttab","main")
      av_set_defaults("logopts",paste0(rv$logopts,collapse=";",sep=";"))
      av_set_defaults("verbose", "verbose" %in% rv$logopts)
      av_set_defaults("autocopy","data2clipboard" %in% rv$logopts)
      save_avs_state("all",msg="sEToPTS")
      th1 <- th1[,.(nm,old=toget)][dump_the(),on=.(nm)][,format:=fifelse(old==toget,"","yellow")][]
      th1 <- th1[,.SD,.SDcols=s("classtype;nm;toget;format")]
      quick_message("istr1","No data in inventory; load or ask for some via PriceTS", eval=(nrow(th1)<=0))
      output$dumpthe <- render_gt(th1 |> gt() |> gt.basetheme(interactive="filter") |> decorate_table())
    })

    observe({ # Want executed at startup
      if(input$RefreshInv==1 || exists("do_on_start",envir=the_av)) {
        if( !quick_message(eval=(nrow(the_av$pxinv)<=0),"istr1","No INventory: Create Data by running a Time Series Graph") ) {
          invtosend <- the_av$pxinv[,.SD,.SDcol=!s("earnf_next;div_lastval;lastearn_dt;earnf_nextdt;earnf_ts")]
          output$inv1 <- invtosend[,age:=Sys.Date()-end_dt] |> gt.avtheme(themeset="pxinv") |> render_gt() #  gt.avtheme(themeset="pxinv") |>
          output$inv2 <- dump_assetgroups(returngt=TRUE) |>  gt.avtheme(themeset="assetgroups") |> render_gt()
        }
        the_av$starttab <- "INVENTORY"
        the_av$do_on_start <- NULL
        message_if_green(the_av$verbose,"Inventory on way to tab")
        updateTabsetPanel(session,"inTabset",selected=the_av$starttab)
      }
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

    observeEvent(!(input$capture_av_save==the_av$capture_av_save), {
      if("SaveNowOnOptUpdate" %in% input$capture_av_save) {
        save_av_data(data.table(),"SaveNowOnOptUpdate")
        updateSelectInput(session,"capture_av_save",selected=the_av$capture_av_save)
      }
      })

    observeEvent(input$istr1_enter, {
      rv <- isolate(reactiveValuesToList(input))
      thisenv <- environment()
      if(the_av$avapikey=="NOT_SET") {
        lapply(s("istr1;istr2"), \(x) quick_message(x,"SET Alphavantage API key"))
        return()
      }
      message_if(the_av$verbose,"avrs(",tver,") >>>> input(",rv$istr1,") Line2:",rv$istr2)
      # Recopy older items, but not everything
      out <- lapply(grepv("TS",names(out)), \(x) out[[x]])

      parse_inpline(toupper(rv$istr1)) # New variables created:  todo todofunc todoargs assetline
      runfunc_set <-  the_av$avsh_funcs[runcode==s(todofunc," ")[[1]],]
      quick_message("istr1",fifelse(nrow(runfunc_set)<=0,paste(todo,":Invalid choice"),""))
      if(nrow(runfunc_set)<=0) { return() }
      # Set defaults
      av_set_defaults("starttab",tolower(runfunc_set[[1,"focus"]]))
      av_set_defaults("inpline1",rv$istr1)
      av_set_defaults("inpline2",rv$istr2)
      av_set_default_set("onrun",rv,save="the")
      rv$istr1 <- assetline

      rv$seriesnm <- av_set_defaults("seriesnm", fifelse(grepl("useTotRtn",the_av$logopts),"adjusted_close","close"))
      rv$uselive <- av_set_defaults("uselive",grepl("useLivePx",the_av$logopts))
      avsh_set_tabtitle(makefocus=FALSE)

      # General Magick here:
      #cAssign("todo;rv",silent=TRUE)
      tenv <- thisenv
      if(runfunc_set$func_src=="user") { tenv<-.GlobalEnv() }
      outres <- do.call(runfunc_set$func_name, list(todo,rv), envir=tenv)
      outres <- setNames(outres,av_determine_output_locs(outres))

      for(nm in names(outres)) { out[[nm]]<-outres[[nm]] } # hash w/o hash

      # Save outputs, May turn off
 #     names_to_cp <- grepv(fifelse("persistOutput" %in% the_av$logopts,"*","TS"), names(out) )
#      av_set_defaults("outcopy",out[names_to_cp])

      # Render unto Caesar, several types, except null text
      # Had to do this at one point:the_av$dyg1h <- fifelse( "dygraphs" %in% class(out[["TS1"]]), "600px","auto") , then send to renderDygraphs
      torend <- avsd$avsh_element[data.table(outname=names(out)),on=.(outname)]
      mapply( \(outnm,innm,intype) {
        local({
          output[[outnm]]<-switch(gsub("::","",intype),
                                  gt_tbl = render_gt(out[[innm]]),
                                  dygraphs = renderDygraph(out[[innm]]),
                                  ggplot2ggplot = renderPlot({ out[[innm]] },execOnResize=TRUE),
                                  text = renderPrint( out[[innm]] ))
        })},
        torend$ui_out, torend$outname, torend$inclass   )
      updateTabsetPanel(session,"inTabset",selected=the_av$starttab)
      save_avs_state("all",msg="RUNLN")
    })
  } # Server
  return(av_server)
}


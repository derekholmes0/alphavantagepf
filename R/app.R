#source("./R/utilities.R")
tver<-"0.8.437"

# 438: Documentation, summary in options search
# 437: OPtion Search done
# 436: Seasoanlity works, Weekends options
# 435: Refactor live prices
# 434: Last  user index fix.  assetnames needed caps
# 432: av.inv(grep),user index fix
# 431: Seasonality, appbreviations
# 430: Add correlatiosn to RV
# 420: Refactored Asset List UI
# 410: Add file timestamps to inventory file to check for new data
# 400: CHange to quick_message, vignettes done, av.inputs

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
            textInput("istr2", "CounterAsset", the_av$inpline2,width='100%'),
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
            column(width=8,
                   fluidRow(
                   div(class = "enter-submit", textInput("istr1", paste("AVShiny",tver), the_av$inpline1,width='100%'))),
                   div(class = "msgcopy", textOutput("msg"))
            ),
            column(width=2,selectizeInput("assetgp_list","AssetGroups",c("AssetListnm"="", c("",sort(unique(the_av$assetgroups$listnm)))),size="80%",options=list(create=TRUE))),
            column(width=2,selectInput("ag_state","",c("--","Expand","Save","Delete"),size=4,selectize=FALSE)),
          ),
          fluidRow(
            tabsetPanel(id="inTabset",selected=the_av$starttab,
              tabPanel("MAIN", value="main",
                  fluidRow(
                    htmlOutput("htm1"),
                    gt_output(outputId = "t1gt"),
                    gt_output(outputId = "t2gt"),
                    div(class = "no-gap-row",
                        div(class = "table-pane", style="flex: 1", gt_output("t3lgt")),
                        div(class = "table-pane", style="flex: 1", gt_output("t3rgt")) ),
                    uiOutput("dy1_container"),
                    uiOutput("dy2_container"),
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
                  tabsetPanel(id="inv_tabset",
                    tabPanel("Assets",value="inv1", gt_output(outputId="inv1") ),
                    tabPanel("AssetList",value="inv2", gt_output(outputId="inv2") )
                  )
              ),
              tabPanel("OPTIONS",value="options",
                fluidRow(
                  column(width=3,textInput(inputId="ochains", label="Default Chains",value=the_av$ochains)),
                  column(width=2,numericInput(inputId="omindelta", label="omindelta", value=the_av$omindelta,min=0,max=100)),
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
                    numericInput(inputId="maxagedays", label="Maximum News Age (Days)", value=the_av$maxagedays,min=0),
                    ),
                  column(width=8,
                    gt_output(outputId = "newsgt")
                    )
                  )
                ),
              tabPanel("AVOPTS",value="avopts",
                  column(width=2,
                    actionButton("SetOpts","Set Opts",width='50%',class = "btn btn-primary"),
                    span(passwordInput(inputId="avapikey", label="av api key", value=the_av$avapikey),style=avsd$labelcss),
                    span(textInput(inputId="avapientitlement", label="av entitlement", value=the_av$avapientitlement),style=avsd$labelcss),
                    span(textInput(inputId="cachedir", label="Cache Data Directory", value=the_av$cachedir),style=avsd$labelcss),
                    #span(textInput(inputId="extracalc_file", label="extracalc csv", value=the_av$extracalc_file),style=avsd$labelcss), ## <<--- TODO
                    span(textInput(inputId="ts_colorset", label="fgts colorset", value=the_av$ts_colorset),style=avsd$labelcss),
                    span(textInput(inputId="av_dump_dir", label="AV dump Directory", value=the_av$av_dump_dir),style=avsd$labelcss),
                    selectInput(inputId="capture_av_what",label="Capture AV Data",c("none","pricesonly","noprices","all"),
                                selected=s(the_av$capture_av_what), multiple=FALSE),
                    selectInput(inputId="capture_av_update",label="Update or Cumulative",c("update","cum"),
                                selected=s(the_av$capture_av_update), multiple=FALSE),
                    selectInput(inputId="capture_av_save",label="Data Saving Options",c("none","CleanOnStart","SaveEveryAVCall","SaveNowOnOptUpdate"),
                                selected=the_av$capture_av_save, multiple=TRUE),
                  ),
                  column(width=2,
                    numericInput(inputId="maxage_earn_days", label="Max Earnings Age (Days)", value=the_av$maxage_earn_days,min=0),
                    numericInput(inputId="maxage_px_hrs", label="Max Price Age (Hours)", value=the_av$maxage_px_hrs,min=0),
                    textInput(inputId="ts_volparams", label="Histvolparams", value=the_av$ts_volparams),
                    selectInput(inputId="sigpct","Regr Significance", c("0.05","0.025","0.1"),selected=c("0.025"),multiple=FALSE),
                    checkboxGroupInput(inputId="logopts",label="Options",choices=s(avsd$defaults[var=="avsh_logopts",]$value_str),
                                            selected=s(the_av$logopts))
                  ),
                  column(width=6,gt_output(outputId = "dumpthe"))
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
  wh=ts_rebase=ts_events=ts_volparams=imp=x_close=y_close=ui_out=outname=displayed=inclass=displayheight=todoargs=NULL
  out <- list()
  av_server<-function(input, output,session) {
    inlist=list_ts=vartype=todofunc=todo=assetline=NULL
    curr_assetgroups <- sort(unique(the_av$assetgroups$listnm))
    # On Startup download current index list if not there
    update_tickerlists( is.null(the_av$tickerlist) || nrow(the_av$tickerlist)<=0 ||
            (max(the_av$tickerlist$list_ts)<=Sys.Date()-4) )
    FinanceGraphs::fg_sync_group("avshiny")
    if("CleanOnStart" %in% the_av$capture_av_save) {  save_av_data(data.table(),"KILL") }
   # height_from_obs <- reactive({ the_av$out1h })
    need_index_asset <- reactive({
      is_in_list <- s(input$istr2)[1] %in% the_av$pxinv$symbol
      shinyFeedback::feedbackWarning("is_in_list", !is_in_list, "(1) Need an asset in inventory to compare against")
    })

    dyheight1 <- reactive({ the_av$renderset[ui_out=="dy1",]$displayheight})
    dyheight2 <- reactive({ the_av$renderset[ui_out=="dy2",]$displayheight})

    observeEvent(input$gropts, {
      req(input$gropts)
      av_set_defaults("gropts",paste0(input$gropts,sep=";"))
    })

    observeEvent(input$ochains, {
      req(input$ochains)
      quick_message(opt_explation(input$ochains),wh="ochains")
    })

    observeEvent(input$ag_state, {
      req(input$ag_state)
      if(input$ag_state=="--") { return() }
      quick_message(set_list(input$ag_state,input$assetgp_list,input$istr1,session))
    })

    observeEvent(input$assetgp_list, {
      req(input$assetgp_list)
      if(length(setdiff(input$assetgp_list,curr_assetgroups))<=0) { # New Asset LIst Name
        parse_inpline(input$istr1) # Makes todo;assetline
        av_set_defaults("inpline1", paste0(input$assetgp_list," ",todo))
        updateTextInput(session,"istr1", value= the_av[["inpline1"]])
      }
    })

    observeEvent(input$SetOpts, {
      old=toget=NULL
      oldcache <- the_av$cachedir
      rv <- isolate(reactiveValuesToList(input))
      th1<- dump_state()
      avpf_api_key(rv$avapikey,rv$avapientitlement)
      av_set_default_set("setopts",rv)
      newcache<-av_validate_directory(rv$cachedir,"cachedir")
      if( nchar(newcache<-av_validate_directory(rv$cachedir,"cachedir"))>0 ) {
        if(!(newcache==oldcache)) {
          message_if_red(TRUE,"Cache directory moved; cleaning up old price/inventory data from ",oldcache)
          sapply(avsd$defaults[vartype=="cache",]$value_str, \(x) unlink(paste0(oldcache,"/",x),force = TRUE))
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
      thnew <- dump_state()
      th1 <- th1[,.(nm,old=toget)][thnew,on=.(nm)][,format:=fifelse(old==toget,"","yellow")][]
      th1 <- th1[,.SD,.SDcols=s("nm;classtype;toget;format")]
      quick_message("No data in inventory; load or ask for some via PriceTS", eval=(nrow(th1)<=0))
      th1 <- th1[nm=="avapikey",toget:="<< redacted >>"]
      output$dumpthe <- render_gt(th1 |> gt() |> gt.basetheme(interactive="filter") |> decorate_table())
    })

    observe({ # Want executed at startup
      if(input$RefreshInv==1 || exists("do_on_start",envir=the_av)) {
        if( !quick_message(eval=(nrow(the_av$pxinv)<=0),"No INventory: Create Data by running a Time Series Graph") ) {
          invtosend <- the_av$pxinv[,.SD,.SDcol=!s("earnf_next;div_lastval;lastearn_dt;earnf_nextdt;earnf_ts")]
          output$inv1 <- invtosend[,age:=Sys.Date()-end_dt] |> gt.avtheme(themeset="pxinv") |> render_gt() #  gt.avtheme(themeset="pxinv") |>
          output$inv2 <- dump_assetgroups() |>gt.avtheme(themeset="assetgroups") |> render_gt()
        }
        the_av$starttab <- "inventory"
        if( exists("do_on_start",envir=the_av) ) {
          rm("do_on_start",envir=the_av) }
        else {
          message_if_green(the_av$verbose,"Inventory on way to tab")
          updateTabsetPanel(session,"inTabset",selected=the_av$starttab)
        }
      }
    })

    observeEvent(input$capture_av_what, {
      req(input$capture_av_what)
      txtset <- data.table(wh=s("none;pricesonly;noprices;all"),txt=s("No Data Saving;Only prices captured;Only non price data captured;All Data captured"))
      feedtxt <- txtset[wh==input$capture_av_what,]$txt
      if(!("No " %chin% feedtxt)) {
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
      newts <- fifelse(file.exists(the_av$inv_fn), as.POSIXct( file.info(the_av$inv_fn)$mtime), Sys.time())
      if(!grepl("^av",rv$istr1,ignore.case=TRUE)) {
        the_av$cmdhist <- rbindlist(list(the_av$cmdhist,data.table(cmd=rv$istr1,ts=Sys.time())), fill=TRUE,use.names=TRUE)
      }
      thisenv <- environment()
      if( quick_message("SET Alphavantage API key",eval=the_av$avapikey=="NOT_SET") |
          quick_message("Enter a valid command", eval=nchar(rv$istr1)<=0) ) {
        return()
      }
      message_if(the_av$verbose,"avrs(",tver,") >>>> input(",rv$istr1,") Line2:",rv$istr2, " invts:",newts)
      # Clear all but TS graphs
      the_av$user_feedback <- ""
      out <- list()
      outcopy <- the_av$outcopy %||% list()
      # reload data if necessary
      #message("  reload check: newts= ",newts," oldts: ",the_av$inv_fn_ts)
      if(newts>the_av$inv_fn_ts) {
        message_if_red(TRUE,"RELOADING DATA updated outside the app at ",newts)
        restore_avs_state(msg="reload")
        the_av$inv_fn_ts<-newts
      }
      # ----------------
      # New variables created and added to rv:  todo todofunc todoargs assetline
      parse_inpline(toupper(rv$istr1))  # NEw 26-08-15: Expand assetgrouplists
      rv <- c(rv,setNames(list(todo,todofunc,todoargs,assetline), s("todo;todofunc;todoargs;assetline"))) # Augment rv
      # out for Production, IN for testing
      out[["MSG"]]<- rv$istr1_enter
      #cAssign("todo;todofunc;rv;todoargs;assetline",silent=TRUE)
      runfunc_set <-  the_av$avsh_funcs[runcode==todofunc,]
      quick_message(fifelse(nrow(runfunc_set)<=0,paste(todo,":Invalid function"),""))
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

      tenv <- thisenv
      if( runfunc_set$func_src=="user" ) { tenv <-  .GlobalEnv }
      if( !exists(runfunc_set$func_name,envir=tenv)) {
        quick_message(paste0("Function Code not found for ",runfunc_set$func_name),color="red")
        return()
      }
      # ---- General Magick here:
      outres <- do.call(runfunc_set$func_name, list(todo,rv), envir=tenv)
      # -----
      # Commands returned
      if("CMD" %in% names(outres)) {
        tcmd <- s(outres[["CMD"]],":")
        newcmd<-""
        if(tcmd[[1]]=="toinput") { newcmd<-tcmd[[2]] }
        if(tcmd[[1]]=="clear") {  output<-list(); the_av$outcopy<-list() } # Not working
        updateTextInput(session,"istr1", value= newcmd)
        return()
      }

      if(!quick_message("Invalid ticker or analysis, check logs",color="red", eval=length(outres)<=0)) {
        outres <- setNames(outres,av_determine_output_locs(outres))
        for(nm in names(outres)) { out[[nm]]<-outres[[nm]] } # hash w/o hash
      }
      # Final Message
      quick_message(the_av$user_feedback,eval=nchar(the_av$user_feedback)>0)

      # Save outputs ONLY if another graph is being asked for OR persistOut is TRUE
      outcopy_grepstr <- fcase("persistOutput" %in% the_av$logopts,"*",grepl("^G",todo),"TS", default="NoMatch")
      outcopy_names <- setdiff(grepv(outcopy_grepstr, names(outcopy)), names(outres))
      for(nm in outcopy_names) { out[[nm]]<-outcopy[[nm]]  }
      torend <- copy(avsd$avsh_element) # Replaces everything
      torend <- torend[outname %in% names(out),displayed:=TRUE][inclass=="dygraphs",displayheight:=fifelse(displayed,"400px","0px")]
      av_set_defaults("outcopy",out)
      av_set_defaults("renderset",torend)
      mapply( \(outnm,innm,intype) {
        local({
          output[[outnm]]<-switch(gsub("::","",intype),
                                  gt_tbl = render_gt(out[[innm]]),
                                  dygraphs = renderDygraph(out[[innm]]),
                                  ggplot2ggplot = renderPlot({ suppressWarnings(out[[innm]]) },execOnResize=TRUE),
                                  text = renderText( out[[innm]] ))
        })},
        torend$ui_out, torend$outname, torend$inclass   )

      # Dygraph heights tricky.
      output$dy1_container <- renderUI({ dygraphOutput("dy1", height= torend[ui_out=="dy1",]$displayheight) })
      output$dy2_container <- renderUI({ dygraphOutput("dy2", height= torend[ui_out=="dy2",]$displayheight) })
      updateTabsetPanel(session,"inTabset",selected=the_av$starttab)
      save_avs_state("all",msg="RUNLN")
      updateTextInput("istr1",value="",session=session)
    })
  } # Server
  return(av_server)
}


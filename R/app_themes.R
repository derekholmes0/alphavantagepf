# =======================================================================================================
#' App formatting functions
#'
#' @name gt.basetheme
#' @description Adds default elements to [gt()] table
#' @param x Input data or `gt` table
#' @param gtopts Which elements to add
#' @param sizepct (default: 70) How big to make tables
#' @param style (default: 4) Style number (see [gt()])
#' @param size (default: "") Size of text, either `""` or `"small"`
#' @param digits (default: 2) Number formatting
#' @param seps (default: FALSE) Number formatting thousands separators
#' @param na_format (default: "-") WHat to show for NAs
#' @param interactive (default: `FALSE`) Add interactive elements

#' @import gt
#' @import data.table

gt.basetheme<-function(x,gtopts="all",sizepct=100,style=4,digits=2,seps=FALSE,na_format="-",size="",interactive=FALSE) {
  style <- fifelse(interactive==TRUE,1,style)
  if(gtopts=="all" | gtopts=="fmtnumber") {
    x = x |> tab_style_body(style=cell_text(color="red"),columns=where(is.numeric),fn=function(x) x<0) |> fmt_number(accounting=TRUE,decimals = digits,use_seps=seps)
    x = x |> sub_missing(missing_text=na_format)
  }
  if(gtopts=="all" | gtopts=="theme") {
    x = x  |> opt_stylize(style=style) |> tab_options(table.font.size=sprintf("%2.0f%%",sizepct))
  }
  if(gtopts=="all" | gtopts=="padding") {
    x = x |> opt_vertical_padding(scale=0.6)
  }
  if(interactive==TRUE) {
    x = x |> opt_interactive(use_filters=TRUE,use_resizers=TRUE, page_size_default=70,use_compact_mode=TRUE)
  }
  if(grepl("wide",size) | interactive==TRUE) {
    x = x |> tab_options(table.width=pct(95),table.align="left",table.margin.left=px(0))
  }
  if(!grepl("smalltext",size)) {
    x = x |> opt_table_font(stack = "rounded-sans")
  }
  return(x)
}

#' @noRd
decorate_table <- function(gtx) {
  return(gtx |>
           tab_style(style=cell_text(size="xx-small"),locations=cells_body(rows=(format=="xx-small"))) |>
           tab_style(style=cell_text(size="x-small"),locations=cells_body(rows=(format=="x-small"))) |>
           tab_style(style=cell_text(weight="bold"),locations=cells_body(rows=(format=="bold"))) |>
           tab_style(style=cell_fill(color="yellow"),locations=cells_body(rows=(format=="yellow"))) |>
           tab_style(style=cell_fill(color="green"),locations=cells_body(rows=(format=="green"))) |>
           fmt_url(rows=(format=="fmt_url")) |>
           cols_hide(columns=c(format))
  )
}

#' @import gt
#' @import data.table
gt.avtheme<- function(x,themeset="",...) {
  term=`i.to`=estimate_Beta=p.value_Beta=loadts=n=catprio=prio=matchScore=daysExp=nlink=sntmt=time_published=estiamtedEPS=NULL
  est_low=est_high=est_n=est_30dpchg=est_90dpchg=divdays=estimatedEPS=volume=low=high=chgpct=EH_chgpct=age=EH_mid=isah=inlist=NULL
  ldots = list(...)
  # -- Tables that require further processing
  if(themeset=="activeregression") {
    x <- x[.(term=c("(Intercept)","x"), to=c("a","Beta")),on="term",term:=i.to]
    x <- x |> dcast(regfactor ~ term,value.var=setdiff(colnames(x),s("regfactor;term")))
    setnames(x,"regfactor","ticker")
    x <- x |> gt() |> tab_spanner_delim(delim="_") |> gt.basetheme(gtopts="theme") |>
      tab_style(style=cell_text(color="red",weight="bold"),
                locations=cells_body(columns=estimate_Beta, rows=p.value_Beta<=as.numeric(ldots[[2]]))) |>
      tab_header(title=paste0("Return regression vs ",ldots[[1]])) |>
      tab_footnote(paste("Estimated highlighted at p=",100*as.numeric(ldots[[2]]), "pct as of",Sys.time()))
    return(x)
  }
  if(themeset=="live") {
    # x = av_get_pf(c("ORCL","IBM","EWZ","ARGT"),"REALTIME_BULK_QUOTES",melt=FALSE)
    setnames(x, s("change_percent;extended_hours_quote;extended_hours_change;extended_hours_change_percent;previous_close"),
              s("chgpct;EH_mid;EH_chg;EH_chgpct;prevclose"),skip_absent=TRUE)
    x[,let(age=timestamp-Sys.time(), volume=volume/10^6)]
    x[,let(chgfropenbp=10000*(close/open-1), lowbpopen=10000*(low/open-1), hibpopen=10000*(high/open-1))]
    x[,let(isah=is.na(chgpct), chgpct = fcoalesce(chgpct,EH_chgpct))]
    thisgt = x |> gt() |> gt.basetheme(interactive=TRUE,size="wide") |>
      tab_spanner_delim(delim="_") |> fmt_duration(columns=age,output_units=c("days", "hours", "minutes")) |>
      cols_move_to_start(s("symbol;chgpct;close;age")) |>
      tab_style(style=cell_text(color="red",weight="bold"), locations=cells_body(columns=c(close,EH_mid), rows=chgpct<0)) |>
      tab_style(style=cell_text(color="blue",weight="bold"), locations=cells_body(columns=c(close,EH_mid), rows=chgpct>=0)) |>
      tab_style(style=cell_text(weight="bold"), locations=cells_body(columns=c(chgpct))) |>
      tab_style(style=cell_fill(color="pink"), locations=cells_body(columns=c(symbol,chgpct), rows=(isah==TRUE))) |>
      cols_hide(columns=c(timestamp,prevclose,lowbpopen,hibpopen,isah)) |>
      cols_width(age ~ px(90), everything() ~ px(60)) |>
      tab_footnote("Extended hours in pink, prices are colored by direction from previous close")
    if("inlist" %in% names(x)) {
      thisgt = thisgt |>
        tab_style(style=cell_fill(color="lightgreen"), locations=cells_body(columns=everything(),  rows=(inlist==TRUE))) |>
        cols_hide(s("inlist"))
    }
    return(thisgt)
  }
  # If a gt =============================================================
  if (!"gt_tbl" %in% class(x)) {
    if(nrow(x)<=0) {
      return(x)
    }
    x <- x |> gt()
  }
  # savelist ============================================================= savelist
  if(themeset=="savelist") {
    x <- x |> gt.basetheme() |> tab_header(title="Asset lists")
  }
  # Inventory ============================================================= Inventory
  if(themeset=="assetlist") {
    x <- x |> gt.basetheme() |> tab_header(title="Asset lists")
  }
  if(themeset=="pxinv") {
    x <- x |> tab_header(title="Data Inventory") |>
      fmt_datetime(columns=loadts,date_style="y.mn.day",time_style="iso-short") |>
      tab_style(style=cell_fill(color="pink"), locations=cells_body(columns=c(symbol,type,currency,age), rows=(as.numeric(age)>=3))) |>
      tab_footnote(paste("Data older than 3 days highlighted")) |>
      gt.basetheme()
      # todo: color rows that are out of date
  }
  if(themeset=="mktstatus") {
    x <- x |> gt.basetheme() |> tab_header(title="Market Status") |> tab_footnote(paste("Retrieved as of ",Sys.time()))
  }
  # TS:ActiveTS ============================================================= TS:ActiveTS
  if(themeset=="tr_regression") {
    x <- x |> gt.basetheme() |> tab_header(title="Daily Total Return regression results", subtitle=html(paste0("Asset vs ",ldots[[1]])))
  }
  # Gen:Movers ============================================================= Gen:Movers
  if(themeset=="Gen:Movers") {
    x <- x |> gt.basetheme() |> tab_spanner_delim(delim = "_",reverse=TRUE) |>
          tab_header(title="US Movers") |> tab_footnote(paste("As Of",ldots[[1]])) |>
          fmt_integer(columns=n)
  }
  # EQ:DES ============================================================= EQ:DES
  if(themeset=="eqdesc1") {
    x <- x |> gt.basetheme() |>
      row_order(catprio,prio) |> decorate_table() |> cols_hide(columns=c(catprio,prio)) |>
      tab_header(title="Equities") |> tab_footnote(paste("As Of",Sys.time())) |>
      fmt_number(suffixing=TRUE) |>
      cols_width(category ~ px(50), variable ~ px(150), everything() ~ px(120)) |>
      gt.basetheme()
  }
  if(themeset=="eqdescsec") {
    x <- x |> row_order(catprio,prio) |> decorate_table() |> cols_hide(columns=c(catprio,prio)) |>
      cols_width(category ~ px(50), variable ~ px(150), everything() ~ px(100)) |>
      fmt_number(suffixing=TRUE) |>
      gt.basetheme()
  }
  if(themeset=="etfholdings") {
    x <- x |> gt.basetheme() |> tab_spanner_delim(delim = "_",reverse=TRUE) |>
              fmt_number(suffixing=TRUE) |>
              tab_header(title="ETF Holdings") |> tab_footnote(paste("retrieved as of",Sys.time()))
  }
  # Gen:NameSearch ============================================================= Gen:NameSearch
  if(themeset=="namesearch") {
    x <- x |> gt.basetheme() |> fmt_percent(columns=matchScore,decimals=1)  |>
              decorate_table() |> tab_header(title=paste0("Search for ",ldots[[1]]))
  }
  # EQ:OptSearch=============================================================EQ:OptSearch
  if(themeset=="filteredopts") {
    x <- x |>
        gt.basetheme(interactive=TRUE,size="wide") |>
        tab_header(title=paste0("Opts for ",ldots[[1]]), subtitle=paste0("Columns shown: ",ldots[[2]]," set")) |>
        tab_footnote(paste("retrieved as of",Sys.time())) |>
        fmt_integer(columns=c(daysExp)) |>
        cols_merge(columns=c(symbol,type,daysExp), pattern = "{1} {3}d {2}") |>
        cols_width(symbol ~ px(90), contractID ~ px(150), everything() ~ px(70))
  }
  if(themeset=="earnings") {
    x<- x |> gt.basetheme(digits=2,interactive=TRUE) |>
    cols_merge(columns=c(estimatedEPS,est_low,est_high), pattern = "<<{2}: >>{1}<<: {3}>>") |>
    tab_style(style=cell_text(align="center"),locations=cells_body(columns=estimatedEPS)) |>
    fmt_number(columns=est_n,decimals=0) |> fmt_percent(columns=c(est_30dpchg,est_90dpchg),decimals=1) |>
    cols_width(reportTime ~ px(100),estimatedEPS ~ px(100), everything() ~ px(80)) |>
    tab_header(title=paste0("Earnings")) |>
    fmt_number(suffixing=TRUE) |>
    gt.basetheme()
  }
  if(themeset=="earningstranscript") {
    x<- x |> gt.basetheme(size="widesmalltext") |>
      tab_style(style=cell_text(color="blue"), locations=cells_body(columns=c(title,content), rows=(title=="Analyst"))) |>
      tab_style(style=cell_text(color="black",weight="bold"), locations=cells_body(columns=c(title,content), rows=(title=="CEO"))) |>
      tab_style(style=cell_text(color="gray"), locations=cells_body(columns=c(title,content), rows=(title=="Operator"))) |>
      cols_move_to_start(s("sentiment;title;content")) |>
      tab_header(title=paste0("Earnings transcript for ",ldots[[1]]))
  }
  if(themeset=="dividends") {
    x<- x |> gt.basetheme(digits=2,interactive=TRUE) |>  fmt_number(columns=divdays,decimals=0) |>
      tab_header(title=paste0("Dividends"))
  }
  # Gen:News============================================================= Gen:News
  if(themeset=="news") {
    ff1 <- function(x) { paste0('<a href="',x,'" target="_blank">LK</a>') }
    ff2 <- function(x) { paste0('<a href="#" onclick="window.open(\'', x, '\', \'_blank\'); return false;">','LK','</a>')   }
    thisgt <- x |>
      fmt_url(columns=url,label=icon("link"),color="blue") |>
      tab_style(locations=cells_body(columns=title),style=(size="x-small")) |>
      tab_header(title=paste0("News for ",ldots[[1]])) |> tab_footnote(paste("retrieved as of",Sys.time())) |>
      data_color(columns=sntmt,method = "numeric",palette = c("red","white", "green")) |>
      # data_color(columns=age,method="bin",bins=c(0,3,7,90),palette="Blues",reverse=TRUE) |>
      cols_move_to_start(s("symbol;age;sntmt;url;title")) |>
      fmt_datetime(columns=time_published,date_style="y.mn.day",time_style="iso-short") |>
      fmt_duration(columns=age,output_units=c("days", "hours", "minutes")) |>
      cols_width(time_published ~ px(100), source ~ px(120), url ~ px(35), age ~ px(90), title ~ px(400), everything() ~ px(40)) |>
      gt.basetheme()
    return(thisgt)
  }

  return(x)
}

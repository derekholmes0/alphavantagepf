# =======================================================================================================
#' App formatting functions
#'
#' @name gt.basetheme
#' @description Adds default elements to [gt()] table
#' @param x Input data or `gt` table
#' @param gtopts Which elements to add
#' @param sizepct (default: 70) How big to make tables
#' @param style (default: 4) Style number (see [gt()])
#' @param digits (default: 2) Number formatting
#' @param seps (default: FALSE) Number formatting thousands separators
#' @param na_format (default: "-") WHat to show for NAs
#' @param interactive (default: `FALSE`) Add interactive elements

#' @import gt
#' @import data.table

gt.basetheme<-function(x,gtopts="all",sizepct=100,style=4,digits=2,seps=FALSE,na_format="-",interactive=FALSE) {
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
    x = x |> opt_interactive(use_filters=TRUE,page_size_default=70,use_compact_mode=TRUE)
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
  est_low=est_high=est_n=est_30dpchg=est_90dpchg=divdays=estimatedEPS=NULL
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
  if (!"gt_tbl" %in% class(x)) {
    if(nrow(x)<=0) {
      return(x)
    }
    x <- x |> gt()
  }
  x <- x |> gt.basetheme()
  # savelist ============================================================= savelist
  if(themeset=="savelist") {
    x <- x |> tab_header(title="Asset lists")
  }
  # Inventory ============================================================= Inventory
  if(themeset=="assetlist") {
    x <- x |> tab_header(title="Asset lists")
  }
  if(themeset=="pxinv") {
    x <- x |> tab_header(title="Data Inventory") |>
      fmt_datetime(columns=loadts,date_style="y.mn.day",time_style="iso-short")
      # todo: color rows that are out of date
  }
  if(themeset=="mktstatus") {
    x <- x |> tab_header(title="Market Status") |>
      tab_footnote(paste("Retrieved as of ",Sys.time()))
  }
  # TS:ActiveTS ============================================================= TS:ActiveTS
  if(themeset=="tr_regression") {
    x <- x |> tab_header(title="Daily Total Return regression results", subtitle=html(paste0("Asset vs ",ldots[[1]])))
  }
  # Gen:Movers ============================================================= Gen:Movers
  if(themeset=="Gen:Movers") {
    x <- x |> tab_spanner_delim(delim = "_",reverse=TRUE) |>
          tab_header(title="US Movers") |> tab_footnote(paste("As Of",ldots[[1]])) |>
          fmt_integer(columns=n)
  }
  # EQ:DES ============================================================= EQ:DES
  if(themeset=="eqdesc1") {
    x <- x |>
      row_order(catprio,prio) |> decorate_table() |> cols_hide(columns=c(catprio,prio)) |>
      tab_header(title="Equities") |> tab_footnote(paste("As Of",Sys.time())) |>
      cols_width(category ~ px(50), variable ~ px(150), everything() ~ px(120))
  }
  if(themeset=="eqdescsec") {
    x <- x |> row_order(catprio,prio) |> decorate_table() |> cols_hide(columns=c(catprio,prio)) |>
      cols_width(category ~ px(50), variable ~ px(150), everything() ~ px(100))
  }
  if(themeset=="etfholdings") {
    x <- x |> tab_spanner_delim(delim = "_",reverse=TRUE) |>
              tab_header(title="ETF Holdings") |> tab_footnote(paste("retrieved as of",Sys.time()))
  }
  # Gen:NameSearch ============================================================= Gen:NameSearch
  if(themeset=="namesearch") {
    x <- x |> fmt_percent(columns=matchScore,decimals=1)  |>
              decorate_table() |> tab_header(title=paste0("Search for ",ldots[[1]]))
  }
  # EQ:OptSearch=============================================================EQ:OptSearch
  if(themeset=="filteredopts") {
    x <- x |>
        gt.basetheme(interactive=TRUE) |>
        tab_header(title=paste0("Opts for ",ldots[[1]]), subtitle=paste0("Columns shown: ",ldots[[2]]," set")) |>
        tab_footnote(paste("retrieved as of",Sys.time())) |>
        cols_width(symbol ~ px(50), contractID ~ px(135), everything() ~ px(70)) |>
        fmt_number(columns=daysExp, decimals=0)
  }
  # Gen:News============================================================= Gen:News
  if(themeset=="news") {
    x <- x |>
      fmt_markdown(columns=nlink) |>
      tab_style(locations=cells_body(columns=nlink),style=(size="x-small")) |>
      tab_header(title=paste0("Opts for ",ldots[[1]])) |> tab_footnote(paste("retrieved as of",Sys.time())) |>
      data_color(columns=sntmt,method = "numeric",palette = c("red","white", "green")) |>
      fmt_datetime(columns=time_published,date_style="y.mn.day",time_style="iso-short") |>
      cols_width(time_published ~ px(100), source ~ px(120), nlink ~ px(400), everything() ~ px(40))
  }
  if(themeset=="earnings") {
    x<- x |> gt.basetheme(digits=2,interactive=TRUE) |>
    cols_merge(columns=c(estimatedEPS,est_low,est_high), pattern = "<<{2}: >>{1}<<: {3}>>") |>
    tab_style(style=cell_text(align="center"),locations=cells_body(columns=estimatedEPS)) |>
    fmt_number(columns=est_n,decimals=0) |> fmt_percent(columns=c(est_30dpchg,est_90dpchg),decimals=1) |>
    cols_width(reportTime ~ px(100),estimatedEPS ~ px(100), everything() ~ px(80)) |>
    tab_header(title=paste0("Earnings"))
  }
  if(themeset=="dividends") {
    x<- x |> gt.basetheme(digits=2,interactive=TRUE) |>  fmt_number(columns=divdays,decimals=0) |>
      tab_header(title=paste0("Dividends"))
  }
  return(x)
}

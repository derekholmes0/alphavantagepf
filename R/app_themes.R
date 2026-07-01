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
#' @param interactive (default: "") String with interative elements: contains one of `(all|filter|search)`

#' @import gt
#' @import data.table
gt.basetheme<-function(x,gtopts="all",sizepct=70,style=4,digits=2,seps=FALSE,na_format="-",size="",interactive="") {
  if(nchar(interactive)>0) {
    style <- 1
    sizepct <- 100
  }
  if(gtopts=="all" | gtopts=="fmtnumber") {
    x = x |> tab_style_body(style=cell_text(color="red"),columns=where(is.numeric),fn=function(x) x<0) |> fmt_number(accounting=TRUE,decimals = digits,use_seps=seps)
    x = x |> sub_missing(missing_text=na_format)
  }
  if(gtopts=="all" | gtopts=="theme") {
    x = x  |> opt_stylize(style=style) |> opt_table_font(stack = "rounded-sans",size=sprintf("%2.0f%%",sizepct))
  }
  if(gtopts=="all" | gtopts=="padding") {
    x = x |> opt_vertical_padding(scale=0.6)
  }
  if(nchar(interactive)>0) {
    x = x |> opt_interactive(use_filters=grepl("all|filter",interactive),
                             use_search =grepl("all|search",interactive),
                             use_resizers=grepl("all|filter",interactive),
                             page_size_default=40,use_compact_mode=TRUE) |>
                tab_options( table.font.size = px(12))
  }
  if(grepl("wide",size) |  nchar(interactive)>0) {
    x = x |> tab_options(table.align="left",table.margin.left=px(0))
  }
  if(!grepl("smalltext",size)) {
    x = x |> opt_table_font(stack = "rounded-sans",size=sprintf("%2.0f%%",sizepct))
  }
  return(x)
}

#' @noRd
decorate_table <- function(gtx) {
  return(gtx |>
           tab_style(style=cell_text(size="xx-small"),locations=cells_body(rows=grepl("xx-small",format))) |>
           tab_style(style=cell_text(size="x-small"),locations=cells_body(rows=grepl("x-small",format))) |>
           tab_style(style=cell_text(weight="bold"),locations=cells_body(rows=grepl("bold",format))) |>
           tab_style(style=cell_fill(color="lightyellow"),locations=cells_body(rows=grepl("yellow",format))) |>
           tab_style(style=cell_fill(color="lightgreen"),locations=cells_body(rows=grepl("green",format))) |>
           fmt_url(rows=(format=="fmt_url")) |>
           cols_hide(columns=c(format))
  )
}

#' @noRd
#' @importFrom rlang expr sym
#' @importFrom purrr map2
add_colwidths <- function(gtx,xtablenm) {
  tablenm=aesnm=colname=NULL
  colset <- avsd$table_aes[tablenm==xtablenm & aesnm=="width",]
  if(nrow(colset)<=0) {
    return(gtx)
  }
  colnodef <- colset[!colname=="default",]
  width_list <- map2( colnodef$colname, colnodef$val_num, ~rlang::expr(!!rlang::sym(.x) ~ px(!!.y)) )
  if(nrow(coldef <- colset[colname=="default",])>0) {
    default_width <- coldef[,.SD[1]]$val_num
    width_list <- c(width_list, list(rlang::expr(everything() ~ px(!!default_width))))
  }
  gtx = gtx |> cols_width(.list = width_list)
  return(gtx)
}

#' @import gt
#' @import data.table
gt.avtheme<- function(x,themeset="",...) {
  term=`i.to`=estimate_Beta=p.value_Beta=loadts=n=catprio=prio=matchScore=nlink=sntmt=time_published=estiamtedEPS=NULL
  est_low=est_high=est_n=est_30dpchg=est_90dpchg=divdays=estimatedEPS=volume=low=high=chgpct=EH_chgpct=age=EH_mid=isah=inlist=thisgt=NULL
  tablenm=aesnm=regfactor=estimate=p.value=beg_dt=NULL
  ntable_len<-0
  ldots = list(...)
  # -- Tables that require further processing
  if(themeset=="activeregression") {
    nterms = max(x[,.N,by=.(regfactor)]$N)
    if(nterms==2) {
      x <- x[.(term=c("(Intercept)","x"), to=c("a","Beta")),on="term",term:=i.to]
      x <- x |> dcast(regfactor ~ term,value.var=setdiff(colnames(x),s("regfactor;term")))
      setnames(x,"regfactor","ticker")
      thisgt <- x |> gt() |> tab_spanner_delim(delim="_") |> gt.basetheme(gtopts="theme")
      thisgt = thisgt |> tab_style(style=cell_text(color="red",weight="bold"),
                                   locations=cells_body(columns=estimate_Beta, rows=p.value_Beta<=as.numeric(ldots[[2]])))
    }
    else {
      thisgt <- x |> gt() |> tab_spanner_delim(delim="_") |> gt.basetheme(gtopts="theme")
      thisgt = thisgt |> tab_style(style=cell_text(color="red",weight="bold"),
                                   locations=cells_body(columns=estimate, rows=p.value<=as.numeric(ldots[[2]])))
    }
    thisgt = thisgt |>
      tab_header(title=paste0("Return regression vs ",ldots[[1]])) |>
      tab_footnote(paste("Estimated highlighted at p=",100*as.numeric(ldots[[2]]), "pct as of",Sys.time()))
    return(thisgt)
  }
  if(themeset=="live") {
    # x = av_get_pf(c("ORCL","IBM","EWZ","ARGT"),"REALTIME_BULK_QUOTES",melt=FALSE)
    setnames(x, s("change_percent;extended_hours_quote;extended_hours_change;extended_hours_change_percent;previous_close"),
              s("chgpct;EH_mid;EH_chg;EH_chgpct;prevclose"),skip_absent=TRUE)
    x[,let(age=timestamp-Sys.time(), volume=volume/10^6)]
    x[,let(chgfropenbp=10000*(close/open-1), lowbpopen=10000*(low/open-1), hibpopen=10000*(high/open-1))]
    x[,let(isah=is.na(chgpct), chgpct = fcoalesce(chgpct,EH_chgpct))]
    thisgt = x |> gt() |> gt.basetheme(interactive="all",size="wide") |>
      tab_spanner_delim(delim="_") |> fmt_duration(columns=age,output_units=c("days", "hours", "minutes")) |>
      cols_move_to_start(s("symbol;chgpct;close;age")) |>
      tab_style(style=cell_text(color="red",weight="bold"), locations=cells_body(columns=c(close,EH_mid), rows=chgpct<0)) |>
      tab_style(style=cell_text(color="blue",weight="bold"), locations=cells_body(columns=c(close,EH_mid), rows=chgpct>=0)) |>
      tab_style(style=cell_text(weight="bold"), locations=cells_body(columns=c(chgpct))) |>
      tab_style(style=cell_fill(color="pink"), locations=cells_body(columns=c(symbol,chgpct), rows=(isah==TRUE))) |>
      cols_hide(columns=c(timestamp,prevclose,lowbpopen,hibpopen,isah)) |>
      add_colwidths("live") |>
      tab_footnote("Extended hours in pink, prices are colored by direction from previous close")
    if("inlist" %in% names(x)) {
      thisgt = thisgt |>
        tab_style(style=cell_fill(color="lightgreen"), locations=cells_body(columns=everything(),  rows=(inlist==TRUE))) |>
        cols_hide(s("inlist"))
    }
  }
  # =============================================================
  # If a gt =============================================================
  # =============================================================
  if (is.null(thisgt)) {
    thisgt <- x |> gt()
  }
  # savelist ============================================================= savelist
  if(themeset=="savelist") {
    thisgt <- thisgt |> gt.basetheme() |> tab_header(title="Asset lists")
  }
  # Inventory ============================================================= Inventory
  if(themeset=="assetgroups") {
    thisgt <- thisgt |> gt.basetheme() |> tab_header(title="Asset lists")
  }
  if(themeset=="pxinv") {
    thisgt <- thisgt |> tab_header(title="Data Inventory") |> gt.basetheme(interactive="all") |>
      fmt_datetime(columns=loadts,date_style="Md",time_style="iso-short") |>
      tab_style(style=cell_fill(color="pink"), locations=cells_body(columns=c(symbol,name,type,currency,age), rows=(as.numeric(age)>=3))) |>
      tab_style(style = cell_text(size = px(10)),locations = cells_body(columns = c(loadts))) |>
      tab_footnote(paste("Data older than 3 days highlighted")) |>
      cols_hide(columns=c("list_ts")) |>
      cols_move_to_start(s("type")) |>
      fmt_datetime(columns=c(beg_dt,end_dt),format="y.mn.d") |>
      cols_merge(columns=c(beg_dt,end_dt), pattern = "{1}::{2}") |>
      add_colwidths("pxinv")
  }
  if(themeset=="mktstatus") {
    thisgt <- thisgt |> gt.basetheme() |> tab_header(title="Market Status") |> tab_footnote(paste("Retrieved as of ",Sys.time()))
  }
  # TS:ActiveTS ============================================================= TS:ActiveTS
  if(themeset=="tr_regression") {
    thisgt <- thisgt |> gt.basetheme() |> tab_header(title="Daily Total Return regression results", subtitle=html(paste0("Asset vs ",ldots[[1]])))
  }
  # Gen:Movers ============================================================= Gen:Movers
  if(themeset=="Gen:Movers") {
    thisgt <- thisgt |> gt.basetheme() |> tab_spanner_delim(delim = "_",reverse=TRUE) |>
          tab_header(title="US Movers") |> tab_footnote(paste("As Of",ldots[[1]])) |>
          fmt_integer(columns=n)
  }
  # EQ:DES ============================================================= EQ:DES
  if(themeset=="eqdesc1") {
    #thisgt <- x |> gt(groupname_col="category",row_group_as_column=TRUE) just not formatted right
    thisgt <- thisgt |> gt.basetheme(interactive="all") |>
      row_order(catprio,prio) |> decorate_table() |> cols_hide(columns=c(catprio,prio)) |>
      tab_header(title="Equities") |> tab_footnote(paste("As Of",Sys.time())) |>
      fmt_number(suffixing=TRUE) |>
      add_colwidths("eqdisc1")
  }
  if(themeset=="eqdescsec") {
    #thisgt <- x |> gt(groupname_col="category",row_group_as_column=TRUE) |> gt.basetheme()
    thisgt <- thisgt |> gt.basetheme(interactive="all") |> row_order(catprio,prio) |>
      decorate_table() |> cols_hide(columns=c(catprio,prio)) |>
      tab_header(title="ETF") |>
      add_colwidths("eqdescsec") |>
      fmt_number(suffixing=TRUE)
  }
  if(themeset=="etfholdings") {
    thisgt <- thisgt  |> gt.basetheme(style=1) |> tab_spanner_delim(delim = "_",reverse=TRUE) |>
              fmt_number(suffixing=TRUE) |>
              tab_header(title="ETF Holdings") |> tab_footnote(paste("retrieved as of",Sys.time()))
  }
  # Gen:NameSearch ============================================================= Gen:NameSearch
  if(themeset=="namesearch") {
    thisgt <- thisgt |> gt.basetheme() |> fmt_percent(columns=matchScore,decimals=1)  |>
              decorate_table() |> tab_header(title=paste0("Search for ",ldots[[1]]))
    if(ntable_len>40) {
      thisgt <- thisgt |> opt_interactive(use_search=TRUE,use_resizers=TRUE, page_size_default=70,use_compact_mode=TRUE)
    }
  }
  # EQ:OptSearch=============================================================EQ:OptSearch
  if(themeset=="filteredopts") {
    thisgt <- thisgt  |>
        gt.basetheme(interactive="filter",size="wide") |>
        tab_header(title=paste0("Opts for ",ldots[[1]]), subtitle=paste0("Columns shown: ",ldots[[2]]," set")) |>
        tab_footnote(paste("retrieved as of",Sys.time())) |>
        fmt_integer(columns=c(daysExp)) |>
        cols_hide(columns=c(daysExp,type)) |>
        # kills search capablity: cols_merge(columns=c(symbol,type,daysExp), pattern = "{1} {3}d {2}") |>
        add_colwidths("filteredopts")
  }
  if(themeset=="earnings") {
    thisgt <- thisgt  |> gt.basetheme(digits=2,interactive="search") |>
    cols_merge(columns=c(estimatedEPS,est_low,est_high), pattern = "<<{2}: >>{1}<<: {3}>>") |>
    tab_style(style=cell_text(align="center"),locations=cells_body(columns=estimatedEPS)) |>
    fmt_number(columns=est_n,decimals=0) |> fmt_percent(columns=c(est_30dpchg,est_90dpchg),decimals=1) |>
    add_colwidths("earnings") |>
    tab_header(title=paste0("Earnings")) |>
    fmt_number(suffixing=TRUE) |>
    gt.basetheme()
  }
  if(themeset=="earningstranscript") {
    thisgt <- thisgt  |> gt.basetheme(size="wide") |>
      tab_style(style=cell_text(color="blue"), locations=cells_body(columns=c(title,content), rows=(title=="Analyst"))) |>
      tab_style(style=cell_text(color="black",weight="bold"), locations=cells_body(columns=c(title,content), rows=(title=="CEO"))) |>
      tab_style(style=cell_text(color="gray"), locations=cells_body(columns=c(title,content), rows=(title=="Operator"))) |>
      cols_move_to_start(s("sentiment;title;content")) |>
      tab_header(title=paste0("Earnings transcript for ",ldots[[1]]))
  }
  if(themeset=="dividends") {
    thisgt <- thisgt  |> gt.basetheme(digits=2,interactive="filter") |>  fmt_number(columns=divdays,decimals=0) |>
      tab_header(title=paste0("Dividends"))
  }
  # Gen:News============================================================= Gen:News
  if(themeset=="news") {
    ff1 <- function(x) { paste0('<a href="',x,'" target="_blank">LK</a>') }
    ff2 <- function(x) { paste0('<a href="#" onclick="window.open(\'', x, '\', \'_blank\'); return false;">','LK','</a>')   }
    thisgt <- thisgt |>
      fmt_url(columns=url,label="Link",color="blue") |>
      tab_header(title=paste0("News for ",ldots[[1]])) |> tab_footnote(paste("retrieved as of",Sys.time())) |>
      data_color(columns=sntmt,method = "numeric",palette = c("red","white", "green")) |>
      # data_color(columns=age,method="bin",bins=c(0,3,7,90),palette="Blues",reverse=TRUE) |>
      cols_move_to_start(s("symbol;age;sntmt;url;title")) |>
      fmt_datetime(columns=time_published,date_style="y.mn.day",time_style="iso-short") |>
      fmt_duration(columns=age,output_units=c("days", "hours", "minutes")) |>
      add_colwidths("news") |>
      gt.basetheme(interactive="all")
  }
  # Does not work with opt_interactive  tab_style(locations=cells_body(columns=title),style=(size="x-small")) |>
  if(nrow(avsd$table_aes[tablenm==themeset & aesnm=="autocopy",])>0) {
    avsh_clipboard(x,themeset)
  }
  return(thisgt)
}

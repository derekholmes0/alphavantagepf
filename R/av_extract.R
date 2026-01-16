#' Extract data from Alpha Vantage returned data
#'
#' @name av_extract_df
#' @description
#' `av_extract_df()` pulls out nested data.frames from mixed data returned by  [av_get_pf()]
#' `av_extract_fx()` returns a simplified FX quote in data.table formfrom [av_get_pf()] calls.
#' `av_extract_analytics()` returns melted data.table from calls to `av_get_pf("ANALYTICS_FIXED_WINDOW")` or `av_get_pf("ANALYTICS_SLIDING_WINDOW")`
#'
#' @param indta A data.table as returned by av_get()
#' @param grepstring select which variable (data item) to unnest in data.table returned from av_get_pf
#' @param melt Return data in melted/normalized form
#' @param separate_vars (default : FALSE)  separate out multiple levels of variable names into new keys
#'
#' @returns Extracted data.tables for nested data returned from [av_get_pf()], If `grepstring` is not specified, first nested table is returned. [av_extract_fx()] returns a shortened data.table with FX quotes.
#'
#' @details [av_get_pf()] frequently returns a nested data.table, or a structure with nested data.frames.  These are utilities functions to extract, filter and summarize returned values.
#'
#' @seealso [av_get_pf()], [av_grep_opts()]
#'
#' @examples
#' \dontrun{
#' av_get_pf("","MARKET_STATUS")  |> av_extract_df()
#' av_get_pf("","TOP_GAINERS_LOSERS") |> av_extract_df("top_losers")
#' av_get_pf("USD/BRL","CURRENCY_EXCHANGE_RATE") |> av_extract_fx()
#' av_get_pf(c("ORCL","IBM"),"ANALYTICS_FIXED_WINDOW") |> av_extract_analytics(separate_vars=TRUE)
#' }
#'
#' @rdname av_extract_df
#' @export
av_extract_df <- function(indta,grepstring="",melt=FALSE) {  # Keep symbol, variable
    indta <- indta[which(indta$ltype=="list"),][grepl(grepstring,get("variable")),]
    outdlist <- lapply(seq(1,nrow(indta)),
        \(i) {
            dtax <- data.table::data.table(indta[i,]$value_df[[1]])
            dtax <- suppressMessages(readr::type_convert(dtax,na=c("","NA","None")))
            dtax[,'symbol':=indta[i,]$symbol]
            if(melt) {
                dtax <- melt_tobasetype(dtax)
                dtax[,'variable_p1':=indta[i,]$variable]
            }
            return(dtax)
        }
    )
    outd <- data.table::rbindlist(outdlist,use.names=TRUE,fill=TRUE)
    return(outd)
}

#' @rdname av_extract_df
#' @export
av_extract_fx <- function(indta) {
    thissymbol <- indta[1,]$symbol
    fxquote <- data.table::dcast(indta[get("ltype")=="numeric"],symbol ~ variable,value.var="value_str")
    fxquote <- fxquote[,.(`symbol`=thissymbol,`Ask`=as.numeric(get("Ask Price")),`Bid`=as.numeric(get("Bid Price")),`QuoteTimestamp`=as.POSIXct(get("Last Refreshed")))]
    fxquote <- fxquote[,':='(`Mid`=(get("Ask")+ get("Bid"))/2)]
    return(fxquote[])
}

#' @rdname av_extract_df
#' @export
av_extract_analytics <- function(indta,separate_vars=FALSE) {
  dt_1 <- lapply(names(indta), \(x) data.table::data.table(unlist(indta[[x]]),keep.rownames=TRUE))
  dt_2 <- data.table::rbindlist(dt_1)
  colnames(dt_2) = c("variable","value")
  if(separate_vars) {
    dt_3 <- tidyr::separate_wider_delim(dt_2,"variable",delim=".",names_sep="_",too_few="align_start")
    dt_2 <- data.table::data.table(dt_3)
  }
  return(dt_2)
}


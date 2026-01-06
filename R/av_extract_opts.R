#' Extract data from Alpha Vantage returned data
#'
#' @name av_grep_opts
#'
#' @param indta A data.table as returned by [av_get_pf()]
#' @param grepstring (default `F,M,call`) Three item string to select maturities and option types from `indta`.  Each item is an abbreviated code for what to select.
#'  * How far out to go: `F` for first contract (e.g. front week), `B` for second/back contract, anything else for all contracts.
#'  * Maturity Schedule: `M` for monthly contracts, `Q` for quarterly contract, anythign else for weekly contracts.
#'  * Option Type: `call`, `put`, or `all`
#' @param deltarange (default `c(0.1,0.55)`) Two number list with minimum and maximum absolute valued deltas to pass through.
#' @param mindays (default 3). Minimum number of days to expiration to be passed through from `startdt`
#' @param startdt (default `Sys.Date()`). Date from which expirations will be considered.
#'
#' @returns A reduced set of options obtained from [av_get_pf()] using `HISTORICAL_OPTIONS` function.
#'
#' @details [av_get_pf()] returns a large list of options.  This function helps to narrow down the list by maturity and moneyness.
#'
#' @seealso [av_get_pf()]
#'
#' @examples
#' \dontrun{
#' av_get_pf("IBM","HISTORICAL_OPTIONS") |> av_grep_opts("F,M,put",mindays=2)
#'#' }
#'
#' @export
av_grep_opts<-function(indta, grepstring="F,M,call",mindays=3,startdt=Sys.Date(),deltarange=c(0.1,0.55)) {
  #"F(ront)|W(eek),[call|put|both]", deltarange=c(mindelta,maxdelta)
  # Narrow down options data based on expiration code and delta range
  grepopts <- strsplit(tolower(grepstring),",")[[1]]
  optypes <- data.table::data.table('type'=c(data.table::fifelse(grepl("^(call|all)$",grepopts[3]),"call",NA_character_), data.table::fifelse(grepl("^(put|all)$",grepopts[3]),"put",NA_character_)))
  optypes <- optypes[!is.na('type')]
  matset <- dtmap[data.table::data.table(expiration=unique(indta$expiration)),on=.(DT_ENTRY==expiration)][,.('expiration'=DT_ENTRY,'optexp'=paste0(get("optexp"),"wk"))]
  matset <- matset[grepl(grepopts[2],get("optexp")) & expiration>=(startdt+mindays),]
  matindex <- seq(1,nrow(matset))
  if ( grepopts[1]=="f" ) { matindex<-c(1) }
  if ( grepopts[1]=="b" ) { matindex<-c(2) }
  toget <- indta[data.table::SJ(matset[matindex],optypes[!is.na(type),]),on=.(expiration,type)]
  toget <- toget[data.table::between(abs(delta),deltarange[1],deltarange[2])]
  toget <- toget[,.SD,.SDcols=!c("optexp")]
  return(toget)
}

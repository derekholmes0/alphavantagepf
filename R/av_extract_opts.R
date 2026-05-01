#' Extract data from Alpha Vantage returned data
#'
#' @name av_grep_opts
#'
#' @param indta A data.table as returned by [av_get_pf()]
#' @param grepstring (default `F,M,call,otm,act`) Three to five item string to select specific maturities, option types,  strike ranges, and
#' open interest categories from option sets returned by `av_get_pf(.,"HISTORICAL_OPTIONS")`. Each item in the list is an
#' abbreviated code for what to select. The items are not case sensitive and in order are
#' |Item|Values|description|
#' |:--------------|:---------|:---------------|
#' |Expiration Limit|`[F|B|A]`|`F` is first maturity, `B` is second maturity, `A` (pr blank) are all|
#' |Maturity Type|`[M|Q|A]`|`M` for monthly contracts, `Q` for quarterly,  `A` (pr blank) are all|
#' |Option Type|`[C|P|A]`|Calls, puts, `all` (or blank) for all|
#' |Moneyness|`[otm|itm|A]`|Out of the money, in the money or both|
#' |Activity|`[act|A]`|If `A`, only pass contracts with positive open interest|
#' @param mindelta (default `0.05`) delta limit on both moneyness sides, i.e. pass only options with deltas in range c(`mindelta`,1-`mindelta`)
#' @param spot (default `NULL`) Spot to be used to determine itm/otm, If null then it is inferred from most out of the money call or put
#' Note: This parameter only applies if there is one symbol in `indta`.  If there is more than one ticker in `indta` a column `spot`
#' must be in `indta` to get correct results.
#' @param mindays (default 3). Minimum number of days to expiration to be passed through from `startdt`
#' @param startdt (default `Sys.Date()`). Date from which expirations will be considered.
#' @param dropsymbol (default `FALSE`). Drop symbol from returned data table.
#'
#' @returns A reduced set of options obtained from [av_get_pf()] using Alphavantage `HISTORICAL_OPTIONS` function.
#'
#' @details [av_get_pf()] returns a large list of options.  This function helps to narrow down the list by maturity and moneyness.
#'
#' @seealso [av_get_pf()]
#'
#' @examples
#' \dontrun{
#' av_get_pf("IBM","HISTORICAL_OPTIONS") |> av_grep_opts("F,M,put",mindays=2)
#' }
#'
#' @export
av_grep_opts<-function(indta, grepstring="F,M,C,otm",spot=NULL,mindays=3,startdt=Sys.Date(),mindelta=0.05,dropsymbol=FALSE) {
  mark=strike=open_interest=NULL
  #"F(ront)|W(eek),[call|put|both]", deltarange=c(mindelta,maxdelta)
  # Narrow down options data based on expiration code and delta range
  gopts <- c(sapply(s(tolower(grepstring),","), \(x) substr(x,1,1)),rep("x",5))[1:5]
  # Option types 3
  optypes <- data.table(type=c(fifelse(gopts[3]=="p","","call"),fifelse(gopts[3]=="c","","put")))[nchar(type)>0,]
  # Maturities
  matgrep <- fcase(gopts[2]=="m","mo",gopts[2]=="q","qtr",default="wk")
  matset <- dtmap[data.table(expiration=unique(indta$expiration)),on=.(DT_ENTRY==expiration)][,.('expiration'=DT_ENTRY,'optexp'=paste0(get("optexp"),"wk"))]
  matset <- matset[grepl(matgrep,get("optexp")) & expiration>=(startdt+mindays),]
  matindex <- seq(1,nrow(matset))
  # FM/BM
  if ( gopts[1]=="f" ) { matindex<-c(1) }
  if ( gopts[1]=="b" ) { matindex<-c(2) }
  toget <- indta[SJ(matset[matindex],optypes[!is.na(type),]),on=.(expiration,type)]
  # otm/itm
  if( !("spot" %in% colnames(indta))) {
      spot <- spot %||% indta[type=="call",][,.SD[1]][,.(spot=mark+strike)]$spot
  } # Does the right thing
  if( gopts[4]=="i" ) { toget=toget[(fifelse(type=="call",1,-1)*(spot-strike))>=0,] }
  if( gopts[4]=="o" ) { toget=toget[(fifelse(type=="call",1,-1)*(spot-strike))<=0,] }
  # activity
  if( gopts[5]=="a" ) { toget=toget[open_interest>0,] }
  toget <- toget[between(abs(delta),mindelta,1-mindelta),]
  toget <- toget[,.SD,.SDcols=!patterns(fifelse(dropsymbol==TRUE,"optexp|symbol","optexp"))]
  message_if(the$verbose,"av_grep_opts(",grepstring,") narrows ",nrow(indta)," options to ",nrow(toget)," using spot: ",spot)
  return(toget)
}

#' Add additional data to returned option sets
#'
#' @name av_opt_helper_cols
#' @param indta An option data.table as returned by [av_get_pf()]
#' @param scaling (default `NULL`) Scaling factor for marks, last values, and greeks.  Options are
#' * `NULL` or `"none"` : No Scaling
#' * `"10contracts"` : 10 contracts converted into market value
#' * `"10kMV"` : 10,000 USD converted into equivalent market value
#' * `number` : Any numeric value in thousands of USD into equivalent market value
#' @param spot (default `NULL`) Spot to be used to determine itm/otm, If null then it is inferred from most out of the money call or put
#' Note: This parameter only applies if there is one symbol in `indta`.  If there is more than one ticker in `indta` a column `spot`
#' must be in `indta` to get correct results.
#' @returns An option `data.table` with extra columns helpful for further analysis
#' @details Adds columns including
#' |Column|Definition|
#' |:---------:|:-------------------------|
#' |`daysExp`|Days to Expiration|
#' |`bo_pct`|Bid offer in percent of option mid|
#' |`bid_size_poi`|Bid size percent of open interest|
#' |`ncak`|Notional number of contracts|
#' @seealso [av_grep_opts()]
#' @examples
#' \dontrun{
#' av_get_pf("IBM","HISTORICAL_OPTIONS") |> av_grep_opts("F,M,put",mindays=2) |> av_opt_helper_cols()
#' }
#'
#' @export
av_opt_helper_cols<-function(indta, scaling=NULL, spot=NULL) {
  mark=strike=ask=bid=implied_volatility=bid_size=open_interest=ncak=NULL
  if( !("spot" %in% colnames(indta))) {
      spot <- spot %||% indta[type=="call",][,.SD[1]][,.(spot=mark+strike)]$spot %||% indta[type=="put",][,.SD[.N]][,.(spot=strike-mark)]$spot
  }
  indta <- indta[,let(daysExp=as.integer(expiration-date),bo_pct=200*(ask-bid)/(ask+bid),IV=100*implied_volatility,
                      bid_size_poi=100*bid_size/fcoalesce(open_interest,1L), ask_size_poi=100*bid_size/fcoalesce(open_interest,1L),
                      strikepctspot=100*(strike/spot-1),moneyn=fifelse(type=="call",1,-1)*(spot-strike),
                      ncak=1)]
  if (is.null(scaling) || tolower(scaling)=="none") {
    message_if(the$verbose,"av_opt_helper_cols using Spot ",spot,", and date ",max(indta$date))
    return(indta)
  }
  qcols <- s("mark;last;delta;gamma;theta;vega;rho")
  indta <- indta[,let(ncak=fcase(
    scaling=="10contracts", 10,
    scaling=="10kMV", round(10000/(100*mark),0),
    is.numeric(scaling), round(as.numeric(scaling)*1000/(100*mark),0),
    default = 1)
  ), by=.I]
  indta <- indta[,(qcols):=lapply(.SD,\(x) ncak*100*x), .SDcols=qcols][]
  message_if(the$verbose,"av_opt_helper_cols using Spot ",spot,", scaling ",scaling, " and date ",max(indta$date))
  return(indta)
}


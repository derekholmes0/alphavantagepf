#' Get financial data from the Alpha Vantage API
#'
#' @name av_get_pf
#' @description Interface to alphavantage API.
#'
#'
#' @param symbol A character string of an appropriate stock, fund, or currency
#' See parameter "symbol" in [Alpha Vantage API documentation](https://www.alphavantage.co/documentation/).
#' @param av_fun A character string matching an appropriate Alpha Vantage "function".
#' See parameter "function" in [Alpha Vantage API documentation](https://www.alphavantage.co/documentation/).
#' @param symbolvarnm (default: `symbol`) Variable name which has the `symbol` requested.  Set to a blank string
#' @param dfonerror (default: TRUE) Return an empty data.table when any error occurs
#' @param verbose (default: FALSE) Print debug information helpful for errors
#' @param melt  (default: TRUE) Return molten output.
#' @param ... Additional parameters or overrides passed to the Alpha Vantage API.
#' For a list of parameters, visit the [Alpha Vantage API documentation](https://www.alphavantage.co/documentation/).
#'
#' @returns Returns a data.table with results dependent on the function called.
#' Mixed data is returned as a melted data.table, possibly with nested data.frames.  Time series are returned as data.tables.
#'
#' @seealso [av_api_key()], [av_extract_df()], [av_extract_fx()], [av_grep_opts()],[av_funhelp()]
#'
#' @details
#'
#' __The `av_fun` argument replaces the API parameter “function”__ because function is a reserved name in R. All other arguments match the Alpha Vantage API parameters.
#'
#' __There is no need to specify the `apikey`, `datatype`, or `outputsize` parameters__ as arguments to av_get_pf(). Before using, you must set the API key using av_api_key("YOUR_API_KEY"). `outputsize` defaults to "full" unless overridden with "compact in `...`."
#'
#' __Required parameters other than `symbol`__ must be passed as named arguments via `...`.
#'
#' __Optional parameters have defaults__ which can be obtained  by calling  [av_funhelp()] and overridden via `...`.
#'
#' __There is no need to specify the datatype parameter__ as an argument to [av_get_pf()]. The function will return a data.table.
#'
#' __ForEx "FROM/TO" symbol details.__ FOREX symbols in the `av_get_pf()` function are
#' supplied in `"FROM/TO"` format, which are then parsed in the Alpha Vantage API
#' into `from_currency` and `to_currency` API parameters. Usage example:
#' `av_get_pf(symbol = "EUR/USD", av_fun = "FX_DAILY")`
#'
#'
#' @examples
#' \dontrun{
#' av_api_key("YOUR_API_KEY")
#'
#' # example code
#'
#' # ---- 1.0 SINGLE NAME EQUITY SUMMARY INFORMATION AND SEARCH ----
#'
#' av_get_pf("IBM","OVERVIEW") |> str()
#'
#' av_get_pf("EWZ","ETF_PROFILE")
#' av_get_pf("EWZ","ETF_PROFILE") |> av_extract_df("holdings")
#'
#' av_get_pf("","SYMBOL_SEARCH",keywords="COMMERCE")
#'
#' # ---- 2.0 SINGLE NAME QUOTES  ----
#'
#' av_get_pf("IBM","GLOBAL_QUOTE")
#'
#' av_get_pf("USD/BRL","CURRENCY_EXCHANGE_RATE") |> av_extract_fx()
#'
#' # ---- 3.0 SINGLE NAME HISTORICAL DATA  ----
#'
#' av_get_pf("IBM","TIME_SERIES_DAILY")
#'
#' av_get_pf("IBM","TIME_SERIES_INTRADAY")
#'
#' # ---- 4.0 MARKET PRICING DATA  ----
#'
#' av_get_pf("","MARKET_STATUS")  |> av_extract_df()
#'
#' av_get_pf("","TOP_GAINERS_LOSERS") |> av_extract_df("top_losers")
#'
#' av_get_pf("","TREASURY_YIELD",maturity='7year')
#'
#'  # ---- 4.0 SINGLE NAME NON-PRICING DATA  ----
#'
#' av_get_pf("IBM","DIVIDENDS")
#'
#' av_get_pf("IBM","EARNINGS")  |> av_extract_df("quarter",melt=TRUE)
#'
#' av_get_pf("IBM","NEWS_SENTIMENT") |> av_extract_df("feed")
#'
#' av_get_pf("IBM","EARNINGS_CALL_TRANSCRIPT",quarter="2024Q3")  |> av_extract_df("transcript")
#'  # Note that quarter is a required parameter, not specifying will throw an error
#'
#'  # ---- 5.0 SINGLE NAME OPTION PRICING DATA  ----
#'
#' av_get_pf("IBM","HISTORICAL_OPTIONS") |> av_grep_opts("F,M,put",mindays=2)
#'
#' # ---- 6.0 TECHNICAL INDICATORS  ----
#'
#' av_funhelp("SMA")  # Shows parameters and defaults chosen by this package.
#' av_get_pf("IBM","SMA",time_period=20)
#'
#' }
#'
#' @export
av_get_pf <- function(symbol, av_fun, symbolvarnm="symbol",dfonerror=TRUE,melt=TRUE,verbose=FALSE, ...) {

    if (missing(symbol)) symbol <- NULL
    # Checks
    if (is.null(av_api_key())) {
        stop("Set API key using av_api_key(). If you do not have an API key, please claim your free API key on (https://www.alphavantage.co/support/#api-key). It should take less than 20 seconds, and is free permanently.",
             call. = FALSE)
    }
    ua   <- httr::user_agent("https://github.com/derekholmes0")

    # parameterss
    dots <- list(...)
    dots$symbol      <- symbol
    dots$apikey      <- av_api_key()

    # Forex
    is_forex <- !is.null(symbol) && stringr::str_detect(symbol, "\\/")
    if (is_forex) {
        currencies  <- symbol |> stringr::str_split_fixed("\\/", 2) |> as.vector()
        dots[c("from_currency","to_currency","from_symbol","to_symbol")] <- currencies[c(1,2,1,2)]
        dots$symbol <- NULL
    }

    # Generate URL
    url_params <-  av_form_param_url(av_fun,dots)
    if(is.null(url_params)) { # Missing something required
        stop("av_get_pf cannot create url; are you missing a required parameter?")
    }
    url <- glue::glue("https://www.alphavantage.co/query?function={av_fun}&{url_params}")

    # Alpha Advantage API call
    response <- httr::GET(url, ua)
    content_type <- httr::http_type(response)

    if(verbose) {
        urlset = strsplit(url,"&")[[1]]
        zz=lapply(urlset, \(x) message(sprintf("%-45s",strsplit(x,"=")[[1]])))
        message("> response: ",httr::status_code(response), " type: ",content_type)
    }

    # Handle bad status codes errors
    if ((httr::status_code(response) > 200 && httr::status_code(response) < 300)) {
        stop(httr::content(response, as="text"), call. = FALSE)
    }

    # # Clean data
    if (content_type == "application/json") {
        # JSON returned, valid call
        content <- httr::content(response, as = "text", encoding = "UTF-8")
        content_list <- content |> jsonlite::fromJSON()
        if ("Error Message" %in% names(content_list)) {
            message("av_get_pf:", content_list[[1]])
            return(data.frame())
        }

        # Detect good/bad call
        if (length(content_list)>0) {
            if(content_list[1] |> names() == "Meta Data") {  # Good call with Metadata
                stop(" Meta Data no longer received??")
            }
            else {  # Mixed results, process as best as possible
                dt_types = data.table::data.table(ltype=c("character","list"),varnm=c("value_str","value_df"))
                # data.table 3.33 ms vs xibble 9.4 ms
                    if(is_forex) {
                        content_list <- purrr::flatten(content_list)
                    }
                    lm0 <- data.table::data.table(variable=names(content_list),value=lapply(names(content_list), \(x) content_list[[x]]))  #unlist breaks apart too much
                    lm0$ltype <- purrr::map_chr(lm0$value,typeof)
                    lm0 <- dt_types[lm0, on=.(ltype)]
                    content <- data.table::dcast(lm0,variable + ltype ~ varnm,value.var=c("value"))
                    if("value_str" %in% names(content)) {
                        content <- content[,let('value_str'=as.character(get("value_str")))][,`:=`('value_num'=as.vector(suppressWarnings(readr::parse_number(get("value_str")))))]
                        content <- content[,`:=`(ltype = data.table::fifelse(is.na(get("value_num")), get("ltype"),"numeric"))][]
                    }
                if(is_forex) {
                    content <- content[,let(variable=gsub("^[0-9]. ","",variable))]  #Mutate does not work
                }
            }
        }
        else {     # Bad Call
            params_list <- c(symbol = data.table::fifelse(is.null(symbol),"NULL",symbol), av_fun = av_fun, dots)
            params_list <- params_list[setdiff(names(params_list),c("apikey","datatype"))]
            params <- paste(names(params_list), params_list, sep = "=", collapse = ", ")
            params <- gsub("av_fun","function",params)
            content <- content  |> paste(". API parameters used: ", params)
            message("av_get_pf Error: ", content)
            return(data.table::data.table())
        }

    } else { #  application/x-download
        # CSV Returned - Good Call - Time Series CSV file
        contx <- httr::content(response, as = "text", encoding = "UTF-8")
        content <- gsub("%", "",contx) |> data.table::fread(,na.strings=c(".","NA"))
        if(nrow(content)==1 & melt==TRUE) {
            content <- content |> melt_tobasetype(idvar=symbolvarnm)
            }
    }
    if( !av_funcmap[av_fn==av_fun][1,]$hassymbol ) {
        content$symbol = av_fun
    }
    # Return desc
    if ("timestamp" %in% names(content)) {
        data.table::setorder(content,timestamp)
    }
    if(nchar(symbolvarnm)>0) {
        data.table::setcolorder(content[,c(symbolvarnm):=symbol],c(symbolvarnm))
    }
    return(content[])
}

melt_tobasetype <- function(dta,idvar="symbol",varname="variable") {
    # idvar must always be in input
    if( !(idvar %in% names(dta))) {
        stop(paste0("melt_tobasetype> Need ",idvar," in input dta"))
    }
    char_cols <- setdiff(names(which(sapply(dta, is.character))),idvar)
    num_cols <- c(names(which(sapply(dta, \(x) !is.character(x)))))
    mm1 <- data.table::data.table()
    if(length(char_cols)>0) {
        mm1 <- data.table::melt(dta, id.vars=idvar,measure.vars=char_cols,variable.name=varname,value.name="value_str")
    }
    # Warning will always be given for numeric-like objects coerced into numeric
    mm2 <- suppressWarnings(data.table::melt(dta,id.vars=idvar,measure.vars=num_cols,variable.name=varname,value.name="value_num"))
    return(data.table::rbindlist(list(mm1,mm2),use.names=TRUE,fill=TRUE))
}

av_form_param_url<- function(this_av_fn,dots) {
    pset <- av_funcmap[get("av_fn")=="defaultparam" | get("av_fn")==this_av_fn,]
    pset <- data.table::data.table(`paramname`=names(dots),`newval`=unlist(dots))[pset,on=c("paramname")]
    pset <- pset[,.(`paramname`=get("paramname"),`ro`=get("ro"),'value'=data.table::fcoalesce(get("newval"),get("def_value")))]
    missing_required = pset[get("ro")=="R" & is.na(value),] # get marginally faster
    if(nrow(missing_required)>0) {
        message("av_form_param_url: ERROR ",this_av_fn," missing required parameters: ", paste0(missing_required$paramname,collapse=", "))
        return(NULL)
    }
    url_list <- pset[!is.na(get("value")),]
    paste0(url_list$paramname,"=",url_list$value,collapse="&")
}


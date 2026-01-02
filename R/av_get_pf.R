#' Get financial data from the Alpha Vantage API
#'
#' @name av_get_pf
#'
#' @param symbol A character string of an appropriate stock, fund, or currency
#' See parameter "symbol" in [Alpha Vantage API documentation](https://www.alphavantage.co/documentation/).
#' @param av_fun A character string matching an appropriate Alpha Vantage "function".
#' See parameter "function" in [Alpha Vantage API documentation](https://www.alphavantage.co/documentation/).
#' @param symbolvarnm Variable name of symbol requested inn output, default "symbol"
#'
#' @param dfonerror Return an empty data.table when any error occurs, default TRUE
#'
#' @param verbose Print debug information helpful for errors
#'
#' @param ... Additional parameters or overrides passed to the Alpha Vantage API.
#' For a list of parameters, visit the [Alpha Vantage API documentation](https://www.alphavantage.co/documentation/).
#'
#' @returns Returns a data.table with results dependent on the function called.
#' Mixed data is returned as a melted data.table, possibly with nested data.frames.  Time series are returned as data.tables.
#'
#' @seealso [av_api_key()], [av_extract_df()], [av_extract_fx()], [av_narrowopts()],[av_funhelp()]
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
#' @examples
#' \dontrun{
#'
#' # SETUP API KEY
#' av_api_key("YOUR_API_KEY")
#'
#' # ---- 1.0 STOCK TIME SERIES ----
#'
#' # 1.1 TIME SERIES INTRADAY
#' av_get_pf("MSFT", av_fun = "TIME_SERIES_INTRADAY", interval = "5min", outputsize = "full")
#'
#' # 1.2 TIME SERIES DAILY ADJUSTED
#' av_get_pf("MSFT", av_fun = "TIME_SERIES_DAILY_ADJUSTED", outputsize = "full")
#'
#' # 1.3 QUOTE ENDPOINTS
#' av_get_pf("MSFT", av_fun = "GLOBAL_QUOTE")
#'
#' # ---- 2.0 FOREX ----
#'
#' # 2.1 CURRENCY EXCHANGE RATES
#' av_get_pf("EUR/USD", av_fun = "CURRENCY_EXCHANGE_RATE")
#'
#' # 2.2 FX INTRADAY
#' av_get_pf("EUR/USD", av_fun = "FX_INTRADAY", interval = "5min", outputsize = "full")
#'
#' # 2.3. FX DAILY
#' av_get_pf("EUR/USD", av_fun = "FX_DAILY", outputsize = "full")
#'
#' # ---- 3.0 TECHNICAL INDICATORS ----
#'
#' # 3.1 SMA
#' av_get_pf("MSFT", av_fun = "SMA", interval = "weekly", time_period = 10, series_type = "open")
#'
#' # ---- 4.0 SECTOR PERFORMANCE ----
#'
#' # 4.1 Sector Performance
#' av_get_pf(av_fun = "SECTOR")
#' }
#'
#'
#'
#' @export
av_get_pf <- function(symbol, av_fun, symbolvarnm="symbol",dfonerror=TRUE,verbose=TRUE, ...) {

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
                # content_list <- unlist(content_list,recursive=FALSE)  #flattens everything
                # data.table 3.33 ms vs xibble 9.4 ms
                    if(is_forex) {
                        content_list <- purrr:flatten(content_list)
                    }
                    lm0 <- data.table::data.table(variable=names(content_list),value=lapply(names(content_list), \(x) content_list[[x]]))  #unlist breaks apart too much
                    lm0$ltype <- purrr::map_chr(lm0$value,typeof)
                    lm0 <- dt_types[lm0, on="ltype"]
                    content <- data.table::dcast(lm0,variable + ltype ~ varnm,value.var=c("value"))
                    if("value_str" %in% names(content)) {
                        content <- content[,let(value_str=as.character(value_str))][,`:=`(value_num=suppressWarnings(readr::parse_number(value_str)))]
                        content <- content[,`:=`(ltype = data.table::fifelse(is.na(value_num), ltype,"numeric"))][]
                    }
                if(is_forex) {
                    content <- content[,let(variable=gsub("^[0-9]. ","",variable))]  #Mutate does not work
                }
            }
        }
        else {     # Bad Call
            params_list <- c(symbol = fifelse(is.null(symbol),"NULL",symbol), av_fun = av_fun, dots)
            params_list <- params_list[setdiff(names(params_list),c("apikey","datatype"))]
            params <- paste(names(params_list), params_list, sep = "=", collapse = ", ")
            params <- gsub("av_fun","function",params)
            content <- content  |> paste(". API parameters used: ", params)
            message("av_get_pf Error: ", content)
            #stop(content, call. = F)
            return(data.table())
        }

    } else { #  application/x-download
        # CSV Returned - Good Call - Time Series CSV file
        contx <- httr::content(response, as = "text", encoding = "UTF-8")
        content <- gsub("%", "",contx) |> data.table::fread(,na.strings=c(".","NA"))
        if(nrow(content)==1) {
            content <- content |> melt_tobasetype(idvar=symbolvarnm)
            }

    }
    if( !av_funcmap[av_fn==av_fun][1,]$hassymbol ) {
        content$symbol = av_fun
    }
    # Fix names
#    names(content) <- names(content) |>
#        stringr::str_replace_all("[0-9]+\\. ", "") |>
#        make.names() |>
#        stringr::str_replace_all("\\.", "_") |>
#        tolower()

    # Return desc
    if ("timestamp" %in% names(content)) {
        setorder(content,timestamp)
    }
    if(nchar(symbolvarnm)>0) {
        content <- content |> dplyr::mutate(!!sym(symbolvarnm):=symbol) |> dplyr::relocate(!!sym(symbolvarnm))
    }
    return(content)
}

melt_tobasetype <- function(dta,idvar="symbol",varname="variable") {
    # idvar must always be in input
    char_cols <- setdiff(names(which(sapply(dta, is.character))),idvar)
    num_cols <- c(idvar,names(which(sapply(dta, \(x) !is.character(x)))))
    mm1 <- data.table::data.table()
    if(length(char_cols)>0) {
        mm1 <- data.table::melt(dta, id.vars=idvar,measure.vars=char_cols,variable.name=varname,value.name="value_str")
    }
    mm2 <- suppressWarnings(data.table::melt(dta,id.vars=idvar,measure.vars=num_cols,variable.name=varname,value.name="value_num"))
    return(data.table::rbindlist(list(mm1,mm2),use.names=TRUE,fill=TRUE))
}

av_form_param_url<- function(this_av_fn,dots) {
    pset <- av_funcmap[av_fn=="defaultparam" | av_fn==this_av_fn,]
    pset <- data.table::data.table(paramname=names(dots),newval=unlist(dots))[pset,on=.(paramname)][,.(paramname,ro,value=data.table::fcoalesce(newval,def_value))]
    missing_required = pset[ro=="R" & is.na(value),]
    if(nrow(missing_required)>0) {
        message("av_form_param_url: ",this_av_fn," missing required parameters: ", paste0(missing_required$paramname,collapse=", "))
        return(NULL)
    }
    url_list <- pset[!is.na(value),]
    paste0(url_list$paramname,"=",url_list$value,collapse="&")
}


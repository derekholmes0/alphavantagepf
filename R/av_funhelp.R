#' Extract data from Alpha Vantage retuned data
#'
#' @name av_funhelp
#'
#' @param av_fun A Alpha Vantage function name.
#' For a list of parameters, visit the [Alpha Vantage API documentation](https://www.alphavantage.co/documentation/).
#'
#' @returns Help Text and default parameters.
#'
#' @details Returns defaults and parameter lists for Alphavantage functions
#'
#' @seealso [av_get_pf()]
#'
#' @examples
#' \dontrun{
#' av_funhelp("GLOBAL_QUOTE")
#' }
#'
#' @export
av_funhelp <- function(av_fun) {
    subdf <- av_funcmap[av_fn==toupper(av_fun),]
    paramlist <- paste0(" - ",subdf$paramname,data.table::fifelse(is.na(subdf$def_value),"",paste0(" (default: ",subdf$def_value,")")),"\n",collapse="")
    helptext <- cat(paste0("Function: ",av_fun,"\nCategory: ",unique(subdf$category),"\n\nParameters:\n",paramlist))
    return(helptext)
}

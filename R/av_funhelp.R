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
    subdf <- av_funcmap[get("av_fn")==toupper(av_fun),]
    make_plist <- function(wh) {
      r_params <- subdf[get("ro")==wh,]
      paste0(wh,"> ",r_params$paramname,data.table::fifelse(is.na(r_params$def_value),"",paste0(" (default: ",r_params$def_value,")")),"\n",collapse="")
      }
    helptext <- cat(paste0("Function: ",av_fun,"\nCategory: ",unique(subdf$category),"\n\nParameters:\n",make_plist("R"),make_plist("O")))
    return(helptext)
}

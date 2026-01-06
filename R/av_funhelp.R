#' Extract data from Alpha Vantage retuned data
#'
#' @name av_funhelp
#'
#' @param av_fun_grep A Alpha Vantage function name or portions of one.
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
av_funhelp <- function(av_fun_grep="") {
    if(av_fun_grep=="") {
      u1<-av_funcmap[,.N,by=.(category,av_fn)][!(category=="ta" | category=="default")]
      u1<-u1[order(category,av_fn)][,`no`:=.I-min(.I),by=c("category")]
      return(data.table::dcast(u1,no ~ category,value.var="av_fn",fill=""))
    }
    subdf <- av_funcmap[grepl(toupper(av_fun_grep),get("av_fn")),]
    make_plist <- function(indta,wh) {
      r_params <- indta[get("ro")==wh,]
      if(nrow(r_params)>0)
        return(paste0(wh,"> ",r_params$paramname,data.table::fifelse(is.na(r_params$def_value),"",paste0(" (default: ",r_params$def_value,")")),"\n",collapse=""))
    }
    helptestlist <- lapply(unique(subdf$av_fn), function(tfn) {
                      tdta = subdf[get("av_fn")==tfn,];
                      paste0("Function: ",tfn,"\nCategory: ",unique(tdta$category),"\n\nParameters:\n",make_plist(tdta,"R"),make_plist(tdta,"O"))
                })
    helptext<-cat(paste(helptestlist,collapse="==========\n"))
    return(helptext)
}

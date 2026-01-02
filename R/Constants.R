#' Extract data from Alpha Vantage retuned data
#'
#' @name av_make_funcmap
#'
#' @returns function map dat.table, also written to data directory
#'
#' @details Creates a data.table with API signatures and default values for Alphavantage API calls
#'
#' @seealso [av_get_pf()]
#'
#' @examples
#' \dontrun{
#' av_make_funcmap()
#' }
#'
#' @export
av_make_funcmap <- function() {
    av_funcmap_all <- data.table::fread("../data/av_funcmap.csv") |>
        data.table::melt(id.vars=c("av_fn","category"))
    av_funclist <- av_funcmap_all[,.(paramname="placeholder"),by=.(category,av_fn)]
    av_funcmap <- av_funcmap_all[nchar(value)>0,] |>
        tidyr::separate(value,c("ro","paramname"),sep=":",fill="left") |>
        tidyr::separate(paramname,c("paramname","def_value"),sep="=",fill="right",extra="merge") |>
        data.table(key=c("av_fn","paramname")) |> dplyr::arrange(av_fn,variable)
    # hasssymbol needed to add function name to returned data.
    f_w_symbols <- av_funcmap[paramname=="symbol",.(hassymbol=(.N>0)),by=.(av_fn)]
    av_funcmap <- f_w_symbols[av_funcmap,on=.(av_fn)][,':='(hassymbol=data.table::fcoalesce(hassymbol,FALSE))][]
    av_nofunc <- av_funclist[!av_funcmap[,.N,by=.(category,av_fn)],on=.(av_fn)][,':='(hassymbol=FALSE)]
    av_funcmap <- data.table::rbindlist(list(av_funcmap,av_nofunc),use.names=TRUE,fill=TRUE)
    #save(av_funcmap,file="../data/av_funcmap.rda",compress="xz")
    return(av_funcmap)
}

# av_funcmap <- av_make_funcmap()
# usethis::use_data(av_funcmap, internal = TRUE)

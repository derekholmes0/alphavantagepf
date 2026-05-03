the <- new.env(parent = emptyenv())
the$defaultcachedir <- tools::R_user_dir("alphavantagepf", which = "cache")
# load("./R/sysdata.rda",envir=the)
#  loads tevents_defaults and ratingsmapmelt
the$constants_fn <- paste0( the$defaultcachedir, "/avpf_constants.RD")
the$pxd <- data.table()
the$pxinv <- data.table()

if(file.exists(the$constants_fn)) {
  restore_avs_state()
  options(av_api_key = the$avapikey)
  options(av_api_entitlement = the$avapientitlement)
} else {
  # What is not in constatns
  the$cachedir <- the$defaultcachedir
  the$save_dir <- ""
  the$assetlist_fn <- paste0( the$cachedir, "/avpf_assetlist.RD")
  the$assetlist <- data.table(listnm=c(rep("defaultIdx",2),rep("ix2",2)),ticker=c("SPY","QQQ","EWZ","EEM"))
  the$pxd_fn <- paste0( the$cachedir, "/avpf_px.fst")
  the$inv_fn <- paste0( the$cachedir, "/avpf_inv.RD")
  the$avapikey <- "NOT_SET"
  the$avapientitlement <- "delayed"
  u1<-lapply(s("verbose;cleanonstart;save_prices;save_cum;save_ts;verbose"), \(x) assign(x,TRUE,envir=the))
  u1<-lapply(s("save_data"), \(x) assign(x,FALSE,envir=the))
  the$inpline1 <-"SPY"
  the$inpline2 <-"SPY"
  the$ts_colorset <- "lines"
  the$extracalc_file<-""
}

.datatable.aware = TRUE


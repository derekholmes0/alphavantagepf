the <- new.env(parent = emptyenv())
the$defaultcachedir <- tools::R_user_dir("alphavantagepf", which = "cache")
# load("./R/sysdata.rda",envir=the)
the$constants_fn <- paste0( the$defaultcachedir, "/avpf_constants.RD")
the$NY_local_hrs = as.POSIXct(paste0(Sys.Date()," 12:00:00"),tz="UTC") - as.POSIXct(paste0(Sys.Date()," 12:00:00"),tz="America/New_York")
the$pxd <- data.table()
the$pxinv <- data.table()
# Other item in the, downloaded on startup
the$indexlist <- data.table()
# Minimal data needed
the$assetlist <- data.table(listnm=c(rep("defaultIdx",3)),ticker=c("SPY","QQQ","DIA"))
the$cachedir <- the$defaultcachedir

.datatable.aware = TRUE

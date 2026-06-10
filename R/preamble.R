the_av <- new.env(parent = emptyenv())
the_av$defaultcachedir <- tools::R_user_dir("alphavantagepf", which = "cache")
the_av$constants_fn <- paste0( the_av$defaultcachedir, "/avpf_constants.RD")
the_av$NY_local_hrs = as.POSIXct(paste0(Sys.Date()," 12:00:00"),tz="UTC") - as.POSIXct(paste0(Sys.Date()," 12:00:00"),tz="America/New_York")
the_av$pxd <- data.table()
the_av$pxinv <- data.table()
# Other item in the, downloaded on startup
the_av$tickerlist <- data.table()
# Minimal data needed
the_av$assetgroups <- data.table(listnm=c(rep("defaultIdx",3)),ticker=c("SPY","QQQ","DIA"))
the_av$cachedir <- the_av$defaultcachedir

.datatable.aware = TRUE

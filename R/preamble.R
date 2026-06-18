the_av <- new.env(parent = emptyenv())
# Minimal data needed to get Started
the_av$defaultcachedir <- gsub("\\","/",tools::R_user_dir("alphavantagepf", which = "cache"),fixed=TRUE)
the_av$constants_fn <- paste0( the_av$defaultcachedir, "/avpf_constants.RD")
the_av$NY_local_hrs = as.POSIXct(paste0(Sys.Date()," 12:00:00"),tz="UTC") - as.POSIXct(paste0(Sys.Date()," 12:00:00"),tz="America/New_York")
the_av$cachedir <- the_av$defaultcachedir

.datatable.aware = TRUE

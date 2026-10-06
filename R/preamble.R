# Minimal data needed to get Started
if(!exists("the_av")) { the_av <- new.env(parent = emptyenv()) }
.onLoad <- function(libname, pkgname) {
  if(!exists("inv_fn_ts", envir=the_av)) {
    the_av$defaultcachedir <- gsub("\\","/",tools::R_user_dir("alphavantagepf", which = "cache"),fixed=TRUE)
    the_av$constants_fn <- paste0( the_av$defaultcachedir, "/avpf_constants.RD")
    if(!file.exists(the_av$constants_fn)) {
      the_av$NY_local_hrs = as.POSIXct(paste0(Sys.Date()," 12:00:00"),tz="UTC") - as.POSIXct(paste0(Sys.Date()," 12:00:00"),tz="America/New_York")
      the_av$cachedir <- the_av$defaultcachedir
      the_av$avsh_funcs <- data.table()
      the_av$pxinv <- data.table()
      the_av$max_requests_per_min <- avsd$defaults[get("var")=="max_requests_per_min",]$value_num
      the_av$logopts <- avsd$defaults[get("var")=="logopts",]$value_str
    }
  }
}

.datatable.aware = TRUE

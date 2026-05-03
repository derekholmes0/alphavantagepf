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
  the$assetlist <- data.table(listnm=c(rep("defaultIdx",2),rep("ix2",2)),ticker=c("SPY","QQQ","EWZ","EEM"))
  the$cachedir <- the$defaultcachedir
  u1 <- avsd$defaults[varType=="cache",]
  u1 <- lapply(seq(1,nrow(u1)), \(i) assign(u1[i,]$var, paste0(the$cachedir,"/",u1[i,]$value_str), envir=the))
  u1 <- avsd$defaults[varType=="str",]
  u1 <- lapply(seq(1,nrow(u1)), \(i) assign(u1[i,]$var, u1[i,]$value_str, envir=the))
  u1 <- avsd$defaults[varType=="log",]
  u1 <- lapply(seq(1,nrow(u1)), \(i) assign(u1[i,]$var, u1[i,]$value_log, envir=the))
}

.datatable.aware = TRUE


# =======================================================================================================
#' Date Utilities
#'

# =======================================================================================================
#' Other utititlies internal to this package

#' @name message_if
#' @description Prints message if condition met
#' @param reallydothis Condition to be met
#' @param ... Additional items to be passed to `message`
#'
message_if <- function(reallydothis,...) {  if(reallydothis) { message(...) } }
message_if_red <- function(reallydothis,...) {  if(reallydothis) { message("\033[31m",...,"\033[0m") } }
message_if_green <- function(reallydothis,...) {  if(reallydothis) { message("\033[32m",...,"\033[0m") } }

#' @noRd
s<-function(x,sep=";",fixed=TRUE,rtn=NULL) {
    if(is.null(x) || is.logical(x)) { return(x) }
    y=unlist(strsplit(x,sep,fixed=fixed))
    if(is.numeric(rtn)) { if(length(y)>=rtn) { y=y[rtn] } }
    return(y)
    }

#txtcolors <-list("red"="\033[31m","green"="\033[32m","off"="\033[0m")

#' @noRd
dump_hash<-function(out) {
  u1=lapply(names(out), \(x) message("out[",x,"]: ",paste(class(out[[x]]),collapse=",")))
}

#  CAnt get this oto work from within shiny app
#' @noRd
#' @importFrom purrr map2
lineAssign<-function(xline) {
  if("data.table" %in% class(xline)) {
    if(nrow(xline)>1) message("lineAssign cannot assigm more than oneline")
    aaa1 <- purrr::map2(names(as.list(xline[1,])),as.list(xline[1,]), function(x,y){assign(x,y,envir=sys.frame(1))})
  }
  if("list" %in% class(xline)) {
    aaa1<-lapply(names(xline),\(x) {  assign(x,xline[[x]],envir=sys.frame(1))})
  }
}

#' @noRd
find_col_bytype <- function(indt,typeoffn,firstonly=TRUE,takeout=NA_character_) {
    rtn <- names(indt)[sapply(indt, typeoffn)]
    if(length(rtn)==0) { return(NULL)}
    rtn=setdiff(rtn,takeout)
    if(firstonly) return (rtn[1])
    else return(rtn)
}

# =======================================================================================================
#' data table utilities
#' @noRd
DTappend <- function(indta,newdta) { data.table::rbindlist(list(indta,newdta),use.names=TRUE,fill=TRUE) }
DTUpsert<-function(a,b,keys, fill=FALSE,verbose="",replaceifbempty=NULL) { # DT kind of tough to use this replaces old data
  if(!data.table::is.data.table(b)) {
    b<- data.table::data.table(b) }
  if (is.character(a)) { aandb <- b }
  else if(nrow(a)<=0) { aandb <-b }
  else if(nrow(b)<=0 | length(setdiff(keys,colnames(b)))>0) {
    if( is.data.table(replaceifbempty) ) { aandb <- replaceifbempty }
    else { aandb <-a }
  }
  else {
    data.table::setkeyv(a,keys)
    data.table::setkeyv(b,keys)
    aandb<- data.table::rbindlist(list(a[!b],b),use.names=TRUE,fill=fill)
    if(nchar(verbose)>1) { message("DTUpsert(",verbose,"): adds ",nrow(b)," rows, now ",nrow(aandb)) }
    if(  any(grepl(".x",colnames(aandb),fixed=TRUE)) ) {
      stop(" ERORR in DTUpser... colnames: ",paste0(colnames(aandb),collapse=","))
    }
  }
  data.table::setkeyv(aandb,keys)
  return(aandb)
}

#' @noRd
coalesce_DT<-function(DT1,DT2) { # Adds columns as necessary, either row by row or single row
  `.` <- jrep <- NULL
  if (!(nrow(DT2)==1 | nrow(DT1)==nrow(DT2))) {
    stop("coalesce_dt incompatile sizes")
  }
  DT1cols <- colnames(DT1)
  DT2 <-data.table::copy(DT2)[,let(jrep=.I)]
  DT2 <- DT2[,.SD,.SDcols=setdiff(colnames(DT2),colnames(DT1))]  # Should always include jrep
  DT1 <-data.table::copy(DT1)[,let(jrep=seq(1,nrow(DT2)))]
  DT3 <- DT2[DT1,on=.(jrep)][,let(jrep=NULL)]
  setcolorder(DT3,DT1cols)
  return(DT3[])
}

#' @noRd
coalesce_DT_byentry<-function(DT1,DT2) { # Adds columns as necessary, either row by row or single row
  `.` <- jrep <- NULL
  if (!(nrow(DT2)==1 | nrow(DT1)==nrow(DT2))) {
    stop("coalesce_DT_byentry incompatible sizes, either DT2 must be 1 row or nrow(DT1) rows")
  }
  DT1cols <- colnames(DT1)
  DT2cols <- colnames(DT2)
  DTcommoncols <- intersect(DT1cols,DT2cols)
  DTfinalcols <- setdiff(union(DT1cols,DT2cols),c("jrep"))
  DT1copy <- data.table::copy(DT1)
  DT3 <-  data.table::merge.data.table(DT1copy[,let(jrep=seq(1,nrow(DT2)))],DT2[,let(jrep=.I)],by=c("jrep"))
  DT3 <- DT3[,(DTcommoncols):=lapply(DTcommoncols, \(x) data.table::fcoalesce(.SD[[paste0(x,".x")]],.SD[[paste0(x,".y")]])), by=.(jrep)] # Common columns
  DT3 <- DT3[,.SD,.SDcols=DTfinalcols]
  return(DT3[])
}

###


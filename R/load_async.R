get.async.scenlist <- function(uid, scen.dir){
  if(!dir.exists(scen.dir)){
    warning("The scenario directory you specified for asynchronous solves does not exist.", call. = F)
    return(NULL)
  }
  if(!dir.exists(paste0(scen.dir, uid))){
    return(NULL)
  }
  list.dirs()
}
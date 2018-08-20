isShinyProxy <- function(){
  # specifies whether shiny proxy is used
  # 
  # Args:
  #
  # Returns:
  # boolean that specifies whether shiny proxy is used
  
  uid <- Sys.getenv("SHINYPROXY_USERNAME")
  if(is.null(uid) || grepl("^\\s*$",uid)){
    invisible(FALSE)
  }else{
    invisible(TRUE)
  }
}
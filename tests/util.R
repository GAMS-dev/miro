getSelectizeOptions <- function(app, selector){
  app$getDebugLog("browser")
  app$waitFor(paste0("var options=$('", selector, "')[0].selectize.options;options=Object.keys(options);for(i in options){console.log(options[i])};true"))
  options <- app$getDebugLog("browser")$message
  return(substr(options, 1, nchar(options) -4))
}

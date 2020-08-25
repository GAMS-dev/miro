getSelectizeOptions <- function(app, selector){
  app$getDebugLog("browser")
  app$waitFor(paste0("var options=$('", selector, "')[0].selectize.options;options=Object.keys(options);for(i in options){console.log(options[i])};true"))
  options <- app$getDebugLog("browser")$message
  return(rev(substr(options, 1, nchar(options) -4)))
}
saveAdditionalGamsClArgs <- function(miroModelDir, modelToTest, additionalGamsClArgs){
  if(!length(additionalGamsClArgs)){
    return()
  }
  configJSONFileName <- file.path(miroModelDir, paste0("conf_", modelToTest), paste0(modelToTest, ".json"))
  file.copy(configJSONFileName,
            file.path(dirname(configJSONFileName), paste0(modelToTest, "_tmp.json")), overwrite = TRUE)
  configJSON <- suppressWarnings(jsonlite::fromJSON(configJSONFileName, 
                                                    simplifyDataFrame = FALSE, 
                                                    simplifyMatrix = FALSE))
  configJSON$extraClArgs <- c(configJSON$extraClArgs, additionalGamsClArgs)
  jsonlite::write_json(configJSON, configJSONFileName, pretty = TRUE, auto_unbox = TRUE, null = "null")
  return(invisible())
}

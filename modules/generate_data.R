generateDataUI <- function(id, type, height= NULL, customOptions = NULL){
  ns <- NS(id)
  
  typeCustom <- type
  
  tryCatch({
    customInput <- match.fun(typeCustom %+% "Output")
  }, error = function(e){
    stop(sprintf("An input function for the custom generator: '%s' was not found. 
                   Please make sure you first define such a function.", typeCustom), call. = FALSE)
  })
  data <- customInput(ns("custom"), height = height, options = customOptions,
                      path = customRendererDir)
  return(tags$div(id = ns("data"), data))
}

generateData <- function(input, output, session, data, type, 
                         configData = NULL, customOptions = NULL, rendererEnv = NULL,
                         attachments = NULL){
  typeCustom <- type
  
  tryCatch({
    customGenerator <- match.fun(paste0("render", toupper(substr(typeCustom, 1, 1)), 
                                        substr(typeCustom, 2, nchar(typeCustom))))
  }, error = function(e){
    stop(sprintf("A custom generator function: '%s' was not found. 
                   Please make sure you first define such a function.", typeCustom), call. = FALSE)
  })
  tryCatch({
    return(callModule(customGenerator, "custom", as_tibble(data), options = customOptions, 
                      path = customRendererDir, rendererEnv = rendererEnv, attachments = attachments))
  }, error = function(e){
    stop(sprintf("An error occured in the custom generator function: '%s'. Error message: %s.", typeCustom, e), call. = FALSE)
  })
}

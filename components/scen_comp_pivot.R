renderScenPivotCompare <- function(scenData, scenNames, rendererEnv,
                                   langData, views, roundPrecision = 2L){
  scenIdLong     <- "scen_0_"
  outputDsNames  <- names(ioConfig$modelOut)
  scenTableNamesToDisplay <- tolower(ioConfig$scenTableNamesToDisplay)
  
  if(is.null(rendererEnv[[scenIdLong]])){
    rendererEnv[[scenIdLong]] <- new.env(parent = emptyenv())
  }else{
    for(el in ls(envir = rendererEnv[[scenIdLong]])){
      if("Observer" %in% class(rendererEnv[[scenIdLong]][[el]])){
        rendererEnv[[scenIdLong]][[el]]$destroy()
      }
    }
  }
  
  for(tabId in seq_along(scenTableNamesToDisplay)){
    sheetName <- scenTableNamesToDisplay[tabId]
    scenTableId <- match(sheetName, ioConfig$inputDsNames)
    if(is.na(scenTableId)){
      scenTableId <- match(sheetName, outputDsNames)
      graphOptions <- list(lang = langData,
                           resetOnInit = TRUE,
                           "_metadata_" = list(symname = sheetName,
                                               headers = c(list("_scenName" = list(alias = langData$pivotCompScenColName,
                                                                                   type = "string")),
                                                           ioConfig$modelOut[[sheetName]]$headers),
                                               symtype = ioConfig$modelOut[[sheetName]]$symtype))
    }else{
      scenTableId <- length(outputDsNames) + scenTableId
      graphOptions <- list(lang = langData,
                           resetOnInit = TRUE,
                           "_metadata_" = list(symname = sheetName,
                                               headers = c(list("_scenName" = list(alias = langData$pivotCompScenColName,
                                                                                   type = "string")),
                                                           ioConfig$modelIn[[sheetName]]$headers),
                                               symtype = ioConfig$modelIn[[sheetName]]$symtype))
    }
    tryCatch({
      dataToRender <- lapply(scenData, "[[", scenTableId)
      names(dataToRender) <- scenNames
      dataToRender <- bind_rows(dataToRender, .id = "_scenName")
      callModule(renderData, paste0("tab_0_", tabId), 
                 type = "miroPivot", 
                 data = dataToRender,
                 customOptions = graphOptions,
                 roundPrecision = roundPrecision,
                 rendererEnv = rendererEnv[[scenIdLong]],
                 views = views, attachments = attachments)
    }, error = function(e) {
      stop(sprintf("Problem rendering graphs for dataset: '%s'. Error message: %s.",
                   sheetName, conditionMessage(e)), call. = FALSE)
    })
  }
}
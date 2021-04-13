# rendering output tables and graphs
renderOutputData <- function(rendererEnv, views){
  progress <- Progress$new()
  on.exit(progress$close())
  progress$set(message = lang$progressBar$renderOutput$title, value = 0)
  errMsg <- NULL
  for(el in ls(envir = rendererEnv$output)){
    if("Observer" %in% class(rendererEnv$output[[el]])){
      rendererEnv$output[[el]]$destroy()
    }
  }
  dynamicUILoaded[["outputTables"]][] <<- FALSE
  lapply(unlist(outputTabs, use.names = FALSE), function(i){
    tryCatch({
      callModule(renderData, paste0("tab_", i),
                 type = configGraphsOut[[i]]$outType,
                 data = scenData$get("sb",
                                     symNames = c(names(modelOut)[i],
                                                  configGraphsOut[[i]]$additionalData),
                                     drop = TRUE),
                 configData = scenData$getScalars("sb"),
                 dtOptions = configGraphsOut[[i]]$datatable,
                 graphOptions = configGraphsOut[[i]]$graph, 
                 pivotOptions = configGraphsOut[[i]]$pivottable,
                 customOptions = configGraphsOut[[i]]$options,
                 roundPrecision = roundPrecision,
                 modelDir = modelDir,
                 rendererEnv = rendererEnv$output,
                 views = views,
                 attachments = attachments)
      if(modelOutputTableVisible[[i]]){
        callModule(renderData, paste0("table-out_", i),
                   type = "datatable",
                   data = scenData$get("sb", symNames = names(modelOut)[i], drop = TRUE),
                   dtOptions = configGraphsOut[[i]]$datatable,
                   roundPrecision = roundPrecision)
        dynamicUILoaded[["outputTables"]][i] <<- TRUE
      }
    }, error = function(e) {
      flog.error("Problems rendering output charts/tables of dataset: '%s'. Error message: %s.",
                 modelOutAlias[i], conditionMessage(e))
      errMsg <<- paste(errMsg, sprintf(lang$errMsg$renderTable$desc, modelOutAlias[i]), sep = "\n")
      showEl(session, paste0("#tab_", i, "-noData"))
      hideEl(session, paste0("#tab_", i, "-data"))
    })
    progress$inc(1/length(modelOut), detail = paste0(lang$progressBar$renderOutput$progress, i))
  })
  showErrorMsg(lang$errMsg$renderTable$title, errMsg)
}
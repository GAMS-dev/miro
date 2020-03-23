# rendering output tables and graphs
renderOutputData <- function(){
  progress <- Progress$new()
  on.exit(progress$close())
  progress$set(message = lang$progressBar$renderOutput$title, value = 0)
  errMsg <- NULL
  lapply(unlist(outputTabs, use.names = FALSE), function(i){
    tryCatch({
      if(length(configGraphsOut[[i]]$additionalData)){
        additionalOutputIds <- match(configGraphsOut[[i]]$additionalData, names(modelOut))
        additionalOutputIdsNA <- is.na(additionalOutputIds)
        if(any(additionalOutputIdsNA)){
          additionalInputIds <- match(configGraphsOut[[i]]$
                                        additionalData[additionalOutputIdsNA],
                                      names(modelIn))
          additionalOutputIds <- c(i, additionalOutputIds[!additionalOutputIdsNA])
          rendererData <- c(scenData[["scen_1_"]][additionalOutputIds], 
                            modelInputData[additionalInputIds])
          names(rendererData) <- c(names(modelOut)[additionalOutputIds], 
                                   names(modelIn)[additionalInputIds])
        }else{
          additionalOutputIds <- c(i, additionalOutputIds)
          rendererData <- scenData[["scen_1_"]][additionalOutputIds]
          names(rendererData) <- names(modelOut)[additionalOutputIds]
        }
      }else{
        rendererData <- scenData[["scen_1_"]][[i]]
      }
      callModule(renderData, "tab_" %+% i, type = configGraphsOut[[i]]$outType, data = rendererData,
                 configData = scalarData[["scen_1_"]], dtOptions = config$datatable, graphOptions = configGraphsOut[[i]]$graph, 
                 pivotOptions = configGraphsOut[[i]]$pivottable, customOptions = configGraphsOut[[i]]$options,
                 roundPrecision = roundPrecision, modelDir = modelDir)
      callModule(renderData, "table-out_" %+% i, type = "datatable", data = scenData[["scen_1_"]][[i]],
                 dtOptions = config$datatable, roundPrecision = roundPrecision)
    }, error = function(e) {
      flog.error("Problems rendering output charts/tables of dataset: '%s'. Error message: %s.", modelOutAlias[i], e)
      errMsg <<- paste(errMsg, sprintf(lang$errMsg$renderTable$desc, modelOutAlias[i]), sep = "\n")
      showEl(session, paste0("#tab_", i, "-noData"))
      hideEl(session, paste0("#tab_", i, "-data"))
    })
    progress$inc(1/length(modelOut), detail = paste0(lang$progressBar$renderOutput$progress, i))
  })
  showErrorMsg(lang$errMsg$renderTable$title, errMsg)
}
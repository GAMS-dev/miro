# generate tab panel for new scenario
noData <- vector("logical", length = length(scenTableNamesToDisplay))
if(isInSplitView){
  scenCounter <- scenId
  # hide button and show content
  local({
    id <- if(loadInLeftBoxSplit) 1L else 2L
    if(!compareModeTabsetGenerated[id]){
      compareModeTabsetGenerated[id] <<- TRUE
      insertUI(paste0("#scenSplit", id, "_content"), where = "afterBegin",
               generateScenarioTabsetSplit(id + 1L), immediate = TRUE)
    }
    showEl(session, paste0("#scenSplit", id, "_content"))
    hideEl(session, paste0("#scenSplit", id, "_open"))
  })
}else{
  scenCounter <- scenCounterMultiComp
  noData <- vapply(scenTableNamesToDisplay, function(sheetName){
    tabData <- getScenTabData(sheetName)
    if(identical(nrow(scenData[[scenIdLong]][[tabData$scenTableId]]), 0L)){
      return(TRUE)
    }else{
      return(FALSE)
    }
  }, logical(1L))
  newScenTabPanel <- generateScenarioTabsetMulti(scenId, noData, scenCounter = scenCounter)
  # add new Scenario tab
  insertScenTab("scenTabset", newScenTabPanel, "scen_add", "before", 
                scenID = scenId, scenButtonLang = c(list(tooltip = lang$nav$scen$tooltips$btClose),
                                                    lang$nav[["dialogCloseScen"]]))
  numberScenTabs <<- numberScenTabs + 1L
  if(numberScenTabs == 1L){
    hideEl(session, "#no-scen")
  }else{
    enableEl(session, "#btCompareScen")
  }
}
# generate title and date
output[[paste0("title_", scenId)]] <- renderUI(htmltools::htmlEscape(scenMetaData[[scenIdLong]][[3]][1]))
output[[paste0("date_", scenId)]] <- renderText(as.character(scenMetaData[[scenIdLong]][[4]][1]))
eMsg <- NULL

if(is.null(rendererEnv[[scenIdLong]])){
  rendererEnv[[scenIdLong]] <- new.env(parent = emptyenv())
}else{
  for(el in ls(envir = rendererEnv[[scenIdLong]])){
    if("Observer" %in% class(rendererEnv[[scenIdLong]][[el]])){
      rendererEnv[[scenIdLong]][[el]]$destroy()
    }
  }
}
lapply(scenTableNamesToDisplay, function(sheetName){
  # call render functions
  tryCatch({
    # get sheet configuration information
    tabData      <- getScenTabData(sheetName)
    
    rendererData <- NULL
    if(length(modelOut) >= tabData$scenTableId && 
       length(configGraphsOut[[tabData$scenTableId]]$additionalData)){
      additionalDataIds <- match(configGraphsOut[[tabData$scenTableId]]$additionalData, 
                                 names(modelOut))
      additionalDataIds[is.na(additionalDataIds)] <- match(configGraphsOut[[tabData$scenTableId]]$
                                                             additionalData[is.na(additionalDataIds)],
                                                           inputDsNames) + length(modelOut)
      additionalDataIds <- c(tabData$scenTableId, additionalDataIds)
      rendererData <- scenData[[scenIdLong]][additionalDataIds]
      names(rendererData) <- c(names(modelOut), inputDsNames)[additionalDataIds]
    }
    dataToRender <- scenData[[scenIdLong]][[tabData$scenTableId]]
    attr(dataToRender, "aliases") <- tabData$headerAliases
    callModule(renderData, paste0("tab_", scenCounter, "_", tabData$tabId), 
               type = tabData$graphConfig$outType, 
               data = if(length(rendererData)) rendererData else dataToRender, 
               configData = scalarData[[scenIdLong]], 
               dtOptions = tabData$graphConfig$datatable, graphOptions = tabData$graphConfig$graph, 
               pivotOptions = tabData$graphConfig$pivottable, 
               customOptions = tabData$graphConfig$options,
               roundPrecision = roundPrecision, modelDir = modelDir,
               rendererEnv = rendererEnv[[scenIdLong]], views = views, attachments = attachments)
    callModule(renderData, paste0("table_tab_", scenCounter, "_", tabData$tabId), type = "datatable", 
               data = dataToRender, 
               dtOptions = tabData$graphConfig$datatable, roundPrecision = roundPrecision)
  }, error = function(e) {
    flog.error("Problem rendering graphs for dataset: '%s'. Error message: %s.", sheetName, e)
    eMsg <<- paste(eMsg, sprintf(lang$errMsg$renderGraph$desc, sheetName), sep = "\n")
  })
  # show graph view per default
  showEl(session, "#scenGraph_" %+% scenId %+% "_" %+% tabData$tabId)
  hideEl(session, "#scenTable_" %+% scenId %+% "_" %+% tabData$tabId)
})

if(!is.null(eMsg)){
  stop(eMsg, call. = FALSE)
}
flog.trace("New scenario tab added in scenario comparison mode: %s.", scenMetaData[[scenIdLong]][[3]][1])

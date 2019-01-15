# generate tab panel for new scenario
noData <- vector("logical", length = length(scenTableNamesToDisplay))
if(isInSplitView){
  scenCounter <- scenId
  # hide button and show content
  local({
    id <- if(loadInLeftBoxSplit) 1L else 2L
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
  appendTab("scenTabset", newScenTabPanel)
  numberScenTabs <<- numberScenTabs + 1
  if(numberScenTabs == 1){
    hideEl(session, "#no-scen")
  }else{
    enableEl(session, "#btCompareScen")
  }
}
# generate title and date
output[[paste0("title_", scenId)]] <- renderText(scenMetaData[[scenIdLong]][[2]][1])
output[[paste0("date_", scenId)]] <- renderText(as.character(scenMetaData[[scenIdLong]][[3]][1]))
eMsg <- NULL
for(sheetName in scenTableNamesToDisplay){
  # get sheet configuration information
  tabData <- getScenTabData(sheetName)
  # call render functions
  tryCatch({
    callModule(renderData, "tab_" %+% scenCounter %+% "_" %+% tabData$tabId, type = tabData$graphConfig$outType, 
               data = scenData[[scenIdLong]][[tabData$scenTableId]], configData = scalarData[[scenIdLong]], 
               dtOptions = config$datatable, graphOptions = tabData$graphConfig$graph, 
               pivotOptions = tabData$graphConfig$pivottable, 
               customOptions = tabData$graphConfig$options,
               roundPrecision = roundPrecision, modelDir = modelDir)
    callModule(renderData, "table_tab_" %+% scenCounter %+% "_" %+% tabData$tabId, type = "datatable", 
               data = scenData[[scenIdLong]][[tabData$scenTableId]], 
               dtOptions = config$datatable, roundPrecision = roundPrecision)
  }, error = function(e) {
    flog.error("Problem rendering graphs for dataset: '%s'. Error message: %s.", sheetName, e)
    eMsg <<- paste(eMsg, sprintf(lang$errMsg$renderGraph$desc, sheetName), sep = "\n")
  })
  # show graph view per default
  showEl(session, "#scenGraph_" %+% scenId %+% "_" %+% tabData$tabId)
  hideEl(session, "#scenTable_" %+% scenId %+% "_" %+% tabData$tabId)
}

if(!is.null(eMsg)){
  stop(eMsg, call. = F)
}
flog.trace("New scenario tab added in scenario comparison mode: %s.", scenMetaData[[scenIdLong]][[2]][1])


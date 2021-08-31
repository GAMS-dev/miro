observe({
  i <- as.integer(strsplit(input$outputTabset, "_")[[1]][2])
  if (i > length(outputTabTitles)) {
    return(hideEl(session, "#btSymbolLink"))
  }
  if (length(outputTabTitles[[i]]) > 1L) {
    j <- as.integer(strsplit(input[[paste0("outputTabset_", i)]], "_")[[1]][3])
    i <- outputTabs[[i]][j]
  } else {
    i <- outputTabs[[i]][1]
  }
  if (is.na(i)) {
    flog.error("Output tabset set to invalid value. This looks like an attempt to tamper with the app!")
    return(NULL)
  }
  if (length(modelOut[[i]]$symbolLink)) {
    return(showEl(session, "#btSymbolLink"))
  }
  return(hideEl(session, "#btSymbolLink"))
})
observeEvent(input$btSymbolLink, {
  if (!length(modelOut)) {
    # no output sheets available
    return(NULL)
  }
  i <- as.integer(strsplit(input$outputTabset, "_")[[1]][2])
  if (length(outputTabs[[i]]) > 1L) {
    j <- as.integer(strsplit(input[[paste0("outputTabset_", i)]], "_")[[1]][3])
    i <- outputTabs[[i]][j]
  } else {
    i <- outputTabs[[i]][1]
  }
  if (is.na(i) || i < 1L || i > length(modelOut)) {
    flog.error("Symbol link button for symbol that doesn't exist clicked. This is most likely an attempt to tamper with the application!")
    return(NULL)
  }
  flog.debug("Symbol link button of output sheet: '%s' clicked", names(modelOut)[i])
  if (!length(modelOut[[i]]$symbolLink)) {
    flog.error("Symbol link button clicked for symbol with no symbol link defined. This looks like an attempt to tamper with the app!")
    return(NULL)
  }
  overwriteInput <- 1L
  errMsg <- NULL
  loadMode <- "scen"
  newInputCount <- 0L

  scenInputData <- scenData$get("sb", names(modelOut)[i], drop = TRUE)
  inputId <- match(modelOut[[i]]$symbolLink, names(modelIn))
  names(scenInputData) <- names(modelIn[[inputId]]$headers)
  scenInputData <- list(scenInputData)
  names(scenInputData) <- modelOut[[i]]$symbolLink
  datasetsToFetch <- modelOut[[i]]$symbolLink
  dfClArgs <- NULL
  source("./modules/input_load.R", local = TRUE)
  markUnsaved(markDirty = TRUE)
  if (!is.null(errMsg)) {
    return(NULL)
  }

  switchTab(session, "input")
  inputTabId <- tabSheetMap$input[[inputId]]
  updateTabsetPanel(session, "inputTabset", paste0("inputTabset_", inputTabId[1]))
  if (length(outputTabTitles) > 1L) {
    updateTabsetPanel(
      session, paste0("inputTabset", inputTabId[1]),
      paste0(
        "inputTabset", inputTabId[1], "_",
        inputTabId[2]
      )
    )
  }
})

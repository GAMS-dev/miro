resetCompTabset <- function(tabsetId) {
  updateTabsetPanel(
    session, paste0("contentScen_", tabsetId),
    paste0("contentScen_", tabsetId, "_1")
  )
  if (isGroupOfSheets[[1]]) {
    updateTabsetPanel(
      session, paste0("contentScen_", tabsetId, "_1"),
      paste0("contentScen_", tabsetId, "_1_1")
    )
  }
}

observeEvent(input$btSplitView, {
  switchCompareMode(session, input$btSplitView, numberScenTabs, config[["customCompareModules"]])
  if (identical(input$btSplitView, "tabView")) {
    currentCompMode <<- "tab"
  } else if (identical(input$btSplitView, "pivotView")) {
    currentCompMode <<- "pivot"
  } else {
    currentCompMode <<- "split"
  }
  if (isInCompareMode) {
    rv$btCompareScen <- isolate(rv$btCompareScen + 1L)
  }
})
observeEvent(input$btScenSplit1_open, {
  flog.debug("Load Scenario button clicked (left box in split view).")
  loadInLeftBoxSplit <<- TRUE

  updateTabsetPanel(session, "contentScen_2", "contentScen_2_1")
  if (isGroupOfSheets[[1]]) {
    updateTabsetPanel(session, "contentScen_2_1", "contentScen_2_1_1")
  }
  rv$btLoadScen <<- isolate(rv$btLoadScen + 1)
})
observeEvent(input$btScenSplit2_open, {
  flog.debug("Load Scenario button clicked (right box in split view).")
  loadInLeftBoxSplit <<- FALSE

  updateTabsetPanel(session, "contentScen_3", "contentScen_3_1")
  if (isGroupOfSheets[[1]]) {
    updateTabsetPanel(session, "contentScen_3_1", "contentScen_3_1_1")
  }
  rv$btLoadScen <<- isolate(rv$btLoadScen + 1)
})
loadSandboxScen <- function(scenId, refresh = FALSE) {
  if (tryCatch(
    {
      scenData$loadSandbox(
        getInputDataFromSandbox(),
        modelInFileNames, activeScen$getMetadataDf()
      )
      FALSE
    },
    no_data = function(e) {
      flog.error(conditionMessage(e))
      showErrorMsg(lang$errMsg$GAMSInput$title, conditionMessage(e))
      return(TRUE)
    },
    error = function(e) {
      flog.error("Unexpected error while fetching input data from sandbox. Error message: '%s'", conditionMessage(e))
      showErrorMsg(lang$errMsg$GAMSInput$title, lang$errMsg$unknownError)
      return(TRUE)
    }
  )) {
    return()
  }
  views$duplicateSandboxConf(scenId)
  scenData$load("sb", refId = tabIdToRef(scenId))
  renderScenInCompMode(scenId, refreshData = refresh)
  # load script results
  if (length(config$scripts$base)) {
    scriptOutput$loadResultsBase(scriptOutput$getResults(),
      scenId = scenId
    )
  }
}
observeEvent(input$loadActiveScenSplitComp, {
  flog.debug(
    "Load sandbox scenario to split comparison mode clicked ID: '%s'.",
    isolate(input$loadActiveScenSplitComp)
  )
  id <- suppressWarnings(as.integer(isolate(input$loadActiveScenSplitComp)))
  showEl(session, "#loading-screen")
  on.exit(hideEl(session, "#loading-screen"))
  if (identical(id, 2L)) {
    loadInLeftBoxSplit <<- TRUE
    if (!dynamicUILoaded$compareModeTabsets[1]) {
      dynamicUILoaded$compareModeTabsets[1] <<- TRUE
      insertUI("#scenSplit1_content",
        where = "afterBegin",
        generateScenarioTabsetSplit(2), immediate = TRUE
      )
    }
  } else if (identical(id, 3L)) {
    loadInLeftBoxSplit <<- FALSE
    if (!dynamicUILoaded$compareModeTabsets[2]) {
      dynamicUILoaded$compareModeTabsets[2] <<- TRUE
      insertUI("#scenSplit2_content",
        where = "afterBegin",
        generateScenarioTabsetSplit(3), immediate = TRUE
      )
    }
  } else {
    flog.error("Button ID (load active scenario to split comp) has invalid value: '%s'. This should never happen!
               User most likely tried to tamper with the app.", isolate(input$loadActiveScenSplitComp))
    return()
  }
  loadSandboxScen(id, refresh = FALSE)
})
closeScenSplitBox <- function(tabsetId) {
  tabsetIdChar <- as.character(tabsetId)
  if (!is.null(dynamicUILoaded$dynamicTabsets[[paste0("tab_", tabsetIdChar)]])) {
    dynamicUILoaded$dynamicTabsets[[paste0("tab_", tabsetIdChar)]][["content"]][] <<- FALSE
  }
  views$clearConf(tabsetIdChar)
  scenData$clear(if (identical(tabsetId, 2L)) "cmpSplitL" else "cmpSplitR")

  # show button and hide content
  resetCompTabset(tabsetIdChar)
  hideEl(session, paste0("#cmpScenTitle_", tabsetIdChar))
  hideEl(session, paste0("#scenSplit", tabsetId - 1L, "_content"))
  showEl(session, paste0("#scenSplit", tabsetId - 1L, "_open"))
}
observeEvent(input$btScenSplit1_close, {
  flog.debug("Close Scenario button clicked (left box in split view).")
  closeScenSplitBox(2L)
})
observeEvent(input$btScenSplit2_close, {
  flog.debug("Close Scenario button clicked (right box in split view).")
  closeScenSplitBox(3L)
})

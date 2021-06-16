# load scenario from database
checkIfInputDataExists <- function(){
  inputDatasetsExist <- vapply(seq_along(modelIn), function(i){
    if(length(isolate(rv[["in_" %+% i]]))){
      return(TRUE)
    }else{
      return(FALSE)
    }
  }, logical(1), USE.NAMES = FALSE)
  
  if(any(inputDatasetsExist)){
    hideEl(session, "#importDataTabset")
    showEl(session, "#btOverwriteScen")
    showEl(session, "#importDataOverwrite")
  }else{
    overwriteInput <<- FALSE
    rv$btOverwriteScen <<- isolate(rv$btOverwriteScen + 1L)
  }
}
observeEvent(input$btLoadScen, {
  flog.debug("Load Scenario button clicked (multiple scenarios view).")
  rv$btLoadScen <<- isolate(rv$btLoadScen + 1L)
})

observeEvent(input$selLoadScenTags, {
  oderByIdentifier <- if(btSortTime) "_stime" else "_sname"
  desc <- if(btSortNameDesc || btSortTimeDesc) TRUE else FALSE
  if(!length(input$selLoadScenTags)){
    if(length(scenMetaDb) && nrow(scenMetaDb) > maxNoScenToShow){
      scenMetaDbSubset <<- scenMetaDb[order(scenMetaDb[[oderByIdentifier]], 
                                            decreasing = desc), ][seq_len(maxNoScenToShow), ]
      showHideEl(session, "#importScenMaxNoScen", 4000L)
    }else{
      scenMetaDbSubset <<- scenMetaDb
    }
    updateSelectInput(session, "selLoadScen", 
                      choices = formatScenList(scenMetaDbSubset, uid, oderByIdentifier, 
                                               desc = desc))
    return()
  }
  scenMetaDbSubset <<- scenMetaDb[vapply(scenMetaDb[["_stag"]], function(tags){
    any(csv2Vector(tags) %in% input$selLoadScenTags)}, logical(1L), USE.NAMES = FALSE), ]
  updateSelectInput(session, "selLoadScen", 
                    choices = formatScenList(scenMetaDbSubset, uid, 
                                             oderByIdentifier, desc = desc))
}, ignoreNULL = FALSE)

#load scenario button clicked
observeEvent(virtualActionButton(rv$btLoadScen), {
  # fetch list of saved scenarios
  # only load single scenario as not in comparison mode
  loadIntoSandbox <<- FALSE
  errMsg <- NULL
  tryCatch({
    scenMetaDb <<- db$fetchScenList(scode = SCODEMAP[['scen']])
  }, error = function(e){
    flog.error("Problems fetching list of scenarios from database. Error message: %s.",
               conditionMessage(e))
    errMsg <<- lang$errMsg$fetchScenData$desc
  })
  if(is.null(showErrorMsg(lang$errMsg$fetchScenData$title, errMsg))){
    return()
  }
  if(is.null(scenMetaDb) || !nrow(scenMetaDb)){
    scenMetaDb <<- tibble(`_sid` = integer(),
                          `_uid` = character(),
                          `_sname` = character(),
                          `_stime` = character(),
                          `_stag` = character(),
                          `_accessr` = character(),
                          `_accessw` = character(),
                          `_accessx` = character(),
                          `_scode` = integer())
  }
  # fetch only those scenarios that are not already loaded into the ui
  uiSidList <- scenData$getRefScenMap()
  uiSidList[["sb"]] <- NULL
  includeSandboxScen <- FALSE
  if(!LAUNCHHCUBEMODE && identical(currentCompMode, "pivot")){
    baseScenName <- paste(if(length(rv$activeSname)) rv$activeSname else lang$nav$dialogNewScen$newScenName, lang$nav$scen$scenNameSandboxSuffix)
  }else{
    baseScenName <- NULL
  }
  if(length(names(uiSidList))){
    if(identical(currentCompMode, "split")){
      splitCompRefIds <- names(uiSidList) %in% c("cmpSplitL", "cmpSplitR")
      scenMetaDb <<- scenMetaDb[!as.character(scenMetaDb[[1]]) %in% unlist(uiSidList[splitCompRefIds],
                                                                           use.names = FALSE), ]
      uiSidList <- unique(unlist(uiSidList[!splitCompRefIds],
                                 use.names = FALSE))
    }else if(identical(currentCompMode, "pivot")){
      sidsLoadedInPivotMode <- uiSidList[["cmpPivot"]]
      if(length(sidsLoadedInPivotMode)){
        uiSidList <- sidsLoadedInPivotMode
      }else{
        scenMetaDb <<- scenMetaDb[!as.character(scenMetaDb[[1]]) %in% uiSidList[["cmpPivot"]], ]
        uiSidList <- unique(unlist(uiSidList[names(uiSidList) != "cmpPivot"],
                                   use.names = FALSE))
      }
    }else{
      tabRefs <- startsWith(names(uiSidList), "cmpTab_")
      sidsAlreadyLoaded <- unlist(uiSidList[tabRefs], use.names = FALSE)
      includeSandboxScen <- !any(startsWith(as.character(sidsAlreadyLoaded), "sb"))
      scenMetaDb <<- scenMetaDb[!as.character(scenMetaDb[[1]]) %in% sidsAlreadyLoaded, ]
      uiSidList <- unique(unlist(uiSidList[!tabRefs], use.names = FALSE))
    }
  }else{
    uiSidList <- integer()
    includeSandboxScen <- identical(currentCompMode, "tab")
  }
  
  maxNoScenExceeded <- FALSE
  if(nrow(scenMetaDb) > maxNoScenToShow){
    scenMetaDbSubset <<- scenMetaDb[order(scenMetaDb[["_stime"]], 
                                          decreasing = TRUE), ][seq_len(maxNoScenToShow), ]
    maxNoScenExceeded <- TRUE
  }else{
    scenMetaDbSubset <<- scenMetaDb
    maxNoScenExceeded <- FALSE
  }
  dbTagList <- scenMetaDb[["_stag"]]
  dbTagList <- csv2Vector(dbTagList[dbTagList != ""])
  if(!LAUNCHHCUBEMODE && includeSandboxScen){
    sandboxMeta <- tibble(`_sid` = "sb",
                          `_uid` = uid,
                          `_sname` = paste(if(length(rv$activeSname))
                            rv$activeSname else lang$nav$dialogNewScen$newScenName,
                            lang$nav$scen$scenNameSandboxSuffix),
                          `_stime` = Sys.time(),
                          `_stag` = "",
                          `_accessr` = paste0(",", uid, ","),
                          `_accessw` = paste0(",", uid, ","),
                          `_accessx` = paste0(",", uid, ","),
                          `_scode` = 0)
    if(is.character(scenMetaDb[["_stime"]])){
      sandboxMeta[["_stime"]] <- as.character(sandboxMeta[["_stime"]])
    }
    scenMetaDbSubset <- bind_rows(mutate(scenMetaDbSubset, `_sid` = as.character(`_sid`)),
                                  sandboxMeta)
  }
  if(!is.null(uiSidList)){
    uiSidList <- uiSidList[!startsWith(as.character(uiSidList), "sb")]
    if(length(uiSidList)){
      uiSidList <- bind_rows(scenData$getById("meta", scenIds = uiSidList))
      if(LAUNCHHCUBEMODE){
        # Hypercube scenarios are not part of dbSidList, so we need to append them
        scenMetaDbSubset <- bind_rows(scenMetaDbSubset,
                                      uiSidList[uiSidList[["_scode"]] != SCODEMAP["scen"], ])
      }
      if(identical(currentCompMode, "split")){
        uiSidList <- formatScenList(uiSidList,
                                    uid, "_stime", desc = TRUE)
      }else{
        uiSidList <- paste0(uiSidList[["_sid"]], "_", uiSidList[["_uid"]])
      }
    }else{
      uiSidList <- NULL
    }
  }
  # by default, put most recently saved scenario first
  dbSidList <- formatScenList(scenMetaDbSubset, uid, "_stime", desc = TRUE)
  if(length(dbSidList) || length(uiSidList)){
    showLoadScenDialog(dbSidList, uiSidList, identical(currentCompMode, "split"),
                       dbTagList = dbTagList, baseScenName = baseScenName)
    if(maxNoScenExceeded)
      showHideEl(session, "#importScenMaxNoScen", 4000L)
  }else{
    return(showErrorMsg(lang$nav$dialogLoadScen$titleNoScen, 
                        lang$nav$dialogLoadScen$descNoScen))
  }
})

# sort by name
observeEvent(input$btSortName, {
  flog.debug("Button to sort scenarios by name clicked (%s).", 
             if(btSortNameDesc) "ascending order" else "descending order")
  removeClassEl(session, "#btSortTime", "scen-sort-by-selected")
  addClassEl(session, "#btSortName", "scen-sort-by-selected")
  btSortTime <<- FALSE
  if(btSortNameDesc){
    updateSelectInput(session, "selLoadScen", 
                      choices = formatScenList(scenMetaDbSubset, 
                                               uid, "_sname", 
                                               desc = FALSE))
    updateActionButton(session, "btSortName", 
                       label = lang$nav$dialogLoadScen$btSortNameDESC, 
                       icon = icon("sort-alpha-down-alt"))
    btSortNameDesc <<- FALSE
  }else{
    updateSelectInput(session, "selLoadScen", 
                      choices = formatScenList(scenMetaDbSubset, 
                                               uid, "_sname", desc = TRUE))
    updateActionButton(session, "btSortName", 
                       label = lang$nav$dialogLoadScen$btSortNameASC, 
                       icon = icon("sort-alpha-down"))
    btSortNameDesc <<- TRUE
  }
  
})
# sort by time
observeEvent(input$btSortTime, {
  flog.debug("Button to sort scenarios by time clicked (%s).", 
             if(btSortTimeDesc) "ascending order" else "descending order")
  removeClassEl(session, "#btSortName", "scen-sort-by-selected")
  addClassEl(session, "#btSortTime", "scen-sort-by-selected")
  btSortTime <<- TRUE
  if(btSortTimeDesc){
    updateSelectInput(session, "selLoadScen", 
                      choices = formatScenList(scenMetaDbSubset, 
                                               uid, "_stime", desc = FALSE))
    updateActionButton(session, "btSortTime", 
                       label = lang$nav$dialogLoadScen$btSortTimeDESC, 
                       icon = icon("sort-numeric-down-alt"))
    btSortTimeDesc <<- FALSE
  }else{
    updateSelectInput(session, "selLoadScen", choices = formatScenList(
      scenMetaDbSubset, uid, "_stime", desc = TRUE))
    updateActionButton(session, "btSortTime", 
                       label = lang$nav$dialogLoadScen$btSortTimeASC, 
                       icon = icon("sort-numeric-down"))
    btSortTimeDesc <<- TRUE
  }
})

observeEvent(input$btRefreshComp, {
  if(LAUNCHHCUBEMODE){
    flog.error("Refresh scenario in compare mode triggered while Hypercube is launched. This should never happen and is likely an attempt to tamper with the app!")
    return()
  }
  tabsetId <- input$btRefreshComp
  if(identical(tabsetId, 0L)){
    flog.debug("Refresh scenario in pivot compare mode clicked.")
  }else if(tabsetId %in% c(2L, 3L)){
    flog.debug("Refresh scenario in split compare mode clicked.")
  }else{
    flog.debug("Refresh scenario in tab compare mode clicked.")
  }
  refId <- tabIdToRef(tabsetId)
  scenData$clear(refId, clearRef = FALSE)
  if(!is.null(dynamicUILoaded$dynamicTabsets[[paste0("tab_", tabsetId)]])){
    dynamicUILoaded$dynamicTabsets[[paste0("tab_", tabsetId)]][["content"]][] <<- FALSE
  }
  scenIds <- scenData$getRefScenMap(refId)
  sbScenId <- startsWith(as.character(scenIds), "sb")
  if(any(sbScenId)){
    if(tryCatch({
      scenData$loadSandbox(getInputDataFromSandbox(saveInputDb = TRUE),
                           modelInFileNames, activeScen$getMetadata())
      FALSE
    }, no_data = function(e){
      flog.error(conditionMessage(e))
      showErrorMsg(lang$errMsg$GAMSInput$title, conditionMessage(e))
      return(TRUE)
    }, error = function(e){
      flog.error("Unexpected error while fetching input data from sandbox. Error message: '%s'", conditionMessage(e))
      showErrorMsg(lang$errMsg$GAMSInput$title, lang$errMsg$unknownError)
      return(TRUE)
    })){
      return()
    }
    views$duplicateSandboxConf(tabsetId)
  }
  sidsToLoad <- as.integer(scenIds[!sbScenId])
  if(length(config$scripts$base) && !identical(tabsetId, 0L)){
    if(any(sbScenId)){
      scriptOutput$loadResultsBase(scriptOutput$getResults(), tabsetId)
    }else if(length(sidsToLoad)){
      scriptOutput$loadResultsBase(db$loadScriptResults(sidsToLoad)[[1]], tabsetId)
    }
  }
  # initialize metadata
  scenData$get(refId, symNames = character(), showProgress = FALSE)
  if(length(sidsToLoad)){
    inputDataSids <- scenData$getInputDataSids(sidsToLoad)
    inputDataSids[is.na(inputDataSids)] <- sidsToLoad[is.na(inputDataSids)]
    views$loadConf(db$importDataset(tableName = "_scenViews", 
                                    subsetSids = inputDataSids), FALSE,
                   tabsetId, inputDataSids)
  }
  sheetNames <- getSheetnamesByTabsetId(tabsetId)
  if(length(sheetNames)){
    loadDynamicTabContent(session, tabsetId,
                          sheetNames,
                          initEnv = TRUE)
  }
  metaTmp <- scenData$getById("meta", refId = refId, drop = TRUE)
  showElReplaceTxt(session, paste0("#cmpScenTitle_", tabsetId),
                   paste0(if(!identical(uid, metaTmp[["_uid"]][1])) paste0(metaTmp[["_uid"]][1], ": "),
                          metaTmp[["_sname"]][1]))
  showElReplaceTxt(session, paste0("#cmpScenDate_", tabsetId),
                   metaTmp[["_stime"]][1])
})

# load scenario confirmed
observeEvent(input$btLoadScenConfirm, {
  flog.debug("Confirm load scenario button clicked.")
  sandboxScenId <- NULL
  if(identical(isolate(input$sidebarMenuId), "scenarios") &&
     identical(isolate(input$tabsetLoadScen), "loadScenUI")){
    scenSelected <- isolate(input$selLoadScenUI)
  }else{
    scenSelected <- isolate(input$selLoadScen)
    if(!isInSolveMode && !LAUNCHHCUBEMODE && identical(currentCompMode, "pivot")){
      scenSelected <- c("sb", scenSelected)
    }
  }
  if(!length(scenSelected)){
    return()
  }
  loadIntoSandbox <<- FALSE
  sidsToLoad  <<- lapply(strsplit(scenSelected, split = "_", fixed = TRUE), '[[', 1)
  
  # if in comparison mode skip input data check
  if(!isInSolveMode){
    rv$btOverwriteScen <<- rv$btOverwriteScen + 1L
    return()
  }
  checkIfInputDataExists()
})

observeEvent(input$btOverwriteScen, {
  flog.debug("Overwrite scenario button clicked.")
  if(!length(sidsToLoad)){
    return()
  }
  overwriteInput <<- TRUE
  rv$btOverwriteScen <<- rv$btOverwriteScen + 1L
})

observeEvent(input$btBatchLoadSbOverwrite, {
  flog.debug("Batch load: overwriting current content in sandbox confirmed.")
  overwriteInput <<- TRUE
  switchTab(session, "input")
  rv$btOverwriteScen <<- rv$btOverwriteScen + 1L
})

observeEvent(input$btBatchLoadSb, {
  flog.debug("Load scenario into sandbox from batch load module button clicked.")
  isInSolveMode <<- TRUE
  loadIntoSandbox <<- FALSE
  if(any(vapply(seq_along(modelIn), function(i){
    if(length(isolate(rv[["in_" %+% i]]))){
      return(TRUE)
    }else{
      return(FALSE)
    }
  }, logical(1), USE.NAMES = FALSE))){
    hideEl(session, ".batch-load-content")
    showEl(session, ".batch-load-sb-content")
  }else{
    switchTab(session, "input")
    rv$btOverwriteScen <<- rv$btOverwriteScen + 1L
  }
})

observeEvent(input$btBatchCompare, {
  if(identical(input$btBatchCompare, "pivot")){
    viewMode <- "pivotView"
    currentCompMode <<- "pivot"
  }else if(identical(input$btBatchCompare, "tab")){
    viewMode <- "tabView"
    currentCompMode <<- "tab"
  }else{
    viewMode <- "splitView"
    currentCompMode <<- "split"
    sidInLeftSplit <- length(scenData$getRefScenMap("cmpSplitL"))
    sidInRightSplit <- length(scenData$getRefScenMap("cmpSplitR"))
    if(length(sidsToLoad) > (2L - sum(sidInLeftSplit, sidInRightSplit))){
      flog.info("Closing currently open scenarios in split comparison mode to load new ones.")
      loadInLeftBoxSplit <<- TRUE
      closeScenSplitBox(2L)
      if(identical(length(sidsToLoad), 2L)){
        closeScenSplitBox(3L)
      }
    }else{
      loadInLeftBoxSplit <<- identical(sidInLeftSplit, 0L)
    }
  }
  flog.debug("Load batch of scenarios to compare mode (%s) button clicked.", viewMode)
  isInSolveMode <<- FALSE
  loadIntoSandbox <<- FALSE
  switchCompareMode(session, viewMode, length(sidsToLoad))
  rv$btOverwriteScen <<- rv$btOverwriteScen + 1L
})

observeEvent(virtualActionButton(rv$btOverwriteScen), {
  flog.debug("Loading and rendering scenarios: '%s'.",
             paste(sidsToLoad, collapse = ", "))
  suppressCloseModalLocal <- suppressCloseModal
  suppressCloseModal      <<- FALSE
  if(!length(sidsToLoad)){
    return()
  }
  sidsToLoadVector <- unlist(sidsToLoad, use.names = FALSE)
  
  if(sum(!occupiedSidSlots) < length(sidsToLoad)){
    flog.info("Maximum number of scenarios in scenario comparison mode reached (%d).", 
              length(occupiedSidSlots))
    return(showErrorMsg(lang$errMsg$maxScen$title, lang$errMsg$maxScen$desc))
  }
  if(identical(currentCompMode, "pivot") && isInRefreshMode){
    on.exit(hideEl(session, "#loading-screen"), add = TRUE)
  }
  if(isInSolveMode){
    refId <- "sb"
    viewsSids <- 1L
    symToFetch <- NULL
  }else{
    # get names of symbols on first tab
    if(length(outputTabs)){
      symToFetch <- names(modelOut)[outputTabs[[1]]]
    }else{
      symToFetch <- scenInputTabs[[1]]
      if(identical(sheetName, 0L)){
        symToFetch <- scalarsFileName
      }else{
        symToFetch <- names(modelIn)[symToFetch]
      }
    }
    if(identical(currentCompMode, "pivot")){
      refId <- "cmpPivot"
      viewsSids <- 0L
      # check if we are modifying already opened scenarios
      sidsInPivotComp <- scenData$getRefScenMap("cmpPivot")
      if(length(sidsInPivotComp)){
        if(!is.null(dynamicUILoaded$dynamicTabsets[["tab_0"]])){
          dynamicUILoaded$dynamicTabsets[["tab_0"]][["content"]][] <<- FALSE
        }
        sidsToRemoveFromPivotComp <- !sidsInPivotComp %in% c(sidsToLoadVector, "sb_cmpPivot")
        if(any(sidsToRemoveFromPivotComp)){
          scenData$clear(refId, sidsInPivotComp[sidsToRemoveFromPivotComp])
        }
        if(isGroupOfSheets[[1]]){
          tabId <- 1
        }
        symToFetch <- getSheetnamesByTabsetId("0")
      }
    }else if(identical(currentCompMode, "tab")){
      refId <- NULL
      viewsSids <- which(!occupiedSidSlots)[seq_along(sidsToLoadVector)] + 3
    }else if(identical(currentCompMode, "split")){
      if(identical(length(sidsToLoadVector), 2L)){
        refId <- NULL
        viewsSids <- c(2L, 3L)
      }else if(loadInLeftBoxSplit){
        refId <- "cmpSplitL"
        viewsSids <- 2L
      }else{
        refId <- "cmpSplitR"
        viewsSids <- 3L
      }
    }else{
      flog.error("Invalid state (not in solve mode, and currentCompMode is neither: pivot, tab nor split. Please contact GAMS!.")
      return(showErrorMsg(lang$errMsg$loadScen$title, lang$errMsg$unknownError))
    }
  }
  errMsg <- NULL
  tryCatch({
    scriptDataTmp <- NULL
    sandboxId <- 1L
    if("sb" %in% sidsToLoadVector){
      if(tryCatch({
        scenData$loadSandbox(getInputDataFromSandbox(saveInputDb = TRUE),
                             modelInFileNames, activeScen$getMetadata())
        if(length(refId)){
          # there are multiple ref ids in tab comparison mode.
          scenData$addRefId(refId, "sb")
        }
        FALSE
      }, no_data = function(e){
        flog.error(conditionMessage(e))
        showErrorMsg(lang$errMsg$GAMSInput$title, conditionMessage(e))
        return(TRUE)
      }, error = function(e){
        flog.error("Unexpected error while fetching input data from sandbox. Error message: '%s'", conditionMessage(e))
        showErrorMsg(lang$errMsg$GAMSInput$title, lang$errMsg$unknownError)
        return(TRUE)
      })){
        return()
      }
      sandboxId <- which("sb" == sidsToLoadVector)[1]
      sidsToLoadVector <- sidsToLoadVector[-sandboxId]
      views$duplicateSandboxConf(viewsSids[sandboxId])
      if(length(config$scripts$base)){
        scriptDataTmp <- scriptOutput$getResults()
      }
      viewsSids <- viewsSids[-sandboxId]
      snameTmp <- if(length(rv$activeSname)) rv$activeSname
      else lang$nav$dialogNewScen$newScenName
      snameTmp <- paste(snameTmp, lang$nav$scen$scenNameSandboxSuffix)
    }
    if(length(sidsToLoadVector)){
      if(length(refId)){
        # there are multiple ref ids in tab comparison mode.
        scenData$load(as.integer(sidsToLoadVector), refId = refId,
                      symNames = symToFetch,
                      isHcJobConfig = LAUNCHHCUBEMODE && isInSolveMode)
        if(identical(refId, "sb")){
          inputDataSids <- sidsToLoadVector
        }else{
          inputDataSids <- scenData$getInputDataSids(sidsToLoadVector)
          inputDataSids[is.na(inputDataSids)] <- sidsToLoadVector[is.na(inputDataSids)]
        }
        views$loadConf(db$importDataset(tableName = "_scenViews", 
                                        subsetSids = inputDataSids), isInSolveMode,
                       viewsSids, inputDataSids)
      }
      scriptDataTmp <- append(db$loadScriptResults(sidsToLoadVector,
                                                   msgProgress = lang$progressBar$loadScenDb),
                              scriptDataTmp, sandboxId - 1)
    }
  }, error = function(e){
    flog.error("Some error occurred loading scenarios: '%s' from database. Error message: %s.", 
               paste(sidsToLoad, collapse = ", "), conditionMessage(e))
    errMsg <<- lang$errMsg$loadScen$desc
  })
  if(is.null(showErrorMsg(lang$errMsg$loadScen$title, errMsg))){
    return()
  }
  
  if(isInSolveMode){
    # close currently opened scenario
    resetWidgetsOnClose <<- FALSE
    if(!closeScenario(clearMeta = FALSE)){
      return()
    }
    if(!loadIntoSandbox){
      tryCatch({
        activeScen <<- Scenario$new(db = db, sid = sidsToLoad[[1]],
                                    views = views, attachments = attachments)
      },
      error = function(e){
        flog.error("Error generating new Scenario object. Error message: '%s'.",
                   conditionMessage(e))
        errMsg <<- lang$errMsg$loadScen$desc
      })
      if(is.null(showErrorMsg(lang$errMsg$loadScen$title, errMsg))){
        return()
      }
      if(length(activeScen$getLockUid())){
        showNotification(tagList(tags$b(lang$nav$dialogLockScen$title), tags$br(),
                                 tags$span(sprintf(lang$nav$dialogLockScen$desc,
                                                   activeScen$getLockUid()))),
                         duration = 10,
                         type = "warning")
      }
    }
    
    # check whether all input datasets were imported
    if(length(inputDsNames)){
      scenInputData <- scenData$get("sb", symNames = inputDsNames)
    }else{
      scenInputData <- list()
    }
    if(length(inputDsNames) != length(scenInputData)){
      flog.error("Length of data provided (%d) does not match the number of input sheets (%d).",
                 length(scenInputData), length(inputDsNames))
      return(showErrorMsg(lang$nav$dialogLoadScen$titleDataError, 
                          lang$nav$dialogLoadScen$descDataError))
    }
    names(scenInputData) <- inputDsNames
    
    if(scalarsFileName %in% inputDsNames){
      scalarDataset <- scenInputData[[length(scenInputData)]]
    }else{
      scalarDataset <- NULL
    }
    
    errMsg    <-  NULL
    loadMode  <-  "scen"
    newInputCount <- 0L
    datasetsToFetch <- names(modelIn)
    dfClArgs <- NULL
    source("./modules/input_load.R", local = TRUE)
    if(is.null(errMsg) && isFALSE(suppressCloseModalLocal)){
      removeModal()
    }
    
    if(LAUNCHHCUBEMODE){
      noOutputData <<- TRUE
    }else{
      # render output data
      noOutputData <<- !scenData$getSandboxHasOutputData(scriptOutput)
      # rendering tables and graphs
      renderOutputData()
      
      # load script results
      if(length(config$scripts$base)){
        scriptOutput$loadResultsBase(scriptDataTmp[[1]])
      }
    }
    
    if(loadIntoSandbox){
      flog.debug("Scenario data from a scenario with id: '%s' was loaded into sandbox.", 
                 sidsToLoad[[1]])
      markUnsaved()
    }else{
      flog.debug("Scenario: '%s' was loaded into UI", activeScen$getSid())
      # update scenario name
      rv$activeSname  <<- activeScen$getScenName()
    }
    # function ends here in case not in compare mode!
    return()
  }
  # scenario comparison mode
  
  if(identical(currentCompMode, "pivot")){
    if(!dynamicUILoaded$compareModeTabsets[3]){
      dynamicUILoaded$compareModeTabsets[3] <<- TRUE
      insertUI("#pivotCompScenWrapper", where = "afterBegin",
               generateScenarioTabset(0L, pivotCompare = TRUE), immediate = TRUE)
    }
    return(tryCatch({
      loadDynamicTabContent(session, 0L, symToFetch, initEnv = TRUE)
      hideEl(session, "#pivotCompBtWrapper")
      showEl(session, "#pivotCompScenWrapper")
      switchTab(session, "scenComp")
      isInRefreshMode <<- TRUE
      enableEl(session, "#btClosePivotComp")
      removeModal()
      flog.debug("Scenarios: '%s' loaded and rendered in scenario comparison mode (pivot view).", 
                 paste(sidsToLoad, collapse = ", "))
    }, error = function(e){
      flog.warn("Problems rendering scenarios in pivot compare mode. Error message: %s", conditionMessage(e))
      showErrorMsg(lang$errMsg$loadScen$title, lang$errMsg$loadScen$desc)
    }))
  }
  # split/tab comparison mode
  errMsg <- NULL
  lastImportedTabsetId <- NULL
  
  lapply(seq_along(sidsToLoad), function(i){
    tryCatch({
      refIdToLoad <- NULL
      if(identical(currentCompMode, "tab")){
        scenId   <- which.min(occupiedSidSlots) + 3L
        refIdToLoad <- paste0("cmpTab_", scenId)
      }else{
        if(identical(length(sidsToLoad), 2L)){
          scenId <- i + 1L
          scenData$load(sidsToLoad[[i]], symNames = symToFetch,
                        showProgress = TRUE,
                        refId = if(identical(scenId, 2L)) "cmpSplitL" else "cmpSplitR")
        }else if(loadInLeftBoxSplit){
          scenId   <- 2L
        }else{
          scenId   <- 3L
        }
        if(!dynamicUILoaded$compareModeTabsets[scenId - 1L]){
          dynamicUILoaded$compareModeTabsets[scenId - 1L] <<- TRUE
          insertUI(paste0("#scenSplit", scenId - 1L, "_content"), where = "afterBegin",
                   generateScenarioTabsetSplit(scenId), immediate = TRUE)
        }
      }
      if(length(refIdToLoad)){
        scenData$load(sidsToLoad[[i]], symNames = symToFetch,
                      showProgress = TRUE, refId = refIdToLoad)
        inputDataSids <- scenData$getInputDataSids(sidsToLoad[[i]])
        inputDataSids[is.na(inputDataSids)] <- sidsToLoad[[i]][is.na(inputDataSids)]
        views$loadConf(db$importDataset(tableName = "_scenViews", 
                                        subsetSids = inputDataSids), FALSE,
                       viewsSids[[i]], inputDataSids)
      }
      renderScenInCompMode(scenId, refreshData = FALSE)
      # load script results
      if(length(config$scripts$base)){
        scriptOutput$loadResultsBase(scriptDataTmp[[i]], scenId)
      }
    }, error = function(e){
      flog.error(e)
      errMsg  <<- lang$errMsg$loadScen$desc
    })
    
    flog.debug("Scenario: '%s' loaded into UI (compare mode).", sidsToLoad[[i]])
    if(identical(currentCompMode, "tab")){
      lastImportedTabsetId <<- scenId
      occupiedSidSlots[scenId - 3] <<- TRUE
      sidCompOrder <<- c(sidCompOrder, scenId)
    }
  })
  switchTab(session, "scenComp")
  if(identical(currentCompMode, "tab")){
    updateTabsetPanel(session, "scenTabset", selected = paste0("scen_", lastImportedTabsetId, "_"))
  }
  if(is.null(showErrorMsg(lang$errMsg$loadScen$title, errMsg))){
    return()
  }
  
  removeModal()
  flog.debug("Scenarios: '%s' loaded and rendered in scenario comparison mode.", 
             paste(sidsToLoad, collapse = ", "))
  return()
})

if(LAUNCHHCUBEMODE){
  observeEvent(input$tb_importData, {
    if(!identical(isolate(input$tb_importData), "tb_importData_base") ||
       !identical(isolate(input$selLoadScen_base), "")){
      return()
    }
    on.exit(hideEl(session, "#importDataDbSpinner"))
    tryCatch({
      if(!length(scenMetaDbBaseList)){
        scenMetaDbBase <<- db$fetchScenList(scode = SCODEMAP[['scen']])
        if(length(scenMetaDbBase) && nrow(scenMetaDbBase)){
          if(nrow(scenMetaDbBase) > maxNoScenToShow){
            scenMetaDbBaseSubset <<- scenMetaDbBase[order(scenMetaDbBase[["_stime"]], 
                                                          decreasing = TRUE), ][seq_len(maxNoScenToShow), ]
            showHideEl(session, "#importScenMaxNoScen", 4000L)
          }else{
            scenMetaDbBaseSubset <<- scenMetaDbBase
          }
          scenMetaDbBaseList    <<- formatScenList(scenMetaDbBaseSubset, uid, 
                                                   "_stime", desc = TRUE)
          scenMetaDbBaseTagList <<- csv2Vector(scenMetaDbBase[["_stag"]])
        }else{
          showEl(session, "#importDataDbNoContent")
          return()
        }
      }
      updateSelectInput(session, "selLoadScen_base",
                        choices = scenMetaDbBaseList)
      if(length(scenMetaDbBaseTagList)){
        updateSelectInput(session, "selLoadScenTags_base", 
                          choices = scenMetaDbBaseTagList)
      }else{
        hideEl(session, "#selLoadScenTagsDiv_base")
      }
      showEl(session, "#importDataDbContent")
      return()
    }, error = function(e){
      flog.error("Problems fetching list of base-mode-scenarios from database. Error message: %s.",
                 conditionMessage(e))
      showEl(session, "#importDataDbUnknownError")
    })
  })
  observeEvent(input$selLoadScenTags_base, {
    oderByIdentifier <- if(btSortTimeBase) "_stime" else "_sname"
    desc <- if(btSortNameDesc || btSortTimeDesc) TRUE else FALSE
    if(!length(input$selLoadScenTags_base)){
      if(length(scenMetaDbBase) && nrow(scenMetaDbBase) > maxNoScenToShow){
        scenMetaDbBaseSubset <<- scenMetaDbBase[order(scenMetaDbBase[[oderByIdentifier]], 
                                                      decreasing = desc), ][seq_len(maxNoScenToShow), ]
        showHideEl(session, "#importScenMaxNoScen", 4000L)
      }else{
        scenMetaDbBaseSubset <<- scenMetaDbBase
      }
      updateSelectInput(session, "selLoadScen_base", 
                        choices = formatScenList(scenMetaDbBaseSubset, oderByIdentifier, uid, 
                                                 desc = desc))
      return()
    }
    scenMetaDbBaseSubset <<- scenMetaDbBase[vapply(scenMetaDbBase[["_stag"]], function(tags){
      any(csv2Vector(tags) %in% input$selLoadScenTags_base)}, logical(1L), USE.NAMES = FALSE), ]
    updateSelectInput(session, "selLoadScen_base", 
                      choices = formatScenList(scenMetaDbBaseSubset, uid, 
                                               oderByIdentifier, desc = desc))
  }, ignoreNULL = FALSE)
  # sort by name
  observeEvent(input$btSortName_base, {
    flog.debug("Button to sort base mode scenarios by name clicked (%s).", 
               if(btSortNameDescBase) "ascending order" else "descending order")
    removeClassEl(session, "#btSortTime_base", "scen-sort-by-selected")
    addClassEl(session, "#btSortName_base", "scen-sort-by-selected")
    btSortTime <<- FALSE
    if(btSortNameDescBase){
      updateSelectInput(session, "selLoadScen_base", 
                        choices = formatScenList(scenMetaDbBaseSubset, uid, 
                                                 "_sname", 
                                                 desc = FALSE))
      updateActionButton(session, "btSortName_base", 
                         label = lang$nav$dialogLoadScen$btSortNameDESC, 
                         icon = icon("sort-alpha-down-alt"))
      btSortNameDescBase <<- FALSE
    }else{
      updateSelectInput(session, "selLoadScen_base", 
                        choices = formatScenList(scenMetaDbBaseSubset, uid, 
                                                 "_sname", desc = TRUE))
      updateActionButton(session, "btSortName_base", 
                         label = lang$nav$dialogLoadScen$btSortNameASC, 
                         icon = icon("sort-alpha-down"))
      btSortNameDescBase <<- TRUE
    }
    
  })
  # sort by time
  observeEvent(input$btSortTime_base, {
    flog.debug("Button to sort base mode scenarios by time clicked (%s).", 
               if(btSortTimeDescBase) "ascending order" else "descending order")
    removeClassEl(session, "#btSortName_base", "scen-sort-by-selected")
    addClassEl(session, "#btSortTime_base", "scen-sort-by-selected")
    btSortTime <<- TRUE
    if(btSortTimeDescBase){
      updateSelectInput(session, "selLoadScen_base", 
                        choices = formatScenList(scenMetaDbBaseSubset, uid, 
                                                 "_stime", desc = FALSE))
      updateActionButton(session, "btSortTime_base", 
                         label = lang$nav$dialogLoadScen$btSortTimeDESC, 
                         icon = icon("sort-numeric-down-alt"))
      btSortTimeDescBase <<- FALSE
    }else{
      updateSelectInput(session, "selLoadScen_base", choices = formatScenList(
        scenMetaDbBaseSubset, uid, "_stime", desc = TRUE))
      updateActionButton(session, "btSortTime_base", 
                         label = lang$nav$dialogLoadScen$btSortTimeASC, 
                         icon = icon("sort-numeric-down"))
      btSortTimeDescBase <<- TRUE
    }
  })
  observeEvent(input$btLoadFromBase, {
    flog.debug("Button to import scenario from base mode clicked.")
    loadIntoSandbox <<- TRUE
    scenSelected <- isolate(input[["selLoadScen_base"]])
    if(!length(scenSelected)){
      return()
    }
    sidsToLoad  <<- lapply(regmatches(scenSelected, 
                                      regexpr("_", scenSelected), 
                                      invert = TRUE), '[[', 1)
    checkIfInputDataExists()
  })
}else{
  # import from Hcube via hash module
  observeEvent(input$hcHashLookup, {
    flog.debug("Look up Hypercube scenario by hash value button clicked.")
    hashVal <- isolate(input$hcHashLookup)
    if(!identical(nchar(hashVal), 64L) || 
       identical(grepl("^[A-Fa-f0-9]{64}$", hashVal)[[1L]], FALSE)){
      return()
    }
    showEl(session, "#hcHashLookup_load")
    on.exit(hideEl(session, "#hcHashLookup_load"))
    noErr <- TRUE
    tryCatch({
      matchingScen <- db$importDataset("_scenMeta", 
                                       colNames = c("_sid", "_stag", "_stime"),
                                       tibble(c("_sname",
                                                "_scode"), 
                                              c(hashVal, SCODEMAP[['scen']]), c("=", ">")))
    }, error = function(e){
      flog.error("Problems fetching scenario metadata from database. Error message: '%s'.",
                 conditionMessage(e))
      showHideEl(session, "#importScenError")
      noErr <<- FALSE
    })
    if(!noErr)
      return()
    if(length(matchingScen)){
      if(identical(nrow(matchingScen), 0L)){
        showHideEl(session, "#importScenNoHcubeScen", 4000L)
        return()
      }else if(identical(nrow(matchingScen), 1L)){
        sidsToLoad <<- list(matchingScen[[1L]][1])
        loadIntoSandbox <<- TRUE
        checkIfInputDataExists()
        return()
      }
    }
    output$hcHashLookupResults <- renderUI(getHcubeHashLookupTable(matchingScen))
  })
  observeEvent(input$loadHcubeHashSid, {
    sidToLoad <- suppressWarnings(as.integer(input$loadHcubeHashSid))
    if(!length(sidToLoad) || is.na(sidToLoad)){
      flog.error("Could not load Hypercube scenario with sid: '%s'. This looks like an attempt to tamper with the app!",
                 input$loadHcubeHashSid)
      return()
    }
    sidsToLoad <<- list(sidToLoad)
    loadIntoSandbox <<- TRUE
    checkIfInputDataExists()
  })
}

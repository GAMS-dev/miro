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
  oderByIdentifier <- if(btSortTime) stimeIdentifier else snameIdentifier
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
                      choices = db$formatScenList(scenMetaDbSubset, oderByIdentifier, 
                                                  desc = desc))
    return()
  }
  scenMetaDbSubset <<- scenMetaDb[vapply(scenMetaDb[[stagIdentifier]], function(tags){
    any(csv2Vector(tags) %in% input$selLoadScenTags)}, logical(1L), USE.NAMES = FALSE), ]
  updateSelectInput(session, "selLoadScen", 
                    choices = db$formatScenList(scenMetaDbSubset, 
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
    flog.error("Problems fetching list of scenarios from database. Error message: %s.", e)
    errMsg <<- lang$errMsg$fetchScenData$desc
  })
  if(is.null(showErrorMsg(lang$errMsg$fetchScenData$title, errMsg))){
    return()
  }
  if(is.null(scenMetaDb) || !nrow(scenMetaDb)){
    return(showErrorMsg(lang$nav$dialogLoadScen$titleNoScen, 
                        lang$nav$dialogLoadScen$descNoScen))
  }
  # fetch only those scenarios that are not already loaded into the ui
  uiSidList <- NULL
  uiSidListTmp <- NULL
  if(isInSplitView){
    scenMetaDb <<- scenMetaDb[!as.character(scenMetaDb[[1]]) %in% sidsInSplitComp, ]
    currentMode <- "split"
    sidsLoadedInOtherModes <- setdiff(c(sidsInComp, sidsInPivotComp), sidsInSplitComp)
    sidsLoadedInOtherModes <- sidsLoadedInOtherModes[sidsLoadedInOtherModes != 0]
    if(length(sidsLoadedInOtherModes)){
      uiSidListTmp <- sidsLoadedInOtherModes
    }
  }else if(is.null(input$btSplitView) && identical(config$defCompMode, "pivot") ||
      identical(input$btSplitView, "pivotView")){
    currentMode <- "pivot"
    sidsLoadedInPivotMode <- sidsInPivotComp[!is.na(sidsInPivotComp) & sidsInPivotComp != 0]
    scenMetaDb <<- scenMetaDb[!as.character(scenMetaDb[[1]]) %in% sidsLoadedInPivotMode, ]
    if(length(sidsLoadedInPivotMode)){
      uiSidListTmp <- sidsLoadedInPivotMode
    }else{
      sidsLoadedInOtherModes <- c(sidsInSplitComp, sidsInComp)
      sidsLoadedInOtherModes <- sidsLoadedInOtherModes[sidsLoadedInOtherModes != 0 &
                                                         !is.na(sidsLoadedInOtherModes)]
      if(length(sidsLoadedInOtherModes)){
        uiSidListTmp <- sidsLoadedInOtherModes
      }
    }
  }else{
    scenMetaDb <<- scenMetaDb[!as.character(scenMetaDb[[1]]) %in% sidsInComp, ]
    currentMode <- "tab"
    sidsLoadedInOtherModes <- setdiff(c(sidsInSplitComp, sidsInPivotComp), sidsInComp)
    sidsLoadedInOtherModes <- sidsLoadedInOtherModes[sidsLoadedInOtherModes != 0 &
                                                       !is.na(sidsLoadedInOtherModes)]
    if(length(sidsLoadedInOtherModes)){
      uiSidListTmp <- sidsLoadedInOtherModes
    }
  }
  
  if(is.null(scenMetaDb) || !nrow(scenMetaDb)){
    return(showErrorMsg(lang$nav$dialogLoadScen$titleNoScen, 
                        lang$nav$dialogLoadScen$descNoScen))
  }
  
  maxNoScenExceeded <- FALSE
  if(nrow(scenMetaDb) > maxNoScenToShow){
    scenMetaDbSubset <<- scenMetaDb[order(scenMetaDb[[stimeIdentifier]], 
                                          decreasing = TRUE), ][seq_len(maxNoScenToShow), ]
    maxNoScenExceeded <- TRUE
  }else{
    scenMetaDbSubset <<- scenMetaDb
    maxNoScenExceeded <- FALSE
  }
  dbTagList <- scenMetaDb[[stagIdentifier]]
  dbTagList <- csv2Vector(dbTagList[dbTagList != ""])
  # by default, put most recently saved scenario first
  dbSidList <- db$formatScenList(scenMetaDbSubset, stimeIdentifier, desc = TRUE)
  if(!is.null(uiSidListTmp)){
    sidListTmp <- bind_rows(scenMetaData[vapply(scenMetaData, function(el) !is.null(el[[1]]), 
                                                logical(1L), USE.NAMES = FALSE)])
    names(sidListTmp)[1:4] <- db$getScenMetaColnames()[c('sid', 'uid', 'sname', 'stime')]
    uiSidList <- db$formatScenList(filter(sidListTmp, `_sid` %in% uiSidListTmp),
                                   stimeIdentifier, desc = TRUE)
    uiSidList <- uiSidList[!duplicated(uiSidList)]
  }
  showLoadScenDialog(dbSidList, uiSidList, isInSplitView, dbTagList = dbTagList)
  if(maxNoScenExceeded)
    showHideEl(session, "#importScenMaxNoScen", 4000L)
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
                      choices = db$formatScenList(scenMetaDbSubset, 
                                                  snameIdentifier, 
                                                  desc = FALSE))
    updateActionButton(session, "btSortName", 
                       label = lang$nav$dialogLoadScen$btSortNameDESC, 
                       icon = icon("sort-by-alphabet-alt", lib = "glyphicon"))
    btSortNameDesc <<- FALSE
  }else{
    updateSelectInput(session, "selLoadScen", 
                      choices = db$formatScenList(scenMetaDbSubset, 
                                                  snameIdentifier, desc = TRUE))
    updateActionButton(session, "btSortName", 
                       label = lang$nav$dialogLoadScen$btSortNameASC, 
                       icon = icon("sort-by-alphabet", lib = "glyphicon"))
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
                      choices = db$formatScenList(scenMetaDbSubset, 
                                                  stimeIdentifier, desc = FALSE))
    updateActionButton(session, "btSortTime", 
                       label = lang$nav$dialogLoadScen$btSortTimeDESC, 
                       icon = icon("sort-by-order-alt", lib = "glyphicon"))
    btSortTimeDesc <<- FALSE
  }else{
    updateSelectInput(session, "selLoadScen", choices = db$formatScenList(
      scenMetaDbSubset, stimeIdentifier, desc = TRUE))
    updateActionButton(session, "btSortTime", 
                       label = lang$nav$dialogLoadScen$btSortTimeASC, 
                       icon = icon("sort-by-order", lib = "glyphicon"))
    btSortTimeDesc <<- TRUE
  }
})

# load scenario confirmed
observeEvent(input$btLoadScenConfirm, {
  flog.debug("Confirm load scenario button clicked.")
  if(identical(isolate(input$sidebarMenuId), "scenarios") &&
     identical(isolate(input$tabsetLoadScen), "loadScenUI")){
    scenSelected <- isolate(input$selLoadScenUI)
  }else{
    scenSelected <- isolate(input$selLoadScen)
  }
  if(!length(scenSelected)){
    return()
  }
  loadIntoSandbox <<- FALSE
  sidsToLoad  <<- lapply(regmatches(scenSelected, 
                                    regexpr("_", scenSelected), 
                                    invert = TRUE), '[[', 1)

  # if in comparison mode skip input data check
  if(!isInSolveMode){
    rv$btOverwriteScen <<- isolate(rv$btOverwriteScen + 1L)
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
  rv$btOverwriteScen <<- isolate(rv$btOverwriteScen + 1L)
})

observeEvent(input$btHcubeLoad, {
  flog.debug("Load Hypercube job scenarios to compare mode button clicked.")
  isInSolveMode <<- FALSE
  loadIntoSandbox <<- FALSE
  if(isInSplitView){
    switchCompareMode(session, "pivotView")
    isInSplitView <<- FALSE
  }
  rv$btOverwriteScen <<- isolate(rv$btOverwriteScen + 1L)
})

observeEvent(virtualActionButton(rv$btOverwriteScen), {
  flog.debug("Loading and rendering scenarios: '%s'.",
             paste(sidsToLoad, collapse = ", "))
  suppressCloseModalLocal <- suppressCloseModal
  suppressCloseModal      <<- FALSE
  if(!length(sidsToLoad)){
    return()
  }
  
  if(sum(!occupiedSidSlots) < length(sidsToLoad)){
    flog.info("Maximum number of scenarios in scenario comparison mode reached (%d).", 
              length(occupiedSidSlots))
    if(is.null(showErrorMsg(lang$errMsg$maxScen$title, lang$errMsg$maxScen$desc))){
      return()
    }
  }
  sidsInMem <- c(sidsInComp, sidsInSplitComp[!is.na(sidsInSplitComp)])
  if(!isInSolveMode && all(unlist(sidsToLoad, use.names = FALSE) %in% sidsInMem)){
    allSidsInComp <- TRUE
    scenDataTmp   <- match(unlist(sidsToLoad, use.names = FALSE), sidsInMem)
    scenDataTmp[scenDataTmp > length(sidsInComp)] <- scenDataTmp[scenDataTmp > length(sidsInComp)] - length(sidsInComp) + 1L
  }else{
    allSidsInComp <- FALSE
    errMsg <- NULL
    tryCatch({
      sidsToLoadVector <- unlist(sidsToLoad, use.names = FALSE)
      scenDataTmp <- db$loadScenarios(sidsToLoadVector, 
                                      msgProgress = lang$progressBar$loadScenDb)
      scriptDataTmp <- db$loadScriptResults(sidsToLoadVector,
                                            msgProgress = lang$progressBar$loadScenDb)
      views$loadConf(db$importDataset(tableName = dbSchema$tabName[["_scenViews"]], 
                                      subsetSids = sidsToLoadVector), isInSolveMode)
    }, error = function(e){
      flog.error("Some error occurred loading scenarios: '%s' from database. Error message: %s.", 
                 paste(sidsToLoad, collapse = ", "), e)
      errMsg <<- lang$errMsg$loadScen$desc
    })
    if(is.null(showErrorMsg(lang$errMsg$loadScen$title, errMsg))){
      return()
    }
  }
  
  idxScalarOut <- match(paste0(gsub("_", "", modelName, fixed = TRUE), 
                               "_", scalarsOutName), scenTableNames)[[1]]
  
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
        flog.error("Error generating new Scenario object. Error message: '%s'.", e)
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
    if(length(modelOut)){
      scenInputData <- scenDataTmp[[1]][-seq_along(modelOut)]
    }else{
      scenInputData <- scenDataTmp[[1]]
    }
    if(length(inputDsNames) != length(scenInputData)){
      flog.error("Length of data provided (%d) does not match the number of input sheets (%d).",
                 length(scenInputData), length(inputDsNames))
      if(is.null(showErrorMsg(lang$nav$dialogLoadScen$titleDataError, 
                              lang$nav$dialogLoadScen$descDataError))){
        return()
      }
    }
    names(scenInputData)         <- inputDsNames
    
    if(scalarsFileName %in% inputDsNames){
      scalarDataset <- scenInputData[[length(scenInputData)]]
    }else{
      scalarDataset <- NULL
    }
    
    errMsg    <-  NULL
    loadMode  <-  "scen"
    newInputCount <- 0L
    datasetsToFetch <- names(modelIn)
    source("./modules/input_load.R", local = TRUE)
    if(is.null(errMsg) && isFALSE(suppressCloseModalLocal)){
      removeModal()
    }
    if(length(scalarDataset) && nrow(scalarDataset)){
      scalarData[["scen_1_"]] <<- scalarDataset
    }
    
    if(LAUNCHHCUBEMODE){
      noOutputData <<- TRUE
    }else{
      # render output data
      # generate data
      noOutput <- TRUE
      # load scalar data if available
      lapply(seq_along(modelOut), function(i){
        if(!nrow(scenDataTmp[[1]][[i]])){
          scenData[["scen_1_"]][[i]] <<- scenDataTemplate[[i]]
        }else{
          noOutput <<- FALSE
          scenData[["scen_1_"]][[i]] <<- scenDataTmp[[1]][[i]]
          attr(scenData[["scen_1_"]][[i]], "aliases") <<- attr(modelOutTemplate[[i]], "aliases")
          if(identical(i, idxScalarOut)){
            # scalar data exists
            removeRows                 <- tolower(scenData[["scen_1_"]][[i]][[1]]) %in% config$hiddenOutputScalars
            scalarData[["scen_1_"]]    <<- bind_rows(scalarData[["scen_1_"]], scenData[["scen_1_"]][[i]])
            scenData[["scen_1_"]][[i]] <<- scenData[["scen_1_"]][[i]][!removeRows, ]
          }
        }
      })
      lapply(length(modelOut) + seq_along(modelInFileNames), function(i){
        if(!nrow(scenDataTmp[[1]][[i]])){
          scenData[["scen_1_"]][[i]] <<- scenDataTemplate[[i]]
        }else{
          scenData[["scen_1_"]][[i]] <<- scenDataTmp[[1]][[i]]
          attr(scenData[["scen_1_"]][[i]], "aliases") <<- attr(modelInTemplate[[i - length(modelOut)]], "aliases")
        }
      })
      
      if(noOutput){
        noOutputData <<- TRUE
      }else{
        noOutputData <<- FALSE
      }
      # rendering tables and graphs
      renderOutputData(rendererEnv, views)
      
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
      scenMetaData[["scen_1_"]] <<- activeScen$
        getMetadata(lang$nav$excelExport$metadataSheet)
      # update scenario name
      rv$activeSname  <<- activeScen$getScenName()
    }
    # function ends here in case not in compare mode!
    return()
  }
  # scenario comparison mode
  
  # in Hypercube mode, sids are vector not list
  if(!is.list(sidsToLoad)){
    rowIds              <- as.integer(rv$fetchedScenarios[[1]]) %in% sidsToLoad
    metadataFull        <- rv$fetchedScenarios[rowIds, 1:4]
    metDataColNames     <- db$getScenMetaColnames()
    names(metadataFull) <- metDataColNames[c("sid", "uid", "stime", "sname")]
    metadataFull[[metDataColNames["sname"]]] <- as.character(seq.int(isolate(rv$scenId) - 3L, 
                                                           isolate(rv$scenId) - 4L + nrow(metadataFull)))
  }else if(!allSidsInComp){
    metadataFull        <- scenMetaDb
  }
  errMsg <- NULL
  lastImportedSid <- NULL
  idxScalarIn <- match(paste0(gsub("_", "", modelName, fixed = TRUE), 
                              "_", scalarsFileName), scenTableNames)[[1]]
  
  if(is.null(input$btSplitView) && identical(config$defCompMode, "pivot") ||
     identical(input$btSplitView, "pivotView")){
    if(!compareModeTabsetGenerated[3]){
      compareModeTabsetGenerated[3] <<- TRUE
      insertUI("#pivotCompScenWrapper", where = "afterBegin",
               generateScenarioTabset(0L, pivotCompare = TRUE), immediate = TRUE)
    }
    return(tryCatch({
      sidsInPivotCompTmp <- unlist(sidsToLoad)
      if(allSidsInComp){
        scenIdsLong    <- paste0("scen_", scenDataTmp, "_")
        metadataFull   <- bind_rows(scenMetaData[scenIdsLong])
        scenNamesToCompare <- paste0(vapply(metadataFull[[2]], function(scenOwner){
          if(identical(scenOwner, uid)){
            return("")
          }
          return(paste0(scenOwner, ": "))
        }, character(1L), USE.NAMES = FALSE), metadataFull[[3]])
        renderScenPivotCompare(scenData[scenIdsLong], scenNamesToCompare, 
                               rendererEnv, views, roundPrecision)
      }else{
        sidsToLoadVector <- match(sidsInPivotCompTmp, metadataFull[[1]])
        sidsToLoadVector <- sidsToLoadVector[!is.na(sidsToLoadVector)]
        scenNamesToCompare <- paste0(vapply(metadataFull[["_uid"]][sidsToLoadVector], function(scenOwner){
          if(identical(scenOwner, uid)){
            return("")
          }
          return(paste0(scenOwner, ": "))
        }, character(1L), USE.NAMES = FALSE), metadataFull[["_sname"]][sidsToLoadVector])
        renderScenPivotCompare(scenDataTmp, scenNamesToCompare, 
                               rendererEnv, views, roundPrecision)
      }
      hideEl(session, "#pivotCompBtWrapper")
      showEl(session, "#pivotCompScenWrapper")
      switchTab(session, "scenComp")
      sidsInPivotComp <<- as.integer(sidsInPivotCompTmp)
      removeModal()
      flog.debug("Scenarios: '%s' loaded and rendered in scenario comparison mode (pivot view).", 
                 paste(sidsToLoad, collapse = ", "))
    }, error = function(e){
      flog.warn("Problems rendering scenarios in pivot compare mode. Error message: %s", conditionMessage(e))
      showErrorMsg(lang$errMsg$loadScen$title, lang$errMsg$loadScen$desc)
    }))
  }
  lapply(seq_along(scenDataTmp), function(i){
    tryCatch({
      if(!isInSplitView){
        scenId   <- isolate(rv$scenId)
      }else{
        if(loadInLeftBoxSplit){
          scenId   <- 2L
        }else{
          scenId   <- 3L
        }
      }
      scenIdLong <- paste0("scen_", scenId, "_")
      if(allSidsInComp){
        currScenIdLong <- paste0("scen_", scenDataTmp[i], "_")
        scenData[[scenIdLong]]                   <<- scenData[[currScenIdLong]]
        scalarData[[scenIdLong]]                 <<- scalarData[[currScenIdLong]]
        scenMetaData[[scenIdLong]]               <<- scenMetaData[[currScenIdLong]] 
      }else{
        metadata <- metadataFull[metadataFull[[1]] == sidsToLoad[[i]], ]
        # load scenario data
        scenData[[scenIdLong]]                   <<- scenDataTmp[[i]]
        # load scalar data if available
        if(!is.na(idxScalarIn) && nrow(scenData[[scenIdLong]][[idxScalarIn]])){
          scalarData[[scenIdLong]]               <<- scenData[[scenIdLong]][[idxScalarIn]]
        }else{
          scalarData[[scenIdLong]]               <<- tibble()
        }
        if(!is.na(idxScalarOut) && nrow(scenData[[scenIdLong]][[idxScalarOut]])){
          # scalar data exists
          rowIdsToRemove                         <- tolower(scenData[[scenIdLong]][[idxScalarOut]][[1]]) %in% config$hiddenOutputScalars
          scalarData[[scenIdLong]]               <<- bind_rows(scalarData[[scenIdLong]], scenData[[scenIdLong]][[idxScalarOut]])
          scenData[[scenIdLong]][[idxScalarOut]] <<- scenData[[scenIdLong]][[idxScalarOut]][!rowIdsToRemove, ]
        }
        # add scenario metadata
        scenMetaData[[scenIdLong]]               <<- db$getMetadata(sid = metadata[[sidIdentifier]],
                                                                    uid = metadata[[uidIdentifier]], 
                                                                    sname = metadata[[snameIdentifier]], 
                                                                    stime = metadata[[stimeIdentifier]],
                                                                    uidAlias = lang$nav$excelExport$metadataSheet$uid, 
                                                                    snameAlias = lang$nav$excelExport$metadataSheet$sname, 
                                                                    stimeAlias = lang$nav$excelExport$metadataSheet$stime)
      }
      
      source("./modules/scen_render.R", local = TRUE)
      # load script results
      if(length(config$scripts$base)){
        scriptOutput$loadResultsBase(scriptDataTmp[[i]], scenId)
      }
    }, error = function(e){
      flog.error(e)
      errMsg  <<- lang$errMsg$loadScen$desc
    })
    
    flog.debug("Scenario: '%s' loaded into UI (compare mode).", sidsToLoad[[i]])
    if(!isInSplitView){
      lastImportedSid <<- scenId
      sidsInComp[scenId] <<- sidsToLoad[[i]]
      occupiedSidSlots[scenId - 3] <<- TRUE
      sidCompOrder <<- c(sidCompOrder, scenId)
      rv$scenId <<- which.min(occupiedSidSlots) + 3
      scenCounterMultiComp <<- scenCounterMultiComp + 1
    }else{
      local({
        id <- if(loadInLeftBoxSplit) 1L else 2L
        sidsInSplitComp[id] <<- as.integer(sidsToLoad[[i]])
      })
    }
  })
  rm(scenDataTmp)
  if(!is.list(sidsToLoad)){
    switchTab(session, "scenComp")
  }
  if(!isInSplitView){
    switchTab(session, "scenComp")
    updateTabsetPanel(session, "scenTabset", selected = "scen_" %+% lastImportedSid %+% "_")
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
            scenMetaDbBaseSubset <<- scenMetaDbBase[order(scenMetaDbBase[[stimeIdentifier]], 
                                                          decreasing = TRUE), ][seq_len(maxNoScenToShow), ]
            showHideEl(session, "#importScenMaxNoScen", 4000L)
          }else{
            scenMetaDbBaseSubset <<- scenMetaDbBase
          }
          scenMetaDbBaseList    <<- db$formatScenList(scenMetaDbBaseSubset, 
                                                      stimeIdentifier, desc = TRUE)
          scenMetaDbBaseTagList <<- csv2Vector(scenMetaDbBase[[stagIdentifier]])
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
      flog.error("Problems fetching list of base-mode-scenarios from database. Error message: %s.", e)
      showEl(session, "#importDataDbUnknownError")
    })
  })
  observeEvent(input$selLoadScenTags_base, {
    oderByIdentifier <- if(btSortTimeBase) stimeIdentifier else snameIdentifier
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
                        choices = db$formatScenList(scenMetaDbBaseSubset, oderByIdentifier, 
                                                    desc = desc))
      return()
    }
    scenMetaDbBaseSubset <<- scenMetaDbBase[vapply(scenMetaDbBase[[stagIdentifier]], function(tags){
      any(csv2Vector(tags) %in% input$selLoadScenTags_base)}, logical(1L), USE.NAMES = FALSE), ]
    updateSelectInput(session, "selLoadScen_base", 
                      choices = db$formatScenList(scenMetaDbBaseSubset, 
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
                        choices = db$formatScenList(scenMetaDbBaseSubset, 
                                                    snameIdentifier, 
                                                    desc = FALSE))
      updateActionButton(session, "btSortName_base", 
                         label = lang$nav$dialogLoadScen$btSortNameDESC, 
                         icon = icon("sort-by-alphabet-alt", lib = "glyphicon"))
      btSortNameDescBase <<- FALSE
    }else{
      updateSelectInput(session, "selLoadScen_base", 
                        choices = db$formatScenList(scenMetaDbBaseSubset, 
                                                    snameIdentifier, desc = TRUE))
      updateActionButton(session, "btSortName_base", 
                         label = lang$nav$dialogLoadScen$btSortNameASC, 
                         icon = icon("sort-by-alphabet", lib = "glyphicon"))
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
                        choices = db$formatScenList(scenMetaDbBaseSubset, 
                                                    stimeIdentifier, desc = FALSE))
      updateActionButton(session, "btSortTime_base", 
                         label = lang$nav$dialogLoadScen$btSortTimeDESC, 
                         icon = icon("sort-by-order-alt", lib = "glyphicon"))
      btSortTimeDescBase <<- FALSE
    }else{
      updateSelectInput(session, "selLoadScen_base", choices = db$formatScenList(
        scenMetaDbBaseSubset, stimeIdentifier, desc = TRUE))
      updateActionButton(session, "btSortTime_base", 
                         label = lang$nav$dialogLoadScen$btSortTimeASC, 
                         icon = icon("sort-by-order", lib = "glyphicon"))
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
      dbSchemaTmp  <- db$getDbSchema()
      matchingScen <- db$importDataset(dbSchemaTmp$tabName['_scenMeta'], 
                                       colNames = dbSchemaTmp$colNames[['_scenMeta']][c('sid', 'stag', 'stime')],
                                       tibble(c(dbSchemaTmp$colNames[['_scenMeta']][['sname']],
                                                dbSchemaTmp$colNames[['_scenMeta']][['scode']]), 
                                              c(hashVal, SCODEMAP[['scen']]), c("=", ">")))
    }, error = function(e){
      flog.error("Problems fetching scenario metadata from database. Error message: '%s'.", e)
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

# load scenario from database
# define which input datasheets to load to interface (default: load all datasets)
datasetsToFetch <- names(modelIn)

observeEvent(input$btLoadScen, {
  flog.debug("Load Scenario button clicked (multiple scenarios view).")
  rv$btLoadScen <<- isolate(rv$btLoadScen + 1L)
})
observeEvent(input$selLoadScenTags, {
  oderByIdentifier <- if(btSortTime) stimeIdentifier else snameIdentifier
  desc <- if(btSortNameDesc || btSortTimeDesc) TRUE else FALSE
  if(!length(input$selLoadScenTags)){
    scenMetaDbSubset <<- scenMetaDb
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
  errMsg <- NULL
  tryCatch({
    scenMetaDb <<- db$fetchScenList(noBatch = TRUE)
  }, error = function(e){
    flog.error("Problems fetching list of scenarios from database. Error message: %s.", e)
    errMsg <<- lang$errMsg$fetchScenData$desc
  })
  if(is.null(showErrorMsg(lang$errMsg$fetchScenData$title, errMsg))){
    return()
  }
  
  if(!is.null(scenMetaDb) && nrow(scenMetaDb)){
    # fetch only those scenarios that are not already loaded into the ui
    if(isInSplitView){
      scenMetaDb <<- scenMetaDb[!as.character(scenMetaDb[[1]]) %in% sidsInSplitComp, ]
    }else{
      scenMetaDb <<- scenMetaDb[!as.character(scenMetaDb[[1]]) %in% sidsInComp, ]
    }
  }
  
  if(!is.null(scenMetaDb) && nrow(scenMetaDb)){
    scenMetaDbSubset <<- scenMetaDb
    dbTagList <- scenMetaDb[[stagIdentifier]]
    dbTagList <- csv2Vector(dbTagList[dbTagList != ""])
    # by default, put most recently saved scenario first
    dbSidList <- db$formatScenList(scenMetaDbSubset, stimeIdentifier, desc = TRUE)
    if(isInSplitView){
      uiSidList <- scenMetaDb[as.character(scenMetaDb[[1]]) %in% sidsInComp, ]
      uiSidList <- db$formatScenList(uiSidList, stimeIdentifier, desc = TRUE)
    }else{
      uiSidList <- NULL
    }
    showLoadScenDialog(dbSidList, uiSidList, isInSplitView, dbTagList = dbTagList)
  }else{
    if(is.null(showErrorMsg(lang$nav$dialogLoadScen$titleNoScen, 
                            lang$nav$dialogLoadScen$descNoScen))){
      return()
    }
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
                      choices = db$formatScenList(scenMetaDbSubset, 
                                                  snameIdentifier, 
                                                  desc = FALSE))
    updateActionButton(session, "btSortName", 
                       label = lang$nav$dialogLoadScen$btSortNameDESC, 
                       icon = icon("sort-by-alphabet-alt", lib = "glyphicon"))
    btSortNameDesc <<- FALSE
  }else{
    updateSelectInput(session, "selLoadScen", 
                      choices = db$formatScenList(scenMetaDbSubset, snameIdentifier, desc = TRUE))
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
      scenMetaDb, stimeIdentifier, desc = TRUE))
    updateActionButton(session, "btSortTime", 
                       label = lang$nav$dialogLoadScen$btSortTimeASC, 
                       icon = icon("sort-by-order", lib = "glyphicon"))
    btSortTimeDesc <<- TRUE
  }
})

# load scenario confirmed
observeEvent(input$btLoadScenConfirm, {
  flog.debug("Confirm load scenario button clicked.")
  if(identical(isolate(input$tabsetLoadScen), "loadScenUI")){
    scenSelected <- isolate(input$selLoadScenUI)
  }else{
    scenSelected <- isolate(input$selLoadScen)
  }
  
  scenSelected <- regmatches(scenSelected, 
                             regexpr("_", scenSelected), 
                             invert = TRUE)
  sidsToLoad  <<- lapply(scenSelected, '[[', 1)
  rm(scenSelected)
  # if in comparison mode skip input data check
  if(!isInSolveMode){
    rv$btOverwriteScen <<- isolate(rv$btOverwriteScen + 1L)
    return()
  }
  
  # update input sheets
  # check whether current input datasets are empty
  if(identical(isolate(input$cbSelectManually), TRUE) && 
     length(isolate(input$selInputData))){
    inputDatasetIdxToImport <- match(tolower(isolate(input$selInputData)), names(modelIn))
    # remove NAs
    inputDatasetIdxToImport <- inputDatasetIdxToImport[!is.na(inputDatasetIdxToImport)]
  }else{
    inputDatasetIdxToImport <- seq_along(modelIn)
  }
  datasetsToFetch <<- names(modelIn[inputDatasetIdxToImport])
  
  inputDatasetsExist <- vapply(inputDatasetIdxToImport, function(i){
    if(length(isolate(rv[["in_" %+% i]]))){
      return(TRUE)
    }else{
      return(FALSE)
    }
  }, logical(1))
  
  if(any(inputDatasetsExist)){
    hideEl(session, "#importDataTabset")
    showEl(session, "#btOverwriteScen")
    showEl(session, "#importDataOverwrite")
  }else{
    overwriteInput <<- FALSE
    rv$btOverwriteScen <<- isolate(rv$btOverwriteScen + 1L)
  }
})

observeEvent(input$btOverwriteScen, {
  flog.debug("Overwrite scenario button clicked.")
  overwriteInput <<- TRUE
  rv$btOverwriteScen <<- isolate(rv$btOverwriteScen + 1L)
})

observeEvent(input$btBatchLoad, {
  flog.debug("Load batch scenarios to compare mode button clicked.")
  isInSolveMode <<- FALSE
  if(isInSplitView){
    rv$btSplitView <<- isolate(rv$btSplitView + 1L)
  }
  rv$btOverwriteScen <<- isolate(rv$btOverwriteScen + 1L)
})

observeEvent(virtualActionButton(rv$btOverwriteScen), {
  flog.debug("Loading and rendering scenarios: '%s'.",
             paste(sidsToLoad, collapse = ", "))
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
  
  errMsg <- NULL
  tryCatch({
    scenDataTmp <- db$loadScenarios(unlist(sidsToLoad, use.names = FALSE), 
                                    msgProgress = lang$progressBar$loadScenDb)
  }, error = function(e){
    flog.error("Some error occurred loading scenarios: '%s' from database. Error message: %s.", 
               paste(sidsToLoad, collapse = ", "), e)
    errMsg <<- lang$errMsg$loadScen$desc
  })
  if(is.null(showErrorMsg(lang$errMsg$loadScen$title, errMsg))){
    return()
  }
  
  idxScalarOut <- match(gsub("_", "", modelName, fixed = TRUE) %+% 
                          "_" %+% scalarsOutName, scenTableNames)[[1]]
  
  if(isInSolveMode){
    # close currently opened scenario
    if(!closeScenario()){
      return()
    }
    activeScen <<- Scenario$new(db = db, sid = sidsToLoad[[1]])
    
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
    newInputCount                <- 0
    
    if(scalarsFileName %in% inputDsNames){
      scalarDataset <- scenInputData[[length(scenInputData)]]
    }else{
      scalarDataset <- NULL
    }
    
    errMsg    <-  NULL
    loadMode  <-  "scen"
    source("./modules/input_load.R", local = TRUE)
    if(!is.null(errMsg)){
      return()
    }
    removeModal()
    # render output data (not compare mode)
    # generate data
    noOutput <- TRUE
    # load scalar data if available
    lapply(seq_along(modelOut), function(i){
      if(!nrow(scenDataTmp[[1]][[i]])){
        scenData[["scen_1_"]][[i]] <<- scenDataTemplate[[i]]
      }else{
        noOutput <<- FALSE
        scenData[["scen_1_"]][[i]] <<- scenDataTmp[[1]][[i]]
        if(identical(i, idxScalarOut)){
          # scalar data exists
          removeRows                 <- grepl(config$gamsMetaDelim, 
                                              scenData[["scen_1_"]][[i]][[2]])
          scalarData[["scen_1_"]]    <<- scenData[["scen_1_"]][[i]][removeRows, ]
          scenData[["scen_1_"]][[i]] <<- scenData[["scen_1_"]][[i]][!removeRows, ]
        }else{
          scalarData[["scen_1_"]]    <<- data.frame()
        }
      }
    })
    scenMetaData[["scen_1_"]] <<- activeScen$getMetadata(lang$nav$excelExport$metadataSheet)
    if(noOutput){
      noOutputData <<- TRUE
    }else{
      noOutputData <<- FALSE
    }
    # rendering tables and graphs
    renderOutputData()
    
    flog.debug("Scenario: '%s' was loaded into UI", sidsToLoad[[1]])
    
    # update scenario name
    rv$activeSname  <<- activeScen$getScenName()
    
    # function ends here in case not in compare mode!
    return()
  }
  
  # scenario comparison mode
  
  # in batch mode, sids are vector not list
  if(!is.list(sidsToLoad)){
    rowIds         <- as.integer(rv$fetchedScenarios[[1]]) %in% sidsToLoad
    scenMetaTmp    <- rv$fetchedScenarios[rowIds, ]
    scenMetaTmp[[snameIdentifier]] <- as.character(seq_len(nrow(scenMetaTmp)))
    metadataFull   <- scenMetaTmp[, db$getScenMetaColnames()[c("sid", "uid", 
                                                               "stime", "sname")]]
  }else{
    metadataFull   <- scenMetaDb
  }
  errMsg <- NULL
  lastImportedSid <- NULL
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
      scenIdLong <- "scen_" %+% scenId %+% "_"
      metadata <- metadataFull[metadataFull[[1]] == sidsToLoad[[i]], ]
      # load scenario data
      scenData[[scenIdLong]]                   <<- scenDataTmp[[i]]
      # load scalar data if available
      if(!is.na(idxScalarOut) && nrow(scenData[[scenIdLong]][[idxScalarOut]])){
        # scalar data exists
        rowIdsToRemove                            <- grepl(config$gamsMetaDelim, 
                                                        scenData[[scenIdLong]][[idxScalarOut]][[2]])
        scalarData[[scenIdLong]]               <<- scenData[[scenIdLong]][[idxScalarOut]][rowIdsToRemove, ]
        scenData[[scenIdLong]][[idxScalarOut]] <<- scenData[[scenIdLong]][[idxScalarOut]][!rowIdsToRemove, ]
      }else{
        scalarData[[scenIdLong]]               <<- data.frame()
      }
      # add scenario metadata
      scenMetaData[[scenIdLong]]               <<- db$getMetadata(uid = metadata[[uidIdentifier]], 
                                                                  sname = metadata[[snameIdentifier]], 
                                                                  stime = metadata[[stimeIdentifier]],
                                                                  uidAlias = lang$nav$excelExport$metadataSheet$uid, 
                                                                  snameAlias = lang$nav$excelExport$metadataSheet$sname, 
                                                                  stimeAlias = lang$nav$excelExport$metadataSheet$stime)
      source("./modules/scen_render.R", local = TRUE)
      
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
    updateTabsetPanel(session, "sidebarMenuId", selected = "scenarios")
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
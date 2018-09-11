# load scenario from database

# define which input datasheets to load to interface (default: load all datasets)
datasets.to.fetch <- names(modelIn)

observeEvent(input$btLoadScen, {
  flog.debug("Load Scenario button clicked (multiple scenarios view).")
  rv$btLoadScen <<- isolate(rv$btLoadScen + 1)
})
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
    # by default, put most recently saved scenario first
    dbSidList <- db$formatScenList(scenMetaDb, stime.identifier, desc = TRUE)
    if(isInSplitView){
      uiSidList <- scenMetaDb[as.character(scenMetaDb[[1]]) %in% sidsInComp, ]
      uiSidList <- db$formatScenList(uiSidList, stime.identifier, desc = TRUE)
    }else{
      uiSidList <- NULL
    }
    showLoadScenDialog(dbSidList, uiSidList, isInSplitView)
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
             if(bt.sortName.desc) "ascending order" else "descending order")
  shinyjs::removeClass("btSortTime", class = "scen-sort-by-selected")
  shinyjs::addClass("btSortName", class = "scen-sort-by-selected")
  if(bt.sortName.desc){
    updateSelectInput(session, "selLoadScen", 
                      choices = db$formatScenList(scenMetaDb, 
                                                  sname.identifier, 
                                                  desc = FALSE))
    updateActionButton(session, "btSortName", 
                       label = lang$nav$dialogLoadScen$btSortNameDESC, 
                       icon = icon("sort-by-alphabet-alt", lib = "glyphicon"))
    bt.sortName.desc <<- FALSE
  }else{
    updateSelectInput(session, "selLoadScen", 
                      choices = db$formatScenList(scenMetaDb, sname.identifier, desc = TRUE))
    updateActionButton(session, "btSortName", 
                       label = lang$nav$dialogLoadScen$btSortNameASC, 
                       icon = icon("sort-by-alphabet", lib = "glyphicon"))
    bt.sortName.desc <<- TRUE
  }
  
})
# sort by time
observeEvent(input$btSortTime, {
  flog.debug("Button to sort scenarios by time clicked (%s).", 
             if(bt.sortTime.desc) "ascending order" else "descending order")
  shinyjs::removeClass("btSortName", class = "scen-sort-by-selected")
  shinyjs::addClass("btSortTime", class = "scen-sort-by-selected")
  if(bt.sortTime.desc){
    updateSelectInput(session, "selLoadScen", 
                      choices = db$formatScenList(scenMetaDb, 
                                                  stime.identifier, desc = FALSE))
    updateActionButton(session, "btSortTime", 
                       label = lang$nav$dialogLoadScen$btSortTimeDESC, 
                       icon = icon("sort-by-order-alt", lib = "glyphicon"))
    bt.sortTime.desc <<- FALSE
  }else{
    updateSelectInput(session, "selLoadScen", choices = db$formatScenList(
      scenMetaDb, stime.identifier, desc = TRUE))
    updateActionButton(session, "btSortTime", 
                       label = lang$nav$dialogLoadScen$btSortTimeASC, 
                       icon = icon("sort-by-order", lib = "glyphicon"))
    bt.sortTime.desc <<- TRUE
  }
})

# load scenario confirmed
observeEvent(input$btLoadScenConfirm, {
  flog.debug("Confirm load scenario button clicked.")
  scenSelected <- regmatches(isolate(input$selLoadScen), 
                             regexpr("_", isolate(input$selLoadScen)), 
                             invert = TRUE)
  sidsToLoad  <<- lapply(scenSelected, '[[', 1)
  rm(scenSelected)
  # if in comparison mode skip input data check
  if(!isInSolveMode){
    rv$btOverrideScen <<- isolate(rv$btOverrideScen + 1)
    return()
  }
  
  # update input sheets
  # check whether current input datasets are empty
  if(isolate(input$cbSelectManually) && length(isolate(input$selInputData))){
    inputDatasetIdxToImport <- match(tolower(isolate(input$selInputData)), names(modelIn))
    # remove NAs
    inputDatasetIdxToImport <- inputDatasetIdxToImport[!is.na(inputDatasetIdxToImport)]
  }else{
    inputDatasetIdxToImport <- seq_along(modelIn)
  }
  datasets.to.fetch <<- names(modelIn[inputDatasetIdxToImport])
  
  inputDatasetsExist <- vapply(inputDatasetIdxToImport, function(i){
    if(length(isolate(rv[[paste0("in_", i)]]))){
      return(TRUE)
    }else{
      return(FALSE)
    }
  }, logical(1))
  
  if(any(inputDatasetsExist)){
    showOverrideScenDialog()
  }else{
    overrideInput <<- FALSE
    rv$btOverrideScen <<- isolate(rv$btOverrideScen + 1)
  }
})

observeEvent(input$btOverrideScen, {
  flog.debug("Override scenario button clicked.")
  overrideInput <<- TRUE
  rv$btOverrideScen <<- isolate(rv$btOverrideScen + 1)
})

# update input button pressed
observeEvent(virtualActionButton(rv$btOverrideScen), {
  flog.debug("Loading and rendering scenarios: '%s'.",
             paste(sidsToLoad, collapse = ", "))
  if(!length(sidsToLoad)){
    return()
  }
  
  if(sum(!occupied.sid.slots) < length(sidsToLoad)){
    flog.info("Maximum number of scenarios in scenario comparison mode reached (%d).", 
              length(occupied.sid.slots))
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
    if(length(input.ds.names) != length(scenInputData)){
      flog.error("Length of data provided (%d) does not match the number of input sheets (%d).",
                 length(scenInputData), length(input.ds.names))
      if(is.null(showErrorMsg(lang$nav$dialogLoadScen$titleDataError, 
                              lang$nav$dialogLoadScen$descDataError))){
        return()
      }
    }
    names(scenInputData)         <- input.ds.names
    newInputCount                <- 0
    
    if(scalars.file.name %in% input.ds.names){
      scalarDataset <- scenInputData[[length(scenInputData)]]
    }else{
      scalarDataset <- NULL
    }
    
    errMsg    <-  NULL
    load.mode <-  "scen"
    source("./modules/input_load.R", local = TRUE)
    if(!is.null(errMsg)){
      return()
    }
    removeModal()
    # render output data (not compare mode)
    # generate data
    no.output <- TRUE
    # load scalar data if available
    idx.scalarOut <- match(tolower(modelName %+% "_" %+% scalars.out.name), 
                           scen.table.names)[[1]]
    lapply(seq_along(modelOut), function(i){
      if(!nrow(scenDataTmp[[1]][[i]])){
        scenData[["scen_1_"]][[i]] <<- scenDataTemplate[[i]]
      }else{
        no.output <<- FALSE
        scenData[["scen_1_"]][[i]] <<- scenDataTmp[[1]][[i]]
        if(identical(i, idx.scalarOut)){
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
    scenMetaData[["scen_1_"]] <<- activeScen$getMetadata(
      uidAlias = lang$nav$excelExport$metadataSheet$uid, 
      snameAlias = lang$nav$excelExport$metadataSheet$sname, 
      stimeAlias = lang$nav$excelExport$metadataSheet$stime)
    if(no.output){
      no.output.data <<- TRUE
    }else{
      no.output.data <<- FALSE
    }
    # rendering tables and graphs
    renderOutputData()
    
    flog.debug("Scenario: '%s' was loaded into UI", sidsToLoad[[1]])
    
    # update scenario name
    rv$active.sname  <<- activeScen$getScenName()
    
    # function ends here in case not in compare mode!
    return()
  }
  
  # scenario comparison mode
  
  # in batch mode, sids are vector not list
  if(!is.list(sidsToLoad)){
    scenMetaTmp    <- rv$fetchedScenarios[rv$fetchedScenarios[[1]] %in% sidsToLoad, ]
    scenMetaTmp[[sname.identifier]] <- as.character(seq_len(nrow(scenMetaTmp)))
    metadataFull   <- scenMetaTmp[, db$getScenMetaColnames()[c("sid", "uid", 
                                                               "stime", "sname")]]
  }else{
    metadataFull    <- scenMetaDb
  }
  errMsg <- NULL
  lastImportedSid <- NULL
  lapply(seq_along(scenDataTmp), function(i){
    noError <- TRUE
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
      scen.str <- "scen_" %+% scenId %+% "_"
      
      metadata <- metadataFull[metadataFull[[1]] == sidsToLoad[[i]], ]
      # load scenario data
      scenData[[scen.str]]                    <<- scenDataTmp[[i]]
      # load scalar data if available
      idx.scalarOut <- match(tolower(modelName %+% "_" %+% scalars.out.name), scen.table.names)[[1]]
      if(!is.na(idx.scalarOut) && nrow(scenData[[scen.str]][[idx.scalarOut]])){
        # scalar data exists
        remove.rows                           <- grepl(config$gamsMetaDelim, 
                                                       scenData[[scen.str]][[idx.scalarOut]][[2]])
        scalarData[[scen.str]]                <<- scenData[[scen.str]][[idx.scalarOut]][remove.rows, ]
        scenData[[scen.str]][[idx.scalarOut]] <<- scenData[[scen.str]][[idx.scalarOut]][!remove.rows, ]
      }else{
        scalarData[[scen.str]]                <<- data.frame()
      }
      # add scenario metadata
      scenMetaData[[scen.str]]                <<- db$getMetadata(uid = metadata[[uid.identifier]], 
                                                                 sname = metadata[[sname.identifier]], 
                                                                 stime = metadata[[stime.identifier]],
                                                                 uidAlias = lang$nav$excelExport$metadataSheet$uid, 
                                                                 snameAlias = lang$nav$excelExport$metadataSheet$sname, 
                                                                 stimeAlias = lang$nav$excelExport$metadataSheet$stime)
      source("./modules/scen_render.R", local = TRUE)
      
    }, error = function(e){
      flog.error(e)
      errMsg  <<- paste(errMsg, e, sep = "\n")
      noError <<- NULL
    })
    if(is.null(noError)){
      return()
    }
    flog.debug("Scenario: '%s' loaded into UI (compare mode).", sidsToLoad[[i]])
    if(!isInSplitView){
      lastImportedSid <<- scenId
      sidsInComp[scenId] <<- sidsToLoad[[i]]
      occupied.sid.slots[scenId - 3] <<- TRUE
      sid.comp.order <<- c(sid.comp.order, scenId)
      rv$scenId <<- which.min(occupied.sid.slots) + 3
      scenCounterMultiComp <<- scenCounterMultiComp + 1
    }else{
      local({
        id <- if(loadInLeftBoxSplit) 1L else 2L
        sidsInSplitComp[id] <<- sidsToLoad[[i]]
      })
    }
  })
  rm(scenDataTmp)
  if(!is.list(sidsToLoad)){
    updateTabsetPanel(session, "sidebar.menu", selected = "scenarios")
  }
  if(!isInSplitView){
    updateTabsetPanel(session, "scenTabset", selected = "scen_" %+% lastImportedSid %+% "_")
  }
  if(is.null(showErrorMsg(lang$errMsg$renderGraph$title, errMsg))){
    return()
  }
  
  removeModal()
  flog.debug("Scenarios: '%s' loaded and rendered in scenario comparison mode.", 
             paste(sidsToLoad, collapse = ", "))
  return()
})
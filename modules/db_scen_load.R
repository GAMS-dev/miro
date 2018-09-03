# load scenario from database

# define which input datasheets to load to interface (default: load all datasets)
datasets.to.fetch <- names(modelIn)

observeEvent(input$btLoadScen, {
  flog.debug("Load Scenario button clicked (multiple scenarios view).")
  # set comparison identifier to multiple scenarios comp view
  scen.comp.mode <<- 1L
  rv$btLoadScen <<- isolate(rv$btLoadScen + 1)
})
#load scenario button clicked
observeEvent(virtualActionButton(rv$btLoadScen), {
  # fetch list of saved scenarios
  # only load single scenario as not in comparison mode
  errMsg <- NULL
  tryCatch({
    saved.scenarios <<- db$fetchScenList()
  }, error = function(e){
    flog.error("Problems fetching list of scenarios from database. Error message: %s.", e)
    errMsg <<- lang$errMsg$fetchScenData$desc
  })
  if(is.null(showErrorMsg(lang$errMsg$fetchScenData$title, errMsg))){
    return(NULL)
  }
  
  if(!is.null(saved.scenarios) && nrow(saved.scenarios)){
    # fetch only those scenarios that are not already loaded into the ui
    if(scen.comp.mode < 2){
      saved.scenarios <<- dplyr::filter(saved.scenarios, !(!!as.name(sid.identifier) %in% sids.loaded.in.scen.comp))
    }else{
      saved.scenarios <<- dplyr::filter(saved.scenarios, !(!!as.name(sid.identifier) %in% sids.loaded.in.split.comp))
    }
    
  }
  
  if(!is.null(saved.scenarios) && nrow(saved.scenarios)){
    # by default, put most recently saved scenario first
    
    showModal(modalDialog(
      title = lang$nav$dialogLoadScen$title,
      selectInput("selLoadScen", lang$nav$dialogLoadScen$selLoadScen, db$formatScenList(saved.scenarios, stime.identifier, desc = TRUE), 
                  multiple = if(identical(scen.comp.mode, 1L)) TRUE else FALSE, width = "100%"),
      tags$div(class = "space"),
      tags$div(
        lang$nav$dialogLoadScen$sortBy,
        actionButton("btSortName", label = lang$nav$dialogLoadScen$btSortNameASC, icon = icon("sort-by-alphabet", lib = "glyphicon"), class = "scen-sort-by"), 
        actionButton("btSortTime", label = lang$nav$dialogLoadScen$btSortTimeASC, icon = icon("sort-by-order", lib = "glyphicon"), class = "scen-sort-by")
      ),
      footer = tagList(
        modalButton(lang$nav$dialogLoadScen$cancelButton),
        actionButton("btLoadScenConfirm", lang$nav$dialogLoadScen$okButton, class = "btOrange")),
      fade = TRUE, easyClose = FALSE))
    shinyjs::addClass("btSortTime", class = "scen-sort-by-selected")
  }else{
    if(is.null(showErrorMsg(lang$nav$dialogLoadScen$titleNoScen, lang$nav$dialogLoadScen$descNoScen))){
      return(NULL)
    }
  }
})

# sort by name
observeEvent(input$btSortName, {
  flog.debug("Button to sort scenarios by name clicked (%s).", if(bt.sortName.desc) "ascending order" else "descending order")
  shinyjs::removeClass("btSortTime", class = "scen-sort-by-selected")
  shinyjs::addClass("btSortName", class = "scen-sort-by-selected")
  if(bt.sortName.desc){
    updateSelectInput(session, "selLoadScen", choices = db$formatScenList(saved.scenarios, sname.identifier, desc = FALSE))
    updateActionButton(session, "btSortName", label = lang$nav$dialogLoadScen$btSortNameDESC, icon = icon("sort-by-alphabet-alt", lib = "glyphicon"))
    bt.sortName.desc <<- FALSE
  }else{
    updateSelectInput(session, "selLoadScen", choices = db$formatScenList(saved.scenarios, sname.identifier, desc = TRUE))
    updateActionButton(session, "btSortName", label = lang$nav$dialogLoadScen$btSortNameASC, icon = icon("sort-by-alphabet", lib = "glyphicon"))
    bt.sortName.desc <<- TRUE
  }
  
})
# sort by time
observeEvent(input$btSortTime, {
  flog.debug("Button to sort scenarios by time clicked (%s).", if(bt.sortTime.desc) "ascending order" else "descending order")
  shinyjs::removeClass("btSortName", class = "scen-sort-by-selected")
  shinyjs::addClass("btSortTime", class = "scen-sort-by-selected")
  if(bt.sortTime.desc){
    updateSelectInput(session, "selLoadScen", choices = db$formatScenList(saved.scenarios, stime.identifier, desc = FALSE))
    updateActionButton(session, "btSortTime", label = lang$nav$dialogLoadScen$btSortTimeDESC, icon = icon("sort-by-order-alt", lib = "glyphicon"))
    bt.sortTime.desc <<- FALSE
  }else{
    updateSelectInput(session, "selLoadScen", choices = db$formatScenList(saved.scenarios, stime.identifier, desc = TRUE))
    updateActionButton(session, "btSortTime", label = lang$nav$dialogLoadScen$btSortTimeASC, icon = icon("sort-by-order", lib = "glyphicon"))
    bt.sortTime.desc <<- TRUE
  }
})

# load scenario confirmed
observeEvent(input$btLoadScenConfirm, {
  flog.debug("Confirm load scenario button clicked.")
  selected.scen <- regmatches(isolate(input$selLoadScen), regexpr("_", isolate(input$selLoadScen)), invert = TRUE)
  sids.to.load  <<- lapply(selected.scen, '[[', 1)
  rm(selected.scen)
  # if in comparison mode skip input data check
  if(scen.comp.mode){
    if(is.null(isolate(rv$btOverrideScen))){
      rv$btOverrideScen <<- 1
    }else{
      rv$btOverrideScen <<- isolate(rv$btOverrideScen + 1)
    }
    return(NULL)
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
  flog.debug("Loading and rendering scenarios: '%s'. Scenario comparison mode: '%s' 
             (0 = no comp mode, 1 = multiple view comparison mode, 2= left window split view, 
             3 = right window split view).", paste(sids.to.load, collapse = ", "), scen.comp.mode)
  if(!length(sids.to.load)){
    return(NULL)
  }
  
  # load selected scenarios from database
  errMsg <- NULL
  tryCatch({
    scenDataTmp <- db$loadScenarios(unlist(sids.to.load, use.names = FALSE), msgProgress = lang$progressBar$loadScenDb)
  }, error = function(e){
    flog.error("Some error occurred loading scenarios: '%s' from database. Error message: %s.", paste(sids.to.load, collapse = ", "), e)
    errMsg <<- lang$errMsg$loadScen$desc
  })
  if(is.null(showErrorMsg(lang$errMsg$loadScen$title, errMsg))){
    return(NULL)
  }
  
  if(!scen.comp.mode){
    # close currently opened scenario
    if(!closeScenario()){
      return(NULL)
    }
    activeScen <<- Scenario$new(db = db, sid = sids.to.load[[1]])

    # check whether all input datasets were imported
    if(length(modelOut)){
      scenInputData <- scenDataTmp[[1]][-seq_along(modelOut)]
    }else{
      scenInputData <- scenDataTmp[[1]]
    }
    if(length(input.ds.names) != length(scenInputData)){
      flog.error("Length of data provided (%d) does not match the number of input sheets (%d).", length(scenInputData), length(input.ds.names))
      if(is.null(showErrorMsg(lang$nav$dialogLoadScen$titleDataError, lang$nav$dialogLoadScen$descDataError))){
        return(NULL)
      }
    }
    names(scenInputData)         <- input.ds.names
    count.new.input             <- 0
    
    if(scalars.file.name %in% input.ds.names){
      scalar.dataset <- scenInputData[[length(scenInputData)]]
    }else{
      scalar.dataset <- NULL
    }
    
    errMsg    <-  NULL
    load.mode <-  "scen"
    source("./modules/input_load.R", local = TRUE)
    if(!is.null(errMsg)){
      return(NULL)
    }
    removeModal()
    # render output data (not compare mode)
    # generate data
    no.output <- TRUE
    # load scalar data if available
    idx.scalarOut <- match(tolower(modelName %+% "_" %+% scalars.out.name), scen.table.names)[[1]]
    lapply(seq_along(modelOut), function(i){
      if(!nrow(scenDataTmp[[1]][[i]])){
        scenData[["scen_1_"]][[i]] <<- scenDataTemplate[[i]]
      }else{
        no.output <<- FALSE
        scenData[["scen_1_"]][[i]] <<- scenDataTmp[[1]][[i]]
        if(identical(i, idx.scalarOut)){
          # scalar data exists
          remove.rows                <- grepl(config$gamsMetaDelim, scenData[["scen_1_"]][[i]][[2]])
          scalarData[["scen_1_"]]    <<- scenData[["scen_1_"]][[i]][remove.rows, ]
          scenData[["scen_1_"]][[i]] <<- scenData[["scen_1_"]][[i]][!remove.rows, ]
        }else{
          scalarData[["scen_1_"]]    <<- data.frame()
        }
      }
    })
    scenMetaData[["scen_1_"]] <<- activeScen$getMetadata(uidAlias = lang$nav$excelExport$metadataSheet$uid, 
                                                         snameAlias = lang$nav$excelExport$metadataSheet$sname, 
                                                         stimeAlias = lang$nav$excelExport$metadataSheet$stime)
    if(no.output){
      no.output.data <<- TRUE
    }else{
      no.output.data <<- FALSE
    }
    # rendering tables and graphs
    renderOutputData()
    
    flog.debug("Scenario: '%s' was loaded into UI", sids.to.load[[1]])
    
    # update scenario name
    rv$active.sname  <<- activeScen$getScenName()
    #no.check[]       <<- TRUE
    #lapply(seq_along(modelIn), function(i){
    #  switch(modelIn[[i]]$type,
    #         hot = {
    #           if(identical(dplyr::all_equal(previous.input.data[[i]], model.input.data[[i]]), TRUE)){
    #             no.check[[i]] <<- FALSE
    #             hot.init[[i]] <<- TRUE
    #           }
    #         },
    #         {
    #           if(identical(all(previous.input.data[[i]] == model.input.data[[i]]), TRUE)){
    #             no.check[[i]]       <<- FALSE
    #             no.data.changes[i]  <<- TRUE
    #           }
    #         })
    #})
    # function ends here in case not in compare mode!
    return(NULL)
  }
  
  # scenario comparison mode
  
  # check whether there are enough free slots left to import scenarios
  if(sum(!occupied.sid.slots) < length(sids.to.load)){
    flog.info("Maximum number of scenarios in scenario comparison mode reached (%d).", length(occupied.sid.slots))
    if(is.null(showErrorMsg(lang$errMsg$maxScen$title, lang$errMsg$maxScen$desc))){
      return(NULL)
    }
  }
  
  errMsg <- NULL
  lastImportedSid <- NULL
  lapply(seq_along(scenDataTmp), function(i){
    noError <- TRUE
    tryCatch({
      if(identical(scen.comp.mode, 1L)){
        scenId   <- isolate(rv$scenId)
      }else{
        # if in split view scenario id is max.number.scenarios + 2 (/3)
        scenId   <- scen.comp.mode
      }
      scen.str <- "scen_" %+% scenId %+% "_"
      
      metadata <- filter(saved.scenarios, UQ(as.name(sid.identifier)) == sids.to.load[[i]])
      # load scenario data
      scenData[[scen.str]]                    <<- scenDataTmp[[i]]
      # load scalar data if available
      idx.scalarOut <- match(tolower(modelName %+% "_" %+% scalars.out.name), scen.table.names)[[1]]
      if(!is.na(idx.scalarOut) && nrow(scenData[[scen.str]][[idx.scalarOut]])){
        # scalar data exists
        remove.rows                           <- grepl(config$gamsMetaDelim, scenData[[scen.str]][[idx.scalarOut]][[2]])
        scalarData[[scen.str]]                <<- scenData[[scen.str]][[idx.scalarOut]][remove.rows, ]
        scenData[[scen.str]][[idx.scalarOut]] <<- scenData[[scen.str]][[idx.scalarOut]][!remove.rows, ]
      }else{
        scalarData[[scen.str]]                <<- data.frame()
      }
      # add scenario metadata
      scenMetaData[[scen.str]]                <<- db$getMetadata(uid = metadata[[uid.identifier]], sname = metadata[[sname.identifier]], stime = metadata[[stime.identifier]],
                                                                 uidAlias = lang$nav$excelExport$metadataSheet$uid, snameAlias = lang$nav$excelExport$metadataSheet$sname, 
                                                                 stimeAlias = lang$nav$excelExport$metadataSheet$stime)
      source("./modules/scen_render.R", local = TRUE)
      
    }, error = function(e){
      flog.error(e)
      errMsg  <<- paste(errMsg, e, sep = "\n")
      noError <<- NULL
    })
    if(is.null(noError)){
      return(NULL)
    }
    flog.debug("Scenario: '%s' loaded into UI (compare mode).", sids.to.load[[i]])
    if(identical(scen.comp.mode, 1L)){
      lastImportedSid <<- scenId
      sids.loaded.in.scen.comp[scenId] <<- sids.to.load[[i]]
      occupied.sid.slots[scenId - 3] <<- TRUE
      sid.comp.order <<- c(sid.comp.order, scenId)
      rv$scenId <<- which.min(occupied.sid.slots) + 3
      scenCounterMultiComp <<- scenCounterMultiComp + 1
    }else{
      sids.loaded.in.split.comp[scen.comp.mode - 1] <<- sids.to.load[[i]]
    }
  })
  rm(scenDataTmp)
  if(identical(scen.comp.mode, 1L)){
    updateTabsetPanel(session, "scenTabset", selected = "scen_" %+% lastImportedSid %+% "_")
  }
  if(is.null(showErrorMsg(lang$errMsg$renderGraph$title, errMsg))){
    return(NULL)
  }
  
  removeModal()
  flog.debug("Scenarios: '%s' loaded and rendered in scenario comparison mode.", paste(sids.to.load, collapse = ", "))
  return(NULL)
})
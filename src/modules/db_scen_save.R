# save scenario to database

# Save As button clicked
observeEvent(input$btSaveAs, {
  saveAsFlag <<- TRUE
  flog.debug("%s: Save As button clicked.", uid)
  rv$btRemoveOutputData <<- isolate(rv$btRemoveOutputData + 1)
})
#Save button clicked
observeEvent(input$btSave, {
  saveAsFlag <<- FALSE
  flog.debug("%s: Save button clicked.", uid)
  rv$btRemoveOutputData <<- isolate(rv$btRemoveOutputData + 1)
})

observeEvent(virtualActionButton(rv$btRemoveOutputData), {
  saveOutput <<- TRUE
  if(dirtyFlag){
    showRemoveExistingOutputDataDialog()
  }else{
    if(saveAsFlag || is.null(activeScen) || !length(activeScen$getSid())){
      rv$btSaveAs <<- isolate(rv$btSaveAs + 1)
    }else{
      # overwrite current scenario data
      rv$btSaveConfirm <<- isolate(rv$btSaveConfirm + 1L)
    }
  }
})
observeEvent(input$btRemoveOutput, {
  flog.debug("%s: User confirmed that output data for scenario will be removed.", uid)
  saveOutput <<- FALSE
  if(saveAsFlag || is.null(activeScen) || !length(activeScen$getSid())){
    rv$btSaveAs <<- isolate(rv$btSaveAs + 1L)
  }else{
    # overwrite current scenario data
    rv$btSaveConfirm <<- isolate(rv$btSaveConfirm + 1L)
  }
})
observeEvent(input$btSaveOutput, {
  flog.debug("%s: User confirmed that output data for scenario will be saved regardless of possible corruption", uid)
  saveOutput <<- TRUE
  if(saveAsFlag || is.null(activeScen) || !length(activeScen$getSid())){
    rv$btSaveAs <<- isolate(rv$btSaveAs + 1L)
  }else{
    # overwrite current scenario data
    rv$btSaveConfirm <<- isolate(rv$btSaveConfirm + 1L)
  }
})
observeEvent(input$btSaveReadonly, 
             rv$btSaveAs <<- isolate(rv$btSaveAs + 1L)
)
# enter scenario name
observeEvent(virtualActionButton(rv$btSaveAs), {
  saveAsFlag <<- TRUE
  if(is.null(rv$activeSname)){
    tmpScenName <- lang$nav$dialogNewScen$newScenName
  }else{
    tmpScenName <- rv$activeSname
  }
  currentTags <- character(0L)
  if(length(activeScen)){
    currentTags <- activeScen$getStags()
  }
  showNewScenDialog(tmpScenName, scenTags = currentTags, 
                    showDiscardButtons = length(activeScen$getSid()) > 0L,
                    discardPermDefault = !identical(activeScen$getScenUid(), uid))
})

observeEvent(input$btNewName, {
  hideEl(session, "#scenarioExits")
  showEl(session, "#scenNameWrapper")
  showEl(session, "#dialogSaveInit")
  hideEl(session, "#dialogSaveConfirm")
  return(NULL)
})


# check scenario name
observeEvent(input$btCheckName, {
  scenName <- input$scenName
  scenTags <<- isolate(input$newScenTags)
  flog.debug("Name for new scenario entered: %s.", scenName)
  
  if(isBadScenName(scenName)){
    showHideEl(session, "#badScenarioName", 4000L)
    return()
  }else{
    errMsg <- NULL
    tryCatch({
      if(db$checkSnameExists(scenName)){
        scenExists <- TRUE
      }else{
        scenExists <- FALSE
      }
    }, error = function(e){
      flog.error("Some error occurred while checking whether scenario: '%s' exists. Error message: '%s'.", scenName, e)
      errMsg <<- lang$errMsg$fetchScenData$desc
    })
    if(is.null(showErrorMsg(lang$errMsg$fetchScenData$title, errMsg))){
      return()
    }
    if(scenExists){
      showEl(session, "#scenarioExits")
      hideEl(session, "#scenNameWrapper")
      hideEl(session, "#dialogSaveInit")
      showEl(session, "#dialogSaveConfirm")
      return(NULL)
    }else{
      scenTags         <<- scenTags
      rv[[input$btCheckName]] <- rv[[input$btCheckName]] + 1L
    }
  }
})
observeEvent(input$btSaveConfirm, 
             rv$btSaveConfirm <<- isolate(rv$btSaveConfirm + 1)
)
observeEvent(virtualActionButton(rv$btSaveConfirm), {
  # check whether scenario is currently locked
  errMsg <- NULL
  if(!is.null(activeScen) && !saveAsFlag){
    tryCatch({
      if(activeScen$isReadonlyOrLocked){
        showReadonlyDialog()
        flog.info("Scenario is readonly or locked.")
        return(NULL)
      }
    }, error = function(e){
      flog.error("Problems while trying to determine whether scenario is readonly or locked. Error message: %s.", e)
      errMsg <<- lang$errMsg$lockScen$desc
    })
    if(is.null(showErrorMsg(lang$errMsg$lockScen$title, errMsg))){
      return(NULL)
    }
  }
  
  source("./modules/scen_save.R", local = TRUE)
  if(is.null(showErrorMsg(lang$errMsg$GAMSInput$title, errMsg))){
    return(NULL)
  }
  removeModal()
  # save to database
  scenStr <- "scen_1_"
  duplicatedMetadata <- NULL
  tryCatch({
    if(saveAsFlag){
      if(!is.null(activeScen)){
        if(length(activeScen$getSid())){
          duplicatedMetadata <- activeScen$getMetadataInfo(input$newScenDiscardAttach, 
                                                           input$newScenDiscardPerm)
          activeScen <<- NULL
          gc()
        }else{
          activeScen$updateMetadata(newName = input$scenName, newTags = scenTags)
          if(isTRUE(input$newScenDiscardAttach)){
            attachments$removeAll()
          }
          if(isTRUE(input$newScenDiscardPerm)){
            activeScen$resetAccessPerm()
          }
        }
      }
      if(identical(input$scenName, rv$activeSname)){
        # make sure title is refreshed even when scen name is identical
        # (e.g. because owner changed)
        markUnsaved()
      }else{
        rv$activeSname <<- input$scenName
      }
      if(isTRUE(input$newScenDiscardViews)){
        views$clearConf()
      }
    }
    if(is.null(activeScen)){
      activeScen <<- Scenario$new(db = db, sname = isolate(rv$activeSname), 
                                  tags = scenTags, overwrite = identical(saveAsFlag, TRUE),
                                  isNewScen = TRUE, duplicatedMetadata = duplicatedMetadata,
                                  views = views, attachments = attachments)
      scenTags   <<- NULL
    }
    activeScen$save(scenData[[scenStr]], msgProgress = lang$progressBar$saveScenDb)
    if(saveOutput){
      if(config$saveTraceFile && length(traceData)){
        activeScen$saveTraceData(traceData)
      }
      if(!is.null(scriptOutput) && scriptOutput$hasResults()){
        activeScen$saveScriptResults(scriptOutput$getResults())
      }
    }
    scenMetaData[["scen_1_"]] <<- activeScen$getMetadata(lang$nav$excelExport$metadataSheet)
    flog.debug("%s: Scenario saved to database (Scenario: %s).", uid, activeScen$getScenName())
  }, error = function(e) {
    flog.error("Some error occurred saving scenario to database. Error message: %s.",
               conditionMessage(e))
    errMsg <<- lang$errMsg$saveScen$desc
  })
  if(is.null(showErrorMsg(lang$errMsg$saveScen$title, errMsg))){
    return(NULL)
  }
  # check whether output data was saved and in case it was set identifier accordingly
  if(!LAUNCHHCUBEMODE && any(vapply(scenData[[scenStr]][seq_along(modelOut)], 
                                    hasContent, logical(1L), USE.NAMES = FALSE))){
    noOutputData <<- FALSE
  }else{
    noOutputData <<- TRUE
  }
  if(!saveOutput){
    # remove output from UI
    renderOutputData(rendererEnv, views)
  }
  
  # reset dirty flag and unsaved status
  markSaved()
})
observeEvent(input$btEditMeta, {
  req(activeScen)
  
  attachmentMetadata <- NULL
  viewsMetadata <- NULL
  if(config$activateModules$attachments){
    attachmentList <<- attachments$getMetadata()
    attachmentMetadata <- attachmentList
    viewsMetadata <- views$getSummary(modelInRaw, modelOut)
  }
  showEditMetaDialog(activeScen$getMetadata(c(uid = "uid", sname = "sname", stime = "stime", stag = "stag",
                                              readPerm = "readPerm", writePerm = "writePerm", execPerm = "execPerm"), noPermFields = FALSE), 
                     allowAttachments = config$activateModules$attachments, 
                     attachmentMetadata = attachmentMetadata, 
                     viewsMetadata = viewsMetadata,
                     attachAllowExec = attachAllowExec, 
                     ugroups = c(uid, csv2Vector(ugroups)),
                     isLocked = length(activeScen) != 0L && length(activeScen$getLockUid()) > 0L)
})

observeEvent(input$btUpdateMeta, {
  req(activeScen)
  scenName <- isolate(input$editMetaName)
  hideEl(session, "#editMetaBadName")
  hideEl(session, "#editMetaError")
  hideEl(session, "#editMetaNameExists")
  hideEl(session, "#attachMaxSizeError")
  hideEl(session, "#attachMaxNoError")
  hideEl(session, "#attachDuplicateError")
  hideEl(session, "#attachUnknownError")
  
  if(isBadScenName(scenName)){
    showHideEl(session, "#editMetaBadName", 6000)
    return()
  }else{
    disableEl(session, "#btUpdateMeta")
    errMsg <- NULL
    tryCatch({
      if(db$checkSnameExists(scenName)){
        scenExists <- TRUE
      }else{
        scenExists <- FALSE
      }
    }, error = function(e){
      flog.error("Some error occurred while checking whether scenario: '%s' exists. Error message: '%s'.", 
                 scenName, e)
      errMsg <<- lang$errMsg$fetchScenData$desc
    })
    if(!is.null(errMsg)){
      showHideEl(session, "#editMetaError", 6000)
      return()
    }
    if((!identical(activeScen$getScenName(), scenName)) && scenExists){
      enableEl(session, "#btUpdateMeta")
      showHideEl(session, "#editMetaNameExists", 6000)
      return()
    }
    currentReadPerm <- activeScen$getReadPerm()
    currentWritePerm <- activeScen$getWritePerm()
    currentExecPerm <- activeScen$getExecPerm()
    
    activeUserGroups <- c(uid, csv2Vector(ugroups))
    
    
    if(activeScen$isReadonlyOrLocked){
      newWritePerm <- character(0L)
      newExecPerm <- character(0L)
      newReadPerm  <- character(0L)
    }else{
      newWritePerm <- input$editMetaWritePerm
      newExecPerm  <- input$editMetaExecPerm
      newReadPerm  <- input$editMetaReadPerm
      if(any(c(length(newReadPerm), length(newWritePerm), length(newExecPerm)) < 1L)){
        enableEl(session, "#btUpdateMeta")
        flog.debug("Empty permissions entered.")
        showHideEl(session, "#editMetaEmptyPerm", 6000)
        return()
      }
      if(any(!newReadPerm %in% c(activeUserGroups, currentReadPerm))){
        showHideEl(session, "#editMetaError")
        flog.error("Attempt to tamper with read access permissions!")
        return()
      }
      if(any(!newWritePerm %in% c(activeUserGroups, currentWritePerm))){
        showHideEl(session, "#editMetaError")
        flog.error("Attempt to tamper with write access permissions!")
        return()
      }
      if(any(!newExecPerm %in% c(activeUserGroups, currentExecPerm))){
        showHideEl(session, "#editMetaError")
        flog.error("Attempt to tamper with execute access permissions!")
        return()
      }
      scenOwner <- activeScen$getScenUid()[[1L]]
      if((!scenOwner %in% newReadPerm) 
         || (!scenOwner %in% newWritePerm) 
         || (!scenOwner %in% newExecPerm)){
        showHideEl(session, "#editMetaIncapOwner")
        flog.debug("Attempt to revoke the scenario owner's access rights.")
        return()
      }
    }
    
    tryCatch({
      activeScen$updateMetadata(scenName, isolate(input$editMetaTags), 
                                newReadPerm, newWritePerm, newExecPerm)
      rv$activeSname <- scenName
      scenMetaData[["scen_1_"]] <<- activeScen$getMetadata(lang$nav$excelExport$metadataSheet)
      markUnsaved()
      removeModal()
    }, error = function(e){
      flog.error("Problems updating scenario metadata. Error message: %s.", e)
      errMsg <<- character(1L)
    })
    if(!is.null(errMsg)){
      enableEl(session, "#btUpdateMeta")
      showHideEl(session, "#editMetaError")
      return()
    }
  }
})

if(config$activateModules$attachments){
  ################################################
  #            ATTACHMENTS
  ################################################
  lapply(seq_len(attachMaxNo), function(i){
    observeEvent(input[["btRemoveAttachment_" %+% i]], {
      req(nchar(attachmentList[["name"]][[i]]) > 0L, activeScen)
      attachments$remove(session = NULL, attachmentList[["name"]][[i]])
      attachmentList[i, ] <<- list(NA_character_, FALSE)
      showHideEl(session, "#attachSuccess")
    })
    observe({
      value <- suppressWarnings(as.logical(input[["execPermAttachment_" %+% i]]))
      req(activeScen)
      if(is.na(attachmentList[["name"]][[i]]) || length(value) != 1L || is.na(value) ||
         value == attachmentList[["execPerm"]][[i]]){
        return(NULL)
      }
      tryCatch({
        attachments$setExecPerm(session = NULL, attachmentList[["name"]][[i]], 
                                value)
        attachmentList[i, "execPerm"] <<- value
        showHideEl(session, "#attachSuccess")
      }, error = function(e){
        flog.error(e)
        showHideEl(session, "#attachUnknownError", 6000) 
      })
      
    })
  })
  output$downloadAttachmentData <- downloadHandler(
    filename = function() { 
      i <- isolate(input$downloadAttachment)
      if(!is.integer(i) || length(attachmentList[["name"]]) < i){
        return("error.txt")
      }
      attachmentList[["name"]][[input$downloadAttachment]] },
    content = function(file) {
      i <- input$downloadAttachment
      if(!is.integer(i) || length(attachmentList[["name"]]) < i){
        return(writeLines("error", file))
      }
      attachments$download(file, fileNames = attachmentList[["name"]][[i]], 
                           fullPath = TRUE)
    }
  )
  observeEvent(input$file_addAttachments$datapath, {
    req(activeScen)
    
    showEl(session, "#addAttachLoading")
    hideEl(session, "#attachMaxSizeError")
    hideEl(session, "#attachMaxNoError")
    hideEl(session, "#attachDuplicateError")
    hideEl(session, "#attachUnknownError")
    errMsg <- NULL
    
    tryCatch({
      attachments$add(session = NULL, isolate(input$file_addAttachments$datapath),
                      fileNames = isolate(input$file_addAttachments$name))
      idxes <- vector("integer", length(isolate(input$file_addAttachments$name)))
      for(i in seq_along(isolate(input$file_addAttachments$name))){
        idx <- which(is.na(attachmentList[["name"]]))
        if(!length(idx)){
          idx <- nrow(attachmentList) + 1L
          if(idx > attachMaxNo){
            stop("The number of attachments exceeds the maximum allowed number. 
               This is not supposed to happen as addAttachment method of Scenario class should raise an appropriate exception!", call. = FALSE)
          }
          attachmentList <<- add_row(attachmentList, name = isolate(input$file_addAttachments$name)[[i]], execPerm = TRUE)
        }else{
          idx <- idx[[1]]
          attachmentList[idx, ] <<- list(isolate(input$file_addAttachments$name)[[i]], TRUE)
        }
        idxes[[i]] <- idx
      }
      updateAttachList(session, fileName = isolate(input$file_addAttachments$name), id = idxes, token = session$token, 
                       labelCb = lang$nav$dialogEditMeta$attachmentsExecPerm, allowExec = attachAllowExec)
      showHideEl(session, "#attachSuccess")
    },
    error_forbidden_filename = function(e){
      flog.info("Attachment wasn't added because the filename is forbidden.")
      showHideEl(session, "#attachForbiddenFnameError", 6000)
    },
    error_max_size = function(e){
      flog.info("Attachment wasn't added because the size is too large.")
      showHideEl(session, "#attachMaxSizeError", 6000)
    },
    error_duplicate_files = function(e){
      flog.info("Attachment wasn't added because the filename already exists.")
      showHideEl(session, "#attachDuplicateError", 6000)
    },
    error_max_no = function(e){
      flog.info("Attachment wasn't added because the maximum number of attachment is reached.")
      showHideEl(session, "#attachMaxNoError", 6000)
    },
    error = function(e){
      flog.error(conditionMessage(e))
      showHideEl(session, "#attachUnknownError", 6000) 
    })
    hideEl(session, "#addAttachLoading")
  })
  ################################################
  #            VIEWS
  ################################################
  updateViewsTable <- function(){
    newViewData <- views$getSummary(modelInRaw, modelOut)
    if(!length(newViewData$symAlias)){
      newViewData <- tibble(symAlias = character(0L),
                            id = character(0L),
                            symName = character(0L))
    }
    session$sendCustomMessage("gms-updateTable", 
                              list(id = "currentViewsTable", hierarchical = TRUE,
                                   valCol = 2L,
                                   data = list(I(newViewData[["symAlias"]]),
                                               I(newViewData[["id"]]),
                                               I(newViewData[["symName"]]))))
  }
  observeEvent(input$removeViews, {
    req(activeScen)
    showEl(session, "#addViewsLoading")
    flog.debug("Request to remove views received.")
    on.exit(hideEl(session, "#addViewsLoading"))
    
    viewsToRemove <- tryCatch({
      safeFromJSON(input$removeViews,
                   simplifyMatrix = FALSE)
    }, error = function(e){
      flog.error("Problems parsing request to remove views. This seems like an attempt to tamper with the app! Error message: %s.",
                 conditionMessage(e))
      return(NULL)
    })
    if(is.null(viewsToRemove)){
      return()
    }
    if(length(viewsToRemove) == 0){
      showHideEl(session, "#viewsNoneSelected", delay = 4000L)
      return()
    }
    
    if(tryCatch({
      views$removeConf(viewsToRemove)
      FALSE
    }, error = function(e){
      flog.warn("Problems removing rows from table: %s. Error message: %s.",
                as.character(input$removeViews), conditionMessage(e))
      return(TRUE)
    })){
      showHideEl(session, "#viewsUnknownError", 4000L)
      return()
    }
    updateViewsTable()
  })
  output$downloadViews <- downloadHandler(
    filename = function() {
      scenName <- isolate(rv$activeSname)
      if(!length(scenName)){
        scenName <- lang$nav$dialogNewScen$newScenName
      }
      return(paste0(scenName, "_views.json"))
    },
    content = function(file){
      flog.debug("Request to download views received.")
      tryCatch(write_file(views$getJSON(safeFromJSON(input$downloadViews,
                                                     simplifyMatrix = FALSE)), file),
               error = function(e){
                 flog.warn("Problems writing views JSON file. Error message: %s",
                           conditionMessage(e))
                 return(writeLines('{"error": "Some problem occurred while writing views."}', file))
               })
    }, contentType = "application/json"
  )
  observeEvent(input$file_addViews$datapath, {
    req(activeScen)
    flog.debug("New view configuration uploaded.")
    showEl(session, "#addViewsLoading")
    on.exit(hideEl(session, "#addViewsLoading"))
    
    if(!identical(try(tolower(tools::file_ext(input$file_addViews$datapath)),
                      silent = TRUE), "json")){
      showHideEl(session, "#viewsInvalidDataError", 4000L)
      return()
    }
    
    if(tryCatch({
      views$addConf(safeFromJSON(read_file(input$file_addViews$datapath),
                                 simplifyDataFrame = FALSE, simplifyVector = FALSE))
      invalidViews <- views$getInvalidViews()
      if(length(invalidViews)){
        showHideEl(session, "#viewsCustomError",
                   msg = sprintf(lang$nav$dialogEditMeta$viewsInvalidError,
                                 paste(invalidViews, collapse = "', '")), 4000L)
      }
      FALSE
    }, error = function(e){
      flog.info("Problems adding views configuration. Error message: %s",
                conditionMessage(e))
      showHideEl(session, "#viewsInvalidDataError", 4000L)
      TRUE
    })){
      return()
    }
    duplicatedViews <- views$getDuplicatedViews()
    if(length(duplicatedViews)){
      showHideEl(session, "#viewsCustomError",
                 msg = sprintf(lang$nav$dialogEditMeta$viewsDuplicateError,
                               paste(duplicatedViews, collapse = "', '")),
                 4000L)
    }
    updateViewsTable()
  })
}
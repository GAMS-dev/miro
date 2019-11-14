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
  if(!is.null(rv$activeSname)){
    tmpScenName <- rv$activeSname
  }else{
    tmpScenName <- lang$nav$dialogNewScen$newScenName
  }
  currentTags <- character(0L)
  if(length(activeScen) && !length(activeScen$getSid())){
    currentTags <- activeScen$getStags()
  }
  showNewScenDialog(tmpScenName, scenTags = currentTags)
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
      rv$activeSname   <- scenName
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
  tryCatch({
    if(saveAsFlag){
      if(!is.null(activeScen)){
        if(!length(activeScen$getSid())){
          activeScen$updateMetadata(newName = input$scenName, newTags = scenTags)
        }else{
          activeScen <<- NULL
          gc()
        }
      }
      rv$activeSname <<- input$scenName
    }
    if(is.null(activeScen)){
      activeScen <<- Scenario$new(db = db, sname = isolate(rv$activeSname), 
                                  tags = scenTags, overwrite = identical(saveAsFlag, TRUE),
                                  isNewScen = TRUE)
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
    flog.debug("%s: Scenario saved to database (Scenario: %s).", uid, activeScen$getScenName())
  }, error = function(e) {
    flog.error("Some error occurred saving scenario to database. Error message: %s.", e)
    errMsg <<- lang$errMsg$saveScen$desc
  })
  if(is.null(showErrorMsg(lang$errMsg$saveScen$title, errMsg))){
    return(NULL)
  }
  # check whether output data was saved and in case it was set identifier accordingly
  if(any(vapply(scenData[[scenStr]][seq_along(modelOut)], 
                hasContent, logical(1L), USE.NAMES = FALSE))){
    noOutputData <<- FALSE
  }else{
    noOutputData <<- TRUE
  }
  if(!saveOutput){
    # remove output from UI
    renderOutputData()
  }
  
  # reset dirty flag and unsaved status
  markSaved()
})
observeEvent(input$btEditMeta, {
  req(activeScen)
  
  attachmentMetadata <- NULL
  if(config$activateModules$attachments){
    attachmentList <<- activeScen$fetchAttachmentList()
    attachmentMetadata <- attachmentList
  }
  showEditMetaDialog(activeScen$getMetadata(c(uid = "uid", sname = "sname", stime = "stime", stag = "stag",
                                              readPerm = "readPerm", writePerm = "writePerm", execPerm = "execPerm"), noPermFields = FALSE), 
                     allowAttachments = config$activateModules$attachments, 
                     attachmentMetadata = attachmentMetadata, 
                     attachAllowExec = attachAllowExec, 
                     ugroups = c(uid, csv2Vector(ugroups)))
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
    
    
    if(any(activeUserGroups %in% currentWritePerm)){
      newWritePerm <- input$editMetaWritePerm
      newExecPerm  <- input$editMetaExecPerm
      newReadPerm  <- input$editMetaReadPerm
    }else{
      newWritePerm <- currentWritePerm
      newExecPerm <- currentExecPerm
      newReadPerm  <- currentReadPerm
    }
    
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
    tryCatch({
      activeScen$updateMetadata(scenName, isolate(input$editMetaTags), 
                                newReadPerm, newWritePerm, newExecPerm)
      rv$activeSname <- scenName
      scenMetaData[["scen_1_"]] <<- activeScen$getMetadata(lang$nav$excelExport$metadataSheet)
      hideEl(session, "#editMetaUI")
      showEl(session, "#editMetaSuccess")
      markUnsaved()
      hideModal(session, 1L)
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
  lapply(seq_len(attachMaxNo), function(i){
    observeEvent(input[["btRemoveAttachment_" %+% i]], {
      req(nchar(attachmentList[["name"]][[i]]) > 0L, activeScen)
      activeScen$removeAttachments(attachmentList[["name"]][[i]])
      attachmentList[i, ] <<- c(NA_character_, FALSE)
      markUnsaved()
      showHideEl(session, "#attachSuccess")
    })
    observe({
      input[["btexecPermAttachment_" %+% i]]
      value <- input[["execPermAttachment_" %+% i]]
      req(activeScen)
      if(is.na(attachmentList[["name"]][[i]]) || is.null(value) ||
         identical(value, attachmentList[["execPerm"]][[i]])){
        return(NULL)
      }
      tryCatch({
        activeScen$setAttachmentExecPerm(attachmentList[["name"]][[i]], 
                                         value, workDir = workDir)
        attachmentList[i, "execPerm"] <<- value
        markUnsaved()
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
      activeScen$downloadAttachmentData(file, fileNames = attachmentList[["name"]][[i]], 
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
      activeScen$addAttachments(isolate(input$file_addAttachments$datapath), workDir = workDir,
                                fileNames = isolate(input$file_addAttachments$name),
                                forbiddenFnames = c(paste0(c(names(modelInTabularData), 
                                                             names(modelOut)), ".csv"), paste0(modelName, c(".log", ".lst"))))
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
          attachmentList[idx, ] <<- c(isolate(input$file_addAttachments$name)[[i]], TRUE)
        }
        idxes[[i]] <- idx
      }
      updateAttachList(session, fileName = isolate(input$file_addAttachments$name), id = idxes, token = session$token, 
                       labelCb = lang$nav$dialogEditMeta$attachmentsExecPerm, allowExec = attachAllowExec)
      markUnsaved()
      showHideEl(session, "#attachSuccess")
    }, error = function(e){
      errMsg <<- character(1L)
      switch(conditionMessage(e),
             maxSizeException = {
               flog.info("Attachment wasn't added because the size is too large.")
               showHideEl(session, "#attachMaxSizeError", 6000)
             },
             maxNoException = {
               flog.info("Attachment wasn't added because the maximum number of attachment is reached.")
               showHideEl(session, "#attachMaxNoError", 6000)
             },
             duplicateException = {
               flog.info("Attachment wasn't added because the filename already exists.")
               showHideEl(session, "#attachDuplicateError", 6000)
             },
             forbiddenFnameException = {
               flog.info("Attachment wasn't added because the filename is forbidden.")
               showHideEl(session, "#attachForbiddenFnameError", 6000)
             },
             roException = {
               flog.info("Attachment wasn't added because scenario is readonly.")
               showHideEl(session, "#attachRO", 6000)
             },
             {
               flog.error(e)
               showHideEl(session, "#attachUnknownError", 6000) 
             })
    })
    hideEl(session, "#addAttachLoading")
  })
}

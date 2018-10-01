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
    if(saveAsFlag || is.null(isolate(rv$activeSname))){
      rv$btSaveAs <<- isolate(rv$btSaveAs + 1)
    }else{
      # overrride current scenario data
      rv$btSaveConfirm <<- isolate(rv$btSaveConfirm + 1L)
    }
  }
})
observeEvent(input$btRemoveOutput, {
  flog.debug("%s: User confirmed that output data for scenario will be removed.", uid)
  saveOutput <<- FALSE
  if(saveAsFlag || is.null(isolate(rv$activeSname))){
    rv$btSaveAs <<- isolate(rv$btSaveAs + 1L)
  }else{
    # overrride current scenario data
    rv$btSaveConfirm <<- isolate(rv$btSaveConfirm + 1L)
  }
})
observeEvent(input$btSaveOutput, {
  flog.debug("%s: User confirmed that output data for scenario will be saved regardless of possible corruption", uid)
  saveOutput <<- TRUE
  if(saveAsFlag || is.null(isolate(rv$activeSname))){
    rv$btSaveAs <<- isolate(rv$btSaveAs + 1L)
  }else{
    # overrride current scenario data
    rv$btSaveConfirm <<- isolate(rv$btSaveConfirm + 1L)
  }
})
observeEvent(input$btSaveReadonly, 
             rv$btSaveAs <<- isolate(rv$btSaveAs + 1L)
             )
# enter scenario name
observeEvent(virtualActionButton(rv$btSaveAs), {
  if(!is.null(isolate(rv$activeSname))){
    tmpScenName <- isolate(rv$activeSname)
  }else if(!is.null(activeSnameTmp)){
    tmpScenName <- activeSnameTmp
  }else{
    tmpScenName <- lang$nav$dialogNewScen$newScenName
  }
  showNewScenDialog(tmpScenName)
})

observeEvent(input$btNewName, {
  shinyjs::hide("scenarioExits")
  shinyjs::show("scenName")
  shinyjs::show("dialogSaveInit")
  shinyjs::hide("dialogSaveConfirm")
  return(NULL)
})


# check scenario name
observeEvent(input$btCheckName, {
  scenName <- input$scenName
  flog.debug("Name for new scenario entered: %s.", scenName)
  
  if(grepl("^\\s*$", scenName)){
    shinyjs::show("badScenarioName")
    return(NULL)
  #}else if(!is.null(isolate(rv$activeSname)) && scenName == isolate(rv$activeSname)){
  #  rv$btSaveConfirm <<- isolate(rv$btSaveConfirm + 1)
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
      return(NULL)
    }
    if(scenExists){
      shinyjs::show("scenarioExits")
      shinyjs::hide("scenName")
      shinyjs::hide("dialogSaveInit")
      shinyjs::show("dialogSaveConfirm")
      return(NULL)
    }else{
      rv$activeSname <<- scenName
      rv$btSaveConfirm <<- isolate(rv$btSaveConfirm + 1)
    }
  }
})
observeEvent(input$btSaveConfirm, 
             rv$btSaveConfirm <<- isolate(rv$btSaveConfirm + 1)
             )
observeEvent(virtualActionButton(rv$btSaveConfirm), {
  # check whether scenario is currently locked
  errMsg <- NULL
  if(config$activateModules$sharedScenarios && !is.null(activeScen)){
    tryCatch({
      if(activeScen$isReadonlyOrLocked){
        activeScen <<- NULL
        showReadonlyDialog()
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
  removeModal()
  
  # save to database
  scenStr <- "scen_1_"
  tryCatch({
    if(is.null(activeScen) || saveAsFlag){
      if(saveAsFlag){
        rv$activeSname <<- isolate(input$scenName)
      }
      activeScen <<- Scenario$new(db = db, sname = isolate(rv$activeSname))
    }
    activeScen$save(scenData[[scenStr]], msgProgress = lang$progressBar$saveScenDb)
    if(config$saveTraceFile){
      activeScen$saveTraceData(traceData)
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
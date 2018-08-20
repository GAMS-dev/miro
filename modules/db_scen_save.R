# save scenario to database

# Save As button clicked
observeEvent(input$btSaveAs, {
  saveAsFlag <<- T
  flog.debug("%s: Save As button clicked.", uid)
  rv$btRemoveOutputData <<- isolate(rv$btRemoveOutputData + 1)
})
#Save button clicked
observeEvent(input$btSave, {
  saveAsFlag <<- F
  flog.debug("%s: Save button clicked.", uid)
  rv$btRemoveOutputData <<- isolate(rv$btRemoveOutputData + 1)
})

observeEvent(virtualActionButton(rv$btRemoveOutputData), {
  save.output <<- T
  if(dirty.flag){
    showRemoveExistingOutputDataDialog()
  }else{
    if(saveAsFlag || is.null(isolate(rv$active.sname))){
      rv$btSaveAs <<- isolate(rv$btSaveAs + 1)
    }else{
      # overrride current scenario data
      rv$btSaveConfirm <<- isolate(rv$btSaveConfirm + 1)
    }
  }
})
observeEvent(input$btRemoveOutput, {
  flog.debug("%s: User confirmed that output data for scenario will be removed.", uid)
  save.output <<- F
  if(saveAsFlag || is.null(isolate(rv$active.sname))){
    rv$btSaveAs <<- isolate(rv$btSaveAs + 1)
  }else{
    # overrride current scenario data
    rv$btSaveConfirm <<- isolate(rv$btSaveConfirm + 1)
  }
})
observeEvent(input$btSaveOutput, {
  flog.debug("%s: User confirmed that output data for scenario will be saved regardless of possible corruption", uid)
  save.output <<- T
  if(saveAsFlag || is.null(isolate(rv$active.sname))){
    rv$btSaveAs <<- isolate(rv$btSaveAs + 1)
  }else{
    # overrride current scenario data
    rv$btSaveConfirm <<- isolate(rv$btSaveConfirm + 1)
  }
})

# enter scenario name
observeEvent(virtualActionButton(rv$btSaveAs, input$btSaveReadonly), {
  if(!is.null(isolate(rv$active.sname))){
    tmpScenName <- isolate(rv$active.sname)
  }else if(!is.null(active.sname.tmp)){
    tmpScenName <- active.sname.tmp
  }else{
    tmpScenName <- lang$nav$dialogNewScen$newScenName
  }
  showNewScenDialog(tmpScenName)
})

observeEvent(input$btNewName, {
  shinyjs::hide("scen.exits")
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
    shinyjs::show("bad.scen.name")
    return(NULL)
  #}else if(!is.null(isolate(rv$active.sname)) && scenName == isolate(rv$active.sname)){
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
      shinyjs::show("scen.exits")
      shinyjs::hide("scenName")
      shinyjs::hide("dialogSaveInit")
      shinyjs::show("dialogSaveConfirm")
      return(NULL)
    }else{
      rv$active.sname <<- scenName
      rv$btSaveConfirm <<- isolate(rv$btSaveConfirm + 1)
    }
  }
})

observeEvent(virtualActionButton(input$btSaveConfirm, rv$btSaveConfirm), {
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
  scen.str <- "scen_1_"
  tryCatch({
    if(is.null(activeScen) || saveAsFlag){
      if(is.null(isolate(rv$active.sname))){
        rv$active.sname <<- isolate(input$scenName)
      }
      activeScen <<- Scenario$new(db = db, sname = isolate(rv$active.sname))
    }
    activeScen$save(scenData[[scen.str]], msgProgress = lang$progressBar$saveScenDb)
    #showModal(modalDialog(
    #  title = lang$nav$dialogSaveSuccess$title, 
    #  HTML(addHtmlLineBreaks(sprintf(lang$nav$dialogSaveSuccess$desc, count.datasets.saved)))
    #))
    flog.debug("%s: Scenario saved to database (Scenario: %s).", uid, activeScen$getScenName())
  }, error = function(e) {
    flog.error("Some error occurred saving scenario to database. Error message: %s.", e)
    errMsg <<- lang$errMsg$saveScen$desc
  })
  if(is.null(showErrorMsg(lang$errMsg$saveScen$title, errMsg))){
    return(NULL)
  }
  
  # check whether output data was saved and in case it was set identifier accordingly
  if(any(vapply(scenData[[scen.str]][seq_along(modelOut)], hasContent, logical(1L), USE.NAMES = FALSE))){
    no.output.data <<- FALSE
  }else{
    no.output.data <<- TRUE
  }
  if(!save.output){
    # remove output from UI
    renderOutputData()
  }
  
  # reset dirty flag and unsaved status
  markSaved()
})
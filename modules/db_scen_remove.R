#remove the currently active scenario
closeScenario <- function(){
  # remove output data
  errMsg <- NULL
  lapply(seq_along(modelOut), function(i){
    scenData[["scen_1_"]][[i]] <<- scenDataTemplate[[i]]
  })
  # clear scalar data
  scalarData[["scen_1_"]] <<- data.frame()
  traceData <<- data.frame()
  # reset input data sheets
  modelInputData     <<- modelInTemplate
  tableContent       <<- vector(mode = "list", length = length(modelIn))
  inputInitialized[] <<- FALSE
  isolate(rv$datasetsModified[] <- FALSE)
  
  if(resetWidgetsOnClose){
    lapply(seq_along(modelIn), function(i){
      switch(modelIn[[i]]$type,
             hot = {
               # set identifier that data was overwritten 
               hotInit[i]        <<- FALSE
               isEmptyInput[i]   <<- TRUE
             },
             slider = {
               if(is.null(modelInWithDep[[names(modelIn)[[i]]]])){
                 if(length(modelIn[[i]]$slider$default))
                   updateSliderInput(session, paste0("slider_", i), value = modelIn[[i]]$slider$default)
               }else{
                 showEl(session, "#no_data_dep_" %+% i)
                 hideEl(session, "#slider_" %+% i)
                 updateSliderInput(session, "slider_" %+% i, min = 0, max = 1, 
                                   value = 0, step = 1)
               }
             },
             dropdown = {
               if(is.null(modelInWithDep[[names(modelIn)[[i]]]])){
                 if(length(modelIn[[i]]$dropdown$selected))
                   updateSelectInput(session, paste0("dropdown_", i), 
                                     selected = modelIn[[i]]$dropdown$selected)
               }else{
                 selectedDepEl[[i]] <<- character(0)
                 showEl(session, "#no_data_dep_" %+% i)
                 hideEl(session, "#dropdown_" %+% i)
                 updateSelectInput(session, "dropdown_" %+% i, choices = "_", selected = "_")
               }
             },
             date = {
               if(is.null(modelInWithDep[[names(modelIn)[[i]]]]) && length(modelIn[[i]]$date$value)){
                 updateDateInput(session, "date_" %+% i, value = modelIn[[i]]$date$value)
               }
               previousInputData[[i]] <<- isolate(input[["date_" %+% i]])
             },
             daterange = {
               if(is.null(modelInWithDep[[names(modelIn)[[i]]]]) && length(modelIn[[i]]$daterange$start) &&
                  length(modelIn[[i]]$daterange$end)){
                 updateDateRangeInput(session, "daterange_" %+% i, 
                                      start = modelIn[[i]]$daterange$start, 
                                      end = modelIn[[i]]$daterange$end)
               }
             },
             checkbox = {
               if(length(modelIn[[i]]$checkbox$value)){
                 updateCheckboxInput(session, "cb_" %+% i, value = modelIn[[i]]$checkbox$value)
               }
             },
             textinput = {
               if(length(modelIn[[i]]$textinput$value)){
                 updateTextInput(session, "text_" %+% i, value = modelIn[[i]]$textinput$value)
               }
             }
      )
      # make sure data is cleaned even when modified manually 
      # (and thus rv$in_i is NULL)
      if(is.null(isolate(rv[["in_" %+% i]]))){
        rv[["in_" %+% i]] <- 1L
      }
      rv[["in_" %+% i]]    <- NULL
    })
  }
  resetWidgetsOnClose <<- TRUE
  
  unlink(list.files(workDir, recursive = TRUE))
  if(is.R6(activeScen))
    flog.debug("Scenario: '%s' closed.", activeScen$getScenName())
  # reset input data
  lapply(seq_along(modelIn), function(i){
    hideEl(session, "#graph-in_" %+% i)
    showEl(session, "#data-in_" %+% i)
  })
  # load external data
  lapply(seq_along(externalInputData), function(i){
    if(length(externalInputData[[i]]) && nrow(externalInputData[[i]])){
      modelInputData[[i]] <<- externalInputData[[i]]
      
      if(length(isolate(rv[[paste0("in_", i)]]))){
        rv[[paste0("in_", i)]] <<- isolate(rv[[paste0("in_", i)]]) + 1L
      }else{
        rv[[paste0("in_", i)]] <<- 1L
      }
    }
  })
  
  # reset model output data
  renderOutputData()
  activeScenario    <<- NULL
  activeScen        <<- NULL
  gc()
  activeSnameTmp    <<- NULL
  rv$activeSname    <<- NULL
  scenTags          <<- NULL
  noCheck[]         <<- FALSE
  attachmentList    <<- tibble(name = vector("character", attachMaxNo), 
                               execPerm = vector("logical", attachMaxNo))
  if(length(modelInMustImport))
    disableEl(session, "#btSolve")
  markSaved()
  noOutputData      <<- TRUE
  if(!is.null(errMsg)){
    invisible(FALSE)
  }else{
    invisible(TRUE)
  }
}

observeEvent(input$btDelete, {
  req(activeScen)
  flog.debug("Button to delete scenario from database clicked.")
  showDeleteScenDialog()
})
observeEvent(input$btDeleteConfirm, {
  flog.debug("Button to confirm deleting scenario from database clicked.")
  if(is.null(activeScen)){
    flog.error("No active scenario ID found to delete.")
    return()
  }
  errMsg <- NULL
  tryCatch({
    activeScen$delete()
    hideEl(session, "#deleteScen_db")
    hideEl(session, "#btDeleteConfirm")
    showEl(session, "#deleteScen_ui")
    showEl(session, "#btRemoveDeletedConfirm")
  }, error = function(e){
    flog.error("Problems deleting scenario: '%s'. Error message: '%s'.", activeScen$getScenName(), e)
    errMsg <<- lang$errMsg$deleteScen$desc
  })
  if(!is.null(errMsg)){
    showErrorMsg(lang$errMsg$deleteScen$title, errMsg)
    return()
  }
  activeScen <<- NULL
})

# button changes from NULL to 0 when initialised (modalDialog opens)
# so code needs to be duplicated here
observeEvent(input$btRemoveDeletedConfirm, {
  flog.debug("Remove scenario data from UI confirmed.")
  closeScenario()
  removeModal()
})
observeEvent(input$btRemoveConfirm, {
  flog.debug("Remove scenario data from UI confirmed.")
  closeScenario()
  removeModal()
})

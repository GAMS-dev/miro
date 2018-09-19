#remove the currently active scenario
closeScenario <- function(){
  # remove output data
  lapply(seq_along(modelOut), function(i){
    scenData[["scen_1_"]][[i]] <<- scenDataTemplate[[i]]
  })
  # clear scalar data
  scalarData[["scen_1_"]] <<- data.frame()
  
  # reset input data sheets
  modelInputData      <<- modelInTemplate
  input.initialized[] <<- FALSE
  noDataChanges[]     <<- FALSE
  lapply(seq_along(modelIn), function(i){
    switch(modelIn[[i]]$type,
           hot = {
             # set identifier that data was overwritten 
             hotInit[i]        <<- FALSE
             isEmptyInput[i]   <<- TRUE
           },
           slider = {
             if(is.null(modelInWithDep[[names(modelIn)[[i]]]])){
               shiny::updateSliderInput(session, paste0("slider_", i), value = modelIn[[i]]$slider$default)
             }else{
               shinyjs::show("no_data_dep_" %+% i)
               shinyjs::hide("slider_" %+% i)
               shiny::updateSliderInput(session, "slider_" %+% i, min = NULL, max = NULL, 
                                  value = NULL, step = 1)
             }
           },
           dropdown = {
             if(is.null(modelInWithDep[[names(modelIn)[[i]]]])){
               shiny::updateSelectInput(session, paste0("dropdown_", i), selected = modelIn[[i]]$dropdown$selected)
             }else{
               shinyjs::show("no_data_dep_" %+% i)
               shinyjs::hide("dropdown_" %+% i)
               shiny::updateSelectInput(session, "dropdown_" %+% i, choices = character(0), selected = character(0))
             }
           },
           date = {
             if(is.null(modelInWithDep[[names(modelIn)[[i]]]])){
               shiny::updateDateInput(session, "date_" %+% i, value = modelIn[[i]]$date$value)
             }
             previousInputData[[i]] <<- isolate(input[["date_" %+% i]])
           },
           daterange = {
             if(is.null(modelInWithDep[[names(modelIn)[[i]]]])){
               shiny::updateDateRangeInput(session, "daterange_" %+% i, 
                                           start = modelIn[[i]]$daterange$start, 
                                           end = modelIn[[i]]$daterange$end)
             }
           },
           checkbox = {
             shiny::updateCheckboxInput(session, "cb_" %+% i, value = modelIn[[i]]$checkbox$value)
           }
    )
    rv[["in_" %+% i]]    <<- NULL
  })
  flog.debug("Scenario: '%s' closed.", activeScen$getScenName())
  # reset model output data
  renderOutputData()
  active.scenario   <<- NULL
  active.sid        <<- NULL
  activeScen        <<- NULL
  activeSnameTmp    <<- NULL
  rv$activeSname    <<- NULL
  noCheck[]         <<- FALSE
  markSaved()
  noOutputData      <<- TRUE
  if(!is.null(errMsg)){
    invisible(FALSE)
  }else{
    invisible(TRUE)
  }
}

observeEvent(input$btDelete, {
  flog.debug("Button to delete scenario from database clicked.")
  showDeleteScenDialog()
})
observeEvent(input$btDeleteConfirm, {
  flog.debug("Button to confirm deleting scenario from database clicked.")
  removeModal()
  if(is.null(activeScen)){
    flog.error("No active scenario ID found to delete.")
    return(NULL)
  }
  errMsg <- NULL
  tryCatch({
    activeScen$delete()
    showRemoveDeletedScenFromUIDialog()
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

# remove scenario from UI
observeEvent(virtualActionButton(input$btRemove, input$btRemoveO), {
  flog.debug("Remove scenario data from UI button clicked.")
  showRemoveActiveScenFromUIDialog()
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

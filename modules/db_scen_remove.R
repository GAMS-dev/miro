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
  modelInputData     <<- vector(mode = "list", length = length(modelIn))
  inputInitialized[] <<- FALSE
  datasetsModified[] <<- FALSE
  lapply(seq_along(modelIn), function(i){
    switch(modelIn[[i]]$type,
           hot = {
             # set identifier that data was overwritten 
             hotInit[i]        <<- FALSE
             isEmptyInput[i]   <<- TRUE
           },
           slider = {
             if(is.null(modelInWithDep[[names(modelIn)[[i]]]])){
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
               updateSelectInput(session, paste0("dropdown_", i), selected = modelIn[[i]]$dropdown$selected)
             }else{
               showEl(session, "#no_data_dep_" %+% i)
               hideEl(session, "#dropdown_" %+% i)
               updateSelectInput(session, "dropdown_" %+% i, choices = character(0), selected = character(0))
             }
           },
           date = {
             if(is.null(modelInWithDep[[names(modelIn)[[i]]]])){
               updateDateInput(session, "date_" %+% i, value = modelIn[[i]]$date$value)
             }
             previousInputData[[i]] <<- isolate(input[["date_" %+% i]])
           },
           daterange = {
             if(is.null(modelInWithDep[[names(modelIn)[[i]]]])){
               updateDateRangeInput(session, "daterange_" %+% i, 
                                           start = modelIn[[i]]$daterange$start, 
                                           end = modelIn[[i]]$daterange$end)
             }
           },
           checkbox = {
             updateCheckboxInput(session, "cb_" %+% i, value = modelIn[[i]]$checkbox$value)
           },
           textinput = {
             updateTextInput(session, "text_" %+% i, value = modelIn[[i]]$textinput$value)
           }
    )
    rv[["in_" %+% i]]    <<- NULL
  })
  unlink(list.files(workDir, recursive = TRUE))
  if(is.R6(activeScen))
    flog.debug("Scenario: '%s' closed.", activeScen$getScenName())
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

#remove the currently active scenario
closeScenario <- function(){
  # remove output data
  lapply(seq_along(modelOut), function(i){
    scenData[["scen_1_"]][[i]] <<- scenDataTemplate[[i]]
  })
  # clear scalar data
  scalarData[["scen_1_"]] <<- data.frame()
  
  # reset input data sheets
  model.input.data <<- modelInTemplate
  input.initialized[] <<- FALSE
  no.data.changes[]   <<- FALSE
  lapply(seq_along(modelIn), function(i){
    switch(modelIn[[i]]$type,
           hot = {
             # set identifier that data was overwritten 
             hot.init[i]       <<- FALSE
             is.empty.input[i] <<- TRUE
             #if(!is.null(isolate(input[[paste0("in_", i)]]))){
             #  previous.input.data[[i]] <<- rhandsontable::hot_to_r(isolate(input[[paste0("in_", i)]]))
             #}else{
             #  previous.input.data[[i]] <<- list(NULL)
             #}
           },
           slider = {
             if(is.null(modelIn.with.dep[[names(modelIn)[[i]]]])){
               shiny::updateSliderInput(session, paste0("slider_", i), value = modelIn[[i]]$slider$default)
             }else{
               shinyjs::show("no_data_dep_" %+% i)
               shinyjs::hide("slider_" %+% i)
               shiny::updateSliderInput(session, "slider_" %+% i, min = NULL, max = NULL, 
                                  value = NULL, step = 1)
             }
             #previous.input.data[[i]] <<- isolate(input[[paste0("slider_", i)]])
           },
           dropdown = {
             if(is.null(modelIn.with.dep[[names(modelIn)[[i]]]])){
               shiny::updateSelectInput(session, paste0("dropdown_", i), selected = modelIn[[i]]$dropdown$selected)
             }else{
               shinyjs::show("no_data_dep_" %+% i)
               shinyjs::hide("dropdown_" %+% i)
               shiny::updateSelectInput(session, "dropdown_" %+% i, choices = character(0), selected = character(0))
             }
             #previous.input.data[[i]] <<- isolate(input[[paste0("dropdown_", i)]])
           },
           date = {
             if(is.null(modelIn.with.dep[[names(modelIn)[[i]]]])){
               shiny::updateDateInput(session, "date_" %+% i, value = modelIn[[i]]$date$value)
             }
             previous.input.data[[i]] <<- isolate(input[["date_" %+% i]])
           },
           daterange = {
             if(is.null(modelIn.with.dep[[names(modelIn)[[i]]]])){
               shiny::updateDateRangeInput(session, "daterange_" %+% i, 
                                           start = modelIn[[i]]$daterange$start, 
                                           end = modelIn[[i]]$daterange$end)
             }
             #previous.input.data[[i]] <<- isolate(input[[paste0("daterange_", i)]])
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
  active.sname.tmp  <<- NULL
  rv$active.sname   <<- NULL
  no.check[]        <<- FALSE
  markSaved()
  no.output.data    <<- TRUE
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

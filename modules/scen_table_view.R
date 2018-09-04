observeEvent(input[["table_" %+% i]], {
  # get sheet ID for current scenario
  j <- as.integer(strsplit(isolate(input[[paste0("content.scen_", i)]]), "_")[[1]][3])
  flog.debug("%s: Table view in scenario with id: %d for sheet: %d activated.", uid, i, j)
  if(isolate(input$btCompareScen)%%2 != 0){
    # compare scenario mode active
    if(scen.comp.mode < 2L){
      # single view compare mode
      lapply(seq_len(maxNumberScenarios + 1), function(i){
        shinyjs::toggle(paste0("scenTable_", k, "_", j))
        shinyjs::toggle(paste0("scenGraph_", k, "_", j))
      })
    }else{
      # split view comparison mode
      lapply((maxNumberScenarios + 2):(maxNumberScenarios + 3), function(i){
        shinyjs::toggle(paste0("scenTable_", k, "_", j))
        shinyjs::toggle(paste0("scenGraph_", k, "_", j))
      })
    }
  }else{
    shinyjs::toggle(paste0("scenTable_", i, "_", j))
    shinyjs::toggle(paste0("scenGraph_", i, "_", j))
  }
  
  
  # get alias for sheet
  #if(j <= length(modelOut)){
  #  name <- modelOut.alias[[j]]
  #}else{
  #  if(input.ds.names[j - length(modelOut)] == scalars.file.name){
  #    name <- lang$nav$scen$scalarsAlias
  #  }else{
  #    name <- modelIn.alias[[match(input.ds.names[j - length(modelOut)], tolower(names(modelIn)))[1]]]
  #  }
  #}
#
  #graphs.modal <- modalDialog(
  #  title = name,
  #  tags$img(src = "load.gif", class = "loading-input"),
  #  tryCatch({
  #    renderOutput(type = "datatable", data = scenData[[scen.str]][[j]], 
  #                 dt.options = config$datatable, roundPrecision = roundPrecision, static = TRUE)
  #  }, error = function(e) {
  #    removeModal(session = session)
  #    errMsg <- sprintf(lang$errMsg$renderGraph$desc, modelIn.alias[i], conditionMessage(e))
  #    showModal(modalDialog(
  #      title = lang$errMsg$renderGraph$title, HTML(addHtmlLineBreaks(errMsg))
  #    ))
  #  }),
  #  size = "l", fade = TRUE, easyClose = TRUE)
  #showModal(graphs.modal)
})
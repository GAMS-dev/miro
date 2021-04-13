observeEvent(input$outputTableView, {
  # switch between graphical and tabular view in output sheets
  if(!length(modelOut)){
    # no output sheets available
    return(NULL)
  }
  showLoadingScreen(session, 500)
  on.exit(hideLoadingScreen(session))
  i <- as.integer(strsplit(isolate(input$outputTabset), "_")[[1]][2])
  errMsg <- NULL
  
  if(!is.integer(i) || i < 1 || i > length(outputTabTitles)){
    return(NULL)
  }
  if(length(outputTabTitles[[i]]) > 1L){
    j <- as.integer(strsplit(isolate(input[[paste0("outputTabset", i)]]), "_")[[1]][2])
    i <- outputTabs[[i]][j]
  }else{
    i <- outputTabs[[i]]
  }
  if(any(is.na(i))){
    flog.error("Table view button for symbol that doesn't exist clicked. This is most likely an attempt to tamper with the application!")
    return(NULL)
  }
  for(iEl in i){
    toggleEl(session, paste0("#graph-out_", iEl))
    toggleEl(session, paste0("#data-out_", iEl))
    if(modelOutputTableVisible[[iEl]]){
      flog.debug("Table view for model output in sheet(s): %s activated.", paste0(iEl, collapse = ", "))
      modelOutputTableVisible[[iEl]] <<- FALSE
    }else{
      flog.debug("Table view for model output in sheet(s): %s deactivated.", paste0(iEl, collapse = ", "))
      modelOutputTableVisible[[iEl]] <<- TRUE
      if(!dynamicUILoaded[["outputTablesUI"]][iEl]){
        tryCatch({
          insertUI(paste0("#data-out_", iEl),
                   ui = renderDataUI(paste0("table-out_", iEl),
                                     type = "datatable"),
                   immediate = TRUE)
          dynamicUILoaded[["outputTablesUI"]][iEl] <<- TRUE
        }, error = function(e) {
          flog.error(sprintf("Problems generating UI elements for table for output dataset: '%s'. Error message: %s.",
                             modelOutAlias[iEl], conditionMessage(e)))
          errMsg <<- sprintf(lang$errMsg$renderGraph$desc, modelOutAlias[iEl])
        })
        if(is.null(showErrorMsg(lang$errMsg$renderGraph$title, errMsg))){
          return()
        }
      }
      if(!dynamicUILoaded[["outputTables"]][iEl]){
        tryCatch({
          callModule(renderData, paste0("table-out_", iEl),
                     type = "datatable",
                     data = scenData$get("sb", names(modelOut)[iEl], drop = TRUE),
                     dtOptions = configGraphsOut[[iEl]]$datatable,
                     roundPrecision = roundPrecision)
          dynamicUILoaded[["outputTables"]][iEl] <<- TRUE
        }, error = function(e) {
          flog.error(sprintf("Problems rendering table for output dataset: '%s'. Error message: %s.",
                             modelOutAlias[iEl], conditionMessage(e)))
          errMsg <<- sprintf(lang$errMsg$renderGraph$desc, modelOutAlias[iEl])
        })
        if(is.null(showErrorMsg(lang$errMsg$renderGraph$title, errMsg))){
          return()
        }
      }
    }
  }
})

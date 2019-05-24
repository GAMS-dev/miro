observeEvent(input$outputTableView, {
  # switch between graphical and tabular view in output sheets
  if(!length(modelOut)){
    # no output sheets available
    return(NULL)
  }
  i <- as.integer(strsplit(isolate(input$contentCurrent), "_")[[1]][2])
  if(length(outputTabs[[i]]) > 1L){
    j <- as.integer(strsplit(isolate(input[[paste0("contentCurrent", i)]]), "_")[[1]][2])
    i <- outputTabs[[i]][j]
  }else{
    i <- outputTabs[[i]][1]
  }
  flog.debug("Table view for model output in sheet: %d activated.", i)
  toggleEl(session, paste0("#graph-out_", i))
  toggleEl(session, paste0("#data-out_", i))
})
observeEvent(input$outputTableView, {
  # switch between graphical and tabular view in output sheets
  if(!length(modelOut)){
    # no output sheets available
    return(NULL)
  }
  i <- as.integer(strsplit(isolate(input$outputTabset), "_")[[1]][2])
  
  if(!is.integer(i) || i < 1 || i > length(outputTabTitles)){
    return(NULL)
  }
  if(length(outputTabTitles[[i]]) > 1L){
    j <- as.integer(strsplit(isolate(input[[paste0("outputTabset", i)]]), "_")[[1]][2])
    i <- outputTabs[[i]][j]
  }else{
    i <- outputTabs[[i]][1]
  }
  if(is.na(i)){
    flog.error("Table view button for symbol that doesn't exist clicked. This is most likely an attempt to tamper with the application!")
    return(NULL)
  }
  flog.debug("Table view for model output in sheet: %d activated.", i)
  toggleEl(session, paste0("#graph-out_", i))
  toggleEl(session, paste0("#data-out_", i))
})

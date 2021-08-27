observeEvent(input$outputTableView, {
  # switch between graphical and tabular view in output sheets
  if (!length(modelOut)) {
    # no output sheets available
    return(NULL)
  }
  flog.trace("Toggle table view button (sandbox output tables) clicked.")
  showLoadingScreen(session, 500)
  on.exit(hideLoadingScreen(session))
  i <- as.integer(strsplit(isolate(input$outputTabset), "_")[[1]][2])
  errMsg <- NULL

  if (!is.integer(i) || i < 1 || i > length(outputTabTitles)) {
    return(NULL)
  }
  if (length(outputTabTitles[[i]]) > 1L) {
    j <- as.integer(strsplit(isolate(input[[paste0("outputTabset_", i)]]), "_")[[1]][3])
    i <- outputTabs[[i]][j]
  } else {
    i <- outputTabs[[i]]
  }
  if (any(is.na(i))) {
    flog.error("Table view button for symbol that doesn't exist clicked. This is most likely an attempt to tamper with the application!")
    return(NULL)
  }
  for (iEl in i) {
    toggleEl(session, paste0("#scenGraph_1_", iEl))
    toggleEl(session, paste0("#scenTable_1_", iEl))
  }
})

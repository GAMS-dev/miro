observeEvent(input[["table_" %+% i]], {
  # get sheet ID for current scenario
  j <- as.integer(strsplit(isolate(input[["contentScen_" %+% i]]), "_")[[1]][3])
  flog.debug("Table view in scenario with id: %d for sheet: %d activated.", i, j)
  if(isInCompareMode){
    if(isInSplitView){
      lapply(2:3, function(k){
        toggleEl(session, paste0("#scenTable_", k, "_", j))
        toggleEl(session, paste0("#scenGraph_", k, "_", j))
      })
    }else{
      lapply(4:(maxNumberScenarios + 3), function(k){
        toggleEl(session, paste0("#scenTable_", k, "_", j))
        toggleEl(session, paste0("#scenGraph_", k, "_", j))
      })
    }
  }else{
    toggleEl(session, paste0("#scenTable_", i, "_", j))
    toggleEl(session, paste0("#scenGraph_", i, "_", j))
  }
})
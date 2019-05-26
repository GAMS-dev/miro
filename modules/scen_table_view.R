observeEvent(input[["table_" %+% i]], {
  # get sheet ID for current scenario
  j <- strsplit(isolate(input[[paste0("contentScen_", i)]]), 
                "_", fixed = TRUE)[[1L]][[3L]]
  if(isGroupOfSheets[[j]]){
    j <- getTabId(groupId = j,
                  sheetId = strsplit(isolate(input[[paste0("contentScen_", i, "_", j)]]), 
                                     "_", fixed = TRUE)[[1L]][[4L]])
  }else{
    j <- getTabId(groupId = j)
  }
  flog.debug("Table view in scenario with id: %s for sheet: %s activated.", i, j)
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
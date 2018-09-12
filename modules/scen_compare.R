# scenario comparison
observeEvent(input$btCompareScen,{
  if(isolate(input$btCompareScen)%%2 == 0){
    flog.debug("%s: Compare scenario button clicked (stop compare).", uid)
    updateActionButton(session, "btCompareScen", label = lang$nav$sidebarButtons$compareStart)
    if(isInSplitView){
      lapply(2:3, function(i) obs.compare[[i]]$suspend())
    }else{
      lapply(seq_len(maxNumberScenarios + 3), function(i) obs.compare[[i]]$suspend())
    }
  }else{
    flog.debug("%s: Compare scenario button clicked (start compare).", uid)
    updateActionButton(session, "btCompareScen", label = lang$nav$sidebarButtons$compareStop)
    if(isInSplitView){
      lapply(2:3, function(i) obs.compare[[i]]$resume())
    }else{
      lapply(4:(maxNumberScenarios + 3), function(i) obs.compare[[i]]$resume())
    }
  }
})
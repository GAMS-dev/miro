# close scenario tab
observeEvent(input[["btClose_" %+% i]],{
  flog.debug("Close scenario '%d' button clicked.", i)
  showCloseScenDialog(scenId = i)
})
# close scenario tab confirmed
observeEvent(input[["btCloseFinal_" %+% i]],{
  flog.debug("Close scenario '%d' confirmed.", i)
  removeModal()
  removeTab("scenTabset", scenIdLong)
  scenData[[scenIdLong]]      <<- list(NULL)
  scalarData[[scenIdLong]]    <<- list(NULL)
  scenMetaData[[scenIdLong]]  <<- list(NULL)
  sidsInComp[i]               <<- 0
  numberScenTabs              <<- numberScenTabs - 1
  occupiedSidSlots[i - 3]     <<- FALSE
  rv$scenId                   <<- i
  sidCompOrder                <<- sidCompOrder[-which(sidCompOrder == i)]
  if(!numberScenTabs){
    showEl(session, "#no-scen")
  }else if(numberScenTabs == 1){
    disableEl(session, "#btCompareScen")
  }
})
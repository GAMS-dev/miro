# close scenario tab
observeEvent(input[["btClose_" %+% i]],{
  flog.debug("Close scenario '%d' button clicked.", i)
  showCloseScenDialog(scenId = i)
})
# close scenario tab confirmed
observeEvent(input[["btCloseFinal_" %+% i]],{
  flog.debug("Close scenario '%d' confirmed.", i)
  removeModal()
  removeTab("scenTabset", scen.str)
  scenData[[scen.str]]        <<- list(NULL)
  scalarData[[scen.str]]      <<- list(NULL)
  scenMetaData[[scen.str]]    <<- list(NULL)
  sidsInComp[i]               <<- 0
  numberScenTabs              <<- numberScenTabs - 1
  occupiedSidSlots[i - 3]     <<- FALSE
  rv$scenId                   <<- i
  sidCompOrder                <<- sidCompOrder[-which(sidCompOrder == i)]
  if(!numberScenTabs){
    shinyjs::show("noScen")
  }else if(numberScenTabs == 1){
    shinyjs::disable("btCompareScen")
  }
})
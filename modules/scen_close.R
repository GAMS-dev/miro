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
  number.scen.tabs            <<- number.scen.tabs - 1
  occupied.sid.slots[i - 3]   <<- FALSE
  rv$scenId                   <<- i
  sid.comp.order              <<- sid.comp.order[-which(sid.comp.order == i)]
  if(!number.scen.tabs){
    shinyjs::show("noScen")
  }else if(number.scen.tabs == 1){
    shinyjs::disable("btCompareScen")
  }
})
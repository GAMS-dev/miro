# interrupt the paver module (python)

observeEvent(input$btPaverInterrupt,{
  req(paver)
  errMsg <- NULL
  tryCatch({
    paver$kill()
  }, error= function(e){
    flog.error("Problems interrupting the python call. Error message: %s.", e)
    errMsg <<- lang$errMsg$paverTerm$desc
  })
  showErrorMsg(lang$errMsg$paverTerm$title, errMsg)
  disableEl(session, "#btPaverInterrupt")
  hideEl(session, "#paver_load")
  showEl(session, "#newPaverRunButton")
})
observeEvent(input$btNewPaverRun,{
  updateTabsetPanel(session, "sidebarMenuId", selected = "loadResults")
})

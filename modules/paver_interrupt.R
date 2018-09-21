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
  shinyjs::disable("btPaverInterrupt")
  shinyjs::hide("paver_load")
  shinyjs::show("newPaverRunButton")
})
observeEvent(input$btNewPaverRun,{
  updateTabsetPanel(session, "sidebarMenuId", selected = "loadResults")
})

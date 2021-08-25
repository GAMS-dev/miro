observeEvent(input$btInterrupt,{
  errMsg <- NULL

  tryCatch({
    worker$interrupt()
  }, error= function(e){
    flog.error("Problems interrupting the GAMS process. Error message: %s.", conditionMessage(e))
    errMsg <<- lang$errMsg$gamsTerm$desc
  })

  if(is.null(showErrorMsg(lang$errMsg$gamsTerm$title, errMsg))){
    return()
  }
  updateActionButton(session, "btInterrupt", icon = icon("skull"))
})

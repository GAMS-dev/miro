observeEvent(input$btInterrupt,{
  errMsg <- NULL
  tryCatch({
    gams$kill_tree()
  }, error= function(e){
    flog.error("Problems interrupting the gams call. Error message: %s.", e)
    errMsg <<- lang$errMsg$gamsTerm$desc
  })
  if(is.null(showErrorMsg(lang$errMsg$gamsTerm$title, errMsg))){
    return()
  }
  disable("btInterrupt")
})
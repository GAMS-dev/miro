observeEvent(input$btInterrupt,{
  errMsg <- NULL
  tryCatch({
    if(serverOS == 'windows'){
      processx::run(command = 'taskkill', args = c("/F", "/PID", gams$get_pid(), "/T"), windows_hide_window = TRUE)
    }else if (serverOS %in% c('linux', 'osx')){
      processx::run(command = 'kill', args = c("-SIGKILL", -gams$get_pid()))
    }else{
      stop(sprintf("Operating system: '%s' not supported.", serverOS), call. = FALSE)
    }
  }, error= function(e){
    flog.error("Problems interrupting the gams call. Error message: %s.", e)
    errMsg <<- lang$errMsg$gamsTerm$desc
  })
  showErrorMsg(lang$errMsg$gamsTerm$title, errMsg)
  shinyjs::disable("btInterrupt")
})
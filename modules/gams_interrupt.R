observeEvent(input$btInterrupt,{
  errMsg <- NULL
  tryCatch({
    gams$kill_tree()
  }, error= function(e){
    errMsg <<- "error"
  })
  if(!is.null(errMsg)){
    errMsg <- NULL
    tryCatch({
      if(serverOS == 'windows'){
        run(command = 'taskkill', args = c("/F", "/PID", gams$get_pid(), "/T"), windows_hide_window = TRUE)
      }else if (serverOS %in% c('linux', 'osx')){
        run(command = 'kill', args = c("-SIGKILL", -gams$get_pid()))
      }else{
        stop(sprintf("Operating system: '%s' not supported.", serverOS), call. = FALSE)
      }
    },error = function(e){
      flog.error("Problems interrupting the gams call. Error message: %s.", e)
      errMsg <<- lang$errMsg$gamsTerm$desc
    })
  }

  if(is.null(showErrorMsg(lang$errMsg$gamsTerm$title, errMsg))){
    return()
  }
  disable("btInterrupt")
})
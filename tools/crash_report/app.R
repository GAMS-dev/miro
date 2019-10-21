# module to send crash reports (will be invoked in case MIRO crashes)
source(file.path("..", "..", "global.R"))

uploadFile <- function(file, url, userpwd){
  stopifnot(file.exists(file))
  stopifnot(is.character(url), length(url) == 1L)
  con <- file(file, open = "rb")
  on.exit(close(con))
  h <- curl::new_handle(upload = TRUE, filetime = FALSE, httpauth = 1, userpwd = userpwd)
  curl::handle_setopt(h, readfunction = function(n) {
    readBin(con, raw(), n = n)
  })
  curl::curl_fetch_memory(url, handle = h)
}

modelPath <- Sys.getenv("GMSMODELNAME")
modelName <- basename(modelPath)
modelPath <- dirname(modelPath)

ui_crashrep <- fluidPage(
  titlePanel("MIRO terminated unexpectedly"),
  fluidRow(align="center",
           HTML("<br>"),
           div(
             paste0("MIRO discovered that it was terminated unexpectedly. We are constantly striving to improve MIRO.
                            Would you like to send the error report to GAMS in order to avoid such crashes in the future?
                            The ONLY files that we send (encrypted via HTTPS) are the configuration files: '", modelName, 
                    "_io.json' and '", modelName, ".json' as well as the error log. 
                            None of your .gms model files will be sent!")),
           HTML("<br>"),
           actionButton("crash_dontsend", "Don't send"),
           actionButton("crash_send", "Send")
  )
)
server_crashrep <- function(input, output, session){
  if(curl::has_internet() && 
     file.exists(file.path(modelPath, ".crash.zip"))){
    observeEvent(input$crash_dontsend, {
      on.exit(stopApp())
      unlink(file.path(modelPath, ".crash.zip"))
      removeModal()
    })
    observeEvent(input$crash_send, {
      on.exit(unlink(file.path(modelPath, ".crash.zip")))
      on.exit(stopApp(), add = TRUE)
      try(
        uploadFile(
          file = file.path(modelPath, ".crash.zip"), 
          url = paste0(bugReportUrl$url, format(Sys.time(), "%y.%m.%d_%H.%M.%S"), 
                       "_", substr(modelName, 1L, 3L), ".zip"), 
          userpwd = paste0(bugReportUrl$dir, ":")
        ))
      
      removeModal()
    })
  }else{
    try(unlink(file.path(modelPath, ".crash.zip")), silent = TRUE)
    cat("Could not generate error report")
    stopApp()
  }
  session$onSessionEnded(function() {
    # remove temporary files and folders
    stopApp()
    q("no")
  })
}

shinyApp(ui = ui_crashrep, server = server_crashrep)

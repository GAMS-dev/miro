observeEvent(input$btOpenWhatsNewDialog, {
  whatsNewFileName <- file.path("whats-new", paste0(paste(strsplit(MIROVersion, ".", fixed = TRUE)[[1]][1:2], collapse = "."), ".json"))
  whatsNewData <- tryCatch(suppressWarnings(read_json(whatsNewFileName,
    simplifyDataFrame = FALSE,
    simplifyMatrix = FALSE
  )), error = function(err) {
    flog.error("Could not release whats-new file: %s. Error message: %s", whatsNewFileName, conditionMessage(err))
    showNotification(lang$errMsg$unknownError, duration = 10L, type = "error")
    return(NULL)
  })
  if (is.null(whatsNewData)) {
    return()
  }
  showWhatsNewDialog(whatsNewData)
})

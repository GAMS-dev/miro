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
if (config$activateModules$remoteExecution) {
  observeEvent(input$btShowSettingsDialog, {
    flog.debug("Button to show Engine Settings dialog clicked")
    showSettingsDialog()
    engineClient$populateInstanceSelector(session, "selEngineDefaultInstance", lang$nav$dialogJobSubmission$instanceDropdownCategories)
    engineClient$populateVolumeInfoCard(session, "engineQuotaInfo")
  })
  observeEvent(input$btUpdateEngineDefaultInstance, {
    flog.debug("Button to change default instance clicked.")
    tryCatch(
      {
        showEl(session, "#selEngineDefaultInstanceSpinner")
        hideEl(session, "#selEngineDefaultInstanceWrapper")
        engineClient$updateDefaultInstance(input$selEngineDefaultInstance)
        flog.debug("Engine default instance updated to: %s.", )
      },
      error = function(err) {
        flog.error("Problems updating user's default instance. Error message: %s", conditionMessage(err))
        showEl(session, "#settingsDialogUnknownError")
      },
      finally = {
        hideEl(session, "#selEngineDefaultInstanceSpinner")
        showEl(session, "#selEngineDefaultInstanceWrapper")
      }
    )
  })
}

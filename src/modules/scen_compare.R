# scenario comparison
observeEvent(
  input$btCompareScen,
  rv$btCompareScen <- isolate(rv$btCompareScen + 1L)
)
observeEvent(virtualActionButton(rv$btCompareScen), {
  if (isInCompareMode) {
    flog.debug("%s: Compare scenario button clicked (comparison mode stopped).", uid)
    isInCompareMode <<- FALSE
    updateActionButton(session, "btCompareScen", label = lang$nav$sidebarButtons$compareStart)
    if (identical(currentCompMode, "split")) {
      lapply(2:3, function(i) obsCompare[[i]]$suspend())
    } else {
      lapply(seq_len(maxNumberScenarios + 3), function(i) obsCompare[[i]]$suspend())
    }
  } else {
    flog.debug("%s: Compare scenario button clicked (comparison mode started).", uid)
    isInCompareMode <<- TRUE
    updateActionButton(session, "btCompareScen", label = lang$nav$sidebarButtons$compareStop)
    if (identical(currentCompMode, "split")) {
      lapply(2:3, function(i) obsCompare[[i]]$resume())
    } else {
      lapply(4:(maxNumberScenarios + 3), function(i) obsCompare[[i]]$resume())
    }
  }
})

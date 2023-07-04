closeScenario <- function(clearMeta = TRUE) {
  # remove output data
  errMsg <- NULL
  traceData <<- data.frame()
  # reset input data sheets
  modelInputData <<- modelInTemplate
  tableContent <<- vector(mode = "list", length = length(modelIn))
  sandboxInputData$reset()

  if (is.R6(activeScen)) {
    flog.debug("Scenario: '%s' closed.", activeScen$getScenName())
  }
  # reset input data
  modelInputGraphVisible[] <<- FALSE
  lapply(seq_along(modelIn), function(i) {
    hideEl(session, "#graph-in_" %+% i)
    showEl(session, "#data-in_" %+% i)
  })
  hideEl(session, "#btRefreshGraphIn")

  # reset model output data
  if (length(activeScen)) {
    if (clearMeta) {
      scenData$clearSandbox()
      views$clearConf()
      attachments$clear(cleanLocal = TRUE)
    }
    activeScen$finalize()
  }
  renderOutputData()
  activeScen <<- Scenario$new(
    db = db, sname = lang$nav$dialogNewScen$newScenName,
    isNewScen = TRUE, views = views, attachments = attachments,
    rv = rv
  )
  scenTags <<- NULL
  attachmentList <<- tibble(
    name = vector("character", attachMaxNo),
    execPerm = vector("logical", attachMaxNo)
  )
  if (length(config$scripts$base)) {
    scriptOutput$clearContent()
  }
  renderOutputData()

  markSaved()
  clearLogs(session)
  inconsistentOutput <<- TRUE
  noOutputData <<- TRUE
  if (!is.null(errMsg)) {
    invisible(FALSE)
  } else {
    invisible(TRUE)
  }
}

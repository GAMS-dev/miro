# remove the currently active scenario
observeEvent(input$btDelete, {
  req(activeScen, activeScen$getSid())
  flog.debug("Button to delete scenario from database clicked.")
  showDeleteScenDialog()
})
observeEvent(input$btDeleteConfirm, {
  flog.debug("Button to confirm deleting scenario from database clicked.")
  if (is.null(activeScen) || !length(activeScen$getSid())) {
    flog.error("No active scenario ID found to delete.")
    return()
  }
  if (activeScen$isReadonlyOrLocked) {
    flog.info("Scenario can't be removed as it is readonly or locked.")
    return(showErrorMsg(lang$nav$dialogReadonly$title, lang$nav$dialogReadonly$descErr))
  }
  errMsg <- NULL
  tryCatch(
    {
      activeScen$delete()
      hideEl(session, "#deleteScen_db")
      hideEl(session, "#btDeleteConfirm")
      showEl(session, "#deleteScen_ui")
      showEl(session, "#btRemoveDeletedConfirm")
    },
    error = function(e) {
      flog.error(
        "Problems deleting scenario: '%s'. Error message: '%s'.",
        activeScen$getScenName(), conditionMessage(e)
      )
      errMsg <<- lang$errMsg$deleteScen$desc
    }
  )
  if (!is.null(errMsg)) {
    showErrorMsg(lang$errMsg$deleteScen$title, errMsg)
    return()
  }
  # we need to force the scenario name to update as scenario is
  # no longer stored in database
  rv$sandboxUnsaved <- FALSE
  rv$sandboxUnsaved <- TRUE
})

# button changes from NULL to 0 when initialised (modalDialog opens)
# so code needs to be duplicated here
observeEvent(input$btRemoveDeletedConfirm, {
  flog.debug("Remove scenario data from UI confirmed.")
  closeScenario()
  removeModal()
})
observeEvent(input$btRemoveConfirm, {
  flog.debug("Remove scenario data from UI confirmed.")
  closeScenario()
  removeModal()
})

# remove the currently active scenario
closeScenario <- function(clearMeta = TRUE) {
  # remove output data
  errMsg <- NULL
  traceData <<- data.frame()
  # reset input data sheets
  modelInputData <<- modelInTemplate
  tableContent <<- vector(mode = "list", length = length(modelIn))
  inputInitialized[] <<- FALSE
  noCheck[] <<- FALSE
  if (resetWidgetsOnClose) {
    widgetModifiedSkipCount[] <<- 1L
    lapply(seq_along(modelIn), function(i) {
      switch(modelIn[[i]]$type,
        hot = {
          # set identifier that data was overwritten
          hotInit[i] <<- FALSE
          isEmptyInput[i] <<- TRUE
        },
        slider = {
          if (is.null(modelInWithDep[[names(modelIn)[[i]]]])) {
            if (length(modelIn[[i]]$slider$default)) {
              updateSliderInput(session, paste0("slider_", i), value = modelIn[[i]]$slider$default)
            }
          } else {
            showEl(session, "#no_data_dep_" %+% i)
            hideEl(session, "#slider_" %+% i)
            updateSliderInput(session, "slider_" %+% i,
              min = 0, max = 1,
              value = 0, step = 1
            )
          }
          noCheck[i] <<- TRUE
        },
        dropdown = {
          if (is.null(modelInWithDep[[names(modelIn)[[i]]]])) {
            if (length(modelIn[[i]]$dropdown$selected)) {
              updateSelectInput(session, paste0("dropdown_", i),
                selected = modelIn[[i]]$dropdown$selected
              )
            }
          } else {
            selectedDepEl[[i]] <<- character(0)
            showEl(session, "#no_data_dep_" %+% i)
            hideEl(session, "#dropdown_" %+% i)
            updateSelectInput(session, "dropdown_" %+% i, choices = "_", selected = "_")
          }
          noCheck[i] <<- TRUE
        },
        date = {
          if (is.null(modelInWithDep[[names(modelIn)[[i]]]]) && length(modelIn[[i]]$date$value)) {
            updateDateInput(session, "date_" %+% i, value = modelIn[[i]]$date$value)
            noCheck[i] <<- TRUE
          }
        },
        daterange = {
          if (is.null(modelInWithDep[[names(modelIn)[[i]]]]) && length(modelIn[[i]]$daterange$start) &&
            length(modelIn[[i]]$daterange$end)) {
            updateDateRangeInput(session, "daterange_" %+% i,
              start = modelIn[[i]]$daterange$start,
              end = modelIn[[i]]$daterange$end
            )
            noCheck[i] <<- TRUE
          }
        },
        checkbox = {
          if (length(modelIn[[i]]$checkbox$value)) {
            updateCheckboxInput(session, "cb_" %+% i, value = modelIn[[i]]$checkbox$value)
            noCheck[i] <<- TRUE
          }
        },
        textinput = {
          if (length(modelIn[[i]]$textinput$value)) {
            updateTextInput(session, "text_" %+% i, value = modelIn[[i]]$textinput$value)
            noCheck[i] <<- TRUE
          }
        },
        numericinput = {
          if (length(modelIn[[i]]$numericinput$value)) {
            updateNumericInput(session, "numeric_" %+% i, value = modelIn[[i]]$numericinput$value)
            noCheck[i] <<- TRUE
          }
        }
      )
      # make sure data is cleaned even when modified manually
      # (and thus rv$in_i is NULL)
      if (is.null(isolate(rv[["in_" %+% i]]))) {
        rv[["in_" %+% i]] <- 1L
      }
      rv[["in_" %+% i]] <- NULL
    })
  }
  resetWidgetsOnClose <<- TRUE

  if (is.R6(activeScen)) {
    flog.debug("Scenario: '%s' closed.", activeScen$getScenName())
  }
  # reset input data
  modelInputGraphVisible[] <<- FALSE
  lapply(seq_along(modelIn), function(i) {
    hideEl(session, "#graph-in_" %+% i)
    showEl(session, "#data-in_" %+% i)
  })

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
    isNewScen = TRUE, views = views, attachments = attachments
  )
  rv$activeSname <<- NULL
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
  markUnsaved()
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

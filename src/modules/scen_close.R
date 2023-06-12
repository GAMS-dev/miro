closeScenario <- function(clearMeta = TRUE, resetWidgets = TRUE) {
  # remove output data
  errMsg <- NULL
  traceData <<- data.frame()
  # reset input data sheets
  modelInputData <<- modelInTemplate
  tableContent <<- vector(mode = "list", length = length(modelIn))
  if (resetWidgets) {
    sandboxInputData$reset()
    # widgetModifiedSkipCount[] <<- 1L
    # lapply(seq_along(modelIn), function(i) {
    #   switch(modelIn[[i]]$type,
    #     hot = {
    #       # set identifier that data was overwritten
    #       hotInit[i] <<- FALSE
    #     },
    #     slider = {
    #       if (is.null(modelInWithDep[[names(modelIn)[[i]]]])) {
    #         if (length(modelIn[[i]]$slider$default)) {
    #           updateSliderInput(session, paste0("slider_", i), value = modelIn[[i]]$slider$default)
    #         }
    #       } else {
    #         showEl(session, "#no_data_dep_" %+% i)
    #         hideEl(session, "#slider_" %+% i)
    #         updateSliderInput(session, "slider_" %+% i,
    #           min = 0, max = 1,
    #           value = 0, step = 1
    #         )
    #       }
    #     },
    #     dropdown = {
    #       if (is.null(modelInWithDep[[names(modelIn)[[i]]]])) {
    #         if (length(modelIn[[i]]$dropdown$selected)) {
    #           updateSelectInput(session, paste0("dropdown_", i),
    #             selected = modelIn[[i]]$dropdown$selected
    #           )
    #         }
    #       } else {
    #         selectedDepEl[[i]] <<- character(0)
    #         showEl(session, "#no_data_dep_" %+% i)
    #         hideEl(session, "#dropdown_" %+% i)
    #         updateSelectInput(session, "dropdown_" %+% i, choices = "_", selected = "_")
    #       }
    #     },
    #     date = {
    #       if (is.null(modelInWithDep[[names(modelIn)[[i]]]]) && length(modelIn[[i]]$date$value)) {
    #         updateDateInput(session, "date_" %+% i, value = modelIn[[i]]$date$value)
    #       }
    #     },
    #     daterange = {
    #       if (is.null(modelInWithDep[[names(modelIn)[[i]]]]) && length(modelIn[[i]]$daterange$start) &&
    #         length(modelIn[[i]]$daterange$end)) {
    #         updateDateRangeInput(session, "daterange_" %+% i,
    #           start = modelIn[[i]]$daterange$start,
    #           end = modelIn[[i]]$daterange$end
    #         )
    #       }
    #     },
    #     checkbox = {
    #       if (length(modelIn[[i]]$checkbox$value)) {
    #         updateCheckboxInput(session, "cb_" %+% i, value = modelIn[[i]]$checkbox$value)
    #       }
    #     },
    #     textinput = {
    #       if (length(modelIn[[i]]$textinput$value)) {
    #         updateTextInput(session, "text_" %+% i, value = modelIn[[i]]$textinput$value)
    #       }
    #     },
    #     numericinput = {
    #       if (length(modelIn[[i]]$numericinput$value)) {
    #         updateNumericInput(session, "numeric_" %+% i, value = modelIn[[i]]$numericinput$value)
    #       }
    #     }
    #   )
    #   # make sure data is cleaned even when modified manually
    #   # (and thus rv$in_i is NULL)
    #   if (is.null(isolate(rv[["in_" %+% i]]))) {
    #     rv[["in_" %+% i]] <- 1L
    #   }
    #   rv[["in_" %+% i]] <- NULL
    # })
  }

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

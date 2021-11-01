# render input data for input sheets with forward dependency on other input sheets
getScalarValue <- function(data, operator) {
  stopifnot(length(operator) >= 0L)
  if (!length(data)) {
    return(NULL)
  }
  errMsg <- NULL
  scalarVal <- NULL
  switch(operator,
    count = {
      scalarVal <- length(data)
    },
    max = {
      try(scalarVal <- max(as.numeric(data)))
    },
    min = {
      try(scalarVal <- min(as.numeric(data)))
    },
    mean = {
      try(scalarVal <- mean(as.numeric(data)))
    },
    median = {
      try(scalarVal <- median(as.numeric(data)))
    },
    var = {
      try(scalarVal <- var(as.numeric(data)))
    },
    sd = {
      try(scalarVal <- sd(as.numeric(data)))
    },
    {
      flog.error("Unknown operator: '%s'.", operator)
      stop()
    }
  )
  if (!is.numeric(scalarVal) || is.na(scalarVal)) {
    stop("Input data is not numeric.", call. = FALSE)
  } else {
    return(scalarVal)
  }
}
isValidDropdownVal <- function(value) {
  if (is.null(value) || all(is.na(value)) ||
    identical(value, "")) {
    return(FALSE)
  }
  return(TRUE)
}
getData <- vector(mode = "list", length = length(modelInWithDep))
getSelected <- vector(mode = "list", length = length(modelIn))
inputInitialized <- vector(mode = "logical", length = length(modelInWithDep))
newDefaultValue <- vector(mode = "list", length = length(modelInWithDep))

lapply(seq_along(modelIn), function(id) {
  if (length(modelIn[[id]]$definedByExternalSymbol)) {
    dataModelIn[[id]] <<- reactive({
      if (is.null(rv[["in_" %+% id]])) {
        return(NULL)
      }
      modelInputData[[id]]
    })
    return()
  }
  i <- match(names(modelIn)[[id]], names(modelInWithDep))[1]
  if (!is.na(i)) {
    name <- names(modelInWithDep)[[i]]
  }
  switch(modelIn[[id]]$type,
    checkbox = {
      dataModelIn[[id]] <<- reactive({
        input[[paste0("cb_", id)]]
      })
      if (is.na(i)) {
        # no dependency
        getSelected[[id]] <<- reactive({
          if (is.null(rv[["in_" %+% id]])) {
            return(NULL)
          }
          if (!length(modelInputData[[id]][[1]])) {
            return(isolate(input[["cb_" %+% id]]))
          } else {
            if (identical(as.integer(modelInputData[[id]]), 1L)) {
              value <- TRUE
            } else {
              value <- FALSE
            }
            modelInputData[[id]] <<- list(NULL)
            return(value)
          }
        })
        observe({
          value <- getSelected[[id]]()
          if (!is.null(value) && !identical(
            suppressWarnings(as.logical(value)),
            isolate(input[[paste0("cb_", id)]])
          )) {
            noCheck[id] <<- TRUE
            updateCheckboxInput(session, paste0("cb_", id), value = value)
          }
        })
      } else {
        # has dependency
        observe({
          k <- modelIn[[id]]$checkbox$sheetId
          htmlSelectorPefix <- "in_"
          if (isMobileDevice && identical(modelIn[[k]]$type, "hot")) {
            htmlSelectorPefix <- "in_m_"
          }
          value <- NULL
          errMsg <- NULL
          noShared <- FALSE
          rv[["in_" %+% k]]
          input[[htmlSelectorPefix %+% k]]
          rv[["in_" %+% id]]

          tryCatch(
            {
              symIdToFetch <- k
              if (length(modelIn[[k]]$definedByExternalSymbol)) {
                symIdTmp <- match(modelIn[[i]]$definedByExternalSymbol, names(modelIn))
                symIdToFetch <- symIdTmp
              }
              if (identical(modelIn[[k]]$type, "custom")) {
                if (length(modelIn[[k]]$widgetSymbols)) {
                  force(modelInputDataVisible[[symIdToFetch]][[names(modelIn)[k]]]())
                } else {
                  force(modelInputDataVisible[[k]]())
                }
              }
              value <- getInputDataset(symIdToFetch, visible = TRUE, subSymName = names(modelIn)[[k]])[[modelIn[[id]]$checkbox$max]]
            },
            error = function(e) {
              flog.error(
                "Some problem occurred attempting to fetch values for checkbox: '%s' " %+%
                  "(forward dependency on dataset: '%s'). Error message: %s.",
                modelInAlias[id], modelInAlias[k], conditionMessage(e)
              )
              errMsg <<- paste(errMsg, lang$errMsg$dataError$desc, sep = "\n")
            }
          )
          if (is.null(showErrorMsg(lang$errMsg$dataError$title, errMsg))) {
            return()
          }
          tryCatch(
            {
              value <- getScalarValue(unlist(value, use.names = FALSE), modelIn[[id]]$checkbox$operator)
            },
            error = function(e) {
              flog.warn(
                "Input type for checkbox: '%s' is not numeric. (Operator: '%s')",
                name, modelIn[[id]]$checkbox$operator
              )
              errMsg <<- paste(errMsg, lang$errMsg$dataError$desc, sep = "\n")
            }
          )
          if (is.null(showErrorMsg(lang$errMsg$dataError$title, errMsg))) {
            return()
          }
          if (!inputInitialized[i]) {
            if (is.numeric(value) && !identical(value, -Inf)) {
              inputInitialized[i] <<- TRUE
              showEl(session, "#cbDiv_" %+% id)
              hideEl(session, "#no_data_dep_" %+% id)
            } else {
              return()
            }
          }

          if (length(modelInputData[[id]][[1]])) {
            selected <- suppressWarnings(as.integer(modelInputData[[id]]))
            if (value < selected) {
              flog.warn("A checkbox value of 1 was fetched from database, but maximum allowed value is 0. Selected value was set to 0.")
              selected <- 0L
            }
            noShared <- TRUE
            if (is.na(value)) {
              flog.warn("Bad input value for checkbox returned from database.")
              return()
            }
          } else {
            selected <- isolate(input[["cb_" %+% id]])
          }
          if (!is.null(selected) && !identical(
            suppressWarnings(as.logical(selected)),
            isolate(input[["cb_" %+% id]])
          )) {
            noCheck[id] <<- TRUE
            updateCheckboxInput(session, "cb_" %+% id, value = selected)

            if (value <= 0.5) {
              disableEl(session, "#cb_" %+% id)
            } else {
              enableEl(session, "#cb_" %+% id)
            }
          }
        })
      }
    },
    textinput = {
      dataModelIn[[id]] <<- reactive({
        input[[paste0("text_", id)]]
      })
      getSelected[[id]] <<- reactive({
        if (is.null(rv[["in_" %+% id]])) {
          return(NULL)
        }
        if (!length(modelInputData[[id]][[1]])) {
          return(isolate(input[["text_" %+% id]]))
        } else {
          value <- modelInputData[[id]]
          modelInputData[[id]] <<- list(NULL)
          return(value)
        }
      })
      observe({
        value <- getSelected[[id]]()
        if (!is.null(value) && !identical(value, isolate(input[["text_" %+% id]]))) {
          noCheck[id] <<- TRUE
          updateTextInput(session, "text_" %+% id, value = value)
        }
      })
    },
    numericinput = {
      dataModelIn[[id]] <<- reactive({
        input[[paste0("numeric_", id)]]
      })
      getSelected[[id]] <<- reactive({
        if (is.null(rv[["in_" %+% id]])) {
          return(NULL)
        }
        if (!length(modelInputData[[id]][[1]])) {
          return(isolate(input[["numeric_" %+% id]]))
        } else {
          value <- modelInputData[[id]]
          modelInputData[[id]] <<- list(NULL)
          return(value)
        }
      })
      observe({
        value <- as.numeric(getSelected[[id]]())
        if (length(value) && !identical(value, isolate(input[["numeric_" %+% id]]))) {
          noCheck[id] <<- TRUE
          updateNumericInput(session, "numeric_" %+% id, value = value)
        }
      })
    },
    date = {
      dataModelIn[[id]] <<- reactive({
        input[[paste0("date_", id)]]
      })
      getSelected[[id]] <<- reactive({
        if (is.null(rv[["in_" %+% id]])) {
          return(NULL)
        }
        if (!length(modelInputData[[id]][[1]])) {
          return(isolate(input[["date_" %+% id]]))
        } else {
          value <- modelInputData[[id]]
          modelInputData[[id]] <<- list(NULL)
          return(value)
        }
      })
      # TODO: support dependency
      observe({
        value <- getSelected[[id]]()
        if (!is.null(value) && !identical(value, isolate(input[["date_" %+% id]]))) {
          noCheck[id] <<- TRUE
          updateDateInput(session, "date_" %+% id, value = value)
        }
      })
    },
    daterange = {
      dataModelIn[[id]] <<- reactive({
        input[[paste0("daterange_", id)]]
      })
      getSelected[[id]] <<- reactive({
        if (is.null(rv[["in_" %+% id]])) {
          return(NULL)
        }
        if (!length(modelInputData[[id]])) {
          return(isolate(input[["daterange_" %+% id]]))
        } else {
          value <- modelInputData[[id]]
          modelInputData[[id]] <<- list(NULL)
          return(value)
        }
      })
      # TODO: support dependency

      observe({
        value <- getSelected[[id]]()
        if (!is.null(value) && !identical(value, isolate(input[["daterange_" %+% id]]))) {
          noCheck[id] <<- TRUE
          updateDateRangeInput(session, "daterange_" %+% id,
            start = value[[1]],
            end = value[[2]]
          )
        }
      })
    },
    dropdown = {
      dataModelIn[[id]] <<- reactive({
        input[[paste0("dropdown_", id)]]
      })
      # retrieve selected value for dropdown menu
      getSelected[[id]] <<- reactive({
        if (is.null(rv[["in_" %+% id]])) {
          return(NULL)
        }
        if (!length(modelInputData[[id]][[1]])) {
          return(isolate(input[["dropdown_" %+% id]]))
        } else {
          value <- modelInputData[[id]][[1]]
          modelInputData[[id]] <<- list(NULL)
          return(value)
        }
      })
      if (is.na(i)) {
        # does not have any dependencies on other datasets

        # observe changes of dropdown menu data
        observe({
          value <- getSelected[[id]]()
          if (isValidDropdownVal(value) &&
            !identical(value, isolate(input[[paste0("dropdown_", id)]]))) {
            noCheck[id] <<- TRUE
            updateSelectInput(session, "dropdown_" %+% id, selected = value)
          }
        })
      } else {
        # has dependencies on other datasets

        # retrieve choices for dropdown menu
        getData[[i]] <<- reactive({
          choices <- vector(mode = "list", length = length(ddownDep[[name]]$fw) + 1)
          aliases <- vector(mode = "list", length = length(ddownDep[[name]]$aliases) + 1)
          # retrieve single value data
          if (!is.null(choicesNoDep[[name]])) {
            choices[[1]] <- choicesNoDep[[name]]
          }
          if (!is.null(aliasesNoDep[[name]])) {
            aliases[[1]] <- aliasesNoDep[[name]]
          }

          if (length(ddownDep[[name]]$fw)) {
            errMsg <- NULL
            # reset counter
            j <- 2
            for (dataSheet in unique(tolower(names(ddownDep[[name]]$fw)))) {
              k <- match(dataSheet, names(modelIn))
              htmlSelectorPefix <- "in_"
              if (isMobileDevice && identical(modelIn[[k]]$type, "hot")) {
                htmlSelectorPefix <- "in_m_"
              }
              input[[htmlSelectorPefix %+% k]]
              rv[["in_" %+% k]]
              symIdToFetch <- k
              if (length(modelIn[[k]]$definedByExternalSymbol)) {
                symIdTmp <- match(modelIn[[i]]$definedByExternalSymbol, names(modelIn))
                symIdToFetch <- symIdTmp
              }
              if (identical(modelIn[[k]]$type, "custom")) {
                if (length(modelIn[[k]]$widgetSymbols)) {
                  force(modelInputDataVisible[[symIdToFetch]][[names(modelIn)[k]]]())
                } else {
                  force(modelInputDataVisible[[k]]())
                }
              } else if (identical(modelIn[[k]]$type, "dt") ||
                (isMobileDevice && identical(modelIn[[k]]$type, "hot"))) {
                force(rv[[paste0("wasModified_", k)]])
              }
              tryCatch(
                {
                  dataTmp <- getInputDataset(symIdToFetch, visible = TRUE, subSymName = names(modelIn)[[k]])
                },
                error = function(e) {
                  flog.error(
                    "Some problem occurred attempting to fetch values for dropdown menu: '%s' " %+%
                      "(forward dependency on dataset: '%s'). Error message: %s.",
                    modelInAlias[id], modelInAlias[k], conditionMessage(e)
                  )
                  errMsg <<- paste(errMsg, lang$errMsg$dataError$desc, sep = "\n")
                }
              )
              if (!is.null(errMsg)) {
                next
              }
              choices[[j]] <- dataTmp[[ddownDep[[name]]$fw[[dataSheet]][[1]]]]
              if (!length(choices[[j]]) || identical(choices[[j]][[1]], "")) {
                return(NULL)
              }
              if (!is.null(ddownDep[[name]]$aliases[[dataSheet]])) {
                aliases[[j]] <- dataTmp[[ddownDep[[name]]$aliases[[dataSheet]][[1]]]]
              }
              j <- j + 1
            }
            showErrorMsg(lang$errMsg$dataError$title, errMsg)
          }
          aliases <- unique(unlist(aliases, use.names = FALSE))
          choices <- unique(unlist(choices, use.names = FALSE))
          if (length(aliases)) {
            if (length(aliases) == length(choices)) {
              names(choices) <- aliases
              return(sort(choices))
            } else {
              # length of aliases and choices does not match
              flog.error(lang$errMsg$ddLenMismatch$desc, modelInAlias[k])
              errMsg <- sprintf(lang$errMsg$ddLenMismatch$desc, modelInAlias[k])
              showErrorMsg(lang$errMsg$ddLenMismatch$title, errMsg)
            }
          } else {
            return(sort(choices))
          }
        })

        # observe changes of dropdown menu data
        observe({
          # update choices
          if (!inputInitialized[i]) {
            choices <- getData[[i]]()
            if (length(choices)) {
              selectedEl <- modelIn[[id]]$dropdown$selected[[1]]
              if ((!length(selectedEl) && (!isTRUE(modelIn[[id]]$dropdown$multiple) ||
                isTRUE(modelIn[[id]]$dropdown$single))) ||
                (length(selectedEl) && !selectedEl %in% choices)) {
                selectedEl <- choices[[1]]
              }
              if (!identical(selectedEl, isolate(input[["dropdown_" %+% id]]))) {
                noCheck[id] <<- TRUE
              }
              if (is.null(selectedEl)) {
                selectedEl <- character(0L)
              }
              selectedDepEl[[id]] <<- selectedEl
              updateSelectInput(session, paste0("dropdown_", id),
                choices = choices,
                selected = selectedEl
              )
              inputInitialized[i] <<- TRUE
              showEl(session, paste0("#dropdown_", id))
              hideEl(session, paste0("#no_data_dep_", id))
              # refresh selected item in case it was uploaded (e.g. via Excel or database)
              if (length(isolate(rv[[paste0("in_", id)]]))) {
                rv[[paste0("in_", id)]] <<- isolate(rv[[paste0("in_", id)]]) + 1
              } else if (length(modelInputData[[id]][[1]])) {
                noCheck[[id]] <<- TRUE
                rv[[paste0("in_", id)]] <<- 1
              }
            }
          } else {
            selectedEl <- isolate(input[[paste0("dropdown_", id)]])
            if (is.null(selectedEl)) {
              selectedEl <- character(0L)
            }
            selectedDepEl[[id]] <<- selectedEl
            updateSelectInput(session, paste0("dropdown_", id),
              choices = getData[[i]](),
              selected = selectedDepEl[[id]]
            )
          }
        })
        # observe changes of dropdown default value
        observe(
          {
            value <- getSelected[[id]]()
            if (isValidDropdownVal(value) && !identical(value, selectedDepEl[[id]])) {
              noCheck[id] <<- TRUE
              updateSelectInput(session, paste0("dropdown_", id), selected = value)
            }
          },
          priority = -1
        )
      }
    },
    slider = {
      dataModelIn[[id]] <<- reactive({
        input[[paste0("slider_", id)]]
      })
      # retrieve selected value for slider
      getSelected[[id]] <<- reactive({
        if (is.null(rv[[paste0("in_", id)]])) {
          return(NULL)
        }
        if (is.numeric(modelInputData[[id]]) && length(modelInputData[[id]]) > 0L) {
          value <- sort(modelInputData[[id]])
          modelInputData[[id]] <<- list(NULL, value, TRUE)
        } else if (!length(modelInputData[[id]][[1]])) {
          value <- isolate(input[[paste0("slider_", id)]])
          modelInputData[[id]] <<- list(NULL, value, FALSE)
        } else {
          value <- suppressWarnings(as.numeric(modelInputData[[id]][[1]]))
          if (any(is.na(value))) {
            return(NULL)
          }
          value <- sort(value)
          modelInputData[[id]] <<- list(NULL, value, TRUE)
        }
        return(value)
      })
      if (is.na(i)) {
        # does not have any dependencies on other datasets
        # observe changes of slider data
        observe({
          value <- getSelected[[id]]()
          if (!is.null(value) && !identical(value, as.numeric(isolate(input[["slider_" %+% id]])))) {
            noCheck[id] <<- TRUE
            updateSliderInput(session, paste0("slider_", id), value = value)
          }
        })
      } else {
        # has dependencies on other datasets

        # retrieve choices for slider
        getData[[i]] <<- reactive({
          errMsg <- NULL
          dataTmp <- NULL
          sliderData <- lapply(seq_along(sliderValues[[name]]), function(valId) {
            el <- sliderValues[[name]][[valId]]
            # return numeric data (no external dependency)
            if (is.numeric(el)) {
              return(el)
            } else if (names(sliderValues[[name]])[valId] %in% c("def", "def1", "def2") &&
              is.list(modelInputData[[id]]) && length(modelInputData[[id]]) == 3L) {
              if (modelInputData[[id]][[3L]]) {
                selected <- modelInputData[[id]][[2L]]
                modelInputData[[id]][[3L]] <<- FALSE
              } else {
                selected <- isolate(input[[paste0("slider_", id)]])
              }
              if (names(sliderValues[[name]])[valId] == "def2") {
                return(max(selected))
              }
              return(min(selected))
            } else {
              # retrieve externally dependent data
              k <- match(names(el)[1], tolower(names(modelIn)))[1]
              htmlSelectorPefix <- "in_"
              if (isMobileDevice && identical(modelIn[[k]]$type, "hot")) {
                htmlSelectorPefix <- "in_m_"
              }
              if (modelIn[[k]]$type == "daterange") {
                rv[["in_" %+% k]]
                switch(el[["$operator"]],
                  count = {
                    dateRange <- input[[paste0("daterange_", k)]]
                    return(as.numeric(difftime(dateRange[[2]], dateRange[[1]])))
                  },
                  {
                    errMsg <<- paste(errMsg, "Bad operator for input type: daterange", sep = "\n") # todo: language file!
                  }
                )
                return(NULL)
              }
              if (length(rv[["in_" %+% k]]) && ((identical(modelIn[[k]]$type, "hot") && !isMobileDevice &&
                !is.null(input[[htmlSelectorPefix %+% k]])) ||
                (length(rv[[paste0("wasModified_", k)]]) && !is.null(tableContent[[k]])) ||
                identical(modelIn[[k]]$type, "custom") && length(modelInputDataVisible[[k]])) &&
                !isEmptyInput[k]) {
                tryCatch(
                  {
                    symIdToFetch <- k
                    if (length(modelIn[[k]]$definedByExternalSymbol)) {
                      symIdTmp <- match(modelIn[[i]]$definedByExternalSymbol, names(modelIn))
                      symIdToFetch <- symIdTmp
                    }
                    if (identical(modelIn[[k]]$type, "custom")) {
                      if (length(modelIn[[k]]$widgetSymbols)) {
                        force(modelInputDataVisible[[symIdToFetch]][[names(modelIn)[k]]]())
                      } else {
                        force(modelInputDataVisible[[k]]())
                      }
                    }
                    dataTmp <- unique(getInputDataset(symIdToFetch, visible = TRUE, subSymName = names(modelIn)[[k]])[[el[[1]][1]]])
                  },
                  error = function(e) {
                    flog.error(
                      "Some problem occurred attempting to fetch values for slider: '%s' " %+%
                        "(forward dependency on dataset: '%s'). Error message: %s.",
                      modelInAlias[id], modelInAlias[k], conditionMessage(e)
                    )
                    errMsg <<- paste(errMsg, lang$errMsg$dataError$desc, sep = "\n")
                  }
                )
              } else if (length(modelInputData[[k]][[1]]) && !is.na(modelInputData[[k]][[1]][1]) && isEmptyInput[k]) {
                # no input is shown in UI, so get hidden data
                try(dataTmp <- unique(modelInputData[[k]][[el[[1]][1]]]))
              } else {
                return(NULL)
              }
              if (!length(dataTmp) || identical(dataTmp, "")) {
                return(NULL)
              }
              tryCatch(
                {
                  scalarVal <- getScalarValue(dataTmp, el[["$operator"]])
                  return(scalarVal)
                },
                error = function(e) {
                  flog.warn("Input type for slider: '%s' is not numeric.", name)
                  errMsg <<- paste(errMsg, sprintf(lang$errMsg$renderSlider$desc, el[[1]][1], name), sep = "\n")
                }
              )
            }
          })
          names(sliderData) <- names(sliderValues[[name]])
          showErrorMsg(lang$errMsg$renderSlider$title, errMsg)

          if (is.numeric(sliderData$def1) && is.numeric(sliderData$def2)) {
            sliderData$def <- c(sliderData$def1, sliderData$def2)
          }
          return(sliderData)
        })

        # observe changes of slider data
        observe({
          value <- getData[[i]]()$def
          if (inputInitialized[i] && is.numeric(modelIn[[id]]$slider$default)) {
            # in case slider has only numeric values as default (no dependencies), keep currently selected value(s)
            value <- isolate(input[[paste0("slider_", id)]])
          }
          # if(!is.null(value) && !identical(value, as.numeric(isolate(input[["slider_" %+% id]])))){
          noCheck[id] <<- TRUE
          # }
          newDefaultValue[[i]] <<- value
          updateSliderInput(session,
            inputId = paste0("slider_", id), value = value, min = getData[[i]]()$min,
            max = getData[[i]]()$max, step = getData[[i]]()$step
          )
          if (!inputInitialized[i]) {
            if (!is.null(isolate(getData[[i]]()$min)) && !is.null(isolate(getData[[i]]()$max))) {
              inputInitialized[i] <<- TRUE
              showEl(session, paste0("#slider_", id))
              hideEl(session, paste0("#no_data_dep_", id))
              # refresh selected item in case it was uploaded (e.g. via Excel or database)
              if (length(isolate(rv[[paste0("in_", id)]]))) {
                rv[[paste0("in_", id)]] <<- isolate(rv[[paste0("in_", id)]]) + 1
              } else if (length(modelInputData[[id]][[1]])) {
                rv[[paste0("in_", id)]] <<- 1
              }
            }
          }
        })
        # update slider default value
        observe(
          {
            value <- getSelected[[id]]()
            if (!is.null(value)) {
              if (length(newDefaultValue[[i]])) {
                if (!identical(value, newDefaultValue[[i]])) {
                  noCheck[id] <<- TRUE
                  updateSliderInput(session, inputId = paste0("slider_", id), value = value)
                }
                newDefaultValue[[i]] <<- numeric(0L)
              } else if (!identical(value, as.numeric(isolate(input[[paste0("slider_", id)]])))) {
                noCheck[id] <<- TRUE
                updateSliderInput(session, inputId = paste0("slider_", id), value = value)
              }
            }
          },
          priority = -1
        )
      }
      if (identical(modelIn[[id]]$slider$single, TRUE) ||
        identical(modelIn[[id]]$slider$double, TRUE)) {
        observe(
          {
            rv[["in_" %+% id]]
            value <- modelInputDataHcube[[id]]
            if (length(value) && !identical(
              value[1],
              as.numeric(isolate(input[[paste0("hcubeStep_", id)]]))
            )) {
              noCheck[id] <<- TRUE
              updateNumericInput(session, "hcubeStep_" %+% id,
                value = value[1]
              )
            }
            if (length(value) > 1 && !identical(
              value[2],
              as.numeric(isolate(input[[paste0("hcubeMode_", id)]]))
            )) {
              noCheck[id] <<- TRUE
              updateCheckboxInput(session, "hcubeMode_" %+% id,
                value = value[2]
              )
            }
          },
          priority = -1
        )
      }
    }
  )
})

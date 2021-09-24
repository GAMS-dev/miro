# render tabular datasets
proxy <- vector("list", length(modelIn))

getVisibleTabData <- function(id, type) {
  if (identical(type, "hot")) {
    data <- hotToR(
      isolate(input[["in_" %+% id]]),
      modelIn[[id]]
    )
  } else if (identical(type, "dt")) {
    data <- tableContent[[id]]
  } else {
    stop("Unsupported type (getVisibleTabData).", call. = FALSE)
  }
  if (length(modelIn[[id]]$pivotCols)) {
    keyIdx <- match(
      modelIn[[id]]$pivotCols[[1]],
      names(modelIn[[id]]$headers)
    )[[1L]]
    if (identical(names(data)[keyIdx], modelIn[[id]]$pivotCols[[1]]) &&
      length(data) == length(modelIn[[id]]$headers)) {
      names(data) <- names(modelIn[[id]]$headers)
      return(data)
    }
    if (length(data) < length(modelIn[[id]]$headers) - 1L) {
      return(modelInTemplate[[id]])
    }
    return(select(pivot_longer(suppressWarnings(
      mutate_at(data, seq(
        length(modelIn[[id]]$headers) - 1L,
        length(data)
      ), as.numeric)
    ),
    cols = seq(
      length(modelIn[[id]]$headers) - 1L,
      length(data)
    ),
    names_to = modelIn[[id]]$pivotCols[[1]],
    values_to = names(modelIn[[id]]$headers)[length(modelIn[[id]]$headers)],
    values_drop_na = TRUE
    ), !!!names(modelIn[[id]]$headers)))
  }
  return(data)
}

getInputDataset <- function(id, visible = FALSE, subSymName = NULL) {
  if (!length(modelIn[[id]]$pivotCols)) {
    intermDataTmp <- getInputDatasetRaw(id, subSymName = subSymName)
    if (is_tibble(intermDataTmp)) {
      return(intermDataTmp %>%
        mutate_if(is.character,
          replace_na,
          replace = ""
        ))
    }
    return(intermDataTmp)
  }
  if (visible) {
    widgetType <- modelIn[[id]]$type
    if (isMobileDevice && identical(widgetType, "hot")) {
      widgetType <- "dt"
    }
    if (identical(widgetType, "hot")) {
      intermDataTmp <- getVisibleTabData(id, "hot")
    } else if (identical(widgetType, "dt")) {
      intermDataTmp <- getVisibleTabData(id, "dt")
    } else {
      flog.error(
        "Cannot get input datatset: '%s': Unsupported type: '%s'.",
        names(modelIn)[id], widgetType
      )
      return(modelInTemplate[[id]])
    }
  } else {
    intermDataTmp <- getInputDatasetRaw(id, subSymName = subSymName)
    if (!is_tibble(intermDataTmp)) {
      return(intermDataTmp)
    }
  }
  if (modelIn[[id]]$pivotCols[[1]] %in% names(intermDataTmp)) {
    # table not yet initialised (so not pivoted either)
    return(intermDataTmp %>%
      mutate_if(is.character,
        replace_na,
        replace = ""
      ))
  }

  return(intermDataTmp)
}
getInputDatasetRaw <- function(id, subSymName = NULL) {
  widgetType <- modelIn[[id]]$type
  htmlSelector <- paste0("in_", id)
  if (isMobileDevice && identical(widgetType, "hot")) {
    widgetType <- "dt"
    htmlSelector <- paste0("in_m_", id)
  }
  if (widgetType %in% c("dt", "hot")) {
    if ((!is.null(isolate(input[[htmlSelector]])) && hotInit[[id]]) || length(tableContent[[id]])) {
      if (length(colsWithDep[[id]])) {
        if (!isEmptyInput[id]) {
          if (widgetType == "hot") {
            dataTmp <- getVisibleTabData(id, "hot")
            if (!length(dataTmp) || identical(nrow(dataTmp), 1L) &&
              all(vapply(dataTmp, identical, logical(1L), "", USE.NAMES = FALSE))) {
              return(bind_rows(
                modelInputData[[id]],
                modelInputDataVisible[[id]]
              ))
            }
            return(bind_rows(dataTmp, modelInputData[[id]]))
          }
          return(bind_rows(
            getVisibleTabData(id, "dt"),
            modelInputData[[id]]
          ))
        }
        return(modelInputData[[id]])
      }
      if (!isEmptyInput[id]) {
        if (widgetType == "hot") {
          return(getVisibleTabData(id, "hot"))
        }
        return(getVisibleTabData(id, "dt"))
      }
      return(modelInTemplate[[id]])
    } else if (!is.null(modelInputData[[id]])) {
      # tab was never activated, so shiny does not update handsontable thus it is
      # empty although data was loaded
      return(modelInputData[[id]])
    }
    stop("No input data found.", call. = FALSE)
  } else {
    if (length(modelIn[[id]]$widgetSymbols)) {
      data <- isolate(modelInputDataVisible[[id]][[subSymName]]())
      if (!length(modelIn[[subSymName]]$headers)) {
        return(data)
      }
      symIdTmp <- match(subSymName, names(modelIn))
    } else {
      data <- isolate(modelInputDataVisible[[id]]())
      symIdTmp <- id
    }
    if (identical(scalarsFileName, names(modelIn)[[symIdTmp]])) {
      if (is.null(data)) {
        if (length(modelInputData[[symIdTmp]])) {
          return(modelInputData[[symIdTmp]])
        }
        return(scalarsInTemplate)
      }
      if (length(data) < 2L || length(data) > 3L) {
        stop("Scalars dataframe must not have less than 2 and not more than 3 columns.", call. = FALSE)
      }
      if (!nrow(data)) {
        return(scalarsInTemplate)
      }
      scalarIdsTmp <- match(tolower(data[[1]]), scalarsInTemplate[[1]])
      if (any(is.na(scalarIdsTmp))) {
        stop(sprintf(
          "Scalars dataframe contains invalid scalar(s): '%s'.",
          paste(data[[1]][is.na(scalarIdsTmp)], collapse = ", ")
        ), call. = FALSE)
      }
      dataToReturn <- scalarsInTemplate
      if (identical(length(data), 2L)) {
        dataToReturn[[3]][scalarIdsTmp] <- data[[2L]]
      } else {
        dataToReturn[[3]][scalarIdsTmp] <- data[[3L]]
      }
      return(dataToReturn)
    } else if (is.null(data)) {
      if (length(modelInputData[[symIdTmp]])) {
        return(modelInputData[[symIdTmp]])
      }
      return(modelInTemplate[[symIdTmp]])
    } else if (identical(length(data), length(modelIn[[symIdTmp]]$headers)) &&
      hasValidHeaderTypes(data, modelIn[[symIdTmp]]$colTypes)) {
      names(data) <- names(modelInTemplate[[symIdTmp]])
      return(data)
    }
    stop("No valid input data found.", call. = FALSE)
  }
}
pivotData <- function(i, tabData, force = FALSE) {
  if (isEmptyInput[i] && !force) {
    return(list(data = tabData, colnames = attr(modelInTemplate[[i]], "aliases")))
  }
  pivotIdx <- match(modelIn[[i]]$pivotCols[[1]], names(modelIn[[i]]$headers))[[1L]]
  attrTmp <- attr(modelInTemplate[[i]], "aliases")[-c(pivotIdx, length(modelInTemplate[[i]]))]
  if (tryCatch(
    {
      tabData <- pivot_wider(tabData,
        names_from = !!pivotIdx,
        values_from = !!length(tabData),
        names_sort = isTRUE(modelIn[[i]]$sortPivotCols)
      )
      FALSE
    },
    error = function(e) {
      TRUE
    }
  )) {
    if (force) {
      return(list(
        data = tabData[-c(pivotIdx, length(modelInTemplate[[i]]))],
        colnames = attrTmp
      ))
    }
    return(list(data = tabData, colnames = attr(modelInTemplate[[i]], "aliases")))
  }

  attrTmp <- c(
    attrTmp,
    names(tabData)[seq(
      length(attrTmp) + 1L,
      length(tabData)
    )]
  )
  return(list(data = tabData, colnames = attrTmp))
}
observe({
  req(input$inputTabset)
  i <- as.integer(strsplit(input$inputTabset, "_")[[1]][2])
  if (is.na(i) || i < 1L) {
    disableEl(session, "#btGraphIn")
    return()
  }
  if (length(inputTabTitles[[i]]) > 1L) {
    j <- as.integer(strsplit(input[[paste0("inputTabset", i)]], "_")[[1]][2])
    i <- inputTabs[[i]][j]
  } else {
    i <- inputTabs[[i]]
    if (length(i) > 1L) {
      # always enable if multiple symbols on same tab
      enableEl(session, "#btGraphIn")
      return()
    }
  }
  if (is.null(configGraphsIn[[i]]) || isEmptyInput[i]) {
    disableEl(session, "#btGraphIn")
  } else {
    enableEl(session, "#btGraphIn")
  }
})
observeEvent(input$btGraphIn, {
  i <- as.integer(strsplit(input$inputTabset, "_")[[1]][2])
  showLoadingScreen(session, 500)
  on.exit(hideLoadingScreen(session))
  if (is.na(i) || i < 1) {
    return()
  }
  if (length(inputTabTitles[[i]]) > 1L) {
    j <- as.integer(strsplit(input[[paste0("inputTabset", i)]], "_")[[1]][2])
    ids <- inputTabs[[i]][j]
  } else {
    ids <- inputTabs[[i]]
  }
  errMsg <- NULL
  lapply(ids, function(i) {
    toggleEl(session, "#graph-in_" %+% i)
    toggleEl(session, "#data-in_" %+% i)

    if (modelInputGraphVisible[[i]]) {
      flog.debug("Graph view for model input in sheet: %d deactivated", i)
      modelInputGraphVisible[[i]] <<- FALSE
      return()
    } else {
      flog.debug("Graph view for model input in sheet: %d activated.", i)
      modelInputGraphVisible[[i]] <<- TRUE
    }

    if (is.null(configGraphsIn[[i]])) {
      return()
    } else if (modelIn[[i]]$type %in% c("hot", "dt")) {
      errMsg <- NULL
      tryCatch(
        {
          data <- getInputDataset(i, visible = TRUE)
        },
        error = function(e) {
          flog.error(
            "Dataset: '%s' could not be loaded. Error message: '%s'.",
            modelInAlias[i], conditionMessage(e)
          )
          errMsg <<- sprintf(
            lang$errMsg$GAMSInput$noData,
            modelInAlias[i]
          )
        }
      )
      if (is.null(showErrorMsg(lang$errMsg$GAMSInput$title, errMsg))) {
        return()
      }
    } else {
      if (length(modelIn[[i]]$widgetSymbols)) {
        data <- tryCatch(modelInputDataVisible[[i]][[names(modelIn)[[i]]]](), error = function(e) {
          flog.warn("Problems getting data from custom widget. Error message: %s", conditionMessage(e))
          return(modelInTemplate[[i]])
        })
      } else {
        data <- tryCatch(modelInputDataVisible[[i]](), error = function(e) {
          flog.warn("Problems getting data from custom widget. Error message: %s", conditionMessage(e))
          return(modelInTemplate[[i]])
        })
      }
    }
    if (!dynamicUILoaded$inputGraphs[i]) {
      tryCatch(
        {
          insertUI(paste0("#graph-in_", i),
            ui = renderDataUI(paste0("in_", i),
              type = configGraphsIn[[i]]$outType,
              graphTool = configGraphsIn[[i]]$graph$tool,
              customOptions = configGraphsIn[[i]]$options,
              filterOptions = configGraphsIn[[i]]$graph$filter,
              height = configGraphsIn[[i]]$height,
              createdDynamically = TRUE
            ),
            immediate = TRUE
          )
          dynamicUILoaded$inputGraphs[i] <<- TRUE
        },
        error = function(e) {
          flog.error(sprintf(
            "Problems generating UI elements for chart for dataset: '%s'. Error message: %s.",
            modelInAlias[i], conditionMessage(e)
          ))
          errMsg <<- sprintf(lang$errMsg$renderGraph$desc, modelInAlias[i])
        }
      )
      if (is.null(showErrorMsg(lang$errMsg$renderGraph$title, errMsg))) {
        return()
      }
    }
    tryCatch(
      {
        if (is.null(rendererEnv[[paste0("in_", i)]])) {
          rendererEnv[[paste0("in_", i)]] <- new.env(parent = emptyenv())
        } else {
          for (el in ls(envir = rendererEnv[[paste0("in_", i)]])) {
            if ("Observer" %in% class(rendererEnv[[paste0("in_", i)]][[el]])) {
              rendererEnv[[paste0("in_", i)]][[el]]$destroy()
            }
          }
        }
        callModule(renderData, paste0("in_", i),
          type = configGraphsIn[[i]]$outType,
          data = data,
          dtOptions = config$datatable,
          graphOptions = configGraphsIn[[i]]$graph,
          pivotOptions = configGraphsIn[[i]]$pivottable,
          customOptions = configGraphsIn[[i]]$options,
          roundPrecision = roundPrecision, modelDir = modelDir,
          rendererEnv = rendererEnv[[paste0("in_", i)]],
          views = views, attachments = attachments
        )
      },
      error = function(e) {
        flog.error(
          "Problems rendering output charts and/or tables for dataset: '%s'. Error message: %s.",
          modelInAlias[i], conditionMessage(e)
        )
        errMsg <<- paste(errMsg, sprintf(lang$errMsg$renderGraph$desc, modelInAlias[i]), sep = "\n")
      }
    )
  })
  showErrorMsg(lang$errMsg$renderGraph$title, errMsg)
})

lapply(modelInTabularData, function(sheet) {
  # get input element id of dataset
  i <- match(sheet, tolower(names(modelIn)))[[1]]
  if (isTRUE(modelIn[[i]]$dropdown$multiple) ||
    length(modelIn[[i]]$definedByExternalSymbol)) {
    return()
  }
  widgetType <- modelIn[[i]]$type
  htmlSelector <- paste0("in_", i)
  if (isMobileDevice && identical(widgetType, "hot")) {
    widgetType <- "dt"
    htmlSelector <- paste0("in_m_", i)
  }
  if (length(colsWithDep[[i]])) {
    dataModelIn[[i]] <<- reactive({
      if (identical(widgetType, "hot")) {
        hotInit[[i]] <<- TRUE
      }
      # make sure data will be updated when old data is overwritten
      rv[["in_" %+% i]]
      if (isEmptyInput[i]) {
        data <- modelInputData[[i]]
      } else {
        # save changes made in handsontable
        if (identical(widgetType, "hot") &&
          !is.null(isolate(input[["in_" %+% i]]))) {
          tableContent[[i]] <<- hotToR(
            isolate(input[["in_" %+% i]]),
            modelIn[[i]]
          )
        }

        tryCatch(
          {
            data <- bind_rows(tableContent[[i]], modelInputData[[i]])
          },
          error = function(e) {
            flog.warn(paste0(lang$errMsg$dataError$desc, e))
            errMsg <<- paste(errMsg, lang$errMsg$dataError$desc, sep = "\n")
          }
        )
        modelInputData[[i]] <<- data
      }
      for (iDep in seq_along(colsWithDep[[i]])) {
        # get id of element (e.g. dropdown menu) that causes backward dependency
        id <- colsWithDep[[i]][[iDep]]
        # in case nothing was selected in dropdown menu, skip this iteration
        if (is.null(input[["dropdown_" %+% id]]) ||
          input[["dropdown_" %+% id]] %in% c("", "_")) {
          next
        }
        # get column name with dependency
        col <- names(colsWithDep[[i]])[[iDep]]
        # filter data frame
        data <- data[data[[col]] %in% input[["dropdown_" %+% id]], ]
      }
      modelInputData[[i]] <<- anti_join(modelInputData[[i]],
        data,
        by = idsIn[[i]]
      )

      if (identical(widgetType, "hot")) {
        if (!nrow(data)) {
          data[1, ] <- NA
          data <- mutate_if(data, is.character,
            replace_na,
            replace = ""
          )
          if (!is.null(configGraphsIn[[i]]) &&
            length(inputTabs[[tabSheetMap$input[[i]]]]) == 1L) {
            disableEl(session, "#btGraphIn")
          }
          isEmptyInput[i] <<- TRUE
        } else {
          if (!is.null(configGraphsIn[[i]])) {
            enableEl(session, "#btGraphIn")
          }
          if (!identical(names(modelIn)[i], scalarsFileName) ||
            any(!is.na(modelInputData[[i]][[3]]))) {
            isEmptyInput[i] <<- FALSE
          }
        }
        modelInputDataVisible[[i]] <<- data
      } else {
        if (!nrow(data)) {
          if (!is.null(configGraphsIn[[i]]) &&
            length(inputTabs[[tabSheetMap$input[[i]]]]) == 1L) {
            disableEl(session, "#btGraphIn")
          }
          isEmptyInput[i] <<- TRUE
        } else {
          if (!is.null(configGraphsIn[[i]])) {
            enableEl(session, "#btGraphIn")
          }
          if (!identical(names(modelIn)[i], scalarsFileName) ||
            any(!is.na(modelInputData[[i]][[3]]))) {
            isEmptyInput[i] <<- FALSE
          }
        }
        tableContent[[i]] <<- data
      }
      return(data)
    })
  } else {
    dataModelIn[[i]] <<- reactive({
      rv[["in_" %+% i]]
      if (identical(widgetType, "hot")) {
        hotInit[[i]] <<- TRUE
        if (length(modelInputData[[i]]) &&
          nrow(modelInputData[[i]]) > 0L) {
          if (!is.null(configGraphsIn[[i]])) {
            enableEl(session, "#btGraphIn")
          }
          if (!identical(names(modelIn)[i], scalarsFileName) ||
            any(!is.na(modelInputData[[i]][[3]]))) {
            isEmptyInput[i] <<- FALSE
          }
        } else {
          modelInputData[[i]][1, ] <<- NA
          modelInputData[[i]] <- mutate_if(modelInputData[[i]], is.character,
            replace_na,
            replace = ""
          )
          if (!is.null(configGraphsIn[[i]]) &&
            length(inputTabs[[tabSheetMap$input[[i]]]]) == 1L) {
            disableEl(session, "#btGraphIn")
          }
          isEmptyInput[i] <<- TRUE
        }
        return(modelInputData[[i]])
      }
      if (length(modelInputData[[i]]) &&
        nrow(modelInputData[[i]]) > 0L) {
        if (!is.null(configGraphsIn[[i]])) {
          enableEl(session, "#btGraphIn")
        }
        if (!identical(names(modelIn)[i], scalarsFileName) ||
          any(!is.na(modelInputData[[i]][[3]]))) {
          isEmptyInput[i] <<- FALSE
        }
      } else {
        if (!is.null(configGraphsIn[[i]]) &&
          length(inputTabs[[tabSheetMap$input[[i]]]]) == 1L) {
          disableEl(session, "#btGraphIn")
        }
        isEmptyInput[i] <<- TRUE
      }
      tableContent[[i]] <<- modelInputData[[i]]
      return(tableContent[[i]])
    })
  }
  if (identical(widgetType, "hot")) {
    # rendering handsontables for input data
    if (length(modelIn[[i]]$pivotCols)) {
      noDomains <- sum(vapply(modelIn[[i]]$headers, function(header) {
        identical(header$type, "string")
      }, logical(1L), USE.NAMES = FALSE)) - 1L
    }
    output[[paste0("in_", i)]] <- renderRHandsontable({
      noCheck[i] <<- TRUE
      isPivoted <- FALSE
      tabData <- dataModelIn[[i]]()

      if (length(modelIn[[i]]$pivotCols)) {
        isPivoted <- TRUE
        tabData <- pivotData(i, tabData, force = TRUE)
        colnames <- tabData$colnames
        tabData <- tabData$data
      } else {
        colnames <- attr(modelInTemplate[[i]], "aliases")
      }

      # check for readonly columns
      colsReadonly <- vapply(seq_along(modelIn[[i]]$headers), function(j) {
        if (identical(modelIn[[i]]$headers[[j]]$readonly, TRUE)) {
          modelIn[[i]]$headers[[j]]$alias
        } else {
          NA_character_
        }
      }, character(1L), USE.NAMES = FALSE)
      colsReadonly <- colsReadonly[!is.na(colsReadonly)]

      isRo <- FALSE
      if (isTRUE(modelIn[[i]]$readonly) || length(colsReadonly) > 0L) {
        isRo <- TRUE
      }

      ht <- rhandsontable(tabData,
        height = hotOptions$height,
        rowHeaders = if (isTRUE(modelIn[[i]]$hideIndexCol)) NULL else rownames(tabData),
        colHeaders = colnames, useTypes = TRUE,
        width = hotOptions$width, search = hotOptions$search,
        readOnly = if (isTRUE(modelIn[[i]]$readonly)) TRUE else NULL,
        selectCallback = TRUE, digits = NA,
        naAsNull = isPivoted || isTRUE(attr(modelInTemplate[[i]], "isTable"))
      )
      ht <- hot_table(ht,
        contextMenu = hotOptions$contextMenu$enabled,
        highlightCol = hotOptions$highlightCol,
        highlightRow = hotOptions$highlightRow,
        rowHeaderWidth = hotOptions$rowHeaderWidth,
        stretchH = hotOptions$stretchH,
        overflow = hotOptions$overflow
      )
      if (isTRUE(hotOptions$contextMenu$enabled)) {
        if (isPivoted && !isRo && !isTRUE(modelIn[[i]]$pivotColIsReadonly)) {
          ht <- hot_context_menu(ht,
            allowRowEdit = hotOptions$contextMenu$allowRowEdit,
            allowColEdit = FALSE,
            customOpts = getHotCustomColOptions(noDomains)
          )
        } else {
          ht <- hot_context_menu(ht,
            allowRowEdit = if (isRo) FALSE else hotOptions$contextMenu$allowRowEdit,
            allowColEdit = FALSE,
            allowReadOnly = hotOptions$contextMenu$allowReadOnly
          )
        }
      }
      ht <- hot_cols(ht,
        columnSorting = if (isPivoted) FALSE else hotOptions$columnSorting,
        manualColumnMove = hotOptions$manualColumnMove,
        manualColumnResize = hotOptions$manualColumnResize,
        colWidths = if (length(modelIn[[i]]$colWidths)) {
          modelIn[[i]]$colWidths
        } else {
          hotOptions$colWidths
        },
        fixedColumnsLeft = modelIn[[i]]$fixedColumnsLeft
      )
      for (colSourceConfig in modelIn[[i]]$dropdownCols) {
        k <- match(colSourceConfig$symbol, names(modelIn))
        source <- NULL
        if (length(colSourceConfig$static)) {
          source <- colSourceConfig$static
        } else if (length(rv[["in_" %+% k]]) && !isEmptyInput[k] &&
          (widgetType == "hot" &&
            !is.null(input[["in_" %+% k]]) ||
            (length(rv[[paste0("wasModified_", k)]]) && !is.null(tableContent[[k]])) ||
            identical(widgetType, "custom") && length(modelInputDataVisible[[k]]))) {
          tryCatch(
            {
              source <- unique(getInputDataset(k)[[colSourceConfig$colId]])
            },
            error = function(e) {
              flog.error(
                "Some problem occurred attempting to fetch values for table: '%s' " %+%
                  "(forward dependency on dataset: '%s'). Error message: %s.",
                modelInAlias[id], modelInAlias[k], conditionMessage(e)
              )
            }
          )
        } else if (length(modelInputData[[k]][[1]]) && !is.na(modelInputData[[k]][[1]][1]) &&
          isEmptyInput[k]) {
          force(input[["in_" %+% k]])
          source <- unique(modelInputData[[k]][[colSourceConfig$colId]])
        }
        if (length(source)) {
          ht <- hot_col(ht, colSourceConfig$ddColId,
            type = colSourceConfig$type, source = I(source), strict = TRUE,
            allowInvalid = FALSE
          )
        }
      }
      for (colOption in modelIn[[i]]$colFormat) {
        ht$x$columns[[colOption$colId]]$numericFormat <- list(pattern = colOption$format)
        if (length(colOption$language)) {
          ht$x$columns[[colOption$colId]]$numericFormat$culture <- colOption$language
        }
      }
      if (length(colsReadonly)) {
        ht <- hot_col(ht, colsReadonly, readOnly = TRUE)
      }
      if (identical(modelIn[[i]]$heatmap, TRUE)) {
        return(hot_heatmap(ht))
      }
      return(ht)
    })
    observe({
      input[[paste0("in_", i)]]
      if (is.null(input[[paste0("in_", i)]])) {
        return()
      }
      if (is.null(isolate(rv[[paste0("in_", i)]])) && !isEmptyInput[i]) {
        modelInputData[[i]] <<- getVisibleTabData(i, "hot")
        isolate(rv[[paste0("in_", i)]] <- 1L)
      }
    })
  } else if (identical(widgetType, "dt")) {
    output[[htmlSelector]] <- renderDT({
      errMsg <- NULL
      tabData <- dataModelIn[[i]]()

      if (length(modelIn[[i]]$pivotCols)) {
        isPivoted <- TRUE
        tabData <- pivotData(i, tabData)
        colnames <- tabData$colnames
        tabData <- tabData$data
        tableContent[[i]] <<- tabData
      } else {
        colnames <- attr(modelInTemplate[[i]], "aliases")
      }
      tryCatch(
        {
          dtOptions <- modifyList(
            config$datatable,
            list(
              editable = !identical(
                modelIn[[i]]$readonly,
                TRUE
              ),
              options = list(scrollX = TRUE),
              colnames = colnames
            )
          )

          dt <- renderDTable(tabData, dtOptions,
            roundPrecision = roundPrecision, render = FALSE
          )
        },
        error = function(e) {
          flog.error(
            "Problems rendering table for input dataset: %s. Error message: %s.",
            modelInAlias[[i]], conditionMessage(e)
          )
          errMsg <<- sprintf(lang$errMsg$renderTable$desc, modelInAlias[i])
        }
      )
      if (is.null(showErrorMsg(lang$errMsg$renderTable$title, errMsg))) {
        return(dataModelIn[[i]]())
      }
      return(dt)
    })
    proxy[[i]] <<- dataTableProxy(htmlSelector)

    observeEvent(input[[paste0(htmlSelector, "_add_row")]], {
      if (!length(tableContent[[i]])) {
        flog.warn(
          "Remove rows button (symbol: %s) was clicked but tableContent has no length.",
          names(modelIn)[i]
        )
        return()
      }
      removeUI("body>.selectize-dropdown", multiple = TRUE, immediate = TRUE)

      noRowHeaders <- sum(vapply(modelIn[[i]]$headers, function(header) {
        return(identical(header$type, "string"))
      }, logical(1L), USE.NAMES = FALSE))

      if (length(modelIn[[i]]$pivotCols) && nrow(tableContent[[i]]) > 0L) {
        noRowHeaders <- noRowHeaders - 1L
        pivotIdx <- match(modelIn[[i]]$pivotCols[[1]], names(modelIn[[i]]$headers))[[1L]]
        colnames <- c(
          attr(modelInTemplate[[i]], "aliases")[-c(pivotIdx, length(modelInTemplate[[i]]))],
          names(tableContent[[i]])[seq(noRowHeaders + 1L, length(tableContent[[i]]))]
        )
      } else {
        colnames <- attr(modelInTemplate[[i]], "aliases")
      }
      newRowId <- suppressWarnings(as.integer(input[[paste0(htmlSelector, "_rows_selected")]]))
      if (any(is.na(newRowId))) {
        return(flog.error("Invalid data for 'rows_selected' (symbol: %s).", names(modelIn)[i]))
      }
      if (length(newRowId) == 1L) {
        newRowId <- c(newRowId - 1L, newRowId)
      } else if (length(newRowId) > 1L) {
        newRowId <- c(min(newRowId) - 1L, max(newRowId))
      }
      showModal(
        modalDialog(
          title = lang$renderers$miroPivot$dialogAddRow$title,
          tags$div(id = "newRowError", class = "gmsalert gmsalert-error"),
          tags$div(
            class = "table-responsive", style = "margin-top:30px",
            tags$table(
              class = "table",
              tags$tr(
                lapply(colnames, tags$th)
              ),
              tags$tr(lapply(seq_along(colnames), function(j) {
                tags$td(
                  class = "table-add-row",
                  if (j <= noRowHeaders) {
                    serverSelectInput(session, paste0("newRow_", j), NULL,
                      unique(tableContent[[i]][[j]]),
                      multiple = TRUE,
                      width = "100%",
                      options = list(
                        create = TRUE, maxItems = 1L,
                        dropdownParent = "body"
                      )
                    )
                  } else {
                    textInput(paste0("newRow_", j), NULL, NA, width = "100%")
                  }
                )
              }))
            )
          ),
          selectInput("newRowId", NULL,
            choices = if (length(newRowId) == 0L) {
              setNames(
                c(0L, nrow(tableContent[[i]])),
                lang$renderers$datatable$addRowPosNoneSelected
              )
            } else {
              setNames(newRowId, lang$renderers$datatable$addRowPos)
            },
            selected = newRowId[2]
          ),
          footer = tagList(
            tags$div(
              class = "modal-footer-mobile",
              modalButton(lang$renderers$miroPivot$dialogAddRow$btCancel),
              actionButton(paste0(htmlSelector, "_add_row_confirm"),
                label = lang$renderers$miroPivot$btAddRow,
                class = "bt-highlight-1 bt-gms-confirm"
              )
            )
          ),
          fade = TRUE, easyClose = FALSE, size = "l"
        )
      )
    })
    observeEvent(input[[paste0(htmlSelector, "_add_row_confirm")]], {
      flog.trace("Add row button for table: %s clicked.", names(modelIn)[i])
      if (!length(tableContent[[i]])) {
        flog.warn(
          "Add rows button (symbol: %s) was clicked but tableContent has no length.",
          names(modelIn)[i]
        )
        return()
      }
      noRowHeaders <- sum(vapply(modelIn[[i]]$headers, function(header) {
        return(identical(header$type, "string"))
      }, logical(1L), USE.NAMES = FALSE))

      if (length(modelIn[[i]]$pivotCols) && nrow(tableContent[[i]]) > 0L) {
        noRowHeaders <- noRowHeaders - 1L
      }
      newKeys <- vapply(seq_len(noRowHeaders), function(i) {
        editedKey <- tryCatch(trimws(input[[paste0("newRow_", i)]]),
          error = function(e) {
            NA_character_
          }
        )
        if (isValidUEL(editedKey)) {
          return(editedKey)
        }
        return(NA_character_)
      }, character(1L), USE.NAMES = FALSE)
      if (any(is.na(newKeys))) {
        return(showHideEl(
          session, paste0("#", "newRowError"), 5000L,
          lang$renderers$miroPivot$dialogAddRow$invalidKeysError
        ))
      }
      invalidValue <- FALSE
      newValues <- vapply(seq(noRowHeaders + 1L, length(tableContent[[i]])), function(i) {
        newVal <- trimws(input[[paste0("newRow_", i)]])
        if (identical(newVal, "")) {
          return(NA_real_)
        }
        newVal <- tryCatch(suppressWarnings(as.numeric(input[[paste0("newRow_", i)]])),
          error = function(e) {
            NA_real_
          }
        )
        if (length(newVal) != 1L) {
          return(NA_real_)
        }
        if (is.na(newVal) && !invalidValue) {
          invalidValue <<- TRUE
        }
        return(newVal)
      }, numeric(1L), USE.NAMES = FALSE)

      if (invalidValue) {
        return(showHideEl(
          session, paste0("#", "newRowError"), 5000L,
          lang$renderers$miroPivot$dialogAddRow$invalidValuesError
        ))
      }

      newRowId <- suppressWarnings(as.integer(input$newRowId))
      if (length(newRowId) != 1L || is.na(newRowId)) {
        return(flog.error("Invalid data for 'newRowId' (symbol: %s).", names(modelIn)[i]))
      }
      tableContent[[i]] <<- add_row(tableContent[[i]],
        !!!setNames(
          c(
            as.list(newKeys),
            as.list(newValues)
          ),
          names(tableContent[[i]])
        ),
        .after = newRowId
      )
      replaceData(proxy[[i]], tableContent[[i]], resetPaging = FALSE, rownames = config$datatable$rownames)
      removeModal()
      if (is.null(rv[[paste0("in_", i)]])) {
        modelInputData[[i]] <<- tableContent[[i]]
        rv[[paste0("in_", i)]] <- 1
      }
      if (is.null(rv[[paste0("wasModified_", i)]])) {
        rv[[paste0("wasModified_", i)]] <- 1
      } else {
        rv[[paste0("wasModified_", i)]] <- rv[[paste0("wasModified_", i)]] + 1L
      }
      flog.trace("Added row (symbol: %s).", names(modelIn)[i])
    })
    observeEvent(input[[paste0(htmlSelector, "_remove_row")]], {
      flog.trace("Remove rows button (symbol: %s) was clicked.", names(modelIn)[i])
      idsToRemove <- input[[paste0(htmlSelector, "_rows_selected")]]
      if (!length(idsToRemove)) {
        flog.trace("No rows selected (symbol: %s).", names(modelIn)[i])
        return()
      }
      if (!length(tableContent[[i]])) {
        flog.warn(
          "Remove rows button (symbol: %s) was clicked but tableContent has no length.",
          names(modelIn)[i]
        )
        return()
      }
      tableContent[[i]] <<- tableContent[[i]][-idsToRemove, ]
      replaceData(proxy[[i]], tableContent[[i]], resetPaging = FALSE, rownames = config$datatable$rownames)
      if (is.null(rv[[paste0("wasModified_", i)]])) {
        rv[[paste0("wasModified_", i)]] <- 1
      } else {
        rv[[paste0("wasModified_", i)]] <- rv[[paste0("wasModified_", i)]] + 1L
      }
      flog.trace("Removed %s row(s) (symbol: %s).", length(idsToRemove), names(modelIn)[i])
    })
    observeEvent(input[[paste0(htmlSelector, "_cell_edit")]], {
      rownames <- config$datatable$rownames
      info <- input[[paste0(htmlSelector, "_cell_edit")]]
      row <- info$row
      if (rownames) {
        col <- info$col
        if (col < 1) {
          return()
        }
      } else {
        col <- info$col + 1L
      }
      val <- info$value
      tableContent[[i]][row, col] <<- suppressWarnings(coerceValue(
        val,
        tableContent[[i]][[col]][row]
      ))
      replaceData(proxy[[i]], tableContent[[i]], resetPaging = FALSE, rownames = rownames)
      if (is.null(rv[[paste0("wasModified_", i)]])) {
        rv[[paste0("wasModified_", i)]] <- 1
      } else {
        rv[[paste0("wasModified_", i)]] <- rv[[paste0("wasModified_", i)]] + 1L
      }
      flog.trace("Modified value of row: %s, column: %s, value: %s (symbol: %s).", row, col, val, names(modelIn)[i])
    })
  } else if (identical(widgetType, "custom")) {
    rendererEnv[[paste0("input_", i)]] <- new.env(parent = emptyenv())
    if (identical(modelIn[[i]]$apiVersion, 2L)) {
      tryCatch(
        {
          if (length(modelIn[[i]]$additionalData)) {
            dataIds <- c(i, match(modelIn[[i]]$additionalData, names(modelIn)))
          } else {
            dataIds <- i
          }
          if (length(modelIn[[i]]$widgetSymbols)) {
            dataIds <- c(dataIds, match(modelIn[[i]]$widgetSymbols, names(modelIn)))
          }
          dataIds <- unique(dataIds)
          modelInputDataVisible[[i]] <<- callModule(generateData, paste0("data-in_", i),
            type = modelIn[[i]]$rendererName,
            data = if (length(dataIds) > 1L) {
              dataModelIn[dataIds]
            } else {
              dataModelIn[[i]]
            },
            customOptions = modelIn[[i]]$options,
            rendererEnv = rendererEnv[[paste0("input_", i)]],
            attachments = attachments,
            views = views
          )
          widgetModifiedSkipCount[[i]] <<- widgetModifiedSkipCount[[i]] + 1L
        },
        error = function(e) {
          flog.error(
            "Problems rendering table for input dataset: %s. Error message: %s.",
            modelInAlias[[i]], conditionMessage(e)
          )
          errMsg <<- sprintf(lang$errMsg$renderTable$desc, modelInAlias[i])
        }
      )
    } else {
      observe({
        tryCatch(
          {
            for (el in ls(envir = rendererEnv[[paste0("input_", i)]])) {
              if ("Observer" %in% class(rendererEnv[[paste0("input_", i)]][[el]])) {
                rendererEnv[[paste0("input_", i)]][[el]]$destroy()
              }
            }
            modelInputDataVisible[[i]] <<- callModule(generateData, paste0("data-in_", i),
              type = modelIn[[i]]$rendererName,
              data = as_tibble(dataModelIn[[i]]()),
              customOptions = modelIn[[i]]$options,
              rendererEnv = rendererEnv[[paste0("input_", i)]],
              attachments = attachments,
              views = views
            )
            isolate(rv[[paste0("reinit_", i)]] <- isFALSE(rv[[paste0("reinit_", i)]]))
            widgetModifiedSkipCount[[i]] <<- widgetModifiedSkipCount[[i]] + 1L
          },
          error = function(e) {
            flog.error(
              "Problems rendering table for input dataset: %s. Error message: %s.",
              modelInAlias[[i]], conditionMessage(e)
            )
            errMsg <<- sprintf(lang$errMsg$renderTable$desc, modelInAlias[i])
          }
        )
      })
    }
    if (length(modelIn[[i]]$widgetSymbols)) {
      symIdsTmp <- c(i, match(modelIn[[i]]$widgetSymbols, names(modelIn)))
      lapply(seq_along(symIdsTmp), function(idx) {
        symIdTmp <- symIdsTmp[[idx]]
        observe({
          force(rv[[paste0("reinit_", symIdTmp)]])
          if (is.null(force(modelInputDataVisible[[i]][[idx]]()))) {
            return()
          }
          if (widgetModifiedSkipCount[[symIdTmp]] > 0L) {
            widgetModifiedSkipCount[[symIdTmp]] <<- widgetModifiedSkipCount[[symIdTmp]] - 1L
            return()
          }
          if (any(widgetModifiedSkipCount[symIdTmp] > 0L)) {
            return()
          }
          isolate({
            if (is.null(rv[[paste0("wasModified_", i)]])) {
              rv[[paste0("wasModified_", i)]] <- 1
            } else {
              rv[[paste0("wasModified_", i)]] <- rv[[paste0("wasModified_", i)]] + 1L
            }
          })
        })
      })
    } else {
      observe({
        force(rv[[paste0("reinit_", i)]])
        if (is.null(force(modelInputDataVisible[[i]]()))) {
          return()
        } else if (widgetModifiedSkipCount[[i]] > 0L) {
          widgetModifiedSkipCount[[i]] <<- widgetModifiedSkipCount[[i]] - 1L
          return()
        }
        isolate({
          if (is.null(rv[[paste0("wasModified_", i)]])) {
            rv[[paste0("wasModified_", i)]] <- 1
          } else {
            rv[[paste0("wasModified_", i)]] <- rv[[paste0("wasModified_", i)]] + 1L
          }
        })
      })
    }
  }
})

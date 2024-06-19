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
      enableGraphViewButton <- FALSE
      showRefreshGraphButton <- FALSE
      for (ii in i) {
        if (!is.null(configGraphsIn[[ii]])) {
          enableGraphViewButton <- TRUE
          if (modelInputGraphVisible[[ii]]) {
            if (length(configGraphsIn[[ii]]$additionalData)) {
              showRefreshGraphButton <- TRUE
              break
            }
          } else {
            # input graph can not be visible for only one of the symbols on the same tab
            break
          }
        }
      }
      if (enableGraphViewButton) {
        enableEl(session, "#btGraphIn")
      } else {
        disableEl(session, "#btGraphIn")
      }
      if (showRefreshGraphButton) {
        showEl(session, "#btRefreshGraphIn")
      } else {
        hideEl(session, "#btRefreshGraphIn")
      }
      return()
    }
  }
  if (is.null(configGraphsIn[[i]])) {
    hideEl(session, "#btRefreshGraphIn")
    disableEl(session, "#btGraphIn")
  } else {
    if (modelInputGraphVisible[[i]] && length(configGraphsIn[[i]]$additionalData)) {
      showEl(session, "#btRefreshGraphIn")
    } else {
      hideEl(session, "#btRefreshGraphIn")
    }
    enableEl(session, "#btGraphIn")
  }
})
renderInputGraph <- function(i) {
  if (is.null(configGraphsIn[[i]])) {
    return()
  }
  getInputDataset <- function(symName) {
    datasetTmp <- isolate(sandboxInputData$getData(symName)())
    if (length(modelIn[[symName]]$headers) > 0L) {
      if (identical(modelIn[[symName]]$type, "dropdown") &&
        !length(modelIn[[symName]]$definedByExternalSymbol)) {
        # multi dropdown
        datasetTmp <- ddToTibble(datasetTmp, modelIn[[symName]])
      }
      tryCatch(
        {
          datasetTmp <- fixColTypes(datasetTmp, modelIn[[symName]]$colTypes)
        },
        error_bad_format = function(e) {
          stop(sprintf(
            "Input data found for symbol: %s has wrong number of columns (expected: %d, actual: %d).",
            symName, nchar(modelIn[[symName]]$colTypes), length(datasetTmp)
          ), call. = FALSE)
        }
      )
    }
    names(datasetTmp) <- names(modelIn[[symName]]$headers)
    return(datasetTmp)
  }
  dataIds <- c(i, match(configGraphsIn[[i]]$additionalData, names(modelIn)))
  tryCatch(
    {
      if (length(dataIds) > 1L) {
        dsNames <- names(modelIn)[dataIds]
        data <- lapply(dsNames, function(dsName) {
          return(getInputDataset(dsName))
        })
        names(data) <- dsNames
      } else {
        data <- getInputDataset(names(modelIn)[[dataIds]])
      }
    },
    error = function(e) {
      flog.error(
        "Dataset: '%s' could not be loaded. Error message: '%s'.",
        modelInAlias[i], conditionMessage(e)
      )
      stop_custom("error_load_data", sprintf(
        lang$errMsg$GAMSInput$noData,
        modelInAlias[i]
      ), call. = FALSE)
    }
  )
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
          "Problems generating UI elements for chart for input dataset: '%s'. Error message: %s.",
          modelInAlias[i], conditionMessage(e)
        ))
        stop_custom("error_build_ui", sprintf(lang$errMsg$renderGraph$desc, modelInAlias[i]),
          call. = FALSE
        )
      }
    )
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
        customOptions = configGraphsIn[[i]]$options,
        roundPrecision = roundPrecision, modelDir = modelDir,
        rendererEnv = rendererEnv[[paste0("in_", i)]],
        views = views, attachments = attachments
      )
    },
    error = function(e) {
      flog.error(
        "Problems rendering input charts and/or tables for dataset: '%s'. Error message: %s.",
        modelInAlias[i], conditionMessage(e)
      )
      stop_custom("error_render_graph", sprintf(lang$errMsg$renderGraph$desc, modelInAlias[i]),
        call. = FALSE
      )
    }
  )
}
observeEvent(input$btRefreshGraphIn, {
  flog.debug("Button to refresh input graph clicked.")
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
    if (!modelInputGraphVisible[[i]]) {
      flog.error("Refresh input graph button clicked while graph view is not active. This should never happen and is likely an attempt to tamper with the app!")
    }
    tryCatch(renderInputGraph(i),
      error = function(e) {
        errMsg <<- paste(errMsg, conditionMessage(e), sep = "\n")
      }
    )
  })
  showErrorMsg(lang$errMsg$renderGraph$title, errMsg)
})
observeEvent(input$btGraphIn, {
  flog.debug("Button to toggle input view clicked.")
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
      hideEl(session, "#btRefreshGraphIn")
      return()
    } else {
      flog.debug("Graph view for model input in sheet: %d activated.", i)
      modelInputGraphVisible[[i]] <<- TRUE
      showEl(session, "#btRefreshGraphIn")
    }

    tryCatch(renderInputGraph(i),
      error = function(e) {
        errMsg <<- paste(errMsg, conditionMessage(e), sep = "\n")
      }
    )
  })
  showErrorMsg(lang$errMsg$renderGraph$title, errMsg)
})

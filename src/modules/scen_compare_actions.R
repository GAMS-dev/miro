observeEvent(input[["btScenTableView"]], {
  req(length(input[["btScenTableView"]]) == 1L)
  # get sheet ID for current scenario
  tabsetId <- suppressWarnings(as.integer(input[["btScenTableView"]]))
  if (is.na(tabsetId) || tabsetId < 1) {
    flog.error(
      "Problems switching table view for table ID: '%s'. This looks like an attempt to tamper with the app!",
      input[["btScenTableView"]]
    )
    return()
  }
  j <- suppressWarnings(as.integer(strsplit(isolate(input[[paste0("contentScen_", tabsetId)]]),
    "_",
    fixed = TRUE
  )[[1L]][[3L]]))
  if (is.na(j) || j < 1 || j > length(isGroupOfSheets)) {
    return(NULL)
  }
  if (isGroupOfSheets[[j]]) {
    j <- groupSheetToTabIdMap[[j]][[as.integer(strsplit(isolate(input[[paste0("contentScen_", tabsetId, "_", j)]]),
      "_",
      fixed = TRUE
    )[[1L]][[4L]])]]
  } else {
    j <- unlist(groupSheetToTabIdMap[[j]], use.names = FALSE)
  }
  flog.debug(
    "Table view in scenario with tabset id: %s for sheet: %s activated.", tabsetId,
    paste0(j, collapse = ", ")
  )
  if (isInCompareMode) {
    if (identical(currentCompMode, "split")) {
      for (k in 2:3) {
        for (jId in j) {
          toggleEl(session, paste0("#scenTable_", k, "_", jId))
          toggleEl(session, paste0("#scenGraph_", k, "_", jId))
        }
      }
    } else {
      for (k in 4:(maxNumberScenarios + 3)) {
        for (jId in j) {
          toggleEl(session, paste0("#scenTable_", k, "_", jId))
          toggleEl(session, paste0("#scenGraph_", k, "_", jId))
        }
      }
    }
  } else {
    for (jId in j) {
      toggleEl(session, paste0("#scenTable_", tabsetId, "_", jId))
      toggleEl(session, paste0("#scenGraph_", tabsetId, "_", jId))
    }
  }
})

# close scenario tab confirmed
closeCmpTab <- function(tabsetId) {
  removeTab("scenTabset", paste0("scen_", tabsetId, "_"))
  scenData$clear(tabIdToRef(tabsetId))
  dynamicUILoaded$dynamicTabsets[[paste0("tab_", tabsetId)]][["ui"]][] <<- FALSE
  dynamicUILoaded$dynamicTabsets[[paste0("tab_", tabsetId)]][["content"]][] <<- FALSE
  views$clearConf(tabsetId)
}
observeEvent(input[["btCmpTabCloseAll"]], {
  flog.debug("Close all scenarios button clicked (tab comparison mode)")
  removeModal()
  occupiedTabsetIds <- which(occupiedSidSlots) + 3L
  if (!length(occupiedTabsetIds)) {
    flog.error("Close all scenarios button clicked even though there are no tabset ids currently occupied. This should not happen and is likely an attempt to tamper with the app!")
    return()
  }
  for (tabsetId in occupiedTabsetIds) {
    closeCmpTab(tabsetId)
  }
  numberScenTabs <<- 0L
  occupiedSidSlots[] <<- FALSE
  hideEl(session, "#btCmpTabCloseAll")
  showEl(session, "#cmpTabNoScenWrapper")
  disableEl(session, "#btCompareScen")
})
observeEvent(input[["btScenClose"]], {
  tabsetId <- suppressWarnings(as.integer(input[["btScenClose"]]))
  if (is.na(tabsetId) || tabsetId < 1) {
    flog.error(
      "Problems closing scenario with ID: '%s'. This looks like an attempt to tamper with the app!",
      input[["btScenClose"]]
    )
    return()
  }
  flog.debug("Close scenario '%d' button clicked", tabsetId)
  removeModal()

  closeCmpTab(tabsetId)
  numberScenTabs <<- numberScenTabs - 1
  occupiedSidSlots[tabsetId - 3] <<- FALSE
  if (!numberScenTabs) {
    hideEl(session, "#btCmpTabCloseAll")
    showEl(session, "#cmpTabNoScenWrapper")
  } else if (numberScenTabs == 1) {
    disableEl(session, "#btCompareScen")
  }
})

# export scenario data
output[["scenExportHandler"]] <- downloadHandler(
  contentType = tryCatch(
    {
      exportFileType <- getExportFileType(input$exportFileType, datasetsRemoteExport)
      exportFileType$contentType
    },
    error_bad_type = function(e) {
      return("text/plain")
    }
  ),
  filename = function() {
    tabsetId <- suppressWarnings(as.integer(input[["scenExportId"]]))
    if (is.na(tabsetId) || tabsetId < 1L) {
      flog.error(
        "Problems exporting scenario with ID: '%s'. This looks like an attempt to tamper with the app!",
        input[["scenExportId"]]
      )
      return("error.txt")
    }
    exportFileType <- tryCatch(getExportFileType(input$exportFileType, datasetsRemoteExport),
      error_bad_type = function(e) {
        list(fileName = "error.txt")
      }
    )
    if (!is.null(exportFileType$fileName)) {
      return(exportFileType$fileName)
    }
    isolate({
      fileExt <- exportFileType$fileExt
      if (identical(fileExt, "csv")) {
        if (isTRUE(input$cbSelectManuallyExp)) {
          if (length(input$selDataToExport) > 1L) {
            fileExt <- "zip"
          }
        } else if (length(modelOut) + length(inputDsNames) > 1L) {
          fileExt <- "zip"
        }
      }
      if (tabsetId == 1) {
        # active scenario (editable)
        return(paste0(modelName, "_", activeScen$getScenName(), ".", fileExt))
      }
      fileName <- paste0(
        modelName, "_",
        scenData$getById("meta", refId = tabIdToRef(tabsetId), drop = TRUE)[["_sname"]][1],
        ".", fileExt
      )
      flog.debug("File: '%s' was downloaded.", fileName)
    })
    return(fileName)
  },
  content = function(file) {
    tabsetId <- suppressWarnings(as.integer(input[["scenExportId"]]))
    if (is.na(tabsetId) || tabsetId < 1L) {
      flog.error(
        "Problems exporting scenario with ID: '%s'. This looks like an attempt to tamper with the app!",
        input[["scenExportId"]]
      )
      return(downloadHandlerError(file, lang$errMsg$unknownError))
    }
    dsToExport <- NULL

    exportFileType <- tryCatch(getExportFileType(input$exportFileType, datasetsRemoteExport),
      error_bad_type = function(e) {
        flog.error(
          "Problems determining export file type: %s. This looks like an attempt to tamper with the app!",
          conditionMessage(e)
        )
        return(list(isError = TRUE))
      }
    )

    if (isTRUE(exportFileType$isError)) {
      return(downloadHandlerError(file, lang$errMsg$unknownError))
    }

    if (exportFileType$isCustom) {
      # custom export function
      customDataIO$setConfig(datasetsRemoteExport[[exportFileType$customExporterId]])
      if (length(datasetsRemoteExport[[exportFileType$customExporterId]][["symNames"]])) {
        # custom export function defined only for certain data sets
        dsToExport <- c(names(modelOut), inputDsNames)
        dsToExport <- dsToExport[dsToExport %in% datasetsRemoteExport[[exportFileType$customExporterId]][["symNames"]]]
      }
      exportFileTypeId <- exportFileType$customExporterId
    } else {
      exportFileTypeId <- exportFileType$fileExt
    }

    if (isTRUE(input$cbSelectManuallyExp)) {
      if (is.null(dsToExport)) {
        dsToExport <- c(names(modelOut), inputDsNames)
      }
      dsToExport <- dsToExport[dsToExport %in% input$selDataToExport]

      if (!length(dsToExport)) {
        flog.info("No datasets selected. Nothing will be exported.")
        showElReplaceTxt(session, "#scenExportError", lang$nav$dialogExportScen$noDsSelected)
        return(downloadHandlerError(file, lang$nav$dialogExportScen$noDsSelected))
      }
    }

    if (tabsetId == 1) {
      # active scenario (editable)
      errMsg <- tryCatch(
        {
          scenData$loadSandbox(
            getInputDataFromSandbox(),
            if (length(modelInFileNames)) modelInFileNames else character(),
            activeScen$getMetadataDf()
          )
          NULL
        },
        no_data = function(e) {
          flog.error(conditionMessage(e))
          return(conditionMessage(e))
        },
        error = function(e) {
          flog.error("Unexpected error while fetching input data from sandbox. Error message: '%s'", conditionMessage(e))
          return(lang$errMsg$unknownError)
        }
      )
      if (!is.null(errMsg)) {
        return(downloadHandlerError(file, errMsg))
      }
    }
    refId <- tabIdToRef(tabsetId)
    if (identical(refId, "cmpPivot")) {
      stop("not implemented", call. = FALSE)
    }
    data <- scenData$get(refId, includeHiddenScalars = TRUE)
    suppressRemoveModal <- FALSE
    if (identical(scenData$getById("dirty", refId = refId, drop = TRUE), TRUE)) {
      showElReplaceTxt(session, "#scenExportError", lang$errMsg$loadScen$inconsistentDataWarning)
      suppressRemoveModal <- TRUE
    }
    if (scalarsOutName %in% names(data)) {
      data[[scalarsOutName]] <- scenData$getScalars(refId, outputScalarsOnly = TRUE)
    }


    if (!is.null(dsToExport)) {
      data <- data[names(data) %in% dsToExport]
    }

    return(exportScenario(file, data, exportFileTypeId, refId, tabsetId, attachments, views,
      scenData, xlsio,
      suppressRemoveModal = suppressRemoveModal, session = session,
      excelConfig = list(
        includeMeta = config$excelIncludeMeta,
        includeEmpty = config$excelIncludeEmptySheets
      ), customDataIO = customDataIO,
      sandboxScenario = if (tabsetId == 1) activeScen else NULL
    ))
  }
)
observeEvent(input[["scenRemoteExportHandler"]], {
  flog.debug("Remote export button clicked.")
  hideEl(session, "#scenExportError")
  tabsetId <- suppressWarnings(as.integer(input[["scenExportId"]]))
  if (is.na(tabsetId) || tabsetId < 1L) {
    flog.error(
      "Problems exporting scenario with ID: '%s'. This looks like an attempt to tamper with the app!",
      input[["scenExportId"]]
    )
    return()
  }
  exportFileType <- tryCatch(getExportFileType(input$exportFileType, datasetsRemoteExport),
    error_bad_type = function(e) {
      flog.error(
        "Problems determining export file type: %s. This looks like an attempt to tamper with the app!",
        conditionMessage(e)
      )
      return(list(isError = TRUE))
    }
  )

  if (isTRUE(exportFileType$isError)) {
    return()
  }

  if (is.null(exportFileType$customExporterId)) {
    flog.error("Remote export button clicked, but selected exporter is not a remote exporter. This looks like an attempt to tamper with the app!")
    return()
  }

  expConfig <- datasetsRemoteExport[[exportFileType$customExporterId]]
  if (length(expConfig$localFileOutput)) {
    flog.error("Remote export button clicked, but selected exporter is custom exporter with local file output. This looks like an attempt to tamper with the app!")
    return()
  }

  dsToExport <- NULL

  if (length(expConfig$functionName) &&
    length(expConfig$symNames)) {
    dsToExport <- c(names(modelOut), inputDsNames)
    dsToExport <- dsToExport[dsToExport %in% expConfig$symNames]
  } else {
    # old config (remoteExport)
    # FIXME: remove when removing remoteExport
    dsToExport <- c(names(modelOut), inputDsNames)
    dsToExport <- dsToExport[dsToExport %in% names(expConfig)]
  }

  if (isTRUE(input$cbSelectManuallyExp)) {
    if (is.null(dsToExport)) {
      dsToExport <- c(names(modelOut), inputDsNames)
    }
    dsToExport <- dsToExport[dsToExport %in% input$selDataToExport]

    if (!length(dsToExport)) {
      flog.info("No datasets selected. Nothing will be exported.")
      showElReplaceTxt(session, "#scenExportError", lang$nav$dialogExportScen$noDsSelected)
      return()
    }
  }

  prog <- Progress$new()
  on.exit(suppressWarnings(prog$close()))
  prog$set(message = lang$progressBar$exportScen$title, value = 0.2)
  if (tabsetId == 1) {
    # active scenario (editable)
    if (tryCatch(
      {
        scenData$loadSandbox(
          getInputDataFromSandbox(),
          if (length(modelInFileNames)) modelInFileNames else character(),
          activeScen$getMetadataDf()
        )
        FALSE
      },
      no_data = function(e) {
        flog.error(conditionMessage(e))
        showElReplaceTxt(session, "#scenExportError", conditionMessage(e))
        return(TRUE)
      },
      error = function(e) {
        flog.error("Unexpected error while fetching input data from sandbox. Error message: '%s'", conditionMessage(e))
        showElReplaceTxt(session, "#scenExportError", lang$errMsg$unknownError)
        return(TRUE)
      }
    )) {
      return()
    }
  }
  refId <- tabIdToRef(tabsetId)
  if (identical(refId, "cmpPivot")) {
    stop("not implemented", call. = FALSE)
  }
  suppressRemoveModal <- FALSE
  data <- scenData$get(refId, includeHiddenScalars = TRUE)
  if (identical(scenData$getById("dirty", refId = refId, drop = TRUE), TRUE)) {
    showElReplaceTxt(session, "#scenExportError", lang$errMsg$loadScen$inconsistentDataWarning)
    suppressRemoveModal <- TRUE
  }

  if (scalarsOutName %in% names(data) && length(config$hiddenOutputScalars)) {
    # we want to export hidden output scalars as well
    data[[scalarsOutName]] <- scenData$getScalars(refId, outputScalarsOnly = TRUE)
  }

  errMsg <- NULL
  customDataIO$setConfig(expConfig)

  if (!is.null(dsToExport)) {
    data <- data[names(data) %in% dsToExport]
  }
  tryCatch(
    customDataIO$write(data,
      sandboxScenario = if (tabsetId == 1) activeScen else NULL
    ),
    error_custom = function(e) {
      flog.debug(
        "Custom exporter: %s reported a custom error: %s",
        input$exportFileType,
        conditionMessage(e)
      )
      errMsg <<- conditionMessage(e)
    },
    error = function(e) {
      flog.warn(
        "Problems exporting data (export name: '%s'). Error message: '%s'.",
        input$exportFileType, conditionMessage(e)
      )
      errMsg <<- lang$errMsg$saveScen$desc
    }
  )
  if (is.null(showErrorMsg(lang$errMsg$saveScen$title, errMsg))) {
    return()
  }
  flog.debug("Data exported successfully.")
  if (!suppressRemoveModal) {
    removeModal()
  }
  return()
})

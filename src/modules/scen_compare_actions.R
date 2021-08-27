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
  sidCompOrder <<- NULL
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
  sidCompOrder <<- sidCompOrder[-which(sidCompOrder == tabsetId)]
  if (!numberScenTabs) {
    hideEl(session, "#btCmpTabCloseAll")
    showEl(session, "#cmpTabNoScenWrapper")
  } else if (numberScenTabs == 1) {
    disableEl(session, "#btCompareScen")
  }
})

# export scenario data
output[["scenExportHandler"]] <- downloadHandler(
  filename = function() {
    tabsetId <- suppressWarnings(as.integer(input[["scenExportId"]]))
    if (is.na(tabsetId) || tabsetId < 1L) {
      flog.error(
        "Problems exporting scenario with ID: '%s'. This looks like an attempt to tamper with the app!",
        input[["scenExportId"]]
      )
      return()
    }
    isolate({
      fileExt <- exportFileType
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
        if (is.null(isolate(rv$activeSname))) {
          # as no scenario name could be found, set scenario name to model name
          return(paste0(modelName, ".", fileExt))
        } else {
          return(paste0(modelName, "_", isolate(rv$activeSname), ".", fileExt))
        }
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
    prog <- Progress$new()
    on.exit(suppressWarnings(prog$close()))
    prog$set(message = lang$progressBar$exportScen$title, value = 0.1)

    tabsetId <- suppressWarnings(as.integer(input[["scenExportId"]]))
    if (is.na(tabsetId) || tabsetId < 1L) {
      flog.error(
        "Problems exporting scenario with ID: '%s'. This looks like an attempt to tamper with the app!",
        input[["scenExportId"]]
      )
      return(downloadHandlerError(file, lang$errMsg$unknownError))
    }

    if (tabsetId == 1) {
      # active scenario (editable)
      errMsg <- tryCatch(
        {
          scenData$loadSandbox(
            getInputDataFromSandbox(),
            modelInFileNames, activeScen$getMetadata()
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

    if (isTRUE(input$cbSelectManuallyExp)) {
      outputDataToExport <- names(modelOut)[names(modelOut) %in% input$selDataToExport]
      inputDataToExport <- inputDsNames[inputDsNames %in% input$selDataToExport]

      if (!length(outputDataToExport) && !length(inputDataToExport)) {
        flog.info("No datasets selected. Nothing will be exported.")
        showElReplaceTxt(session, "#scenExportError", lang$nav$dialogExportScen$noDsSelected)
        return(downloadHandlerError(file, lang$nav$dialogExportScen$noDsSelected))
      }
      data <- data[names(data) %in% c(outputDataToExport, inputDataToExport)]
    } else {
      outputDataToExport <- names(modelOut)
      inputDataToExport <- inputDsNames
    }
    noDatasets <- length(data)
    prog$set(value = 0.2)

    if (identical(exportFileType, "gdx")) {
      return(tryCatch(
        {
          gdxio$wgdx(file, data, squeezeZeros = "n")
          if (!suppressRemoveModal) {
            removeModal()
          }
        },
        error_duplicate_records = function(e) {
          flog.info("Duplicate records found when writing GDX file: %s", conditionMessage(e))
          showElReplaceTxt(session, "#scenExportError", conditionMessage(e))
          downloadHandlerError(file, conditionMessage(e))
        }
      ))
    }
    if (identical(exportFileType, "miroscen")) {
      return(tryCatch(
        {
          generateMiroScen(
            file, scenData$getById("meta", refId = refId, drop = TRUE),
            data, attachments, views, tabsetId
          )
          if (!suppressRemoveModal) {
            removeModal()
          }
        },
        error_duplicate_records = function(e) {
          flog.info(
            "Duplicate records found when writing GDX file (in miroscen file): %s",
            conditionMessage(e)
          )
          showElReplaceTxt(session, "#scenExportError", conditionMessage(e))
          downloadHandlerError(file, conditionMessage(e))
        },
        error = function(e) {
          flog.info("Unexpected error while creating miroscen file for export: %s", conditionMessage(e))
          showElReplaceTxt(session, "#scenExportError", lang$errMsg$unknownError)
          downloadHandlerError(file, lang$errMsg$unknownError)
        }
      ))
    }
    if (identical(exportFileType, "csv")) {
      if (!suppressRemoveModal) {
        removeModal()
      }
      if (length(data) == 0L) {
        return(readr::write_csv(tibble(), file))
      } else if (length(data) == 1L) {
        return(readr::write_csv(data[[1L]], file))
      }
      tmpDir <- file.path(tempdir(), paste0(uid, "_exp_tmp_dir"))
      if (file.exists(tmpDir) && !identical(unlink(tmpDir, recursive = TRUE), 0L)) {
        flog.error("Could not remove temporary directory: '%s'.", tmpDir)
        return(downloadHandlerError(file, "Directory could not be removed"))
      }
      if (!dir.create(tmpDir, recursive = TRUE)) {
        flog.error("Could not create temporary directory: '%s'.", tmpDir)
        return(downloadHandlerError(file, "Directory could not be created"))
      }
      on.exit(unlink(tmpDir, recursive = TRUE), add = TRUE)
      for (i in seq_along(data)) {
        dsName <- names(data)[i]
        prog$inc(amount = 0.8 / noDatasets, detail = sprintf(lang$progressBar$exportScen$exportDs, i, noDatasets))
        readr::write_csv(data[[dsName]], file.path(tmpDir, paste0(dsName, ".csv")))
      }
      return(suppressWarnings(zip::zipr(file, list.files(tmpDir, full.names = TRUE),
        recurse = FALSE, include_directories = FALSE
      )))
    }
    if (!suppressRemoveModal) {
      removeModal()
    }
    return(xlsio$write(file, data, scenData$getById("meta", refId = tabIdToRef(tabsetId), drop = TRUE),
      includeMetadataSheet = config$excelIncludeMeta,
      includeEmptySheets = config$excelIncludeEmptySheets
    ))
  }
)
observeEvent(input[["scenRemoteExportHandler"]], {
  hideEl(session, "#scenExportError")
  tabsetId <- suppressWarnings(as.integer(input[["scenExportId"]]))
  if (is.na(tabsetId) || tabsetId < 1L) {
    flog.error(
      "Problems exporting scenario with ID: '%s'. This looks like an attempt to tamper with the app!",
      input[["scenExportId"]]
    )
    return()
  }
  if (!length(datasetsRemoteExport) || !length(input$exportFileType)) {
    flog.error(
      "Remote export button clicked but export file type: '%s' does not exist.",
      input$exportFileType
    )
    return()
  }
  exportId <- match(input$exportFileType, names(datasetsRemoteExport))[[1L]]
  if (!is.na(exportId)) {
    expConfig <- datasetsRemoteExport[[input$exportFileType]]
    dsToExport <- names(expConfig)

    if (isTRUE(input$cbSelectManuallyExp)) {
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
            modelInFileNames, activeScen$getMetadata()
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

    noDatasets <- length(dsToExport)
    errMsg <- NULL
    for (expId in seq_along(dsToExport)) {
      dataId <- match(dsToExport[expId], names(data))
      if (is.na(dataId)) {
        next
      }
      dsName <- names(data)[dataId]
      prog$inc(
        amount = 0.8 / noDatasets,
        detail = sprintf(
          lang$progressBar$exportScen$exportDs,
          expId, noDatasets
        )
      )
      tryCatch(dataio$export(data[[dsName]], expConfig[[expId]], dsName),
        error = function(e) {
          flog.warn(
            "Problems exporting data (export name: '%s', dataset: '%s'). Error message: '%s'.",
            input$exportFileType, dsName, conditionMessage(e)
          )
          errMsg <<- lang$errMsg$saveScen$desc
        }
      )
      if (is.null(showErrorMsg(lang$errMsg$saveScen$title, errMsg))) {
        break
      }
    }
    flog.debug("Data exported successfully.")
    if (!suppressRemoveModal) {
      removeModal()
    }
    return()
  }
})

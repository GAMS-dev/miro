# load input data from local file
observe({
  if (!is.character(input$selExcelIndexSheet) ||
    length(input$selExcelIndexSheet) != 1L ||
    is.null(input$excelIndexSheetRng)) {
    return()
  }
  hideEl(session, "#localDataImportError")
  if (identical(input$selExcelIndexSheet, "-")) {
    return(enableEl(session, "#btImportLocal"))
  }
  disableEl(session, "#btImportLocal")
  range <- input$excelIndexSheetRng
  if (tryCatch(
    {
      cellranger::as.cell_limits(range)
      FALSE
    },
    warning = function(w) {
      flog.debug("Invalid index cell range entered while importing spreadsheet")
      return(TRUE)
    },
    error = function(e) {
      flog.debug("Invalid index cell range entered while importing spreadsheet")
      return(TRUE)
    }
  )) {
    showElReplaceTxt(session, "#localDataImportError", lang$errMsg$xlsio$errors$badIndexRange)
    return()
  }
  tryCatch(
    {
      xlsio$readIndex(input$localInput$datapath,
        indexRange = paste0('"', input$selExcelIndexSheet, "!", range, '"'),
        forceInit = TRUE
      )
      rWarn <- xlsio$getWarnings()
      if (length(rWarn)) {
        showElReplaceTxt(session, "#localDataImportError", paste(rWarn, collapse = "\n"))
      }
      enableEl(session, "#btImportLocal")
    },
    error_parse_config = function(e) {
      flog.info("Problems parsing index sheet (local Excel import). Error message: %s", conditionMessage(e))
      showElReplaceTxt(session, "#localDataImportError", conditionMessage(e))
    },
    error = function(e) {
      flog.warn("Problems parsing index sheet (local Excel import). Error message: %s", conditionMessage(e))
      showElReplaceTxt(session, "#localDataImportError", lang$errMsg$unknownError)
    }
  )
})
output$csvHeaderMapping <- renderUI({
  req(rv$importCSV, input$selInputDataLocCSV)
  symSelected <- input$selInputDataLocCSV
  if (!is.character(symSelected) ||
    length(symSelected) != 1L ||
    !symSelected %in% c(names(ioConfig$modelInRaw), ioConfig$hcubeScalars)) {
    flog.error("selInputDataLocCSV has bad format. This is most likely because the user is trying to tamper with the app!")
    return(list())
  }
  if (length(csvio$getRDelim()) > 1L) {
    req(input$csvDelim)
    csvio$setRDelim(input$csvDelim)
  }
  if (symSelected %in% ioConfig$hcubeScalars) {
    symHeaders <- ioConfig$modelIn[[symSelected]]$headers
  } else {
    symHeaders <- ioConfig$modelInRaw[[symSelected]]$headers
  }
  csvHeaders <- csvio$getHeaders()
  return(lapply(seq_along(symHeaders), function(hdrIdx) {
    column(
      6L,
      selectInput(paste0("csvInputHdr_", hdrIdx),
        label = symHeaders[[hdrIdx]]$alias,
        choices = c("-", csvHeaders),
        selected = if (hdrIdx > length(csvHeaders)) "-" else csvHeaders[hdrIdx]
      )
    )
  }))
})
observeEvent(input$localInput, {
  hideEl(session, "#localDataImportError")
  fileExt <- tolower(tools::file_ext(basename(input$localInput$datapath)))
  if (length(fileExt) != 1L) {
    flog.error("Bad file extension format. This looks like an attempt to tamper with the app!")
    return(showElReplaceTxt(session, "#localDataImportError", lang$errMsg$unknownError))
  }
  flog.debug("New local file with extension: %s uploaded", fileExt)
  if (useGdx && identical(fileExt, "miroscen")) {
    hideEl(session, "#localInputSelectManually")
  } else if (fileExt %in% csvio$getValidExtensions()) {
    hideEl(session, "#localInputSelectManually")
  } else {
    showEl(session, "#localInputSelectManually")
  }
  if (fileExt %in% xlsio$getValidExtensions()) {
    showEl(session, "#localInputExcelOptions")
    hideEl(session, "#localInputCsvOptions")
    disableEl(session, "#btImportLocal")
    tryCatch(
      {
        xlsio$rInitFile(input$localInput$datapath)
        xlsSheets <- xlsio$getSheetNames()
        if ("_index" %in% xlsSheets) {
          selectedIndex <- "_index"
        } else {
          selectedIndex <- "-"
          enableEl(session, "#btImportLocal")
        }
        updateSelectInput(session, "selExcelIndexSheet",
          choices = c("-", xlsSheets),
          selected = selectedIndex
        )
      },
      error_bad_format = function(e) {
        flog.info("Problems initializing excel file to read. Error message: %s", conditionMessage(e))
        showElReplaceTxt(session, "#localDataImportError", conditionMessage(e))
      },
      error = function(e) {
        flog.warn("Problems initializing excel file to read. Error message: %s", conditionMessage(e))
        showElReplaceTxt(session, "#localDataImportError", lang$errMsg$unknownError)
      }
    )
    return()
  } else if (fileExt %in% csvio$getValidExtensions()) {
    hideEl(session, "#localInputExcelOptions")
    showEl(session, "#localInputCsvOptions")
    disableEl(session, "#btImportLocal")
    if (!length(modelInToImport)) {
      return()
    }
    if (!identical(length(input$selInputDataLocCSV), 1L)) {
      showHideEl(session, "#importScenError", 4000L)
      flog.error("selInputDataLocCSV has invalid format. This looks like an attempt to tamper with the app!")
      return()
    }
    tryCatch(
      {
        symId <- match(
          tolower(tools::file_path_sans_ext(input$localInput$name)),
          c(names(ioConfig$modelInRaw), ioConfig$hcubeScalars)
        )
        if (!is.na(symId)) {
          updateSelectInput(session, "selInputDataLocCSV",
            selected = c(names(ioConfig$modelInRaw), ioConfig$hcubeScalars)[symId]
          )
        }
        csvio$rInitFile(input$localInput$datapath, needDelim = FALSE)
        rv$importCSV <- rv$importCSV + 1L
        hideEl(session, "#csvDelimWrapper")
        enableEl(session, "#btImportLocal")
      },
      error_bad_delim = function(e) {
        flog.info("Problems detecting delimiter. Error message: %s", conditionMessage(e))
        showElReplaceTxt(session, "#localDataImportError", conditionMessage(e))
      },
      error_bad_encoding = function(e) {
        flog.info("Invalid encoding detected. Error message: %s", conditionMessage(e))
        showElReplaceTxt(session, "#localDataImportError", conditionMessage(e))
      },
      error_ambiguous_delim = function(e) {
        flog.debug("Delimiter could not be uniquely identified.")
        rv$importCSV <- rv$importCSV + 1L
        showElReplaceTxt(
          session, "#localDataImportError",
          lang$errMsg$csvio$warnings$ambiguousDelim
        )
        updateSelectInput(session, "csvDelim", choices = csvio$getRDelim())
        showEl(session, "#csvDelimWrapper")
        enableEl(session, "#btImportLocal")
      },
      error = function(e) {
        flog.warn("Problems initializing delimited value file. Error message: %s", conditionMessage(e))
        showElReplaceTxt(session, "#localDataImportError", lang$errMsg$unknownError)
      }
    )
    return()
  } else {
    hideEl(session, "#localInputExcelOptions")
    hideEl(session, "#localInputCsvOptions")
    if (identical(fileExt, "zip") ||
      (useGdx && fileExt %in% c("miroscen", "gdx"))) {
      enableEl(session, "#btImportLocal")
      return()
    }
    disableEl(session, "#btImportLocal")
    showElReplaceTxt(
      session, "#localDataImportError",
      sprintf(
        lang$errMsg$invalidFileType$desc,
        paste0(c(
          "zip", xlsio$getValidExtensions(), csvio$getValidExtensions(),
          if (useGdx) c("miroscen", "gdx")
        ),
        collapse = ","
        )
      )
    )
  }
})
observeEvent(input$btImportLocal, {
  if (identical(config$activateModules$loadLocal, FALSE)) {
    flog.error("Try to load local data even though the loadLocal module is disabled! This is most likely because the user is trying to tamper with the app!")
    return()
  }
  if (!length(input$localInput$datapath)) {
    return()
  }
  flog.debug("Load local data button clicked.")

  fileExt <- tolower(tools::file_ext(basename(input$localInput$datapath)))
  if (length(fileExt) != 1L) {
    flog.error("Bad file extension format. This looks like an attempt to tamper with the app!")
    return(showElReplaceTxt(session, "#localDataImportError", lang$errMsg$unknownError))
  }
  if (fileExt %in% csvio$getValidExtensions()) {
    decimalSep <- input$csvDecimalSep
    if (!is.character(decimalSep) || length(decimalSep) != 1L) {
      flog.error("csvDecimalSep has bad format. This is most likely because the user is trying to tamper with the app!")
      showHideEl(session, "#importScenError", 4000L)
      return()
    }
    symToFetch <- input$selInputDataLocCSV
    idsToFetch <- match(symToFetch, names(modelIn))
    if (!identical(length(idsToFetch), 1L) ||
      is.na(idsToFetch)) {
      if (identical(symToFetch, scalarsFileName)) {
        # all scalars
        idsToFetch <- seq_along(modelIn)[vapply(modelIn, function(inSym) {
          return(is.null(inSym$headers))
        }, logical(1L), USE.NAMES = FALSE)]
      } else {
        flog.error("selInputDataLocCSV has invalid format. This looks like an attempt to tamper with the app!")
        showElReplaceTxt(session, "#localDataImportError", lang$errMsg$unknownError)
        return()
      }
    }
    if (symToFetch %in% ioConfig$hcubeScalars) {
      colsToRead <- vapply(seq_along(ioConfig$modelIn[[symToFetch]]$headers), function(hdrIdx) {
        return(input[[paste0("csvInputHdr_", hdrIdx)]])
      }, character(1L), USE.NAMES = FALSE)
    } else {
      colsToRead <- vapply(seq_along(ioConfig$modelInRaw[[symToFetch]]$headers), function(hdrIdx) {
        return(input[[paste0("csvInputHdr_", hdrIdx)]])
      }, character(1L), USE.NAMES = FALSE)
    }
    if (any(duplicated(colsToRead[colsToRead != "-"]))) {
      flog.info("Duplicate columns to read detected when uploading CSV file.")
      return(showElReplaceTxt(
        session, "#localDataImportError",
        lang$errMsg$csvio$errors$duplicateCol
      ))
    }
    if (length(csvio$getRDelim) > 1L) {
      csvio$setRDelim(input$csvDelim)
    }
    if (all(colsToRead == "-")) {
      flog.info("No columns to import have been selected.")
      return(showElReplaceTxt(
        session, "#localDataImportError",
        lang$errMsg$csvio$errors$noColsSelected
      ))
    }
    csvio$
      setRSymName(symToFetch)$
      setDecimalSep(decimalSep)$
      setColsToRead(colsToRead, symToFetch)
  } else {
    # check whether current input datasets are empty
    if (input$cbSelectManuallyLoc && length(input$selInputDataLoc)) {
      idsToFetch <- match(tolower(input$selInputDataLoc), names(modelIn))
      # remove NAs
      idsToFetch <- idsToFetch[!is.na(idsToFetch)]
    } else {
      idsToFetch <- seq_along(modelIn)
    }
  }

  datasetsImported <- vapply(idsToFetch, function(i) {
    if (length(isolate(rv[[paste0("in_", i)]]))) {
      return(TRUE)
    } else {
      return(FALSE)
    }
  }, logical(1L))

  if (any(datasetsImported)) {
    hideEl(session, "#importDataTabset")
    fileType <- tolower(tools::file_ext(basename(input$localInput$datapath)))
    if (identical(fileType, "miroscen")) {
      showEl(session, "#btOverwriteScenLocal")
      showEl(session, "#importDataClearSandbox")
    } else {
      showEl(session, "#btReplaceInputData")
      showEl(session, "#btMergeInputData")
      showEl(session, "#importDataOverwrite")
    }
  } else {
    overwriteInput <<- 0L
    rv$btOverwriteInput <<- rv$btOverwriteInput + 1L
  }
})

observeEvent(input$btReplaceInputData, {
  flog.debug("Replace input data button clicked.")
  overwriteInput <<- 1L
  rv$btOverwriteInput <<- rv$btOverwriteInput + 1L
})

observeEvent(input$btMergeInputData, {
  flog.debug("Merge input data button clicked.")
  overwriteInput <<- 2L
  rv$btOverwriteInput <<- rv$btOverwriteInput + 1L
})

observeEvent(input$btOverwriteScenLocal, {
  flog.debug("Clear input data button clicked.")
  overwriteInput <<- 1L
  rv$btOverwriteInput <<- rv$btOverwriteInput + 1L
})

observeEvent(virtualActionButton(rv$btOverwriteInput), {
  if (identical(input$tb_importData, "tb_importData_external")) {
    # import via custom function
    externalSource <- input$selExternalSource
    if (!length(externalSource) ||
      !externalSource %in% names(externalInputConfig)) {
      flog.info(
        "Invalid remote importer: '%s'. This should not happen! Possible attempt of user tampering with the app!",
        externalSource
      )
      return(NULL)
    }
    extConf <- externalInputConfig[[externalSource]]

    if (length(externalInputConfig[[externalSource]]$functionName)) {
      if (length(externalInputConfig[[externalSource]]$symNames)) {
        datasetsToFetch <- externalInputConfig[[externalSource]]$symNames
      } else {
        datasetsToFetch <- c(inputDsNames, names(modelOut))
      }
    } else {
      # old config (remoteImport)
      # FIXME: remove when removing remoteImport
      datasetsToFetch <- names(externalInputConfig[[externalSource]])
    }
    if (input$cbSelectManuallyExt) {
      if (!length(input$selInputDataExt)) {
        return()
      }
      datasetsToFetch <- datasetsToFetch[datasetsToFetch %in%
        tolower(input$selInputDataExt)]
    }
    extConf$datasetsToFetch <- datasetsToFetch
    extConf$label <- externalSource
    customDataIO$setConfig(extConf)
    loadModeFileName <- NULL
    loadModeWorkDir <- NULL
    fileType <- "_ext_"
  } else {
    if (is.null(input$localInput$datapath)) {
      return(NULL)
    }
    if (identical(config$activateModules$loadLocal, FALSE)) {
      flog.error("Try to load local data even though the loadLocal module is disabled! This is most likely because the user is trying to tamper with the app!")
      return()
    }
    loadModeFileName <- basename(input$localInput$datapath)
    loadModeWorkDir <- dirname(input$localInput$datapath)
    fileType <- tolower(tools::file_ext(loadModeFileName))
  }

  # initialize new imported sheets counter
  newInputCount <- 0L
  errMsg <- NULL
  scalarDataset <- NULL

  prog <- Progress$new()
  on.exit(suppressWarnings(prog$close()))
  prog$set(message = lang$progressBar$importScen$title, value = 0.1)

  dfClArgs <- NULL

  if (identical(fileType, "miroscen") && useGdx) {
    resetWidgetsOnClose <<- FALSE
    if (!closeScenario(clearMeta = TRUE)) {
      showHideEl(session, "#importScenError", 4000L)
      return()
    }
    if (!tryCatch(validateMiroScen(input$localInput$datapath), error = function(e) {
      flog.info("Invalid miroscen file. Error message: '%s'.", conditionMessage(e))
      showHideEl(session, "#importScenInvalidFile", 4000L)
      return(FALSE)
    })) {
      return()
    }
    dfClArgs <- tryCatch(loadMiroScen(
      input$localInput$datapath, activeScen, attachments, views,
      names(modelIn)
    ),
    error = function(e) {
      showHideEl(session, "#importScenError", 4000L)
      flog.info(
        "Problems reading miroscen file. Error message: '%s'.",
        conditionMessage(e)
      )
      return(FALSE)
    }
    )
    if (isFALSE(dfClArgs)) {
      return()
    }
    loadModeFileName <- "data.gdx"
    loadMode <- "gdx"
    datasetsToFetch <- names(modelIn)
  } else if (identical(fileType, "gdx") && useGdx) {
    loadMode <- "gdx"
    datasetsToFetch <- names(modelIn)
  } else if (identical(fileType, "zip")) {
    loadMode <- "csv"
    csvFiles <- tryCatch(
      getValidCsvFromZip(
        input$localInput$datapath,
        c(
          names(modelOut),
          inputDsNames
        ), uid
      ),
      error = function(e) {
        errMsg <- conditionMessage(e)
        if (startsWith(errMsg, "e:")) {
          showHideEl(session, "#importScenError", 4000L)
          flog.error(errMsg)
        } else {
          flog.info(errMsg)
          showHideEl(session, "#importScenInvalidFile", 4000L)
        }
        return("e")
      }
    )
    if (identical(csvFiles, "e")) {
      return()
    }
    on.exit(
      {
        if (identical(unlink(csvFiles$tmpDir, recursive = TRUE), 0L)) {
          flog.debug("Temporary directory: '%s' removed.", csvFiles$tmpDir)
        } else {
          flog.error("Problems removing temporary directory: '%s'.", csvFiles$tmpDir)
        }
      },
      add = TRUE
    )
    datasetsToFetch <- substr(
      csvFiles$validFileNames, 1L,
      nchar(csvFiles$validFileNames) - 4L
    )
    loadModeWorkDir <- csvFiles$tmpDir
  } else if (fileType %in% csvio$getValidExtensions()) {
    loadMode <- "scsv"
    datasetsToFetch <- csvio$getRSymName()
    if (!identical(input$selInputDataLocCSV, datasetsToFetch)) {
      flog.error("selInputDataLocCSV has invalid format. This looks like an attempt to tamper with the app!")
      showHideEl(session, "#importScenError", 4000L)
      return()
    }
  } else if (fileType %in% xlsio$getValidExtensions()) {
    loadMode <- "xls"
    datasetsToFetch <- xlsio$getSymbolNames()
  } else if (identical(fileType, "_ext_")) {
    loadMode <- "custom"
  } else {
    removeModal()
    showErrorMsg(
      lang$errMsg$invalidFileType$title,
      sprintf(
        lang$errMsg$invalidFileType$desc,
        paste0(c(
          "zip", xlsio$getValidExtensions(), csvio$getValidExtensions(),
          if (useGdx) c("miroscen", "gdx")
        ), collapse = ",")
      )
    )
    flog.info("Invalid file type: '%s' attempted to be imported. Import interrupted.", fileType)
    return()
  }
  removeModal()
  datasetsToFetch <- datasetsToFetch[datasetsToFetch %in% c(
    names(modelInToImport),
    scalarsFileName
  )]

  # extract scalar sheets
  if (!identical(loadMode, "gdx") &&
    (length(modelIn) > length(modelInTabularData) || !scalarsFileName %in% names(modelIn))) {
    # at least one scalar input element that is not in tabular form
    i <- match(scalarsFileName, tolower(datasetsToFetch))[[1]]
    if (!is.na(i)) {
      # scalar table in workbook
      # add scalar datasets (e.g. slider/dropdown)
      if (scalarsFileName %in% tolower(modelInTabularData)) {
        # scalars is also amongst tabular data
        datasetsToFetch <- c(datasetsToFetch, names(modelIn)[!(names(modelIn) %in% modelInTabularData)])
      } else {
        # all scalar values are dropdown/slider etc. so remove scalar table from datasetsToFetch
        datasetsToFetch <- c(datasetsToFetch[-i], names(modelIn)[!(names(modelIn) %in% modelInTabularData)])
      }
    }
  }

  # find out which datasets to import from Excel sheet
  if (input$cbSelectManuallyLoc && length(input$selInputDataLoc)) {
    datasetsToFetch <- datasetsToFetch[tolower(datasetsToFetch) %in%
      tolower(isolate(input$selInputDataLoc))]
  }
  prog$set(detail = lang$progressBar$importScen$renderInput, value = 0.4)

  # reset input data
  modelInputGraphVisible[] <<- FALSE
  lapply(seq_along(modelIn)[names(modelIn) %in% datasetsToFetch], function(i) {
    hideEl(session, "#graph-in_" %+% i)
    showEl(session, "#data-in_" %+% i)
  })
  hideEl(session, "#btRefreshGraphIn")


  loadErrors <- character(0L)
  source("./modules/input_load.R", local = TRUE)
  if (identical(fileType, "miroscen")) {
    # update scenario name
    rv$activeSname <<- activeScen$getScenName()
  } else {
    markUnsaved()
  }
  if (!is.null(errMsg)) {
    return(NULL)
  }
  errMsg <- NULL
  # save input data
  scenData$loadSandbox(scenInputData, names(scenInputData), activeScen$getMetadata())
  if (!identical(loadMode, "scsv")) {
    prog$set(detail = lang$progressBar$importScen$renderOutput, value = 0.8)
    tryCatch(
      {
        outputData <- loadScenData(
          metaData = modelOut,
          workDir = loadModeWorkDir,
          templates = modelOutTemplate,
          method = loadMode,
          fileName = loadModeFileName,
          xlsio = xlsio, csvio = csvio, customDataIO = customDataIO
        )
        loadErrors <- c(loadErrors, outputData$errors)
      },
      error = function(e) {
        flog.info(
          "Problems loading output data. Error message: %s.",
          conditionMessage(e)
        )
        errMsg <<- conditionMessage(e)
      }
    )
    if (is.null(showErrorMsg(lang$errMsg$GAMSOutput$title, errMsg))) {
      return()
    }
    scenData$loadSandbox(outputData$tabular, names(modelOut))
    renderOutputData()
    if (scenData$getSandboxHasOutputData(scriptOutput)) {
      noOutputData <<- FALSE
    } else {
      noOutputData <<- TRUE
    }
  }

  if (newInputCount) {
    showNotification(sprintf(lang$nav$notificationNewInput$new, newInputCount))
  } else {
    showNotification(lang$nav$notificationNewInput$noNew, type = "error")
  }
  if (length(loadErrors)) {
    showErrorMsg(lang$errMsg$dataError$title, paste(loadErrors, collapse = "\n"))
  }
})
if (length(externalInputConfig)) {
  lapply(seq_along(externalInputConfig), function(extConfId) {
    if (length(externalInputConfig[[extConfId]]$localFileInput)) {
      observe({
        localFile <- input[[paste0("externalSourceFile_", extConfId)]]
        if (length(localFile) && length(localFile$datapath)) {
          customDataIO$setLocalFile(localFile)
        }
      })
    }
  })
  observe({
    req(isTRUE(input$cbSelectManuallyExt))
    if (length(input$selExternalSource) != 1L) {
      flog.error(
        "Bad external source: '%s'. This looks like an attempt to tamper with the app!",
        input$selExternalSource
      )
      return()
    }
    extSourceID <- match(input$selExternalSource, names(externalInputConfig))
    if (is.na(extSourceID)) {
      flog.error(
        "Invalid external source: '%s'. This looks like an attempt to tamper with the app!",
        input$selExternalSource
      )
      return()
    }
    if (length(externalInputConfig[[extSourceID]]$functionName)) {
      if (length(externalInputConfig[[extSourceID]]$symNames)) {
        datasetsToImport <- externalInputConfig[[extSourceID]]$symNames
      } else {
        datasetsToImport <- names(modelInToImport)
      }
    } else {
      # old config (remoteImport)
      # FIXME: remove when removing remoteImport
      datasetsToImport <- names(externalInputConfig[[extSourceID]])
    }
    extSourceDatasheets <- match(
      datasetsToImport,
      names(modelInToImport)
    )
    extSourceDatasheets <- extSourceDatasheets[!is.na(extSourceDatasheets)]
    updateSelectInput(session, "selInputDataExt",
      choices = setNames(
        names(modelInToImport)[extSourceDatasheets],
        modelInToImportAlias[extSourceDatasheets]
      )
    )
  })
  observeEvent(input$btImportExternal, {
    externalSource <- input$selExternalSource
    flog.trace(
      "Import external data button clicked with remote importer: '%s' selected.",
      externalSource
    )

    if (!identical(length(externalSource), 1L) ||
      !externalSource %in% names(externalInputConfig)) {
      flog.info(
        "Invalid remote importer: '%s'. This should not happen! Possible attempt of user tampering with the app!",
        externalSource
      )
      return(NULL)
    }
    if (length(externalInputConfig[[externalSource]]$functionName)) {
      if (length(externalInputConfig[[externalSource]]$symNames)) {
        datasetsToImport <- externalInputConfig[[externalSource]]$symNames
      } else {
        datasetsToImport <- names(modelInToImport)
      }
    } else {
      # old config (remoteImport)
      # FIXME: remove when removing remoteImport
      datasetsToImport <- names(externalInputConfig[[externalSource]])
    }
    if (input$cbSelectManuallyExt) {
      if (!length(input$selInputDataExt)) {
        return()
      }
      datasetsToImport <- datasetsToImport[datasetsToImport %in%
        tolower(input$selInputDataExt)]
    }
    datasetsImported <- vapply(match(datasetsToImport, names(modelIn)), function(i) {
      if (length(isolate(rv[[paste0("in_", i)]]))) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    }, logical(1L))

    if (any(datasetsImported)) {
      hideEl(session, "#importDataTabset")
      showEl(session, "#btReplaceInputData")
      showEl(session, "#btMergeInputData")
      showEl(session, "#importDataOverwrite")
    } else {
      overwriteInput <<- 0L
      rv$btOverwriteInput <<- rv$btOverwriteInput + 1L
    }
  })
}

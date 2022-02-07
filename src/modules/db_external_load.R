# import datasets from external data sources
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
  extSourceDatasheets <- match(
    names(externalInputConfig[[extSourceID]]),
    names(modelInToImport)
  )
  updateSelectInput(session, "selInputDataExt",
    choices = setNames(
      names(modelInToImport)[extSourceDatasheets],
      modelInToImportAlias[extSourceDatasheets]
    )
  )
})

observeEvent(input$btImportExternal, {
  externalSource <- input$selExternalSource
  removeModal()
  flog.trace(
    "Import remote data button clicked with remote importer: '%s' selected.",
    externalSource
  )

  if (!length(externalSource) ||
    !externalSource %in% names(externalInputConfig)) {
    flog.info(
      "Invalid remote importer: '%s'. This should not happen! Possible attempt of user tampering with the app!",
      externalSource
    )
    return(NULL)
  }
  errMsg <- NULL
  extConf <- externalInputConfig[[externalSource]]
  scalarDataset <- NULL

  datasetsToImport <- names(modelIn)

  if (input$cbSelectManuallyExt) {
    if (!length(input$selInputDataExt)) {
      return()
    }
    datasetsToImport <- datasetsToImport[datasetsToImport %in%
      tolower(input$selInputDataExt)]
  }
  if (length(extConf$localFileInput)) {
    extConfId <- match(externalSource, names(externalInputConfig))[[1]]
    localFile <- input[[paste0("externalSourceFile_", extConfId)]]
  } else {
    localFile <- NULL
  }

  scenInputData <- lapply(datasetsToImport, function(inputName) {
    extIdx <- match(inputName, names(extConf))[1L]
    if (is.na(extIdx)) {
      return(NULL)
    }
    item <- extConf[[extIdx]]
    if (!length(item)) {
      return()
    }
    i <- match(inputName, names(modelIn))

    # load from database
    tryCatch(
      {
        externalInputData[[i]] <<- dataio$import(item, inputName, localFile = localFile)
      },
      error = function(e) {
        flog.error("Problems fetching external data. Error message: %s.", conditionMessage(e))
        errMsg <<- paste(errMsg, sprintf(lang$errMsg$fetchDataset$desc, inputName), sep = "\n")
      }
    )
    if (!is.null(errMsg)) {
      return(NULL)
    }
    if (!length(externalInputData[[i]])) {
      errMsg <<- paste(errMsg, sprintf(lang$errMsg$fetchDataset$noData, modelInAlias[i]), sep = "\n")
      return(NULL)
    }
    switch(modelIn[[i]]$type,
      hot = ,
      dt = {
        return(externalInputData[[i]])
      },
      {
        flog.error("Input type: '%s' is not supported for share datesets.", modelIn[[i]]$type)
        errMsg <<- paste(errMsg, sprintf(lang$errMsg$fetchDataset$desc, modelInAlias[i]), sep = "\n")
        return(NULL)
      }
    )
  })
  names(scenInputData) <- datasetsToImport
  errMsg <- NULL
  loadMode <- "scen"
  newInputCount <- 0L
  overwriteInput <- 1L
  datasetsToFetch <- datasetsToImport[!vapply(scenInputData, is.null,
    logical(1L),
    USE.NAMES = FALSE
  )]
  dfClArgs <- NULL
  source("./modules/input_load.R", local = TRUE)
  showErrorMsg(lang$errMsg$fetchDataset$title, errMsg)
  markUnsaved()
})

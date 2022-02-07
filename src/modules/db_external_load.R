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
      dropdown = {
        if (!is.null(item$colSubset)) {
          subsetIdx <- match(tolower(item$colSubset), tolower(names(externalInputData[[i]])))
          if (any(is.na(subsetIdx))) {
            errMsg <<- paste(errMsg, sprintf(
              lang$errMsg$fetchDataset$badColName,
              paste(item$colSubset[is.na(subsetIdx)], collapse = ","),
              inputName
            ), sep = "\n")
            return(NULL)
          }
          choices <- externalInputData[[i]][item$colSubset]
        } else {
          choices <- externalInputData[[i]]
        }
        if (!length(choices)) {
          errMsg <<- paste(errMsg, sprintf(lang$errMsg$fetchDataset$noData, modelInAlias[i]), sep = "\n")
          return(NULL)
        } else if (length(choices) > 1) {
          # set aliases
          choices <- setNames(choices[[1]], choices[[2]])
        } else {
          # only 1 column so no aliases
          choices <- choices[[1]]
        }
        updateSelectInput(session, "dropdown_" %+% i,
          choices = choices,
          selected = modelIn[[i]]$dropdown$selected
        )

        observe(
          {
            if (!identical(length(input[["dropdown_" %+% i]]), 1L) ||
              identical(nchar(input[["dropdown_" %+% i]]), 0L)) {
              return()
            }
            externalInputData_filtered[[i]] <<- externalInputData[[i]][externalInputData[[i]][[item$colSubset[[1]]]] == input[["dropdown_" %+% i]], ]
          },
          priority = 1e6
        )
        return(NULL)
      },
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

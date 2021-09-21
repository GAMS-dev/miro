# load model input data
errMsg <- NULL
if (!identical(loadMode, "scen")) {
  tryCatch(
    {
      tabularDatasetsToFetch <- datasetsToFetch[tolower(datasetsToFetch) %in% modelInTabularData]
      tabularIdsToFetchId <- names(modelIn) %in% tabularDatasetsToFetch
      metaDataTmp <- modelIn[tabularIdsToFetchId]
      namesScenInputData <- names(modelIn)[tabularIdsToFetchId]
      modelInTemplateTmp <- modelInTemplate[tabularIdsToFetchId]
      if (length(scalarsInMetaData) && !scalarsFileName %in% tabularDatasetsToFetch) {
        tabularDatasetsToFetch <- c(tabularDatasetsToFetch, scalarsFileName)
        namesScenInputData <- c(namesScenInputData, scalarsFileName)
        modelInTemplateTmp[[length(metaDataTmp) + 1L]] <- scalarsInTemplate
        metaDataTmp <- c(metaDataTmp, scalarsInMetaData)
      }
      scenInputData <- loadScenData(
        metaData = metaDataTmp,
        workDir = loadModeWorkDir,
        templates = modelInTemplateTmp,
        method = loadMode,
        fileName = loadModeFileName, DDPar = DDPar, GMSOpt = GMSOpt,
        dfClArgs = dfClArgs, xlsio = xlsio, csvio = csvio
      )
      if (length(scenInputData$errors)) {
        loadErrors <- scenInputData$errors
      }
      scenInputData <- scenInputData$tabular
      if (!length(scenInputData)) {
        return()
      }
      names(scenInputData) <- namesScenInputData
      rm(metaDataTmp, namesScenInputData, modelInTemplateTmp)
    },
    error = function(e) {
      flog.error(
        "Problems loading input data. Error message: %s.",
        conditionMessage(e)
      )
      errMsg <<- lang$errMsg$dataError$desc
    }
  )
}
if (!is.null(showErrorMsg(lang$errMsg$GAMSInput$title, errMsg))) {
  lapply(datasetsToFetch, function(dataset) {
    i <- match(tolower(dataset), names(modelIn))[[1]]
    dataTmp <- NULL
    if (is.na(i)) {
      return()
    }
    inputVerified <- FALSE
    # execute only if dataframe has not yet been imported or already imported data shall be overridden
    if (!length(isolate(rv[["in_" %+% i]])) || overwriteInput > 0L) {
      # handsontable, multi dropdown, or daterange
      if (tolower(dataset) %in% modelInTabularData) {
        if (identical(overwriteInput, 2L)) {
          if (length(modelIn[[i]]$definedByExternalSymbol)) {
            rootSymId <- match(modelIn[[i]]$definedByExternalSymbol, names(modelIn))
          } else {
            rootSymId <- i
          }
          dataTmp <- mergeDf(
            fixColTypes(
              getInputDataset(rootSymId, subSymName = names(modelIn)[[i]]),
              modelIn[[i]]$colTypes
            ),
            scenInputData[[dataset]],
            isScalarsTable = identical(names(modelIn)[[i]], scalarsFileName)
          )
        } else {
          dataTmp <- scenInputData[[dataset]]
        }
        if (length(dataTmp) && nrow(dataTmp)) {
          if (identical(names(modelIn)[[i]], scalarsFileName)) {
            if (verifyScalarInput(
              dataTmp, modelIn[[i]]$headers,
              c(scalarInputSym, scalarInputSymToVerify)
            )) {
              scalarDataset <<- dataTmp
              modelInputData[[i]] <<- dataTmp[dataTmp[[1]] %in% modelIn[[i]]$symnames, , drop = FALSE]
              inputVerified <- TRUE
            }
          } else {
            if (verifyInput(dataTmp, modelIn[[i]]$headers)) {
              # GAMS sets are always strings so make sure it is not parsed as a numeric
              numericSet <- vapply(seq_along(dataTmp), function(dataColIdx) {
                if (is.numeric(dataTmp[[dataColIdx]]) &&
                  identical(modelIn[[i]]$headers[[dataColIdx]]$type, "string")) {
                  return(TRUE)
                } else {
                  return(FALSE)
                }
              }, logical(1L), USE.NAMES = FALSE)
              dataTmp[numericSet] <- lapply(dataTmp[numericSet], as.character)
              modelInputData[[i]] <<- dataTmp
              inputVerified <- TRUE
            }
          }
          isEmptyInput[[i]] <<- FALSE
        } else {
          # empty dataset
          if (length(modelInTemplate[[i]])) {
            modelInputData[[i]] <<- modelInTemplate[[i]]
          }
          isEmptyInput[[i]] <<- TRUE
          inputVerified <- TRUE
        }
      } else {
        # single dropdown, slider or date

        scalarName <- tolower(names(modelIn)[[i]])

        # check whether scalar dataset has already been imported
        if (is.null(scalarDataset)) {
          dataTmp <- scenInputData[[scalarsFileName]]
          # assign new input data here as assigning it directly inside the tryCatch environment would result in deleting list elements
          # rather than setting them to NULL
          if (!is.null(dataTmp)) {
            scalarDataset <<- dataTmp
          }
        }
        if (length(scalarDataset) && nrow(scalarDataset)) {
          # double slider has two scalar values saved
          if ((modelIn[[i]]$type == "slider" && length(modelIn[[i]]$slider$default) > 1) ||
            (modelIn[[i]]$type == "daterange")) {
            if (identical(modelIn[[i]]$slider$single, TRUE)) {
              # single slider that was extended in HC Mode
              dataTmp <- scalarDataset[[3]][tolower(scalarDataset[[1]]) %in%
                paste0(scalarName, c("$lo", "$up"))]
            } else {
              dataTmp <- scalarDataset[[3]][tolower(scalarDataset[[1]]) %in%
                paste0(scalarName, c("_lo", "_up"))]
            }
            if (identical(modelIn[[i]]$type, "slider")) {
              dataTmp <- as.numeric(dataTmp)
            }
            if (length(dataTmp) && !all(is.na(dataTmp))) {
              modelInputData[[i]] <<- dataTmp

              if (isTRUE(modelIn[[i]]$slider$single) ||
                isTRUE(modelIn[[i]]$slider$double)) {
                modelInputDataHcubeTmp <- scalarDataset[[3]][tolower(scalarDataset[[1]]) ==
                  paste0(scalarName, "$step")]
                if (isTRUE(modelIn[[i]]$slider$double)) {
                  modelInputDataHcubeTmp <- c(
                    modelInputDataHcubeTmp,
                    scalarDataset[[3]][tolower(scalarDataset[[1]]) ==
                      paste0(scalarName, "$mode")]
                  )
                }
                modelInputDataHcube[[i]] <<- as.numeric(modelInputDataHcubeTmp)
              }
              inputVerified <- TRUE
              newInputCount <<- newInputCount + 1
            }
          } else {
            dataTmp <- scalarDataset[[3]][tolower(scalarDataset[[1]]) == scalarName]
            if (length(dataTmp) && !all(is.na(dataTmp))) {
              if (identical(modelIn[[i]]$type, "dropdown")) {
                dataTmp <- strsplit(dataTmp, "||", fixed = TRUE)[[1]][1]
              }
              modelInputData[[i]] <<- dataTmp
              inputVerified <- TRUE
              newInputCount <<- newInputCount + 1
            }
          }
        }
      }


      # check if input data is valid
      if (inputVerified) {
        if (!isTRUE(isEmptyInput[i])) {
          flog.debug("Dataset: %s loaded successfully (mode: %s, overwrite: %s)", dataset, loadMode, overwriteInput)
          newInputCount <<- newInputCount + 1
          # set identifier that data was overwritten
          isEmptyInput[i] <<- TRUE
        }
        if (!identical(loadMode, "scen")) {
          # set unsaved flag
          rv$unsavedFlag <<- TRUE
          # if scenario includes output data set dirty flag
          if (!noOutputData) {
            dirtyFlag <<- TRUE
            showEl(session, "#dirtyFlagIcon")
            showEl(session, "#dirtyFlagIconO")
          }
        }
        # reset dependent elements
        inputInitialized[dependentDatasets[[i]]] <<- FALSE

        if (!is.null(modelInWithDep[[tolower(names(modelIn)[[i]])]])) {
          id <- match(tolower(names(modelIn)[[i]]), tolower(names(modelInWithDep)))[1]
          if (inputInitialized[id]) {
            # only update when initialized
            if (length(isolate(rv[[paste0("in_", i)]]))) {
              rv[[paste0("in_", i)]] <<- isolate(rv[[paste0("in_", i)]]) + 1
            } else {
              rv[[paste0("in_", i)]] <<- 1
            }
          }
        } else {
          # no dependencies, so update anyway
          if (length(isolate(rv[[paste0("in_", i)]]))) {
            rv[[paste0("in_", i)]] <<- isolate(rv[[paste0("in_", i)]]) + 1
          } else {
            rv[[paste0("in_", i)]] <<- 1
          }
        }
      } else {
        if (tolower(dataset) %in% names(modelInMustImport)) {
          flog.info("The uploaded dataset: '%s' could not be verified.", modelInAlias[i])
          errMsg <<- paste(errMsg, sprintf(lang$errMsg$GAMSInput$badInputData, modelInAlias[i]), sep = "\n")
        }
      }
    }
  })
  showErrorMsg(lang$errMsg$GAMSInput$title, errMsg)

  # set initialisation flags for handsontables to FALSE
  if (length(datasetsToFetch)) {
    hotInit[match(tolower(datasetsToFetch), names(modelIn))] <<- FALSE
  }
}

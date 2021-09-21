# save input data to dataTmp list

getInputDataFromSandbox <- function() {
  # define temporary list to save input data to
  dataTmp <- vector(mode = "list", length = length(modelInFileNames))
  names(dataTmp) <- modelInFileNames
  j <- 1L
  # first add scalar data which is in a table
  scalarId <- match(scalarsFileName, modelInTabularData)[[1]]

  if (!is.na(scalarId)) {
    i <- match(tolower(modelInTabularData[scalarId]), names(modelIn))[[1]]
    if (length(modelIn[[i]]$definedByExternalSymbol)) {
      symIdTmp <- match(modelIn[[i]]$definedByExternalSymbol, names(modelIn))
    } else {
      symIdTmp <- i
    }
    tryCatch(
      {
        dataTmp[[length(modelInFileNames)]] <- fixColTypes(
          getInputDataset(symIdTmp, subSymName = names(modelIn)[[i]]),
          modelIn[[i]]$colTypes
        )
      },
      error = function(e) {
        flog.error(
          "Dataset: '%s' could not be loaded. Error message: '%s'.",
          modelInAlias[i], conditionMessage(e)
        )
        stop_custom("no_data", sprintf(
          lang$errMsg$GAMSInput$noData,
          modelInAlias[i]
        ), call. = FALSE)
      }
    )
  }
  addScalarVal <- function(scalar, description, value) {
    value <- as.character(value)
    # generate data frame
    if (is.null(dataTmp[[length(modelInFileNames)]])) {
      # no scalar data was written yet, so add headers
      dataTmp[[length(modelInFileNames)]] <<- tibble(scalar, description, value)
      names(dataTmp[[length(modelInFileNames)]]) <<- scalarsFileHeaders
    } else {
      # no headers, just data
      newData <- tibble(scalar, description, value)
      names(newData) <- scalarsFileHeaders
      dataTmp[[length(modelInFileNames)]] <<- rbind(dataTmp[[length(modelInFileNames)]], newData)
    }
  }
  lapply(seq_along(modelIn), function(i) {
    if (length(modelIn[[i]]$definedByExternalSymbol)) {
      symIdTmp <- match(modelIn[[i]]$definedByExternalSymbol, names(modelIn))
      tryCatch(
        {
          valTmp <- getInputDataset(symIdTmp, subSymName = names(modelIn)[[i]])
        },
        error = function(e) {
          flog.error(
            "Dataset: '%s' could not be loaded. Error message: '%s'.",
            modelInAlias[i], conditionMessage(e)
          )
          stop_custom("no_data", sprintf(
            lang$errMsg$GAMSInput$noData,
            modelInAlias[i]
          ), call. = FALSE)
        }
      )
    } else {
      valTmp <- NULL
    }
    noErr <- TRUE
    switch(modelIn[[i]]$type,
      slider = {
        if (!length(modelIn[[i]]$definedByExternalSymbol)) {
          valTmp <- isolate(input[[paste0("slider_", i)]])
        }
        if (!is.null(valTmp)) {
          value <- valTmp
        } else if (is.numeric(sliderValues[[tolower(names(modelIn)[[i]])]]$def)) {
          value <- sliderValues[[tolower(names(modelIn)[[i]])]]$def
        } else {
          flog.error("Dataset: '%s' could not be loaded.", modelInAlias[i])
          stop_custom("no_data", sprintf(
            lang$errMsg$GAMSInput$noData,
            modelInAlias[i]
          ), call. = FALSE)
        }
        # add name and description fields
        if (length(value) > 1) {
          # double slider (two values)
          scalar <- paste0(names(modelIn)[i], c("_lo", "_up"))
          description <- paste0(modelInAlias[i], c(" (lower)", " (upper)"))
        } else {
          # standard slider (one value)
          scalar <- names(modelIn)[[i]]
          description <- modelInAlias[i]
        }
        addScalarVal(scalar, description, value)
      },
      date = {
        if (!length(modelIn[[i]]$definedByExternalSymbol)) {
          valTmp <- isolate(input[[paste0("date_", i)]])
        }
        if (!is.null(valTmp)) {
          value <- as.character(valTmp)
          if (length(value) != 1L || is.na(value)) {
            value <- ""
          }
        } else if (!is.null(modelIn[[i]]$date$value)) {
          value <- as.character(modelIn[[i]]$date$value)
        } else {
          flog.error("Dataset: '%s' could not be loaded.", modelInAlias[i])
          stop_custom("no_data", sprintf(
            lang$errMsg$GAMSInput$noData,
            modelInAlias[i]
          ), call. = FALSE)
        }
        # add name and description fields
        scalar <- names(modelIn)[[i]]
        description <- modelInAlias[i]
        addScalarVal(scalar, description, value)
      },
      daterange = {
        if (!length(modelIn[[i]]$definedByExternalSymbol)) {
          valTmp <- isolate(input[[paste0("daterange_", i)]])
        }
        if (length(valTmp)) {
          value <- as.character(valTmp)
          emptyDate <- is.na(value)
          if (any(emptyDate)) {
            value[emptyDate] <- ""
          }
        } else if (!is.null(modelIn[[i]]$daterange$start) && !is.null(modelIn[[i]]$daterange$end)) {
          value <- c(
            as.character(modelIn[[i]]$daterange$start),
            as.character(modelIn[[i]]$daterange$end)
          )
        } else {
          flog.error("Dataset: '%s' could not be loaded.", modelInAlias[i])
          stop_custom("no_data", sprintf(
            lang$errMsg$GAMSInput$noData,
            modelInAlias[i]
          ), call. = FALSE)
        }
        # add name and description fields
        scalar <- paste0(names(modelIn)[[i]], c("_lo", "_up"))
        description <- paste0(modelInAlias[i], c(" (lower)", " (upper)"))
        addScalarVal(scalar, description, value)
      },
      textinput = {
        if (!length(modelIn[[i]]$definedByExternalSymbol)) {
          valTmp <- isolate(input[[paste0("text_", i)]])
        }
        if (!is.null(valTmp)) {
          value <- as.character(valTmp)
        } else if (!is.null(modelIn[[i]]$textinput$value)) {
          value <- as.character(modelIn[[i]]$textinput$value)
        } else {
          flog.error("Dataset: '%s' could not be loaded.", modelInAlias[i])
          stop_custom("no_data", sprintf(
            lang$errMsg$GAMSInput$noData,
            modelInAlias[i]
          ), call. = FALSE)
        }
        # add name and description fields
        scalar <- names(modelIn)[[i]]
        description <- modelInAlias[i]
        addScalarVal(scalar, description, value)
      },
      numericinput = {
        if (!length(modelIn[[i]]$definedByExternalSymbol)) {
          valTmp <- isolate(input[[paste0("numeric_", i)]])
        }
        if (length(valTmp) == 1L &&
          !identical(valTmp, "")) {
          value <- valTmp
        } else if (!is.null(modelIn[[i]]$numericinput$value)) {
          value <- modelIn[[i]]$numericinput$value
        } else {
          value <- 0L
        }
        # add name and description fields
        scalar <- names(modelIn)[[i]]
        description <- modelInAlias[i]
        addScalarVal(scalar, description, value)
      },
      dropdown = {
        if (!length(modelIn[[i]]$definedByExternalSymbol)) {
          valTmp <- isolate(input[[paste0("dropdown_", i)]])
        }
        if (!is.null(valTmp)) {
          value <- valTmp
        } else if (names(modelIn)[[i]] %in% modelInTabularDataBase) {
          value <- character(0L)
        } else if (!is.null(modelIn[[i]]$dropdown$selected)) {
          value <- modelIn[[i]]$dropdown$selected
        } else {
          stop_custom("no_data", sprintf(
            lang$errMsg$GAMSInput$noData,
            modelInAlias[i]
          ), call. = FALSE)
        }
        value <- value[value != "_"]
        if (names(modelIn)[[i]] %in% modelInTabularDataBase ||
          names(modelIn)[[i]] %in% ioConfig$hcubeScalars) {
          # generate data frame (multi dropdown menu)
          dataTmp[[j]] <<- ddToTibble(value, modelIn[[i]])
          j <<- j + 1L
        } else {
          # standard dropdown menu (one value)
          scalar <- names(modelIn)[[i]]
          description <- modelInAlias[i]
          addScalarVal(scalar, description, value)
        }
      },
      checkbox = {
        if (!length(modelIn[[i]]$definedByExternalSymbol)) {
          valTmp <- isolate(input[[paste0("cb_", i)]])
        }
        if (!is.null(valTmp)) {
          value <- if (identical(valTmp, TRUE)) 1L else 0L
        } else if (!is.null(modelIn[[i]]$checkbox$value)) {
          value <- if (identical(modelIn[[i]]$checkbox$value, TRUE)) 1L else 0L
        } else {
          stop_custom("no_data", sprintf(
            lang$errMsg$GAMSInput$noData,
            modelInAlias[i]
          ), call. = FALSE)
        }
        # add name and description fields
        scalar <- names(modelIn)[[i]]
        description <- modelInAlias[i]
        addScalarVal(scalar, description, value)
      },
      {
        if (names(modelIn)[[i]] != scalarsFileName) {
          tryCatch(
            {
              if (!length(modelIn[[i]]$definedByExternalSymbol)) {
                valTmp <- getInputDataset(i, subSymName = names(modelIn)[[i]])
              }
              dataTmp[[j]] <<- fixColTypes(valTmp, modelIn[[i]]$colTypes)
            },
            error = function(e) {
              flog.error(
                "Dataset: '%s' could not be loaded. Error message: '%s'.",
                modelInAlias[i], conditionMessage(e)
              )
              stop_custom("no_data", sprintf(
                lang$errMsg$GAMSInput$noData,
                modelInAlias[i]
              ), call. = FALSE)
            }
          )
          j <<- j + 1
        }
      }
    )
    flog.trace("Dataset: %s saved in dataTmp.", names(modelIn)[i])
  })
  return(dataTmp)
}

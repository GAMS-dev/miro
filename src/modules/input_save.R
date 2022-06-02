getInputDataset <- function(i, visible = FALSE) {
  if (length(modelIn[[i]]$definedByExternalSymbol)) {
    symIdTmp <- match(modelIn[[i]]$definedByExternalSymbol, names(modelIn))
    tryCatch(
      {
        valTmp <- getTabularInputDataset(symIdTmp,
          visible = visible,
          subSymName = names(modelIn)[[i]]
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
  } else {
    valTmp <- NULL
  }
  noErr <- TRUE
  if (identical(modelIn[[i]]$type, "slider")) {
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
    return(list(isScalar = TRUE, scalar = scalar, description = description, value = value))
  }
  if (identical(modelIn[[i]]$type, "date")) {
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
    return(list(isScalar = TRUE, scalar = scalar, description = description, value = value))
  }
  if (identical(modelIn[[i]]$type, "daterange")) {
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
    return(list(isScalar = TRUE, scalar = scalar, description = description, value = value))
  }
  if (identical(modelIn[[i]]$type, "textinput")) {
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
    return(list(isScalar = TRUE, scalar = scalar, description = description, value = value))
  }
  if (identical(modelIn[[i]]$type, "numericinput")) {
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
    return(list(isScalar = TRUE, scalar = scalar, description = description, value = value))
  }
  if (identical(modelIn[[i]]$type, "dropdown")) {
    if (length(modelIn[[i]]$definedByExternalSymbol)) {
      if ("data.frame" %in% class(valTmp)) {
        valTmp <- valTmp[[1L]]
      }
    } else {
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
      return(list(isScalar = FALSE, value = ddToTibble(value, modelIn[[i]])))
    }
    # standard dropdown menu (one value)
    scalar <- names(modelIn)[[i]]
    description <- modelInAlias[i]
    return(list(isScalar = TRUE, scalar = scalar, description = description, value = value))
  }
  if (identical(modelIn[[i]]$type, "checkbox")) {
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
    return(list(isScalar = TRUE, scalar = scalar, description = description, value = value))
  }
  tryCatch(
    {
      if (!length(modelIn[[i]]$definedByExternalSymbol)) {
        valTmp <- getTabularInputDataset(i,
          visible = visible,
          subSymName = names(modelIn)[[i]]
        )
      }
      return(list(isScalar = FALSE, value = fixColTypes(valTmp, modelIn[[i]]$colTypes)))
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
getInputDataFromSandbox <- function() {
  # define temporary list to save input data to
  dataTmp <- vector(mode = "list", length = length(modelInFileNames))
  names(dataTmp) <- modelInFileNames
  scalarData <- bind_rows(lapply(seq_along(modelIn), function(i) {
    inputDatasetTmp <- getInputDataset(i)
    if (identical(inputDatasetTmp[["isScalar"]], TRUE)) {
      return(tibble(
        scalar = inputDatasetTmp[["scalar"]],
        description = inputDatasetTmp[["description"]],
        value = as.character(inputDatasetTmp[["value"]])
      ))
    }
    dataTmp[[match(names(modelIn)[[i]], modelInFileNames)]] <<- inputDatasetTmp[["value"]]
    flog.trace("Input symbol: %s pulled from sandbox.", names(modelIn)[[i]])
    return(tibble(
      scalar = character(),
      description = character(),
      value = character()
    ))
  }))
  if (nrow(scalarData) > 0L) {
    names(scalarData) <- scalarsFileHeaders
    if (is.null(dataTmp[[length(modelInFileNames)]])) {
      dataTmp[[length(modelInFileNames)]] <- scalarData
    } else {
      dataTmp[[length(modelInFileNames)]] <- bind_rows(
        dataTmp[[length(modelInFileNames)]],
        scalarData
      )
    }
  }
  return(dataTmp)
}

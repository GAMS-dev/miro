confFileVersion <- 1L
validateGraphConfig <- function(graphConfig, skipOutTypeCheck = FALSE) {
  if (!skipOutTypeCheck && !identical(graphConfig$outType, "miroPivot")) {
    return(TRUE)
  }
  errMsgTmp <- NULL
  if (length(graphConfig$options$aggregationFunction) &&
    identical(graphConfig$options[["_metadata_"]]$symtype, "set") &&
    !graphConfig$options$aggregationFunction %in% c("count", "min")) {
    errMsgTmp <- "Sets can only have 'count' or 'min' as aggregation function."
  }
  noNumericHeaders <- sum(vapply(graphConfig$options[["_metadata_"]]$headers,
    function(header) {
      identical(header$type, "numeric")
    }, logical(1L),
    USE.NAMES = FALSE
  ))
  validHeaders <- names(graphConfig$options[["_metadata_"]]$headers)
  if (noNumericHeaders > 1L) {
    validHeaders <- c(
      validHeaders[seq_len(length(validHeaders) - noNumericHeaders)],
      "Hdr"
    )
  } else {
    validHeaders <- validHeaders[-length(validHeaders)]
  }
  for (id in c("rows", "cols", "aggregations", "filter", "domainFilter")) {
    if (length(graphConfig$options[[id]])) {
      if (identical(id, "rows")) {
        indices <- graphConfig$options[[id]]
      } else if (identical(id, "domainFilter")) {
        indices <- graphConfig$options$domainFilter$domains
      } else {
        indices <- names(graphConfig$options[[id]])
      }
      invalidIndices <- !indices %in% validHeaders
      if (any(invalidIndices)) {
        errMsgTmp <- paste(
          errMsgTmp,
          paste0(
            "\nInvalid ", id, ": ",
            paste(indices[invalidIndices],
              collapse = ", "
            )
          )
        )
      }
    }
  }
  if (length(graphConfig$options$domainFilter$default) &&
    !graphConfig$options$domainFilter$default %in% c("_none", graphConfig$options$domainFilter$domains)) {
    errMsgTmp <- paste(
      errMsgTmp,
      paste0(
        "\nDefault domain filter: ",
        graphConfig$options$domainFilter$default,
        " must be among the list of domains."
      )
    )
  }
  if (is.null(errMsgTmp)) {
    return(TRUE)
  }
  return(errMsgTmp)
}

validateDashboardConfig <- function(graphConfig, dashboardType) {
  errMsgTmp <- NULL

  vb <- graphConfig$options$valueBoxes
  dv <- graphConfig$options$dataViews
  dc <- graphConfig$options$dataViewsConfig

  allLengths <- sapply(vb, length)
  if (any(allLengths != allLengths[1])) {
    errMsgTmp <- "All lists in the 'valueBoxes' configuration must have the same length."
  }

  if (any(grepl(" ", vb$id))) {
    errMsgTmp <- paste(
      errMsgTmp,
      sprintf(
        "\nSpaces are not allowed in valueBox ids. The following valueBox id(s) contain(s) a space: '%s'",
        paste(vb$id[grepl(" ", vb$id)], collapse = ", ")
      )
    )
  }

  vbSymbols <- vb$valueScalar[!is.na(vb$valueScalar)]
  noSymbol <- vbSymbols[!(vbSymbols %in% modelOut[[scalarsOutName]]$symnames)]
  if (length(noSymbol)) {
    errMsgTmp <- paste(
      errMsgTmp,
      sprintf(
        "\nNo scalar symbol '%s' found for valueBox",
        paste(noSymbol, collapse = ", ")
      )
    )
  }

  noMatchingId <- names(dv)[!names(dv) %in% vb$id]
  if (length(noMatchingId)) {
    errMsgTmp <- paste(
      errMsgTmp,
      sprintf(
        "\nThe following dataView id(s) do not match with any valuebox id: '%s'",
        paste(noMatchingId, collapse = ", ")
      )
    )
  }

  chartIds <- unname(unlist(lapply(dv, function(x) names(x))))
  if (any(grepl(" ", chartIds))) {
    errMsgTmp <- paste(
      errMsgTmp,
      sprintf(
        "\nSpaces are not allowed in dataViews Ids. The following dataView id(s) contain(s) a space: '%s'",
        paste(chartIds[grepl(" ", chartIds)], collapse = ", ")
      )
    )
  }


  for (view in names(dc)) {
    config <- dc[[view]]

    if (!is.list(config)) {
      # custom user output
      next
    }
    if (identical(dashboardType, "compare")) {
      if (!is.null(config$data) && config$data %in% names(modelOut)) {
        symbol <- config$data
      } else {
        errMsgTmp <- paste(
          errMsgTmp,
          sprintf(
            "\nSymbol '%s' in dataViewConfig '%s' not found among the symbols that the dashboard compare mode has access to. Make sure to provide a valid symbol name (property `data`).",
            if (!is.null(config$data)) config$data else "[NULL]", view
          )
        )
        next
      }
    } else if (length(config$data)) {
      if (config$data %in% c(graphConfig$options[["_metadata_"]]$symname, graphConfig$additionalData)) {
        symbol <- config$data
      } else {
        errMsgTmp <- paste(
          errMsgTmp,
          sprintf(
            "\nSymbol '%s' not found among the symbols that the renderer has access to.",
            config$data
          )
        )
        next
      }
    } else {
      symbol <- graphConfig$options[["_metadata_"]]$symname
    }

    noNumericHeaders <- sum(vapply(modelOut[[symbol]]$header,
      function(header) {
        identical(header$type, "numeric")
      }, logical(1L),
      USE.NAMES = FALSE
    ))
    validHeaders <- names(modelOut[[symbol]]$headers)
    if (noNumericHeaders > 1L) {
      validHeaders <- c(
        validHeaders[seq_len(length(validHeaders) - noNumericHeaders)],
        "Hdr"
      )
    } else {
      validHeaders <- validHeaders[-length(validHeaders)]
    }
    if (identical(dashboardType, "compare")) {
      validHeaders <- c(validHeaders, "_scenName")
    }

    allIndices <- c()
    for (id in c("rows", "cols", "aggregations", "filter", "userFilter")) {
      if (length(config[[id]])) {
        if (id %in% c("rows", "userFilter")) {
          if (identical(id, "userFilter") && length(config[[id]]) == 1 && config[[id]] %in% names(dc)) {
            # If a dataViewsConfig ID is set as userFilter, the IDs filters are applied here as well.
            next
          }
          indices <- config[[id]]
        } else {
          indices <- names(config[[id]])
        }
        allIndices <- c(allIndices, indices)
        invalidIndices <- !indices %in% validHeaders
        if (any(invalidIndices)) {
          errMsgTmp <- paste(
            errMsgTmp,
            paste0(
              "\nInvalid ", id, " in view ", view, ": ",
              paste(indices[invalidIndices],
                collapse = ", "
              )
            )
          )
        }
      }
    }
    if (identical(dashboardType, "compare") && !"_scenName" %in% allIndices) {
      errMsgTmp <- paste(
        errMsgTmp,
        paste0(
          "\nMissing '_scenName' index in dataViewConfig '", view, "'"
        )
      )
    }
  }

  if (is.null(errMsgTmp)) {
    return(TRUE)
  }
  return(errMsgTmp)
}

errMsg <- installAndRequirePackages("V8", installedPackages, RLibPath, CRANMirror, miroWorkspace)
# check whether there exists a config file and if not create an empty one
if (is.null(errMsg)) {
  if (nchar(modelName) > 60) {
    errMsg <- sprintf(
      "The MIRO app name: '%s' is too long! A maximum length of 60 characters is allowed!",
      modelName
    )
  } else if (startsWith(modelName, "~$")) {
    errMsg <- sprintf(
      "The MIRO app name: '%s' must not start with: '~$'!",
      modelName
    )
  } else if (!file.exists(paste0(
    currentModelDir, .Platform$file.sep, "conf_", modelName,
    .Platform$file.sep, modelName, ".json"
  ))) {
    tryCatch(
      cat("{}\n", file = paste0(
        currentModelDir, .Platform$file.sep, "conf_", modelName,
        .Platform$file.sep, modelName, ".json"
      )),
      error = function(e) {
        errMsg <<- paste0(
          "A configuration file was not found and no data could be written to the location of the config folder. Please check read/write permissions in folder: ",
          paste0(currentModelDir, .Platform$file.sep, "conf_", modelName)
        )
      }
    )
  }
}

if (is.null(errMsg)) {
  # files that require schema file
  jsonFilesWithSchema <- c(
    paste0(
      currentModelDir, .Platform$file.sep,
      "conf_", modelName, .Platform$file.sep,
      modelName, ".json"
    ),
    paste0(
      currentModelDir, .Platform$file.sep,
      "conf_", modelName, .Platform$file.sep,
      modelName, "_io.json"
    ),
    file.path(
      currentModelDir,
      paste0("conf_", modelName),
      "views.json"
    )
  )
  jsonFilesMissing <- !file.exists(jsonFilesWithSchema[1:2])
  if (any(jsonFilesMissing)) {
    errMsg <- paste(errMsg, paste0(
      "JSON file(s): '", basename(jsonFilesWithSchema[jsonFilesMissing]),
      "' is required/are required but missing or you have no read permissions. Please make sure this file/these files are available.\n
If you run MIRO for the first time with your new model, the GAMS/MIRO data contract needs to be generated.\n
Run \"gams ", modelName, ".gms IDCGenerateJSON=conf_", modelName, "/", modelName, "_io.json IDCGenerateGDX=data_",
      modelName, "/", "default.gdx\" inside your model directory to generate the data contract as well as a default scenario.\n
Note that GAMS will not create the directories 'conf_", modelName, "' and 'data_", modelName, "', so you need to create these first."
    ),
    sep = "\n"
    )
  }
  rm(jsonFilesMissing)
  jsonSchemaMap <- list(
    config = c(
      jsonFilesWithSchema[1],
      file.path(getwd(), "conf", "config_schema.json")
    ),
    io_config = c(
      jsonFilesWithSchema[2],
      file.path(getwd(), "conf", "io_config_schema.json")
    ),
    views = c(
      jsonFilesWithSchema[3],
      file.path(getwd(), "conf", "views_schema.json")
    )
  )
}

# validate json files
if (is.null(errMsg)) {
  jsonValidator <- JSONValidator$new()
  lapply(seq_along(jsonSchemaMap), function(i) {
    if (tryCatch(
      {
        valid <- jsonValidator$validate(
          jsonSchemaMap[[i]][1],
          jsonSchemaMap[[i]][2],
          returnRawData = identical(names(jsonSchemaMap)[[i]], "io_config")
        )
        FALSE
      },
      error = function(e) {
        if (!names(jsonSchemaMap)[[i]] %in% c("views")) {
          errMsg <<- paste(errMsg, paste0(
            "Some error occurred validating JSON file: '",
            basename(jsonFilesWithSchema[i]), "'. Error message: ",
            conditionMessage(e)
          ), sep = "\n")
        }
        return(TRUE)
      }
    )) {
      return()
    }

    if (is.null(valid$errors)) {
      if (identical(names(jsonSchemaMap)[[i]], "config")) {
        config <<- valid$data
      } else if (identical(names(jsonSchemaMap)[[i]], "io_config")) {
        config <<- c(config, valid$data)
        if (is.null(config$modelTitle)) {
          # we have to use jsonlite::readJSON instead of JSON.parse for parsing
          # the data contract due to an issue with headers being reordered if they
          # use integer values (e.g. "1", "2" etc.). Thus, AJV won't insert the
          # defaults from the schema (only one luckily for the io_schema) into the data
          # object. We have to do it manually.
          config$modelTitle <- "Unnamed model"
        }
      } else if (identical(names(jsonSchemaMap)[[i]], "views")) {
        config$globalViews <<- valid$data
      }
    } else {
      errMsg <<- paste(errMsg,
        paste0(
          "Some error occurred parsing JSON file: '",
          basename(jsonFilesWithSchema[i]),
          "'. Error message: ", valid$errors
        ),
        sep = "\n"
      )
    }
  })
}

# load model input and output parameters
if (is.null(errMsg)) {
  modelIn <- config[["inputSymbols"]]
  names(modelIn) <- tolower(names(modelIn))

  modelOut <- config[["outputSymbols"]]
  names(modelOut) <- tolower(names(modelOut))

  # TODO: Update API version when dataContract is used elsewhere than in Configuration Mode
  dataContract <- list(
    inputSymbols = config[["inputSymbols"]],
    outputSymbols = config[["outputSymbols"]]
  )

  config[["inputSymbols"]] <- NULL
  config[["outputSymbols"]] <- NULL

  if (!length(config$pageTitle) || nchar(config$pageTitle) == 0L) {
    config$pageTitle <- config$modelTitle
  }
  config$modelTitle <- NULL
  # rename input and output scalar aliases
  if (length(modelIn[[scalarsFileName]])) {
    modelIn[[scalarsFileName]]$alias <- lang$nav$scalarAliases$scalars
    modelIn[[scalarsFileName]]$headers[[1]]$alias <- lang$nav$scalarAliases$cols$name
    modelIn[[scalarsFileName]]$headers[[2]]$alias <- lang$nav$scalarAliases$cols$desc
    modelIn[[scalarsFileName]]$headers[[3]]$alias <- lang$nav$scalarAliases$cols$value
  }
  if (length(modelOut[[scalarsOutName]])) {
    modelOut[[scalarsOutName]]$count <- length(modelOut[[scalarsOutName]]$symnames)
    modelOut[[scalarsOutName]]$alias <- lang$nav$scalarAliases$scalarsOut
    modelOut[[scalarsOutName]]$headers[[1]]$alias <- lang$nav$scalarAliases$cols$name
    modelOut[[scalarsOutName]]$headers[[2]]$alias <- lang$nav$scalarAliases$cols$desc
    modelOut[[scalarsOutName]]$headers[[3]]$alias <- lang$nav$scalarAliases$cols$value
  }
  invalidAliases <- integer(0L)
  if (length(config[["overwriteAliases"]])) {
    overwriteSymNames <- names(config[["overwriteAliases"]])
    for (idx in seq_along(config[["overwriteAliases"]])) {
      i <- match(overwriteSymNames[idx], names(modelIn))
      if (!is.na(i)) {
        modelIn[[i]]$alias <- config[["overwriteAliases"]][[idx]][["newAlias"]]
        next
      }
      i <- match(overwriteSymNames[idx], names(modelOut))
      if (!is.na(i)) {
        modelOut[[i]]$alias <- config[["overwriteAliases"]][[idx]][["newAlias"]]
        next
      }
      i <- match(overwriteSymNames[idx], modelIn[[scalarsFileName]]$symnames)
      if (!is.na(i)) {
        modelIn[[scalarsFileName]]$symtext[i] <- config[["overwriteAliases"]][[idx]][["newAlias"]]
        next
      }
      i <- match(overwriteSymNames[idx], modelOut[[scalarsOutName]]$symnames)
      if (!is.na(i)) {
        modelOut[[scalarsOutName]]$symtext[i] <- config[["overwriteAliases"]][[idx]][["newAlias"]]
        next
      }
      if (LAUNCHCONFIGMODE) {
        invalidAliases <- c(invalidAliases, idx)
      }
      warning(sprintf(
        "The alias of symbol: '%s' was selected to be overwritten. However, this symbol could not be found.",
        overwriteSymNames[idx]
      ), call. = FALSE)
      next
    }
    config[["overwriteAliases"]] <- NULL
  }
  invalidHeaderAliases <- integer(0L)
  if (length(config[["overwriteHeaderAliases"]])) {
    overwriteSymNames <- names(config[["overwriteHeaderAliases"]])
    for (idx in seq_along(config[["overwriteHeaderAliases"]])) {
      i <- match(names(config[["overwriteHeaderAliases"]])[[idx]], names(modelIn))
      if (is.na(i)) {
        i <- match(overwriteSymNames[idx], names(modelOut))
        newHeaders <- config[["overwriteHeaderAliases"]][[idx]][["newHeaders"]]
        if (is.na(i)) {
          if (LAUNCHCONFIGMODE) {
            invalidHeaderAliases <- c(invalidHeaderAliases, idx)
          }
          warning(sprintf(
            "The headers of symbol: '%s' were selected to be overwritten. However, this symbol could not be found.",
            overwriteSymNames[idx]
          ), call. = FALSE)
          next
        }
        if (length(modelOut[[i]]$headers) != length(newHeaders)) {
          if (!LAUNCHCONFIGMODE) {
            warning(sprintf(
              "The headers of symbol: '%s' were selected to be overwritten. However, the dimensions do not match!",
              overwriteSymNames[idx]
            ), call. = FALSE)
          }
          next
        }
        for (j in seq_along(modelOut[[i]]$headers)) {
          modelOut[[i]]$headers[[j]]$alias <- newHeaders[j]
        }
        next
      }
      newHeaders <- config[["overwriteHeaderAliases"]][[idx]][["newHeaders"]]
      if (length(modelIn[[i]]$headers) != length(newHeaders)) {
        warning(sprintf(
          "The headers of symbol: '%s' were selected to be overwritten. However, the dimensions do not match!",
          overwriteSymNames[idx]
        ), call. = FALSE)
        next
      }
      for (j in seq_along(modelIn[[i]]$headers)) {
        modelIn[[i]]$headers[[j]]$alias <- newHeaders[j]
      }
    }
    config[["overwriteHeaderAliases"]] <- NULL
  }
  scalarTableIds <- match(c(scalarsFileName, scalarEquationsName), names(modelIn))
  scalarTableIds <- scalarTableIds[!is.na(scalarTableIds)]
  for (scalarTableId in scalarTableIds) {
    modelIn[[scalarTableId]]$headers[[1]]$readonly <- TRUE
    modelIn[[scalarTableId]]$headers[[2]]$readonly <- TRUE
  }

  modelInRaw <- modelIn

  invalidWidgetsToRender <- character(0L)
  inputSymInForeignRenderers <- character(0L)

  for (el in names(config$inputWidgets)) {
    i <- match(tolower(el), names(modelIn))

    widgetConfig <- config$inputWidgets[[el]]
    widgetType <- widgetConfig$widgetType
    widgetConfig$widgetType <- NULL
    isScalarWidget <- is.na(i)
    if (isScalarWidget) {
      if (identical(widgetType, "table")) {
        errMsg <- paste(errMsg, sprintf(
          "The output type for the GAMS symbol: '%s' is not valid. This widget type can not represent 0 dimensional data.",
          names(modelIn)[i]
        ), sep = "\n")
        next
      }
      j <- NA
      if (tolower(scalarsFileName) %in% names(modelIn)) {
        j <- match(tolower(el), tolower(modelIn[[scalarsFileName]]$symnames))
      }
      i <- tolower(el)
      modelIn[[i]] <- list(isScalarDs = TRUE)

      if (!is.na(j)) {
        if (is.null(widgetConfig[["alias"]])) {
          modelIn[[i]]$alias <- modelIn[[scalarsFileName]]$symtext[j]
        } else {
          modelIn[[i]]$alias <- widgetConfig[["alias"]]
          widgetConfig$alias <- NULL
        }
        modelIn[[tolower(scalarsFileName)]]$symnames <- modelIn[[tolower(scalarsFileName)]]$symnames[-c(j)]
        if (!length(modelIn[[tolower(scalarsFileName)]]$symnames)) {
          # remove scalar table entirely if no scalar symbols are left
          if (scalarsFileName %in% names(config$inputWidgets)) {
            errMsg <- paste(errMsg,
              "You specified an input widget for the scalars table.\nHowever, all scalar symbols are also defined as widgets, thus the scalar table was removed.\nPlease either remove the widget for the scalars table or some of the scalar widgets from your configuration.",
              sep = "\n"
            )
          }
          modelIn[[tolower(scalarsFileName)]] <- NULL
          scalarIdOverwriteSheetOrder <- match(scalarsFileName, config$overwriteSheetOrder$input)
          if (!is.na(scalarIdOverwriteSheetOrder)) {
            config$overwriteSheetOrder$input <- config$overwriteSheetOrder$input[-scalarIdOverwriteSheetOrder]
          }
        } else {
          modelIn[[tolower(scalarsFileName)]]$symtypes <- modelIn[[tolower(scalarsFileName)]]$symtypes[-c(j)]
          modelIn[[tolower(scalarsFileName)]]$symtext <- modelIn[[tolower(scalarsFileName)]]$symtext[-c(j)]
        }
      } else if (any(startsWith(el, c(prefixDDPar, prefixGMSOpt)))) {
        if (!is.null(widgetConfig[["alias"]])) {
          modelIn[[i]]$alias <- widgetConfig[["alias"]]
          widgetConfig$alias <- NULL
        } else if (startsWith(el, prefixGMSOpt)) {
          modelIn[[i]]$alias <- substr(toupper(el), 9L, nchar(el))
        } else {
          modelIn[[i]]$alias <- paste0("--", toupper(substr(el, 9L, nchar(el))))
        }
      } else if (LAUNCHCONFIGMODE) {
        invalidWidgetsToRender <- c(invalidWidgetsToRender, el)
        modelIn[[i]] <- NULL
        i <- NA
      } else {
        errMsgTmp <- paste0("'", el, "' was defined to be an input widget, but is not part of the data contract! Start the Configuration Mode to reconfigure your app.")
        errMsg <- paste(errMsg, errMsgTmp, sep = "\n")
        modelIn[[i]] <- NULL
        i <- NA
      }
    } else {
      if (!is.null(widgetConfig[["alias"]])) {
        modelIn[[i]]$alias <- widgetConfig[["alias"]]
        widgetConfig$alias <- NULL
      }
    }
    if (!is.na(i)) {
      symDim <- length(modelIn[[i]]$headers)
      if (symDim > 1L && !(widgetType %in% c("table", "custom"))) {
        if (!(identical(widgetType, "dropdown") && identical(symDim, 2L) && identical(modelIn[[i]]$headers[[2]]$type, "string"))) {
          errMsg <- paste(errMsg, sprintf(
            "The output type for the GAMS symbol: '%s' is not valid. This widget type can not represent multi-dimensional data.",
            el
          ), sep = "\n")
          next
        }
      }
      if (identical(symDim, 1L) && !(widgetType %in% c("table", "custom", "dropdown"))) {
        errMsg <- paste(errMsg, sprintf(
          "The output type for the GAMS symbol: '%s' is not valid. This widget type can not represent 1 dimensional data.",
          el
        ), sep = "\n")
        next
      }
      if (!is.null(widgetConfig$options)) {
        modelIn[[i]]$options <- widgetConfig$options
        widgetConfig$options <- NULL
      }
      if (identical(widgetType, "table")) {
        if (identical(widgetConfig$bigData, TRUE) || identical(widgetConfig$tableType, "bigdata")) {
          modelIn[[i]]$dtHeaders <- TRUE
        } else if (identical(widgetConfig$tableType, "pivot")) {
          if (sum(vapply(modelIn[[i]]$headers, function(header) {
            return(identical(header$type, "numeric"))
          }, logical(1L))) > 1L) {
            errMsg <- paste(errMsg, sprintf(
              "The GAMS symbol: %s is declared as table. This causes the last column to be pivoted in MIRO.\nMIRO pivot cannot be used with already pivoted symbols.\nPlease declare the symbol as a parameter instead.",
              el
            ), sep = "\n")
            next
          }
          modelIn[[i]]$type <- "custom"
          modelIn[[i]]$rendererName <- "miroPivot"
          modelIn[[i]]$options$readonly <- identical(widgetConfig[["readonly"]], TRUE)
          modelIn[[i]]$options <- c(
            modelIn[[i]]$options,
            list(
              "_input_" = TRUE,
              "_metadata_" = list(
                symname = el,
                headers = modelIn[[i]]$headers,
                symtype = modelIn[[i]]$symtype
              )
            )
          )
          validGraphConfig <- validateGraphConfig(modelIn[[i]], skipOutTypeCheck = TRUE)
          if (!identical(validGraphConfig, TRUE)) {
            errMsg <- paste(errMsg, paste0(
              "Invalid widget config for symbol '", el,
              "': ", validGraphConfig, "."
            ), sep = "\n")
            next
          }
          if (is.null(config$dataRendering[[el]])) {
            if (length(config$dataRendering)) {
              config$dataRendering[[el]] <- list(outType = "datatable")
            } else {
              config$dataRendering <- list(list(outType = "datatable"))
              names(config$dataRendering) <- el
            }
          }
        } else if (!is.null(widgetConfig$dropdownCols)) {
          colNames <- names(widgetConfig$dropdownCols)
          if (any(!colNames %in% names(modelIn[[i]]$headers))) {
            errMsg <- paste(errMsg, sprintf(
              "GAMS symbol: '%s': Invalid column name(s): '%s' in configuration for dropdown columns.",
              names(modelIn)[i], paste(colNames[!colNames %in% names(modelIn[[i]]$headers)], collapse = "', '")
            ),
            sep = "\n"
            )
            next
          }
          pivotColId <- NULL
          if (length(widgetConfig$pivotCols) && any(colNames %in% widgetConfig$pivotCols)) {
            errMsg <- paste(errMsg, sprintf(
              "The column: '%s' of the GAMS symbol: '%s': cannot be declared both as a pivot column and a drop-down column!",
              paste(colNames[colNames %in% widgetConfig$pivotCols], collapse = "', '"),
              el
            ),
            sep = "\n"
            )
          }
          hasErr <- FALSE
          widgetConfig$dropdownCols <- lapply(names(widgetConfig$dropdownCols), function(dropdownCol) {
            dataSource <- widgetConfig$dropdownCols[[dropdownCol]]
            ddColId <- match(dropdownCol, names(modelIn[[i]]$headers))
            if (length(widgetConfig$pivotCols) &&
              match(widgetConfig$pivotCols[1], names(modelIn[[i]]$headers)) <= ddColId) {
              # need to adjust id in case column before is pivoted
              ddColId <- ddColId - 1L
            }
            if (length(dataSource$static)) {
              return(list(
                static = dataSource$static,
                type = if (identical(dataSource$colType, "dropdown")) {
                  "dropdown"
                } else {
                  "autocomplete"
                },
                ddColId = ddColId
              ))
            }
            if (!dataSource$symbol %in% names(modelIn)) {
              errMsg <<- paste(errMsg, sprintf(
                "The GAMS symbol: '%s' defined as data source for symbol: '%s' does not exist in data contract!",
                dataSource$symbol, el
              ),
              sep = "\n"
              )
              hasErr <<- TRUE
              return(NULL)
            }
            if (identical(dataSource$symbol, el)) {
              errMsg <<- paste(errMsg, sprintf("The GAMS symbol: '%s' has a data source (dropdown column) defined on itself!", names(modelIn)[i]), sep = "\n")
              hasErr <<- TRUE
              return(NULL)
            }
            colId <- match(dataSource$column, names(modelIn[[dataSource$symbol]]$headers))
            if (is.na(colId)) {
              errMsg <<- paste(errMsg, sprintf(
                "The GAMS symbol: '%s' defined as data source for symbol: '%s' does not have a column named: '%s'!",
                dataSource$symbol, el, dataSource$column
              ),
              sep = "\n"
              )
              hasErr <<- TRUE
              return(NULL)
            }
            if (length(config$inputWidgets[[dataSource$symbol]]) &&
              length(config$inputWidgets[[dataSource$symbol]]$pivotCols)) {
              pivotColId <- match(
                config$inputWidgets[[dataSource$symbol]]$pivotCols[1],
                names(modelIn[[dataSource$symbol]]$headers)
              )
              if (identical(colId, pivotColId)) {
                errMsg <<- paste(errMsg, sprintf(
                  "The column: '%s' of GAMS symbol: '%s' is defined as data source for symbol: '%s'. However, this data source is a pivoted column and can therefore not be used!",
                  dataSource$column, dataSource$symbol, el
                ),
                sep = "\n"
                )
                hasErr <<- TRUE
                return(NULL)
              }
              if (!is.na(pivotColId) && pivotColId <= colId) {
                # need to adjust id in case column before is pivoted
                colId <- colId - 1L
              }
            }
            return(list(
              symbol = dataSource$symbol, colId = colId,
              type = if (identical(dataSource$colType, "dropdown")) {
                "dropdown"
              } else {
                "autocomplete"
              },
              ddColId = ddColId
            ))
          })
          if (hasErr) {
            next
          }
          modelIn[[i]]$dropdownCols <- widgetConfig$dropdownCols
          widgetConfig$dropdownCols <- NULL
        }
        if (identical(widgetConfig$tableType, "default")) {
          colNames <- names(widgetConfig$validateCols)
          if (any(!colNames %in% names(modelIn[[i]]$headers))) {
            errMsg <- paste(errMsg, sprintf(
              "GAMS symbol: '%s': Invalid column name(s): '%s' in configuration for columns to validate.",
              el, paste(colNames[!colNames %in% names(modelIn[[i]]$headers)], collapse = "', '")
            ),
            sep = "\n"
            )
            next
          }
          if (length(widgetConfig$pivotCols) && any(colNames %in% widgetConfig$pivotCols)) {
            errMsg <- paste(errMsg, sprintf(
              "The column: '%s' of the GAMS symbol: '%s': cannot be declared both as a pivot column and a column to validate!",
              paste(colNames[colNames %in% widgetConfig$pivotCols], collapse = "', '"),
              el
            ),
            sep = "\n"
            )
          }
          hasErr <- FALSE
          widgetConfig$validateCols <- lapply(names(widgetConfig$validateCols), function(validateCol) {
            validateConf <- widgetConfig$validateCols[[validateCol]]
            colId <- match(validateCol, names(modelIn[[i]]$headers))
            if (length(widgetConfig$pivotCols) &&
              match(widgetConfig$pivotCols[1], names(modelIn[[i]]$headers)) <= colId) {
              # need to adjust id in case column before is pivoted
              colId <- colId - 1L
            }
            if (length(validateConf$min) && length(validateConf$max) && as.numeric(validateConf$min) > as.numeric(validateConf$max)) {
              errMsg <<- paste(errMsg, sprintf(
                "Column: '%s' of the GAMS symbol: '%s': For the column validation, a minimum value was specified that is greater than the specified maximum value!",
                validateCol, el
              ),
              sep = "\n"
              )
              hasErr <<- TRUE
              return(NULL)
            }
            if (length(validateConf$choices) && length(validateConf$exclude) && any(validateConf$choices %in% validateConf$exclude)) {
              errMsg <<- paste(errMsg, sprintf(
                "Column: '%s' of the GAMS symbol: '%s': The column validation has values that are declared as choices and should be excluded at the same time!",
                validateCol, el
              ),
              sep = "\n"
              )
              hasErr <<- TRUE
              return(NULL)
            }
            return(list(
              colId = colId,
              min = validateConf$min,
              max = validateConf$max,
              choices = validateConf$choices,
              exclude = validateConf$exclude,
              allowInvalid = validateConf$allowInvalid
            ))
          })
          if (hasErr) {
            next
          }
          modelIn[[i]]$validateCols <- widgetConfig$validateCols
          widgetConfig$validateCols <- NULL
        }
      }
      if (!is.null(widgetConfig$noHcube)) {
        modelIn[[i]]$noHcube <- widgetConfig$noHcube
        widgetConfig$noHcube <- NULL
      }
      if (!is.null(widgetConfig$noImport)) {
        modelIn[[i]]$noImport <- widgetConfig$noImport
        widgetConfig$noImport <- NULL
      }
      if (isTRUE(widgetConfig$clearValue)) {
        config$textOnlySymbols <- c(config$textOnlySymbols, el)
        if (!identical(widgetType, "dropdown")) {
          widgetConfig$clearValue <- NULL
        }
      }
      if (!is.null(widgetConfig$rendererName)) {
        modelIn[[i]]$rendererName <- widgetConfig$rendererName
        if (length(widgetConfig$packages)) {
          modelIn[[i]]$packages <- widgetConfig$packages
        }
        widgetConfig$rendererName <- NULL
        widgetConfig$packages <- NULL
        if (identical(widgetConfig$apiVersion, 2L)) {
          modelIn[[i]]$apiVersion <- 2L
        } else {
          warningMsgTmp <- "API version 1 for custom input widgets is deprecated and will be removed with a future release of MIRO. Please migrate your input widgets to API version 2. Go here to find the latest documentation: https://gams.com/miro/configuration_json_only.html#custom-input-widgets"
          warning(warningMsgTmp, call. = FALSE)
          warningMsg <<- paste(warningMsg, warningMsgTmp, sep = "\n")
        }
        if (length(widgetConfig$additionalData)) {
          invalidDsNames <- !widgetConfig$additionalData %in% c(names(modelIn), names(config$inputWidgets))
          if (any(invalidDsNames) && !LAUNCHCONFIGMODE) {
            errMsg <- paste(errMsg, sprintf(
              "Invalid additional data for custom input widget: '%s' declared. The dataset(s): '%s' are not input widgets. If you want to include a scalar value as additional data, please declare it as a widget first.",
              el,
              paste(widgetConfig$additionalData[invalidDsNames], collapse = "', '")
            ),
            sep = "\n"
            )
            next
          }
          modelIn[[i]]$additionalData <- widgetConfig$additionalData
        }
        if (length(widgetConfig$widgetSymbols)) {
          widgetSymbolsTmp <- tolower(unique(widgetConfig$widgetSymbols))
          if (el %in% widgetSymbolsTmp) {
            widgetSymbolsTmp <- widgetSymbolsTmp[!widgetSymbolsTmp %in% el]
          }
          invalidDsNames <- !widgetConfig$widgetSymbols %in% c(
            names(config$inputWidgets),
            names(modelIn)
          )
          if (any(invalidDsNames) && !LAUNCHCONFIGMODE) {
            errMsg <- paste(errMsg, sprintf(
              "The symbol(s): %s declared for the custom widget of symbol: %s were not found in the list of input symbols.",
              paste(widgetConfig$widgetSymbols[invalidDsNames], collapse = "', '"),
              el
            ),
            sep = "\n"
            )
            next
          }
          modelIn[[i]]$widgetSymbols <- widgetSymbolsTmp
          invalidAdditionalData <- match(modelIn[[i]]$additionalData, modelIn[[i]]$widgetSymbols)
          invalidAdditionalData <- invalidAdditionalData[!is.na(invalidAdditionalData)]
          if (length(invalidAdditionalData) && !LAUNCHCONFIGMODE) {
            errMsg <- paste(errMsg, sprintf(
              "Invalid additional data for custom input widget: '%s' declared. Additional data: '%s' cannot be part of renderer symbols.",
              el,
              paste(modelIn[[i]]$additionalData[invalidAdditionalData], collapse = "', '")
            ),
            sep = "\n"
            )
            next
          }
          inputSymInForeignRenderers <- c(
            inputSymInForeignRenderers,
            setNames(
              widgetSymbolsTmp,
              rep.int(
                el,
                length(widgetSymbolsTmp)
              )
            )
          )
        }
      }
      if (!widgetType %in% c("table", "custom")) {
        modelIn[[i]]$headers <- NULL
        modelIn[[i]][[widgetType]] <- widgetConfig
        next
      }
      if (!is.null(widgetConfig[["label"]])) {
        modelIn[[i]]$label <- widgetConfig[["label"]]
        widgetConfig$label <- NULL
      }
      if (!is.null(widgetConfig[["hideIndexCol"]])) {
        modelIn[[i]]$hideIndexCol <- widgetConfig[["hideIndexCol"]]
        widgetConfig$hideIndexCol <- NULL
      }
      if (!is.null(widgetConfig[["readonly"]])) {
        modelIn[[i]]$readonly <- widgetConfig[["readonly"]]
        widgetConfig$readonly <- NULL
      }
      if (isTRUE(widgetConfig[["heatmap"]])) {
        modelIn[[i]]$heatmap <- TRUE
        widgetConfig$heatmap <- NULL
      }
      if (!is.null(widgetConfig[["pivotCols"]])) {
        modelIn[[i]]$pivotCols <- widgetConfig$pivotCols
        widgetConfig$pivotCols <- NULL
      }
      if (!is.null(widgetConfig[["fixedColumnsLeft"]])) {
        modelIn[[i]]$fixedColumnsLeft <- widgetConfig$fixedColumnsLeft
        widgetConfig$fixedColumnsLeft <- NULL
      }
      if (!is.null(widgetConfig[["colWidths"]])) {
        if (length(widgetConfig[["colWidths"]]) > 1) {
          if (length(modelIn[[i]]$pivotCols)) {
            errMsg <- paste(errMsg, sprintf(
              "Invalid configuration for input symbol '%s'. For a symbol with pivot columns (pivotCols) only one column width may be specified, which is valid for all columns.",
              el
            ),
            sep = "\n"
            )
            next
          }
          if (length(widgetConfig[["colWidths"]]) != length(modelIn[[i]]$headers)) {
            errMsg <- paste(errMsg, sprintf(
              "Invalid configuration for input symbol '%s'. The number of defined column widths (%s) does not match the number of symbol headers (%s).",
              el,
              length(widgetConfig[["colWidths"]]),
              length(modelIn[[i]]$headers)
            ),
            sep = "\n"
            )
            next
          }
        }
        modelIn[[i]]$colWidths <- widgetConfig$colWidths
        widgetConfig$colWidths <- NULL
      }
      if (!is.null(widgetConfig[["sortPivotCols"]])) {
        modelIn[[i]]$sortPivotCols <- widgetConfig$sortPivotCols
        widgetConfig$sortPivotCols <- NULL
      }
      if (length(widgetConfig$readonlyCols)) {
        colsArePivoted <- widgetConfig$readonlyCols %in% modelIn[[i]]$pivotCols
        if (any(colsArePivoted)) {
          modelIn[[i]]$pivotColIsReadonly <- TRUE
          widgetConfig$readonlyCols <- widgetConfig$readonlyCols[!colsArePivoted]
        }
        for (col in widgetConfig$readonlyCols) {
          if (col %in% names(modelIn[[i]]$headers)) {
            modelIn[[i]]$headers[[col]]$readonly <- TRUE
          } else {
            errMsg <- paste(errMsg, sprintf(
              "The column: '%s' of table: '%s' was set to be readonly. However, such a column does not exist in the table.",
              names(modelIn[[i]]$headers)[[j]], el,
            ))
            break
          }
        }
      }
      if (length(widgetConfig$colFormat)) {
        if (length(modelIn[[i]]$pivotCols)) {
          errMsg <- paste(errMsg, sprintf(
            "colFormat is not supported when pivotCols are active (table: %s).",
            el
          ))
          break
        }
        colIds <- match(names(widgetConfig$colFormat), names(modelIn[[i]]$headers))
        if (any(is.na(colIds))) {
          errMsg <- paste(errMsg, sprintf(
            "The column(s): '%s' of table: '%s' specified in colFormat does not exist.",
            paste(names(widgetConfig$colFormat)[is.na(colIds)], collapse = ", "), el
          ))
          break
        }
        modelIn[[i]]$colFormat <- lapply(seq_along(widgetConfig$colFormat), function(i) {
          formatTmp <- widgetConfig$colFormat[[i]]
          formatTmp$colId <- colIds[i]
          return(formatTmp)
        })
      }
    }
  }
  config$inputWidgets <- NULL
  # make sure two input or output data sheets dont share the same name (case insensitive)
  if (any(duplicated(names(modelIn)))) {
    errMsg <- paste(errMsg, "Two or more input datasets share the same name. Please make sure the identifiers are unique for each input datasheet!", sep = "\n")
  }
  if (any(duplicated(names(modelOut)))) {
    errMsg <- paste(errMsg, "Two or more output datasets share the same name. Please make sure the identifiers are unique for each output datasheet!", sep = "\n")
  }
  if (length(inputSymInForeignRenderers)) {
    if (any(duplicated(inputSymInForeignRenderers))) {
      errMsg <- paste(errMsg, sprintf(
        "You cannot define the same input symbol(s) ('%s') for multiple (custom) widgets.",
        paste(inputSymInForeignRenderers[duplicated(inputSymInForeignRenderers)],
          collapse = "', '"
        )
      ), sep = "\n")
    } else {
      for (i in seq_along(inputSymInForeignRenderers)) {
        modelIn[[inputSymInForeignRenderers[[i]]]]$definedByExternalSymbol <- names(inputSymInForeignRenderers)[[i]]
      }
    }
  }
}

if (is.null(errMsg)) {
  # declare GAMS double dash parameters and GAMS options
  DDPar <- names(modelIn)[startsWith(names(modelIn), prefixDDPar)]
  GMSOpt <- names(modelIn)[startsWith(names(modelIn), prefixGMSOpt)]
  if (any(nchar(DDPar) <= nchar(prefixDDPar))) {
    errMsg <- "Unnamed Double Dash Parameter(s) detected. Empty names are not allowed!"
  }
  if (any(nchar(GMSOpt) <= nchar(prefixGMSOpt) | GMSOpt %in% paste0(prefixGMSOpt, reservedGMSOpt))) {
    errMsg <- sprintf("Invalid GAMS option(s) detected. GAMS options must not be unnamed and must not be one of the reserved options: '%s'!", paste(reservedGMSOpt, collapse = "', '"))
  }

  modelInToImport <- getInputToImport(modelIn, keywordsNoImport)
  modelInMustImport <- getInputToImport(modelIn, keywordsNoMustImport)
  # declare input and output aliases
  modelInAlias <- vapply(seq_along(modelIn), function(i) {
    if (is.null(modelIn[[i]]$alias)) {
      names(modelIn)[[i]]
    } else {
      modelIn[[i]]$alias
    }
  }, character(1L), USE.NAMES = FALSE)
  modelInToImportAlias <- vapply(seq_along(modelInToImport), function(i) {
    if (is.null(modelInToImport[[i]]$alias)) {
      names(modelInToImport)[[i]]
    } else {
      modelInToImport[[i]]$alias
    }
  }, character(1L), USE.NAMES = FALSE)
  modelOutAlias <- vapply(seq_along(modelOut), function(i) {
    if (is.null(modelOut[[i]]$alias)) {
      names(modelOut)[[i]]
    } else {
      modelOut[[i]]$alias
    }
  }, character(1L), USE.NAMES = FALSE)
  # add input type to list
  lapply(seq_along(modelIn), function(i) {
    tryCatch(
      {
        modelIn[[i]]$type <<- getInputType(modelIn[[i]],
          keywordsType = keywordsType,
          modelIn[[i]]$dropdown$multiple
        )
        if (names(modelIn)[[i]] %in% c(DDPar, GMSOpt) &&
          modelIn[[i]]$type %in% c("hot", "dt", "custom")) {
          stop(sprintf("Tables are not supported for GAMS command line parameters ('%s').
                     Please specify another widget type.", modelInAlias[i]))
        }
        if (identical(modelIn[[i]]$type, "checkbox") && is.character(modelIn[[i]]$checkbox$max)) {
          cbValueTmp <- strsplit(modelIn[[i]]$checkbox$max, "\\(|\\$")[[1]]
          if (length(cbValueTmp) %in% c(4L, 5L) && cbValueTmp[[1]] %in% listOfOperators) {
            cbValueTmp <- gsub(")", "", cbValueTmp, fixed = TRUE)
            modelIn[[i]]$checkbox$operator <<- cbValueTmp[[1]]
            modelIn[[i]]$checkbox$max <<- paste(cbValueTmp[c(-1)], collapse = "$")
          } else {
            stop(sprintf(
              "The checkbox: '%s' has a bad dependency format. Format for checkboxes dependent on other datasets should be:
                       operator(dataset$column) or operator(dataset$keyColumn[key]$valueColumn).
                       Currently, the following operators are supported: '%s'.", modelInAlias[i],
              paste(listOfOperators, collapse = "', '")
            ))
          }
        }
      },
      error = function(e) {
        errMsg <<- paste(errMsg, paste0(
          modelInAlias[i],
          " has no valid input type defined. Error message: ",
          e
        ), sep = "\n")
      }
    )
  })
}
if (is.null(errMsg)) {
  # declare hidden output scalars and remove entire scalar table if all hidden
  if (length(config$hiddenOutputScalars)) {
    config$hiddenOutputScalars <- tolower(config$hiddenOutputScalars)
    outScalarTmp <- character(0L)
    if (scalarsOutName %in% names(modelOut)) {
      outScalarTmp <- modelOut[[scalarsOutName]]$symnames
    }
    isValidScalar <- config$hiddenOutputScalars %in% outScalarTmp
    if (any(!isValidScalar)) {
      errMsg <- paste(errMsg, sprintf(
        "Some output scalars you declared to be hidden were not defined in your GAMS model as scalars to be displayed in MIRO.
These scalars are: '%s'. Please either add them in your model or remove them from the list of hidden output scalars in your '%s.json' file.",
        config$hiddenOutputScalars[!config$hiddenOutputScalars %in% names(modelOut)], modelName
      ), sep = "\n")
    } else if (length(outScalarTmp) > 0L && identical(sum(isValidScalar), length(outScalarTmp))) {
      modelOut[[scalarsOutName]]$hidden <- TRUE
    }
    rm(outScalarTmp, isValidScalar)
  }

  if (length(config$overwriteSheetOrder$input) && !LAUNCHCONFIGMODE) {
    isWidgetGroup <- startsWith(config$overwriteSheetOrder$input, "_widgets")
    if (any(isWidgetGroup)) {
      isInputWidget <- vapply(modelIn, function(el) {
        if (el$type %in% c("hot", "dt", "custom") || length(el$definedByExternalSymbol)) {
          return(FALSE)
        }
        return(TRUE)
      }, logical(1L), USE.NAMES = FALSE)
      if (any(isInputWidget)) {
        remainingWidgetsUnassigned <- TRUE
        for (widgetGroupId in which(isWidgetGroup)) {
          i <- suppressWarnings(as.integer(substring(config$overwriteSheetOrder$input[widgetGroupId], 9L)))
          if (is.na(i)) {
            if (remainingWidgetsUnassigned) {
              if (length(config$inputWidgetGroups)) {
                firstUnassignedWidgetId <- which(!names(modelIn)[isInputWidget] %in% unlist(lapply(config$inputWidgetGroups, function(el) {
                  return(el$members)
                }), use.names = FALSE))[1]
                if (is.na(firstUnassignedWidgetId)) {
                  config$overwriteSheetOrder$input[widgetGroupId] <- NA
                  remainingWidgetsUnassigned <- FALSE
                  next
                }
              } else {
                firstUnassignedWidgetId <- 1L
              }
              config$overwriteSheetOrder$input[widgetGroupId] <- names(modelIn)[isInputWidget][firstUnassignedWidgetId]
              remainingWidgetsUnassigned <- FALSE
            }
            next
          } else if (i > length(config$inputWidgetGroups)) {
            if (remainingWidgetsUnassigned && sum(isWidgetGroup) <= length(config$inputWidgetGroups)) {
              if (length(config$inputWidgetGroups)) {
                firstUnassignedWidgetId <- which(!names(modelIn)[isInputWidget] %in% unlist(lapply(config$inputWidgetGroups, function(el) {
                  return(el$members)
                }), use.names = FALSE))[1]
                if (is.na(firstUnassignedWidgetId)) {
                  config$overwriteSheetOrder$input[widgetGroupId] <- NA
                  remainingWidgetsUnassigned <- FALSE
                  next
                }
              } else {
                firstUnassignedWidgetId <- 1L
              }
              config$overwriteSheetOrder$input[widgetGroupId] <- names(modelIn)[isInputWidget][firstUnassignedWidgetId]
              remainingWidgetsUnassigned <- FALSE
            } else {
              config$overwriteSheetOrder$input[widgetGroupId] <- NA
            }
            next
          }
          widgetIdTmp <- match(config$inputWidgetGroups[[i]]$members[1], names(modelIn))
          if (is.na(widgetIdTmp)) {
            errMsg <- paste(errMsg, sprintf(
              "The widget: '%s' of the input widget group: '%s' does not exist.",
              config$inputWidgetGroups[[i]]$members[1], config$inputWidgetGroups[[i]]$name
            ),
            sep = "\n"
            )
            break
          }
          # remove sets that are members of this group from overwriteSheetOrder
          # in order to not mess up the order
          config$overwriteSheetOrder$input[config$overwriteSheetOrder$input %in%
            config$inputWidgetGroups[[i]]$members] <- NA
          config$overwriteSheetOrder$input[widgetGroupId] <- names(modelIn)[widgetIdTmp]
          i <- i + 1L
        }
        config$overwriteSheetOrder$input <- config$overwriteSheetOrder$input[!is.na(config$overwriteSheetOrder$input)]
      } else {
        config$overwriteSheetOrder$input <- config$overwriteSheetOrder$input[-which(isWidgetGroup)]
      }
    }
    invalidOverwriteSheetOrderElements <- is.na(match(config$overwriteSheetOrder$input, names(modelIn)))
    if (any(invalidOverwriteSheetOrderElements)) {
      errMsg <- paste(errMsg,
        sprintf(
          "Some of the input elements in the 'overwriteSheetOrder' option are not defined in the data contract: %s!",
          paste(config$overwriteSheetOrder$input[invalidOverwriteSheetOrderElements], collapse = ", ")
        ),
        sep = "\n"
      )
      inputSheetIdsToDisplay <- integer()
    } else {
      appendSheetIds <- NULL
      if (length(config$overwriteSheetOrder$input) != length(modelIn)) {
        appendSheetIds <- seq_along(modelIn)[!names(modelIn) %in% config$overwriteSheetOrder$input]
      }
      inputSheetIdsToDisplay <- c(match(
        unique(config$overwriteSheetOrder$input),
        names(modelIn)
      ), appendSheetIds)
    }
  } else {
    inputSheetIdsToDisplay <- seq_along(modelIn)
  }
  scenInputSheetIdsToDisplay <- inputSheetIdsToDisplay
}
if (is.null(errMsg)) {
  scenWidgetIds <- lapply(inputSheetIdsToDisplay, function(i) {
    if (modelIn[[i]]$type %in% c("hot", "dt", "custom")) {
      return(NA)
    }
    return(i)
  })
  additionalScenWidgetIds <- lapply(inputSheetIdsToDisplay[is.na(scenWidgetIds)], function(i) {
    if (identical(modelIn[[i]]$isScalarDs, TRUE)) {
      return(i)
    }
    return(NA)
  })
  additionalScenWidgetIds <- unlist(additionalScenWidgetIds[!is.na(additionalScenWidgetIds)], use.names = FALSE)
  scenWidgetIds <- unlist(scenWidgetIds[!is.na(scenWidgetIds)], use.names = FALSE)
  if (length(inputSymInForeignRenderers)) {
    symIdsForeignRenderers <- match(inputSymInForeignRenderers, names(modelIn))
    inputSheetIdsToDisplay <- inputSheetIdsToDisplay[!inputSheetIdsToDisplay %in% symIdsForeignRenderers]
    widgetIds <- scenWidgetIds[!scenWidgetIds %in% symIdsForeignRenderers]
  } else {
    widgetIds <- scenWidgetIds
  }

  scenWidgetIds <- c(scenWidgetIds, additionalScenWidgetIds)

  # Hypercube Mode configuration
  if (config$activateModules$hcube) {
    if (!length(config$hcubeWidgetGroups)) {
      config$hcubeWidgetGroups <- list(list(name = NULL, members = names(modelIn)))
    }
    for (el in names(config$hcubeWidgets)) {
      if (!el %in% names(modelIn)) {
        errMsg <- paste(errMsg, sprintf(
          "Unknown symbol: '%s' defined as a Hypercube widget.",
          el
        ))
      } else if (!identical(modelIn[[el]]$isScalarDs, TRUE)) {
        errMsg <- paste(errMsg, sprintf(
          "Hypercube widgets can only be defined for scalar symbols. '%s' is not a scalar.",
          el
        ))
      } else {
        widgetType <- config$hcubeWidgets[[el]]$widgetType
        config$hcubeWidgets[[el]]$type <- widgetType
        config$hcubeWidgets[[el]][[widgetType]] <- config$hcubeWidgets[[el]]
      }
    }
    hcubeWidgetsAlreadyAssigned <- c()
    hcWidgetsTmp <- lapply(seq_along(modelIn), function(widgetId) {
      el <- names(modelIn)[[widgetId]]
      if (el %in% names(config$hcubeWidgets)) {
        widgetConfig <- config$hcubeWidgets[[el]]
      } else {
        widgetConfig <- modelIn[[el]]
      }
      if (isTRUE(widgetConfig$noHcube)) {
        return(NA)
      }
      if (identical(widgetConfig$type, "checkbox")) {
        return(list(
          type = "dropdown",
          baseType = "checkbox",
          alias = widgetConfig$checkbox$alias,
          name = el,
          widgetId = widgetId,
          label = widgetConfig$checkbox$label,
          choices = c(0L, 1L),
          selected = widgetConfig$checkbox$value,
          multiple = TRUE
        ))
      }
      if (identical(widgetConfig$type, "dropdown")) {
        if (isTRUE(widgetConfig$dropdown$multiple)) {
          return(NA)
        }
        if (identical(widgetConfig$symtype, "set")) {
          return(NA)
        }
        ret <- widgetConfig$dropdown
        ret$type <- "dropdown"
        ret$baseType <- "dropdown"
        ret$multiple <- TRUE
        ret$name <- el
        ret$widgetId <- widgetId
        if (length(ret$aliases)) {
          ret$choices <- setNames(ret$choices, ret$aliases)
        }
        return(ret)
      }
      if (identical(widgetConfig$type, "slider")) {
        ret <- widgetConfig$slider
        ret$type <- "slider"
        ret$baseType <- "slider"
        ret$name <- el
        ret$widgetId <- widgetId
        ret$ticks <- !isFALSE(ret$ticks)
        if (!length(ret$minStep)) {
          ret$minStep <- 0L
        }
        if (length(ret$default) == 1) {
          ret$single <- TRUE
          ret$default <- rep(ret$default, 2L)
        } else {
          ret$single <- FALSE
        }
        return(ret)
      }
      return(NA)
    })
    names(hcWidgetsTmp) <- names(modelIn)
    hcWidgetsTmp <- hcWidgetsTmp[!is.na(hcWidgetsTmp)]
    config$hcModule$scalarsConfig <- lapply(config$hcubeWidgetGroups, function(group) {
      membersAlreadyAssigned <- group$members %in% hcubeWidgetsAlreadyAssigned
      if (any(membersAlreadyAssigned)) {
        warning(sprintf(
          "Widget(s): '%s' are declared as members of multiple Hypercube widget groups.",
          paste(group$members[membersAlreadyAssigned], collapse = "', '")
        ))
      }
      groupMembers <- group$members[!membersAlreadyAssigned]
      widgetIndices <- match(groupMembers, names(hcWidgetsTmp))
      isValidMember <- !is.na(widgetIndices)
      widgetIndices <- widgetIndices[isValidMember]
      if (!length(widgetIndices)) {
        return(NA)
      }
      membersTmp <- hcWidgetsTmp[widgetIndices]
      hcubeWidgetsAlreadyAssigned <<- c(hcubeWidgetsAlreadyAssigned, group$members[isValidMember])
      return(list(name = group$name, members = membersTmp))
    })
    config$hcModule$scalarsConfig <- config$hcModule$scalarsConfig[!is.na(config$hcModule$scalarsConfig)]
    unassignedWidgets <- !names(hcWidgetsTmp) %in% hcubeWidgetsAlreadyAssigned
    if (any(unassignedWidgets)) {
      warningMsgTmp <- sprintf(
        "Widget(s): '%s' are not assigned to any Hypercube widget group.",
        paste(names(hcWidgetsTmp)[unassignedWidgets], collapse = "', '")
      )
      warningMsg <- paste(warningMsg, warningMsgTmp, sep = "\n")
      warning(warningMsgTmp)
      config$hcModule$scalarsConfig <- c(
        list(list(name = NULL, members = hcWidgetsTmp[unassignedWidgets])),
        config$hcModule$scalarsConfig
      )
    }
    if (!length(config$hcModule$scalarsConfig)) {
      warningMsgTmp <- "You have selected to enable the Hypercube module, but no widgets could be found that are suitable for use with the Hypercube module. Hypercube module has therefore been disabled..."
      warning(warningMsgTmp)
      warningMsg <- paste(warningMsg, warningMsgTmp, sep = "\n")
      config$activateModules$hcube <- FALSE
    }
  }
  scalarSymbolsBase <- character(0L)

  config$inputGroups <- c(
    config$inputGroups,
    genWidgetGroups(names(modelIn)[widgetIds],
      config$inputWidgetGroups,
      lang$nav$inputScreen$widgetTabTitle,
      aggregateWidgets = isTRUE(config$aggregateWidgets),
      inputGroups = config$inputGroups
    )
  )
  inputTabs <- getTabs(names(modelIn), modelInAlias, config$inputGroups,
    idsToDisplay = inputSheetIdsToDisplay, widgetIds = widgetIds
  )
  inputTabTitles <- inputTabs$tabTitles
  config$inputTabSettings <- inputTabs$tabSettings
  tabSheetMap <- list(input = NULL, output = NULL)
  tabSheetMap$input <- inputTabs$tabSheetMap
  inputTabs <- inputTabs$tabs
  # get input tabs where scalars are merged to single table (scenario comparison mode)
  scenInputTabs <- getTabs(names(modelIn), modelInAlias, config$inputGroups,
    idsToDisplay = scenInputSheetIdsToDisplay,
    widgetIds = scenWidgetIds, scalarsTabName = lang$nav$scalarAliases$scalars,
    mergeScalars = TRUE,
    widgetIdsMultiDim = vapply(scenWidgetIds, function(widgetId) {
      if (names(modelIn)[widgetId] %in% names(modelInRaw)) {
        return(widgetId)
      } else {
        return(0L)
      }
    }, integer(1L), USE.NAMES = FALSE)
  )
  scenInputTabTitles <- scenInputTabs$tabTitles
  scenInputTabs <- scenInputTabs$tabs

  # read graph data for input and output sheets
  config$activateModules$miroLogFile <- length(config$miroLogFile) > 0L &&
    nchar(config$miroLogFile) > 2L

  # get input sheets with dependencies on other sheets
  # get dropdown dependencies

  ddownDep <- list()
  choicesNoDep <- list()
  aliasesNoDep <- list()
  modelInWithDep <- list()
  dependentDatasets <- vector("list", length = length(modelIn))

  modelInTabularData <- lapply(seq_along(modelIn), function(i) {
    name <- names(modelIn)[[i]]
    switch(modelIn[[i]]$type,
      dropdown = {
        # a dropdown menu cannot use the reserved name for the scalar table
        if (names(modelIn)[[i]] %in% c(scalarsFileName, scalarsOutName)) {
          errMsg <<- paste(errMsg, paste0(
            "The dropdown menu: '", modelInAlias[i],
            "' uses a reserved name as the identifier. Please choose a different name."
          ), sep = "\n")
        }
        # get dependencies
        tryCatch(
          {
            choices <- getDependenciesDropdown(choices = modelIn[[i]]$dropdown$choices, modelIn = modelIn, name = name)
            if (!is.null(modelIn[[i]]$dropdown$aliases)) {
              if (choices$hasDep) {
                aliases <- getDependenciesDropdown(choices = modelIn[[i]]$dropdown$aliases, modelIn = modelIn, name = name)
              } else {
                aliases <- list(strings = unlist(gsub("$$", "$", modelIn[[i]]$dropdown$aliases, fixed = TRUE),
                  use.names = FALSE
                ))
              }
            } else {
              aliases <- NULL
            }
          },
          error = function(e) {
            errMsg <<- paste(errMsg, paste0(
              "'", modelInAlias[i],
              "' has no valid input type defined. Error message: ",
              conditionMessage(e)
            ), sep = "\n")
          }
        )
        if (!is.null(errMsg)) {
          if (LAUNCHCONFIGMODE) {
            errMsg <<- NULL
            return(NULL)
          } else {
            return(NULL)
          }
        }

        # in case dropdown menu has aliases, validate that they are of matching length as choices
        if (!is.null(aliases)) {
          if (length(aliases$strings) != length(choices$strings)) {
            errMsg <<- paste(errMsg, paste0(
              "The number of fixed aliases for dropdown menu: ", modelInAlias[i],
              " does not match the number of choices without dependencies.
                                                Aliases: '", paste(aliases$strings, collapse = ","),
              "'. Choices: '", paste(choices$strings, collapse = ","), "'."
            ), sep = "\n")
            return(NULL)
          } else if (length(aliases$fw) != length(choices$fw)) {
            errMsg <<- paste(errMsg, paste0(
              "The number of aliases with dependencies for dropdown menu: ",
              modelInAlias[i], " does not match the number of choices with dependencies.
                                                Aliases: '", paste(aliases$fw, collapse = ","),
              "'. Choices: '", paste(choices$fw, collapse = ","), "'."
            ), sep = "\n")
            return(NULL)
            # sheet names of aliases and choices do not match
          } else if (any(vapply(names(aliases$fw), function(sheet) {
            if (sheet %in% names(choices$fw)) {
              return(F)
            } else {
              return(T)
            }
          }, logical(1)))) {
            errMsg <<- paste(errMsg, paste0("When both the choices and their aliases have dependencies on external data sheets, please make sure they depend on the same dataset.
                                                (dropdown menu: '", modelInAlias[i], "', aliases: '", paste(aliases$fw, collapse = ","), "', choices: '", paste(choices$fw, collapse = ","), "'."), sep = "\n")
            return(NULL)
          }
        }
        # set vector of choices to static ones
        modelIn[[i]]$dropdown$choices <<- choices$strings
        modelIn[[i]]$dropdown$aliases <<- aliases$strings
        ddownDep[[name]]$fw <<- choices$fw
        if (length(choices$bw)) {
          warningMsgTmp <- sprintf(
            paste0(
              "The symbol: %s is configurated with a backward dependency (filter on table). This feature was dropped with MIRO 2.8.",
              "Please use custom input widgets instead: https://www.gams.com/miro/configuration_json_only.html#custom-input-widgets"
            ), names(modelIn)[i]
          )
          warning(warningMsgTmp, call. = FALSE)
          warningMsg <<- paste(warningMsg, warningMsgTmp, sep = "\n")
        }
        ddownDep[[name]]$aliases <<- aliases$fw
        choicesNoDep[[name]] <<- choices$strings
        aliasesNoDep[[name]] <<- aliases$strings
        modelIn[[i]]$dropdown$dependencyConfig <<- list(
          fw = choices$fw,
          bw = choices$bw,
          aliases = aliases$fw,
          staticChoices = choices$strings,
          staticAliases = aliases$strings
        )
        if (length(choices$fw)) {
          modelInWithDep[[name]] <<- modelIn[i]
        }
        if (identical(modelIn[[i]]$symtype, "set")) {
          modelIn[[i]]$headers <<- modelInRaw[[name]]$headers
          modelIn[[i]]$symtype <<- "set"
          return(name)
        } else if (isTRUE(modelIn[[i]]$dropdown$multiple)) {
          modelIn[[i]]$symtype <<- "set"
          modelIn[[i]]$headers <<- list(list(
            type = "string",
            alias = modelIn[[i]]$alias
          ))
          names(modelIn[[i]]$headers) <<- names(modelIn)[[i]]
          return(name)
        } else {
          return(NULL)
        }
      },
      slider = {
        # a slider cannot use the reserved name for the scalar table
        if (names(modelIn)[[i]] %in% c(scalarsFileName, scalarsOutName)) {
          errMsg <<- paste(errMsg,
            paste0(
              "The slider: '", modelInAlias[i],
              "' uses a reserved name as its identifier. Please choose a different name."
            ),
            sep = "\n"
          )
        }
        tryCatch(
          {
            sliderValues <- getDependenciesSlider(
              min = modelIn[[i]]$slider$min,
              max = modelIn[[i]]$slider$max,
              def = modelIn[[i]]$slider$default,
              step = modelIn[[i]]$slider$step,
              modelIn = modelIn,
              listOfOperators = listOfOperators
            )
            # no dependencies for slider
            modelIn[[i]]$slider$hasDependency <<- !is.null(sliderValues)
            if (is.null(sliderValues)) {
              modelIn[[i]]$sliderConfig <- list(
                "min" = as.numeric(modelIn[[i]]$slider$min),
                "max" = as.numeric(modelIn[[i]]$slider$max),
                "def" = as.numeric(modelIn[[i]]$slider$default),
                "step" = as.numeric(modelIn[[i]]$slider$step)
              )
              if (suppressWarnings(any(is.na(sliderValues)))) {
                errMsg <<- paste(errMsg,
                  paste0(
                    "The slider: '", modelInAlias[i],
                    " has non numeric values. Please make sure min, max etc. are numeric."
                  ),
                  sep = "\n"
                )
              }
            } else {
              modelInWithDep[[name]] <<- modelIn[[i]]
              modelIn[[i]]$sliderConfig <<- sliderValues
            }
            if (length(modelIn[[i]]$slider$default) > 1L &&
              !identical(modelIn[[i]]$slider$single, TRUE)) {
              # double dash parameters declared as double sliders are automatically
              # expanded to 2 double dash parameters with suffixes _lo and _up
              # TODO: allow specifying any names for lower and upper limit
              # (possibly even 2 GAMS scalars)
              DDPar <<- c(DDPar[DDPar != name], paste0(name, c("_lo", "_up")))
            }
          },
          error = function(e) {
            errMsg <<- paste(errMsg, paste0(
              "'", modelInAlias[i],
              "' has no valid input type defined. Error message: ",
              conditionMessage(e)
            ), sep = "\n")
          }
        )
        return(NULL)
      },
      date = {
        if (names(modelIn)[[i]] %in% c(scalarsFileName, scalarsOutName)) {
          errMsg <<- paste(errMsg, paste0(
            "The date selector: '", modelInAlias[i],
            "' uses a reserved name as its identifier. Please choose a different name."
          ), sep = "\n")
        }
        # TODO : support dependency
        return(NULL)
      },
      daterange = {
        if (names(modelIn)[[i]] %in% c(scalarsFileName, scalarsOutName)) {
          errMsg <<- paste(errMsg, paste0(
            "The date range selector: '", modelInAlias[i],
            "' uses a reserved name as its identifier. Please choose a different name."
          ), sep = "\n")
        }
        # double dash parameters declared as dateranges are automatically
        # expanded to 2 double dash parameters with suffixes _lo and _up
        # TODO: allow specifying any names for lower and upper limit
        # (possibly even 2 GAMS scalars)
        DDPar <<- c(DDPar[DDPar != name], paste0(name, c("_lo", "_up")))
        # TODO : support dependency
        return(NULL)
      },
      textinput = {
        if (names(modelIn)[[i]] %in% c(scalarsFileName, scalarsOutName)) {
          errMsg <<- paste(errMsg, paste0(
            "The textInput: '", modelInAlias[i],
            "' uses a reserved name as its identifier. Please choose a different name."
          ), sep = "\n")
        }
        return(NULL)
      },
      numericinput = {
        if (names(modelIn)[[i]] %in% c(scalarsFileName, scalarsOutName)) {
          errMsg <<- paste(errMsg, paste0(
            "The numericInput: '", modelInAlias[i],
            "' uses a reserved name as its identifier. Please choose a different name."
          ), sep = "\n")
        }
        return(NULL)
      },
      checkbox = {
        if (names(modelIn)[[i]] %in% c(scalarsFileName, scalarsOutName)) {
          errMsg <<- paste(errMsg, paste0(
            "The checkbox: '", modelInAlias[i],
            "' uses a reserved name as its identifier. Please choose a different name."
          ), sep = "\n")
        }
        if (is.character(modelIn[[i]]$checkbox$max)) {
          # checkbox has dependency
          # BEGIN error checks
          if (grepl("\\$+$", modelIn[[i]]$checkbox$max)) {
            errMsg <<- paste(errMsg, paste0(
              "The checkbox: '", modelInAlias[i],
              "' has a backward dependency assigned. Currently only forward dependencies are supported for checkboxes."
            ), sep = "\n")
            return(NULL)
          }
          # END error checks

          cbValue <- strsplit(modelIn[[i]]$checkbox$max, "\\$")[[1]]
          idx1 <- match(cbValue[1], names(modelIn))[1]
          if (!is.na(idx1)) {
            # add forward dependency
            modelIn[[i]]$checkbox$sheetId <<- idx1
            tryCatch(modelIn[[i]]$checkbox$max <<- getNestedDep(cbValue[c(-1)]), error = function(e) {
              errMsg <<- paste(errMsg, conditionMessage(e))
            })

            modelInWithDep[[name]] <<- modelIn[[i]]
          } else {
            errMsg <<- paste(errMsg, paste0(
              "The dependent dataset: '", cbValue[1], "' for checkbox: '",
              modelInAlias[i], "' could not be found. Please make sure you define a valid reference."
            ), sep = "\n")
          }
        }
        return(NULL)
      },
      {
        if (identical(modelIn[[i]]$isScalarDs, TRUE)) {
          return(NULL)
        }
        # check that in case dataset is scalar ds, it has correct headers
        if (names(modelIn)[[i]] %in% c(scalarsFileName, scalarsOutName) &&
          !identical(names(modelIn[[i]]$headers), scalarsFileHeaders)) {
          warningMsgTmp <- paste0(
            modelInAlias[i], " is defined to be the scalar input dataset, ",
            "but has incorrect headers. The headers were adjusted accordingly."
          )
          warning(warningMsgTmp, call. = FALSE)
          warningMsg <<- paste(warningMsg, warningMsgTmp, sep = "\n")
          names(modelIn[[i]]$headers) <- scalarsFileHeaders
        }
        return(name)
      }
    )
  })
  dependentDatasets <- lapply(seq_along(modelIn), function(i) {
    dependentDataIds <- vapply(seq_along(ddownDep), function(j) {
      if (names(modelIn)[[i]] %in% names(ddownDep[[j]]$fw)) {
        return(match(names(ddownDep)[[j]], names(modelIn)))
      } else {
        return(NA_integer_)
      }
    }, integer(1L), USE.NAMES = FALSE)
    dependentDataIds <- dependentDataIds[!is.na(dependentDataIds)]
    if (!length(dependentDataIds)) {
      dependentDataIds <- NULL
    }
    return(dependentDataIds)
  })

  modelInTabularData <- unlist(modelInTabularData, use.names = FALSE)
  # get scalar input names
  scalarInputSym <- names(modelIn)[vapply(seq_along(modelIn), function(i) {
    if (!"headers" %in% names(modelIn[[i]]) ||
      isTRUE(modelIn[[i]]$dropdown$single) ||
      isTRUE(modelIn[[i]]$dropdown$checkbox)) {
      return(TRUE)
    }
    return(FALSE)
  }, logical(1L), USE.NAMES = FALSE)]
  modelInTabularDataBase <- modelInTabularData
  if (length(modelInTabularDataBase)) {
    modelInTabularDataBase <- modelInTabularDataBase[!modelInTabularDataBase %in% scalarInputSym]
  }
}

if (is.null(errMsg)) {
  scalarInputSymToVerify <- NULL
  # determine the filenames for the model input datasets
  if (scalarsFileName %in% modelInTabularData) {
    scalarInputSym <- c(scalarInputSym, modelIn[[scalarsFileName]]$symnames)
    # scalars should always be the highest indexed dataset
    modelInFileNames <- c(modelInTabularData[modelInTabularData != scalarsFileName], scalarsFileName)
    inputDsAliases <- modelInAlias[match(modelInFileNames, names(modelIn))]
  } else if (length(modelIn) > (length(modelInTabularData) - length(scalarSymbolsBase))) {
    modelInFileNames <- c(modelInTabularData, scalarsFileName)
    inputDsAliases <- c(modelInAlias[match(modelInTabularData, names(modelIn))], lang$nav$scalarAliases$scalars)
  } else {
    modelInFileNames <- modelInTabularData
    inputDsAliases <- modelInAlias[match(modelInTabularData, names(modelIn))]
  }
  inputDsNames <- modelInFileNames
  # create list of dependencies for each column of input data
  # first define data sheets without forward dependencies on other sheets (tabular data)
  if (length(modelInWithDep)) {
    modelInNoDep <- names(modelIn[-match(names(modelInWithDep), names(modelIn))])
  } else {
    modelInNoDep <- names(modelIn)
  }
  # find ID columns (sets in GAMS) for each input data sheet
  idsIn <- vector(mode = "list", length = length(modelIn))
  for (inputName in modelInTabularData) {
    i <- match(tolower(inputName), names(modelIn))[[1]]
    if (!is.null(modelIn[[i]]$headers) && length(modelIn[[i]]$headers)) {
      idsIn[[i]] <- unlist(lapply(1:length(modelIn[[i]]$headers), function(j) {
        if (modelIn[[i]]$headers[[j]]$type == "string") {
          return(names(modelIn[[i]]$headers)[[j]])
        }
      }), use.names = FALSE)
    }
  }
  scalarsInMetaData <- NULL
  scalarsInTemplate <- NULL
  if (scalarsFileName %in% inputDsNames) {
    scalarsInMetaData <- list(list(
      alias = "Input Scalars",
      colTypes = "ccc",
      headers = list(
        "a" = list(type = "string"),
        "b" = list(type = "string"),
        "c" = list(type = "string")
      ),
      symnames = c(
        modelInRaw[[scalarsFileName]]$symnames,
        DDPar, GMSOpt
      ),
      symtext = c(
        modelInRaw[[scalarsFileName]]$symtext,
        character(length(DDPar) + length(GMSOpt))
      )
    ))
    names(scalarsInMetaData[[1]]$headers) <- scalarsFileHeaders
    names(scalarsInMetaData) <- scalarsFileName

    if (length(modelIn[[scalarsFileName]]$symnames)) {
      scalarsInTemplate <- tibble(
        "a" = modelIn[[scalarsFileName]]$symnames,
        "b" = modelIn[[scalarsFileName]]$symtext,
        "c" = NA_character_
      )
    } else {
      scalarsInTemplate <- tibble(
        "a" = character(0L),
        "b" = character(0L),
        "c" = character(0L)
      )
    }

    names(scalarsInTemplate) <- scalarsFileHeaders
    attr(scalarsInTemplate, "aliases") <- c(
      lang$scalarAliases$cols$name,
      lang$scalarAliases$cols$desc,
      lang$scalarAliases$cols$value
    )
  }
  # initialize data frames for model input data
  modelInTemplate <- vector(mode = "list", length = length(modelIn))
  lapply(modelInTabularData, function(el) {
    i <- match(el, names(modelIn))
    if (!is.null(modelIn[[i]]$headers)) {
      headers <- lapply(modelIn[[i]]$headers, function(header) {
        if (identical(header$type, "numeric")) {
          return(numeric())
        }
        return(character())
      })
      names(headers) <- names(modelIn[[i]]$headers)

      if (identical(el, scalarsFileName)) {
        modelInTemplate[[i]] <<- scalarsInTemplate
      } else if (identical(el, scalarEquationsName)) {
        modelInTemplate[[i]] <<- tibble(
          a = modelIn[[scalarEquationsName]]$symnames,
          b = modelIn[[scalarEquationsName]]$symtext,
          c = NA_real_,
          d = NA_real_,
          e = NA_real_,
          f = NA_real_,
          g = NA_real_
        )
        names(modelInTemplate[[i]]) <<- names(headers)
      } else {
        modelInTemplate[[i]] <<- tibble(!!!headers)
      }
      attr(modelInTemplate[[i]], "aliases") <<- vapply(seq_along(modelIn[[i]]$headers), function(j) {
        alias <- modelIn[[i]]$headers[[j]]$alias
        if (!length(alias)) {
          return(names(modelIn[[i]]$headers)[j])
        }
        return(alias)
      }, character(1L), USE.NAMES = FALSE)
      attr(modelInTemplate[[i]], "isTable") <<- sum(vapply(modelIn[[i]]$headers, function(hdr) {
        identical(hdr$type, "numeric")
      }, logical(1L), USE.NAMES = FALSE)) > 1L

      modelIn[[i]][["template"]] <<- modelInTemplate[[i]]

      if (length(modelIn[[i]]$pivotCols)) {
        if (any(!modelIn[[i]]$pivotCols %in% names(modelIn[[i]]$headers))) {
          errMsg <<- paste(errMsg, sprintf(
            "Some columns you want to pivot could not be found in the symbol: '%s'.",
            modelInAlias[i]
          ))
        } else if (length(modelIn[[i]]$headers) < 3L ||
          sum(vapply(modelIn[[i]]$headers,
            function(header) identical(header$type, "numeric"),
            logical(1L),
            USE.NAMES = FALSE
          )) > 1L) {
          errMsg <<- paste(errMsg, sprintf(
            "You may only pivot symbols that have at least 2 dimensions and have at most 1 value column (symbol: '%s').",
            modelInAlias[i]
          ))
        } else if (identical(modelIn[[i]]$symtype, "set")) {
          errMsg <<- paste(errMsg, sprintf(
            "PivotCols option is currently not supported for sets (symbol: '%s').",
            modelInAlias[i]
          ))
        }
      }

      # abort since handsontable crashes when setting table to readonly if there exist columns with the same name
      if (!LAUNCHCONFIGMODE &&
        identical(modelIn[[i]]$type, "hot") && any(duplicated(attr(modelInTemplate[[i]], "aliases"))) &&
        (identical(modelIn[[i]]$readonly, TRUE) || any(vapply(modelIn[[i]]$headers, function(header) {
          if (identical(header$readonly, TRUE)) {
            return(TRUE)
          }
          return(FALSE)
        }, logical(1L), USE.NAMES = FALSE)))) {
        errMsg <<- paste(errMsg, sprintf(paste0(
          "It is currently not supported to define a table (or certain columns)",
          " to be readonly if this table contains columns with identical names. Please rename the columns ",
          "(by adjusting the explanatory text of your GAMS symbol) or remove the readonly attribute. ",
          "Table that causes the problem: '%s'."
        ), modelInAlias[i]), sep = "\n")
      }
    }
  })
  modelOutTemplate <- vector(mode = "list", length = length(modelOut))
  # declare set of output sheets that should be displayed in webUI
  if (!LAUNCHCONFIGMODE && length(config[["hiddenOutputSymbols"]])) {
    invalidHiddenOutputSymbols <- match(config[["hiddenOutputSymbols"]], names(modelOut))
    if (any(is.na(invalidHiddenOutputSymbols))) {
      errMsg <- paste(errMsg, sprintf(
        "The output symbols: '%s' you want to be hidden do not exist!",
        paste0(config[["hiddenOutputSymbols"]][is.na(invalidHiddenOutputSymbols)],
          collapse = ", "
        )
      ), sep = "\n")
    }
  }
  modelOutToDisplay <- vapply(seq_along(modelOut), function(i) {
    headers <- vector(mode = "numeric", length = length(modelOut[[i]]$headers))
    headers <- lapply(modelOut[[i]]$headers, function(header) {
      if (identical(header$type, "numeric")) {
        return(numeric())
      }
      return(character())
    })
    names(headers) <- names(modelOut[[i]]$headers)
    if (identical(names(modelOut)[i], scalarsOutName)) {
      nonHiddenScalars <- !modelOut[[scalarsOutName]]$symnames %in% config$hiddenOutputScalars

      modelOutTemplate[[i]] <<- tibble(
        "a" = modelOut[[scalarsOutName]]$symnames[nonHiddenScalars],
        "b" = modelOut[[scalarsOutName]]$symtext[nonHiddenScalars],
        "c" = NA_character_
      )
      names(modelOutTemplate[[i]]) <<- names(headers)
    } else if (identical(names(modelOut)[i], scalarEquationsOutName)) {
      modelOutTemplate[[i]] <<- tibble(
        a = modelOut[[scalarEquationsOutName]]$symnames,
        b = modelOut[[scalarEquationsOutName]]$symtext,
        c = NA_real_,
        d = NA_real_,
        e = NA_real_,
        f = NA_real_,
        g = NA_real_
      )
      names(modelOutTemplate[[i]]) <<- names(headers)
    } else {
      modelOutTemplate[[i]] <<- tibble::tibble(!!!headers)
    }

    attr(modelOutTemplate[[i]], "aliases") <<- vapply(seq_along(modelOut[[i]]$headers), function(j) {
      alias <- modelOut[[i]]$headers[[j]]$alias
      if (!length(alias)) {
        return(names(modelOut[[i]]$headers)[j])
      }
      return(alias)
    }, character(1L), USE.NAMES = FALSE)

    attr(modelOutTemplate[[i]], "isTable") <<- sum(vapply(modelOut[[i]]$headers, function(hdr) {
      identical(hdr$type, "numeric")
    }, logical(1L), USE.NAMES = FALSE)) > 1L

    if (names(modelOut)[i] %in% config[["hiddenOutputSymbols"]]) {
      modelOut[[i]]$hidden <<- TRUE
      return(FALSE)
    }
    if (isTRUE(modelOut[[i]]$hidden)) {
      return(FALSE)
    } else {
      return(TRUE)
    }
  }, logical(1L), USE.NAMES = FALSE)
  if (length(config$overwriteSheetOrder$output) && !LAUNCHCONFIGMODE) {
    namesModelOutToDisplay <- names(modelOut)[modelOutToDisplay]
    overwriteSheetOrderCleaned <- config$overwriteSheetOrder$output[config$overwriteSheetOrder$output %in%
      namesModelOutToDisplay]
    outputSheetIdsToDisplay <- c(match(
      c(
        overwriteSheetOrderCleaned,
        namesModelOutToDisplay[!namesModelOutToDisplay %in%
          overwriteSheetOrderCleaned]
      ),
      names(modelOut)
    ))
    rm(overwriteSheetOrderCleaned)
  } else {
    outputSheetIdsToDisplay <- seq_along(modelOut)[modelOutToDisplay]
  }
}

if (is.null(errMsg)) {
  # get remote import/export options
  externalDataConfig <- list(
    remoteImport = NULL, remoteExport = NULL,
    customDataImport = NULL, customDataExport = NULL
  )

  for (direction in c("remoteImport", "remoteExport")) {
    if (length(config[[direction]])) {
      warningMsgTmp <- "Remote importers/exporter ('remoteImport'/'remoteExport') are deprecated. Please use 'customDataImport'/'customDataExport' instead. The remoteImport/remoteExport feature will be removed in a future release!"
      warning(warningMsgTmp, call. = FALSE)
      warningMsg <- paste(warningMsg, warningMsgTmp, sep = "\n")
      externalDataConfig[[direction]] <- vector("list", length(config[[direction]]))
      for (i in seq_along(config[[direction]])) {
        remoteConfigs <- lapply(config[[direction]][[i]]$templates, function(remoteConfig) {
          symNames <- tolower(remoteConfig[["symNames"]])
          remoteConfig[["symNames"]] <- NULL

          symIds <- match(symNames, c(names(modelIn), names(modelOut)))
          if (any(is.na(symIds))) {
            errMsg <<- paste(errMsg, sprintf(
              "Some of the datasets you selected for %s: '%s' are not valid: '%s'.",
              direction, config[[direction]][[i]]$name,
              paste(symNames[is.na(symIds)],
                collapse = "', '"
              )
            ))
            return()
          }
          dupSym <- !is.na(match(symNames, names(externalDataConfig[[direction]][[i]])))
          if (any(dupSym)) {
            errMsg <<- paste(errMsg, sprintf(
              "Duplicated datasets found in %s: '%s'. Datasets: '%s'.",
              direction, config[[direction]][[i]]$name,
              paste(symNames[dupSym],
                collapse = "', '"
              )
            ))
            return()
          }

          exportConfig <- rep.int(list(remoteConfig), length(symIds))
          names(exportConfig) <- c(names(modelIn), names(modelOut))[symIds]
          return(exportConfig)
        })
        externalDataConfig[[direction]][[i]] <- unlist(remoteConfigs, recursive = FALSE, use.names = TRUE)
      }
      if (!length(errMsg)) {
        names(externalDataConfig[[direction]]) <- vapply(config[[direction]], "[[",
          character(1L), "name",
          USE.NAMES = FALSE
        )
      }
      config[[direction]] <- NULL
    }
  }
  for (direction in c("customDataImport", "customDataExport")) {
    if (length(config[[direction]])) {
      externalDataConfig[[direction]] <- vector("list", length(config[[direction]]))
      for (i in seq_along(config[[direction]])) {
        symNames <- tolower(config[[direction]][[i]][["symNames"]])
        isScalarSym <- !is.na(match(symNames, scalarInputSym))
        if (any(isScalarSym)) {
          config[[direction]][[i]]$symNamesToFetch <- symNames
          scalarSymNames <- symNames[isScalarSym]
          symNames <- symNames[!isScalarSym]
          config[[direction]][[i]]$scalarSymNames <- scalarSymNames
          config[[direction]][[i]]$symNames <- unique(c(symNames, scalarsFileName))
        }
        invalidSymIds <- is.na(match(symNames, c(names(modelInRaw), names(modelOut))))
        if (any(invalidSymIds)) {
          errMsg <- paste(errMsg, sprintf(
            "Some of the datasets you selected for your %s: '%s' are not valid: '%s'.",
            direction, config[[direction]][[i]]$label,
            paste(symNames[invalidSymIds],
              collapse = "', '"
            )
          ))
          next
        }
      }
      externalDataConfig[[direction]] <- config[[direction]]
      names(externalDataConfig[[direction]]) <- vapply(config[[direction]], "[[",
        character(1L), "label",
        USE.NAMES = FALSE
      )
      config[[direction]] <- NULL
    }
  }
  externalInputConfig <- c(
    externalDataConfig[["customDataImport"]],
    externalDataConfig[["remoteImport"]]
  )
  datasetsRemoteExport <- c(
    externalDataConfig[["customDataExport"]],
    externalDataConfig[["remoteExport"]]
  )
  rm(externalDataConfig)
}

if (is.null(errMsg)) {
  # declare output sheets as they will be displayed in UI
  outputTabs <- getTabs(names(modelOut), modelOutAlias, config$outputGroups,
    idsToDisplay = outputSheetIdsToDisplay
  )
  outputTabTitles <- outputTabs$tabTitles
  config$outputTabSettings <- outputTabs$tabSettings
  tabSheetMap$output <- outputTabs$tabSheetMap
  outputTabs <- outputTabs$tabs
  isGroupOfSheets <- vapply(seq_len(length(outputTabTitles) + length(scenInputTabTitles)), function(tabId) {
    if (tabId > length(outputTabTitles)) {
      if (length(scenInputTabTitles[[tabId - length(outputTabTitles)]]) > 1L) {
        return(TRUE)
      }
    } else if (length(outputTabTitles[[tabId]]) > 1L) {
      return(TRUE)
    }
    return(FALSE)
  }, logical(1L), USE.NAMES = FALSE)

  scalarDsNameIdx <- match(inputDsNames, names(modelIn))
  scenDataTemplate <- c(modelOutTemplate, modelInTemplate[scalarDsNameIdx])
  if (length(scalarDsNameIdx[length(inputDsNames)]) && is.na(scalarDsNameIdx[length(inputDsNames)])) {
    # need to add scalars template manually
    scenDataTemplate[[length(scenDataTemplate)]] <- scalarsInTemplate
  }
  # get column types for tabular datasets
  for (i in seq_along(modelIn)) {
    if (is.null(modelIn[[i]]$headers)) {
      next
    }
    modelIn[[i]]$colTypes <- paste(vapply(modelIn[[i]]$headers, function(header) {
      if (identical(header$type, "numeric")) {
        return("d")
      }
      return("c")
    }, character(1L), USE.NAMES = FALSE), collapse = "")
  }
  for (i in seq_along(modelOut)) {
    modelOut[[i]]$colTypes <- paste(vapply(modelOut[[i]]$headers, function(header) {
      if (identical(header$type, "numeric")) {
        return("d")
      }
      return("c")
    }, character(1L), USE.NAMES = FALSE), collapse = "")
  }
  # validate symbol links
  if (!LAUNCHCONFIGMODE && length(config[["symbolLinks"]])) {
    for (symbolLink in config[["symbolLinks"]]) {
      source <- tolower(symbolLink[["source"]])
      target <- tolower(symbolLink[["target"]])
      if (!source %in% names(modelOut)) {
        errMsg <- paste(errMsg, sprintf(
          "The source symbol: '%s' of a symbol link you specified was not found amongst the output symbols.",
          source
        ))
        next
      }
      if (source %in% c(scalarsOutName, scalarEquationsOutName)) {
        errMsg <- paste(errMsg, sprintf(
          "The source symbol: '%s' must not be the sheet of scalars or the sheet of scalar variables/equations.",
          source
        ))
        next
      }
      if (!target %in% names(modelIn)) {
        errMsg <- paste(errMsg, sprintf(
          "The target symbol: '%s' of a symbol link you specified was not found amongst the input symbols.",
          target
        ))
        next
      }
      if (target %in% c(scalarsFileName, scalarEquationsName)) {
        errMsg <- paste(errMsg, sprintf(
          "The target symbol: '%s' must not be the sheet of scalars or the sheet of scalar variables/equations.",
          target
        ))
        next
      }
      if (!identical(
        vapply(modelOut[[source]]$headers, "[[", character(1L), "type", USE.NAMES = FALSE),
        vapply(modelIn[[target]]$headers, "[[", character(1L), "type", USE.NAMES = FALSE)
      )) {
        errMsg <- paste(errMsg, sprintf(
          "The symbols: '%s' - '%s' are incompatible and can therefore not be linked together.",
          source, target
        ))
        next
      }
      if (length(modelOut[[source]]$symbolLink)) {
        errMsg <- paste(errMsg, sprintf(
          "The symbol: '%s' has multiple symbol links defined. Only one symbol link per output symbol is possible.",
          source
        ))
        next
      }
      modelOut[[source]]$symbolLink <- target
    }
    config$hasSymbolLinks <- TRUE
    config[["symbolLinks"]] <- NULL
  }
  if (length(config$scripts)) {
    if (any(duplicated(vapply(config$scripts$base, "[[", character(1L), "id", USE.NAMES = FALSE)))) {
      errMsg <- paste(errMsg, "Some of your analysis scripts share the same id. Please make sure the ID is unique.")
    }
    if (any(duplicated(vapply(config$scripts$hcube, "[[", character(1L), "id", USE.NAMES = FALSE)))) {
      errMsg <- paste(errMsg, "Some of your Hypercube/batch analysis scripts share the same id. Please make sure the ID is unique.")
    }
  }
}
if (is.null(errMsg)) {
  if (length(modelOut) + length(inputDsNames) < 1L) {
    errMsg <- "You have defined neither input nor output symbols. Please enter at least one external symbol!"
  }
  # define scenario tables to display in interface
  inputIdsNotToDisplay <- vapply(inputDsNames, function(el) {
    if (identical(modelIn[[el]]$dropdown$single, TRUE) ||
      identical(modelIn[[el]]$dropdown$checkbox, TRUE)) {
      return(TRUE)
    }
    return(FALSE)
  }, logical(1L), USE.NAMES = FALSE)
  inputDsNamesNotToDisplay <- inputDsNames[inputIdsNotToDisplay]
  scenTableNamesToDisplay <- c(names(modelOut)[modelOutToDisplay], inputDsNames[!inputIdsNotToDisplay])
  groupSheetToTabIdMap <- lapply(seq_len(length(outputTabs) + length(scenInputTabs)), function(groupId) {
    if (groupId > length(outputTabs)) {
      return(lapply(scenInputTabs[[groupId - length(outputTabs)]], function(sheetId) {
        if (identical(sheetId, 0L)) {
          tabName <- scalarsFileName
        } else {
          tabName <- names(modelIn)[[sheetId]]
        }
        return(match(tabName, scenTableNamesToDisplay))
      }))
    }
    return(lapply(outputTabs[[groupId]], function(sheetId) {
      return(match(names(modelOut)[[sheetId]], scenTableNamesToDisplay))
    }))
  })

  configGraphsIn <- vector(mode = "list", length = length(modelIn))
  configGraphsOut <- vector(mode = "list", length = length(modelOut))

  invalidGraphsToRender <- character(0L)

  # assign default output format to output data that was not set in config
  for (i in seq_along(modelOut)[!names(modelOut) %in% names(config$dataRendering)]) {
    if (isTRUE(modelOut[[i]]$hidden)) {
      next
    }
    elName <- names(modelOut)[[i]]
    if (identical(elName, scalarsOutName)) {
      visibleOutputScalars <- !(modelOut[[i]]$symnames %in% config$hiddenOutputScalars)
      if (sum(visibleOutputScalars) <= maxScalarsValBox &&
        all(modelOut[[i]]$symtypes[visibleOutputScalars] == "parameter")) {
        config$dataRendering[[elName]]$outType <- "valuebox"
        config$dataRendering[[elName]]$options$count <- modelOut[[i]]$count - length(config$hiddenOutputScalars)
      } else {
        config$dataRendering[[elName]]$outType <- "datatable"
      }
    } else {
      config$dataRendering[[elName]]$outType <- config$defaultRendererOutput
    }
  }
  # assign default output format for input sheets that were not set in config
  for (i in seq_along(modelIn)[!names(modelIn) %in% names(config$dataRendering)]) {
    elName <- names(modelIn)[[i]]
    if (identical(modelIn[[i]]$type, "custom")) {
      # make sure custom inputs have graph button activated (table is displayed there)
      config$dataRendering[[elName]] <- list(
        outType = "datatable",
        rendererName = modelIn[[i]]$rendererName,
        packages = modelIn[[i]]$packages
      )
    } else if (config$autoGenInputGraphs) {
      # Create graphs only for tabular input sheets
      if (!is.null(modelIn[[i]]$headers)) {
        if (identical(names(modelIn)[[i]], scalarsFileName)) {
          config$dataRendering[[elName]]$outType <- "datatable"
        } else {
          config$dataRendering[[elName]]$outType <- defInType
        }
      }
    }
  }

  for (el in names(config$dataRendering)) {
    i <- match(tolower(el), names(modelIn))[[1]]
    isOutputGraph <- FALSE
    # data rendering object was found in list of model input sheets
    if (is.na(i)) {
      i <- match(tolower(el), names(modelOut))[[1]]
      # data rendering object was found in list of model output sheets
      if (!is.na(i)) {
        if (isTRUE(modelOut[[i]]$hidden)) {
          warningMsgTmp <- sprintf(
            "You specified chart options for the output symbol: %s. These options will be ignored as the symbol is hidden.",
            names(modelOut)[i]
          )
          warning(warningMsgTmp, call. = FALSE)
          warningMsg <- paste(warningMsg, warningMsgTmp, sep = "\n")
          if (LAUNCHCONFIGMODE) {
            invalidGraphsToRender <- c(invalidGraphsToRender, el)
          }
          next
        } else {
          configGraphsOut[[i]] <- config$dataRendering[[el]]
          configGraphsOut[[i]]$options <- c(
            configGraphsOut[[i]]$options,
            list("_metadata_" = list(
              symname = names(modelOut)[i],
              headers = modelOut[[i]]$headers,
              symtype = modelOut[[i]]$symtype
            ))
          )
          if (identical(configGraphsOut[[i]]$outType, "datatable")) {
            configGraphsOut[[i]]$graph <- configGraphsOut[[i]]
            configGraphsOut[[i]]$graph$outType <- NULL
          } else if (identical(configGraphsOut[[i]]$outType, "miroPivot")) {
            validGraphConfig <- validateGraphConfig(configGraphsOut[[i]])
            if (!identical(validGraphConfig, TRUE)) {
              errMsgTmp <- paste0(
                "Invalid graph config for symbol '", names(modelOut)[i],
                "': ", validGraphConfig, "."
              )
              if (LAUNCHCONFIGMODE) {
                warning(errMsgTmp, call. = FALSE)
                next
              }
              errMsg <- paste(errMsg, paste0(errMsgTmp, " Start the Configuration Mode to reconfigure your app."), sep = "\n")
              next
            }
          } else if (identical(configGraphsOut[[i]]$outType, "dashboard")) {
            configGraphsOut[[i]]$options$dataViews <- lapply(configGraphsOut[[i]]$options$dataViews, function(viewList) {
              if (is.null(names(viewList))) {
                unlist(viewList, recursive = FALSE)
              } else {
                viewList
              }
            })
            validGraphConfig <- validateDashboardConfig(configGraphsOut[[i]], "graph")
            if (!identical(validGraphConfig, TRUE)) {
              errMsgTmp <- paste0(
                "Invalid graph config for symbol '", names(modelOut)[i],
                "': ", validGraphConfig, "."
              )
              if (LAUNCHCONFIGMODE) {
                warning(errMsgTmp, call. = FALSE)
                next
              }
              errMsg <- paste(errMsg, errMsgTmp, sep = "\n")
              next
            }
          } else if (identical(configGraphsOut[[i]]$outType, "valueBox")) {
            if (identical(names(modelOut)[[i]], scalarsOutName)) {
              configGraphsOut[[i]]$options$count <- modelOut[[i]]$count - length(config$hiddenOutputScalars)
            } else {
              errMsg <<- paste(errMsg,
                sprintf(
                  "Output type: 'valueBox' is only valid for scalar tables. Please choose another output type for your dataset : '%s'.",
                  modelOutAlias[i]
                ),
                sep = "\n"
              )
            }
          }
        }
      } else if (LAUNCHCONFIGMODE) {
        invalidGraphsToRender <- c(invalidGraphsToRender, el)
        next
      } else {
        errMsgTmp <- paste0("'", el, "' was defined to be an object to render, but was not found in either the list of model input or the list of model output sheets.")
        errMsg <- paste(errMsg, errMsgTmp, sep = "\n")
        next
      }
      if (length(configGraphsOut[[i]]$additionalData)) {
        badAdditionalDataSheetNames <- !configGraphsOut[[i]]$additionalData %in% c(
          names(modelOut),
          inputDsNames
        )
        if (any(badAdditionalDataSheetNames)) {
          warningMsgTmp <- paste0(
            "The symbols: ",
            paste0(configGraphsOut[[i]]$additionalData[badAdditionalDataSheetNames],
              collapse = ", "
            ),
            " are configured to be additional data for the renderer of symbol: ",
            names(modelOut)[i],
            ". These symbols could not be found among the list of either input or output symbols!"
          )
          warning(warningMsgTmp, call. = FALSE)
          warningMsg <- paste(warningMsg, warningMsgTmp, sep = "\n")
        }
        configGraphsOut[[i]]$additionalData <- configGraphsOut[[i]]$additionalData[!badAdditionalDataSheetNames]
      }
      isOutputGraph <- TRUE
    } else {
      configGraphsIn[[i]] <- config$dataRendering[[el]]
      configGraphsIn[[i]]$options <- c(
        configGraphsIn[[i]]$options,
        list("_metadata_" = list(
          symname = names(modelIn)[i],
          headers = modelIn[[i]]$headers,
          symtype = modelIn[[i]]$symtype
        ))
      )

      if (identical(configGraphsIn[[i]]$outType, "miroPivot")) {
        validGraphConfig <- validateGraphConfig(configGraphsIn[[i]])
        if (!identical(validGraphConfig, TRUE)) {
          errMsgTmp <- paste0(
            "Invalid graph config for symbol '", names(modelIn)[i],
            "': ", validGraphConfig, "."
          )
          if (LAUNCHCONFIGMODE) {
            warning(errMsgTmp, call. = FALSE)
            next
          }
          errMsg <- paste(errMsg, paste0(errMsgTmp, " Start the Configuration Mode to reconfigure your app."), sep = "\n")
          next
        }
      } else if (identical(configGraphsIn[[i]]$outType, "valueBox")) {
        if (identical(names(modelIn)[[i]], scalarsFileName)) {
          configGraphsIn[[i]]$options$count <- modelIn[[i]]$count
        } else {
          errMsg <<- paste(errMsg,
            sprintf(
              "Output type: 'valueBox' is only valid for scalar tables. Please choose another output type for your dataset : '%s'.",
              modelInAlias[i]
            ),
            sep = "\n"
          )
        }
      }
    }
    if (length(config$dataRendering[[el]]$graph$filter)) {
      if (isOutputGraph) {
        categoricalHeaders <- modelOut[[i]]$headers
      } else {
        categoricalHeaders <- modelIn[[i]]$headers
      }
      categoricalHeaders <- unlist(lapply(seq_along(categoricalHeaders), function(hdrId) {
        if (identical(categoricalHeaders[[hdrId]]$type, "string")) {
          return(names(categoricalHeaders)[[hdrId]])
        }
        return(NULL)
      }), use.names = FALSE)
      if (!length(config$dataRendering[[el]]$graph$filter$col) ||
        !config$dataRendering[[el]]$graph$filter$col %in% categoricalHeaders) {
        errMsg <- paste(errMsg, sprintf(
          "The column: '%s' was defined as a dynamic filter for the chart of element: '%s'. This column could not be found among the categorical columns of this symbol: '%s'.",
          config$dataRendering[[el]]$graph$filter$col, el, paste(categoricalHeaders, collapse = "', '")
        ), setp = "\n")
      }
    }
  }
  for (i in seq_along(modelOut)) {
    el <- names(modelOut)[i]
    if (el %in% tolower(names(config$outputTables))) {
      configGraphsOut[[i]]$datatable <- modifyList(config$datatable, config$outputTables[[tolower(el)]])
      if (length(configGraphsOut[[i]]$datatable$pivotCols)) {
        if (any(!configGraphsOut[[i]]$datatable$pivotCols %in% names(modelOut[[i]]$headers))) {
          errMsg <- paste(errMsg, sprintf(
            "Some columns you want to pivot could not be found in the symbol: '%s'.",
            modelOutAlias[i]
          ))
        } else if (length(modelOut[[i]]$headers) < 3L ||
          sum(vapply(modelOut[[i]]$headers,
            function(header) identical(header$type, "numeric"),
            logical(1L),
            USE.NAMES = FALSE
          )) > 1L) {
          errMsg <- paste(errMsg, sprintf(
            "You may only pivot symbols that have at least 2 dimensions and have at most 1 value column (symbol: '%s').",
            modelOutAlias[i]
          ))
        }
      }
    } else {
      configGraphsOut[[i]]$datatable <- config$datatable
    }
  }
  invalidSymbolConfig <- match(tolower(names(config$outputTables)), names(modelOut))
  invalidSymbolConfig <- invalidSymbolConfig[is.na(invalidSymbolConfig)]
  if (length(invalidSymbolConfig)) {
    warning(sprintf(
      "You specified an output table for the symbol(s): '%s'. This/these symbol(s) is/are not part of the data contract between GAMS and MIRO",
      paste(invalidSymbolConfig, collapse = "','")
    ), call. = FALSE)
  }

  # sanitize file names of output attachments
  config$outputAttachments <- lapply(config$outputAttachments, function(el) {
    el$filename <- sanitizeFn(el$filename)
    return(el)
  })

  installPackage <- list()
  installPackage$plotly <- LAUNCHCONFIGMODE || any(vapply(c(configGraphsIn, configGraphsOut),
    function(conf) {
      if (identical(conf$graph$tool, "plotly")) TRUE else FALSE
    },
    logical(1L),
    USE.NAMES = FALSE
  ))
  installPackage$dygraphs <- LAUNCHCONFIGMODE || any(vapply(c(configGraphsIn, configGraphsOut),
    function(conf) {
      if (identical(conf$graph$tool, "dygraphs")) TRUE else FALSE
    },
    logical(1L),
    USE.NAMES = FALSE
  ))
  installPackage$leaflet <- LAUNCHCONFIGMODE || any(vapply(c(configGraphsIn, configGraphsOut),
    function(conf) {
      if (identical(conf$graph$tool, "leaflet")) TRUE else FALSE
    },
    logical(1L),
    USE.NAMES = FALSE
  ))
  installPackage$timevis <- LAUNCHCONFIGMODE || any(vapply(c(configGraphsIn, configGraphsOut),
    function(conf) {
      if (identical(conf$graph$tool, "timevis")) TRUE else FALSE
    },
    logical(1L),
    USE.NAMES = FALSE
  ))
  dbSchemaModel <- list(schema = setNames(lapply(seq_len(length(modelOut) + length(modelIn)), function(i) {
    if (i <= length(modelOut)) {
      el <- modelOut[[i]]
      tabName <- names(modelOut)[i]
    } else {
      el <- modelIn[[i - length(modelOut)]]
      tabName <- names(modelIn)[i - length(modelOut)]
    }
    if (!length(el$headers) || isTRUE(el$dropdown$single) ||
      isTRUE(el$dropdown$checkbox) ||
      tabName %in% c(scalarsFileName, scalarsOutName)) {
      return(NA)
    }
    return(list(
      tabName = tabName,
      colNames = names(el$headers),
      colTypes = el$colTypes
    ))
  }), c(names(modelOut), names(modelIn))), views = list())
  dbSchemaModel$schema[is.na(dbSchemaModel$schema)] <- NULL
  if (scalarsOutName %in% names(modelOut)) {
    scalarMeta <- setNames(
      modelOut[[scalarsOutName]]$symtypes,
      modelOut[[scalarsOutName]]$symnames
    )
    dbSchemaModel$schema <- c(
      dbSchemaModel$schema,
      setNames(lapply(names(scalarMeta), function(scalarName) {
        list(
          tabName = scalarName,
          colNames = scalarName,
          colTypes = if (identical(scalarMeta[[scalarName]], "set")) "c" else "d"
        )
      }), names(scalarMeta))
    )
    dbSchemaModel$views[[scalarsOutName]] <- c(dbSchemaModel$views[[scalarsOutName]], names(scalarMeta))
  }
  if (scalarsFileName %in% names(modelInRaw)) {
    scalarMeta <- setNames(
      modelInRaw[[scalarsFileName]]$symtypes,
      modelInRaw[[scalarsFileName]]$symnames
    )
    dbSchemaModel$schema <- c(
      dbSchemaModel$schema,
      setNames(lapply(names(scalarMeta), function(scalarName) {
        list(
          tabName = scalarName,
          colNames = scalarName,
          colTypes = if (identical(scalarMeta[[scalarName]], "set")) "c" else "d"
        )
      }), names(scalarMeta))
    )
    dbSchemaModel$views[[scalarsFileName]] <- c(dbSchemaModel$views[[scalarsFileName]], names(scalarMeta))
  }
  if (length(GMSOpt) || length(DDPar)) {
    dbSchemaModel$schema <- c(
      dbSchemaModel$schema,
      setNames(lapply(c(GMSOpt, DDPar), function(parName) {
        list(
          tabName = parName,
          colNames = parName,
          colTypes = "c"
        )
      }), c(GMSOpt, DDPar))
    )
    dbSchemaModel$views[[scalarsFileName]] <- c(dbSchemaModel$views[[scalarsFileName]], c(GMSOpt, DDPar))
  }
}
if (is.null(errMsg)) {
  # parse README.md file
  if (length(config$readme$filename)) {
    readmeFilePath <- file.path(
      currentModelDir,
      config$readme$filename
    )
    tryCatch(
      {
        if (endsWith(tolower(readmeFilePath), "html")) {
          config$readmeFile <- read_file(readmeFilePath)
        } else if (file.exists(readmeFilePath)) {
          source(file.path("components", "md_parser.R"), local = TRUE)
          markdownParser <- MarkdownParser$new(!LAUNCHCONFIGMODE &&
            isTRUE(config$readme$enableMath))
          config$readmeFile <- markdownParser$parseFile(readmeFilePath)
        }
      },
      error = function(e) {
        errMsg <<- sprintf(
          "Problems parsing README markdown file. Error message: %s",
          conditionMessage(e)
        )
      }
    )
  }
  dropdownAliases <- lapply(modelIn, function(el) {
    if (identical(el$type, "dropdown") &&
      length(el$dropdown$aliases) &&
      !isTRUE(el$dropdown$multiple)) {
      return(list(
        aliases = el$dropdown$aliases,
        choices = el$dropdown$choices,
        clearValue = isTRUE(el$dropdown$clearValue)
      ))
    }
    return(NULL)
  })
  dropdownAliases <- dropdownAliases[!vapply(dropdownAliases, is.null,
    logical(1L),
    USE.NAMES = FALSE
  )]
}

if (is.null(errMsg)) {
  compareModuleConfigs <- list()
  noCompareModules <- 0L
  for (compareModuleIdx in seq_along(config[["compareModules"]])) {
    compareModuleConfig <- config[["compareModules"]][[compareModuleIdx]]
    if (identical(compareModuleConfig$type, "dashboard")) {
      compareModuleConfig$options$dataViews <- lapply(compareModuleConfig$options$dataViews, function(viewList) {
        if (is.null(names(viewList))) {
          unlist(viewList, recursive = FALSE)
        } else {
          viewList
        }
      })
      validGraphConfig <- validateDashboardConfig(compareModuleConfig, "compare")
      if (!identical(validGraphConfig, TRUE)) {
        errMsgTmp <- paste0(
          "Invalid config for dashboard comparison '", compareModuleIdx,
          "': ", validGraphConfig, "."
        )
        if (LAUNCHCONFIGMODE) {
          warning(errMsgTmp, call. = FALSE)
          next
        }
        errMsg <- paste(errMsg, errMsgTmp, sep = "\n")
        next
      }

      compareModuleConfig$id <- paste0("__dashboard_", compareModuleIdx)
      compareModuleConfigs[[compareModuleConfig$id]] <- compareModuleConfig
      compareModuleConfigs[[compareModuleConfig$id]][["outType"]] <- "dashboardCompare"
      compareModuleConfigs[[compareModuleConfig$id]][["rendererFnName"]] <- "renderDashboardCompare"
      compareModuleConfigs[[compareModuleConfig$id]][["outputFnName"]] <- "dashboardCompareOutput"
      compareModuleConfigs[[compareModuleConfig$id]][["idx"]] <- compareModuleIdx
    }
    noCompareModules <- compareModuleIdx
  }

  for (customCompareModuleIdx in seq_along(config[["customCompareModules"]])) {
    compareModuleIdx <- customCompareModuleIdx + noCompareModules
    compareModuleConfig <- config[["customCompareModules"]][[customCompareModuleIdx]]
    if (compareModuleConfig$id %in% names(compareModuleConfigs)) {
      errMsg <- sprintf(
        "Analysis module ids must be unique (%s).",
        compareModuleConfig$id
      )
    } else if (compareModuleConfig$id %in% c("split", "tab", "pivot") ||
      startsWith(compareModuleConfig$id, "__dashboard_")) {
      errMsg <- sprintf(
        "Analysis module id: '%s' is reserved and cannot be used.",
        compareModuleConfig$id
      )
    } else {
      compareModuleConfigs[[compareModuleConfig$id]] <- compareModuleConfig
      if (length(compareModuleConfig$externalRendererId)) {
        compareRendererFunctionId <- compareModuleConfig$externalRendererId
      } else {
        compareRendererFunctionId <- compareModuleConfig$id
      }
      compareModuleConfigs[[compareModuleConfig$id]][["outType"]] <- paste0(
        "mirocompare_",
        compareRendererFunctionId
      )
      compareModuleConfigs[[compareModuleConfig$id]][["rendererFnName"]] <- paste0(
        "renderMirocompare_",
        compareRendererFunctionId
      )
      compareModuleConfigs[[compareModuleConfig$id]][["outputFnName"]] <- paste0(
        "mirocompare_",
        compareRendererFunctionId,
        "Output"
      )
      compareModuleConfigs[[compareModuleConfig$id]][["idx"]] <- compareModuleIdx
    }
  }
  config[["customCompareModules"]] <- compareModuleConfigs
}

if (is.null(errMsg)) {
  if (length(config[["defCompMode"]])) {
    ids <- vapply(config[["customCompareModules"]], "[[", character(1L), "id", USE.NAMES = FALSE)
    if (!config[["defCompMode"]] %in% c(ids, "split", "tab", "pivot")) {
      errMsg <- sprintf(
        "Invalid defCompMode (default comparison mode) id found. Id: %s doesn't exist. Valid ids are: %s.",
        config[["defCompMode"]], paste(c(ids, "split", "tab", "pivot"), collapse = ", ")
      )
    }
  }
}

if (is.null(errMsg)) {
  validViewSymnames <- c(
    inputDsNames, paste0("_pivotcomp_", inputDsNames),
    names(modelOut), paste0("_pivotcomp_", names(modelOut)),
    paste0("_customcomp_", names(config[["customCompareModules"]]))
  )
  invalidViewSymbols <- !names(config$globalViews) %in% validViewSymnames
  if (any(invalidViewSymbols)) {
    errMsg <- sprintf(
      "Invalid global views found. Symbol(s): %s don't exist.",
      paste(names(config$globalViews)[invalidViewSymbols], collapse = ", ")
    )
  }
}
if (endsWith(tolower(modelGmsName), ".py")) {
  config$isGamsPy <- TRUE
  # lst/trace file not supported with gamspy
  config$activateModules$lstFile <- FALSE
  config$saveTraceFile <- FALSE
}

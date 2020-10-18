errMsg <- installAndRequirePackages("V8", installedPackages, RLibPath, CRANMirror, miroWorkspace)
# check whether there exists a config file and if not create an empty one
if(is.null(errMsg)){
  if(!file.exists(paste0(currentModelDir, .Platform$file.sep, "conf_", modelName,
                         .Platform$file.sep, modelName, ".json"))){
    tryCatch(cat("{}\n", file = paste0(currentModelDir, .Platform$file.sep, "conf_", modelName,
                                       .Platform$file.sep, modelName, ".json")),
             error = function(e){
               errMsg <<- paste0("A configuration file was not found and no data could be written to the location of the config folder. Please check read/write permissions in folder: ",
                                 paste0(currentModelDir, .Platform$file.sep, "conf_", modelName))
             })
  }
}

if(is.null(errMsg)){
  # files that require schema file
  jsonFilesWithSchema <- c(paste0(currentModelDir, .Platform$file.sep,
                                  "conf_", modelName, .Platform$file.sep, 
                                  modelName, ".json"), 
                           paste0(currentModelDir, .Platform$file.sep,
                                  "conf_", modelName, .Platform$file.sep, 
                                  modelName, "_io.json"))
  jsonFilesMissing    <- !file.exists(jsonFilesWithSchema)
  if(any(jsonFilesMissing)){
    errMsg <- paste(errMsg, paste0("JSON file(s): '", basename(jsonFilesWithSchema[jsonFilesMissing]), 
                                   "' is required/are required but missing or you have no read permissions. Please make sure this file/these files are available.\n
If you run MIRO for the first time with your new model, the GAMS/MIRO data contract needs to be generated.\n
Run \"gams ", modelName, ".gms IDCGenerateJSON=conf_", modelName, "/", modelName, "_io.json IDCGenerateGDX=data_", 
                                   modelName, "/", "default.gdx\" inside your model directory to generate the data contract as well as a default scenario.\n
Note that GAMS will not create the directories 'conf_", modelName, "' and 'data_", modelName, "', so you need to create these first."), 
                    sep = "\n")
  }
  rm(jsonFilesMissing)
  jsonSchemaMap <- list(config = c(jsonFilesWithSchema[1], 
                                   file.path(getwd(), "conf", "config_schema.json")), 
                        io_config = c(jsonFilesWithSchema[2], 
                                     file.path(getwd(), "conf", "io_config_schema.json")))
}

# validate json files
config <- NULL
if(is.null(errMsg)){
  config <- NULL
  jsonValidator <- JSONValidator$new()
  lapply(seq_along(jsonSchemaMap), function(i){
    tryCatch({
      valid <- jsonValidator$validate(jsonSchemaMap[[i]][1], 
                                     jsonSchemaMap[[i]][2])
    }, error = function(e){
      errMsg <<- paste(errMsg, paste0("Some error occurred validating JSON file: '", 
                                      basename(jsonFilesWithSchema[i]), "'. Error message: ", 
                                      conditionMessage(e)), sep = "\n")
    })
    if(!is.null(errMsg)){
      return()
    }
    
    if(is.null(valid$errors)){
      if(identical(names(jsonSchemaMap)[[i]], "config")){
        config <<- valid$data
      }else if(identical(names(jsonSchemaMap)[[i]], "io_config")){
        config <<- c(config, valid$data)
      }
    }else{
      errMsg <<- paste(errMsg, 
                       paste0("Some error occurred parsing JSON file: '", 
                              basename(jsonFilesWithSchema[i]), 
                              "'. Error message: ", valid$errors), sep = "\n")
    }
  })
}

# load model input and output parameters
if(is.null(errMsg)){
  # handsontable options
  hotOptions        <- config[["handsontable"]]
  
  modelIn           <- config[["inputSymbols"]]
  names(modelIn)    <- tolower(names(modelIn))
  
  modelOut          <- config[["outputSymbols"]]
  names(modelOut)   <- tolower(names(modelOut))
  
  #TODO: Update API version when dataContract is used elsewhere than in Configuration mode
  dataContract      <- list(inputSymbols = config[["inputSymbols"]],
                            outputSymbols = config[["outputSymbols"]])
  
  config[["inputSymbols"]]  <- NULL
  config[["outputSymbols"]] <- NULL
  
  if(LAUNCHHCUBEMODE && !length(modelIn)){
    errMsg <- "Can not launch Hypercube mode without having input data defined! Please define input data and try again."
  }
  
  if(!length(config$pageTitle) || nchar(config$pageTitle) == 0L){
    config$pageTitle <- config$modelTitle
  }
  config$modelTitle <- NULL
  # rename input and output scalar aliases
  if(length(modelIn[[scalarsFileName]])){
    modelIn[[scalarsFileName]]$alias <-  lang$nav$scalarAliases$scalars
    modelIn[[scalarsFileName]]$headers[[1]]$alias <- lang$nav$scalarAliases$cols$name
    modelIn[[scalarsFileName]]$headers[[2]]$alias <- lang$nav$scalarAliases$cols$desc
    modelIn[[scalarsFileName]]$headers[[3]]$alias <- lang$nav$scalarAliases$cols$value
  }
  if(length(modelOut[[scalarsOutName]])){
    modelOut[[scalarsOutName]]$count <- length(modelOut[[scalarsOutName]]$symnames)
    modelOut[[scalarsOutName]]$alias <- lang$nav$scalarAliases$scalarsOut
    modelOut[[scalarsOutName]]$headers[[1]]$alias <- lang$nav$scalarAliases$cols$name
    modelOut[[scalarsOutName]]$headers[[2]]$alias <- lang$nav$scalarAliases$cols$desc
    modelOut[[scalarsOutName]]$headers[[3]]$alias <- lang$nav$scalarAliases$cols$value
  }
  if(length(config[["overwriteAliases"]])){
    overwriteSymNames <- names(config[["overwriteAliases"]])
    for (idx in seq_along(config[["overwriteAliases"]])){
      i <- match(overwriteSymNames[idx], names(modelIn))
      if(is.na(i)){
        i <- match(overwriteSymNames[idx], names(modelOut))
        if(is.na(i)){
          if(!LAUNCHCONFIGMODE)
            warning(sprintf("The alias of symbol: '%s' was selected to be overwritten. However, this symbol could not be found.", 
                            overwriteSymNames[idx]))
          next
        }
        modelOut[[i]]$alias <- config[["overwriteAliases"]][[idx]][["newAlias"]]
        next
      }
      modelIn[[i]]$alias <- config[["overwriteAliases"]][[idx]][["newAlias"]]
    }
    config[["overwriteAliases"]] <- NULL
  }
  if(length(config[["overwriteHeaderAliases"]])){
    overwriteSymNames <- names(config[["overwriteHeaderAliases"]])
    for (idx in seq_along(config[["overwriteHeaderAliases"]])){
      i <- match(names(config[["overwriteHeaderAliases"]])[[idx]], names(modelIn))
      if(is.na(i)){
        i <- match(overwriteSymNames[idx], names(modelOut))
        newHeaders <- config[["overwriteHeaderAliases"]][[idx]][["newHeaders"]]
        if(is.na(i)){
          if(!LAUNCHCONFIGMODE)
            warning(sprintf("The headers of symbol: '%s' were selected to be overwritten. However, this symbol could not be found.", 
                            overwriteSymNames[idx]))
          next
        }
        if(length(modelOut[[i]]$headers) != length(newHeaders)){
          if(!LAUNCHCONFIGMODE)
            warning(sprintf("The headers of symbol: '%s' were selected to be overwritten. However, the dimensions do not match!", 
                            overwriteSymNames[idx]))
          next
        }
        for (j in seq_along(modelOut[[i]]$headers)){
          modelOut[[i]]$headers[[j]]$alias <- newHeaders[j]
        }
       next 
      }
      newHeaders <- config[["overwriteHeaderAliases"]][[idx]][["newHeaders"]]
      if(length(modelIn[[i]]$headers) != length(newHeaders)){
        warning(sprintf("The headers of symbol: '%s' were selected to be overwritten. However, the dimensions do not match!", 
                        overwriteSymNames[idx]))
        next
      }
      for (j in seq_along(modelIn[[i]]$headers)){
        modelIn[[i]]$headers[[j]]$alias <- newHeaders[j]
      }
    }
    config[["overwriteHeaderAliases"]] <- NULL
  }
  scalarTableIds <- match(c(scalarsFileName, scalarEquationsName), names(modelIn))
  scalarTableIds <- scalarTableIds[!is.na(scalarTableIds)]
  for(scalarTableId in scalarTableIds){
    modelIn[[scalarTableId]]$headers[[1]]$readonly <- TRUE
    modelIn[[scalarTableId]]$headers[[2]]$readonly <- TRUE
  }
  
  modelInRaw        <- modelIn
  customPackages    <- vector("list", length(modelIn))
  
  invalidWidgetsToRender <- character(0L)
  
  for(el in names(config$inputWidgets)){
    i    <- match(tolower(el), names(modelIn))
    el_l <- tolower(el)
    
    widgetConfig    <- config$inputWidgets[[el]]
    widgetType      <- widgetConfig$widgetType
    if(is.na(i)){
      j <- NA
      if(tolower(scalarsFileName) %in% names(modelIn)){
        j <- match(tolower(el), tolower(modelIn[[tolower(scalarsFileName)]]$symnames))
      }
      widgetConfig$widgetType <- NULL
      
      if(!is.na(j)){
        modelIn[[el_l]] <- list()
        
        if(!is.null(widgetConfig$alias)){
          modelIn[[el_l]]$alias <- widgetConfig$alias
          widgetConfig$alias    <- NULL
        }
        if(!is.null(widgetConfig$noHcube)){
          modelIn[[el_l]]$noHcube <- widgetConfig$noHcube
          widgetConfig$noHcube    <- NULL
        }
        if(!is.null(widgetConfig$noImport)){
          modelIn[[el_l]]$noImport <- widgetConfig$noImport
          widgetConfig$noImport    <- NULL
        }
        if(isTRUE(widgetConfig$clearValue) && !identical(widgetType, "dropdown")){
          config$textOnlySymbols <- c(config$textOnlySymbols, el)
          widgetConfig$clearValue  <- NULL
        }
        config$inputWidgets[[el]]     <- NULL
        modelIn[[el_l]][[widgetType]] <- widgetConfig
        modelIn[[tolower(scalarsFileName)]]$symnames <- modelIn[[tolower(scalarsFileName)]]$symnames[-c(j)]
        if(!length(modelIn[[tolower(scalarsFileName)]]$symnames)){
          # remove scalar table entirely if no scalar symbols are left
          modelIn[[tolower(scalarsFileName)]] <- NULL
          scalarIdOverwriteSheetOrder <- match(scalarsFileName, config$overwriteSheetOrder$input)
          if(!is.na(scalarIdOverwriteSheetOrder)){
            config$overwriteSheetOrder$input <- config$overwriteSheetOrder$input[-scalarIdOverwriteSheetOrder]
          }
        }else{
          modelIn[[tolower(scalarsFileName)]]$symtypes <- modelIn[[tolower(scalarsFileName)]]$symtypes[-c(j)]
          modelIn[[tolower(scalarsFileName)]]$symtext  <- modelIn[[tolower(scalarsFileName)]]$symtext[-c(j)]
        }
      }else if(any(startsWith(el, c(prefixDDPar, prefixGMSOpt)))){
        elL <- tolower(el)
        modelIn[[elL]]   <- list()
        
        if(!is.null(widgetConfig$alias)){
          modelIn[[elL]]$alias <- widgetConfig$alias
          widgetConfig$alias <- NULL
        }
        if(!is.null(widgetConfig$noHcube)){
          modelIn[[elL]]$noHcube <- widgetConfig$noHcube
          widgetConfig$noHcube <- NULL
        }
        if(!is.null(widgetConfig$noImport)){
          modelIn[[elL]]$noImport <- widgetConfig$noImport
          widgetConfig$noImport  <- NULL
        }
        modelIn[[elL]][[widgetType]] <- widgetConfig
      }else if(LAUNCHCONFIGMODE){
        invalidWidgetsToRender <- c(invalidWidgetsToRender, el)
      }else{
        errMsgTmp <- paste0("'", el, "' was defined to be an input widget, but is not part of the data contract!")
        errMsg <- paste(errMsg, errMsgTmp, sep = "\n")
      }
    }else{
      symDim          <- length(modelIn[[i]]$headers)
      if(symDim > 1L && !(widgetType %in% c("table", "custom"))){
        if(!(identical(widgetType, "dropdown") && identical(symDim, 2L) && identical(modelIn[[i]]$headers[[2]]$type, "string"))){
          errMsg <- paste(errMsg, sprintf("The output type for the GAMS symbol: '%s' is not valid. This widget type can not represent multi-dimensional data.", 
                                          names(modelIn)[i]), sep = "\n")
          next
        }
      }
      if(identical(symDim, 1L) && !(widgetType %in% c("table", "dropdown"))){
        errMsg <- paste(errMsg, sprintf("The output type for the GAMS symbol: '%s' is not valid. This widget type can not represent 1 dimensional data.", 
                                        names(modelIn)[i]), sep = "\n")
        next
      }
      widgetConfig$widgetType <- NULL
      
      if(!is.null(widgetConfig$options)){
        modelIn[[i]]$options      <- widgetConfig$options
        widgetConfig$options      <- NULL
      }
      if(identical(widgetType, "table")){
        if(identical(widgetConfig$bigData, TRUE) || identical(widgetConfig$tableType, "bigdata")){
          modelIn[[i]]$dtHeaders <- TRUE
        }else if(identical(widgetConfig$tableType, "pivot")){
          if(sum(vapply(modelIn[[i]]$headers, function(header){
            return(identical(header$type, "numeric"))}, logical(1L))) > 1L){
            errMsg <- paste(errMsg, sprintf("The GAMS symbol: %s is declared as table. This causes the last column to be pivoted in MIRO.\nMIRO pivot cannot be used with already pivoted symbols.\nPlease declare the symbol as a parameter instead.", 
                                            names(modelIn)[i]), sep = "\n")
            next
          }
          modelIn[[i]]$type <- "custom"
          modelIn[[i]]$rendererName <- "miroPivot"
          modelIn[[i]]$options <- c(modelIn[[i]]$options, 
                                    list("_input_" = TRUE,
                                         "_metadata_" = list(
                                           symname = names(modelIn)[i],
                                           headers = modelIn[[i]]$headers,
                                           symtype = modelIn[[i]]$symtype)))
          if(is.null(config$dataRendering[[names(modelIn)[i]]])){
            if(length(config$dataRendering)){
              config$dataRendering[[names(modelIn)[i]]] <- list(outType = "datatable")
            }else{
              config$dataRendering <- list(list(outType = "datatable"))
              names(config$dataRendering) <- names(modelIn)[i]
            }
          }
        }else if(!is.null(widgetConfig$dropdownCols)){
          colNames <- names(widgetConfig$dropdownCols)
          if(any(!colNames %in% names(modelIn[[i]]$headers))){
            errMsg <- paste(errMsg, sprintf("GAMS symbol: '%s': Invalid column name(s): '%s' in configuration for dropdown columns.", 
                                            names(modelIn)[i], paste(colNames[!colNames %in% names(modelIn[[i]]$headers)], collapse = "', '")),
                            sep = "\n")
            next
          }
          pivotColId <- NULL
          if(length(widgetConfig$pivotCols) && any(widgetConfig$dropdownCols %in% widgetConfig$pivotCols)){
            errMsg <- paste(errMsg, sprintf("The column: '%s' of the GAMS symbol: '%s': cannot be declared both as a pivot column and a drop-down column!", 
                                            paste(widgetConfig$dropdownCols[widgetConfig$dropdownCols %in% widgetConfig$pivotCols], collapse = "', '"),
                                            names(modelIn)[i]),
                            sep = "\n")
          }
          hasErr <- FALSE
          widgetConfig$dropdownCols <- lapply(widgetConfig$dropdownCols, function(dataSource){
            if(length(dataSource$static)){
              return(list(static = dataSource$static,
                          type = if(identical(dataSource$colType, "dropdown"))
                            "dropdown" else "autocomplete"))
            }
            if(!dataSource$symbol %in% names(modelIn)){
              errMsg <<- paste(errMsg, sprintf("The GAMS symbol: '%s' defined as data source for symbol: '%s' does not exist in data contract!", 
                                              dataSource$symbol, names(modelIn)[i]),
                              sep = "\n")
              hasErr <<- TRUE
              return(NULL)
            }
            colId <- match(dataSource$column, names(modelIn[[dataSource$symbol]]$headers))
            if(is.na(colId)){
              errMsg <<- paste(errMsg, sprintf("The GAMS symbol: '%s' defined as data source for symbol: '%s' does not have a column named: '%s'!", 
                                               dataSource$symbol, names(modelIn)[i], dataSource$column),
                               sep = "\n")
              hasErr <<- TRUE
              return(NULL)
            }
            if(length(config$inputWidgets[[dataSource$symbol]]) &&
               length(config$inputWidgets[[dataSource$symbol]]$pivotCols)){
              pivotColId <- match(config$inputWidgets[[dataSource$symbol]]$pivotCols[1],
                                  names(modelIn[[dataSource$symbol]]$headers))
              if(!is.na(pivotColId) && pivotColId <= colId){
                # need to adjust id in case column before is pivoted
                colId <- colId - 1L
              }
            }
            return(list(symbol = dataSource$symbol, colId = colId,
                        type = if(identical(dataSource$colType, "dropdown"))
                          "dropdown" else "autocomplete"))
          })
          if(hasErr){
            next
          }
          list(j = list(symbol = 1, column = 1))
          modelIn[[i]]$dropdownCols <- widgetConfig$dropdownCols
          widgetConfig$dropdownCols <- NULL
        }
      }
      if(!is.null(widgetConfig$alias)){
        modelIn[[i]]$alias <- widgetConfig$alias
        widgetConfig$alias <- NULL
      }
      if(!is.null(widgetConfig$noHcube)){
        modelIn[[i]]$noHcube <- widgetConfig$noHcube
        widgetConfig$noHcube <- NULL
      }
      if(!is.null(widgetConfig$noImport)){
        modelIn[[i]]$noImport <- widgetConfig$noImport
        widgetConfig$noImport  <- NULL
      }
      if(isTRUE(widgetConfig$clearValue) && !identical(widgetType, "dropdown")){
        config$textOnlySymbols <- c(config$textOnlySymbols, el)
        widgetConfig$clearValue  <- NULL
      }
      if(!is.null(widgetConfig$rendererName)){
        modelIn[[i]]$rendererName <- widgetConfig$rendererName
        if(length(widgetConfig$packages)){
          customPackages[[i]]       <- widgetConfig$packages
        }
        widgetConfig$rendererName  <- NULL
        widgetConfig$packages      <- NULL
      }
      if(!widgetType %in% c("table", "custom")){
        modelIn[[i]]$headers       <- NULL
        modelIn[[i]][[widgetType]] <- widgetConfig
        next
      }
      if(!is.null(widgetConfig[["label"]])){
        modelIn[[i]]$label <- widgetConfig[["label"]]
        widgetConfig$label  <- NULL
      }
      if(!is.null(widgetConfig[["hideIndexCol"]])){
        modelIn[[i]]$hideIndexCol  <- widgetConfig[["hideIndexCol"]]
        widgetConfig$hideIndexCol  <- NULL
      }
      if(!is.null(widgetConfig[["readonly"]])){
        modelIn[[i]]$readonly <- widgetConfig[["readonly"]]
        widgetConfig$readonly  <- NULL
      }
      if(isTRUE(widgetConfig[["heatmap"]])){
        modelIn[[i]]$heatmap <- TRUE
        widgetConfig$heatmap  <- NULL
      }
      if(!is.null(widgetConfig[["pivotCols"]])){
        modelIn[[i]]$pivotCols  <- widgetConfig$pivotCols
        widgetConfig$pivotCols  <- NULL
      }
      if(length(widgetConfig$readonlyCols)){
        for(col in widgetConfig$readonlyCols){
          if(col %in% names(modelIn[[i]]$headers)){
            modelIn[[i]]$headers[[col]]$readonly <- TRUE
          }else{
            errMsg <- paste(errMsg, sprintf("The column: '%s' of table: '%s' was set to be readonly. However, such a column does not exist in the table.", 
                                            names(modelIn)[[i]], names(modelIn[[i]]$headers)[[j]]))
            break
          }
        }
        
      }
    }
  }
  # make sure two input or output data sheets dont share the same name (case insensitive)
  if(any(duplicated(names(modelIn)))){
    errMsg <- "Two or more input datasets share the same name. Please make sure the identifiers are unique for each input datasheet!"
  }
  if(any(duplicated(names(modelOut)))){
    errMsg <- "Two or more output datasets share the same name. Please make sure the identifiers are unique for each output datasheet!"
  }
}

if(is.null(errMsg)){
  # declare GAMS compile time variables and GAMS options
  DDPar               <- getGMSPar(names(modelIn), prefixDDPar)
  GMSOpt              <- getGMSPar(names(modelIn), prefixGMSOpt)
  if(any(c(vapply(DDPar, function(el){ identical(nchar(trimws(substring(el, nchar(prefixDDPar) + 1L))), 0L)}, 
                logical(1L), USE.NAMES = FALSE),
           vapply(GMSOpt, function(el){ identical(nchar(trimws(substring(el, nchar(prefixGMSOpt) + 1L))), 0L)}, 
                  logical(1L), USE.NAMES = FALSE)))){
    errMsg <- "Unnamed GAMS command line parameter(s) detected. Empty names are not allowed!"
  }
  
  modelInToImport     <- getInputToImport(modelIn, keywordsNoImport)
  modelInMustImport   <- getInputToImport(modelIn, keywordsNoMustImport)
  # declare input and output aliases
  modelInAlias        <- vapply(seq_along(modelIn), function(i){
    if(is.null(modelIn[[i]]$alias)){
      names(modelIn)[[i]]
    }else{
      modelIn[[i]]$alias
    }
  }, character(1L), USE.NAMES = FALSE)
  modelInToImportAlias <- vapply(seq_along(modelInToImport), function(i){
    if(is.null(modelInToImport[[i]]$alias)){
      names(modelInToImport)[[i]]
    }else{
      modelInToImport[[i]]$alias
    }
  }, character(1L), USE.NAMES = FALSE)
  modelOutAlias <- vapply(seq_along(modelOut), function(i){
    if(is.null(modelOut[[i]]$alias)){
      names(modelOut)[[i]]
    }else{
      modelOut[[i]]$alias
    }
  }, character(1L), USE.NAMES = FALSE)
  # add input type to list
  lapply(seq_along(modelIn), function(i){
    tryCatch({
      modelIn[[i]]$type <<- getInputType(modelIn[[i]], keywordsType = keywordsType, 
                                         modelIn[[i]]$dropdown$multiple)
      if(names(modelIn)[[i]] %in% c(DDPar, GMSOpt) && 
         modelIn[[i]]$type %in% c("hot", "dt", "custom")){
        stop(sprintf("Tables are not supported for GAMS command line parameters ('%s'). 
                     Please specify another widget type.", modelInAlias[i]))
      }
      if(identical(modelIn[[i]]$type, "checkbox") && is.character(modelIn[[i]]$checkbox$max)){
        cbValueTmp <- strsplit(modelIn[[i]]$checkbox$max, "\\(|\\$")[[1]]
        if(length(cbValueTmp) %in% c(4L, 5L) && cbValueTmp[[1]] %in% listOfOperators){
          cbValueTmp <- gsub(")", "", cbValueTmp, fixed = TRUE)
          modelIn[[i]]$checkbox$operator <<- cbValueTmp[[1]]
          modelIn[[i]]$checkbox$max      <<- paste(cbValueTmp[c(-1)], collapse = "$")
        }else{
          stop(sprintf("The checkbox: '%s' has a bad dependency format. Format for checkboxes dependent on other datasets should be:
                       operator(dataset$column) or operator(dataset$keyColumn[key]$valueColumn). 
                       Currently, the following operators are supported: '%s'.", modelInAlias[i], 
                       paste(listOfOperators, collapse = "', '")))
        }
      }
      }, error = function(e){
        errMsg <<- paste(errMsg, paste0(modelInAlias[i], 
                                        " has no valid input type defined. Error message: ",
                                        e), sep = "\n")
      })
      })
    }
if(is.null(errMsg)){
  # declare hidden output scalars and remove entire scalar table if all hidden
  if(length(config$hiddenOutputScalars)){
    config$hiddenOutputScalars <- tolower(config$hiddenOutputScalars)
    outScalarTmp <- character(0L)
    if(scalarsOutName %in% names(modelOut)){
      outScalarTmp  <- modelOut[[scalarsOutName]]$symnames
    }
    isValidScalar <- config$hiddenOutputScalars %in% outScalarTmp
    if(any(!isValidScalar)){
      errMsg <- paste(errMsg, sprintf("Some output scalars you declared to be hidden were not defined in your GAMS model as scalars to be displayed in MIRO. 
These scalars are: '%s'. Please either add them in your model or remove them from the list of hidden output scalars in your '%s.json' file.", 
                                      config$hiddenOutputScalars[!config$hiddenOutputScalars %in% names(modelOut)], modelName), sep = "\n")
    }else if(length(outScalarTmp) > 0L && identical(sum(isValidScalar), length(outScalarTmp))){
      modelOut[[scalarsOutName]]$hidden <- TRUE
    }
    rm(outScalarTmp, isValidScalar)
  }
  
  if(length(config$overwriteSheetOrder$input) && !LAUNCHCONFIGMODE){
    isWidgetGroup <- startsWith(config$overwriteSheetOrder$input, "_widgets")
    if(any(isWidgetGroup)){
      isInputWidget <- vapply(modelIn, function(el){
        if(el$type %in% c("hot", "dt", "custom")){
          return(FALSE)
        }
        return(TRUE)
      }, logical(1L), USE.NAMES = FALSE)
      if(any(isInputWidget)){
        remainingWidgetsUnassigned <- TRUE
        for(widgetGroupId in which(isWidgetGroup)){
          i <- suppressWarnings(as.integer(substring(config$overwriteSheetOrder$input[widgetGroupId], 9L)))
          if(is.na(i)){
            if(remainingWidgetsUnassigned){
              if(length(config$inputWidgetGroups)){
                firstUnassignedWidgetId <- which(!names(modelIn)[isInputWidget] %in% unlist(lapply(config$inputWidgetGroups, function(el){
                  return(el$members)
                }), use.names = FALSE))[1]
                if(is.na(firstUnassignedWidgetId)){
                  config$overwriteSheetOrder$input[widgetGroupId] <- NA
                  remainingWidgetsUnassigned <- FALSE
                  next
                }
              }else{
                firstUnassignedWidgetId <- 1L
              }
              config$overwriteSheetOrder$input[widgetGroupId] <- names(modelIn)[isInputWidget][firstUnassignedWidgetId]
              remainingWidgetsUnassigned <- FALSE
            }
            next
          }else if(i > length(config$inputWidgetGroups)){
            if(remainingWidgetsUnassigned && sum(isWidgetGroup) <= length(config$inputWidgetGroups)){
              if(length(config$inputWidgetGroups)){
                firstUnassignedWidgetId <- which(!names(modelIn)[isInputWidget] %in% unlist(lapply(config$inputWidgetGroups, function(el){
                  return(el$members)
                }), use.names = FALSE))[1]
                if(is.na(firstUnassignedWidgetId)){
                  config$overwriteSheetOrder$input[widgetGroupId] <- NA
                  remainingWidgetsUnassigned <- FALSE
                  next
                }
              }else{
                firstUnassignedWidgetId <- 1L
              }
              config$overwriteSheetOrder$input[widgetGroupId] <- names(modelIn)[isInputWidget][firstUnassignedWidgetId]
              remainingWidgetsUnassigned <- FALSE
            }else{
              config$overwriteSheetOrder$input[widgetGroupId] <- NA
            }
            next
          }
          widgetIdTmp <- match(config$inputWidgetGroups[[i]]$members[1], names(modelIn))
          if(is.na(widgetIdTmp)){
            errMsg <- paste(errMsg, sprintf("The widget: '%s' of the input widget group: '%s' does not exist.",
                                            config$inputWidgetGroups[[i]]$members[1], config$inputWidgetGroups[[i]]$name),
                            sep = "\n")
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
      }else{
        config$overwriteSheetOrder$input <- config$overwriteSheetOrder$input[-which(isWidgetGroup)]
      }
    }
    if(any(is.na(match(config$overwriteSheetOrder$input, names(modelIn))))){
      errMsg <- paste(errMsg, "Some of the input elements in the 'overwriteSheetOrder' option are not defined in the data model!",
                      sep = "\n")
    }else{
      appendSheetIds <- NULL
      if(length(config$overwriteSheetOrder$input) != length(modelIn)){
        appendSheetIds <- seq_along(modelIn)[!names(modelIn) %in% config$overwriteSheetOrder$input]
      }
      inputSheetIdsToDisplay <- c(match(unique(config$overwriteSheetOrder$input), 
                                        names(modelIn)), appendSheetIds)
    }
  }else{
    inputSheetIdsToDisplay <- seq_along(modelIn)
  }
}
if(is.null(errMsg)){
  widgetIds    <- lapply(inputSheetIdsToDisplay, function(i){
    if(modelIn[[i]]$type %in% c("hot", "dt", "custom")){
      return(NULL)
    }else{
      return(i)
    }
  })
  
  # Hypercube mode configuration
  if(LAUNCHHCUBEMODE){
    scalarSymbolsBase <- lapply(seq_along(modelIn), function(i){
      if(!isTRUE(modelIn[[i]]$noHcube)){
        switch(modelIn[[i]]$type,
               checkbox = {
                 modelIn[[i]]$type <<- "dropdown"
                 modelIn[[i]]$dropdown$label <<- modelIn[[i]]$checkbox$label
                 value <- modelIn[[i]]$checkbox$value
                 if(is.null(modelIn[[i]]$checkbox$max) || !is.na(suppressWarnings(as.integer(modelIn[[i]]$checkbox$max)))){
                   modelIn[[i]]$dropdown$aliases <<- lang$nav$hcubeMode$checkboxAliases
                   modelIn[[i]]$dropdown$choices <<- c(0L, 1L)
                 }else{
                   modelIn[[i]]$checkbox$max      <<- paste0("$", modelIn[[i]]$checkbox$max)
                   modelIn[[i]]$dropdown$operator <<- modelIn[[i]]$checkbox$operator
                   modelIn[[i]]$dropdown$choices  <<- modelIn[[i]]$checkbox$max
                 }
                 modelIn[[i]]$dropdown$selected <<- modelIn[[i]]$checkbox$value
                 modelIn[[i]]$dropdown$width <<- modelIn[[i]]$checkbox$width
                 modelIn[[i]]$dropdown$multiple <<- TRUE
                 modelIn[[i]]$dropdown$checkbox <<- TRUE
                 modelIn[[i]]$checkbox <<- NULL
                 return(names(modelIn)[i])
               },
               dropdown = {
                 if(!isTRUE(modelIn[[i]]$dropdown$multiple)){
                   if(identical(modelIn[[i]]$symtype, "set")){
                     warningMsgTmp <- sprintf("The dataset: '%s' is a set configured as a single dropdown menu. Single dropdown menus for sets are not expanded in Hypercube mode! Use singleton set instead.", 
                                              names(modelIn)[i])
                     warning(warningMsgTmp)
                     warningMsg <<- paste(warningMsg, warningMsgTmp, sep = "\n")
                   }else{
                     # specify that dropdown menu is originally a single select menu
                     modelIn[[i]]$dropdown$single   <<- TRUE
                     modelIn[[i]]$dropdown$multiple <<- TRUE
                   }
                   return(names(modelIn)[i])
                 }
               },
               slider = {
                 if(length(modelIn[[i]]$slider$default) == 1){
                   modelIn[[i]]$slider$single <<- TRUE
                   modelIn[[i]]$slider$default <<-  rep(modelIn[[i]]$slider$default, 2L)
                 }else{
                   modelIn[[i]]$slider$double <<- TRUE
                 }
               },
               date =,
               daterange = ,
               textinput = ,
               numericinput = {
                 warningMsgTmp <- sprintf("The dataset: '%s' uses a widget that is not supported in Hypercube mode.
                                   Thus, it will not be transformed and stays static.", 
                                          names(modelIn)[i])
                 warning(warningMsgTmp)
                 warningMsg <<- paste(warningMsg, warningMsgTmp, sep = "\n")
               })
      }
    })
  }else{
    scalarSymbolsBase <- character(0L)
  }
  
  widgetIds    <- unlist(widgetIds[!vapply(widgetIds, is.null,
                                           numeric(1L), USE.NAMES = FALSE)], 
                         use.names = FALSE)
  config$inputGroups <- c(config$inputGroups, 
                          genWidgetGroups(names(modelIn)[widgetIds], 
                                          config$inputWidgetGroups, 
                                          lang$nav$inputScreen$widgetTabTitle, 
                                          aggregateWidgets = isTRUE(config$aggregateWidgets)))
  inputTabs    <- getTabs(names(modelIn), modelInAlias, config$inputGroups,
                          idsToDisplay = inputSheetIdsToDisplay, widgetIds = widgetIds)
  inputTabTitles <- inputTabs$tabTitles
  tabSheetMap <- list(input = NULL, output = NULL)
  tabSheetMap$input <- inputTabs$tabSheetMap
  inputTabs    <- inputTabs$tabs
  # get input tabs where scalars are merged to single table (scenario comparison mode)
  scenInputTabs    <- getTabs(names(modelIn), modelInAlias, config$inputGroups,
                              idsToDisplay = inputSheetIdsToDisplay, 
                              widgetIds = widgetIds, scalarsTabName = lang$nav$scalarAliases$scalars,
                              mergeScalars = TRUE,
                              widgetIdsMultiDim = vapply(widgetIds, function(widgetId){
                                if(names(modelIn)[widgetId] %in% names(modelInRaw)){
                                  return(widgetId)
                                }else{
                                  return(0L)
                                }
                              }, integer(1L), USE.NAMES = FALSE))
  scenInputTabTitles <- scenInputTabs$tabTitles
  scenInputTabs    <- scenInputTabs$tabs
  
  # read graph data for input and output sheets
  config$activateModules$miroLogFile <- length(config$miroLogFile) > 0L && 
    nchar(config$miroLogFile) > 2L
  # get remote import/export options
  externalDataConfig <- list(remoteImport = NULL, remoteExport = NULL)
  
  for(direction in c("remoteImport", "remoteExport")){
    if(length(config[[direction]])){
      externalDataConfig[[direction]] <- vector("list", length(config[[direction]]))
      for (i in seq_along(config[[direction]])){
        remoteConfigs <- lapply(config[[direction]][[i]]$templates, function(remoteConfig){
          symNames <- tolower(remoteConfig[["symNames"]])
          remoteConfig[["symNames"]] <- NULL
          
          symIds <- match(symNames, c(names(modelIn), names(modelOut)))
          if(any(is.na(symIds))){
            errMsg <<- paste(errMsg, sprintf("Some of the datasets you selected for remote export: '%s' are not valid: '%s'.", 
                                             config[[direction]][[i]]$name, 
                                             paste(symNames[is.na(symIds)], 
                                                   collapse = "', '")))
            return()
          }
          dupSym <- !is.na(match(symNames, names(externalDataConfig[[direction]][[i]])))
          if(any(dupSym)){
            errMsg <<- paste(errMsg, sprintf("Duplicated datasets found in remote export: '%s'. Datasets: '%s'.", 
                                             config[[direction]][[i]]$name,
                                             paste(symNames[dupSym], 
                                                   collapse = "', '")))
            return()
          }
          
          exportConfig <- rep.int(list(remoteConfig), length(symIds))
          names(exportConfig) <- c(names(modelIn), names(modelOut))[symIds]
          return(exportConfig)
        })
        externalDataConfig[[direction]][[i]] <- unlist(remoteConfigs, recursive = FALSE, use.names = TRUE)
      }
      names(externalDataConfig[[direction]]) <- vapply(config[[direction]], "[[",
                                            character(1L), "name", USE.NAMES = FALSE)
      config[[direction]] <- NULL
    }
  }
  externalInputConfig  <- externalDataConfig[["remoteImport"]]
  datasetsRemoteExport <- externalDataConfig[["remoteExport"]]
  rm(externalDataConfig)
  
  # get input sheets with dependencies on other sheets
  # get dropdown dependencies
  
  ddownDep         <- list()
  choicesNoDep     <- list()
  aliasesNoDep     <- list()
  sliderValues     <- list()
  modelInWithDep   <- list()
  dependentDatasets <- vector("list", length = length(modelIn))
  
  modelInTabularData <- lapply(seq_along(modelIn), function(i){
    name <- names(modelIn)[[i]]
    switch(modelIn[[i]]$type,
           dropdown = {
             # a dropdown menu cannot use the reserved name for the scalar table
             if(names(modelIn)[[i]] %in% c(scalarsFileName, scalarsOutName)){
               errMsg <<- paste(errMsg, paste0("The dropdown menu: '", modelInAlias[i], 
                                               "' uses a reserved name as the identifier. Please choose a different name."), sep = "\n")
             }
             # get dependencies
             tryCatch({
               choices <- getDependenciesDropdown(choices = modelIn[[i]]$dropdown$choices, modelIn = modelIn, name = name)
               if(!is.null(modelIn[[i]]$dropdown$aliases)){
                 aliases <- getDependenciesDropdown(choices = modelIn[[i]]$dropdown$aliases, modelIn = modelIn, name = name)
               }else{
                 aliases <- NULL
               }
             }, error = function(e){
               errMsg <<- paste(errMsg, paste0("'", modelInAlias[i], 
                                               "' has no valid input type defined. Error message: ", e), sep = "\n")
             })
             if(!is.null(errMsg)){
               if(LAUNCHCONFIGMODE){
                 errMsg <<- NULL
                 return(NULL)
               }else{
                 return(NULL)
               }
             }
             
             # check whether dropdown menu uses shared data
             if(length(choices$shared > 0) && length(choices$shared) != 1){
               errMsg <<- paste(errMsg,paste0("The dropdown menu : '", modelInAlias[i], 
                                              "' refers to a shared database. However, currently a maximum of 1 column from an external source is supported."), sep = "\n")
               return(NULL)
             }else if(length(choices$shared) == 1){
               if(length(aliases$shared) > 0 && length(aliases$shared) != 1){
                 errMsg <<- paste(errMsg,paste0("The choices for dropdown menu '", modelInAlias[i], 
                                                "' do not match the number of aliases."), sep = "\n")
                 return(NULL)
               }else if(length(aliases$shared) > 0){
                 ddownDep[[name]]$aliases <<- aliases$shared
               }
               # remove identifier string from dropdown that specifies where shared data comes from 
               ddownDep[[name]]$shared <<- choices$shared
               
               colSubset <- character(0L)
               if(!identical(name, choices$shared)){
                 # only subset of columns will be imported
                 colSubset <- c(tolower(ddownDep[[name]]$shared), tolower(ddownDep[[name]]$aliases))
               }
               externalInputConfig[[i]] <<- list(colSubset = colSubset)
             }
             # in case dropdown menu has aliases, validate that they are of matching length as choices
             if(!is.null(aliases)){
               if(length(aliases$strings) != length(choices$strings)){
                 errMsg <<- paste(errMsg,paste0("The number of fixed aliases for dropdown menu: ", modelInAlias[i], 
                                                " does not match the number of choices without dependencies. 
                                                Aliases: '", paste(aliases$strings, collapse = ","), 
                                                "'. Choices: '", paste(choices$strings, collapse = ","), "'."), sep = "\n")
                 return(NULL)
               }else if(length(aliases$fw) != length(choices$fw)){
                 errMsg <<- paste(errMsg,paste0("The number of aliases with dependencies for dropdown menu: ", 
                                                modelInAlias[i], " does not match the number of choices with dependencies. 
                                                Aliases: '", paste(aliases$fw, collapse = ","), 
                                                "'. Choices: '", paste(choices$fw, collapse = ","), "'."), sep = "\n")
                 return(NULL)
                 # sheet names of aliases and choices do not match
               }else if(any(vapply(names(aliases$fw), function(sheet){
                 if(sheet %in% names(choices$fw)) return(F) else return(T)}, logical(1)))){
                 errMsg <<- paste(errMsg,paste0("When both the choices and their aliases have dependencies on external data sheets, please make sure they depend on the same dataset.
                                                (dropdown menu: '", modelInAlias[i], "', aliases: '", paste(aliases$fw, collapse = ","), "', choices: '", paste(choices$fw, collapse = ","), "'."), sep = "\n")
                 return(NULL)
               }
             }
             # set vector of choices to static ones
             modelIn[[i]]$dropdown$choices   <<- choices$strings
             modelIn[[i]]$dropdown$aliases   <<- aliases$strings
             ddownDep[[name]]$fw             <<- choices$fw
             ddownDep[[name]]$bw             <<- choices$bw
             ddownDep[[name]]$aliases        <<- aliases$fw
             choicesNoDep[[name]]            <<- choices$strings
             aliasesNoDep[[name]]            <<- aliases$strings
             if(length(choices$fw)){
               modelInWithDep[[name]]        <<- modelIn[i]
             }
             if(identical(modelIn[[i]]$symtype, "set")){
               modelIn[[i]]$headers <<- modelInRaw[[name]]$headers
               return(name)
             }else if(isTRUE(modelIn[[i]]$dropdown$multiple)){
               modelIn[[i]]$headers <<- list(list(type = "string", 
                                                  alias = modelIn[[i]]$alias))
               names(modelIn[[i]]$headers) <<- names(modelIn)[[i]]
               return(name)
             }else{
               return(NULL)
             }
             },
           slider = {
             # a slider cannot use the reserved name for the scalar table
             if(names(modelIn)[[i]] %in% c(scalarsFileName, scalarsOutName)){
               errMsg <<- paste(errMsg, 
                                paste0("The slider: '", modelInAlias[i], 
                                       "' uses a reserved name as its identifier. Please choose a different name."),
                                sep = "\n")
             }
             tryCatch({
               sliderValues[[name]] <<- getDependenciesSlider(min = modelIn[[i]]$slider$min, 
                                                              max = modelIn[[i]]$slider$max, 
                                                              def = modelIn[[i]]$slider$default, 
                                                              step = modelIn[[i]]$slider$step,
                                                              modelIn = modelIn, 
                                                              listOfOperators = listOfOperators)
               # no dependencies for slider
               if(is.null(sliderValues[[name]])){
                 
                 sliderValues[[name]] <<- list("min" = as.numeric(modelIn[[i]]$slider$min), 
                                               "max" = as.numeric(modelIn[[i]]$slider$max), 
                                               "def" = as.numeric(modelIn[[i]]$slider$default), 
                                               "step" = as.numeric(modelIn[[i]]$slider$step))
                 if(suppressWarnings(any(is.na(sliderValues[[name]])))){
                   errMsg <<- paste(errMsg, 
                                    paste0("The slider: '", modelInAlias[i], 
                                           " has non numeric values. Please make sure min, max etc. are numeric."),
                                    sep = "\n")
                 }
               }else{
                 modelInWithDep[[name]]   <<- modelIn[[i]]
               }
               # used to access default value inside lapply (in input_render_nontab.R)
               attributes(sliderValues[[name]]$def) <- list(ref = "def")
             }, error = function(e){
               errMsg <<- paste(errMsg,paste0("'", modelInAlias[i], 
                                              "' has no valid input type defined. Error message: ", e), sep = "\n")
             })
             return(NULL)
           },
           date = {
             if(names(modelIn)[[i]] %in% c(scalarsFileName, scalarsOutName)){
               errMsg <<- paste(errMsg, paste0("The date selector: '", modelInAlias[i], 
                                               "' uses a reserved name as its identifier. Please choose a different name."), sep = "\n")
             }
             # TODO : support dependency
             return(NULL)
           },
           daterange = {
             if(names(modelIn)[[i]] %in% c(scalarsFileName, scalarsOutName)){
               errMsg <<- paste(errMsg, paste0("The date range selector: '", modelInAlias[i], 
                                               "' uses a reserved name as its identifier. Please choose a different name."), sep = "\n")
             }
             # TODO : support dependency
             return(NULL)
           },
           textinput = {
             if(names(modelIn)[[i]] %in% c(scalarsFileName, scalarsOutName)){
               errMsg <<- paste(errMsg, paste0("The textInput: '", modelInAlias[i], 
                                               "' uses a reserved name as its identifier. Please choose a different name."), sep = "\n")
             }
             return(NULL)
           },
           numericinput = {
             if(names(modelIn)[[i]] %in% c(scalarsFileName, scalarsOutName)){
               errMsg <<- paste(errMsg, paste0("The numericInput: '", modelInAlias[i], 
                                               "' uses a reserved name as its identifier. Please choose a different name."), sep = "\n")
             }
             return(NULL)
           },
           checkbox = {
             if(names(modelIn)[[i]] %in% c(scalarsFileName, scalarsOutName)){
               errMsg <<- paste(errMsg, paste0("The checkbox: '", modelInAlias[i], 
                                               "' uses a reserved name as its identifier. Please choose a different name."), sep = "\n")
             }
             if(is.character(modelIn[[i]]$checkbox$max)){
               # checkbox has dependency
               # BEGIN error checks
               if(grepl("\\$+$", modelIn[[i]]$checkbox$max)){
                 errMsg <<- paste(errMsg,paste0("The checkbox: '", modelInAlias[i], 
                                                "' has a backward dependency assigned. Currently only forward dependencies are supported for checkboxes."), sep = "\n")
                 return(NULL)
               }
               # END error checks
               
               cbValue <- strsplit(modelIn[[i]]$checkbox$max, "\\$")[[1]]
               idx1    <- match(cbValue[1], names(modelIn))[1]
               if(!is.na(idx1)){
                 # add forward dependency
                 modelIn[[i]]$checkbox$sheetId <<- idx1
                 tryCatch(modelIn[[i]]$checkbox$max   <<- getNestedDep(cbValue[c(-1)]), error = function(e){
                   errMsg <<- paste(errMsg, conditionMessage(e))
                 })
                 
                 modelInWithDep[[name]]        <<- modelIn[[i]]
               }else{
                 errMsg <<- paste(errMsg,paste0("The dependent dataset: '", cbValue[1], "' for checkbox: '", 
                                                modelInAlias[i], "' could not be found. Please make sure you define a valid reference."), sep = "\n")
               }
             }
             return(NULL)
           },
           {
             # check that in case dataset is scalar ds, it has correct headers
             if(names(modelIn)[[i]] %in% c(scalarsFileName, scalarsOutName) && 
                !identical(names(modelIn[[i]]$headers), scalarsFileHeaders)){
               warningMsgTmp <- paste0(modelInAlias[i], " is defined to be the scalar input dataset, ",
                                         "but has incorrect headers. The headers were adjusted accordingly.")
               warning(warningMsgTmp)
               warningMsg <<- paste(warningMsg, warningMsgTmp, sep = "\n")
               names(modelIn[[i]]$headers) <- scalarsFileHeaders
             }
             return(name)
           }
    )
  })
  dependentDatasets <- lapply(seq_along(modelIn), function(i){
    dependentDataIds <- vapply(seq_along(ddownDep), function(j){
      if(names(modelIn)[[i]] %in% names(ddownDep[[j]]$fw)){
        return(match(names(ddownDep)[[j]], names(modelIn)))
      }else{
        return(NA_integer_)
      }
    }, integer(1L), USE.NAMES = FALSE)
    dependentDataIds <- dependentDataIds[!is.na(dependentDataIds)]
    if(!length(dependentDataIds)){
      dependentDataIds <- NULL
    }
    return(dependentDataIds)
  })
  
  modelInTabularData <- unlist(modelInTabularData, use.names = FALSE)
  # get scalar input names
  scalarInputSym <- names(modelIn)[vapply(seq_along(modelIn), function(i){
    if(!"headers" %in% names(modelIn[[i]]) || 
       isTRUE(modelIn[[i]]$dropdown$single) || 
       isTRUE(modelIn[[i]]$dropdown$checkbox)){
      return(TRUE)
    }
    return(FALSE)
  }, logical(1L), USE.NAMES = FALSE)]
  modelInTabularDataBase <- modelInTabularData
  if(length(modelInTabularDataBase))
    modelInTabularDataBase <- modelInTabularDataBase[!modelInTabularDataBase %in% scalarInputSym]
  }

if(is.null(errMsg)){
  if(LAUNCHHCUBEMODE && scalarsFileName %in% names(modelIn)){
    scalarInputSymToVerify <- unlist(lapply(scalarInputSym, function(el){
      if(identical(modelIn[[el]]$type, "slider") && length(modelIn[[el]]$slider$default) > 1){
        if(isTRUE(modelIn[[el]]$slider$double)){
          return(paste0(el, c("","$lo", "$up", "$step", "$mode")))
        }
        return(paste0(el, c("", "$lo", "$up", "$step")))
      }else if(identical(modelIn[[el]]$type, "daterange")){
        return(paste0(el, c("", "$lo", "$up")))
      }
      return(NULL)
    }), use.names = FALSE)
  }else{
    scalarInputSymToVerify <- NULL
  }
  # determine the filenames for the model input datasets
  if(scalarsFileName %in% modelInTabularData){
    scalarInputSym <- c(scalarInputSym, modelIn[[scalarsFileName]]$symnames)
    # scalars should always be the highest indexed dataset
    modelInFileNames <- c(modelInTabularData[modelInTabularData != scalarsFileName], scalarsFileName)
    inputDsAliases   <- modelInAlias[match(modelInFileNames, names(modelIn))]
  }else if(length(modelIn) > (length(modelInTabularData) - length(scalarSymbolsBase))){
    modelInFileNames <- c(modelInTabularData, scalarsFileName)
    inputDsAliases   <- c(modelInAlias[match(modelInTabularData, names(modelIn))], lang$nav$scalarAliases$scalars)
  }else{
    modelInFileNames <- modelInTabularData
    inputDsAliases   <- modelInAlias[match(modelInTabularData, names(modelIn))]
  }
  inputDsNames <- modelInFileNames
  # create list of dependencies for each column of input data
  # first define data sheets without forward dependencies on other sheets (tabular data)
  if(length(modelInWithDep)){
    modelInNoDep <- names(modelIn[-match(names(modelInWithDep), names(modelIn))])
  }else{
    modelInNoDep <- names(modelIn)
  }
  
  # initialise list that contains dependency information about each input dataset
  colsWithDep <- vector(mode = "list", length = length(modelIn))
  
  # find dependencies
  lapply(modelInNoDep, function(sheet){
    # get input element id of dataset without dependency
    i <- match(tolower(sheet), names(modelIn))
    # find columns of dataset i with dependency
    
    lapply(names(modelIn[[i]]$headers), function(col){
      lapply(names(modelIn), function(sheetDep){
        # test if sheetDep has a forward dependency on considered sheet without forward dependencies
        if(col %in% ddownDep[[sheetDep]]$bw[[sheet]]){
          if(col %in% names(colsWithDep[[i]])){
            errMsg <<- paste(errMsg,paste0("Column: '", col, "' of input sheet '", sheet, 
                                           "' has more than one dependency. Only one backward dependency per column is allowed."), 
                             sep = "\n")
          }else{
            id <- match(tolower(sheetDep), names(modelIn))
            colsWithDep[[i]][[col]] <<- id
          }
        }
      })
    })
  })
  # find ID columns (sets in GAMS) for each input data sheet
  idsIn <- vector(mode = "list", length = length(modelIn))
  for(inputName in modelInTabularData){
    i <- match(tolower(inputName), names(modelIn))[[1]]
    if(!is.null(modelIn[[i]]$headers) && length(modelIn[[i]]$headers)){
      idsIn[[i]] <- unlist(lapply(1:length(modelIn[[i]]$headers), function(j){
        if(modelIn[[i]]$headers[[j]]$type == "string"){
          return(names(modelIn[[i]]$headers)[[j]])
        }
      }), use.names = FALSE)
    }
  }
  scalarsInMetaData <- NULL
  scalarsInTemplate <- NULL
  if(scalarsFileName %in% inputDsNames){
    scalarsInMetaData <- list(list(alias = "Input Scalars",
                                   colTypes = "ccc",
                                   headers = list(
                                     'a' = list(type = "string"),
                                     'b' = list(type = "string"),
                                     'c' = list(type = "string")
                                   ),
                                   symnames = c(modelInRaw[[scalarsFileName]]$symnames,
                                                DDPar, GMSOpt),
                                   symtext = c(modelInRaw[[scalarsFileName]]$symtext,
                                               character(length(DDPar) + length(GMSOpt))))
    )
    names(scalarsInMetaData[[1]]$headers) <- scalarsFileHeaders
    names(scalarsInMetaData) <- scalarsFileName
    
    if(length(modelIn[[scalarsFileName]]$symnames)){
      scalarsInTemplate        <- tibble('a' = modelIn[[scalarsFileName]]$symnames, 
                                         'b' = modelIn[[scalarsFileName]]$symtext, 
                                         'c' = NA_character_)
    }else{
      scalarsInTemplate        <- tibble('a' = character(0L), 
                                         'b' = character(0L), 
                                         'c' = character(0L))
    }
    
    names(scalarsInTemplate) <- scalarsFileHeaders
    attr(scalarsInTemplate, "aliases") <- c(lang$scalarAliases$cols$name, 
                                            lang$scalarAliases$cols$desc,
                                            lang$scalarAliases$cols$value)
  }
  # initialize data frames for model input data
  modelInTemplate <- vector(mode = "list", length = length(modelIn))
  lapply(modelInTabularData, function(el){
    i <- match(el, names(modelIn))
    if(!is.null(modelIn[[i]]$headers)){
      headers   <- lapply(modelIn[[i]]$headers, function(header){
        if(identical(header$type, "numeric")){
          return(numeric())
        }
        return(character())
      })
      names(headers) <- names(modelIn[[i]]$headers)
      
      if(identical(el, scalarsFileName)){
        modelInTemplate[[i]] <<- scalarsInTemplate
      }else if(identical(el, scalarEquationsName)){
        modelInTemplate[[i]] <<- tibble(a = modelIn[[scalarEquationsName]]$symnames,
                                        b = modelIn[[scalarEquationsName]]$symtext,
                                        c = NA_real_,
                                        d = NA_real_,
                                        e = NA_real_,
                                        f = NA_real_,
                                        g = NA_real_)
        names(modelInTemplate[[i]]) <<- names(headers)
      }else{
        modelInTemplate[[i]] <<- tibble(!!!headers)
      }
      attr(modelInTemplate[[i]], "aliases") <<- vapply(seq_along(modelIn[[i]]$headers), function(j){
        alias <- modelIn[[i]]$headers[[j]]$alias
        if(!length(alias)){
          return(names(modelIn[[i]]$headers)[j])
        }
        return(alias)
      }, character(1L), USE.NAMES = FALSE)
      attr(modelInTemplate[[i]], "isTable") <<- sum(vapply(modelIn[[i]]$headers, function(hdr){
        identical(hdr$type, "numeric")
      }, logical(1L), USE.NAMES = FALSE)) > 1L
        
      if(length(modelIn[[i]]$pivotCols)){
        if(any(!modelIn[[i]]$pivotCols %in% names(modelIn[[i]]$headers))){
          errMsg <<- paste(errMsg, sprintf("Some columns you want to pivot could not be found in the symbol: '%s'.", 
                                           modelInAlias[i]))
        }else if(length(modelIn[[i]]$headers) < 3L || 
                 sum(vapply(modelIn[[i]]$headers, 
                            function(header) identical(header$type, "numeric"), 
                            logical(1L), USE.NAMES = FALSE)) > 1L){
          errMsg <<- paste(errMsg, sprintf("You may only pivot symbols that have at least a dimension of 2 and have at most 1 value column (symbol: '%s').", 
                                           modelInAlias[i]))
        }
      }
      
      # abort since rpivottable crashes when setting table to readonly if there exist columns with the same name
      if(!LAUNCHCONFIGMODE &&
         identical(modelIn[[i]]$type, "hot") && any(duplicated(attr(modelInTemplate[[i]], "aliases"))) &&
         (identical(modelIn[[i]]$readonly, TRUE) || any(vapply(modelIn[[i]]$headers, function(header){
           if(identical(header$readonly, TRUE))
             return(TRUE)
           return(FALSE)
         }, logical(1L), USE.NAMES = FALSE)))){
        
        errMsg <<- paste(errMsg, sprintf(paste0("It is currently not supported to define a table (or certain columns)",
" to be readonly if this table contains columns with identical names. Please rename the columns ", 
"(by adjusting the explanatory text of your GAMS symbol) or remove the readonly attribute. ", 
"Table that causes the problem: '%s'."), modelInAlias[i]), sep = "\n")
      }
    }
  })
  modelOutTemplate <- vector(mode = "list", length = length(modelOut))
  # declare set of output sheets that should be displayed in webUI
  if(!LAUNCHCONFIGMODE && length(config[["hiddenOutputSymbols"]])){
    invalidHiddenOutputSymbols <- match(config[["hiddenOutputSymbols"]], names(modelOut))
    if(any(is.na(invalidHiddenOutputSymbols))){
      errMsg <- paste(errMsg, sprintf("The output symbols: '%s' you want to be hidden do not exist!",
                                      paste0(config[["hiddenOutputSymbols"]][is.na(invalidHiddenOutputSymbols)], 
                                             collapse = ", ")), sep = "\n")
    }
  }
  modelOutToDisplay <- vapply(seq_along(modelOut), function(i){
    headers   <- vector(mode = "numeric", length = length(modelOut[[i]]$headers))
    headers   <- lapply(modelOut[[i]]$headers, function(header){
      if(identical(header$type, "numeric")){
        return(numeric())
      }
      return(character())
    })
    names(headers) <- names(modelOut[[i]]$headers)
    if(identical(names(modelOut)[i], scalarsOutName)){
      nonHiddenScalars <- !modelOut[[scalarsOutName]]$symnames %in% config$hiddenOutputScalars
      
      modelOutTemplate[[i]] <<- tibble('a' = modelOut[[scalarsOutName]]$symnames[nonHiddenScalars], 
                                       'b' = modelOut[[scalarsOutName]]$symtext[nonHiddenScalars], 
                                       'c' = NA_character_)
      names(modelOutTemplate[[i]]) <<- names(headers)
    }else if(identical(names(modelOut)[i], scalarEquationsOutName)){
      modelOutTemplate[[i]] <<- tibble(a = modelOut[[scalarEquationsOutName]]$symnames,
                                      b = modelOut[[scalarEquationsOutName]]$symtext,
                                      c = NA_real_,
                                      d = NA_real_,
                                      e = NA_real_,
                                      f = NA_real_,
                                      g = NA_real_)
      names(modelOutTemplate[[i]]) <<- names(headers)
    }else{
      modelOutTemplate[[i]] <<- tibble::tibble(!!!headers)
    }
    
    attr(modelOutTemplate[[i]], "aliases") <<- vapply(seq_along(modelOut[[i]]$headers), function(j){
      alias <- modelOut[[i]]$headers[[j]]$alias
      if(!length(alias)){
        return(names(modelOut[[i]]$headers)[j])
      }
      return(alias)
    }, character(1L), USE.NAMES = FALSE)
    
    attr(modelOutTemplate[[i]], "isTable") <<- sum(vapply(modelOut[[i]]$headers, function(hdr){
      identical(hdr$type, "numeric")
    }, logical(1L), USE.NAMES = FALSE)) > 1L
    
    if(names(modelOut)[i] %in% config[["hiddenOutputSymbols"]]){
      modelOut[[i]]$hidden <<- TRUE
      return(FALSE)
    }
    if(isTRUE(modelOut[[i]]$hidden))
      return(FALSE)
    else
      return(TRUE)
  }, logical(1L), USE.NAMES = FALSE)
  if(length(config$overwriteSheetOrder$output) && !LAUNCHCONFIGMODE){
    namesModelOutToDisplay <- names(modelOut)[modelOutToDisplay]
    overwriteSheetOrderCleaned <- config$overwriteSheetOrder$output[config$overwriteSheetOrder$output %in% 
                                                                      namesModelOutToDisplay]
    outputSheetIdsToDisplay <- c(match(c(overwriteSheetOrderCleaned,
                                         namesModelOutToDisplay[!namesModelOutToDisplay %in% 
                                                                  overwriteSheetOrderCleaned]),
                                       names(modelOut)))
    rm(overwriteSheetOrderCleaned)
  }else{
    outputSheetIdsToDisplay <- seq_along(modelOut)[modelOutToDisplay]
  }
}
if(is.null(errMsg)){
  # declare output sheets as they will be displayed in UI
  outputTabs <- getTabs(names(modelOut), modelOutAlias, config$outputGroups,
                        idsToDisplay = outputSheetIdsToDisplay)
  outputTabTitles <- outputTabs$tabTitles
  tabSheetMap$output <- outputTabs$tabSheetMap
  outputTabs <- outputTabs$tabs
  isGroupOfSheets <- vapply(seq_len(length(outputTabTitles) + length(scenInputTabTitles)), function(tabId){
    if(tabId > length(outputTabTitles)){
      if(length(scenInputTabTitles[[tabId - length(outputTabTitles)]]) > 1L){
        return(TRUE)
      }
    }else if(length(outputTabTitles[[tabId]]) > 1L){
      return(TRUE)
    }
    return(FALSE)
  }, logical(1L), USE.NAMES = FALSE)
    
  scenDataTemplate <- c(modelOutTemplate, modelInTemplate)
  scenDataTemplate <- scenDataTemplate[!vapply(scenDataTemplate, is.null, logical(1L))]
  
  # get column types for tabular datasets
  for(i in seq_along(modelIn)){
    if(is.null(modelIn[[i]]$headers)){
      next
    }
    modelIn[[i]]$colTypes <- paste(vapply(modelIn[[i]]$headers, function(header){
      if(identical(header$type, "numeric")){
        return("d")
      }
      return("c")
    }, character(1L), USE.NAMES = FALSE), collapse = "")
  }
  for(i in seq_along(modelOut)){
    modelOut[[i]]$colTypes <- paste(vapply(modelOut[[i]]$headers, function(header){
      if(identical(header$type, "numeric")){
        return("d")
      }
      return("c")
    }, character(1L), USE.NAMES = FALSE), collapse = "")
  }
  # validate symbol links
  if(!LAUNCHHCUBEMODE && !LAUNCHCONFIGMODE && length(config[["symbolLinks"]])){
    for(symbolLink in config[["symbolLinks"]]){
      source <- tolower(symbolLink[["source"]])
      target <- tolower(symbolLink[["target"]])
      if(!source %in% names(modelOut)){ 
        errMsg <- paste(errMsg, sprintf("The source symbol: '%s' of a symbol link you specified was not found amongst the output symbols.", 
                                        source))
        next
      }
      if(source %in% c(scalarsOutName, scalarEquationsOutName)){
        errMsg <- paste(errMsg, sprintf("The source symbol: '%s' must not be the sheet of scalars or the sheet of scalar variables/equations.", 
                                        source))
        next
      }
      if(!target %in% names(modelIn)){
        errMsg <- paste(errMsg, sprintf("The target symbol: '%s' of a symbol link you specified was not found amongst the input symbols.", 
                                        target))
        next
      }
      if(target %in% c(scalarsFileName, scalarEquationsName)){
        errMsg <- paste(errMsg, sprintf("The target symbol: '%s' must not be the sheet of scalars or the sheet of scalar variables/equations.", 
                                        target))
        next
      }
      if(!identical(vapply(modelOut[[source]]$headers, "[[", character(1L), "type", USE.NAMES = FALSE),
                    vapply(modelIn[[target]]$headers, "[[", character(1L), "type", USE.NAMES = FALSE))){
        errMsg <- paste(errMsg, sprintf("The symbols: '%s' - '%s' are incompatible and can therefore not be linked together.", 
                                        source, target))
        next
      }
      if(length(modelOut[[source]]$symbolLink)){
        errMsg <- paste(errMsg, sprintf("The symbol: '%s' has multiple symbol links defined. Only one symbol link per output symbol is possible.", 
                                        source))
        next
      }
      modelOut[[source]]$symbolLink <- target
    }
    config$hasSymbolLinks <- TRUE
    config[["symbolLinks"]] <- NULL
  }
  if(length(config$scripts)){
    if(LAUNCHHCUBEMODE){
      if(any(duplicated(vapply(config$scripts$hcube, "[[", character(1L), "id", USE.NAMES = FALSE)))){
        errMsg <- paste(errMsg, "Some of your Hypercube analysis scripts share the same id. Please make sure the ID is unique.")
      }
    }else{
      if(any(duplicated(vapply(config$scripts$base, "[[", character(1L), "id", USE.NAMES = FALSE)))){
        errMsg <- paste(errMsg, "Some of your analysis scripts share the same id. Please make sure the ID is unique.")
      }
    }
  }
}
if(is.null(errMsg)){
  # define table names (format: modelName_scen.prefix_table.name) where "name" is the name of the dataset
  # scenario data is a concatenated list of outputData and inputData
  scenTableNames    <- c(names(modelOut), inputDsNames)
  if(!length(scenTableNames)){
    errMsg <- "You have defined neither input nor output symbols. Please enter at least one external symbol!"
  }
  scenTableNames    <- paste0(gsub("_", "", modelName, fixed = TRUE),
                              "_", scenTableNames)
  # define scenario tables to display in interface
  inputIdsNotToDisplay <- vapply(inputDsNames, function(el){
    if(identical(modelIn[[el]]$dropdown$single, TRUE) || 
       identical(modelIn[[el]]$dropdown$checkbox, TRUE)) 
      return(TRUE) 
    return(FALSE)}, logical(1L), USE.NAMES = FALSE)
  inputDsNamesNotToDisplay <- inputDsNames[inputIdsNotToDisplay]
  scenTableNamesToDisplay <- c(names(modelOut)[modelOutToDisplay], inputDsNames[!inputIdsNotToDisplay])
  groupSheetToTabIdMap <- lapply(seq_len(length(outputTabs) + length(scenInputTabs)), function(groupId){
    if(groupId > length(outputTabs)){
      return(lapply(scenInputTabs[[groupId - length(outputTabs)]], function(sheetId){
        if(identical(sheetId, 0L)){
          tabName <- scalarsFileName
        }else{
          tabName <- names(modelIn)[[sheetId]]
        }
        return(match(tabName, scenTableNamesToDisplay))
      }))
    }
    return(lapply(outputTabs[[groupId]], function(sheetId){
      return(match(names(modelOut)[[sheetId]], scenTableNamesToDisplay))
    }))
  })
  
  configGraphsIn    <- vector(mode = "list", length = length(modelIn))
  configGraphsOut   <- vector(mode = "list", length = length(modelOut))
  
  validateGraphConfig <- function(graphConfig){
    if(!identical(graphConfig$outType, "miroPivot")){
      return(TRUE)
    }
    if(length(graphConfig$options$aggregationFunction) &&
       !identical(graphConfig$options$aggregationFunction, "count") &&
       identical(graphConfig$options[["_metadata_"]]$symtype, "set")){
      return("Sets can only have 'count' as aggregation function.")
    }
    noNumericHeaders <- sum(vapply(graphConfig$options[["_metadata_"]]$headers, 
                                   function(header){
                                     identical(header$type, "numeric")
                                   }, logical(1L), USE.NAMES = FALSE))
    validHeaders <- names(graphConfig$options[["_metadata_"]]$headers)
    if(noNumericHeaders > 1L){
      validHeaders <- c(validHeaders[seq_len(length(validHeaders) - noNumericHeaders)],
                        "Hdr")
    }else{
      validHeaders <- validHeaders[-length(validHeaders)]
    }
    for(id in c("rows", "cols", "aggregations", "filter", "domainFilter")){
      if(length(graphConfig$options[[id]])){
        if(identical(id, "rows")){
          indices <- graphConfig$options[[id]]
        }else if(identical(id, "domainFilter")){
          indices <- graphConfig$options$domainFilter$domains
        }else{
          indices <- names(graphConfig$options$filter)
        }
        invalidIndices <- !indices %in% validHeaders
        if(any(invalidIndices)){
          return(paste0("Invalid ", id, ": ", paste(indices[invalidIndices], 
                                                    collapse = ", ")))
        }
      }
    }
    if(length(graphConfig$options$domainFilter$default) && 
       !graphConfig$options$domainFilter$default %in% graphConfig$options$domainFilter$domains){
      return(paste0("Default domain filter: ", graphConfig$options$domainFilter$default, 
                    " must be among the list of domains."))
    }
    return(TRUE)
  }
  
  invalidGraphsToRender <- character(0L)
  
  # assign default output format to output data that was not set in config
  for(i in seq_along(modelOut)[!names(modelOut) %in% names(config$dataRendering)]){
    if(isTRUE(modelOut[[i]]$hidden)){
      next
    }
    elName <- names(modelOut)[[i]]
    if(identical(elName, scalarsOutName)){
      visibleOutputScalars <- !(modelOut[[i]]$symnames %in% config$hiddenOutputScalars)
      if(sum(visibleOutputScalars) <= maxScalarsValBox && 
         all(modelOut[[i]]$symtypes[visibleOutputScalars] == "parameter")){
        config$dataRendering[[elName]]$outType <- "valuebox"
        config$dataRendering[[elName]]$options$count <- modelOut[[i]]$count - length(config$hiddenOutputScalars)
      }else{
        config$dataRendering[[elName]]$outType <- "datatable"
      }
    }else{
      config$dataRendering[[elName]]$outType <- defOutType
      if(identical(defOutType, "pivot")){
        config$dataRendering[[elName]]$pivottable <- prepopPivot(modelOut[[i]])
      }
    }
  }
  # assign default output format for input sheets that were not set in config
  for(i in seq_along(modelIn)[!names(modelIn) %in% names(config$dataRendering)]){
    elName <- names(modelIn)[[i]]
    if(identical(modelIn[[i]]$type, "custom")){
      # make sure custom inputs have graph button activated (table is displayed there)
      config$dataRendering[[elName]] <- list(outType = "datatable", 
                                             rendererName = modelIn[[i]]$rendererName,
                                             packages = customPackages[[i]])
    }else if(config$autoGenInputGraphs){
      # Create graphs only for tabular input sheets 
      if(!is.null(modelIn[[i]]$headers)){
        if(identical(names(modelIn)[[i]], scalarsFileName)){
          config$dataRendering[[elName]]$outType <- "datatable"
        }else{
          config$dataRendering[[elName]]$outType <- defInType
          if(identical(defInType, "pivot")){
            config$dataRendering[[elName]]$pivottable <- prepopPivot(modelIn[[i]])
          }
        }
      }
    }
  }
  
  for(el in names(config$dataRendering)){
    i <- match(tolower(el), names(modelIn))[[1]]
    isOutputGraph <- FALSE
    # data rendering object was found in list of model input sheets
    if(is.na(i)){
      i <- match(tolower(el), names(modelOut))[[1]]
      # data rendering object was found in list of model output sheets
      if(!is.na(i)){
        if(isTRUE(modelOut[[i]]$hidden)){
          warningMsgTmp <- sprintf("You specified chart options for the output symbol: %s. These options will be ignored as the symbol is hidden.",
                                   names(modelOut)[i])
          warning(warningMsgTmp)
          warningMsg <- paste(warningMsg, warningMsgTmp, sep = "\n")
          if(LAUNCHCONFIGMODE){
            invalidGraphsToRender <- c(invalidGraphsToRender, el)
          }
          next
        }else{
          configGraphsOut[[i]] <- config$dataRendering[[el]]
          configGraphsOut[[i]]$options <- c(configGraphsOut[[i]]$options, 
                                            list("_metadata_" = list(
                                              symname = names(modelOut)[i],
                                              headers = modelOut[[i]]$headers,
                                              symtype = modelOut[[i]]$symtype)))
          if(identical(configGraphsOut[[i]]$outType, "miroPivot")){
            validGraphConfig <- validateGraphConfig(configGraphsOut[[i]])
            if(!identical(validGraphConfig, TRUE)){
              errMsg <- paste(errMsg, paste0("Invalid graph config for symbol '", names(modelOut)[i], 
                                             "': ", validGraphConfig), sep = "\n")
              next
            }
          }else if(identical(configGraphsOut[[i]]$outType, "valueBox")){
            if(identical(names(modelOut)[[i]], scalarsOutName)){
              configGraphsOut[[i]]$options$count <- modelOut[[i]]$count - length(config$hiddenOutputScalars)
            }else{
              errMsg <<- paste(errMsg, 
                               sprintf("Output type: 'valueBox' is only valid for scalar tables. Please choose another output type for your dataset : '%s'.", 
                                       modelOutAlias[i]), sep = "\n")
            }
          }
        }
      }else if(LAUNCHCONFIGMODE){
        invalidGraphsToRender <- c(invalidGraphsToRender, el)
        next
      }else{
        errMsgTmp <- paste0("'", el, "' was defined to be an object to render, but was not found in either the list of model input or the list of model output sheets.")
        errMsg <- paste(errMsg, errMsgTmp, sep = "\n")
        next
      }
      if(length(configGraphsOut[[i]]$additionalData)){
        badAdditionalDataSheetNames <- !configGraphsOut[[i]]$additionalData %in% c(names(modelOut), 
                                                                                   inputDsNames)
        if(any(badAdditionalDataSheetNames)){
          warningMsgTmp <- paste0("The symbols: ", 
                                  paste0(configGraphsOut[[i]]$additionalData[badAdditionalDataSheetNames],
                                         collapse = ", "),
                                  " are configured to be additional data for the renderer of symbol: ",
                                  names(modelOut)[i], 
                                  ". These symbols could not be found among the list of either input or output symbols!")
          warning(warningMsgTmp)
          warningMsg <- paste(warningMsg, warningMsgTmp, sep = "\n")
        }
        configGraphsOut[[i]]$additionalData <- configGraphsOut[[i]]$additionalData[!badAdditionalDataSheetNames]
      }
      isOutputGraph <- TRUE
    }else{
      configGraphsIn[[i]] <- config$dataRendering[[el]]
      configGraphsIn[[i]]$options <- c(configGraphsIn[[i]]$options, 
                                       list("_metadata_" = list(
                                         symname = names(modelIn)[i],
                                         headers = modelIn[[i]]$headers,
                                         symtype = modelIn[[i]]$symtype)))
      
      if(identical(configGraphsIn[[i]]$outType, "miroPivot")){
        validGraphConfig <- validateGraphConfig(configGraphsIn[[i]])
        if(!identical(validGraphConfig, TRUE)){
          errMsg <- paste(errMsg, paste0("Invalid graph config for symbol '", names(modelIn)[i], 
                                         "': ", validGraphConfig), sep = "\n")
          next
        }
      }else if(identical(configGraphsIn[[i]]$outType, "valueBox")){
        if(identical(names(modelIn)[[i]], scalarsFileName)){
          configGraphsIn[[i]]$options$count <- modelIn[[i]]$count
        }else{
          errMsg <<- paste(errMsg, 
                           sprintf("Output type: 'valueBox' is only valid for scalar tables. Please choose another output type for your dataset : '%s'.", 
                                   modelInAlias[i]), sep = "\n")
        }
      }
    }
    if(length(config$dataRendering[[el]]$graph$filter)){
      if(isOutputGraph){
        categoricalHeaders <- modelOut[[i]]$headers
      }else{
        categoricalHeaders <- modelIn[[i]]$headers
      }
      categoricalHeaders <- unlist(lapply(seq_along(categoricalHeaders), function(hdrId){
        if(identical(categoricalHeaders[[hdrId]]$type, "string"))
          return(names(categoricalHeaders)[[hdrId]])
        return(NULL)
      }), use.names = FALSE)
      if(!length(config$dataRendering[[el]]$graph$filter$col) || 
         !config$dataRendering[[el]]$graph$filter$col %in% categoricalHeaders){
        errMsg <- paste(errMsg, sprintf("The column: '%s' was defined as a dynamic filter for the chart of element: '%s'. This column could not be found among the categorical columns of this symbol: '%s'.",
                                        config$dataRendering[[el]]$graph$filter$col, el, paste(categoricalHeaders, collapse = "', '")), setp = "\n")
      }
    }
  }
  for(i in seq_along(modelOut)){
    el <- names(modelOut)[i]
    if(el %in% tolower(names(config$outputTables))){
      configGraphsOut[[i]]$datatable <- modifyList(config$datatable, config$outputTables[[tolower(el)]])
    }else{
      configGraphsOut[[i]]$datatable <- config$datatable
    }
  }
  invalidSymbolConfig <- match(tolower(names(config$outputTables)), names(modelOut))
  invalidSymbolConfig <- invalidSymbolConfig[is.na(invalidSymbolConfig)]
  if(length(invalidSymbolConfig)){
    warning(sprintf("You specified an output table for the symbol(s): '%s'. This/these symbol(s) is/are not part of the data contract between GAMS and MIRO", 
                    paste(invalidSymbolConfig, collapse = "','")))
  }
  
  #sanitize file names of output attachments
  config$outputAttachments <- lapply(config$outputAttachments, function(el){
    el$filename <- sanitizeFn(el$filename)
    return(el)
  })
  
  installPackage    <- list()
  installPackage$plotly <- LAUNCHCONFIGMODE || any(vapply(c(configGraphsIn, configGraphsOut), 
                                                         function(conf){if(identical(conf$graph$tool, "plotly")) TRUE else FALSE}, 
                                                         logical(1L), USE.NAMES = FALSE))
  installPackage$dygraphs <- LAUNCHCONFIGMODE || any(vapply(c(configGraphsIn, configGraphsOut), 
                                                           function(conf){if(identical(conf$graph$tool, "dygraphs")) TRUE else FALSE}, 
                                                           logical(1L), USE.NAMES = FALSE))
  installPackage$leaflet <- LAUNCHCONFIGMODE || any(vapply(c(configGraphsIn, configGraphsOut), 
                                                           function(conf){if(identical(conf$graph$tool, "leaflet")) TRUE else FALSE}, 
                                                           logical(1L), USE.NAMES = FALSE))
  installPackage$timevis <- LAUNCHCONFIGMODE || any(vapply(c(configGraphsIn, configGraphsOut), 
                                                          function(conf){if(identical(conf$graph$tool, "timevis")) TRUE else FALSE}, 
                                                          logical(1L), USE.NAMES = FALSE))
  
  dbSchema <- list(tabName = c('_scenMeta' = scenMetadataTablePrefix %+% modelName, 
                               '_scenLock' = scenLockTablePrefix %+% modelName,
                               '_scenTrc' = tableNameTracePrefix %+% modelName,
                               '_scenAttach' = tableNameAttachPrefix %+% modelName,
                               '_scenScripts' = tableNameScriptsPrefix %+% modelName,
                               '_jobMeta' = tableNameJobPrefix %+% modelName),
                   colNames = list('_scenMeta' = c(sid = sidIdentifier, uid = uidIdentifier, sname = snameIdentifier,
                                                   stime = stimeIdentifier, stag = stagIdentifier, accessR = accessIdentifier %+% "r",
                                                   accessW = accessIdentifier %+% "w", accessX = accessIdentifier %+% "x", 
                                                   scode = scodeIdentifier),
                                   '_scenLock' = c(uid = uidIdentifier, sid = sidIdentifier, lock = slocktimeIdentifier),
                                   '_scenTrc' = traceColNames,
                                   '_scenAttach' = c(sid = sidIdentifier, fn = "fileName",
                                                     fExt = "fileExt", 
                                                     execPerm = "execPerm",
                                                     content = "fileContent",
                                                     time = "timestamp"),
                                   '_scenScripts' = c(sid = sidIdentifier, id = "id",
                                                     content = "scriptContent"),
                                   '_jobMeta' = c(jid = '_jid', uid = uidIdentifier,  
                                                  status = '_status', time = '_jtime', 
                                                  tag = stagIdentifier, pid = '_pid', 
                                                  sid = sidIdentifier, gamsret = '_gamsret',
                                                  scode = scodeIdentifier, sname = snameIdentifier)),
                   colTypes = c('_scenMeta' = "iccTcccci",
                                '_scenLock' = "ciT", '_scenTrc' = "cccccdidddddiiiddddddc",
                                '_scenAttach' = "icclbT", '_scenScripts' = "icc", '_jobMeta' = "iciTcciiic"))
  
  dbSchema$tabName  <- c(dbSchema$tabName, scenTableNames)
  scenColNamesTmp   <- lapply(c(modelOut, modelIn), function(el) return(names(el$headers)))
  scenColTypesTmp   <- lapply(c(modelOut, modelIn), "[[", "colTypes")
  dsIsNoTable       <- vapply(scenColNamesTmp, is.null, logical(1L), USE.NAMES = FALSE)
  if(any(dsIsNoTable)){
    scenColNamesTmp[dsIsNoTable]       <- NULL
    scenColTypesTmp[dsIsNoTable]       <- NULL
    if(!scalarsFileName %in% names(modelIn)){
      scenColNamesTmp[[scalarsFileName]] <- scalarsFileHeaders
      scenColTypesTmp[scalarsFileName]   <- "ccc"
    }
  }
  dbSchema$colNames  <- c(dbSchema$colNames, scenColNamesTmp)
  dbSchema$colTypes  <- c(dbSchema$colTypes, unlist(scenColTypesTmp))
  rm(dsIsNoTable, scenColNamesTmp, scenColTypesTmp)
  
  # generate GAMS return code map
  GAMSReturnCodeMap <- c(
    '-500' = "Internal error",
    '-404' = "Host could not be reached",
    '-401' = "Access denied",
    '-400' = "License expired",
    '-100' = "Model execution timed out",
    '-15' = "Model execution was interrupted",
    '-9' = "Model execution was interrupted",
    '1' = "Solver is to be called, the system should never return this number", 
    '2' = "There was a compilation error", 
    '3' = "There was an execution error", 
    '4' = "System limits were reached",
    '5' = "There was a file error",
    '6' = "There was a parameter error",
    '7' = "There was a licensing error",
    '8' = "There was a GAMS system error",
    '9' = "GAMS could not be started",
    '10' = "Out of memory",
    '11' = "Out of disk",
    '109' = "Could not create process/scratch directory",
    '110' = "Too many process/scratch directories",
    '112' = "Could not delete the process/scratch directory",
    '113' = "Could not write the script gamsnext",
    '114' = "Could not write the parameter file",
    '115' = "Could not read environment variable",
    '144' = "Could not spawn the GAMS language compiler (gamscmex)",
    '400' = "Could not spawn the GAMS language compiler (gamscmex)",
    '145' = "Current directory (curdir) does not exist",
    '401' = "Current directory (curdir) does not exist",
    '146' = "Cannot set current directory (curdir)",
    '402' = "Cannot set current directory (curdir)",
    '148' = "Blank in system directory (UNIX only)",
    '404' = "Blank in system directory (UNIX only)",
    '149' = "Blank in current directory (UNIX only)",
    '405' = "Blank in current directory (UNIX only)",
    '150' = "Blank in scratch extension (scrext)",
    '406' = "Blank in scratch extension (scrext)",
    '151' = "Unexpected cmexRC",
    '407' = "Unexpected cmexRC",
    '152' = "Could not find the process directory (procdir)",
    '408' = "Could not find the process directory (procdir)",
    '153' = "CMEX library could not be found (experimental)",
    '409' = "CMEX library could not be found (experimental)",
    '154' = "Entry point in CMEX library could not be found (experimental)",
    '410' = "Entry point in CMEX library could not be found (experimental)",
    '155' = "Blank in process directory (UNIX only)",
    '411' = "Blank in process directory (UNIX only)",
    '156' = "Blank in scratch directory (UNIX only)",
    '412' = "Blank in scratch directory (UNIX only)",
    '141' = "Cannot add path / unknown UNIX environment / cannot set environment variable",
    '232' = "Driver error: incorrect command line parameters for gams",
    '1000' = "Driver error: incorrect command line parameters for gams",
    '208' = "Cannot add path / unknown UNIX environment / cannot set environment variable",
    '2000' = "Cannot add path / unknown UNIX environment / cannot set environment variable",
    '184' = "Driver error: problems getting current directory",
    '3000' = "Driver error: problems getting current directory",
    '160' = "Driver error: internal error: GAMS compile and execute module not found",
    '4000' = "Driver error: internal error: GAMS compile and execute module not found",
    '126' = "Driver error: internal error: cannot load option handling library",
    '5000' = "Driver error: internal error: cannot load option handling library"
  )
}
if(is.null(errMsg)){
  # parse README.md file 
  if(length(config$readme$filename)){
    readmeFilePath   <- file.path(currentModelDir, 
                                  config$readme$filename)
    tryCatch({
      if(endsWith(tolower(readmeFilePath), 'html')){
        config$readmeFile <- read_file(readmeFilePath)
      }else if(file.exists(readmeFilePath)){
        source(file.path("components", "md_parser.R"), local = TRUE)
        markdownParser <- MarkdownParser$new(!LAUNCHCONFIGMODE && 
                                               isTRUE(config$readme$enableMath))
        config$readmeFile <- markdownParser$parseFile(readmeFilePath)
      }
    }, error = function(e){
      errMsg <<- sprintf("Problems parsing README markdown file. Error message: %s",
                         conditionMessage(e))
    })
  }
  dropdownAliases <- lapply(modelIn, function(el){
    if(identical(el$type, "dropdown") && 
       length(el$dropdown$aliases) && 
       !isTRUE(el$dropdown$multiple)){
      return(list(aliases = el$dropdown$aliases,
                  choices = el$dropdown$choices,
                  clearValue = isTRUE(el$dropdown$clearValue)))
    }
    return(NULL)
  })
  dropdownAliases <- dropdownAliases[!vapply(dropdownAliases, is.null, 
                                             logical(1L), USE.NAMES = FALSE)]
}

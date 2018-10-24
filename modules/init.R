# check whether there exists a config file and if not create an empty one
if(is.null(errMsg)){
  if(!file.exists(currentModelDir %+% configDir %+% "config.json")){
    tryCatch(cat("{}\n", file = currentModelDir %+% configDir %+% "config.json"),
             error = function(e){
               errMsg <<- "A configuration file was not found and no data could be written to the location of the config folder. Please check read/write permissions in folder: " %+% currentModelDir %+% configDir
             })
  }
}

if(is.null(errMsg)){ 
  # fetch JSON schema files
  jsonSchemaMap <- c(getJsonFileSchemaPairs(configDir), 
                     getJsonFileSchemaPairs(fileDir = paste0(currentModelDir, configDir), 
                                                schemaDir = configDir))
  
  lapply(jsonFilesWithSchema, function(file){
    if(is.null(jsonSchemaMap[[file]])){
      errMsg <<- paste(errMsg, "Schema and/or JSON file: '" %+% file %+% 
                         "' is required but missing. Please make sure a valid schema file is available.", 
                       sep = "\n")
    }
  })
}

# validate json files
config <- NULL
jsonErrors <- NULL
if(is.null(errMsg)){
  config <- NULL
  lapply(seq_along(jsonSchemaMap), function(i){
    error <- tryCatch({
      eval <- validateJson(jsonSchemaMap[[i]][1], jsonSchemaMap[[i]][2])
    }, error = function(e){
      errMsg <<- paste(errMsg, "Some error occurred validating JSON file: '" %+% 
                         names(jsonSchemaMap)[[i]] %+% "'. Error message: " %+% e, sep = "\n")
      e
    })
    if(inherits(error, "error")){
      next
    }
    
    if(names(jsonSchemaMap)[[i]] == "config" && is.null(eval[[2]])){
      config <<- c(config, eval[[1]])
    }else if (names(jsonSchemaMap)[[i]] == "GMSIO_config" && is.null(eval[[2]])){
      config <<- c(config, eval[[1]])
    }else if (names(jsonSchemaMap)[[i]] == "db_config" && is.null(eval[[2]])){
      config$db <<- c(config, eval[[1]])
    }else if(!is.null(eval[[2]])){
      errMsgTmp <- "Some error occurred parsing JSON file: '" %+% names(jsonSchemaMap)[[i]] %+% 
        "'. See below for more detailed information."
      errMsg <<- paste(errMsg, errMsgTmp, sep = "\n")
      jsonErrors <<- rbind(jsonErrors, cbind(file_name = paste0(names(jsonSchemaMap)[[i]], ".json"), eval[[2]]))
    }
  })
  if(identical(config$activateModules$sharedScenarios, TRUE) && identical(config$activateModules$scenario, FALSE)){
    flog.info("Can not use module 'share scenarios' without having module 'scenario' activated. 'Share scenarios' module was deactivated.")
    config$activateModules$sharedScenarios <- FALSE
  }
  if(!length(config$db$username)){
    pg_user <- Sys.getenv("GMS_PG_USERNAME", unset = NA)
    if(is.na(pg_user)){
      errMsg <<- paste(errMsg, "The PostgresQL username could not be identified. Please make sure you specify a valid username.",
                       sep = "\n")
    }else{
      config$db$username <- pg_user
    }
  }
  if(!length(config$db$password)){
    pg_pass <- Sys.getenv("GMS_PG_PASSWORD", unset = NA)
    if(is.na(pg_pass)){
      errMsg <<- paste(errMsg, "The PostgresQL password could not be identified. Please make sure you specify a valid password.",
                       sep = "\n")
    }else{
      config$db$password <- pg_pass
    }
  }
}

lang <- NULL
if(is.null(errMsg)){
  # read JSON language file
  if(!file.exists(paste0('./conf/', config$language, ".json"))){
    errMsg <- "The JSON language file: '" %+% config$language %+% ".json' could not be located. Please make sure file is available and accessible."
    flog.fatal(errMsg)
  }else{
    eval <- list(character(), character())
    tryCatch({
      eval <- validateJson(configDir %+% config$language %+% ".json", configDir %+% languageSchemaName, addDefaults = F)
    }, error = function(e){
      errMsg <<- "Some error occurred validating language file: '" %+% config$language %+% ".json'. Error message: " %+% e
      flog.fatal(errMsg)
    })
    if(is.null(eval[[2]])){
      lang <- eval[[1]]
    }else{
      errMsg <- paste(errMsg, "Some error occurred parsing JSON language file: '" %+% config$language %+% ".json'. See below for more detailed information.", sep = "\n")
      flog.fatal(errMsg)
      jsonErrors <- rbind(jsonErrors, cbind(file_name = paste0(config$language, ".json"), eval[[2]]))
    }
  }
}

# load model input and output parameters
if(is.null(errMsg)){
  flog.trace("Language files loaded.")
  # handsontable options
  hotOptions        <- config$handsontable
  
  modelOut          <- config$gamsOutputFiles
  names(modelOut)   <- tolower(names(modelOut))
  # declare set of output sheets that should be displayed in webUI
  modelOutToDisplay <- lapply(seq_along(modelOut), function(i){
    if(identical(modelOut[[i]]$hidden, TRUE)) 
      NA 
    else 
      names(modelOut)[[i]]})
  modelOutToDisplay   <- unlist(modelOutToDisplay[!is.na(modelOutToDisplay)], use.names = FALSE)
  
  modelIn             <- config$gamsInputFiles
  names(modelIn)      <- tolower(names(modelIn))
  
  # make sure two input or output data sheets dont share the same name (case insensitive)
  if(any(duplicated(names(modelIn)))){
    errMsg <- "Two or more input datasets share the same name. Please make sure the identifiers are unique for each input datasheet!"
    flog.fatal(errMsg)
  }
  if(any(duplicated(names(modelOut)))){
    errMsg <- "Two or more output datasets share the same name. Please make sure the identifiers are unique for each output datasheet!"
    flog.fatal(errMsg)
  }
}

if(is.null(errMsg)){
  # declare GAMS compile time variables and GAMS options
  tmpDDPar            <- getGMSPar(names(modelIn), prefixDDPar)
  names(modelIn)      <- tmpDDPar[[1]]
  DDPar               <- tmpDDPar[[2]]
  rm(tmpDDPar)
  tmpGMSOpt           <- getGMSPar(names(modelIn), prefixGMSOpt)
  names(modelIn)      <- tmpGMSOpt[[1]]
  GMSOpt              <- tmpGMSOpt[[2]]
  rm(tmpGMSOpt)
  
  modelInToImport     <- getInputToImport(modelIn, keywordsNoImport)
  modelInMustImport   <- getInputToImport(modelIn, keywordsNoMustImport)
  # declare input and output aliases
  modelInAlias        <- lapply(seq_along(modelIn), function(i){
    if(is.null(modelIn[[i]]$alias)){
      names(modelIn)[[i]]
    }else{
      modelIn[[i]]$alias
    }
  })
  modelInToImportAlias <- lapply(seq_along(modelInToImport), function(i){
    if(is.null(modelInToImport[[i]]$alias)){
      names(modelInToImport)[[i]]
    }else{
      modelInToImport[[i]]$alias
    }
  })
  modelOutAlias <- lapply(seq_along(modelOut), function(i){
    if(is.null(modelOut[[i]]$alias)){
      names(modelOut)[[i]]
    }else{
      modelOut[[i]]$alias
    }
  })
  # add input type to list
  lapply(seq_along(modelIn), function(i){
    tryCatch({
      modelIn[[i]]$type <<- getInputType(modelIn[[i]], keywordsType = keywordsType)
    }, error = function(e){
      flog.fatal(errMsgTmp)
      errMsg <<- paste(errMsg, paste0(modelInAlias[i], " has no valid input type defined. Error message: ", e), sep = "\n")
    })
  })
  
  # declare input sheets as they will be displayed in UI
  if(!length(config$aggregateWidgets$title)){
    # every input element on its own tab
    inputTabs  <- seq_along(modelIn)
    inputTabTitles <- modelInAlias
  }else{
    # aggregate input widgets on a single tab
    widgetIds <- lapply(seq_along(modelIn), function(i){
      if(identical(modelIn[[i]]$type, "hot")){
        return(NULL)
      }else{
        return(i)
      }
    })
    widgetIds    <- unlist(widgetIds[!vapply(widgetIds, is.null, numeric(1L))])
    # id of tab with aggregated widgets
    widgetId     <- NULL
    j            <- 1L
    inputTabs <- vector("list", length(modelIn))
    inputTabTitles<- vector("list", length(modelIn))
    
    lapply(seq_along(modelIn), function(i){
      if(identical(modelIn[[i]]$type, "hot")){
        inputTabs[[i]] <<- i
        inputTabTitles[[i]]<<- modelInAlias[[i]]
      }else{
        if(is.null(widgetId)){
          widgetId         <<- i
          inputTabs[[i]] <<- widgetIds
          inputTabTitles[[i]]<<- config$aggregateWidgets$title
        }
      }
    })
    inputTabs   <- inputTabs[!vapply(inputTabs, is.null, numeric(1L))]
    inputTabTitles  <- inputTabTitles[!vapply(inputTabTitles, is.null, numeric(1L))]
    if(!is.null(widgetId) && identical(length(inputTabs[[widgetId]]), 1L)){
      # if there is only a single widget in widget tab use alias of this widget
      inputTabTitles[[widgetId]] <- modelInAlias[[widgetId]]
    }
  }
  
  # read graph data for input and output sheets
  strictMode        <- config$activateModules$strictmode
  configGraphsIn    <- vector(mode = "list", length = length(modelIn))
  configGraphsOut   <- vector(mode = "list", length = length(modelOut))
  
  for(el in names(config$dataRendering)){
    i <- match(tolower(el), names(modelIn))[[1]]
    # data rendering object was found in list of model input sheets
    if(!is.na(i)){
      configGraphsIn[[i]] <- config$dataRendering[[el]]
    }else{
      i <- match(tolower(el), names(modelOut))[[1]]
      # data rendering object was found in list of model output sheets
      if(!is.na(i)){
        configGraphsOut[[i]] <- config$dataRendering[[el]]
      }else if(strictMode){
        errMsgTmp <- paste0("'", el, "' was defined to be an object to render, but was not found in either the list of model input or the list of model output sheets.")
        flog.fatal(errMsgTmp)
        errMsg <- paste(errMsg, errMsgTmp, sep = "\n")
      }
    }
  }
  
  # assign default output format to output data that was not set in config
  lapply(seq_along(modelOut), function(i){
    if(identical(tolower(configGraphsOut[[i]]$outType), "valuebox")){
      if(identical(names(modelOut)[[i]], scalarsOutName)){
        configGraphsOut[[i]]$options$count <<- modelOut[[i]]$count
      }else{
        errMsg <<- paste(errMsg, 
                         sprintf("Output type: 'valueBox' is only valid for scalar tables. Please choose another output type for your dataset : '%s'.", 
                                 modelOutAlias[i]), sep = "\n")
      }
    }
    if(is.null(configGraphsOut[[i]])){
      if(identical(names(modelOut)[[i]], scalarsOutName) && modelOut[[i]]$count < 10){
        configGraphsOut[[i]]$outType <<- "valuebox"
        configGraphsOut[[i]]$options$count <<- modelOut[[i]]$count
      }else{
        configGraphsOut[[i]]$outType <<- defOutType
      }
    }
  })
  # assign default output format for input sheets that were not set in config
  lapply(seq_along(modelIn), function(i){
    if(identical(tolower(configGraphsIn[[i]]$outType), "valuebox")){
      if(identical(names(modelIn)[[i]], scalarsFileName)){
        configGraphsIn[[i]]$options$count <<- modelIn[[i]]$count
      }else{
        errMsg <<- paste(errMsg, 
                         sprintf("Output type: 'valueBox' is only valid for scalar tables. Please choose another output type for your dataset : '%s'.", 
                                 modelInAlias[i]), sep = "\n")
      }
    }
    if(config$autoGenInputGraphs){
      # Create graphs only for tabular input sheets 
      if(!is.null(modelIn[[i]]$headers)){
        if(is.null(configGraphsIn[[i]])){
          configGraphsIn[[i]]$outType <<- defInType
        }
      }
    }
  })
  # batch mode configuration
  modelInGmsString <- NULL
  if(identical(config$activateModules$batchMode, TRUE)){
    modelInGmsString <- unlist(lapply(seq_along(modelIn), function(i){
      if((modelIn[[i]]$type == "slider" 
          && length(modelIn[[i]]$slider$default) > 1) 
         || (modelIn[[i]]$type == "daterange")){
        ""
      }else{
        if(names(modelIn)[i] %in% DDPar){
          return("--" %+% names(modelIn)[i] %+% "=")
        }
        names(modelIn)[i] %+% "="
      }
    }), use.names = FALSE)
    lapply(seq_along(modelIn), function(i){
      if(!identical(modelIn[[i]]$noBatch, TRUE)){
        switch(modelIn[[i]]$type,
               checkbox = {
                 modelIn[[i]]$type <<- "dropdown"
                 modelIn[[i]]$dropdown$label <<- modelIn[[i]]$checkbox$label
                 value <- modelIn[[i]]$checkbox$value
                 if(!is.null(value) && !is.na(suppressWarnings(as.integer(value)))){
                   modelIn[[i]]$dropdown$selected <<- modelIn[[i]]$checkbox$value
                   modelIn[[i]]$dropdown$aliases <<- lang$nav$batchMode$checkboxAliases
                   modelIn[[i]]$dropdown$choices <<- c(0L, 1L)
                 }else{
                   modelIn[[i]]$dropdown$fixedAliases <<- lang$nav$batchMode$checkboxAliases
                   modelIn[[i]]$dropdown$choices <<- modelIn[[i]]$checkbox$value
                 }
                 modelIn[[i]]$dropdown$selected <<- modelIn[[i]]$checkbox$value
                 modelIn[[i]]$dropdown$width <<- modelIn[[i]]$checkbox$width
                 modelIn[[i]]$dropdown$multiple <<- TRUE
                 modelIn[[i]]$dropdown$checkbox <<- TRUE
                 modelIn[[i]]$checkbox <<- NULL
               },
               dropdown = {
                 if(identical(modelIn[[i]]$dropdown$multiple, TRUE)){
                   errMsg <<- paste(errMsg, 
                                    sprintf("Multi dropdown menus are currently " %+% 
                                              "not supported in batch mode (Element: '%s').", 
                                            modelInAlias[i]), sep = "\n")
                 }else{
                   # specify that dropdown menu is originally a single select menu
                   modelIn[[i]]$dropdown$single   <<- TRUE
                   modelIn[[i]]$dropdown$multiple <<- TRUE
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
               hot = {},
               {
                 errMsg <<- paste(errMsg, 
                                  sprintf("Elements other than sliders, checkboxes and single " %+% 
                                            "dropdown menus are currently not supported (Element: '%s').", 
                                          modelInAlias[i]), sep = "\n")
               })
      }
    })
  }
  
  # get datasets whose data source is shared with other models
  sharedData       <- vector("logical", length(modelIn))
  # list of column names used to subset shared dataframe
  colSubset <- vector("list", length(modelIn))
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
               choices <- getDependenciesDropdown(choices = modelIn[[i]]$dropdown$choices, modelIn = modelIn, name = name, strictMode = strictMode)
               if(!is.null(modelIn[[i]]$dropdown$aliases)){
                 aliases <- getDependenciesDropdown(choices = modelIn[[i]]$dropdown$aliases, modelIn = modelIn, name = name, strictMode = strictMode)
               }else{
                 aliases <- NULL
               }
             }, error = function(e){
               errMsg <<- paste(errMsg, paste0("'", modelInAlias[i], 
                                               "' has no valid input type defined. Error message: ", e), sep = "\n")
             })
             if(!is.null(errMsg)){
               return(NULL)
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
               # remove identifier string that specifies where shared data comes from rom dropdown
               ddownDep[[name]]$shared <<- choices$shared
               
               if(!identical(name, choices$shared)){
                 # only subset of columns will be imported
                 colSubset[[i]]   <<- c(tolower(ddownDep[[name]]$shared), tolower(ddownDep[[name]]$aliases))
               }
               
               # add to list of widgets with shared datasource
               sharedData[i] <<- TRUE
             }
             # in case dropdown menu has aliases, validate that they are of matching length as choices
             if(!is.null(aliases)){
               if(length(aliases$strings) != length(choices$strings)){
                 errMsg <<- paste(errMsg,paste0("The number of fixed aliases for dropdown menu: ", modelInAlias[i], 
" does not match the number of choices without dependencies. 
                                                Aliases: '", paste(aliases$strings, collapse = ","), 
". Choices: '", paste(choices$strings, collapse = ","), "'."), sep = "\n")
                 return(NULL)
               }else if(length(aliases$fw) != length(choices$fw)){
                 errMsg <<- paste(errMsg,paste0("The number of aliases with dependencies for dropdown menu: ", 
modelInAlias[i], " does not match the number of choices with dependencies. 
                                                Aliases: '", paste(aliases$fw, collapse = ","), 
". Choices: '", paste(choices$fw, collapse = ","), "'."), sep = "\n")
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
                 if(identical(modelIn[[i]]$dropdown$multiple, TRUE)
                    && !identical(modelIn[[i]]$dropdown$single, TRUE)
                    && !identical(modelIn[[i]]$dropdown$checkbox, TRUE)){
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
             hot = ,
             dt ={
               # check that in case dataset is scalar ds, it has correct headers
               if(names(modelIn)[[i]] %in% c(scalarsFileName, scalarsOutName) && 
                  !identical(names(modelIn[[i]]$headers), scalarsFileHeaders)){
                 warning(paste0(modelInAlias[i], " is defined to be the scalar input dataset, " %+%
"but has incorrect headers. The headers were adjusted accordingly."))
                 names(modelIn[[i]]$headers) <- scalarsFileHeaders
               }
               if(identical(modelIn[[i]]$sharedData, TRUE)){
                 sharedData[i] <<- TRUE
               }
               return(name)
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
             checkbox = {
               if(names(modelIn)[[i]] %in% c(scalarsFileName, scalarsOutName)){
                 errMsg <<- paste(errMsg, paste0("The checkbox: '", modelInAlias[i], 
                                                 "' uses a reserved name as its identifier. Please choose a different name."), sep = "\n")
               }
               if(is.character(modelIn[[i]]$checkbox$value)){
                 # checkbox has dependency
                 
                 # BEGIN error checks
                 if(grepl("\\$+$", modelIn[[i]]$checkbox$value)){
                   errMsg <<- paste(errMsg,paste0("The checkbox: '", modelInAlias[i], 
                                                  "' has a backward dependency assigned. Currently only forward dependencies are supported for checkboxes."), sep = "\n")
                 }
                 # END error checks
                 
                 # remove trailing or leading dollar signs
                 cbValue <- gsub("(\\$+$|^\\$+)", "", modelIn[[i]]$checkbox$value)
                 cbValue <- strsplit(cbValue, "\\$")[[1]]
                 idx1    <- match(cbValue[1], names(modelIn))[1]
                 if(!is.na(idx1)){
                   # add forward dependency
                   modelIn[[i]]$checkbox$sheetId <<- idx1
                   modelIn[[i]]$checkbox$value   <<- cbValue[2]
                   modelInWithDep[[name]]      <<- modelIn[[i]]
                 }else{
                   errMsg <<- paste(errMsg,paste0("The dependent dataset for checkbox: '", 
                                                  modelInAlias[i], "' could not be found. Please make sure you define a valid reference."), sep = "\n")
                 }
               }
               return(NULL)
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
  # get input dataset names (as they will be saved in database or Excel)
  # get worksheet names
  if(tolower(scalarsFileName) %in% modelInTabularData || length(modelIn) == length(modelInTabularData)){
    inputDsNames <- modelInTabularData
  }else{
    inputDsNames <- c(modelInTabularData, scalarsFileName)
  }
  }

  if(is.null(errMsg)){
    # determine the filenames for the model input datasets
    if(scalarsFileName %in% modelInTabularData){
      # scalars should always be the highest indexed dataset
      modelInFileNames <- c(modelInTabularData[modelInTabularData != scalarsFileName], scalarsFileName)
    }else{
      if(length(modelIn) > length(modelInTabularData)){
        modelInFileNames <- c(modelInTabularData, scalarsFileName)
      }else{
        modelInFileNames <- modelInTabularData
      }
    }
    
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
              errMsg <<- paste(errMsg,paste0(col, " of input sheet ", sheet, 
                                             "has more than one dependency. Only one backward dependency per column is allowed.", e), 
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
          if(modelIn[[i]]$headers[[j]]$type == "set"){
            return(names(modelIn[[i]]$headers)[[j]])
          }
        }), use.names = FALSE)
      }
    }
    
    # initialize data frames for model input data
    modelInTemplate <- vector(mode = "list", length = length(modelIn))
    lapply(modelInTabularData, function(el){
      i <- match(el, names(modelIn))
      if(!is.null(modelIn[[i]]$headers)){
        headers   <- vector(mode = "numeric", length = length(modelIn[[i]]$headers))
        headers   <- lapply(modelIn[[i]]$headers, function(header){
          switch(header$type,
                 "set" = character(),
                 "parameter" = numeric(),
                 "acronym" = character(),
                 "string" = character(),
                 "scalar" = character())
        })
        names(headers) <- names(modelIn[[i]]$headers)
        modelInTemplate[[i]] <<- tibble::tibble(!!!headers)
      }
    })
    
    modelOutTemplate <- vector(mode = "list", length = length(modelOut))
    lapply(modelOutToDisplay, function(outputSheet){
      outputSheetIdx <- match(outputSheet, names(modelOut))
      if(!is.null(modelOut[[outputSheetIdx]]$headers)){
        headers   <- vector(mode = "numeric", length = length(modelOut[[outputSheetIdx]]$headers))
        headers   <- lapply(modelOut[[outputSheetIdx]]$headers, function(header){
          switch(header$type,
                 "set" = character(),
                 "parameter" = numeric(),
                 "acronym" = character(),
                 "string" = character(),
                 "scalar" = character())
        })
        names(headers) <- names(modelOut[[outputSheetIdx]]$headers)
        modelOutTemplate[[outputSheetIdx]] <<- tibble::tibble(!!!headers)
      }
    })
    scenDataTemplate <- c(modelOutTemplate, modelInTemplate)
    scenDataTemplate <- scenDataTemplate[!vapply(scenDataTemplate, is.null, logical(1L))]
  }
  if(is.null(errMsg)){
    if(identical(config$activateModules$scenario, TRUE) && 
       !file.exists("./modules/db_scen_save.R")){
      errMsg <- "The GMSWebUI version you possess does not support the scenario mode. " %+%
"Please contact GAMS support if you wish to receive a version that does." 
    }
    
    # define table names (format: modelName_scen.prefix_table.name) where "name" is the name of the dataset
    # scenario data is a concatenated list of outputData and inputData
    scenTableNames    <- c(names(modelOut), inputDsNames)
    scenTableNames    <- gsub("_", "", modelName, fixed = TRUE) %+% "_" %+% scenTableNames
    # define scenario tables to display in interface
    scenTableNamesToDisplay <- c(modelOutToDisplay, inputDsNames)
    # get the operating system that shiny is running on
    serverOS    <- getOS()
    # generate GAMS return code map
    GAMSReturnCodeMap <- c('1' = "Solver is to be called, the system should never return this number", 
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
                           "15" = "Model execution was interrupted",
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
  save(modelIn, modelOut, config, lang, inputDsNames, modelOutToDisplay,
       modelInTemplate, scenDataTemplate, isShinyProxy, modelInTabularData,
       sharedData, colSubset, modelInFileNames, ddownDep, aliasesNoDep,
       choicesNoDep, sliderValues, configGraphsOut, configGraphsIn, 
       inputTabs, inputTabTitles, modelInWithDep, modelOutAlias, 
       modelInMustImport, modelInAlias, DDPar, GMSOpt, currentModelDir, 
       modelInToImportAlias, modelInToImport, scenTableNames,
       scenTableNamesToDisplay, serverOS, GAMSReturnCodeMap, 
       modelInGmsString, file = rSaveFilePath)
}
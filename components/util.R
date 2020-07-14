'%+%' <- function(x, y){
  paste(x, y, sep = '')
}
getCommandArg <- function(argName, exception = TRUE){
  # local mode
  args <- commandArgs(trailingOnly = TRUE)
  matches <- grepl(paste0("^-+", argName, "\\s?=\\s?"), args, 
                   ignore.case = TRUE)
  if(any(matches)){
    return(gsub(paste0("^-+", argName, "\\s?=\\s?"), "", args[matches][1], 
                ignore.case = TRUE))
  }else{
    if(exception){
      stop()
    }else{
      return("")
    }
  }
}
isWindows <- function() .Platform$OS.type == 'windows'
hasContent <- function(x){
  if(inherits(x, "data.frame") && nrow(x) == 0){
    return(FALSE)
  }else if(is.null(x) || length(x) == 0){
    return(FALSE)
  }
  
  return(TRUE)
}
getCommandArg <- function(argName, exception = TRUE){
  # local mode
  args <- commandArgs(trailingOnly = TRUE)
  matches <- grepl(paste0("^-+", argName, "\\s?=\\s?"), args, 
                   ignore.case = TRUE)
  if(any(matches)){
    return(gsub(paste0("^-+", argName, "\\s?=\\s?"), "", args[matches][1], 
                ignore.case = TRUE))
  }else{
    if(exception){
      stop()
    }else{
      return("")
    }
  }
}
getModelPath <- function(modelPath = NULL, envVarPath = NULL){
  # returns name of the model currently rendered
  # 
  # Args:
  # modelPath:                  path of the GAMS model as defined externally (e.g. in development mode)
  # envVarPath:                 name of the environment variable that specifies model path
  #
  # Returns:
  # string with model name or error  in case no model name could be retrieved
  
  envName <- Sys.getenv(envVarPath)
  if(identical(envName, "")){
    modelPath <- file.path(getwd(), modelDir, modelName, modelName %+% ".gms")
  }else if(identical(Sys.getenv("SHINYTEST"), "yes")){
    # shiny proxy mode
    modelPath <- file.path(modeBaseDir, envName, envName %+% ".gms")
  }else{
    modelPath <- envName
  }
  gmsFileName  <- basename(modelPath)
  modelNameRaw <- gsub("\\.[[:alpha:]]{2,3}$", "", gmsFileName)
  modelDir     <- dirname(modelPath)
  return(list(modelDir, gmsFileName, tolower(modelNameRaw), modelNameRaw))
}
getInputToImport <- function(data, keywordsNoImport){
  # Retrieves input data which has to be loaded from an external source
  #
  # Args:
  # data:                       raw list of input data
  # keywordsNoImport:           list of keywords to that define that the input data is NOT to be imported
  #                             (note that only keywords on the first level in the JSON file will be considered)
  #
  # Returns:
  # list of sheets that have to be imported from an external source
  
  dataToImport <- list()
  # index variable as c() is slow
  j <- 1
  for(i in seq_along(data)){
    if(!any(tolower(names(data[[i]])) %in% tolower(keywordsNoImport))){
      dataToImport[[j]] <- data[[i]]
      names(dataToImport)[[j]] <- names(data)[[i]]
      j <- j + 1
    }
  }
  return(dataToImport)
}

getWidgetDependencies <- function(widgetType, depString){
  if(!is.character(depString) || length(depString) != 1L){
    return(character(0L))
  }
  switch(widgetType,
         slider = {
           widgetDepTmp <- strsplit(substr(depString, 1L, 
                                           nchar(depString) - 1L), "(", fixed = TRUE)[[1]]
           return(c(widgetDepTmp[1], strsplit(widgetDepTmp[2], "$", fixed = TRUE)[[1]]))
         },
         dropdown = {
           if(!grepl("([^\\$]+\\$[^\\$]+)|(^\\$[^\\$]+)|([^\\$]+\\$$)", depString)){
             # no dependencies
             return(character(0L))
           }
           if(startsWith(depString, "$")){
             depString <- substring(depString, 2L)
             if(endsWith(depString, "$")){
               depID <- 2L
             }else{
               depID <- 0L
             }
           }else if(endsWith(depString, "$")){
             depID <- 1L
           }else{
             depID <- 0L
           }
           depString <- strsplit(depString, "$", fixed = TRUE)[[1]]
           if(length(depString) == 1L)
             depString <- c("", depString)
           return(c(depID, depString))
         },
         {
           return(character(0L))
         })
}

getInputType <- function(data, keywordsType, isMultiDropdown = FALSE){
  # Retrieves input type from JSON file based on keyword list
  #
  # Args:
  # data:                       raw list of input data
  # keywordsType:               list of return value/keyword pairs (example: "hot" = "columns")
  # isMultiDropdown:            boolean that specifies whether input is multi dropdown
  #
  # Returns:
  # return type corresponding to keyword found or error in case no keywords matched
  if(isTRUE(isMultiDropdown)){
    return("dropdown")
  }
  for(i in seq_along(keywordsType)){
    if(keywordsType[[i]] %in% names(data)){
      return(names(keywordsType)[[i]])
    }
  }
  stop("No valid input type found.", call. = F)
}

getDependenciesDropdown <- function(choices, modelIn, name = NULL){
  # Retrieves list of input sheets that dropdown menu depends on (whose data has to be loaded)
  #
  # Args:
  # choices:                    raw list of choices from JSON file
  # modelIn:                    JSON element with model input data (used to verify dependencies)
  # name:                       name of the dropdown menu
  #
  # Returns:
  # list of sheet and column names that need to be loaded for dropdown menu to have all data required (all lower case).
  # list also contains singular elements without dependencies
  
  ddownDep          <- list(fw = list(), bw = list())
  # define indexing variable for strings as c() is slow
  k <- 1
  
  if(length(choices)){
    choices <- as.character(choices)
    elRaw <- choices
    forwardDep  <- startsWith(choices, "$") & !startsWith(choices, "$$")
    elRaw[forwardDep] <- stringi::stri_trim_left(elRaw[forwardDep], 
                                                 pattern = "[^\\$]")
    backwardDep <- endsWith(choices, "$") & !endsWith(choices, "$$")
    elRaw[backwardDep] <- stringi::stri_trim_right(elRaw[backwardDep], 
                                                   pattern = "[^\\$]")
    hasDep <- forwardDep | backwardDep
    ddownDep$strings <- unlist(gsub("$$", "$", choices[!hasDep], 
                                    fixed = TRUE), use.names = FALSE)
    lapply(seq_along(choices)[hasDep], function(i){
      #check for each element of "choices" if it contains a data reference in json (in the form of a string, e.g. "dataset_1$column_3") or a simple string or number
      # examples: "$a$b"   <- column b from sheet a are choices for dropdown
      #           "$b"     <- all columns b are choices for dropdown
      #           "a$b$"   <- column b from sheet a are NOT choices for dropdown but will be filtered when selected
      #           "$a$b$"  <- column b from sheet a are choices for dropdown AND will be filtered when selected (NOT RECOMMENDED AS FILTERING OF DATA WILL ALSO FILTER DROPDOWN MENU)
      #           "$b$"    <- all columns b are choices for dropdown and will be filtered when selected (NOT RECOMMENDED AS FILTERING OF DATA WILL ALSO FILTER DROPDOWN MENU)
      #           "b$"     <- no input data (choices) for dropdown but all columns b will be filtered when selected
      # to escape a dollar sign in a string, dollar should be used again:
      # examples : "a$$b"  <- "a$b" (string without dependencies), "a$$" -> "a$" (same), "$$a" <- "$a" (same)
      
      # check case with both backward and forward dependency on the same column and issue error if stric mode is active
      # find out if column has dependency defined and replace leading and ending signs
      
      if(grepl("$", elRaw[i], fixed = TRUE)){
        # split string into the layers/elements ("dataset_1$column_3" -> "dataset_1", "column_3")
        el <- strsplit(elRaw[i], "$", fixed = TRUE)[[1]]
        # check if elements in el match with the structure of the considered input data.
        idx1 <- match(el[[1]], names(modelIn))[1]
        idx2 <- match(el[[2]], names(modelIn[[idx1]]$headers))[1]
        if(is.na(idx2)){
          if(identical(el[[1]], name)){
            # first index is element itself, thus it is a reference to a shared database
            if(length(el) > 1){
              ddownDep$shared <<- el[[2]]
            }else{
              ddownDep$shared <<- el[[1]]
            }
            return(ddownDep)
          }else if(!is.na(idx1) && 
                   identical(modelIn[[idx1]]$type, "dropdown") &&
                   length(el) > 1 && forwardDep[i]){
            # dependency on another dropdown menu, so dont check header info
            j <- length(ddownDep$fw[[names(modelIn)[[idx1]]]]) + 1
            ddownDep$fw[[tolower(names(modelIn)[[idx1]])]][[j]] <<- getNestedDep(el[-1])
          }else{
            stop(paste0("The header: '", el[[2]], "' for input sheet: '", 
                        el[[1]], "' could not be found. Make sure you define a valid reference."), 
                 call. = FALSE)
          }
        }else{
          # add another forward dependency
          if(forwardDep[i]){
            j <- length(ddownDep$fw[[names(modelIn)[[idx1]]]]) + 1
            ddownDep$fw[[names(modelIn)[[idx1]]]][[j]] <<- names(modelIn[[idx1]]$headers)[[idx2]]
          }
          # add another backward dependency
          if(backwardDep[i]){
            j <- length(ddownDep$bw[[names(modelIn)[[idx1]]]]) + 1
            ddownDep$bw[[names(modelIn)[[idx1]]]][[j]] <<- names(modelIn[[idx1]]$headers)[[idx2]]
          }else if(!forwardDep[i]){
            # neither forward nor backward dependency selected results in error or rendering as string
            stop(paste0("Neither a forward nor a backward dependency was defined in: '", 
                        choices[[i]], "'. Make sure you define some type of dependency."), 
                 call. = FALSE)
          }
        }
      }else{
        # define identifier variable to check whether column exists
        colFound <- FALSE
        # only column was entered (no sheet name)
        # find all sheets with column names
        if(length(modelIn)){
          for(idx1 in seq_along(modelIn)){
            # return index if available
            idx2 <- match(elRaw[i], names(modelIn[[idx1]]$headers))[1]
            if(!is.na(idx2)){
              # add another forward dependency
              if(forwardDep[i]){
                j <- length(ddownDep$fw[[names(modelIn)[[idx1]]]]) + 1
                ddownDep$fw[[names(modelIn)[[idx1]]]][[j]] <<- names(modelIn[[idx1]]$headers)[[idx2]]
              }
              # add another backward dependency
              if(backwardDep[i]){
                j <- length(ddownDep$bw[[names(modelIn)[[idx1]]]]) + 1
                ddownDep$bw[[names(modelIn)[[idx1]]]][[j]] <<- names(modelIn[[idx1]]$headers)[[idx2]]
              }
              # new element was added so increment counter
              if(forwardDep[i] || backwardDep[i]){
                colFound <- TRUE
              }else{
                # neither forward nor backward dependency selected results in error or rendering as string
                stop(paste0("Neither a forward nor a backward dependency was defined in: '", 
                            choices[[i]], "'. Make sure you define some type of dependency."), call. = FALSE)
              }
            }
          }
          # no column was found with matching name (invalid reference)
          if(!colFound){
            stop(paste0("A column named: '", elRaw[i], "' could not be found. Make sure you define a valid reference."),
                 call. = FALSE)
          }
        }
      }
    })
    return(ddownDep)
  }else{
    stop("The dropdown menu does not have any choices defined. Please make sure you define atleast one option to choose from in the JSON file.", call. = F)
  }
}

getDependenciesSlider <- function(min, max, def, step, modelIn, listOfOperators){
  # Retrieves list of input sheets that dropdown menu depends on (whose data has to be loaded)
  # Note: currently only forward dependencies supported for slider
  #
  # Args:
  # min:                        raw JSON data for minimum slider value
  # max:                        raw JSON data for maximum slider value
  # def:                        raw JSON data for default slider value
  # step:                       raw JSON data for step size
  # modelIn:                    JSON element with model input data (used to verify dependencies)
  # listOfOperators:            list of valid operators for sliders
  #
  # Returns:
  # list of values with either a sheet name/column name pair in case of an external dependency or a numeric value in case of no dependency.
  # returns NULL, if no value of slider has dependency on external data
  
  listOfValues <- c("min" = min, "max" = max, "def" = def, "step" = step)
  # check if any value of slider has dependency on external data
  if(any(grepl("(", listOfValues, fixed = TRUE))){
    # evaluate slider values
    sliderDep <- lapply(listOfValues, function(el){
      if(grepl("(", el, fixed = TRUE)){
        # split string in operator and operand part
        splitted <- tolower(strsplit(el, "\\(|\\)")[[1]])
        operator <- splitted[[1]]
        operatorId <- match(operator, listOfOperators)[1]
        if(is.na(operatorId)){
          stop(paste0("'", operator, "' is not a valid operator for sliders."), 
               call. = FALSE)
        }
        dep      <- splitted[[2]]
        # split string into the sheets/elements ("dataset_1$column_3" -> "dataset_1", "column_3")
        dep <- strsplit(dep, "$", fixed = TRUE)[[1]]
        # make sure that in case a reference is given, the underlying data is also part of the input data
        idx1 <- match(dep[1], names(modelIn))[1]
        idx2 <- match(dep[2], names(modelIn[[idx1]]$headers))[1]
        if(is.na(idx2)){
          if(!is.na(idx1)){
            if(identical(modelIn[[idx1]]$type, "daterange")){
              # dependency on daterange selector
              sliderValue <- list()
              sliderValue[[tolower(dep[[1]])]] <- "$daterange"
              sliderValue[["$operator"]] <- names(listOfOperators)[[operatorId]]
              return(sliderValue)
            }else if(identical(modelIn[[idx1]]$type, "dropdown") && length(dep) > 1){
              # dependency on another dropdown menu
              sliderValue <- list()
              sliderValue[[tolower(dep[[1]])]] <- getNestedDep(dep[c(-1)])
              sliderValue[["$operator"]] <- names(listOfOperators)[[operatorId]]
              return(sliderValue)
            }
          }
          
          if(length(dep) > 1){
            stop(paste0("Invalid reference. The header: '", dep[[2]], 
                        "' specified for input sheet: '", dep[[1]], 
                        "' could not be found."), call. = FALSE)
          }else{
            stop(paste0("Invalid reference. The reference: '", dep, 
                        "' specified could not be found."), call. = FALSE)
          }
        }else{
          sliderValue <- list()
          sliderValue[[dep[[1]]]] <- names(modelIn[[idx1]]$headers)[[idx2]]
          sliderValue[["$operator"]] <- names(listOfOperators)[[operatorId]]
          return(sliderValue)
        }
      }else{
        if(is.na(as.numeric(el))){
          stop(paste0("'", el, "' is not a valid value for a slider."), 
               call. = FALSE)
        }
        return(as.numeric(el))
      }
    })
    return(sliderDep)
  }else{
    return(NULL)
  }
}
isDate <- function(x){
  tryCatch(!is.na(as.Date(x[[1]])), error = function(e){FALSE})
}
verifyScalarInput <- function(data, scalarHeaders, scalarSymbols){
  # Checks whether the scalar dataframe is valid
  #
  # Args:
  #   data:          dataframe to be verified
  #   scalarHeaders: headers that need to match those of the dataframe
  #   scalarSymbols: symbols that need to be present in dataframe
  #
  # Returns:
  #   boolean specifying whether input data is valid (TRUE) or not (FASLE)
  if(!verifyInput(data, scalarHeaders)){
    return(FALSE)
  }
  additionalInputScalars <- is.na(match(tolower(data[[1]]), tolower(scalarSymbols)))
  if(any(additionalInputScalars)){
    flog.info("Additional input scalars found in the dataset you just imported: '%s'. These will be skipped!",
              paste(data[[1]][additionalInputScalars], collapse = "', '"))
  }
  return(TRUE)
}
verifyInput <- function(data, headers){
  # Checks whether a dataframe is valid with regard to a specific schema
  #
  # Args:
  #   data:        dataframe to be verified
  #   headers:     headers that need to match those of the dataframe
  #
  # Returns:
  #   boolean specifying whether input data is valid (TRUE) or not (FASLE)
  
  
  if(!is.null(headers)){
    for(i in seq_along(headers)){
      if(identical(headers[[i]]$type, "numeric") && class(data[[i]]) != "numeric"){
        return(FALSE)
      }
      if(identical(headers[[i]]$type, "string") && 
         !any(class(data[[i]]) %in% c("factor", "character", "numeric", "POSIXt", "Date"))){
        return(FALSE)
      }
    }
    # data has headers, so verify makes sense
    if(all(tolower(names(headers)) == tolower(names(data))) && length(headers) == length(data)){
      return(TRUE)
    }
    return(FALSE)
  }else{
    # data is not a table (e.g. slider or dropdown menu)
    return(TRUE)
  }
}

roundDf <- function(data, decimals){
  # Rounds numeric columns in a data frame.
  #
  # Args:
  #   data:       dataframe with numeric columns
  #   decimals:   number of decimal places
  #
  # Returns:
  #   dataframe with numeric values rounded to number of decimals specified
  
  isNumeric <- vapply(data, is.numeric, FUN.VALUE = logical(1))
  data[is.na(data)] <- 0
  data[,isNumeric] <-  round(data[,isNumeric], digits=decimals)
  return(data)
}

addHtmlLineBreaks <- function(string){
  # Replaces \n with <br> tags to force line breaks in HTML while not allowing other HTML tags in order to prevent XSS
  #
  # Args:
  #   string:     string with \n where new line should start
  #
  # Returns:
  #   string with <br> tags where \n used to be
  
  # escape string
  escapedString <- htmltools::htmlEscape(string)
  # replace \n with <br> tag
  escapedString <- gsub("\\n", "<br>", escapedString)
  
  return(escapedString)
}

getOS <- function(){
  # returns string that identifies the operationg system 
  # that shiny is running on
  
  sysinf <- Sys.info()
  if (!is.null(sysinf)){
    os <- sysinf['sysname']
    if (os == 'Darwin')
      os <- "osx"
  } else {
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os))
      os <- "osx"
    if (grepl("linux-gnu", R.version$os))
      os <- "linux"
  }
  tolower(os)
}

dateColToChar <- function(conn, df){
  # converts date columns to character
  if(inherits(conn, "PqConnection")){
    return(df)
  }
  df[] <- lapply(df, function(col){
    if(inherits(col, "POSIXt") || inherits(col, "Date")){
      as.character(col)
    }else{
      col
    } 
  })
  return(df)
}
addCssDim <- function(x, y){
  # Adds two css dimensions together
  #
  # Args:
  #   x:     string or numeric that contains height and possibly unit
  #   y:     numeric that specifies additional height to be added (same unit as x)
  #
  # Returns:
  #   string with sum of both height and unit
  
  stopifnot(!missing(x), !is.null(x), is.character(x) || is.numeric(x), length(x) == 1)
  stopifnot(!missing(y), !is.null(y), is.numeric(y), length(y) == 1)
  
  tmp    <- strsplit( gsub("([0-9]+)","\\1~", x), "~" )[[1]]
  dgts   <- as.numeric(tmp[1]) + y
  if(is.na(dgts)){
    dgts <- 0 + y
  }
  if(is.na(tmp[2])){
    return(paste0(dgts, "px"))
  }
  return(paste0(dgts, tmp[2]))
}

getGMSPar <- function(inputNames, prefixPar){
  # Finds compile time variables/GAMS options and returns them as a list
  #
  # Args:
  #   inputNames:     vector of names of input sheets
  #   prefixPar:      numeric that specifies additional height to be added (same unit as x)
  #
  # Returns:
  #   vector of names of compile time variables
  
  inputNames       <- tolower(inputNames)
  prefixPar        <- tolower(paste0("^", prefixPar))
  isGMSPar         <- grepl(prefixPar, inputNames)
  
  return(inputNames[isGMSPar])
}
virtualActionButton <- function(...){
  o <- structure(sum(...), class = "shinyActionButtonValue")
  invisible(o)
}
showErrorMsg <- function(title, errMsg){
  if(!is.null(errMsg)){
    stopifnot(is.character(errMsg), length(errMsg) == 1)
  }
  
  if(!is.null(errMsg)){
    stopifnot(is.character(title), length(title) == 1)
    showModal(modalDialog(
      title = title, HTML(addHtmlLineBreaks(errMsg))
    ))
    return(invisible(NULL))
  }
  return(invisible(1))
}
readTraceData <- function(filePath, traceColNames){
  traceData <- suppressWarnings(read_csv(filePath, col_names = FALSE,
                                         skip = 5, col_types = "cccccdidddddiiiddddddc"))
  traceData <- traceData[nrow(traceData), ]
  if(length(traceData) == length(traceColNames)){
    names(traceData) <- traceColNames
    return(traceData)
  }else{
    stop("Trace data has incorrect length.", call. = FALSE)
  }
}
getIcon <- function(name, lib){
  if(!identical(name, NULL)){
    lib <- if(identical(lib, "glyphicon")) "glyphicon" else "font-awesome"
    icon <- icon(name, lib)
  }else{
    icon <-  NULL
  }
  return(icon)
}
csv2Vector <- function(csv){
  if(!length(csv)){
    return(character(0L))
  }
  csv <- vapply(csv, function(el){
    if(startsWith(el, ",")){
      return(substring(el, 2L))
    }
    return(el)
  }, character(1L), USE.NAMES = FALSE)
  return(unlist(strsplit(csv, ",", fixed = TRUE), use.names = FALSE))
}
vector2Csv <- function(vector){
  if(!length(vector)){
    return("")
  }else if(length(vector) > 1L || 
           !(startsWith(vector, ",") && endsWith(vector, ","))){
    return(paste0(",", paste0(vector, collapse = ","), ","))
  }else{
    return(vector)
  }
}
showEl <- function(session, id){
  session$sendCustomMessage("gms-showEl", id)
}
showElReplaceTxt <- function(session, id, txt){
  session$sendCustomMessage("gms-showElReplaceTxt", list(id = id, txt = htmltools::htmlEscape(txt)))
}
hideEl <- function(session, id){
  session$sendCustomMessage("gms-hideEl", id)
}
changeHeightEl <- function(session, id, height, delay = NULL){
  session$sendCustomMessage("gms-changeHeightEl", list(id = id, height = height, delay = delay))
}
showHideEl <- function(session, id, delay = 2000, msg = NULL){
  session$sendCustomMessage("gms-showHideEl", list(id = id, delay = delay, msg = msg))
}
enableEl <- function(session, id){
  session$sendCustomMessage("gms-enableEl", id)
}
scrollDown <- function(session, id){
  session$sendCustomMessage("gms-scrollDown", id)
}
disableEl <- function(session, id){
  session$sendCustomMessage("gms-disableEl", id)
}
slideToggleEl <- function(session, id, duration = 400, toggleIconDiv = NULL){
  session$sendCustomMessage("gms-slideToggleEl", 
                            list(id = id, duration = duration, 
                                 toggleIconDiv = toggleIconDiv))
}
toggleEl <- function(session, id){
  session$sendCustomMessage("gms-toggleEl", id)
}
addClassEl <- function(session, id, class){
  session$sendCustomMessage("gms-addClassEl", list(id = id, newclass = class))
}
removeClassEl <- function(session, id, class){
  session$sendCustomMessage("gms-removeClassEl", list(id = id, oldclass = class))
}
emptyEl <- function(session, id){
  session$sendCustomMessage("gms-emptyEl", id)
}
appendEl <- function(session, id, content, text = TRUE, scroll = FALSE, 
                     triggerChange = FALSE){
  session$sendCustomMessage("gms-appendEl", list(id = id, content = content, 
                                                 text = text, scroll = scroll,
                                                 triggerChange = triggerChange))
}
hideModal <- function(session, delay = 1L){
  session$sendCustomMessage("gms-hideModal", delay)
}
updateAttachList <- function(session, id, fileName, token, labelCb, allowExec = FALSE){
  session$sendCustomMessage("gms-updateAttachList", list(name = fileName, id = id, 
                                                         token = token, labelCb = labelCb, 
                                                         allowExec = allowExec))
}
fitTitleInBox <- function(session, id){
  session$sendCustomMessage("gms-fitTitleInBox", id)
}
isBadScenName <- function(scenName){
  return(grepl("^\\s*$", scenName)[[1L]] || nchar(scenName) > 63)
}
switchTab <- function(session, id){
  session$sendCustomMessage("gms-switchTab", id)
}
# redefined reactiveFileReader and reactivePoll functions since the original shiny functions 
# leak an observer that can not be destoryed
# original implementation can be found here: 
#       https://github.com/rstudio/shiny/blob/19623694f585c8e7a8cf2c38e831a6752e5520c6/R/reactives.R#L1316
# Please note that this is a slightly modified version of the original functions 
# by RStudio licensed under GPL v3
#
# once this issue (https://github.com/rstudio/shiny/issues/1548) is closed, 
# the original functions can be used again!
reactivePoll2 <- function(intervalMillis, session, checkFunc, valueFunc) {
  
  rv <- reactiveValues(cookie = isolate(checkFunc()))
  
  obs <- observe({
    rv$cookie <- checkFunc()
    invalidateLater(intervalMillis, session)
  })
  
  re <- reactive({
    rv$cookie
    valueFunc()
  })
  
  return(list("re" = re, "obs" = obs))
}
reactiveFileReader2 <- function(intervalMillis, session, filePath) {
  reactivePoll2(
    intervalMillis, session,
    function() {
      info <- file.info(filePath)
      return(paste(filePath, info$mtime, info$size))
    },
    function() {
      paste(suppressWarnings(do.call(readLines, list(filePath))), collapse = "\n")
    }
  )
}
reactiveFileReaderAppend <- function(intervalMillis, session, filePath) {
  checkFunc <- function() {
    info <- file.info(filePath)
    return(paste(filePath, info$mtime, info$size))
  }
  valueFunc <- function(skip) {
    read_lines(filePath, skip = skip)
  }
  rv <- reactiveValues(cookie = isolate(checkFunc()))
  cursorPos <- 0L
  
  obs <- observe({
    rv$cookie <- checkFunc()
    invalidateLater(intervalMillis, session)
  })
  
  re <- reactive({
    rv$cookie
    fileContent <- valueFunc(cursorPos)
    
    if(!length(fileContent)){
      return("")
    }
    if(identical(cursorPos, 0L)){
      cursorPos <<- length(fileContent)
    }else{
      cursorPos <<- cursorPos + length(fileContent)
      fileContent <- c("", fileContent)
    }
    return(paste(fileContent, collapse = "\n"))
  })
  
  return(list("re" = re, "obs" = obs))
}
prepopPivot <- function(symbol){
  pivotConf <- list(rows = c(), vals = character(1L), aggregatorName = "Sum")
  setEl     <- vector("character", length(symbol$headers))
  j <- 1L
  for(i in seq_along(symbol$headers)){
    if(symbol$headers[[i]]$type == "numeric"){
      pivotConf$vals <- names(symbol$headers)[[i]]
    }else{
      setEl[j] <- names(symbol$headers)[[i]]
      j <- j + 1L
    }
  }
  pivotConf$rows <- setEl[nchar(setEl) > 0.5]
  return(pivotConf)
}

getNestedDep <- function(depStr){
  if(length(depStr) > 1L){
    if(grepl(".+\\[.+\\]$", depStr[[1]])){
      filterCol <- strsplit(depStr, "[", fixed=TRUE)[[1]]
      return(c(gsub("[", "", filterCol[[1]], fixed = TRUE), gsub("]", "", filterCol[[2]], fixed = TRUE), depStr[[2]]))
    }else{
      stop(sprintf("Dependency: '%s' could not be parsed.", paste0(depStr, collapse = "$")), call. = FALSE)
    }
  }else{
    return(depStr)
  }
}
genSpinner <- function(id = NULL, hidden = FALSE, absolute = TRUE, externalStyle = NULL){
  div(id = id, class = "lds-ellipsis", 
      style = paste0(if(is.null(externalStyle))
        "top:50%;left:50%;z-index:1;margin-left:-32px;margin-top:-32px;" else
          externalStyle, 
        if(absolute) "position:absolute;" else "display:block;", if(hidden) "display:none;"), 
      div(class = "gen-spinner"),
      div(class = "gen-spinner"),
      div(class = "gen-spinner"),
      div(class = "gen-spinner")
  )
}
checkboxInput_MIRO <- function(inputId, label, value = FALSE){
  tags$div(class = "shiny-input-container",
           tags$label(class = "cb-label", "for" = inputId, label),
           tags$div(
             tags$label(class = "checkbox-material", 
                        checkboxInput(inputId, label = NULL, value)
             ))
  )
}
autoNumericInput <- function(id, label = NULL, value = NULL, min = NULL, max = NULL, 
                             sign = NULL, decimal = NULL, decimalCharacter = NULL, digitGroupSeparator = NULL){
  HTML(paste0('<div class="form-group shiny-input-container">\n
    <label for="', id, '">', label, '</label>\n
      <input id="', id, '" type="text" class="form-control miro-auto-numeric" value="', value, 
              '"', if(length(min)) paste0(' data-minimum-value="', min, '"'), 
              if(length(max)) paste0(' data-maximum-value="', max, '"'), 
              if(length(sign)) paste0(' data-currency-symbol="', sign, '"'), 
              if(length(decimalCharacter)) paste0(' data-decimal-character="', decimalCharacter, '"'), 
              if(length(digitGroupSeparator)) paste0(' data-digit-group-separator="', digitGroupSeparator, '"'), 
              if(length(decimal)) paste0(' data-decimal-places-override="', decimal, '"'), ' />\n
    </div>'))
}
filterDf <- function(df, filterCondition){
  filterCondition <- unlist(filterCondition)
  if(length(filterCondition) == 1L){
    return(df[[filterCondition]])
  }else if(length(filterCondition) == 3L){
    return(df[df[[filterCondition[[1]]]] == filterCondition[[2]], ][[filterCondition[[3]]]])
  }else{
    stop(sprintf("Bad filter condition: '%s'.", filterCondition), call. = FALSE)
  }
}
plotlyOutput_spinner <- function(...){
  div(
    genSpinner(), plotlyOutput(...)
  )
}

getNoLinesInFile <- function(filePath){
  as.integer(strsplit(trimws(system2("wc", c("-l", filePath), stdout = TRUE, stderr = TRUE)[[1]]),
                      " ", fixed = TRUE)[[1]][[1]])
}
downloadHandlerError <- function(file, msg = "Some error occurred trying to download this file."){
  return(writeLines(msg, file))
}
validateHeaders <- function(headersData, headersConfig, headerTypes = NULL){
  headerNames <- headersData
  if(!is.null(headerTypes)){
    headerNames <- names(headersData)
  }
  validNames <- identical(length(headerNames), length(headersConfig)) && 
    all(!is.na(match(tolower(headerNames), tolower(headersConfig))))
  if(!validNames){
    return(FALSE)
  }
  if(is.null(headerTypes)){
    return(TRUE)
  }
  return(hasValidHeaderTypes(headersData, headerTypes))
  
}
hasValidHeaderTypes <- function(headersData, headerTypes){
  stopifnot(inherits(headersData, "data.frame"))
  return(all(vapply(seq_along(headersData), function(i){
    if(any(class(headersData[[i]]) == "numeric")){
      return(identical(substr(headerTypes, i, i), "d") ||
               (identical(substr(headerTypes, i, i), "i") &&
                  all(!is.na(as.integer(headersData[[i]])))))
    }else if(any(class(headersData[[i]]) == "integer")){
      return(substr(headerTypes, i, i) %in% c("i", "d"))
    }else if(any(class(headersData[[i]]) == "POSIXt")){
      return(substr(headerTypes, i, i) %in% c("T", "c"))
    }else if(any(class(headersData[[i]]) == "logical")){
      return(identical(substr(headerTypes, i, i), "l"))
    }else if(any(class(headersData[[i]]) == "raw")){
      return(identical(substr(headerTypes, i, i), "b"))
    }else{
      return(identical(substr(headerTypes, i, i), "c"))
    }
  }, logical(1L), USE.NAMES = FALSE)))
} 
fixColTypes <- function(data, colTypes){
  stopifnot(identical(length(data), nchar(colTypes)))
  
  data[] <- lapply(seq_along(data), function(i){
    colType <- substr(colTypes, i, i)
    if(identical(colType, "c") && 
       (is.numeric(data[[i]]) || is.logical(data[[i]]))){
      return(as.character(data[[i]]))
    }else if(identical(colType, "d") && 
             (is.character(data[[i]]) || is.logical(data[[i]]))){
      return(suppressWarnings(as.numeric(data[[i]])))
    }else{
      return(data[[i]])
    } 
  })
  return(data)
}
ddToTibble <- function(values, metaData){
  choiceIdx         <- match(values, 
                             metaData$dropdown$choices)
  if(length(metaData$headers)){
    headers <- names(metaData$headers)
  }else{
    headers <- "val"
  }
  aliases <- ""
  if(length(choiceIdx)){
    if(!any(is.na(choiceIdx)) && 
       length(metaData$dropdown$aliases)){
      aliasCol          <- metaData$dropdown$aliases[choiceIdx]
      aliasCol[is.na(aliasCol)] <- ""
      aliases <- aliasCol
    }
  }else{
    aliases <- character(0L)
    values <- character(0L)
  }
  if(length(headers) > 1L){
    ddTibble <- tibble(val = values, text = aliases)
  }else{
    ddTibble <- tibble(val = values)
  }
  
  names(ddTibble) <- headers
  return(ddTibble)
}
pidExists <- function(pid){
  pid <- suppressWarnings(as.integer(pid))
  if(!length(pid) || is.na(pid)){
    return(FALSE)
  }
  if(isWindows()){
    grepl("Mem Usage", run("tasklist", c("/FI", paste0("PID eq ", pid)), 
                           windows_hide_window = TRUE)$stdout, fixed = TRUE)
  }else{
    pidExists <- TRUE
    tryCatch(processx::run("ps", c("-p", pid)), error = function(e){
      pidExists <<- FALSE
    })
    return(pidExists)
  }
}
escapeGAMSCL <- function(input){
  if(isWindows()){
    ret <- shQuote(input, type = "cmd")
  }else{
    ret <- shQuote(input)
    ret <- gsub("^'|'$", "\"", ret)
  }
  return(ret)
}
gmsFilePath <- function(path){
  if(isWindows()){
    return(gsub("/", "\\", path, fixed = TRUE))
  }
  return(path)
}
uploadFile <- function(file, url, userpwd){
  stopifnot(file.exists(file))
  stopifnot(is.character(url), length(url) == 1L)
  con <- file(file, open = "rb")
  on.exit(close(con))
  h <- curl::new_handle(upload = TRUE, filetime = FALSE, httpauth = 1, userpwd = userpwd)
  curl::handle_setopt(h, readfunction = function(n) {
    readBin(con, raw(), n = n)
  })
  curl::curl_fetch_memory(url, handle = h)
}
CharArray <- R6Class("CharArray", public = list(
  push = function(el){
    stopifnot(length(el) > 0L, is.character(el))
    private$items[[self$size() + 1L]] <- el
    invisible(self)
  },
  pop = function(){
    stopifnot(self$size() > 0L)
    private$items[[self$size()]]
    invisible(self)
  },
  delete = function(el){
    stopifnot(identical(length(el), 1L), is.character(el))
    
    idx <- match(el, private$items)[[1L]]
    if(is.na(idx)){
      return(FALSE)
    }
    private$items[[idx]] <- NULL
    return(TRUE)
  },
  reset = function(){
    private$items <- private$initialItems
    invisible(self)
  },
  update = function(old, new){
    if(length(old)){
      stopifnot(identical(length(old), 1L), is.character(old))
    }else{
      self$push(new)
      return(TRUE)
    }
    if(length(new)){
      stopifnot(identical(length(new), 1L), is.character(new))
    }else if(self$delete(old)){
      return(TRUE)
    }else{
      return(FALSE)
    }
    
    idx <- match(old, private$items)[[1L]]
    if(is.na(idx)){
      return(FALSE)
    }
    private$items[[idx]] <- new
    return(TRUE)
  },
  get = function(){
    unlist(private$items, use.names = FALSE)
  },
  initialize = function(el = NULL){
    if(length(el)){
      stopifnot(is.character(el))
      private$initialItems <- as.list(el)
      private$items <- private$initialItems
    }
    invisible(self)
  },
  size = function(){
    length(private$items)
  }
), private = list(
  initialItems = list(),
  items = list()
))
Set <- R6Class("Set", inherit = CharArray, public = list(
  push = function(el){
    el <- as.character(el)
    stopifnot(identical(length(el), 1L))
    if(!el %in% private$items){
      private$items[[self$size() + 1L]] <- el
    }
    invisible(self)
  },
  initialize = function(el = NULL){
    if(length(el)){
      el <- unique(as.character(el))
      private$initialItems <- as.list(el)
      private$items <- private$initialItems
    }
    invisible(self)
  },
  update = function(old, new){
    if(length(new) && new %in% private$items){
      return(invisible(self))
    }
    super$update(old, new)
  }))
IdIdxMap <- R6Class("IdIdxMap", public = list(
  push = function(arrayID, elID){
    stopifnot(identical(length(arrayID), 1L), is.character(arrayID))
    stopifnot(identical(length(elID),  1L))
    if(elID %in% private$items[[arrayID]]){
      return(self$get(arrayID, elID))
    }
    private$items[[arrayID]] <- append(private$items[[arrayID]], elID)
    self$get(arrayID, elID)
  },
  pop = function(arrayID, elID){
    stopifnot(identical(length(arrayID), 1L), is.character(arrayID))
    stopifnot(identical(length(elID),  1L))
    
    if(identical(self$size(arrayID), 0L)){
      return(integer(0L))
    }
    arrIdx <- match(elID, private$items[[arrayID]])
    if(is.na(arrIdx)){
      return(integer(0L))
    }
    private$items[[arrayID]] <- private$items[[arrayID]][-arrIdx]
    return(arrIdx)
  },
  get = function(arrayID, elID){
    stopifnot(identical(length(arrayID), 1L), is.character(arrayID))
    stopifnot(identical(length(elID),  1L))
    
    if(!arrayID %in% names(private$items)){
      return(integer(0L))
    }
    arrIdx <- match(elID, private$items[[arrayID]])
    if(is.na(arrIdx)){
      return(integer(0L))
    }
    return(arrIdx)
  },
  initialize = function(el = NULL){
    if(length(el)){
      private$items <- el
    }
    invisible(self)
  },
  size = function(arrayID){
    stopifnot(identical(length(arrayID), 1L), is.character(arrayID))
    if(arrayID %in% names(private$items)){
      return(length(private$items[[arrayID]]))
    }
    return(0L)
  }
), private = list(
  items = list()
))
parseMiroLog <- function(session, logPath, 
                         inputSymbols, inputScalars = NULL){
  logContent <- htmltools::htmlEscape(read_lines(logPath))
  if(!length(inputSymbols)){
    return(list(annotations = list(), content = logContent))
  }
  parsedLog <- list()
  for(i in seq_along(logContent)){
    logLine <- logContent[i]
    logLineSplitted <- stri_split_fixed(str = logLine, 
                                        pattern = "::", n = 2)[[1L]]
    if(length(logLineSplitted) < 2L){
      next
    }
    symbolName <- tolower(trimws(logLineSplitted[[1L]]))
    if(symbolName %in% inputSymbols){
      parsedLog[[symbolName]] <- c(parsedLog[[symbolName]], 
                                   paste0('<li ondblclick="Miro.jumpToLogMark(', i, ')">', 
                                          paste(logLineSplitted[-1], collapse = ""),
                                          '</li>'))
      logContent[i] <- paste0('<mark id="mlogMark_', i, 
                              '" class="miro-log-mark">', logContent[i], '</mark>')
      next
    }
    if(length(inputScalars) && symbolName %in% inputScalars){
      parsedLog[[scalarsFileName]] <- c(parsedLog[[symbolName]], 
                                        paste(logLineSplitted[-1], collapse = ""))
      logContent[i] <- paste0('<mark id="mlogMark_', i, 
                              '" class="miro-log-mark">', logContent[i], '</mark>')
      next
    }
  }
  return(list(content = logContent, annotations = parsedLog))
}
filterScalars <- function(scalars, scalarsOutList, type = c("input", "output")){
  type <- match.arg(type)
  
  scalarsToFilter <- c()
  if(length(scalarsOutList)){
    scalarsToFilter <- scalarsOutList$symnames
  }
  if(identical(type, "input")){
    return(scalars[!tolower(scalars[[1]]) %in% scalarsToFilter, ])
  }
  return(scalars[tolower(scalars[[1]]) %in% scalarsToFilter, ])
}
setDbConfig <- function(){
  config <- list()
  errMsg <- NULL
  
  envNameDbDataMap <- list(
    list(envVar = 'MIRO_DB_TYPE', keyName = 'type', desc = 'database type', default = 'postgres'),
    list(envVar = 'MIRO_DB_USERNAME', keyName = 'username', desc = 'database username'),
    list(envVar = 'MIRO_DB_PASSWORD', keyName = 'password', desc = 'database password'),
    list(envVar = 'MIRO_DB_NAME', keyName = 'name', desc = 'database name'),
    list(envVar = 'MIRO_DB_HOST', keyName = 'host', desc = 'database host'),
    list(envVar = 'MIRO_DB_PORT', keyName = 'port', desc = 'database port', numeric = TRUE, default = 5432))
  
  for(i in seq_along(envNameDbDataMap)){
    metaData <- envNameDbDataMap[[i]]
    
    data <- Sys.getenv(metaData$envVar, unset = NA)
    Sys.unsetenv(metaData$envVar)
    if(is.na(data)){
      if(length(metaData$default)){
        config[[metaData$keyName]] <- metaData$default
      }else{
        errMsg <- paste(errMsg, paste0("The ", metaData$desc , " could not be identified. Please make sure you specify a valid ", 
                                       metaData$desc, ":\nThe ", metaData$desc, " should be stored in the environment variable: '", metaData$envVar, "'."),
                        sep = "\n")
      }
    }else{
      if(identical(metaData$numeric, TRUE)){
        data <- suppressWarnings(as.numeric(data))
        if(is.na(data)){
          errMsg <- paste(errMsg, paste0("The ", metaData$desc , " must be numeric."),
                          sep = "\n")
          next
        }
      }
      config[[metaData$keyName]] <- data
    }
  }
  return(list(data = config, errMsg = errMsg))
}
file.move <- function(from, to){
  createDirIfNonExistent(to)
  file.rename(from = from,  to = to)
}
file.copy2 <- function(from, to){
  createDirIfNonExistent(to)
  fromIsDir <- dir.exists(from)
  ret1 <- file.copy(from[!fromIsDir], to[!fromIsDir], overwrite = TRUE)
  if(any(!ret1)){
    return(FALSE)
  }
  fromDirs <- from[fromIsDir]
  toDirs   <- dirname(to[fromIsDir])
  for(i in seq_along(fromDirs)){
    if(!file.copy(fromDirs[i], toDirs[i], 
                  recursive = TRUE, overwrite = TRUE)){
      return(FALSE)
    }
  }
  return(TRUE)
}
createDirIfNonExistent <- function(dirs){
  nonExistingDirs <- !dir.exists(dirname(dirs))
  if(any(nonExistingDirs)){
    nonExistingDirs <- unique(dirname(dirs)[nonExistingDirs])
    suppressWarnings({
      for(nonExistingDir in nonExistingDirs){
        dir.create(nonExistingDir, recursive=TRUE)
      }
    })
  }
}
hotToR <- function(data, metaData, fixType = TRUE){
  dataTmp <- suppressWarnings(as_tibble(
    data.table::rbindlist(data$data, use.names = FALSE)))
  if(length(metaData[["pivotCols"]])){
    fixedCols <- length(metaData$headers) - 2L
    names(dataTmp) <- unlist(data$params$colHeaders)
    names(dataTmp)[1:fixedCols] <- unlist(data$params$rColHeaders)[1:fixedCols]
    return(dataTmp)
  }
  dataTmp <- fixColTypes(dataTmp,
                         metaData$colTypes)
  names(dataTmp) <- names(metaData$headers)
  return(dataTmp)
}
isAbsolutePath <- function(path){
  if(isWindows()){
    # credits to: agent-j @ https://stackoverflow.com/questions/6416065/c-sharp-regex-for-file-paths-e-g-c-test-test-exe
    return(grepl("^(?:[a-zA-Z]\\:|\\\\\\\\[\\w\\.]+\\\\[\\w.$]+)\\\\(?:[\\w]+\\\\)*\\w([\\w.])+$", 
                 path, perl = TRUE))
  }
  return(startsWith(path, "/"))
}
zipMiro <- function(zipfile, files, baseDir, ...){
  if(any(isAbsolutePath(files))){
    stop("Absolute paths not allowed when zipping!", call. = FALSE)
  }
  if(any(grepl("[.][.][/\\\\]", files))){
    stop("Directory climbing not allowed when zipping!", call. = FALSE)
  }
  currentWd <- getwd()
  on.exit(setwd(currentWd))
  setwd(baseDir)
  
  suppressMessages(zip::zip(zipfile, files, ...))
}
getHcubeScalars <- function(modelIn){
  return(names(modelIn)[vapply(seq_along(modelIn), 
                               function(i) 
                                 isTRUE(modelIn[[i]]$dropdown$single) ||
                                 isTRUE(modelIn[[i]]$dropdown$checkbox), 
                               logical(1L), USE.NAMES = FALSE)])
}
loadPfFileContent <- function(content, GMSOpt = character(0L), DDPar = character(0L)){
  content <- stri_split_regex(content, "=| ", 2)
  content <- tryCatch(tibble(scalar = trimws(vapply(content, "[[", 
                                                    character(1L), 1L, 
                                                    USE.NAMES = FALSE), "left", "-"), 
                             description = character(length(content)), 
                             value = trimws(vapply(content, "[[", 
                                                   character(1L), 2L, 
                                                   USE.NAMES = FALSE), "both", '"')), 
                      error = function(e){
                        return(tibble())
                      })
  if(!length(content)){
    return(tibble())
  }
  content       <- content[tolower(content[[1]]) %in% 
                             c(GMSOpt, 
                               outer(DDPar, c("", "_lo", "_up"), FUN = "paste0")), , 
                           drop = FALSE]
  clArgsTmp     <- stri_replace_first_regex(content[[1]], 
                                            "_lo$", "\\$lo")
  clArgsTmp     <- stri_replace_first_regex(clArgsTmp, "_up$", "\\$up")
  if(!length(clArgsTmp)){
    return(tibble())
  }
  content[[1]]  <- clArgsTmp
  return(content)
}

getValidCsvFromZip <- function(zipFileName, dsToVerify, uid){
  tryCatch({
    filesInArchive <- zip_list(zipFileName)
  }, error = function(e){
    stop(sprintf("e: Could not read zip archive: '%s'.", 
                 zipFileName))
  })
  
  filesInArchive   <- filesInArchive[filesInArchive$compressed_size > 0, ]$filename
  validFileNames <- grep("^((?!\\.\\.).)*\\.csv$", filesInArchive, 
                         ignore.case = TRUE, value = TRUE, perl = TRUE)
  validFileNames <- validFileNames[tolower(validFileNames) %in% paste0(dsToVerify, ".csv")]
  
  if(!identical(length(filesInArchive), length(validFileNames))){
    stop(sprintf("Zip archive contains invalid files: '%s'.", 
                 zipFileName))
  }
  
  tmpDir <- file.path(tempdir(), paste0(uid, "_imp_tmp_dir"))
  
  if(file.exists(tmpDir) && !identical(unlink(tmpDir, recursive = TRUE), 0L)){
    stop(sprintf("e: Could not remove temporary directory: '%s'.", tmpDir))
  }
  if(!dir.create(tmpDir, recursive = TRUE)){
    stop(sprintf("e: Could not create temporary directory: '%s'.", tmpDir))
  }
  
  tryCatch(
    csvPaths <- zip::unzip(zipFileName, exdir = tmpDir, 
                           junkpaths = TRUE)
    , error = function(e){
      unlink(tmpDir, recursive = TRUE)
      stop(sprintf("e: Problems extracting zip archive. Error message: '%s'.", 
                   conditionMessage(e)))
    })
  if(any(Sys.readlink(file.path(tmpDir, validFileNames)) != "")){
    unlink(tmpDir, recursive = TRUE)
    stop(sprintf("zip archive contains symlinks! Import stopped."))
  }
  return(list(tmpDir = tmpDir, validFileNames = validFileNames))
}
DTbuildColHeaderContainer <- function(colNames, noRowHeaders, rowHeaders){
  if(noRowHeaders > 0){
    colNameHeaders <- colNames[-seq_len(noRowHeaders)]
  }else{
    colNameHeaders <- colNames
  }
  # note that split character is not an ASCII full stop, but UNICODE U+2024
  colNameList <- stri_split_fixed(colNameHeaders, "\U2024")
  noColDim <- length(colNameList[[1L]])
  noCols   <- length(colNameHeaders)
  
  colNameList <- purrr::transpose(colNameList)
  colGroupBorders <- integer(0L)
  return(htmltools::withTags(tags$table(
    class = 'display',
    tags$thead(
      lapply(seq_len(noColDim - 1L), function(i){
        k <- 1L
        colSpan <- 1L
        headerRowHTML <- vector("list", length(colNameHeaders))
        currDimColNames <- colNameList[[i]]
        if(identical(noCols, 1L)){
          headerRowHTML[[k]] <- tags$th(colspan = 2L, class = "pivot-hdr-col",
                                        currDimColNames[[1]])
        }else{
          for(j in seq(2L, noCols)){
            if(j %in% colGroupBorders || !identical(currDimColNames[[j]], currDimColNames[[j - 1L]])){
              headerRowHTML[[k]] <- tags$th(colspan = colSpan, class = "pivot-hdr-col", currDimColNames[[j - 1L]])
              k <- k + 1L
              colSpan <- 1L
              if(!j %in% colGroupBorders){
                colGroupBorders <<- c(colGroupBorders, j)
              }
              if(identical(j, noCols)){
                headerRowHTML[[k]] <- tags$th(colspan = 1L, class = "pivot-hdr-col", currDimColNames[[j]])
              }
            }else if(identical(j, noCols)){
              headerRowHTML[[k]] <- tags$th(colspan = colSpan + 1L, class = "pivot-hdr-col",
                                            currDimColNames[[j]])
            }else{
              colSpan <- colSpan + 1L
            }
          }
        }
        if(i == 1L){
          return(tags$tr(c(lapply(rowHeaders, tags$th, rowspan = noColDim), 
                           headerRowHTML[seq_len(k)])))
        }
        return(tags$tr(headerRowHTML[seq_len(k)]))
      }),
      tags$tr(lapply(if(identical(noColDim, 1L)) 
        c(rowHeaders, colNameList[[noColDim]]) else colNameList[[noColDim]], tags$th))
    )
  )))
}
sanitizeFn <- function(filename) {
  # DO NOT USE THIS TO GET REALLY SECURE FILENAMES!
  # IT IS NOT MEANT TO CATCH ALL EDGE CASES AND CAN BE BYPASSED EASILY!
  filename <- gsub("[/\\\\\\?%*:|\"<>]", "", filename)
  return(stringi::stri_trim_left(filename, pattern = "[^\\.]"))
}
genWidgetGroups <- function(widgetNames, widgetGroups, widgetTabName, aggregateWidgets = FALSE){
  newWidgetGroups <- NULL
  if(length(widgetGroups)){
    newWidgetGroups <- lapply(widgetGroups, function(widgetGroup){
      groupMemberIds <- match(widgetGroup$members, widgetNames)
      if(any(is.na(groupMemberIds))){
        stop(sprintf("The members: '%s' of the widget group: '%s' are not in the list of input widgets. Please fix the configuration and try again!",
                     paste(groupMemberIds[is.na(groupMemberIds)], collapse = "', '"), widgetGroup),
             call. = FALSE)
      }
      widgetNames <<- widgetNames[-groupMemberIds]
      return(widgetGroup)
    })
  }
  if(length(widgetNames)){
    return(c(list(list(name = widgetTabName, members = widgetNames, sameTab = aggregateWidgets)), 
             newWidgetGroups))
  }
  return(newWidgetGroups)
}
getTabs <- function(names, aliases, groups, idsToDisplay = NULL, widgetIds = NULL, 
                    scalarsTabName = "Scalars", mergeScalars = FALSE, widgetIdsMultiDim = integer(0L)){
  j              <- 1L
  tabs     <- vector("list", length(names))
  tabTitles<- vector("list", length(names))
  tabSheetMap <- vector("list", length(names))
  isAssigned     <- vector("logical", length(names))
  scalarAssigned <- FALSE
  widgetId     <- NULL
  if(is.null(idsToDisplay)){
    idsToDisplay <- seq_along(names)
  }
  for(i in idsToDisplay){
    if(isAssigned[i]){
      next
    }
    if(mergeScalars && i %in% widgetIds){
      if(i %in% widgetIdsMultiDim){
        tabTitles[[j]]   <-  aliases[i]
        tabSheetMap[[i]] <- j
        tabs[[j]]        <-  i
        j <- j + 1L
      }
      if(!scalarAssigned){
        scalarAssigned <- TRUE
        tabTitles[[j]] <-  scalarsTabName
        tabs[[j]]      <-  0L
        j <- j + 1L
      }
      next
    }
    if(length(groups)){
      groupId <- vapply(seq_along(groups), 
                        function(gId){ 
                          if(names[i] %in% groups[[gId]]$members)
                            return(gId)
                          else
                            return(NA_integer_)}, integer(1L), USE.NAMES = FALSE)
      if(any(!is.na(groupId))){
        groupId <- groupId[!is.na(groupId)]
        if(length(groupId) > 1L){
          warningMsgTmp <- sprintf("Dataset: '%s' appears in more than one group. Only the first group will be used.", 
                                   aliases[i])
          warning(warningMsgTmp)
          warningMsg <<- paste(warningMsg, warningMsgTmp, sep = "\n")
        }
        groupMemberIds      <- match(groups[[groupId]]$members, names)
        groupMemberIds      <- groupMemberIds[groupMemberIds %in% idsToDisplay]
        if(any(is.na(groupMemberIds))){
          warningMsgTmp <- sprintf("The table(s): '%s' that you specified in group: '%s' do not exist. Thus, they were ignored.", 
                                   paste(groups[[groupId]]$members[is.na(groupMemberIds)], collapse = "', '"),
                                   groups[[groupId]]$name)
          warning(warningMsgTmp)
          warningMsg <<- paste(warningMsg, warningMsgTmp, sep = "\n")
          groupMemberIds <- groupMemberIds[!is.na(groupMemberIds)]
        }
        tabs[[j]]      <-  groupMemberIds
        tabSheetMap[groupMemberIds] <- j
        if(isTRUE(groups[[groupId]][["sameTab"]])){
          for(groupMemberId in groupMemberIds){
            tabSheetMap[[groupMemberId]] <- tabSheetMap[[groupMemberId]]
          }
        }else{
          for(k in seq_along(groupMemberIds)){
            groupMemberId <- groupMemberIds[k]
            tabSheetMap[[groupMemberId]] <- c(tabSheetMap[[groupMemberId]], k)
          }
        }
        if(mergeScalars){
          groupScalarId <- match(scalarsFileName, names[groupMemberIds])
          if(!is.na(groupScalarId)){
            if(scalarAssigned){
              if(length(groupMemberIds) <= 1L){
                next
              }
              groupMemberIds <- groupMemberIds[-groupScalarId]
            }else{
              groupMemberIds[groupScalarId] <- 0L
              scalarAssigned <- TRUE
            }
          }
        }
        if(isTRUE(groups[[groupId]][["sameTab"]])){
          tabTitles[[j]] <-  groups[[groupId]]$name
        }else{
          tabTitles[[j]] <-  c(groups[[groupId]]$name, aliases[groupMemberIds])
        }
        isAssigned[groupMemberIds] <- TRUE 
        j <- j + 1L
        next
      }
    }
    sheetId <- i
    tabSheetMap[[sheetId]] <- j
    if(mergeScalars && identical(names[i], scalarsFileName)){
      if(scalarAssigned){
        next
      }
      sheetId <- 0L
      scalarAssigned <- TRUE
    }
    tabs[[j]]      <-  sheetId
    
    tabTitles[[j]] <-  aliases[[i]]
    tabSheetMap[sheetId] <- j
    j <- j + 1L
    next
  }
  return(list(tabs = tabs[!vapply(tabs, is.null, logical(1L), USE.NAMES = FALSE)],
              tabTitles = tabTitles[!vapply(tabTitles, is.null, logical(1L), USE.NAMES = FALSE)],
              tabSheetMap = tabSheetMap))
}

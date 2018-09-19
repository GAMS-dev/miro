#renderGraphOutput<-function(tool,id){
#  # Renders the placeholder for a graph that will be inserted at a later stage.
#  #
#  # Args:
#  #   tool: tool that shall be used for rendering the graph 
#  #   id:   id of the placeholder (div) created
#  #
#  # Returns:
#  #   placeholder for graph with type according to tool
#  
#  if(tool=='plotly'){
#    plotly::plotlyOutput(id)
#  }else{
#    stop(paste0("The tool you selected for: '",id,"' is not supported by the current version of GAMS WebUI."))
#  }
#}
'%+%' <- function(x, y){
  paste(x, y, sep = '')
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

getModelName <- function(modelName = NULL, isShinyProxy = FALSE, envVarName = NULL){
  # returns name of the model currently rendered
  # 
  # Args:
  # modelName:                  name of the GAMS model as defined externally (e.g. in development mode)
  # isShinyProxy:               boolean that specifies whether shiny proxy is used
  # envVarName:                 name of the environment variable that specifies model name in shiny proxy
  #
  # Returns:
  # string with model name or error  in case no model name could be retrieved
  
  if(isShinyProxy){
    # shiny proxy mode
    if(is.null(envVarName)){
      if(is.null(modelName)){
        stop("Model name could not be retrieved.", call. = FALSE)
      }
    }else{
      envName <- Sys.getenv(envVarName)
      if(length(envName)){
        modelName <- envName
      }else if(is.null(modelName)){
        stop("Model name could not be retrieved.", call. = FALSE)
      }
    }
  }else{
    # local mode
    args <- commandArgs(trailingOnly = TRUE)
    matches <- grepl("^-+modelName\\s?=\\s?", args, ignore.case = TRUE)
    if(any(matches)){
      modelName <- gsub("^-+modelName\\s?=\\s?", "", args[matches][1], ignore.case = TRUE)
    }else if(is.null(modelName)){
      stop("Model name could not be retrieved.", call. = FALSE)
    }
  }
  
  return(tolower(modelName))
}

# DEPRECATED
#get.config.dir <- function(modelName, modelDir){
#  # attempts to copy model specific configuration files into Conf folder
#  # 
#  # Args:
#  # modelName:                  name of the GAMS model
#  # modelDir:                   location of the GAMS model
#  #
#  # Returns:
#  # no return value
#  
#  if(missing(modelDir) || missing(modelName)){
#    stop("Please specify a GAMS model directory and model name.", call. = F)
#  }
#
#  # check whether modelname.txt is available
#  if(file.access("conf/modelname.txt", mode = 4)){
#    if(file.access(paste0(modelDir, "conf/config.json"), mode = 4)){
#      stop("File: 'config.json' could not be found or user has no read permissions.", call. = F)
#    }
#    if(file.access(paste0(modelDir, "conf/GMSIO_config.json"), mode = 4)){
#      stop("File: 'GMSIO_config.json' could not be found or user has no read permissions.", call. = F)
#    }
#  }else{
#    tryCatch({
#      suppressWarnings(
#        curr.model <- readLines("./conf/modelname.txt")
#      )
#    }, error = function(e){
#      stop(paste0("Could not read file: 'modelname.txt'. Error message: ", e), call. = F)
#    })
#    if(curr.model == modelName){
#      # config files already up to date
#      return(NULL)
#    }
#  }
#  
#  # current config files are not for correct model, so attempt to copy correct config files
#  if(all(file.copy(paste0(modelDir, "conf/", c("config.json", "GMSIO_config.json")), 
#                   paste0("./conf/", c("config.json", "GMSIO_config.json")), overwrite = T))){
#    # success, so update modelname.txt file
#    tryCatch({
#      write(modelName, file = "./conf/modelname.txt")
#    }, error = function(e){
#      stop("Failed to write to file: './conf/modelname.txt'. Please make sure you have write permissions.", call. = F)
#    })
#    return(NULL)
#  }else{
#    stop("Failed to copy config files. Please make sure you have write permissions.", call. = F)
#  }
#  
#}
get.input.to.import <- function(data, keywordsNoImport){
  # Retrieves input data which has to be loaded from an external source
  #
  # Args:
  # data:                       raw list of input data
  # keywordsNoImport:           list of keywords to that define that the input data is NOT to be imported
  #                             (note that only keywords on the first level in the JSON file will be considered)
  #
  # Returns:
  # list of sheets that have to be imported from an external source
  
  data.to.import <- list()
  # index variable as c() is slow
  j <- 1
  for(i in seq_along(data)){
    if(!any(tolower(names(data[[i]])) %in% tolower(keywordsNoImport))){
      data.to.import[[j]] <- data[[i]]
      names(data.to.import)[[j]] <- names(data)[[i]]
      j <- j + 1
    }
  }
  return(data.to.import)
}

getInputType <- function(data, keywordsType){
  # Retrieves input type from JSOn file based on keyword list
  #
  # Args:
  # data:                       raw list of input data
  # keywordsType:               list of return value/keyword pairs (example: "hot" = "columns")
  #
  # Returns:
  # return type corresponding to keyword found or error in case no keywords matched
  
  for(i in 1:length(keywordsType)){
    if(keywordsType[[i]] %in% names(data)){
      return(names(keywordsType)[[i]])
    }
  }
  stop("No valid input type found.", call. = F)
}

getDependenciesDropdown <- function(choices, modelIn, name = NULL, strictMode = TRUE){
  # Retrieves list of input sheets that dropdown menu depends on (whose data has to be loaded)
  #
  # Args:
  # choices:                    raw list of choices from JSON file
  # modelIn:                    JSON element with model input data (used to verify dependencies)
  # name:                       name of the dropdown menu
  # strictMode:                 throws an error instead of accepting possibly faulty user entries
  #
  # Returns:
  # list of sheet and column names that need to be loaded for dropdown menu to have all data required (all lower case).
  # list also contains singular elements without dependencies
  
  ddownDep          <- list()
  ddownDep$strings  <- list()
  ddownDep$fw       <- list()
  ddownDep$bw       <- list()
  # define indexing variable for strings as c() is slow
  k <- 1
  
  if(length(choices)){
    lapply(seq_along(choices), function(i){
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
      
      if(grepl("([^\\$]+\\$[^\\$]+)|(^\\$[^\\$]+)|([^\\$]+\\$$)", choices[[i]])){
        # find out if column has dependency defined and replace leading and ending signs
        forward.dep  <- grepl("^\\$", choices[[i]])
        backward.dep <- grepl("\\$$", choices[[i]])
        el.raw <- tolower(gsub("^\\$|\\$$", "", choices[[i]]))
        if(grepl("\\$", el.raw)){
          # split string into the layers/elements ("dataset_1$column_3" -> "dataset_1", "column_3")
          el <- strsplit(el.raw, "\\$")[[1]]
          # check if elements in el match with the structure of the considered input data.
          idx1 <- match(el[[1]], names(modelIn))[1]
          idx2 <- match(el[[2]], tolower(names(modelIn[[idx1]]$headers)))[1]
          if(!is.na(idx2)){
            # add another forward dependency
            if(forward.dep){
              j <- length(ddownDep$fw[[names(modelIn)[[idx1]]]]) + 1
              ddownDep$fw[[tolower(names(modelIn)[[idx1]])]][[j]] <<- names(modelIn[[idx1]]$headers)[[idx2]]
            }
            # add another backward dependency
            if(backward.dep){
              j <- length(ddownDep$bw[[names(modelIn)[[idx1]]]]) + 1
              ddownDep$bw[[tolower(names(modelIn)[[idx1]])]][[j]] <<- names(modelIn[[idx1]]$headers)[[idx2]]
            }
            # new element was added so increment counter
            if(!(forward.dep || backward.dep)){
              # neither forward nor backward dependency selected results in error or rendering as string
              if(strictMode){
                stop(paste0("Neither a forward nor a backward dependency was defined in: '", choices[[i]], "'. Make sure you define some type of dependency."), call. = F)
              }else{
                ddownDep$strings[[k]] <<- choices[[i]]
                k <<- k + 1
              }
            }
            # make sure that in case a reference is given, the underlying data is also part of the input data
          }else{
            if(identical(el[[1]], name)){
              # first index is element itself, thus it is a reference to a shared database
              if(length(el) > 1){
                ddownDep$shared <<- el[[2]]
              }else{
                ddownDep$shared <<- el[[1]]
              }
              return(ddownDep)
            }else if(!is.na(idx1) && identical(modelIn[[idx1]]$type, "dropdown") && length(el) > 1 && forward.dep){
              # dependency on another dropdown menu, so dont check header info
              j <- length(ddownDep$fw[[names(modelIn)[[idx1]]]]) + 1
              ddownDep$fw[[tolower(names(modelIn)[[idx1]])]][[j]] <<- strsplit(gsub("^\\$|\\$$", "", choices[[i]]), "\\$")[[1]][[2]]
            }else{
              if(strictMode){
                stop(paste0("The header: '", el[[2]], "' for input sheet: '", el[[1]], "' could not be found. Make sure you define a valid reference."), call. = F)
              }else{
                ddownDep$strings[[k]] <<- choices[[i]]
                k <<- k + 1
              }
            }
          }
        }else{
          # define identifier variable to check whether column exists
          col.found <- FALSE
          # only column was entered (no sheet name)
          # find all sheets with column names
          if(length(modelIn)){
            for(idx1 in seq_along(modelIn)){
              # return index if available
              idx2 <- match(el.raw, tolower(names(modelIn[[idx1]]$headers)))[1]
              if(!is.na(idx2)){
                # add another forward dependency
                if(forward.dep){
                  j <- length(ddownDep$fw[[names(modelIn)[[idx1]]]]) + 1
                  ddownDep$fw[[tolower(names(modelIn)[[idx1]])]][[j]] <<- names(modelIn[[idx1]]$headers)[[idx2]]
                }
                # add another backward dependency
                if(backward.dep){
                  j <- length(ddownDep$bw[[names(modelIn)[[idx1]]]]) + 1
                  ddownDep$bw[[tolower(names(modelIn)[[idx1]])]][[j]] <<- names(modelIn[[idx1]]$headers)[[idx2]]
                }
                # new element was added so increment counter
                if(forward.dep || backward.dep){
                  col.found <- TRUE
                }else{
                  # neither forward nor backward dependency selected results in error or rendering as string
                  if(strictMode){
                    stop(paste0("Neither a forward nor a backward dependency was defined in: '", choices[[i]], "'. Make sure you define some type of dependency."), call. = F)
                  }else{
                    ddownDep$strings[[k]] <<- choices[[i]]
                    k <<- k + 1
                  }
                }
              }
            }
            # no column was found with matching name (invalid reference)
            if(!col.found){
              if(strictMode){
                stop(paste0("A column named: '", el.raw, "' could not be found. Make sure you define a valid reference."), call. = F)
              }else{
                ddownDep$strings[[k]] <<- choices[[i]]
                k <<- k + 1
              }
            }
          }
        }
      }else{
        # element is a simple string or number (replace double dollars by single dollar)
        string <- gsub("\\$\\$", "\\$", choices[[i]])
        ddownDep$strings[[k]] <<- string
        k <<- k + 1
      }
    })
    if(length(ddownDep$strings)){
      ddownDep$strings <- unlist(ddownDep$strings, use.names = FALSE)
    }
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
  
  list.of.values <- c("min" = min, "max" = max, "def" = def, "step" = step)
  # check if any value of slider has dependency on external data
  if(any(grepl("\\(", list.of.values))){
    # evaluate slider values
    slider.dep <- lapply(list.of.values, function(el){
      if(grepl("\\(", el)){
        # split string in operator and operand part
        splitted <- strsplit(el, "\\(|\\)")[[1]]
        operator <- splitted[[1]]
        if(!operator %in% listOfOperators){
          stop(paste0("'", operator, "' is not a valid operator for sliders."), call. = F)
        }
        dep      <- splitted[[2]]
        # split string into the sheets/elements ("dataset_1$column_3" -> "dataset_1", "column_3")
        dep <- strsplit(dep, "\\$")[[1]]
        # make sure that in case a reference is given, the underlying data is also part of the input data
        idx1 <- match(tolower(dep[1]), names(modelIn))[1]
        idx2 <- match(tolower(dep[2]), tolower(names(modelIn[[idx1]]$headers)))[1]
        if(!is.na(idx2)){
          slider.value <- list()
          slider.value[[tolower(dep[[1]])]] <- names(modelIn[[idx1]]$headers)[[idx2]]
          slider.value[["$operator"]] <- names(listOfOperators)[[match(operator,listOfOperators)]]
          return(slider.value)
        }else{
          if(!is.na(idx1) && modelIn[[idx1]]$type == "daterange"){
            # dependency on daterange selector
            slider.value <- list()
            slider.value[[tolower(dep[[1]])]] <- "$daterange"
            slider.value[["$operator"]] <- names(listOfOperators)[[match(operator, listOfOperators)]]
            return(slider.value)
          }else if(!is.na(idx1) && modelIn[[idx1]]$type == "dropdown" && length(dep) > 1){
            # dependency on another dropdown menu
            slider.value <- list()
            slider.value[[tolower(dep[[1]])]] <- dep[2]
            slider.value[["$operator"]] <- names(listOfOperators)[[match(operator, listOfOperators)]]
            return(slider.value)
          }
          if(length(dep) > 1){
            stop(paste0("Invalid reference. The header: '", dep[[2]], "' specified for input sheet: '", dep[[1]], "' could not be found."), call. = F)
          }else{
            stop(paste0("Invalid reference. The reference: '", dep, "' specified could not be found."), call. = F)
          }
        }
      }else{
        if(!is.na(as.numeric(el))){
          return(as.numeric(el))
        }else{
          stop(paste0("'", el, "' is not a valid ", deparse(substitute(el))," value for a slider."), call. = F)
        }
      }
    })
    return(slider.dep)
  }else{
    return(NULL)
  }
}

#save.data.to.excel <- function(data, file.name){
#  # Saves list of dataframes to Excel
#  #
#  # Args:
#  #   data:                     list of dataframes that is to be saved
#  #   file.name:                name for the new Excel file
#  #
#  # Returns:
#  #   TRUE in case of success, else error message
#  
#  writexl::write_xlsx(data, paste0(file.name, ".xlsx"))
#  
#}

renderOutput <- function(data, type, dt.options = NULL, graph.options = NULL, map.options = NULL, pivot.options = NULL, custom.options = NULL,
                         height = NULL, roundPrecision = 2, static = FALSE){
  # Renders output sheets according to visualization options specified
  #
  # Args:
  #   data:                     dataframe containing output data to be visualized
  #   type:                     type of visualization chosen
  #   dt.options:               options specifed to customize datatable
  #   graph.options:            options specified to cusutomize graphs
  #   map.options:              options specified to cusutomize interactive maps
  #   pivot.options:            options specified to customize pivot table
  #   custom.options:           options specified for custom renderer
  #   height:                   height of output object  
  #   roundPrecision:           number of decimal places data should be rounded to
  #   static:                   boolean which specifies whether return value is static DT object or renderDT object used 
  #                             for reactive programming
  #
  # Returns:
  #   Rendered output data
  
  switch(type,
         pivot = {
           return(renderPivot(data, options = pivot.options, height = height, roundPrecision = roundPrecision, static = static))
         },
         datatable = {
           return(renderDTable(data, options = dt.options, height = height, roundPrecision = roundPrecision))
         },
         dtGraph = {
           return(tagList(
             column(6, renderDTable(data, options = dt.options, height = height, roundPrecision = roundPrecision)),
             column(6, renderGraph(data,graph.options, height = height))
           ))
         },
         graph = {
           return(renderGraph(data, graph.options, height = height))
         }
  )
  tryCatch({
    custom.renderer <- match.fun(paste0("render", toupper(substr(type, 1, 1)), tolower(substr(type, 2, nchar(type)))))
    }, error = function(e){
      stop(paste0("A custom renderer function: '", type, "' was not found. Please make sure you first define such a function."), call. = F)
    })
  tryCatch({
    return(custom.renderer(data, options = custom.options))
  }, error = function(e){
    stop(paste0("An error occurred while using the custom renderer: '", type, "'. Error message: ", conditionMessage(e)), call. = F)
  })
    
}

verify.input <- function(data, headers){
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
      if(tolower(headers[[i]]$type) %in% c("scalar", "parameter") && class(data[[i]]) != "numeric"){
        return(FALSE)
      }
      if(tolower(headers[[i]]$type) %in% c("acronym", "set") && !(class(data[[i]]) %in% c("factor", "character", "numeric"))){
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
  escaped.string <- htmltools::htmlEscape(string)
  # replace \n with <br> tag
  escaped.string <- gsub("\\n", "<br>", escaped.string)
  
  return(escaped.string)
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
add.css.dim <- function(x, y){
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
  inputRawNames    <- gsub(prefixPar, "", inputNames)
  
  return(list(inputRawNames, inputRawNames[isGMSPar]))
}
virtualActionButton <- function(...){
  o <- structure(sum(...), class = "shinyActionButtonValue")
  invisible(o)
}
showErrorMsg <- function(title, errMsg){
  stopifnot(is.character(title), length(title) == 1)
  if(!is.null(errMsg)){
    stopifnot(is.character(errMsg), length(errMsg) == 1)
  }
  
  if(!is.null(errMsg)){
    showModal(modalDialog(
      title = title, HTML(addHtmlLineBreaks(errMsg))
    ))
    return(invisible(NULL))
  }
  return(invisible(1))
}
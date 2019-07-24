DataInstance <- R6Class("DataInstance", public = list(
  initialize = function(datasetNames){
    stopifnot(is.character(datasetNames), 
              length(datasetNames) > 0L)
    private$datasetNames <- datasetNames
  },
  push = function(datasetName, data){
    stopifnot(is.character(datasetName), 
              identical(length(datasetName), 1L),
              datasetName %in% private$datasetNames,
              inherits(data, "data.frame"))
    private$data[[datasetName]] <- data
  },
  add = function(datasetNames, data){
    stopifnot(identical(length(datasetNames), length(data)))
    for(i in seq_along(datasetNames)){
      self$push(datasetNames[[i]], data[[i]])
    }
  },
  get = function(datasetName = NULL){
    if(length(datasetName)){
      stopifnot(is.character(datasetName), identical(datasetName, 1L))
      if(length(private$data) && datasetName %in% names(private$data)){
        return(private$data[[datasetName]])
      }
      stop("Dataset not found.", call. = FALSE)
    }
    return(private$data)
  },
  addFilePaths = function(filePaths){
    if(is.character(filePaths) && length(filePaths)){
      private$filePaths <- c(private$filePaths, filePaths)
    }
    return(self)
  },
  writeCSV = function(filePath, datasetName = NULL, delim = ","){
    stopifnot(is.character(filePath), identical(length(filePath), 1L))
    if(length(datasetName)){
      stopifnot(is.character(datasetName), identical(datasetName, 1L))
      if(length(private$data) && datasetName %in% names(private$data)){
        return(self)
      }
      idsToWrite <- match(datasetName, names(private$data))
    }else{
      idsToWrite <- seq_along(private$data)
    }
    errMsg <- NULL
    for(id in idsToWrite){
      fileName <- file.path(filePath, paste0(names(private$data)[[id]], ".csv"))
      tryCatch({
        write_delim(private$data[[id]], fileName, 
                    delim = delim, na = "")
      }, error = function(e) {
        flog.error("Error writing csv file: '%s'. Error message: '%s'.", fileName, e)
        errMsg <<- paste(errMsg, sprintf(lang$errMsg$GAMSInput$writeCsv, fileName), sep = "\n")
      })
    }
    if(length(errMsg)){
      stop(errMsg, call. = FALSE)
    }
    private$filePaths <- c(private$filePaths, paste0(filePath, names(private$data)[idsToWrite], ".csv"))
    return(self)
  },
  writeGDX = function(fileName, squeezeZeros = c('y', 'n', 'e')){
    stopifnot(is.character(fileName), identical(length(fileName), 1L))
    squeezeZeros <- match.arg(squeezeZeros)
    gdxio$wgdx(fileName, private$data, squeezeZeros)
    private$filePaths <- c(private$filePaths, fileName)
    return(self)
  },
  compress = function(fileName){
    stopifnot(is.character(fileName), identical(length(fileName), 1L))
    if(!length(private$filePaths)){
      stop("Nothing to compress.", call. = FALSE)
    }
    zipr(fileName, private$filePaths, recurse = FALSE, compression_level = 9L)
    return(self)
  }
), private = list(
  data = list(),
  filePaths = character(0L),
  datasetNames = character(0L)
))
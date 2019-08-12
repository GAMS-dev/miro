DataInstance <- R6Class("DataInstance", public = list(
  initialize = function(datasetNames = character(0L)){
    if(!length(datasetNames)){
      private$datasetNames <- character(0L)
    }else{
      stopifnot(is.character(datasetNames))
      private$datasetNames <- datasetNames
    }
    return(invisible(self))
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
    return(invisible(self))
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
    for(id in idsToWrite){
      fileName <- file.path(filePath, 
                            paste0(names(private$data)[[id]], 
                                   ".csv"))
      write_delim(private$data[[id]], fileName, 
                  delim = delim, na = "")
    }
    private$filePaths <- c(private$filePaths, 
                           paste0(filePath, 
                                  names(private$data)[idsToWrite], 
                                  ".csv"))
    return(invisible(self))
  },
  writeGDX = function(fileName, squeezeZeros = c('y', 'n', 'e')){
    stopifnot(is.character(fileName), identical(length(fileName), 1L))
    squeezeZeros <- match.arg(squeezeZeros)
    gdxio$wgdx(fileName, private$data, squeezeZeros)
    private$filePaths <- c(private$filePaths, fileName)
    return(invisible(self))
  },
  compress = function(fileName = NULL, recurse = FALSE){
    if(!is.null(fileName)){
      stopifnot(is.character(fileName), identical(length(fileName), 1L))
    }else{
      fileName <- tempfile(fileext = ".zip")
    }
    if(!length(private$filePaths)){
      stop("Nothing to compress.", call. = FALSE)
    }
    zipr(fileName, private$filePaths, recurse = recurse, compression_level = 9L)
    return(invisible(fileName))
  }
), private = list(
  data = list(),
  filePaths = character(0L),
  datasetNames = character(0L)
))
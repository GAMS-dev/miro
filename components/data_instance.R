DataInstance <- R6Class("DataInstance", public = list(
  initialize = function(datasetNames = character(0L), 
                        fileExchange = c("csv", "gdx"), 
                        gdxio = NULL,
                        csvDelim = ",",
                        sortedNames = character(0L)){
    if(!length(datasetNames)){
      private$datasetNames <- character(0L)
    }else{
      stopifnot(is.character(datasetNames))
      private$datasetNames <- datasetNames
    }
    if(length(gdxio)){
      private$gdxio <- gdxio
    }
    if(is.character(sortedNames) && length(sortedNames)){
      private$sortedNames <- sortedNames
    }
    private$fileExchange <- match.arg(fileExchange)
    private$csvDelim     <- csvDelim
    return(invisible(self))
  },
  push = function(datasetName, data){
    stopifnot(is.character(datasetName), 
              identical(length(datasetName), 1L),
              inherits(data, "data.frame"))
    if(!datasetName %in% private$datasetNames){
      private$datasetNames <- c(private$datasetNames, datasetName)
    }
    private$data[[datasetName]] <- data
  },
  addInexFile = function(workDir, modelDataFiles){
    if(!length(modelDataFiles))
      return(NULL)
    stopifnot(is.character(modelDataFiles), is.character(workDir), 
              identical(length(workDir), 1L))
    
    inexFileName <- file.path(workDir, "_inex_file_")
    
    jsonlite::write_json(list(files = modelDataFiles, type = "include"), 
                         inexFileName, auto_unbox = TRUE)
    
    return(inexFileName)
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
  writeDisk = function(filePath, datasetName = NULL, fileName = NULL, ...){
    stopifnot(is.character(filePath), identical(length(filePath), 1L))
    if(length(datasetName)){
      stopifnot(is.character(datasetName), identical(datasetName, 1L))
      if(length(private$data) && datasetName %in% names(private$data)){
        return(self)
      }
      idsToWrite <- match(datasetName, names(private$data))
    }else if(length(private$sortedNames)){
      idsToWrite <- order(match(names(private$data), private$sortedNames))
    }else{
      idsToWrite <- seq_along(private$data)
    }
    if(private$fileExchange == "csv")
      return(private$writeCSV(filePath, idsToWrite, ...))
    
    return(private$writeGDX(file.path(filePath, fileName), idsToWrite, ...))
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
  gdxio = NULL,
  filePaths = character(0L),
  datasetNames = character(0L),
  fileExchange = character(1L),
  csvDelim = character(1L),
  sortedNames = character(0L),
  writeGDX = function(filePath, idsToWrite, squeezeZeros = c('y', 'n', 'e')){
    squeezeZeros <- match.arg(squeezeZeros)
    private$gdxio$wgdx(filePath, private$data[idsToWrite], squeezeZeros)
    private$filePaths <- c(private$filePaths, filePath)
    return(invisible(self))
  },
  writeCSV = function(filePath, idsToWrite){
    for(id in idsToWrite){
      fileName <- file.path(filePath, 
                            paste0(names(private$data)[[id]], 
                                   ".csv"))
      write_delim(private$data[[id]], fileName, 
                  delim = private$csvDelim, na = "")
    }
    private$filePaths <- c(private$filePaths, 
                           paste0(filePath, 
                                  names(private$data)[idsToWrite], 
                                  ".csv"))
    return(invisible(self))
  }
))
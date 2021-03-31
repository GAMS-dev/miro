DataInstance <- R6Class("DataInstance", public = list(
  initialize = function(datasetNames = character(0L), 
                        fileExchange = c("csv", "gdx"), 
                        gdxio = NULL,
                        csvDelim = ",",
                        sortedNames = character(0L),
                        activeScen = NULL,
                        attachments = NULL,
                        views = NULL){
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
    private$activeScen   <- activeScen
    private$attachments  <- attachments
    private$views        <- views
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
  addDirPaths = function(dirPaths){
    if(is.character(dirPaths) && length(dirPaths)){
      private$dirPaths <- c(private$dirPaths, dirPaths)
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
  copyMiroWs = function(wsPath, clArgsDf){
    miroMetaDir <- file.path(wsPath, "_miro_ws_")
    if(file.exists(miroMetaDir) &&
       !identical(unlink(miroMetaDir, recursive = TRUE, force = TRUE), 0L)){
      stop(sprintf("Could not remove (temporary) directory: %s", miroMetaDir), call. = FALSE)
    }
    if(!dir.create(miroMetaDir)){
      stop(sprintf("Could not create (temporary) directory: %s", miroMetaDir), call. = FALSE)
    }
    generateMiroScenMeta(miroMetaDir, private$activeScen$getMetadata(),
                         private$attachments, private$views,
                         scenId = 1L, clArgs = clArgsDf)
    self$addDirPaths(miroMetaDir)
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
    if(length(private$dirPaths)){
      if(any(duplicated(dirname(private$dirPaths)))){
        stop("Multiple directories with different root currently not supported!", call. = FALSE)
      }
      zipr_append(fileName, private$dirPaths, root = dirname(private$dirPaths[[1]]),
                  recurse = TRUE, compression_level = 9L)
    }
    return(invisible(fileName))
  }
), private = list(
  data = list(),
  gdxio = NULL,
  filePaths = character(0L),
  dirPaths = character(0L),
  datasetNames = character(0L),
  fileExchange = character(1L),
  csvDelim = character(1L),
  sortedNames = character(0L),
  activeScen = NULL,
  attachments = NULL,
  views = NULL,
  writeGDX = function(filePath, idsToWrite, squeezeZeros = c("n", "y", "e")){
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
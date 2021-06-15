HcubeBuilder <- R6Class("HcubeBuilder", public = list(
  initialize = function(dataHashes){
    hashesToOrder <- startsWith(names(dataHashes), "__")
    scenHashOrder <- order(names(dataHashes)[hashesToOrder])
    allParValCombinations <- c(dataHashes[!hashesToOrder],
                               dataHashes[hashesToOrder][scenHashOrder])
    private$colsNeedSplit <- vector("logical", length(dataHashes))
    names(private$colsNeedSplit) <- names(dataHashes)
    private$isDynamicCol <- vector("logical", length(dataHashes))
    names(private$isDynamicCol) <- names(dataHashes)
    private$dataHashes <- dataHashes
    private$dataRaw <- dataHashes
    if(scalarsFileName %in% names(ioConfig$modelIn)){
      private$scalarTypes <- setNames(ioConfig$modelIn[[scalarsFileName]]$symtypes,
                                      ioConfig$modelIn[[scalarsFileName]]$symnames)
    }
    return(invisible(self))
  },
  setDataHashes = function(dataHashes){
    for(dsId in names(dataHashes)){
      private$dataHashes[[dsId]] <- dataHashes[[dsId]]
    }
    return(invisible(self))
  },
  push = function(datasetName, data, ddChoices = NULL, allCombinations = FALSE){
    if(datasetName %in% ioConfig$DDPar){
      dsPrefix <- paste0("--", substring(datasetName, 9L), "= ")
      dsId <- paste0("__cl_", datasetName)
      isClArg <- TRUE
    }else if(datasetName %in% ioConfig$GMSOpt){
      dsPrefix <- paste0(substring(datasetName, 9L), "= ")
      dsId <- paste0("__cl_", datasetName)
      isClArg <- TRUE
    }else{
      dsPrefix <- paste0("--HCUBE_SCALARV_", datasetName, "= ")
      dsId <- datasetName
      isClArg <- FALSE
    }
    private$isDynamicCol[[dsId]] <- TRUE
    private$dataRaw[[dsId]] <- data
    if(length(ddChoices)){
      if(!isClArg && length(names(ddChoices))){
        if(identical(ioConfig$modelIn[[datasetName]]$dropdown$clearValue, TRUE)){
          private$dataHashes[[dsId]] <- paste0("--HCUBE_SCALART_", datasetName, "= ",
                                               escapeGAMSCL(names(ddChoices)[match(data, ddChoices)]))
          return(invisible(self))
        }
        private$colsNeedSplit[[dsId]] <- TRUE
        private$dataHashes[[dsId]] <- paste0(dsPrefix, escapeGAMSCL(data),
                                             '|"""|--HCUBE_SCALART_', datasetName, "= ",
                                             escapeGAMSCL(names(ddChoices)[match(data, ddChoices)]))
        return(invisible(self))
      }
      private$dataHashes[[dsId]] <- paste0(dsPrefix, escapeGAMSCL(data))
      return(invisible(self))
    }
    private$dataHashes[[dsId]] <- paste0(dsPrefix, data)
    return(invisible(self))
  },
  generateScenHashes = function(){
    allParValCombinations <- do.call("expand.grid", 
                                     c(unname(private$dataHashes),
                                       stringsAsFactors = FALSE))
    private$parValCombinations <- do.call(function(...) Map(list,...),
                                          allParValCombinations[which(private$isDynamicCol)])
    for(colIdx in which(private$colsNeedSplit)){
      allParValCombinations[[colIdx]] <- gsub('|"""|', " ", allParValCombinations[[colIdx]],
                                              fixed = TRUE)
    }
    names(private$parValCombinations) <- vapply(unite(allParValCombinations, "val",
                                                      seq_along(allParValCombinations),
                                                      remove = TRUE, sep = " ")[[1]],
                                                digest::digest, character(1L), algo = "sha256", 
                                                serialize = FALSE, USE.NAMES = FALSE)
    return(names(private$parValCombinations))
  },
  getNoScen = function(){
    return(length(private$parValCombinations))
  },
  getHcubeScalars = function(){
    allParValCombinationsRaw <- do.call("expand.grid", 
                                        c(unname(private$dataRaw[which(private$isDynamicCol)]),
                                          stringsAsFactors = FALSE))
    names(allParValCombinationsRaw) <- names(private$dataRaw)[which(private$isDynamicCol)]
    return(allParValCombinationsRaw %>%
             bind_cols(`_hash` = names(private$parValCombinations)) %>%
             mutate_if(~ !is.character(.), as.character) %>%
             pivot_longer(!`_hash`,
                          names_to = "scalar", values_to = "value"))
  },
  writeHcubeFile = function(workDir){
    filePath <- file.path(workDir, "hcube.json")
    write_json(list(jobs = lapply(names(private$parValCombinations), function(scenId){
      return(list(id = scenId,
                  arguments = unlist(
                    strsplit(unlist(private$parValCombinations[[scenId]],
                                    use.names = FALSE),
                             '|"""|', fixed = TRUE), use.names = FALSE)))
    })), filePath, auto_unbox = TRUE)
    return(filePath)
  }
), private = list(
  parValCombinations = NULL,
  dataRaw = NULL,
  dataHashes = NULL,
  isDynamicCol = NULL,
  colsNeedSplit = NULL
))
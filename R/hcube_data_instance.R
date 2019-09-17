HcubeDataInstance <- R6Class("HcubeDataInstance", public = list(
  initialize = function(remote){
    if(remote){
      private$json <- TRUE
    }
    return(invisible(self))
  },
  genGmsString = function(par, val, modelName){
    allParValCombinations <- do.call("expand.grid", 
                                     c(val, 
                                       stringsAsFactors = FALSE))
    if(private$json){
      private$parValCombinations <- lapply(seq_len(nrow(allParValCombinations)), 
                                         function(row){
                                           paste(par, allParValCombinations[row, ], sep = "")
                                         })
      allParValCombinations <- vapply(private$parValCombinations, paste, character(1L),
                                      USE.NAMES = FALSE, collapse = " ")
    }else{
      allParValCombinations <- vapply(
        seq_len(nrow(allParValCombinations)), function(row){
          paste(par, allParValCombinations[row, ], sep = "", collapse = " ")
        }, character(1L), USE.NAMES = FALSE)
    }
    return(trimws(allParValCombinations))
  },
  pushJobIDs = function(jobIDs){
    if(private$json)
      private$jobIDs <- jobIDs
    return(jobIDs)
  },
  subsetJobIDs = function(jobIdx){
    if(!private$json)
      return(invisible(self))
    private$jobIDs     <- private$jobIDs[jobIdx]
    parValCombinations <- private$parValCombinations[jobIdx]
    return(invisible(self))
  },
  writeHcube = function(workDir){
    if(!identical(length(private$jobIDs), length(private$parValCombinations))){
      stop("", call. = FALSE)
    }
    
    filePath <- paste0(workDir, "hcube.json")
    write_json(list(jobs = lapply(seq_along(private$jobIDs), function(i){
      return(list(id = private$jobIDs[[i]], arguments = private$parValCombinations[[i]]))
    })), filePath, auto_unbox = TRUE)
    
    return(filePath)
  }
), private = list(
  parValCombinations = NULL,
  jobIDs = NULL,
  json   = FALSE
))
HcubeDataInstance <- R6Class("HcubeDataInstance", public = list(
  initialize = function(modelGmsName) {
    stopifnot(is.character(modelGmsName), length(modelGmsName) == 1L)
    private$modelGmsName <- modelGmsName
    return(invisible(self))
  },
  genGmsString = function(val, modelName) {
    allParValCombinations <- do.call(
      "expand.grid",
      c(val,
        stringsAsFactors = FALSE
      )
    )
    private$parValCombinations <- lapply(
      seq_len(nrow(allParValCombinations)),
      function(row) {
        as.character(allParValCombinations[row, ])
      }
    )
    allParValCombinations <- vapply(private$parValCombinations, paste, character(1L),
      USE.NAMES = FALSE, collapse = " "
    )
    return(trimws(allParValCombinations))
  },
  pushJobIDs = function(jobIDs) {
    private$jobIDs <- jobIDs
    return(jobIDs)
  },
  getJobIDs = function() {
    return(private$jobIDs)
  },
  getNoJobs = function() {
    return(length(private$jobIDs))
  },
  subsetJobIDs = function(idsSolved) {
    subsetIds <- !private$jobIDs %in% idsSolved
    private$jobIDs <- private$jobIDs[subsetIds]
    private$parValCombinations <- private$parValCombinations[subsetIds]
    return(invisible(self))
  },
  writeHcubeFile = function(workDir) {
    if (!identical(length(private$jobIDs), length(private$parValCombinations))) {
      stop("", call. = FALSE)
    }
    filePath <- file.path(workDir, "hcube.json")
    write_json(list(jobs = lapply(seq_along(private$jobIDs), function(i) {
      parValCombinations <- private$parValCombinations[[i]]
      parValCombinations <- parValCombinations[!startsWith(parValCombinations, "--HCUBE_STATIC_")]
      parValCombinations <- unlist(strsplit(parValCombinations, '|"""|', fixed = TRUE),
        use.names = FALSE
      )

      return(list(id = private$jobIDs[[i]], arguments = parValCombinations))
    }), model_gms_name = private$modelGmsName), filePath, auto_unbox = TRUE)
    return(filePath)
  }
), private = list(
  parValCombinations = NULL,
  modelGmsName = NULL,
  jobIDs = NULL
))

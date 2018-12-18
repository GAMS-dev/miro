
output$btBatchAll <- downloadHandler(
  filename = function() {
    tolower(modelName) %+% ".zip"
  },
  content = function(file) {
    # solve all scenarios in batch run
    prog <- shiny::Progress$new()
    on.exit(prog$close())
    prog$set(message = lang$nav$dialogBatch$waitDialog$title, value = 0)
    updateProgress <- function(incAmount, detail = NULL) {
      prog$inc(amount = incAmount, detail = detail)
    }
    
    # BEGIN EPIGRIDS specific
    workDirBatch <- workDir %+% "batch"
    unlink(workDirBatch, recursive = TRUE, force = TRUE)
    dir.create(workDirBatch, showWarnings = FALSE, recursive = TRUE)
    homeDir <- getwd()
    setwd(workDirBatch)
    on.exit(setwd(homeDir), add = TRUE)
    on.exit(unlink(workDirBatch), add = TRUE)
    
    writeLines(scenGmsPar, workDirBatch %+% .Platform$file.sep %+% tolower(modelName) %+% ".gmsb")

    # Copy files that are needed to solve model
    file.copy(paste0(homeDir, .Platform$file.sep, modelDir), workDirBatch, recursive = TRUE)
    file.copy(paste0(homeDir, .Platform$file.sep, modelDir) %+% "batch_submission.gms", workDirBatch)
    do.call(file.remove, list(list.files(paste0(homeDir, .Platform$file.sep, modelDir), pattern = "\\.gmsconf$", full.names = TRUE, recursive = TRUE)))
    
    updateProgress(incAmount = 1, detail = lang$nav$dialogBatch$waitDialog$desc)
    removeModal()
    zip(file, list.files(recursive = TRUE), compression_level = 6)
    # END EPIGRIDS specific
  },
  contentType = "application/zip")

output$btBatchNew <- downloadHandler(
  filename = function() {
    tolower(modelName) %+% ".zip"
  },
  content = function(file) {
    # solve only scenarios that do not yet exist
    prog <- shiny::Progress$new()
    on.exit(prog$close())
    prog$set(message = lang$nav$dialogBatch$waitDialog$title, value = 0)
    updateProgress <- function(incAmount, detail = NULL) {
      prog$inc(amount = incAmount, detail = detail)
    }
    # BEGIN EPIGRIDS specific
    workDirBatch <- workDir %+% "batch"
    unlink(workDirBatch, recursive = TRUE, force = TRUE)
    dir.create(workDirBatch, showWarnings = FALSE, recursive = TRUE)
    homeDir <- getwd()
    setwd(workDirBatch)
    on.exit(setwd(homeDir), add = TRUE)
    on.exit(unlink(workDirBatch), add = TRUE)
    
    writeLines(scenGmsPar[idxDiff], workDirBatch %+% .Platform$file.sep %+% tolower(modelName) %+% ".gmsb")
    
    # Copy files that are needed to solve model
    file.copy(paste0(homeDir, .Platform$file.sep, modelDir), workDirBatch, recursive = TRUE)
    file.copy(paste0(homeDir, .Platform$file.sep, modelDir) %+% "batch_submission.gms", workDirBatch)
    do.call(file.remove, list(list.files(paste0(homeDir, .Platform$file.sep, modelDir), pattern = "\\.gmsconf$", full.names = TRUE, recursive = TRUE)))
    updateProgress(incAmount = 1, detail = lang$nav$dialogBatch$waitDialog$desc)
    removeModal()
    zip(file, list.files(recursive = TRUE), compression_level = 6)
    # END EPIGRIDS specific
  },
  contentType = "application/zip")
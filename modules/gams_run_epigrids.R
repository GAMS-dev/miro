
output$btBatchAll_dl <- downloadHandler(
  filename = function() {
    tolower(modelName) %+% ".zip"
  },
  content = function(file) {
    # solve all scenarios in batch run
    
    # BEGIN EPIGRIDS specific
    workDirBatch <- workDir %+% "batch"
    unlink(workDirBatch, recursive = TRUE, force = TRUE)
    dir.create(workDirBatch, showWarnings = FALSE, recursive = TRUE)
    homeDir <- getwd()
    setwd(workDirBatch)
    on.exit(setwd(homeDir), add = TRUE)
    on.exit(unlink(workDirBatch), add = TRUE)
    
    genBatchJobFolder(fromDir = paste0(currentModelDir, "..", .Platform$file.sep), 
                      modelDir = paste0(currentModelDir, "..", .Platform$file.sep),
                      toDir = workDirBatch, scenGmsPar = scenGmsPar)
    
    removeModal()
    zip(file, list.files(recursive = TRUE), compression_level = 6)
    # END EPIGRIDS specific
  },
  contentType = "application/zip")

output$btBatchNew_dl <- downloadHandler(
  filename = function() {
    tolower(modelName) %+% ".zip"
  },
  content = function(file) {
    # solve only scenarios that do not yet exist
    
    # BEGIN EPIGRIDS specific
    workDirBatch <- workDir %+% "batch"
    unlink(workDirBatch, recursive = TRUE, force = TRUE)
    dir.create(workDirBatch, showWarnings = FALSE, recursive = TRUE)
    homeDir <- getwd()
    setwd(workDirBatch)
    on.exit(setwd(homeDir), add = TRUE)
    on.exit(unlink(workDirBatch), add = TRUE)
    
    genBatchJobFolder(fromDir = paste0(currentModelDir, "..", .Platform$file.sep), 
                      modelDir = paste0(currentModelDir, "..", .Platform$file.sep),
                      toDir = workDirBatch, scenGmsPar = scenGmsPar[idxDiff])
    
    removeModal()
    zip(file, list.files(recursive = TRUE), compression_level = 6)
    # END EPIGRIDS specific
  },
  contentType = "application/zip")
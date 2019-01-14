
output$btHcubeAll_dl <- downloadHandler(
  filename = function() {
    tolower(modelName) %+% ".zip"
  },
  content = function(file) {
    # solve all scenarios in Hypercube run
    
    # BEGIN EPIGRIDS specific
    workDirHcube <- workDir %+% "hcube"
    unlink(workDirHcube, recursive = TRUE, force = TRUE)
    dir.create(workDirHcube, showWarnings = FALSE, recursive = TRUE)
    homeDir <- getwd()
    setwd(workDirHcube)
    on.exit(setwd(homeDir), add = TRUE)
    on.exit(unlink(workDirHcube), add = TRUE)
    
    genHcubeJobFolder(fromDir = paste0(currentModelDir, "..", .Platform$file.sep), 
                      modelDir = paste0(currentModelDir, "..", .Platform$file.sep),
                      toDir = workDirHcube, scenGmsPar = scenGmsPar)
    
    removeModal()
    zip(file, list.files(recursive = TRUE), compression_level = 6)
    # END EPIGRIDS specific
  },
  contentType = "application/zip")

output$btHcubeNew_dl <- downloadHandler(
  filename = function() {
    tolower(modelName) %+% ".zip"
  },
  content = function(file) {
    # solve only scenarios that do not yet exist
    
    # BEGIN EPIGRIDS specific
    workDirHcube <- workDir %+% "hcube"
    unlink(workDirHcube, recursive = TRUE, force = TRUE)
    dir.create(workDirHcube, showWarnings = FALSE, recursive = TRUE)
    homeDir <- getwd()
    setwd(workDirHcube)
    on.exit(setwd(homeDir), add = TRUE)
    on.exit(unlink(workDirHcube), add = TRUE)
    
    genHcubeJobFolder(fromDir = paste0(currentModelDir, "..", .Platform$file.sep), 
                      modelDir = paste0(currentModelDir, "..", .Platform$file.sep),
                      toDir = workDirHcube, scenGmsPar = scenGmsPar[idxDiff])
    
    removeModal()
    zip(file, list.files(recursive = TRUE), compression_level = 6)
    # END EPIGRIDS specific
  },
  contentType = "application/zip")
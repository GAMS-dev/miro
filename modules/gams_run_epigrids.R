filesToDownloadForBatchRun <- c("cases", "calc_PowerFlow.gms", "calc_Ybus.gms", "cost_objective.gms", 
                                "cost_objective_uc.gms", "extract_data.gms", "extract_data_uc.gms", 
                                "reactive_limits.gms", "save_domain_info.gms", "save_solution.gms", 
                                "save_solution_uc.gms", "webui_in.gms", "webui_out.gms", "batch_submission.gms", 
                                "webui.gms", "epigrids", "epigrids_uc", "ic_iv", "ic_polar", "ic_rect",
                                "iv_acopf", "polar_acopf", "polar_decoupled", "rect_acopf", "uc_dc", 
                                "uc_iv", "uc_polar", "uc_rect", "ybus_iv_acopf", "ybus_polar_acopf", 
                                "ybus_polar_decoupled", "ybus_rect_acopf", "DataUtilities")



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
    file.copy(paste0(homeDir, .Platform$file.sep, modelDir) %+% filesToDownloadForBatchRun, workDirBatch, recursive = TRUE)
    
    updateProgress(incAmount = 1, detail = lang$nav$dialogBatch$waitDialog$desc)
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
    file.copy(paste0(homeDir, .Platform$file.sep, modelDir) %+% filesToDownloadForBatchRun, workDirBatch, recursive = TRUE)
    updateProgress(incAmount = 1, detail = lang$nav$dialogBatch$waitDialog$desc)
    
    zip(file, list.files(recursive = TRUE), compression_level = 6)
    # END EPIGRIDS specific
  },
  contentType = "application/zip")
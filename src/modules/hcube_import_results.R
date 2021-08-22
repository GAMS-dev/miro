importHcJob <- function(filePath, jobMeta){
  outputAttachmentNames <- vapply(config$outputAttachments, "[[", character(1L),
                                  "filename", USE.NAMES = FALSE)
  if(config$activateModules$attachments && config$storeLogFilesDuration > 0L){
    if(any(c(config$activateModules$logFile, config$activateModules$lstFile)))
      outputAttachmentNames  <- c(outputAttachmentNames,
                                  paste0(modelNameRaw,
                                         c(if(config$activateModules$logFile) ".log",
                                           if(config$activateModules$lstFile) ".lst")))
    if(config$activateModules$miroLogFile)
      outputAttachmentNames <- c(outputAttachmentNames, config$miroLogFile)
  }
  prog <- Progress$new()
  if(tryCatch({
    hcubeResults <- HcubeResults$new(db, gdxio, filePath, outputAttachmentNames, jobMeta,
                                     includeTrc = config$saveTraceFile,
                                     filesMayInclude = c(paste0(modelNameRaw,
                                                                c(if(config$activateModules$logFile) ".log",
                                                                  if(config$activateModules$lstFile) ".lst")),
                                                         config$miroLogFile))
    FALSE
  },
  error_bad_format = function(e){
    flog.warn("Invalid Hypercube job archive file format: %s",
              filePath)
    showErrorMsg(lang$errMsg$hcubeImport$invalidJob$title,
                 lang$errMsg$hcubeImport$invalidJob$desc)
    return(TRUE)
  },
  error_invalid_results = function(e){
    flog.warn("Invalid Hypercube job archive. Error message: %s",
              conditionMessage(e))
    showErrorMsg(lang$errMsg$hcubeImport$invalidJob$title,
                 lang$errMsg$hcubeImport$invalidJob$desc)
    return(TRUE)
  },
  error = function(e){
    flog.error("Unexpected error while validating Hypercube job archive. Error message: %s",
               conditionMessage(e))
    showErrorMsg(lang$errMsg$hcubeImport$extract$title,
                 lang$errMsg$unknownError)
    return(TRUE)
  })){
    prog$close()
    return()
  }
  tryCatch({
    hcubeResults$extractArchive(session, workDir,
                                progressCallback = function(progress){
                                  prog$set(message = lang$progressBar$hcubeImport$title,
                                           detail = lang$progressBar$hcubeImport$zipExtract,
                                           value = progress)
                                },
                                callbackError = function(procStat, errMsg){
                                  flog.error("Problems extracting Hypercube job results archive. Return code: %s, stderr: %s",
                                             procStat, errMsg)
                                  showErrorMsg(lang$errMsg$hcubeImport$extract$title,
                                               lang$errMsg$hcubeImport$extract$desc)
                                  prog$close()
                                },
                                callbackSuccess = function(){
                                  if(tryCatch({
                                    hcubeResults$writeToDb(progressCallback = function(progress){
                                      prog$set(message = lang$progressBar$hcubeImport$title,
                                               detail = lang$progressBar$hcubeImport$dbUpload,
                                               value = progress)
                                    })
                                    FALSE
                                  }, error_db = function(e){
                                    showErrorMsg(lang$errMsg$hcubeImport$dbUpload$title,
                                                 lang$errMsg$hcubeImport$dbUpload$desc)
                                    return(TRUE)
                                  }, error = function(e){
                                    flog.error("Unexpected error while writing Hypercube job to database. Error message: %s",
                                               conditionMessage(e))
                                    showErrorMsg(lang$errMsg$hcubeImport$dbUpload$title,
                                                 lang$errMsg$unknownError)
                                    return(TRUE)
                                  })){
                                    prog$close()
                                    return()
                                  }
                                  prog$close()
                                  tryCatch({
                                    worker$updateJobStatus(JOBSTATUSMAP[["imported"]],
                                                           jobMeta[["_jid"]][1])
                                  }, error = function(e){
                                    flog.warn("Problems updating job status. Error message: '%s'.",
                                              conditionMessage(e))
                                  })
                                  rv$jobListPanel <- rv$jobListPanel + 1L
                                })
  }, error_extraction = function(e){
    flog.warn("Couldn't extract Hypercube job archive. Error message: %s",
             conditionMessage(e))
    showErrorMsg(lang$errMsg$hcubeImport$extract$title,
                 lang$errMsg$hcubeImport$extract$desc)
  })
}

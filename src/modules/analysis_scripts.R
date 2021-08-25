scriptsPath <- file.path(workDir, paste0("scripts_", modelName))
scriptOutput <- ScriptOutput$new(session, scriptsPath,
                                 config$scripts, lang$nav$scriptOutput$errMsg,
                                 gamsSysDir = gamsSysDir)

observeEvent(input$btAnalysisConfig, {
  hideEl(session, ".batch-load-content")
  showEl(session, ".batch-load-analysis-content")
  showEl(session, ".batch-load-analysis-footer", inline = TRUE)
})
if(length(config$scripts$base)){
  observeEvent(input$runScript, {
    scriptId <- suppressWarnings(as.integer(input$runScript))
    if(is.na(scriptId) || scriptId < 1 ||
       scriptId > length(config$scripts$base)){
      flog.error("A script with id: '%s' was attempted to be executed. However, this script does not exist. Looks like an attempt to tamper with the app!",
                 input$runScript)
      return()
    }
    scriptIdChar <- config$scripts$base[[scriptId]]$id
    if(scriptOutput$isRunning(scriptIdChar)){
      flog.debug("Button to interrupt script: '%s' clicked.", scriptId)
      scriptOutput$interrupt(scriptIdChar)
      hideEl(session, paste0("#scriptOutput_", scriptId, " .script-spinner"))
      hideEl(session, paste0("#scriptOutput_", scriptId, " .script-output"))
      showEl(session, paste0("#scriptOutput_", scriptId, " .out-no-data"))

      showElReplaceTxt(session, paste0("#scriptOutput_", scriptId, " .btn-run-script"),
                       lang$nav$scriptOutput$runButton)
      return()
    }
    flog.debug("Button to execute script: '%s' clicked.", scriptId)

    if(!dir.exists(scriptsPath)){
      if(dir.exists(file.path(currentModelDir, paste0("scripts_", modelName)))){
        if(!file.copy2(file.path(currentModelDir, paste0("scripts_", modelName)),
                       scriptsPath)){
          flog.error("Problems copying files from: '%s' to: '%s'.",
                     scriptsPath,
                     file.path(currentModelDir, paste0("scripts_", modelName)))
          hideEl(session, paste0("#scriptOutput_", scriptId, " .script-spinner"))
          hideEl(session, paste0("#scriptOutput_", scriptId, " .out-no-data"))
          return(scriptOutput$sendContent(lang$errMsg$unknownError, scriptId, isError = TRUE))
        }
      }else{
        flog.info("No 'scripts_%s' directory was found. Did you forget to include it in the model assembly file ('%s_files.txt')?",
                  modelName, modelName)
        hideEl(session, paste0("#scriptOutput_", scriptId, " .script-spinner"))
        showEl(session, paste0("#scriptOutput_", scriptId, " .out-no-data"))
        return(scriptOutput$sendContent(lang$nav$scriptOutput$errMsg$noScript, scriptId, isError = TRUE))
      }
    }
    showEl(session, paste0("#scriptOutput_", scriptId, " .script-spinner"))
    hideEl(session, paste0("#scriptOutput_", scriptId, " .script-output"))
    hideEl(session, paste0("#scriptOutput_", scriptId, " .out-no-data"))

    errMsg <- NULL

    tryCatch({
      scenData$loadSandbox(getInputDataFromSandbox(),
                           if(length(modelInFileNames)) modelInFileNames else character())
      gdxio$wgdx(file.path(workDir, paste0("scripts_", modelName), "data.gdx"),
                 scenData$get("sb", includeHiddenScalars = TRUE), squeezeZeros = 'n')
    }, error = function(e){
      flog.error("Problems writing gdx file for script: '%s'. Error message: '%s'.",
                 scriptId, conditionMessage(e))
      errMsg <<- sprintf(lang$errMsg$fileWrite$desc, "data.gdx")
      hideEl(session, paste0("#scriptOutput_", scriptId, " .script-spinner"))
      hideEl(session, paste0("#scriptOutput_", scriptId, " .out-no-data"))
      scriptOutput$sendContent(errMsg, scriptId, isError = TRUE)
    })
    if(!is.null(errMsg)){
      return()
    }
    tryCatch({
      scriptOutput$run(scriptId)
      showElReplaceTxt(session, paste0("#scriptOutput_", scriptId, " .btn-run-script"),
                       lang$nav$scriptOutput$interruptButton)
    }, error = function(e){
      flog.info("Script: '%s' crashed during startup. Error message: '%s'.",
                scriptId, conditionMessage(e))
      scriptOutput$sendContent(lang$nav$scriptOutput$errMsg$crash, scriptId,
                               hcube = FALSE, isError = TRUE)
    })
  })
  observeEvent(input$outputGenerated,{
    noOutputData <<- FALSE
  })
}
if(length(config$scripts$hcube)){
  observeEvent(input$btRunHcubeScript, {
    scriptId <- suppressWarnings(as.integer(input$selHcubeAnalysisScript))
    flog.debug("Button to execute Hypercube analysis script: '%s' clicked.", scriptId)

    if(is.na(scriptId) || scriptId < 1 || scriptId > length(config$scripts$hcube)){
      flog.error("A script with id: '%s' was attempted to be executed. However, this script does not exist. Looks like an attempt to tamper with the app!",
                 scriptId)
      showHideEl(session, "#analysisRunUnknownError", 6000L)
      return()
    }
    hcScriptIds <- vapply(config$scripts$hcube, "[[", character(1L), "id", USE.NAMES = FALSE)
    if(any(scriptOutput$getRunningScriptIds() %in% hcScriptIds)){
      flog.debug("A script is already running.")
      showHideEl(session, "#analysisRunScriptRunning", 6000L)
      return()
    }
    if(!dir.exists(scriptsPath)){
      if(dir.exists(file.path(currentModelDir, paste0("scripts_", modelName)))){
        if(!file.copy2(file.path(currentModelDir, paste0("scripts_", modelName)),
                       scriptsPath)){
          flog.error("Problems copying files from: '%s' to: '%s'.",
                     file.path(currentModelDir, paste0("scripts_", modelName)),
                     scriptsPath)
          hideModal(session, 6L)
          showHideEl(session, "#analysisRunUnknownError", 6000L)
          return()
        }
      }else{
        flog.info("No 'scripts_%s' directory was found. Did you forget to include it in '%s_files.txt'?",
                  modelName, modelName)
        hideModal(session, 6L)
        showHideEl(session, "#analysisRunUnknownError", 6000L)
        return()
      }
    }
    hideEl(session, ".batch-load-content")
    showEl(session, ".batch-load-script-content")

    prog <- Progress$new()
    on.exit(prog$close(), add = TRUE)
    prog$set(message = lang$nav$dialogHcube$waitDialog$title, value = 0)
    insertUI("#batchLoadAnalysisWrapper",
             ui = tags$div(id = "scriptOutput_hcube", class="script-wrapper",
                           tags$div(class = "out-no-data",
                                    lang$nav$outputScreen$boxResults$noData,
                                    style = "display:none"),
                           tags$div(class = "out-no-data script-error",
                                    style = "display:none"),
                           tags$div(class = "space"),
                           tags$iframe(class = "script-output",
                                       style = "height:800px;height:80vh;")),
             immediate = TRUE)
    if(tryCatch({
      batchLoader$genGdxFiles(sidsToLoad, scriptsPath,
                              gdxio, prog, genScenList = TRUE)
      FALSE
    }, error = function(e){
      flog.error("Problems writing gdx files for script: '%s'. Error message: '%s'.",
                 scriptId, conditionMessage(e))
      hideModal(session, 4L)
      showHideEl(session, "#analysisRunUnknownError", 4000L,
                 sprintf(lang$errMsg$fileWrite$desc, "data.gdx"))
      return(TRUE)
    })){
      return()
    }
    tryCatch({
      scriptOutput$run(scriptId, hcube = TRUE, inModal = TRUE)
    }, error = function(e){
      flog.info("Script: '%s' crashed during startup. Error message: '%s'.",
                scriptId, conditionMessage(e))
      hideModal(session, 4L)
      showHideEl(session, "#analysisRunUnknownError", 4000L,
                 sprintf(lang$nav$scriptOutput$errMsg$crash, "data.gdx"))
    })
  })
  output$btDownloadBatchLoadScript <- downloadHandler(
    config$scripts$hcube[[isolate(as.integer(input$selHcubeAnalysisScript))]]$outputFile,
    content = function(file){
      file.copy(file.path(scriptsPath,
                          config$scripts$hcube[[isolate(as.integer(input$selHcubeAnalysisScript))]]$outputFile),
                file)
    })
}

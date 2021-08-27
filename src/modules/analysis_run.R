# run the paver module (python)
paver <- NULL
traceFileDir <- file.path(workDir, "trace")
# paver solution files
paverFileDir <- file.path(workDir, "paver")

genPaverArgs <- function(traceFilenames, clArgs = NULL) {
  stopifnot(is.character(traceFilenames), length(traceFilenames) >= 1)
  extraClArgs <- NULL
  if (length(clArgs)) {
    stopifnot(is.character(clArgs))
    extraClArgs <- unlist(lapply(strsplit(clArgs, "( ?= ?)| "), function(el) {
      el[1] <- tolower(gsub("--", "", el[1], fixed = TRUE)[1L])
      if (el[1] %in% c(
        "zerogaptol", "nocheckinstanceattr",
        "numpts", "novirt", "refsolver", "nosortsolver",
        "bw", "ccreltol", "ccabstol", "ccfeastol",
        "ccopttol", "timeshift", "nodeshift", "mintime",
        "failtime", "failnodes", "gaptol", "evalgap",
        "filtertime", "filternodes", "timerelimpr",
        "boundrelimpr", "ignoredualbounds", "eval",
        "extendedprofiles", "optfileisrunname"
      )) {
        return(c(paste0("--", el[1]), el[-1]))
      } else {
        flog.info("Ignored unknown paver option: '%s'.", el[1])
        return(NULL)
      }
    }), use.names = FALSE)
    if (length(extraClArgs)) {
      flog.debug("Calling PAVER with extra arguments: '%s'.", paste(extraClArgs, collapse = " "))
    }
  }

  c(
    file.path(getwd(), "tools", "paver", "paver.py"),
    traceFilenames, if (length(extraClArgs)) extraClArgs, "--writehtml", paverFileDir, "--writeimg", paverFileDir, "--gmswebiter", gmswebiter
  )
}

observeEvent(input$btAnalysisConfig, {
  # if already tracefiles in tracefiledir show deletion warning
  hideEl(session, ".batch-load-content")
  showEl(session, ".batch-load-analysis-content")
  showEl(session, ".batch-load-analysis-footer", inline = TRUE)
  if (length(list.files(traceFileDir)) > 0) {
    showEl(session, "#deleteTrace")
  }
})

observeEvent(input$tabsetAnalysisMethod, {
  if (identical(input$tabsetAnalysisMethod, "tabsetAnalysisMethodPaver")) {
    hideEl(session, "#btRunHcubeScript")
    showEl(session, "#btRunPaver")
    return()
  }
  if (identical(input$tabsetAnalysisMethod, "tabsetAnalysisMethodScript")) {
    hideEl(session, "#btRunPaver")
    showEl(session, "#btRunHcubeScript")
    return()
  }
})

gmswebiter <- 0
observeEvent(input$btRunPaver, {
  flog.debug("Run paver button clicked.")
  req(input$selPaverAttribs)
  if ((!is.null(paver) && is.null(paver$get_exit_status())) ||
    (length(scriptOutput) && scriptOutput$isRunning())) {
    showHideEl(session, "#analysisRunScriptRunning", 6000L)
    return()
  }
  gmswebiter <<- gmswebiter + 1
  noErr <- TRUE
  scenToFetch <- batchLoadData[[1]] %in% sidsToLoad
  if (!any(scenToFetch)) {
    flog.warn("Paver was attempted to be started while no scenarios were selected.")
    showHideEl(session, "#analysisRunUnknownError", 6000L)
    return()
  }
  tryCatch(
    {
      exceedsMaxNoSolvers <- batchLoader$exceedsMaxNoSolvers(
        batchLoadData[scenToFetch, , drop = FALSE],
        input$selPaverAttribs, maxSolversPaver,
        isolate(input$paverExclAttrib)
      )
    },
    error = function(e) {
      noErr <<- FALSE
      flog.error(
        "Problems identifying whether maximum number of solvers for paver is exceeded Error message: '%s'.",
        conditionMessage(e)
      )
      showHideEl(session, "#analysisRunUnknownError", 6000L)
    }
  )
  if (!noErr) {
    return()
  }

  if (exceedsMaxNoSolvers) {
    showHideEl(session, "#configPaverMaxSolversErr", 4000L)
    return()
  } else {
    errMsg <- NULL
    paverDir <- file.path(workDir, "paver")
    paverClArgs <- isolate(input$paverClArgs)
    tryCatch(
      {
        if (dir.exists(traceFileDir)) {
          unlink(file.path(traceFileDir, "*"), recursive = TRUE, force = TRUE)
        } else {
          dir.create(traceFileDir, showWarnings = FALSE)
        }
        if (dir.exists(paverDir)) {
          unlink(file.path(paverDir, "*"), recursive = TRUE, force = TRUE)
          unlink(file.path(paverDir, "*.png"), recursive = TRUE, force = TRUE)
        }
        dir.create(paverDir, showWarnings = FALSE)
      },
      error = function(e) {
        flog.error(
          "Problems creating temporary folder where trace files will be stored. Error message: '%s'.",
          conditionMessage(e)
        )
        errMsg <<- sprintf(lang$errMsg$fileWrite$desc, "./trace")
      }
    )
    if (is.null(showErrorMsg(lang$errMsg$fileWrite$title, errMsg))) {
      return()
    }
    noErr <- TRUE
    tryCatch(
      batchLoader$genPaverTraceFiles(traceFileDir, exclTraceCols),
      error = function(e) {
        noErr <<- FALSE
        switch(conditionMessage(e),
          noTrc = {
            flog.info(
              "Unknown error exeuting Paver. Error message: '%s'.",
              conditionMessage(e)
            )
            showHideEl(session, "#paverRunNoTrc", 6000L)
          },
          {
            flog.error(
              "Unknown error exeuting Paver. Error message: '%s'.",
              conditionMessage(e)
            )
            showHideEl(session, "#analysisRunUnknownError", 6000L)
          }
        )
      }
    )
    if (!noErr) {
      return(invisible())
    }
    traceFiles <- list.files(traceFileDir, pattern = ".trc", full.names = TRUE)
  }
  addResourcePath("paver", paverDir)
  hideEl(session, "#scriptOutput_hcube")
  removeModal()
  if (is.null(paver) || !is.null(paver$get_exit_status())) {
    if (gmswebiter > 1) {
      removeTab("analysisResults", "analysisResults_2")
      removeTab("analysisResults", "analysisResults_3")
      removeTab("analysisResults", "analysisResults_4")
      removeTab("analysisResults", "analysisResults_5")
      removeTab("analysisResults", "analysisResults_6")
    }

    hideEl(session, "#paverFail")
    updateTabsetPanel(session, "analysisResults", selected = "analysisResults_1")
    output$paverResults <- renderUI(character())
    showEl(session, "#analysisLoad")
    hideEl(session, "#newPaverRunButton")
    enableEl(session, "#btAnalysisInterrupt")
    switchTab(session, "hcubeAna")

    errMsg <- NULL
    # run paver
    tryCatch(
      {
        if (isWindows()) {
          pyExec <- "python"
        } else {
          pyExec <- "python3"
        }
        paver <<- processx::process$new(pyExec,
          args = genPaverArgs(traceFiles, paverClArgs),
          windows_hide_window = TRUE,
          stdout = paste0(workDir, .Platform$file.sep, modelName, ".paverlog"),
          stderr = "|"
        )
      },
      error = function(e) {
        paver <<- NULL
        errMsg <<- lang$errMsg$paverExec$desc
        flog.error("Paver did not execute successfully. Error message: %s.", e)
      }
    )
    if (is.null(showErrorMsg(lang$errMsg$paverExec$title, errMsg))) {
      return(NULL)
    }
    paverStatus <<- reactivePoll2(5000, session, checkFunc = function() {
      paver$get_exit_status()
    }, valueFunc = function() {
      paver$get_exit_status()
    })
    paverStatusObs <- paverStatus$obs
    paverStatus <- paverStatus$re
    # include html files (seperate tabs)
    output$paverResults <- renderUI({
      if (is.null(paverStatus())) {
        return(NULL)
      }
      paverStatusObs$destroy()
      paverStatus <- NULL
      if (paverStatus() == 0) {
        hideEl(session, "#analysisLoad")
        paverResultTabs <- paste0("analysisResults_", 1:6)
        paverResultFiles <- c("index", "stat_Status", "stat_Efficiency", "stat_SolutionQuality", "solvedata", "documentation")
        paverResultTabNames <- c("Index", "Status", "Efficiency", "Solution Quality", "Solve data", "Documentation")
        lapply(2:length(paverResultTabs), function(i) {
          insertTab("analysisResults",
            target = paverResultTabs[i - 1L], position = "after",
            tabPanel(paverResultTabNames[i],
              value = paverResultTabs[i],
              tags$div(
                id = "wrapper-" %+% paverResultTabs[i],
                style = "overflow: auto; height: 75vh;",
                tryCatch(
                  suppressWarnings(includeHTML(paste0(
                    paverDir, .Platform$file.sep,
                    paverResultFiles[i], ".html"
                  ))),
                  error = function(e) {
                    tags$div(
                      class = "errMsg", style = "text-align:center;font-size:16px;margin-top:50px;",
                      lang$errMsg$paverFileLoad$desc
                    )
                  }
                )
              )
            )
          )
        })
        return(includeHTML(paste0(paverDir, .Platform$file.sep, paverResultFiles[1], ".html")))
      }
      paverError <- paver$read_error()
      paver <<- NULL
      flog.error("Problems while running paver. Error message: '%s'.", paverError)
      hideEl(session, "#analysisLoad")
      duplicatedInstances <- regmatches(paverError, regexpr("on instance [^>]*$", paverError))
      if (length(duplicatedInstances)) {
        showElReplaceTxt(session, "#paverFail", sprintf(
          lang$nav$hcubeAnalyze$duplicatesMsg,
          substr(
            duplicatedInstances, 13,
            nchar(duplicatedInstances) - 3L
          )
        ))
      } else {
        showEl(session, "#paverFail")
      }
    })
  } else {
    showModal(modalDialog(
      title = lang$nav$dialogPaverInUse$title,
      sprintf(lang$nav$dialogPaverInUse$desc),
      footer = tagList(
        modalButton(lang$nav$dialogPaverInUse$closeButton)
      ),
      fade = TRUE, easyClose = FALSE
    ))
  }
})

observeEvent(input$btAnalysisInterrupt, {
  if (is.null(paver)) {
    if (length(scriptOutput) && scriptOutput$isRunning()) {
      errMsg <- NULL
      tryCatch(
        {
          scriptOutput$interrupt()
        },
        error = function(e) {
          flog.error(
            "Problems interrupting process. Error message: %s.",
            conditionMessage(e)
          )
        }
      )
    } else {
      flog.error("Interrupt analysis script button was pressed, but no script is currently running. Likely to be an attempt to tamper with the app!")
      return()
    }
  } else {
    errMsg <- NULL
    tryCatch(
      {
        paver$kill()
      },
      error = function(e) {
        flog.error(
          "Problems interrupting process. Error message: %s.",
          conditionMessage(e)
        )
        errMsg <<- lang$errMsg$paverTerm$desc
      }
    )
    showErrorMsg(lang$errMsg$paverTerm$title, errMsg)
  }
  disableEl(session, "#btAnalysisInterrupt")
  hideEl(session, "#scriptOutput_hcube")
  hideEl(session, "#analysisLoad")
  showEl(session, "#newPaverRunButton")
})
observeEvent(input$btNewAnalysisRun, {
  updateTabsetPanel(session, "sidebarMenuId", selected = "loadResults")
})

if (length(config$scripts$hcube)) {
  observeEvent(input$btRunHcubeScript, {
    scriptId <- suppressWarnings(as.integer(input$selHcubeAnalysisScript))
    flog.debug("Button to execute Hypercube analysis script: '%s' clicked.", scriptId)

    if (is.na(scriptId) || scriptId < 1 || scriptId > length(config$scripts$hcube)) {
      flog.error(
        "A script with id: '%s' was attempted to be executed. However, this script does not exist. Looks like an attempt to tamper with the app!",
        scriptId
      )
      showHideEl(session, "#analysisRunUnknownError", 6000L)
      return()
    }
    if ((!is.null(paver) && is.null(paver$get_exit_status())) || scriptOutput$isRunning()) {
      flog.debug("A script is already running.")
      showHideEl(session, "#analysisRunScriptRunning", 6000L)
      return()
    }

    if (!dir.exists(paste0(workDir, .Platform$file.sep, "scripts_", modelName))) {
      if (dir.exists(paste0(currentModelDir, .Platform$file.sep, "scripts_", modelName))) {
        if (!file.copy2(
          paste0(currentModelDir, .Platform$file.sep, "scripts_", modelName),
          paste0(workDir, .Platform$file.sep, "scripts_", modelName)
        )) {
          flog.error(
            "Problems copying files from: '%s' to: '%s'.",
            paste0(workDir, .Platform$file.sep, "scripts_", modelName),
            paste0(currentModelDir, .Platform$file.sep, "scripts_", modelName)
          )
          showHideEl(session, "#analysisRunUnknownError", 6000L)
          return()
        }
      } else {
        flog.info(
          "No 'scripts_%s' directory was found. Did you forget to include it in '%s_files.txt'?",
          modelName, modelName
        )
        showHideEl(session, "#analysisRunUnknownError", 6000L)
        return()
      }
    }
    removeModal()
    if (gmswebiter > 1) {
      removeTab("analysisResults", "analysisResults_2")
      removeTab("analysisResults", "analysisResults_3")
      removeTab("analysisResults", "analysisResults_4")
      removeTab("analysisResults", "analysisResults_5")
      removeTab("analysisResults", "analysisResults_6")
    }
    hideEl(session, "#paverFail")
    hideEl(session, "#scriptOutput_hcube")
    updateTabsetPanel(session, "analysisResults", selected = "analysisResults_1")
    output$paverResults <- renderUI(character())
    showEl(session, "#analysisLoad")
    hideEl(session, "#newPaverRunButton")
    enableEl(session, "#btAnalysisInterrupt")
    switchTab(session, "hcubeAna")

    errMsg <- NULL

    prog <- Progress$new()
    on.exit(prog$close(), add = TRUE)
    prog$set(message = lang$nav$dialogHcube$waitDialog$title, value = 0)
    updateProgress <- function(incAmount, detail = NULL) {
      prog$inc(amount = incAmount, detail = detail)
    }
    tryCatch(
      {
        batchLoader$genGdxFiles(sidsToLoad, paste0(workDir, .Platform$file.sep, "scripts_", modelName),
          gdxio, prog,
          genScenList = TRUE
        )
      },
      error = function(e) {
        flog.error(
          "Problems writing gdx files for script: '%s'. Error message: '%s'.",
          scriptId, conditionMessage(e)
        )
        errMsg <<- sprintf(lang$errMsg$fileWrite$desc, "data.gdx")
        scriptOutput$sendContent(errMsg, scriptId, hcube = TRUE, isError = TRUE)
      }
    )
    if (!is.null(errMsg)) {
      return()
    }
    tryCatch(
      {
        scriptOutput$run(scriptId, hcube = TRUE)
      },
      error = function(e) {
        flog.info(
          "Script: '%s' crashed during startup. Error message: '%s'.",
          scriptId, conditionMessage(e)
        )
        scriptOutput$sendContent(lang$nav$scriptOutput$errMsg$crash, scriptId,
          hcube = TRUE, isError = TRUE
        )
      }
    )
  })
}

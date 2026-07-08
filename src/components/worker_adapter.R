WorkerAdapter <- R6Class("WorkerAdapter",
  public = list(
    supportsAsync = FALSE,
    processStatus = NULL,
    processId = NULL,
    inputData = NULL,
    quotaWarning = NULL,
    pollInterval = 1000L,
    initialize = function(metadata, workDir) {
      private$metadata <- private$validateMetadata(metadata)
      private$workDir <- workDir
      return(self)
    },
    run = function(solveOptions = NULL, name = NULL) {
      stop("Method 'run' must be implemented by subclass.")
    },
    runHypercube = function(dynamicPar = NULL, solveOptions = NULL, name = NULL) {
      stop("Method 'runHypercube' must be implemented by subclass.")
    },
    interrupt = function(hardKill = FALSE, processId = NULL) {
      stop("Method 'interrupt' must be implemented by subclass.")
    },
    pingProcess = function() {
      stop("Method 'pingProcess' must be implemented by subclass.")
    },
    pingLog = function() {
      stop("Method 'pingLog' must be implemented by subclass.")
    },
    getSid = function(resultsPath = NULL) {
      stop("Method 'getSid' must be implemented by subclass.")
    },
    getResults = function(resultsPath = NULL) {
      stop("Method 'getResults' must be implemented by subclass.")
    },
    removeResults = function() {
      stop("Method 'removeResults' must be implemented by subclass.")
    }
  ),
  active = list(
    log = function(newLogContent) {
      if (missing(newLogContent)) {
        currentLogContent <- private$logContent
        private$logContent <- ""
        return(currentLogContent)
      }
      if (length(newLogContent) > 0 && !identical(newLogContent, "")) {
        private$logContent <- paste0(private$logContent, newLogContent)
        private$updateLog <- private$updateLog + 1L
      }
    }
  ),
  private = list(
    metadata = NULL,
    workDir = NULL,
    updateLog = 0L,
    logContent = "",
    validateMetadata = function(metadata) stop("Method 'validateMetadata' must be implemented by subclass.")
  )
)


# ========================================
#       LOCAL
# ========================================

LocalWorkerAdapter <- R6Class("LocalWorkerAdapter",
  inherit = WorkerAdapter,
  public = list(
    supportsAsync = FALSE,
    pollInterval = 500L,
    run = function(solveOptions = NULL, name = NULL) {
      self$processStatus <- NULL
      self$inputData$writeDisk(private$workDir, fileName = MIROGdxInName)

      if (!is.null(private$metadata$logFileName)) {
        private$logFileInfo <- list(path = file.path(private$workDir, private$metadata$logFileName), cursor = 0L)
      }

      if (private$metadata$isGamsPy) {
        procArgs <- private$metadata$modelGmsName
        procWd <- private$workDir
        stderrHandler <- if (is.null(private$metadata$logFileName)) "2>&1" else NULL
        if (length(private$metadata$extraClArgs)) {
          procArgs <- c(procArgs, private$metadata$extraClArgs)
        }
      } else {
        gamsArgs <- c(
          if (length(private$metadata$extraClArgs)) private$metadata$extraClArgs,
          paste0('curdir="', private$workDir, '"'), "lo=3", private$metadata$clArgs,
          paste0('IDCGDXInput="', MIROGdxInName, '"'),
          "LstTitleLeftAligned=1"
        )
        if (private$metadata$saveTraceFile) {
          gamsArgs <- c(gamsArgs, 'trace="_scenTrc.trc"', "traceopt=3")
        }
        pfFilePath <- gmsFilePath(file.path(private$workDir, tolower(private$metadata$modelName) %+% ".pf"))
        writeLines(c(self$inputData$getClArgs(), gamsArgs), pfFilePath)
        procArgs <- c(private$metadata$modelGmsName, "pf", pfFilePath)
        procWd <- NULL
        stderrHandler <- NULL
      }

      private$process <- process$new(
        command = private$metadata$executablePath,
        args = procArgs,
        stdout = if (is.null(private$metadata$logFileName)) "|" else NULL,
        stderr = stderrHandler,
        windows_hide_window = TRUE,
        wd = procWd,
        env = private$getProcEnv()
      )
      return(private$process$get_pid())
    },
    interrupt = function(hardKill = FALSE, processId = NULL) {
      stopifnot(is.null(processId))
      errMsg <- NULL
      pidToKill <- tryCatch(
        {
          if (hardKill) {
            private$process$kill_tree()
          } else if (isWindows()) {
            if (!private$metadata$isGamsPy && "miroUtil" %in% installedPackages) {
              miroUtil::windowsInterruptGAMS(private$process$get_pid())
            } else {
              private$process$interrupt()
            }
          } else {
            private$process$signal(tools::SIGINT)
          }
          NULL
        },
        error = function(err) {
          flog.warn("Unable to kill process. Error message: %s.", conditionMessage(err))
          return(private$process$get_pid())
        }
      )
      if (!is.null(pidToKill)) {
        errMsg <- NULL
        flog.info("Interrupting process with pid: '%s'.", pidToKill)
        if (isWindows()) {
          tryCatch(
            {
              processx::run(
                command = "taskkill", args = c(
                  if (hardKill) "/F",
                  "/PID",
                  pidToKill,
                  "/T"
                ),
                windows_hide_window = TRUE, timeout = 10L
              )
            },
            error = function(err) {
              flog.error(
                "Problems interrupting process with pid: %s. Error message: '%s'.",
                pidToKill, conditionMessage(err)
              )
            }
          )
        } else {
          tryCatch(
            {
              processx::run(
                command = "kill",
                args = c(
                  if (hardKill) "-SIGKILL" else "-SIGINT",
                  -pidToKill
                ), timeout = 10L
              )
            },
            error = function(err) {
              flog.error(
                "Problems interrupting process with pid: %s. Error message: '%s'.",
                pidToKill, conditionMessage(err)
              )
            }
          )
        }
      }
      return(invisible(self))
    },
    pingProcess = function() {
      exitStatus <- private$process$get_exit_status()
      if (length(exitStatus)) {
        self$processStatus <- exitStatus
      }
      return(self$processStatus)
    },
    pingLog = function() {
      if (is.null(private$logFileInfo)) {
        self$log <- tryCatch(
          private$process$read_output(),
          error = function(err) {
            flog.info("Problems reading process output. Error message: %s", conditionMessage(err))
            return("")
          }
        )
        return(private$updateLog)
      }
      info <- ""
      self$log <- tryCatch(
        {
          info <- file.info(private$logFileInfo$path)
          info <- paste(private$logFileInfo$path, info$mtime, info$size)
          read_lines(private$logFileInfo$path, skip = private$logFileInfo$cursor)
        },
        error = function(err) {
          flog.info("Problems reading log file. Error message: %s", conditionMessage(err))
          return("")
        }
      )
      return(info)
    },
    getResults = function(resultsPath = NULL) {
      return(invisible(self))
    },
    removeResults = function() {
      return(invisible(self))
    }
  ),
  private = list(
    process = NULL,
    logFileInfo = NULL,
    validateMetadata = function(metadata) {
      stopifnot(
        is.logical(metadata$isGamsPy), is.logical(metadata$saveTraceFile),
        is.character(metadata$modelName),
        is.character(metadata$modelGmsName), is.character(metadata$executablePath),
        length(metadata$clArgs) > 0
      )
      return(metadata)
    },
    getProcEnv = function() {
      if (private$metadata$isGamsPy) {
        procEnv <- Sys.getenv()
        procEnv[["GAMS_IDC_GDX_INPUT"]] <- file.path(private$workDir, MIROGdxInName)
        procEnv[["GAMS_IDC_GDX_OUTPUT"]] <- file.path(private$workDir, MIROGdxOutName)
        return(procEnv)
      }
      # workaround since GAMS31 has a bug on Linux that causes an infinite loop in case
      # XDG_DATA_DIRS or XDG_CONFIG_DIRS has more than 8 entries
      procEnv <- NULL
      if (identical(Sys.info()[["sysname"]], "Linux")) {
        procEnv <- Sys.getenv()
        XDG_DATA_DIRS <- strsplit(Sys.getenv("XDG_DATA_DIRS"), ":", fixed = TRUE)[[1L]]
        procEnv[["XDG_DATA_DIRS"]] <- paste(XDG_DATA_DIRS[seq_len(min(length(XDG_DATA_DIRS), 7L))],
          collapse = ":"
        )
      }
      return(procEnv)
    }
  )
)

# ========================================
#       REMOTE
# ========================================

RemoteWorkerAdapter <- R6Class("RemoteWorkerAdapter",
  inherit = WorkerAdapter,
  public = list(
    supportsAsync = TRUE,
    initialize = function(metadata, workDir, engineConfig) {
      super$initialize(metadata, workDir)
      private$engineConfig <- engineConfig
      gamsArgs <- c(
        private$metadata$extraClArgs,
        private$metadata$clArgs
      )
      if (private$metadata$saveTraceFile) {
        traceFileName <- "_scenTrc.trc"
        gamsArgs <- c(gamsArgs, paste0('trace="', traceFileName, '"'), "traceopt=3")
        private$metadata$modelDataFiles <- c(private$metadata$modelDataFiles, traceFileName)
      }
      gamsArgs <- c(gamsArgs, paste0('IDCGDXInput="', MIROGdxInName, '"'))
      private$metadata$gamsArgs <- gamsArgs
      return(self)
    },
    run = function(solveOptions = NULL, name = NULL) {
      flog.debug("Request to submit new job with name: %s received.", name)
      private$gamsReturnCode <- NULL
      self$processId <- NULL
      self$processStatus <- "s"
      self$quotaWarning <- NULL
      if (is_mirai(private$mSubRes) && unresolved(private$mSubRes)) {
        flog.warn("Previous submission promise still running. Stopping it.")
        stop_mirai(private$mSubRes)
      }
      if (is_mirai(private$mJobRes) && unresolved(private$mJobRes)) {
        flog.warn("Previous result download promise still running. Stopping it.")
        stop_mirai(private$mJobRes)
        private$mJobRes <- NULL
      }
      if (is_mirai(private$mLogRes) && unresolved(private$mLogRes)) {
        flog.warn("Previous log fetching promise still running. Stopping it.")
        stop_mirai(private$mLogRes)
        private$mLogRes <- NULL
      }
      private$mSubRes <- private$runInternal(solveOptions, name)
      return(invisible(self))
    },
    runAsync = function(solveOptions = NULL, name = NULL) {
      flog.debug("Request to submit new asynchronous job with name: %s received.", name)
      return(private$runInternal(solveOptions, name))
    },
    runHypercube = function(dynamicPar = NULL, solveOptions = NULL, name = NULL) {
      flog.debug("Request to submit new Hypercube job with name: %s received.", name)
      return(private$runInternal(solveOptions, name, dynamicPar = dynamicPar))
    },
    pingProcess = function() {
      if (!is.null(private$mSubRes) && unresolved(private$mSubRes)) {
        return(self$processStatus)
      }

      if (!is.null(private$mSubRes) && !unresolved(private$mSubRes)) {
        resData <- private$mSubRes$data
        if (is_error_value(resData)) {
          flog.warn("Error submitting job: %s", resData$message)
          if (startsWith(resData$message, "Failed to connect") ||
            startsWith(resData$message, "Could not") ||
            startsWith(resData$message, "Timeout was")) {
            self$processStatus <- -404L
          } else {
            self$processStatus <- -500L
          }
        } else if (resData$status_code == 201L) {
          self$processId <- resData$response$token
          if (is.null(resData$response$queue_position)) {
            self$processStatus <- "q"
          } else {
            self$processStatus <- paste0("q", resData$response$queue_position)
          }
          if (length(resData$response$quota_warning)) {
            self$quotaWarning <- calcRemainingQuota(resData$response$quota_warning)
            self$quotaWarning$error <- FALSE
          }
        } else {
          flog.info(
            "Could not execute model remotely. Status code: %s. Error message: %s",
            resData$status_code, resData$response$message
          )
          self$processStatus <- -resData$status_code
          if (identical(resData$status_code, 402L) && length(resData$response$exceeded_quotas)) {
            self$quotaWarning <- calcRemainingQuota(resData$response$exceeded_quotas)
            self$quotaWarning$error <- TRUE
          }
        }
        private$mSubRes <- NULL
        return(self$processStatus)
      }

      if (!is.null(private$gamsReturnCode)) {
        if (unresolved(private$mJobRes)) {
          return(self$processStatus)
        }
        if (is_error_value(private$mJobRes$data)) {
          flog.error("Error downloading job results: %s", private$mJobRes$data$message)
          self$processStatus <- -500L
          return(self$processStatus)
        }
        if (length(private$mJobRes$data$warnings)) {
          flog.warn("Warnings downloading job results: %s", private$mJobRes$data$warnings)
        }
        self$processStatus <- private$gamsReturnCode
        return(self$processStatus)
      }

      if (!is.null(self$processId)) {
        logResp <- DELETE(
          paste0(private$engineConfig$url, "/jobs/", self$processId, "/unread-logs"),
          add_headers(Authorization = private$engineConfig$authHeader),
          timeout(10L)
        )
        if (status_code(logResp) == 200L) {
          self$processStatus <- tryCatch(
            {
              unreadLogResp <- content(logResp, type = "application/json", encoding = "utf-8")
              if (is.null(private$metadata$logFileName)) {
                self$log <- unreadLogResp$message
              }
              if (identical(unreadLogResp$queue_finished, TRUE)) {
                private$gamsReturnCode <- unreadLogResp$gams_return_code
                private$mJobRes <- self$getResults()
                "d"
              } else {
                NULL
              }
            },
            error = function(err) {
              flog.info(
                "Invalid JSON reponse received from unread-logs endpoint: %s",
                conditionMessage(err)
              )
              return(-404L)
            }
          )
        } else if (status_code(logResp) == 403L) {
          # job still pending. Try again
        } else if (status_code(logResp) == 308L) {
          # partial log no longer available
          gamsRetCode <- tryCatch(self$getJobStatus()$process_status, error = function(err) {
            flog.warn("Could not determine job status. Error message: %s", conditionMessage(err))
            NULL
          })
          if (is.null(gamsRetCode)) {
            flog.error("DELETE unread-logs endpoint returned %d status but job's process_status was NULL.", status_code(logResp))
            self$processStatus <- -500L
          } else {
            self$processStatus <- gamsRetCode
          }
        }
      }
      return(self$processStatus)
    },
    pingLog = function() {
      if (is.null(private$metadata$logFileName)) {
        return(private$updateLog)
      }
      if (is.null(self$processId) || isTRUE(startsWith(as.character(self$processStatus), "q"))) {
        return(private$updateLog)
      }
      if (is.null(private$mLogRes)) {
        private$mLogRes <- mirai(
          {
            streamEntryResp <- httr::DELETE(
              paste0(
                config$url, "/jobs/", pid, "/stream-entry/",
                fileName
              ),
              httr::add_headers(
                Authorization = config$authHeader
              ),
              httr::timeout(20L)
            )
            if (httr::status_code(streamEntryResp) != 200L) {
              stop(sprintf(
                "Could not fetch stream entry (status code: %d). Error: %s",
                httr::status_code(streamEntryResp),
                trimws(httr::content(streamEntryResp, as = "text", encoding = "utf-8"))
              ), call. = FALSE)
            }
            return(httr::content(streamEntryResp, type = "application/json", encoding = "utf-8")$entry_value)
          },
          .args = list(config = private$engineConfig, pid = self$processId, fileName = URLencode(private$metadata$logFileName, reserved = TRUE))
        )
      } else if (unresolved(private$mLogRes)) {
        # wait for promise to resolve
      } else {
        if (is_error_value(private$mLogRes$data)) {
          flog.info("Error fetching MIRO log: %s", private$mLogRes$data$message)
        } else {
          self$log <- private$mLogRes$data
        }
        private$mLogRes <- NULL
      }
      return(private$updateLog)
    },
    interrupt = function(hardKill = FALSE, processId = NULL, isHypercubeJob = FALSE) {
      if (is.null(processId)) {
        stopifnot(!is.null(self$processId))
        processId <- self$processId
        isHypercubeJob <- FALSE
      }

      endpoint <- if (isHypercubeJob) "/hypercube/" else "/jobs/"

      private$validateApiResponse(DELETE(
        paste0(private$engineConfig$url, endpoint, processId),
        body = list(hard_kill = hardKill), encode = "json",
        add_headers(Authorization = private$engineConfig$authHeader),
        timeout(10L)
      ))
      return(0L)
    },
    getResults = function(processId = NULL, resultsPath = NULL, isHypercubeJob = FALSE) {
      if (is.null(resultsPath)) {
        resultsPath <- tempfile(pattern = "res_", fileext = ".zip")
        workDir <- private$workDir
      } else {
        workDir <- dirname(resultsPath)
      }
      if (is.null(processId)) {
        processId <- self$processId
        isHypercubeJob <- FALSE
      }
      return(mirai(
        {
          if (isHypercubeJob) {
            url <- paste0(config$url, "/hypercube/", pid, "/result")
          } else {
            url <- paste0(config$url, "/jobs/", pid, "/result")
          }
          resultResp <- httr::GET(
            url = url,
            httr::write_disk(path, overwrite = TRUE),
            httr::add_headers(Authorization = config$authHeader)
          )
          resultDownloadStatus <- httr::status_code(resultResp)
          if (!identical(resultDownloadStatus, 200L)) {
            unlink(path)
            stop(sprintf(
              "Could not download job results (status code: %d). Error message: %s",
              resultDownloadStatus, httr::content(resultResp, as = "text", encoding = "utf-8")
            ), call. = FALSE)
          }
          if (!isHypercubeJob) {
            tryCatch(
              zip::unzip(path, exdir = workDir),
              error = function(err) {
                contentPreview <- tryCatch(
                  paste(
                    as.character(readBin(path,
                      what = "raw", n = 20L
                    )),
                    collapse = " "
                  ),
                  error = function(err2) {
                    return(conditionMessage(err2))
                  }
                )
                unlink(path)
                stop(sprintf(
                  "Problems extracting results archive. Error message: %s. Preview of content received: %s.",
                  conditionMessage(err), contentPreview
                ), call. = FALSE)
              }
            )
            unlink(path)
          }
          deleteResp <- httr::DELETE(
            url = url,
            httr::add_headers(
              Authorization = config$authHeader
            ),
            httr::timeout(20L)
          )
          if (httr::status_code(deleteResp) != 200L) {
            return(list(warnings = sprintf("Could not delete job results (status code: %d). Response: %s", httr::status_code(deleteResp), httr::content(deleteResp, as = "text", encoding = "utf-8"))))
          }
        },
        .args = list(config = private$engineConfig, pid = processId, path = resultsPath, workDir = workDir, isHypercubeJob = isHypercubeJob)
      ))
    },
    removeResults = function(processId = NULL, isHypercubeJob = FALSE) {
      if (is.null(processId)) {
        stopifnot(!is.null(self$processId))
        processId <- self$processId
      }
      private$validateApiResponse(
        DELETE(
          url = paste0(
            private$engineConfig$url,
            if (isHypercubeJob) "/hypercube/" else "/jobs/",
            processId, "/result"
          ),
          add_headers(
            Authorization = private$engineConfig$authHeader
          ),
          timeout(10L)
        )
      )
      return(invisible(self))
    },
    getJobStatus = function(processId = NULL, isHypercubeJob = FALSE) {
      if (is.null(processId)) {
        stopifnot(!is.null(self$processId))
        processId <- self$processId
        isHypercubeJob <- FALSE
      }
      if (isHypercubeJob) {
        url <- paste0(private$engineConfig$url, "/hypercube/?hypercube_token=", processId)
      } else {
        url <- paste0(private$engineConfig$url, "/jobs/", processId)
      }
      return(private$validateApiResponse(
        GET(
          url = url,
          add_headers(
            Authorization = private$engineConfig$authHeader
          ),
          timeout(10L)
        )
      ))
    },
    getResultsInfo = function(processId = NULL, isHypercubeJob = FALSE) {
      if (is.null(processId)) {
        stopifnot(!is.null(self$processId))
        processId <- self$processId
        isHypercubeJob <- FALSE
      }
      if (isHypercubeJob) {
        url <- paste0(private$engineConfig$url, "/hypercube/", processId, "/result")
      } else {
        url <- paste0(private$engineConfig$url, "/jobs/", processId, "/result")
      }
      return(HEAD(
        url = url,
        add_headers(
          Authorization = private$engineConfig$authHeader
        ),
        timeout(10L)
      ))
    },
    readTextEntry = function(name, processId = NULL, saveDisk = FALSE, maxSize = NULL,
                             workDir = private$workDir, chunkNo = 0L, getSize = FALSE) {
      if (is.null(processId)) {
        stopifnot(!is.null(self$processId))
        processId <- self$processId
      }
      if (!is.null(maxSize)) {
        headResp <- HEAD(
          paste0(
            private$engineConfig$url, "/jobs/", processId, "/text-entry/",
            URLencode(name, reserved = TRUE)
          ),
          add_headers(
            Authorization = private$engineConfig$authHeader
          ),
          timeout(10L)
        )
        if (!identical(status_code(headResp), 200L)) {
          return(status_code(headResp))
        }
        teLength <- tryCatch(
          {
            suppressWarnings(as.numeric(headers(headResp)[["char_length"]]))
          },
          error = function(e) {
            return(404L)
          }
        )
        if (identical(teLength, 404L) || is.na(teLength)) {
          return(404L)
        }
        startPos <- maxSize * chunkNo + 1L
      } else {
        teLength <- NULL
      }

      requestURL <- tryCatch(paste0(
        private$engineConfig$url, "/jobs/", processId, "/text-entry/",
        URLencode(name, reserved = TRUE),
        if (!is.null(teLength)) {
          sprintf(
            "?start_position=%d&length=%d",
            startPos, min(teLength - startPos, maxSize)
          )
        }
      ), error = function(e) {
        if (!endsWith(conditionMessage(e), "for numeric objects")) {
          flog.warn(
            "Unexpected error while building request URL to fetch text entry. Error message: %s",
            conditionMessage(e)
          )
        }
        return(413L)
      })
      if (identical(requestURL, 413L)) {
        return(413L)
      }
      teResp <- GET(
        requestURL,
        add_headers(
          Authorization = private$engineConfig$authHeader
        ),
        timeout(10L)
      )

      if (!identical(status_code(teResp), 200L)) {
        return(status_code(teResp))
      }

      if (saveDisk) {
        entryContent <- content(teResp, encoding = "utf-8")$entry_value
        if (!length(entryContent)) {
          entryContent <- ""
        }
        writeLines(
          entryContent,
          file.path(workDir, name)
        )
        return(200L)
      }
      if (getSize) {
        return(list(
          content = content(teResp, encoding = "utf-8")$entry_value,
          chunkNo = ceiling(teLength / maxSize)
        ))
      }
      return(content(teResp, encoding = "utf-8")$entry_value)
    }
  ),
  private = list(
    engineConfig = NULL,
    mSubRes = NULL,
    mJobRes = NULL,
    mLogRes = NULL,
    gamsReturnCode = NULL,
    validateMetadata = function(metadata) {
      stopifnot(
        is.logical(metadata$isGamsPy), is.logical(metadata$saveTraceFile),
        is.character(metadata$modelName), is.character(metadata$modelId),
        is.character(metadata$modelGmsName), is.character(metadata$modelNameRaw),
        length(metadata$clArgs) > 0L, length(metadata$modelDataFiles) > 0L, length(metadata$textEntries) > 0L,
        is.logical(metadata$useRegistered)
      )
      return(metadata)
    },
    runInternal = function(solveOptions, name, dynamicPar = NULL) {
      isHypercube <- !is.null(dynamicPar)

      self$inputData$writeDisk(private$workDir, fileName = MIROGdxInName)

      requestBody <- list(
        model = private$metadata$modelId,
        run = private$metadata$modelGmsName,
        arguments = paste0("pf=", private$metadata$modelName, ".pf"),
        tag = name,
        namespace = private$engineConfig$namespace
      )
      hypercubeFileName <- NULL

      if (isHypercube) {
        hypercubeFileName <- dynamicPar$writeHcubeFile(private$workDir)
        filesToReturn <- c(
          private$metadata$modelDataFiles,
          private$metadata$textEntries
        )
        filesToReturn <- filesToReturn[filesToReturn != MIROGdxInName]
        submissionUrl <- paste0(private$engineConfig$url, "/hypercube/")
      } else {
        self$inputData$copyMiroWs(private$workDir, jobName = name)
        requestBody$stdout_filename <- paste0(private$metadata$modelNameRaw, ".log")
        filesToReturn <- c(
          private$metadata$modelDataFiles,
          private$metadata$textEntries,
          requestBody$stdout_filename,
          "_miro_ws_/*"
        )
        submissionUrl <- paste0(private$engineConfig$url, "/jobs/")
        if (length(private$metadata$textEntries)) {
          escapedTextEntries <- vapply(private$metadata$textEntries,
            URLencode, character(1L),
            reserved = TRUE,
            USE.NAMES = FALSE
          )
          escapedTextEntries <- paste0("?text_entries=", paste(escapedTextEntries,
            collapse = "&text_entries="
          ))
          submissionUrl <- paste0(submissionUrl, escapedTextEntries)
        }
      }

      pfFilePath <- private$writePfFile()
      inexFilePath <- self$inputData$addInexFile(private$workDir, filesToReturn)

      dataCompressArgs <- self$inputData$addFilePaths(pfFilePath)$getCompressArgs()

      return(mirai(
        {
          isHypercube <- !is.null(hypercubeFileName)
          do.call(zip::zipr, dataCompressArgs)

          if (isHypercube) {
            requestBody$hypercube_file <- httr::upload_file(
              hypercubeFileName,
              type = "application/json"
            )
          } else if (!is.null(meta$logFileName)) {
            requestBody$stream_entries <- meta$logFileName
          }

          requestBody$inex_file <- httr::upload_file(
            inexFilePath,
            type = "application/json"
          )
          requestBody$data <- httr::upload_file(
            dataCompressArgs[["zipfile"]],
            type = "application/zip"
          )

          if (identical(meta$useRegistered, FALSE)) {
            requestBody$model_data <- httr::upload_file(meta$modelDataPath,
              type = "application/zip"
            )
          }
          if (length(solveOptions) && length(solveOptions$selectedInstance)) {
            requestBody$labels <- paste0("instance=", solveOptions$selectedInstance)
          }

          res <- httr::POST(
            url = url,
            body = requestBody,
            encode = "multipart",
            httr::add_headers(Authorization = config$authHeader),
            httr::timeout(120L)
          )

          list(
            status_code = httr::status_code(res),
            response = httr::content(res, type = "application/json", encoding = "utf-8")
          )
        },
        .args = list(
          meta = private$metadata, dataCompressArgs = dataCompressArgs, config = private$engineConfig,
          inexFilePath = inexFilePath, pfFilePath = pfFilePath, url = submissionUrl,
          solveOptions = solveOptions, requestBody = requestBody, hypercubeFileName = hypercubeFileName
        )
      ))
    },
    validateApiResponse = function(response) {
      if (status_code(response) >= 300L) {
        stop(status_code(response),
          call. = FALSE
        )
      }
      return(tryCatch(
        {
          content(response,
            type = "application/json",
            encoding = "utf-8"
          )
        },
        error = function(e) {
          stop(-404L, call. = FALSE)
        }
      ))
    },
    writePfFile = function() {
      pfFilePath <- gmsFilePath(file.path(private$workDir, paste0(tolower(private$metadata$modelName), ".pf")))
      writeLines(c(self$inputData$getClArgs(), private$metadata$gamsArgs), pfFilePath)
      return(pfFilePath)
    }
  )
)

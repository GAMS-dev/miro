AsyncJobManager <- R6Class("AsyncJobManager",
  public = list(
    initialize = function(dbJobSchema, db, adapter) {
      stopifnot(adapter$supportsAsync)
      private$dbJobSchema <- dbJobSchema
      private$db <- db
      private$conn <- db$getConn()
      private$uid <- db$getUid()
      private$adapter <- adapter
      return(invisible(self))
    },
    getInfoFromJobList = function(jID, key = NULL) {
      if (!length(private$jobList) || !nrow(private$jobList)) {
        return(NULL)
      }
      jIDs <- private$jobList[[1]]
      jIdx <- match(jID, jIDs)
      if (is.na(jIdx)) {
        return(NULL)
      }
      if (length(key)) {
        return(private$jobList[[key]][[jIdx]])
      }
      return(private$jobList[jIdx, ])
    },
    getPid = function(jID) {
      return(self$getInfoFromJobList(jID, private$dbJobSchema$colNames[["pid"]]))
    },
    getStatus = function(jID) {
      return(self$getInfoFromJobList(jID, private$dbJobSchema$colNames[["status"]]))
    },
    addJob = function(pid, sid, tags = NULL, status = NULL, name = NULL, isHcJob = FALSE) {
      stopifnot(length(pid) == 1)

      if (length(sid)) {
        stopifnot(length(sid) == 1, is.integer(sid))
      } else {
        sid <- NULL
      }
      if (length(status)) {
        stopifnot(length(status) == 1L, status %in% JOBSTATUSMAP)
      } else {
        status <- JOBSTATUSMAP[["running"]]
      }
      if (length(name)) {
        stopifnot(is.character(name), length(name) == 1L)
        if (nchar(name) > 255) {
          name <- substr(name, 0L, 255L)
        }
      } else {
        name <- ""
      }
      colNames <- private$dbJobSchema$colNames
      tabName <- private$dbJobSchema$tabName
      err <- FALSE
      tryCatch(
        {
          private$db$createJobMeta()
        },
        error = function(e) {
          flog.error(
            "Problems creating job metadata table. Error message: '%s'.",
            conditionMessage(e)
          )
          err <<- TRUE
        }
      )
      if (err) {
        return(-1L)
      }
      tryCatch(
        {
          query <- paste0(
            "INSERT INTO ",
            dbQuoteIdentifier(private$conn, tabName),
            " (", dbQuoteIdentifier(private$conn, colNames[[2]]), ",",
            dbQuoteIdentifier(private$conn, colNames[[3]]), ",",
            dbQuoteIdentifier(private$conn, colNames[[4]]), ",",
            dbQuoteIdentifier(private$conn, colNames[[5]]), ",",
            dbQuoteIdentifier(private$conn, colNames[[6]]), ",",
            dbQuoteIdentifier(private$conn, colNames[[7]]), ",",
            dbQuoteIdentifier(private$conn, colNames[[9]]), ",",
            dbQuoteIdentifier(private$conn, colNames[[10]]),
            ") VALUES (",
            dbQuoteLiteral(private$conn, private$uid), ",",
            status, ",",
            dbQuoteLiteral(private$conn, as.character(Sys.time(), usetz = TRUE)), ",",
            dbQuoteLiteral(private$conn, vector2Csv(tags)), ",",
            dbQuoteLiteral(private$conn, as.character(pid)), ",",
            if (length(sid)) dbQuoteLiteral(private$conn, sid) else "NULL", ",",
            dbQuoteLiteral(
              private$conn,
              if (isHcJob) {
                SCODEMAP[["hcube_jobconfig"]]
              } else {
                SCODEMAP[["scen"]]
              }
            ), ",",
            dbQuoteString(private$conn, name), ")",
            " RETURNING ", dbQuoteIdentifier(private$conn, colNames[[1]])
          )
          jID <- dbGetQuery(private$conn, SQL(query))[[1L]][1L]
          return(jID)
        },
        error = function(e) {
          flog.error(
            "Problems writing job metadata. Error message: '%s'.",
            conditionMessage(e)
          )
        }
      )
      return(-1L)
    },
    updateJobStatus = function(newStatus, jID, tags = NULL, pID = NULL) {
      isHcJob <- FALSE
      if (is.null(pID)) {
        pID <- self$getPid(jID)
      }
      isHcJob <- identical(
        self$getInfoFromJobList(jID, "_scode"),
        SCODEMAP[["hcube_jobconfig"]]
      )
      if (!is.integer(jID) || jID < 0L) {
        flog.warn("Could not update job status as job ID is invalid.")
        return()
      }
      if (length(pID) == 0L) {
        flog.warn("Could not update job status as job process ID is invalid.")
        return()
      }
      gamsRetCode <- NULL
      if (newStatus %in% c(
        JOBSTATUSMAP[["imported"]],
        JOBSTATUSMAP[["discarded"]]
      )) {
        jobStatus <- private$getJobStatus(pID, isHcJob = isHcJob)
        gamsRetCode <- jobStatus$gamsRetCode
        if (identical(newStatus, JOBSTATUSMAP[["discarded"]])) {
          if (jobStatus$status >= JOBSTATUSMAP[["corrupted"]] &&
            jobStatus$status < JOBSTATUSMAP[["discarded"]]) {
            newStatus <- JOBSTATUSMAP[["discarded(corrupted)"]]
          } else if (identical(jobStatus$status, JOBSTATUSMAP[["running"]]) ||
            identical(jobStatus$status, JOBSTATUSMAP[["queued"]])) {
            private$adapter$interrupt(hardKill = TRUE, processId = pID, isHypercubeJob = isHcJob)
            newStatus <- JOBSTATUSMAP[["discarded(running)"]]
          } else if (identical(jobStatus$status, JOBSTATUSMAP[["completed"]])) {
            tryCatch(
              private$adapter$removeResults(pID, isHypercubeJob = isHcJob),
              error = function(err) {
                flog.warn(
                  "Could not remove results for job %s: %s",
                  jID,
                  conditionMessage(err)
                )
              }
            )
            newStatus <- JOBSTATUSMAP[["discarded(completed)"]]
          }
        }
      }

      colNames <- private$dbJobSchema$colNames

      colNamesToUpdate <- colNames[["status"]]
      valuesToUpdate <- newStatus

      if (length(gamsRetCode)) {
        colNamesToUpdate <- c(colNamesToUpdate, colNames[["gamsret"]])
        valuesToUpdate <- c(valuesToUpdate, gamsRetCode)
      }
      if (length(tags)) {
        colNamesToUpdate <- c(colNamesToUpdate, colNames[["tag"]])
        valuesToUpdate <- c(valuesToUpdate, tags)
      }
      private$db$updateRows("_jobMeta",
        tibble(colNames[[1L]], jID),
        colNames = colNamesToUpdate, values = valuesToUpdate
      )
      return(invisible(self))
    },
    getJobList = function(jobHist = FALSE) {
      colNames <- private$dbJobSchema$colNames
      newCompleted <- FALSE
      jobList <- private$db$importDataset("_jobMeta",
        tibble(
          c(
            colNames[["uid"]],
            colNames[["status"]],
            colNames[["scode"]]
          ),
          c(
            private$uid,
            JOBSTATUSMAP[["discarded"]],
            SCODEMAP[["hcube_jobconfig"]]
          ),
          c(
            "=", if (jobHist) ">=" else "<",
            ">="
          )
        ),
        orderBy = colNames[["time"]], orderAsc = FALSE
      )
      if (jobHist) {
        return(list(jobList = jobList, newCompleted = FALSE))
      }
      if (!length(jobList) || !nrow(jobList)) {
        private$jobList <- jobList
        private$jobListInit <- TRUE
        return(list(jobList = private$jobList, newCompleted = newCompleted))
      } else {
        jobList[
          jobList[[1]] %in% self$getFinishedDownloads(),
          3L
        ] <- JOBSTATUSMAP[["downloaded"]]
        private$jobList <- jobList
      }
      jIDs <- private$jobList[[1]]
      pIDs <- private$jobList[[6]]
      jStatus <- private$jobList[[3]]
      for (i in seq_along(jIDs)) {
        if (jStatus[i] > JOBSTATUSMAP[["running"]]) {
          next
        }
        jobStatus <- private$getJobStatus(pIDs[i], isHcJob = identical(
          private$jobList[["_scode"]][i],
          SCODEMAP[["hcube_jobconfig"]]
        ))
        gamsRetCode <- jobStatus$gamsRetCode
        newStatus <- jobStatus$status
        if (identical(newStatus, JOBSTATUSMAP[["completed"]]) &&
          !private$jobListInit) {
          newCompleted <- TRUE
        }
        if (length(newStatus)) {
          self$updateJobStatus(newStatus, jIDs[i])
          private$jobList[i, 3] <- newStatus
        }
      }
      private$jobListInit <- TRUE
      return(list(jobList = private$jobList, newCompleted = newCompleted))
    },
    getJobResultsPath = function(jID) {
      jIDChar <- as.character(jID)
      if (!jIDChar %in% names(private$jobResultsFile)) {
        stop(sprintf("Job directory not found for job: '%s'.", jID),
          call. = FALSE
        )
      }
      return(private$jobResultsFile[[jIDChar]])
    },
    getActiveDownloads = function() {
      return(as.integer(names(private$resultFileSize)))
    },
    getFinishedDownloads = function() {
      return(setdiff(
        as.integer(names(private$jobResultsFile)),
        self$getActiveDownloads()
      ))
    },
    removeActiveDownload = function(jID) {
      jIDChar <- as.character(jID)
      private$mJobRes[[jIDChar]] <- NULL
      filePath <- private$jobResultsFile[[jIDChar]]
      if (length(filePath) > 0 && file.exists(filePath)) {
        if (identical(unlink(filePath,
          force = TRUE
        ), 1L)) {
          flog.error(
            "Problems removing job file: '%s'.",
            filePath
          )
        }
      }
      private$jobResultsFile[[jIDChar]] <- NULL
      private$resultFileSize[[jIDChar]] <- NULL
      return(invisible(self))
    },
    getJobResults = function(jID) {
      isHcJob <- identical(
        self$getInfoFromJobList(jID, "_scode"),
        SCODEMAP[["hcube_jobconfig"]]
      )
      jIDChar <- as.character(jID)
      pid <- self$getPid(jID)
      if (is_mirai(private$mJobRes[[jIDChar]]) && !unresolved(private$mJobRes[[jIDChar]])) {
        if (is_error_value(private$mJobRes[[jIDChar]]$data)) {
          if (is.integer(private$mJobRes[[jIDChar]]$data)) {
            errMsg <- "Aborted"
          } else {
            errMsg <- private$mJobRes[[jIDChar]]$data$message
          }
          stop(sprintf(
            "Problems downloading results of job: '%s' (Hypercube: %s). Error message: '%s'.",
            jIDChar, isHcJob, errMsg
          ), call. = FALSE)
        }
        if (length(private$mJobRes[[jIDChar]]$data$warnings)) {
          flog.warn(
            "Warnings downloading results of job: '%s' (Hypercube: %s): %s",
            jIDChar, isHcJob, paste(private$mJobRes[[jIDChar]]$data$warnings, collapse = ",")
          )
        }
        if (isHcJob) {
          if (!file.exists(private$jobResultsFile[[jIDChar]])) {
            file.rename(
              paste0(private$jobResultsFile[[jIDChar]], ".dl"),
              private$jobResultsFile[[jIDChar]]
            )
            flog.debug(
              "Hypercube results of job: '%s' were downloaded to: '%s'.",
              jIDChar, private$jobResultsFile[[jIDChar]]
            )
          }
        } else {
          if (identical(unlink(paste0(private$jobResultsFile[[jIDChar]], ".dl")), 1L)) {
            stop(sprintf(
              "Could not remove temporary file: '%s'.",
              paste0(private$jobResultsFile[[jIDChar]], ".dl")
            ), call. = FALSE)
          }
          private$jobResultsFile[[jIDChar]] <- dirname(private$jobResultsFile[[jIDChar]])
          flog.debug(
            "Job results of job: '%s' were downloaded to: '%s'.",
            jIDChar, private$jobResultsFile[[jIDChar]]
          )
        }
        private$mJobRes[[jIDChar]] <- NULL
        private$resultFileSize[[jIDChar]] <- NULL
        return(100L)
      }
      if (!length(private$jobResultsFile[[jIDChar]])) {
        jobResultsFile <- file.path(tempdir(TRUE), jIDChar, "results.zip")
        if (file.exists(jobResultsFile)) {
          private$jobResultsFile[[jIDChar]] <- jobResultsFile
          return(100L)
        }
        if (dir.exists(dirname(jobResultsFile)) &&
          identical(unlink(dirname(jobResultsFile),
            recursive = TRUE, force = TRUE
          ), 1L)) {
          stop(sprintf(
            "Problems removing existing directory: '%s'.",
            jobResultsFile
          ), call. = FALSE)
        }
        if (!dir.create(dirname(jobResultsFile), recursive = TRUE)) {
          stop("Problems creating temporary directory for saving results.",
            call. = FALSE
          )
        }
        resultInfoResp <- private$adapter$getResultsInfo(pid, isHcJob)
        if (!identical(status_code(resultInfoResp), 200L)) {
          stop(status_code(resultInfoResp), call. = FALSE)
        }
        fileSize <- suppressWarnings(
          as.integer(headers(resultInfoResp)[["content-length"]])
        )
        if (!length(fileSize) || is.na(fileSize)) {
          stop(sprintf(
            "Could not determine file size of job results (job id: '%s').",
            jIDChar
          ), call. = FALSE)
        }
        private$resultFileSize[[jIDChar]] <- fileSize
        private$jobResultsFile[[jIDChar]] <- jobResultsFile
        private$mJobRes[[jIDChar]] <- private$adapter$getResults(pid, paste0(private$jobResultsFile[[jIDChar]], ".dl"), isHcJob)
        return(5L)
      }
      if (!length(private$resultFileSize[[jIDChar]])) {
        if (!unresolved(private$mJobRes[[jIDChar]])) {
          stop("Future is still running, but no file size determined. This should never happen!",
            call. = FALSE
          )
        }
      }
      bytesDownloaded <- file.info(paste0(private$jobResultsFile[[jIDChar]], ".dl"))[["size"]]
      if (is.na(bytesDownloaded)) {
        return(5L)
      }
      if (identical(private$resultFileSize[[jIDChar]], bytesDownloaded)) {
        return(99L)
      }
      return(max(5L, round(bytesDownloaded / private$resultFileSize[[jIDChar]] * 100)))
    },
    readTextEntry = function(...) {
      return(private$adapter$readTextEntry(...))
    },
    getHcubeJobProgress = function(jID) {
      resp <- private$getJobStatus(self$getPid(jID), isHcJob = TRUE)$resp
      return(c(resp$finished, resp$job_count, resp$successfully_finished))
    }
  ), private = list(
    dbJobSchema = NULL,
    jobList = NULL,
    jobListInit = FALSE,
    db = NULL,
    conn = NULL,
    adapter = NULL,
    uid = NULL,
    mJobRes = list(),
    jobResultsFile = list(),
    resultFileSize = list(),
    getJobStatus = function(pID, isHcJob = FALSE) {
      return(tryCatch(
        {
          statusTmp <- private$adapter$getJobStatus(pID, isHcJob)
          if (isHcJob) {
            if (!length(statusTmp$results)) {
              stop(404L, call. = FALSE)
            }
            statusTmp <- statusTmp$results[[1L]]
            if (identical(statusTmp$finished, statusTmp$job_count)) {
              status <- JOBSTATUSMAP[["completed"]]
            } else {
              status <- JOBSTATUSMAP[["running"]]
            }
            return(list(status = status, gamsRetCode = NULL, resp = statusTmp))
          }
          if (identical(statusTmp$status, 10L)) {
            # job finished successfully
            return(list(
              status = JOBSTATUSMAP[["completed"]],
              gamsRetCode = statusTmp$process_status,
              resp = statusTmp
            ))
          }
          if (identical(statusTmp$status, 0L)) {
            # job queued
            return(list(
              status = JOBSTATUSMAP[["queued"]],
              gamsRetCode = NULL,
              resp = statusTmp
            ))
          }
          if (statusTmp$status %in% c(-3, -1)) {
            # job cancelled or corrupted
            return(list(
              status = JOBSTATUSMAP[["corrupted"]],
              gamsRetCode = NULL,
              resp = statusTmp
            ))
          }
          return(list(
            status = JOBSTATUSMAP[["running"]],
            gamsRetCode = NULL,
            resp = statusTmp
          ))
        },
        error = function(err) {
          errMsg <- conditionMessage(err)
          if (errMsg == 405L) {
            return(list(
              status = JOBSTATUSMAP[["corrupted(noProcess)"]],
              gamsRetCode = NULL,
              resp = NULL
            ))
          } else if (errMsg == 404L) {
            return(list(
              status = JOBSTATUSMAP[["corrupted(noProcess)"]],
              gamsRetCode = NULL,
              resp = NULL
            ))
          } else if (errMsg == -404L) {
            stop(404L, call. = FALSE)
          } else {
            stop(errMsg, call. = FALSE)
          }
        }
      ))
    }
  )
)


Worker <- R6Class("Worker",
  public = list(
    asyncJobManager = NULL,
    initialize = function(adapter, db, dbJobSchema) {
      private$adapter <- adapter
      if (adapter$supportsAsync) {
        self$asyncJobManager <- AsyncJobManager$new(dbJobSchema, db, adapter)
      }
      return(invisible(self))
    },
    run = function(solveOptions = NULL, name = NULL, tags = NULL) {
      private$jID <- NULL
      private$hardKill <- FALSE
      if (private$adapter$supportsAsync) {
        private$jobInfo <- list(name = name, tags = tags, sid = private$adapter$inputData$getSid())
        private$adapter$run(solveOptions, stri_sub(name, 1, 255))
        flog.trace("New synchronous is being submitted.")
      } else {
        procId <- private$adapter$run(solveOptions, stri_sub(name, 1, 255))
        flog.info("New synchronous job with process ID: %s submitted.", procId)
      }
      return(invisible(self))
    },
    runAsync = function(solveOptions = NULL, name = NULL, tags = NULL) {
      stopifnot(private$adapter$supportsAsync)
      subResp <- private$adapter$runAsync(solveOptions, stri_sub(name, 1, 255))[]
      if (is_mirai_error(subResp)) {
        stop(subResp$message, call. = FALSE)
      }
      quotaWarning <- NULL
      if (length(subResp$response$quota_warning)) {
        quotaWarning <- calcRemainingQuota(subResp$response$quota_warning)
        quotaWarning$error <- FALSE
      } else if (length(subResp$response$exceeded_quotas)) {
        quotaWarning <- calcRemainingQuota(subResp$response$exceeded_quotas)
        quotaWarning$error <- TRUE
      }
      if (!identical(subResp$status_code, 201L)) {
        flog.info("Problems submitting job (status code: %d). Response: %s", subResp$status_code, subResp$response)
        return(list(error = TRUE, status = subResp$status_code, quotaWarning = quotaWarning))
      }
      procId <- subResp$response$token
      jobId <- self$asyncJobManager$addJob(pid = procId, sid = private$adapter$inputData$getSid(), name = name, tags = tags, isHcJob = FALSE)
      flog.info("New asynchronous job with token: %s and job ID: %d submitted successfully.", procId, jobId)
      return(list(pid = procId, jid = jobId, quotaWarning = quotaWarning, error = FALSE))
    },
    runHypercube = function(solveOptions = NULL, dynamicPar = NULL, sid = NULL, tags = NULL) {
      stopifnot(private$adapter$supportsAsync)
      if (length(tags)) {
        jobName <- stri_sub(paste(tags, collapse = ","), 1, 255)
      } else {
        jobName <- NULL
      }
      subResp <- private$adapter$runHypercube(dynamicPar, solveOptions, jobName)[]
      if (is_mirai_error(subResp)) {
        stop(subResp$message, call. = FALSE)
      }
      quotaWarning <- NULL
      if (length(subResp$response$quota_warning)) {
        quotaWarning <- calcRemainingQuota(subResp$response$quota_warning)
        quotaWarning$error <- FALSE
      } else if (length(subResp$response$exceeded_quotas)) {
        quotaWarning <- calcRemainingQuota(subResp$response$exceeded_quotas)
        quotaWarning$error <- TRUE
      }
      if (!identical(subResp$status_code, 201L)) {
        flog.info("Problems submitting Hypercube job (status code: %d). Response: %s", subResp$status_code, subResp$response)
        return(list(error = TRUE, status = subResp$status_code, quotaWarning = quotaWarning))
      }
      procId <- subResp$response$hypercube_token
      jobId <- self$asyncJobManager$addJob(pid = procId, sid = sid, tags = tags, isHcJob = TRUE)
      flog.info("New Hypercube job with token: %s and job ID: %s submitted successfully.", procId, jobId)
      return(list(pid = procId, jid = jobId, quotaWarning = quotaWarning, error = FALSE))
    },
    interrupt = function() {
      if (private$hardKill) {
        hardKill <- TRUE
      } else {
        hardKill <- FALSE
        private$hardKill <- TRUE
      }
      flog.debug("Request to interrupt job sent (hardkill: %s).", hardKill)
      return(private$adapter$interrupt(hardKill))
    },
    getReactiveLog = function(session) {
      if (!is.null(private$logObs)) {
        private$logObs$destroy()
      }
      private$logParseDelay <- FALSE
      reactiveLogObsTmp <- reactivePoll2(private$adapter$pollInterval, session, checkFunc = function() {
        if (is.integer(private$adapter$processStatus)) {
          if (private$logParseDelay) {
            session$sendCustomMessage("gms-parseLog", list())
            private$logObs$destroy()
          } else if (private$adapter$processStatus > 0) {
            # did not solve successfully - need to delay parsing until log has been updated
            private$logParseDelay <- TRUE
          } else {
            private$logObs$destroy()
          }
        }
        private$adapter$pingLog()
      }, valueFunc = function() {
        return(private$adapter$log)
      })
      private$logObs <- reactiveLogObsTmp$obs
      return(reactiveLogObsTmp$re)
    },
    getReactiveStatus = function(session) {
      if (!is.null(private$statusObs)) {
        private$statusObs$destroy()
      }
      reactiveStatusTmp <- reactivePoll2(private$adapter$pollInterval, session, checkFunc = function() {
        if (is.integer(private$adapter$processStatus)) {
          private$statusObs$destroy()
        }
        private$adapter$pingProcess()
      }, valueFunc = function() {
        procStat <- private$adapter$processStatus
        if (length(private$jobInfo) && length(private$adapter$processId)) {
          private$jID <- self$asyncJobManager$addJob(pid = private$adapter$processId, sid = private$jobInfo$sid, name = private$jobInfo$name, tags = private$jobInfo$tags, isHcJob = FALSE)
          flog.info("New synchronous job with token: %s and job ID: %d submitted successfully.", private$adapter$processId, private$jID)
          private$jobInfo <- NULL
        }
        return(procStat)
      })
      private$statusObs <- reactiveStatusTmp$obs
      return(reactiveStatusTmp$re)
    },
    destroyObservers = function() {
      if (!is.null(private$statusObs)) {
        private$statusObs$destroy()
        private$statusObs <- NULL
      }
      if (!is.null(private$logObs)) {
        private$logObs$destroy()
        private$logObs <- NULL
      }
      return(invisible(self))
    },
    getQuotaWarning = function() {
      return(private$adapter$quotaWarning)
    },
    updateJobStatus = function(newStatus) {
      if (is.null(self$asyncJobManager)) {
        return(invisible(self))
      }
      return(self$asyncJobManager$updateJobStatus(newStatus, jID = private$jID, pID = private$adapter$processId))
    },
    getJobId = function() {
      return(private$jID)
    }
  ),
  active = list(
    inputData = function(newInputData) {
      if (missing(newInputData)) {
        return(private$adapter$inputData)
      }
      private$adapter$inputData <- newInputData
    }
  ),
  private = list(
    metadata = NULL,
    jID = NULL,
    jobInfo = NULL,
    adapter = NULL,
    statusObs = NULL,
    logObs = NULL,
    jobList = NULL,
    hardKill = FALSE,
    logParseDelay = FALSE
  )
)

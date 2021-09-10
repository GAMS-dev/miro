Worker <- R6Class("Worker", public = list(
  initialize = function(metadata, remote, db = NULL) {
    stopifnot(
      is.list(metadata), is.logical(remote),
      identical(length(remote), 1L)
    )
    private$remote <- remote
    private$metadata <- metadata
    if (length(db)) {
      private$db <- db
      private$conn <- db$getConn()
      jobMetaSchema <- dbSchema$getDbSchema("_jobMeta")
      private$dbColNames <- jobMetaSchema$colNames
      private$dbTabName <- jobMetaSchema$tabName
    }
    return(invisible(self))
  },
  logout = function() {
    private$metadata$url <- ""
    private$metadata$username <- ""
    private$metadata$password <- ""
    private$metadata$namespace <- ""
    private$metadata$useRegistered <- FALSE
    private$authHeader <- character(0L)
    unlink(private$metadata$rememberMeFileName, force = TRUE)
  },
  setCredentials = function(url, username, password, namespace,
                            useRegistered, useBearer = TRUE) {
    engineUrl <- trimws(url, which = "right", whitespace = "/")
    if (!endsWith(engineUrl, "/api")) {
      engineUrl <- paste0(engineUrl, "/api")
    }
    private$metadata$url <- engineUrl
    private$metadata$username <- username
    private$metadata$useRegistered <- useRegistered
    private$metadata$password <- password
    private$metadata$namespace <- namespace

    private$authHeader <- private$buildAuthHeader(useBearer)
    return(invisible(self))
  },
  login = function(url, username, password, namespace,
                   rememberMeFlag = FALSE,
                   useRegistered = FALSE) {
    stopifnot(isFALSE(private$metadata$noNeedCred))

    private$metadata$url <- private$resolveRemoteURL(url)

    if (!private$testConnection()) {
      private$metadata$url <- ""
      stop(426, call. = FALSE)
    }
    private$metadata$username <- username
    private$metadata$useRegistered <- useRegistered
    private$metadata$password <- password
    private$authHeader <- private$buildAuthHeader()

    private$metadata$namespace <- namespace

    if (!length(private$apiInfo) || is.na(private$apiInfo$apiVersionInt) ||
      private$apiInfo$apiVersionInt > 210603L) {
      ret <- GET(
        url = paste0(
          private$metadata$url, "/namespaces/", namespace, "/permissions?username=",
          URLencode(username)
        ),
        add_headers(
          Authorization = private$authHeader,
          Timestamp = as.character(Sys.time(), usetz = TRUE)
        ),
        timeout(10L)
      )
    } else {
      ret <- GET(
        url = paste0(private$metadata$url, "/namespaces/", namespace, "/permissions/me"),
        add_headers(
          Authorization = private$authHeader,
          Timestamp = as.character(Sys.time(), usetz = TRUE)
        ),
        timeout(10L)
      )
    }
    if (identical(status_code(ret), 404L)) {
      stop(444L, call. = FALSE)
    }
    if (identical(status_code(ret), 401L)) {
      stop(401L, call. = FALSE)
    }

    retContent <- tryCatch(
      {
        content(ret,
          type = "application/json",
          encoding = "utf-8"
        )
      },
      error = function(e) {
        stop(404L, call. = FALSE)
      }
    )
    if (identical(status_code(ret), 404L)) {
      stop(400L, call. = FALSE)
    }

    if (identical(status_code(ret), 200L)) {
      permissionLevel <- retContent$permission

      if (identical(useRegistered, TRUE)) {
        if (permissionLevel < 5L) {
          stop(403L, call. = FALSE)
        }

        ret <- GET(
          url = paste0(
            private$metadata$url, "/namespaces/", namespace, "?model=",
            URLencode(private$metadata$modelId)
          ),
          add_headers(
            Authorization = private$authHeader,
            Timestamp = as.character(Sys.time(), usetz = TRUE),
            "X-Fields" = "name"
          ),
          timeout(3L)
        )
        retContent <- tryCatch(
          {
            content(ret,
              type = "application/json",
              encoding = "utf-8"
            )
          },
          error = function(e) {
            return("Invalid JSON")
          }
        )
        if (!identical(status_code(ret), 200L)) {
          stop(sprintf(
            "Problems fetching models on namespace: %s, Error message: %s",
            namespace, retContent
          ), call. = FALSE)
        }
        if (length(retContent) < 1L) {
          stop(445L, call. = FALSE)
        }
        if (rememberMeFlag) {
          private$saveLoginCredentials(
            private$metadata$url, username,
            namespace,
            useRegistered
          )
        }
        return(200L)
      }
      if (permissionLevel < 7L) {
        stop(403L, call. = FALSE)
      }
      if (rememberMeFlag) {
        private$saveLoginCredentials(
          private$metadata$url, username,
          namespace,
          useRegistered
        )
      }
      return(200L)
    } else if (identical(status_code(ret), 403L)) {
      stop(401L, call. = FALSE)
    } else {
      stop(500L, call. = FALSE)
    }
    return(invisible(self))
  },
  getCredentials = function() {
    return(list(
      url = private$metadata$url,
      user = private$metadata$username,
      ns = private$metadata$namespace,
      reg = private$metadata$useRegistered
    ))
  },
  validateCredentials = function() {
    cred <- c(
      private$metadata$url,
      private$metadata$password,
      private$metadata$namespace
    )
    if (private$remote &&
      !identical(private$metadata$noNeedCred, TRUE) &&
      (!length(cred) ||
        any(vapply(cred, private$isEmptyString,
          logical(1L),
          USE.NAMES = FALSE
        )))) {
      return(FALSE)
    }
    return(TRUE)
  },
  setWorkDir = function(workDir) {
    private$workDir <- workDir
  },
  getInputData = function() private$inputData,
  setInputData = function(inputData) {
    private$inputData <- inputData
    return(invisible(self))
  },
  run = function(sid = NULL, name = NULL) {
    private$initRun(sid)

    if (private$remote) {
      private$runRemote(name = name)
      return(0L)
    }
    private$runLocal()
    return(0L)
  },
  runAsync = function(sid = NULL, tags = NULL,
                      dynamicPar = NULL, name = NULL) {
    stopifnot(private$remote)

    private$runRemote(dynamicPar, name = name)
    tryCatch(
      {
        remoteSubValue <- as.character(value(private$fRemoteSub))
      },
      error = function(e) {
        errMsg <- conditionMessage(e)
        flog.error(errMsg)
        if (startsWith(errMsg, "Could not") || startsWith(errMsg, "Timeout was")) {
          stop(404L, call. = FALSE)
        } else {
          stop(500L, call. = FALSE)
        }
      }
    )
    if (startsWith(remoteSubValue, "error:")) {
      flog.info(paste0("Could not execute model remotely: ", remoteSubValue))
      errCode <- suppressWarnings(as.integer(substring(remoteSubValue, 7L, 9L)))
      if (is.na(errCode)) {
        stop(500L, call. = FALSE)
      } else {
        stop(errCode, call. = FALSE)
      }
    } else {
      private$process <- remoteSubValue
      if (length(private$db)) {
        tryCatch(
          {
            private$jID <- self$addJobDb(private$process, sid,
              tags = tags, name = name,
              isHcJob = is.R6(dynamicPar)
            )
          },
          error = function(e) {
            flog.warn(
              "Could not add job to database. Error message; '%s'.",
              conditionMessage(e)
            )
          }
        )
      }
    }
    flog.trace("Job submitted successfuly. Job process ID: '%s'.", private$process)
    private$fRemoteSub <- NULL
    return(private$process)
  },
  runHcube = function(dynamicPar = NULL, sid = NULL, tags = NULL,
                      attachmentFilePaths = NULL) {
    stopifnot(private$remote)
    req(length(private$db) > 0L)

    private$initRun(sid)

    pID <- self$runAsync(
      sid = sid,
      tags = tags, dynamicPar = dynamicPar
    )

    self$updateJobStatus(status = JOBSTATUSMAP[["running"]], updatePid = !private$remote)
    flog.trace("Process ID: '%s' added to Hypercube job ID: '%s'.", pID, private$jID)
    return(0L)
  },
  interrupt = function(hardKill = NULL, process = NULL, isHcJob = FALSE) {
    if (is.null(process)) {
      if (is.null(private$process)) {
        return()
      }
      process <- private$process
    }
    if (is.null(hardKill)) {
      hardKill <- FALSE
      if (private$hardKill) {
        hardKill <- TRUE
      } else {
        private$hardKill <- TRUE
      }
    }

    if (private$remote) {
      return(private$interruptRemote(hardKill, process, isHcJob = isHcJob))
    }

    return(private$interruptLocal(hardKill, process))
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
  updateJobStatus = function(status, jID = NULL, tags = NULL, updatePid = FALSE) {
    if (!private$remote) {
      return()
    }
    tags <- NULL
    isHcJob <- FALSE
    if (is.null(jID)) {
      req(length(private$process) > 0L)

      jID <- private$jID
      if (private$remote) {
        pID <- private$process
      } else {
        pID <- private$process$get_pid()
      }
    } else {
      pID <- self$getPid(jID)
      isHcJob <- identical(
        self$getInfoFromJobList(jID, "_scode"),
        SCODEMAP[["hcube_jobconfig"]]
      )
    }
    if (!is.integer(jID) || jID < 0L) {
      flog.warn("Could not update job status as job ID is invalid.")
      return()
    }
    if (length(pID) == 0L) {
      flog.warn("Could not update job status as job process ID is invalid.")
      return()
    }
    gamsRetCode <- NULL
    if (status %in% c(
      JOBSTATUSMAP[["imported"]],
      JOBSTATUSMAP[["discarded"]]
    )) {
      jobStatus <- private$getJobStatus(pID, jID, isHcJob = isHcJob)
      gamsRetCode <- jobStatus$gamsRetCode
      if (identical(status, JOBSTATUSMAP[["discarded"]])) {
        if (jobStatus$status >= JOBSTATUSMAP[["corrupted"]] &&
          jobStatus$status < JOBSTATUSMAP[["discarded"]]) {
          status <- JOBSTATUSMAP[["discarded(corrupted)"]]
        } else if (identical(jobStatus$status, JOBSTATUSMAP[["running"]]) ||
          identical(jobStatus$status, JOBSTATUSMAP[["queued"]])) {
          self$interrupt(hardKill = TRUE, process = pID, isHcJob = isHcJob)
          status <- JOBSTATUSMAP[["discarded(running)"]]
        } else if (identical(jobStatus$status, JOBSTATUSMAP[["completed"]])) {
          if (private$remote) {
            private$removeJobResults(pID, isHcJob = isHcJob)
          }
          status <- JOBSTATUSMAP[["discarded(completed)"]]
        }
      }
    }

    colNames <- private$dbColNames[["status"]]
    values <- status

    if (length(gamsRetCode)) {
      colNames <- c(colNames, private$dbColNames[["gamsret"]])
      values <- c(values, gamsRetCode)
    }
    if (length(tags)) {
      colNames <- c(colNames, private$dbColNames[["tag"]])
      values <- c(values, tags)
    }
    if (updatePid) {
      colNames <- c(colNames, private$dbColNames[["pid"]])
      values <- c(values, pID)
    }
    private$db$updateRows("_jobMeta",
      tibble(private$dbColNames[[1L]], jID),
      colNames = colNames, values = values
    )

    return(invisible(self))
  },
  getJobList = function(jobHist = FALSE) {
    newCompleted <- FALSE
    jobList <- private$db$importDataset("_jobMeta",
      tibble(
        c(
          private$dbColNames[["uid"]],
          private$dbColNames[["status"]],
          private$dbColNames[["scode"]]
        ),
        c(
          private$metadata$uid,
          JOBSTATUSMAP[["discarded"]],
          SCODEMAP[["hcube_jobconfig"]]
        ),
        c(
          "=", if (jobHist) ">=" else "<",
          ">="
        )
      ),
      orderBy = private$dbColNames[["time"]], orderAsc = FALSE
    )

    # TODO: allow import of jobs solved via Engine if in local mode and vice versa...
    if (length(jobList)) {
      if (private$remote) {
        jobList <- filter(jobList, grepl("-", !!sym(private$dbColNames[["pid"]]), fixed = TRUE))
      } else {
        jobList <- filter(jobList, !grepl("-", !!sym(private$dbColNames[["pid"]]), fixed = TRUE))
      }
    }
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

      jobStatus <- private$getJobStatus(pIDs[i], jIDs[i],
        isHcJob = identical(
          private$jobList[["_scode"]][i],
          SCODEMAP[["hcube_jobconfig"]]
        )
      )
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
  getPid = function(jID) {
    return(self$getInfoFromJobList(jID, "_pid"))
  },
  getSid = function(jID) {
    if (!length(private$jobList) || !nrow(private$jobList)) {
      return(NULL)
    }
    jIDs <- private$jobList[[1]]
    jIdx <- match(jID, jIDs)
    if (is.na(jIdx)) {
      return(NULL)
    }
    return(private$jobList[[7]][[jIdx]])
  },
  getJobName = function(jID) {
    if (!length(private$jobList) || !nrow(private$jobList)) {
      return(NULL)
    }
    jIDs <- private$jobList[[1]]
    jIdx <- match(jID, jIDs)
    if (is.na(jIdx)) {
      return(NULL)
    }
    return(private$jobList[[10]][[jIdx]])
  },
  getStatus = function(jID) {
    if (!length(private$jobList) || !nrow(private$jobList)) {
      return(NULL)
    }
    jIDs <- private$jobList[[1]]
    jIdx <- match(jID, jIDs)
    if (is.na(jIdx)) {
      return(NULL)
    }
    return(private$jobList[[3]][[jIdx]])
  },
  getHcubeJobProgress = function(jID) {
    if (is.na(jID) || length(jID) != 1L) {
      stop(sprintf("Invalid job ID: '%s'.", jID), call. = FALSE)
    }
    stopifnot(private$remote)
    return(private$getHcubeJobProgressRemote(jID))
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
    if (!private$remote) {
      return(invisible(self))
    }
    jIDChar <- as.character(jID)
    private$fJobRes[[jIDChar]] <- NULL
    if (file.exists(private$jobResultsFile[[jIDChar]])) {
      if (identical(unlink(private$jobResultsFile[[jIDChar]],
        force = TRUE
      ), 1L)) {
        flog.error(
          "Problems removing job file: '%s'.",
          private$jobResultsFile[[jIDChar]]
        )
      }
    }
    private$jobResultsFile[[jIDChar]] <- NULL
    private$resultFileSize[[jIDChar]] <- NULL
    return(invisible(self))
  },
  getJobResults = function(jID) {
    req(private$remote)
    isHcJob <- identical(
      self$getInfoFromJobList(jID, "_scode"),
      SCODEMAP[["hcube_jobconfig"]]
    )
    jIDChar <- as.character(jID)
    if (length(private$fJobRes[[jIDChar]]) && resolved(private$fJobRes[[jIDChar]])) {
      if (isHcJob) {
        private$removeJobResults(self$getPid(jID), isHcJob = isHcJob)
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
        if (!identical(value(private$fJobRes[[jIDChar]]), 0L)) {
          stop(sprintf(
            "Problems downloading results of job: '%s'. Error message: '%s'.",
            jIDChar, value(private$fJobRes[[jIDChar]])
          ), call. = FALSE)
        }
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

      private$fJobRes[[jIDChar]] <- NULL
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
      ret <- HEAD(
        url = paste0(
          private$metadata$url,
          if (isHcJob) "/hypercube/" else "/jobs/",
          self$getPid(jID), "/result"
        ),
        add_headers(
          Authorization = private$authHeader,
          Timestamp = as.character(Sys.time(), usetz = TRUE)
        ),
        timeout(10L)
      )
      if (!identical(status_code(ret), 200L)) {
        stop(status_code(ret), call. = FALSE)
      }


      fileSize <- suppressWarnings(
        as.integer(headers(ret)[["content-length"]])
      )

      if (!length(fileSize) || is.na(fileSize)) {
        stop(sprintf(
          "Could not determine file size of job results (job id: '%s').",
          jIDChar
        ), call. = FALSE)
      }

      private$resultFileSize[[jIDChar]] <- fileSize
      private$jobResultsFile[[jIDChar]] <- jobResultsFile

      if (isHcJob) {
        private$fJobRes[[jIDChar]] <- future(
          {
            library(httr)
            private$getRemoteHcubeResults(resultsPath, pID)
          },
          globals = list(
            private = private, pID = self$getPid(jID),
            resultsPath = paste0(private$jobResultsFile[[jIDChar]], ".dl")
          )
        )
      } else {
        private$fJobRes[[jIDChar]] <- future({
          library(httr)
          private$readRemoteOutput(self$getPid(jID),
            workDir = dirname(private$jobResultsFile[[jIDChar]]),
            resultsPath = paste0(private$jobResultsFile[[jIDChar]], ".dl")
          )
        })
      }
      return(5L)
    }

    if (!length(private$resultFileSize[[jIDChar]])) {
      if (length(private$fJobRes[[jIDChar]])) {
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
  readTextEntity = function(name, jID, chunkNo = 0L, getSize = FALSE) {
    req(private$remote)
    return(private$readRemoteTextEntity(name, jID,
      saveDisk = FALSE,
      maxSize = private$metadata$maxSizeToRead,
      chunkNo = chunkNo, getSize = getSize
    ))
  },
  getRemoteAccessGroups = function() {
    stopifnot(private$remote)
    ret <- GET(
      url = paste0(
        private$metadata$url,
        "/namespaces/",
        private$metadata$namespace, "/user-groups"
      ),
      add_headers(
        Authorization = private$authHeader,
        Timestamp = as.character(Sys.time(), usetz = TRUE)
      ),
      timeout(5L)
    )
    groupsTmp <- unlist(lapply(content(ret), function(accessGroup) {
      return(c(
        paste0("#", accessGroup$label),
        vapply(accessGroup$members, "[[", character(1L), "username", USE.NAMES = FALSE)
      ))
    }), use.names = FALSE)
    groupsTmp <- groupsTmp[!tolower(groupsTmp) %in% c("#admins", "#users")]
    groupsTmp <- groupsTmp[!duplicated(groupsTmp)]
    return(c("#users", if ("#admins" %in% tolower(private$db$getUserAccessGroups())) "#admins", groupsTmp))
  },
  pingLog = function() {
    if (inherits(private$process, "process")) {
      return(private$pingLocalLog())
    }
    if (private$metadata$hiddenLogFile &&
      !private$streamEntryQueueFinished &&
      !(identical(private$status, "s") || identical(private$status, "q"))) {
      private$log <- private$readStreamEntity(
        private$process,
        private$metadata$miroLogFile
      )
      if (!identical(private$log, "")) {
        private$updateLog <- private$updateLog + 1L
      }
    }
    return(private$updateLog)
  },
  pingProcess = function() {
    if (is.integer(private$status)) {
      return(private$status)
    }
    if (inherits(private$process, "process")) {
      return(private$pingLocalProcess())
    }
    return(private$pingRemoteProcess())
  },
  getReactiveLog = function(session) {
    if (private$metadata$hiddenLogFile &&
      inherits(private$process, "process")) {
      return(reactiveFileReaderAppend(
        500, session,
        file.path(
          private$workDir,
          private$metadata$miroLogFile
        )
      ))
    }
    return(reactivePoll2(500, session, checkFunc = function() {
      self$pingLog()
    }, valueFunc = function() {
      log <- private$log
      private$log <- ""
      return(log)
    }))
  },
  getReactiveStatus = function(session) {
    return(reactivePoll2(1100, session, checkFunc = function() {
      self$pingProcess()
    }, valueFunc = function() {
      private$status
    }))
  },
  addJobDb = function(pid, sid, tags = NULL, status = NULL, name = NULL, isHcJob = FALSE) {
    stopifnot(length(pid) == 1)

    if (!is.null(sid)) {
      stopifnot(length(sid) == 1, is.integer(sid))
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
    colNames <- private$dbColNames
    tabName <- private$dbTabName
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
          " (", DBI::dbQuoteIdentifier(private$conn, colNames[[2]]), ",",
          dbQuoteIdentifier(private$conn, colNames[[3]]), ",",
          dbQuoteIdentifier(private$conn, colNames[[4]]), ",",
          dbQuoteIdentifier(private$conn, colNames[[5]]), ",",
          dbQuoteIdentifier(private$conn, colNames[[6]]), ",",
          dbQuoteIdentifier(private$conn, colNames[[7]]), ",",
          dbQuoteIdentifier(private$conn, colNames[[9]]), ",",
          dbQuoteIdentifier(private$conn, colNames[[10]]),
          ") VALUES (",
          dbQuoteLiteral(private$conn, private$metadata$uid), ",",
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
          if (inherits(private$conn, "PqConnection")) {
            paste0(" RETURNING", dbQuoteIdentifier(private$conn, colNames[[1]]))
          }
        )
        if (inherits(private$conn, "PqConnection")) {
          jID <- dbGetQuery(private$conn, SQL(query))[[1L]][1L]
        } else {
          private$db$runQuery(query)
          # need to send second SQL statement because SQLite doesn't support RETURNING function
          query <- paste0(
            "SELECT last_insert_rowid() FROM ",
            dbQuoteIdentifier(private$conn, tabName)
          )
          jID <- dbGetQuery(private$conn, SQL(query))[[1]][1L]
        }
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
  }
), private = list(
  remote = logical(1L),
  status = NULL,
  jID = NULL,
  jobName = NULL,
  db = NULL,
  conn = NULL,
  jobListInit = FALSE,
  dbTabName = character(1L),
  dbColNames = character(1L),
  sid = NULL,
  metadata = NULL,
  apiInfo = NULL,
  inputData = NULL,
  log = character(1L),
  authHeader = character(1L),
  process = NULL,
  workDir = NULL,
  hardKill = FALSE,
  updateLog = 0L,
  streamEntryQueueFinished = FALSE,
  gamsRet = NULL,
  waitCnt = integer(1L),
  wait = integer(1L),
  fRemoteSub = NULL,
  fJobRes = list(),
  jobResultsFile = list(),
  resultFileSize = list(),
  fRemoteRes = NULL,
  jobList = NULL,
  runLocal = function() {
    stopifnot(!is.null(private$inputData))
    private$status <- NULL
    private$inputData$writeDisk(private$workDir, fileName = private$metadata$MIROGdxInName)

    gamsArgs <- c(
      if (length(private$metadata$extraClArgs)) private$metadata$extraClArgs,
      paste0('curdir="', private$workDir, '"'), "lo=3", private$metadata$clArgs,
      paste0('IDCGDXInput="', private$metadata$MIROGdxInName, '"'),
      "LstTitleLeftAligned=1"
    )
    if (private$metadata$saveTraceFile) {
      gamsArgs <- c(gamsArgs, 'trace="_scenTrc.trc"', "traceopt=3")
    }
    pfFilePath <- gmsFilePath(file.path(private$workDir, tolower(private$metadata$modelName) %+% ".pf"))
    writeLines(c(private$inputData$getClArgs(), gamsArgs), pfFilePath)

    private$process <- process$new(file.path(private$metadata$gamsSysDir, "gams"),
      args = c(private$metadata$modelGmsName, "pf", pfFilePath),
      stdout = if (private$metadata$hiddenLogFile) NULL else "|",
      windows_hide_window = TRUE,
      env = private$getProcEnv()
    )
    return(self)
  },
  runRemote = function(hcubeData = NULL, name = NULL) {
    stopifnot(!is.null(private$inputData))
    private$status <- "s"
    private$inputData$writeDisk(private$workDir,
      fileName = private$metadata$MIROGdxInName
    )
    if (!is.R6(hcubeData)) {
      private$jobName <- name
      private$inputData$copyMiroWs(private$workDir, jobName = name)
    }
    private$fRemoteSub <- future(
      {
        suppressWarnings(suppressMessages({
          library(zip)
          library(httr)
          library(R6)
          library(readr)
          library(jsonlite)
        }))

        dataFilesToFetch <- metadata$modelDataFiles

        requestBody <- list(
          model = metadata$modelId,
          run = metadata$modelGmsName,
          arguments = paste0("pf=", metadata$modelName, ".pf"),
          namespace = metadata$namespace
        )

        gamsArgs <- c(
          if (length(metadata$extraClArgs)) metadata$extraClArgs,
          metadata$clArgs
        )
        if (metadata$saveTraceFile) {
          traceFileName <- "_scenTrc.trc"
          gamsArgs <- c(gamsArgs, paste0('trace="', traceFileName, '"'), "traceopt=3")
          if (length(dataFilesToFetch)) {
            dataFilesToFetch <- c(dataFilesToFetch, traceFileName)
          }
        }
        textEntities <- ""
        if (is.R6(hcubeData)) {
          gamsArgs <- c(gamsArgs, paste0(
            'IDCGDXInput="',
            metadata$MIROGdxInName, '"'
          ))
          requestBody$hypercube_file <- upload_file(hcubeData$writeHcubeFile(workDir),
            type = "application/json"
          )
          filesToInclude <- c(
            dataFilesToFetch,
            metadata$text_entities,
            requestBody$stdout_filename
          )
          filesToInclude <- filesToInclude[!filesToInclude %in% metadata$MIROGdxInName]
        } else {
          gamsArgs <- c(gamsArgs, paste0('IDCGDXInput="', metadata$MIROGdxInName, '"'))
          if (length(metadata$text_entities)) {
            textEntities <- URLencode(paste0("?text_entries=", paste(metadata$text_entities,
              collapse = "&text_entries="
            )))
          }
          if (metadata$hiddenLogFile) {
            requestBody$stream_entries <- metadata$miroLogFile
          }
          requestBody$stdout_filename <- paste0(metadata$modelNameRaw, ".log")
          filesToInclude <- c(
            dataFilesToFetch,
            metadata$text_entities,
            requestBody$stdout_filename,
            "_miro_ws_/*"
          )
        }
        pfFilePath <- gmsFilePath(file.path(workDir, paste0(tolower(metadata$modelName), ".pf")))
        writeLines(c(pfFileContent, gamsArgs), pfFilePath)

        requestBody$inex_file <- upload_file(inputData$
          addInexFile(
          workDir,
          filesToInclude
        ),
        type = "application/json"
        )
        requestBody$data <- upload_file(inputData$
          addFilePaths(pfFilePath)$
          compress(),
        type = "application/zip"
        )

        if (identical(metadata$useRegistered, FALSE)) {
          requestBody$model_data <- upload_file(metadata[["modelData"]],
            type = "application/zip"
          )
        }
        ret <- POST(paste0(metadata$url, if (is.R6(hcubeData)) "/hypercube" else "/jobs", textEntities),
          encode = "multipart",
          body = requestBody,
          add_headers(
            Authorization = authHeader,
            Timestamp = as.character(Sys.time(),
              usetz = TRUE
            )
          ),
          timeout(120L)
        )
        if (identical(status_code(ret), 201L)) {
          if (is.R6(hcubeData)) {
            return(content(ret)$hypercube_token)
          }
          return(content(ret)$token)
        } else {
          msg <- tryCatch(content(ret, type = "application/json")$message,
            error = function(e) {
              return("")
            }
          )
          statusCode <- status_code(ret)
          if (identical(msg, "")) {
            statusCode <- 404L
          }
          return(paste0("error:", statusCode, msg))
        }
      },
      globals = list(
        metadata = private$metadata, workDir = private$workDir,
        pfFileContent = private$inputData$getClArgs(),
        inputData = private$inputData,
        authHeader = private$authHeader,
        gmsFilePath = gmsFilePath,
        isWindows = isWindows, hcubeData = hcubeData
      )
    )
    return(self)
  },
  readRemoteTextEntity = function(text_entity, jID = NULL, saveDisk = TRUE, maxSize = NULL,
                                  workDir = private$workDir, chunkNo = 0L, getSize = FALSE) {
    if (is.null(jID)) {
      jID <- private$process
    }
    if (!is.null(maxSize)) {
      ret <- HEAD(
        paste0(
          private$metadata$url, "/jobs/", jID, "/text-entry/",
          URLencode(text_entity)
        ),
        add_headers(
          Authorization = private$authHeader,
          Timestamp = as.character(Sys.time(), usetz = TRUE)
        ),
        timeout(10L)
      )

      if (!identical(status_code(ret), 200L)) {
        return(status_code(ret))
      }
      teLength <- tryCatch(
        {
          suppressWarnings(as.numeric(headers(ret)[["char_length"]]))
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

    ret <- GET(
      paste0(
        private$metadata$url, "/jobs/", jID, "/text-entry/",
        URLencode(text_entity),
        if (!is.null(teLength)) {
          paste0(
            "?start_position=", startPos,
            "&length=", min(teLength - startPos, maxSize)
          )
        }
      ),
      add_headers(
        Authorization = private$authHeader,
        Timestamp = as.character(Sys.time(), usetz = TRUE)
      ),
      timeout(10L)
    )

    if (identical(status_code(ret), 200L)) {
      if (saveDisk) {
        entityContent <- content(ret, encoding = "utf-8")$entry_value
        if (!length(entityContent)) {
          entityContent <- ""
        }
        writeLines(
          entityContent,
          file.path(workDir, text_entity)
        )
        return(200L)
      }
      if (getSize) {
        return(list(
          content = content(ret, encoding = "utf-8")$entry_value,
          chunkNo = ceiling(teLength / maxSize)
        ))
      }
      return(content(ret, encoding = "utf-8")$entry_value)
    }
    return(status_code(ret))
  },
  pingLocalProcess = function() {
    exitStatus <- private$process$get_exit_status()

    if (length(exitStatus)) {
      private$gamsRet <- exitStatus
      if (private$metadata$hiddenLogFile) {
        private$status <- exitStatus
      }
    }
    return(private$status)
  },
  pingLocalLog = function() {
    if (!length(private$process)) {
      return(private$updateLog)
    }
    private$log <- tryCatch(
      paste0(private$log, private$process$read_output()),
      error = function(e) {
        return(private$log)
      }
    )
    if (!identical(private$log, "")) {
      private$updateLog <- private$updateLog + 1L
    }
    if (length(private$gamsRet)) {
      private$status <- private$gamsRet
    }
    return(private$updateLog)
  },
  readStreamEntity = function(jID, name) {
    tryCatch(
      {
        return(private$validateAPIResponse(DELETE(
          paste0(
            private$metadata$url, "/jobs/", jID, "/stream-entry/",
            name
          ),
          add_headers(
            Authorization = private$authHeader,
            Timestamp = as.character(Sys.time(), usetz = TRUE)
          ),
          timeout(3L)
        ))$entry_value)
      },
      error = function(e) {
        statusCode <- conditionMessage(e)
        if (identical(statusCode, "308")) {
          private$streamEntryQueueFinished <- TRUE
        } else {
          flog.warn("Problems fetching stream entry. Return code: '%s'.", statusCode)
        }
        return("")
      }
    )
  },
  pingRemoteProcess = function() {
    if (private$wait > 0L) {
      private$wait <- private$wait - 1L
      return(private$status)
    }
    if (length(private$fRemoteSub)) {
      if (resolved(private$fRemoteSub)) {
        noError <- TRUE
        tryCatch(
          {
            remoteSubValue <- value(private$fRemoteSub)
          },
          error = function(e) {
            errMsg <- conditionMessage(e)
            flog.error(errMsg)
            if (startsWith(errMsg, "Could not") || startsWith(errMsg, "Timeout was")) {
              private$status <- -404L
            } else {
              private$status <- -500L
            }
            noError <<- FALSE
          }
        )
        if (!noError) {
          return(private$status)
        }
        private$wait <- 0L
        private$waitCnt <- 0L
        if (startsWith(remoteSubValue, "error:")) {
          flog.info(paste0("Could not execute model remotely: ", remoteSubValue))
          errCode <- suppressWarnings(as.integer(substring(remoteSubValue, 7L, 9L)))
          if (is.na(errCode)) {
            private$status <- -500L
          } else {
            private$status <- -errCode
          }
        } else {
          private$process <- value(private$fRemoteSub)
          if (length(private$db)) {
            tryCatch(
              {
                private$jID <- self$addJobDb(private$process, private$sid,
                  name = private$jobName
                )
              },
              error = function(e) {
                flog.warn(
                  "Could not add job to database. Error message; '%s'.",
                  conditionMessage(e)
                )
              }
            )
          }
          private$status <- "q"
        }
        private$fRemoteSub <- NULL
      } else {
        private$wait <- bitwShiftL(2L, private$waitCnt)
        if (private$waitCnt < private$metadata$timeout) {
          private$waitCnt <- private$waitCnt + 1L
        } else {
          private$status <- -404L
        }
      }
      return(private$status)
    }
    if (length(private$gamsRet)) {
      if (resolved(private$fRemoteRes)) {
        resVal <- value(private$fRemoteRes)
        private$status <- private$gamsRet
        if (!identical(resVal, 0L)) {
          if (identical(resVal, -100L)) {
            private$status <- -100L
            flog.error("Fetching results timed out.")
          } else {
            flog.error(resVal)
            private$status <- -500L
          }
        }
      } else {
        private$wait <- bitwShiftL(2L, private$waitCnt)
        if (private$waitCnt < private$metadata$timeout) {
          private$waitCnt <- private$waitCnt + 1L
        } else {
          private$status <- -404L
        }
      }
      return(private$status)
    }
    tryCatch(
      {
        ret <- DELETE(
          paste0(private$metadata$url, "/jobs/", private$process, "/unread-logs"),
          add_headers(
            Authorization = private$authHeader,
            Timestamp = as.character(Sys.time(), usetz = TRUE)
          ),
          timeout(2L)
        )
        statusCode <- status_code(ret)
      },
      error = function(e) {
        flog.warn("Problems reading log from remote executor. Error message: %s", conditionMessage(e))
        statusCode <<- 403L
      }
    )

    if (identical(statusCode, 200L)) {
      responseContent <- tryCatch(
        {
          content(ret,
            type = "application/json",
            encoding = "utf-8"
          )
        },
        error = function(e) {
          private$status <- -404L
          return(-1L)
        }
      )
      if (identical(responseContent, -1L)) {
        return(private$status)
      }
      if (identical(responseContent$queue_finished, TRUE)) {
        private$gamsRet <- responseContent$gams_return_code
        private$wait <- 0L
        private$waitCnt <- 0L
        private$fRemoteRes <- future({
          library(httr)
          private$readRemoteOutput()
        })
        private$status <- "d"
      } else {
        private$status <- NULL
      }
      if (!private$metadata$hiddenLogFile) {
        private$log <- responseContent$message
        if (!identical(private$log, "")) {
          private$updateLog <- private$updateLog + 1L
        }
      }
      return(private$status)
    } else if (identical(statusCode, 308L)) {
      # job finished, get full log
      ret <- private$getRemoteStatus(private$process)
      gamsRetCode <- content(ret)$gams_return_code

      if (is.null(gamsRetCode)) {
        private$status <- -500L
      } else {
        private$status <- gamsRetCode
      }
    } else if (identical(statusCode, 403L)) {
      private$wait <- bitwShiftL(2L, private$waitCnt)
      if (private$waitCnt < private$metadata$timeout) {
        private$waitCnt <- private$waitCnt + 1L
      } else {
        private$status <- -404L
      }
    } else if (identical(statusCode, 410L)) {
      # job canceled while queued.
      private$status <- -9L
    } else {
      private$status <- -500L
    }
    return(private$status)
  },
  readRemoteOutput = function(jID = NULL, workDir = NULL, resultsPath = NULL) {
    if (is.null(jID)) {
      jID <- private$process
    }
    if (is.null(workDir)) {
      workDir <- private$workDir
    }
    if (!length(jID)) {
      return("Process not started")
    }
    if (!length(resultsPath)) {
      resultsPath <- tempfile(pattern = "res_", fileext = ".zip")
      on.exit(unlink(resultsPath))
    }
    timeout <- FALSE
    tryCatch(
      ret <- GET(
        url = paste0(private$metadata$url, "/jobs/", jID, "/result"),
        write_disk(resultsPath),
        add_headers(
          Authorization = private$authHeader,
          Timestamp = as.character(Sys.time(), usetz = TRUE)
        ),
        timeout(36000)
      ),
      error = function(e) {
        timeout <<- TRUE
      }
    )
    if (timeout) {
      return(-100L)
    }
    private$removeJobResults(jID, isHcJob = FALSE)

    if (identical(status_code(ret), 200L)) {
      unzip(resultsPath, exdir = workDir)
    } else {
      return(content(ret)$message)
    }
    return(0L)
  },
  getRemoteStatus = function(jID) {
    GET(
      paste0(private$metadata$url, "/jobs/", jID),
      add_headers(
        Authorization = private$authHeader,
        Timestamp = as.character(Sys.time(), usetz = TRUE),
        "X-Fields" = "process_status,status"
      ),
      timeout(2L)
    )
  },
  interruptLocal = function(hardKill = FALSE, process = NULL) {
    errMsg <- NULL

    if (is.R6(process)) {
      tryCatch(
        {
          if (hardKill) {
            process$kill_tree()
          } else if (isWindows() && "miro.util" %in% installedPackages) {
            miro.util::windowsInterruptGAMS(process$get_pid())
          } else {
            process$signal(tools::SIGINT)
          }
        },
        error = function(e) {
          errMsg <<- "error"
          pID <<- process$get_pid()
        }
      )
    } else {
      errMsg <- "External process"
      pID <- suppressWarnings(as.integer(process))

      if (is.na(pID)) {
        flog.error("Invalid process id: '%s'. Could not interrupt job", pID)
        return(0L)
      }
      if (!pidExists(pID)) {
        return(0L)
      }
    }
    if (!is.null(errMsg)) {
      errMsg <- NULL
      flog.info("Interrupting process with pid: '%s'.", pID)
      if (private$metadata$serverOS == "windows") {
        tryCatch(
          {
            processx::run(
              command = "taskkill", args = c(
                if (hardKill) "/F",
                "/PID",
                pID,
                "/T"
              ),
              windows_hide_window = TRUE, timeout = 10L
            )
          },
          error = function(e) {
            flog.error(
              "Problems interrupting process with pid: %s. Error message: '%s'.",
              pID, conditionMessage(e)
            )
          }
        )
      } else if (private$metadata$serverOS %in% c("linux", "osx")) {
        tryCatch(
          {
            processx::run(
              command = "kill",
              args = c(
                if (hardKill) "-SIGKILL" else "-SIGINT",
                -pID
              ), timeout = 10L
            )
          },
          error = function(e) {
            flog.error(
              "Problems interrupting process with pid: %s. Error message: '%s'.",
              pID, conditionMessage(e)
            )
          }
        )
      } else {
        flog.error("Operating system: '%s' not supported.", private$metadata$serverOS)
      }
    }
    return(0L)
  },
  interruptRemote = function(hardKill = FALSE, process = NULL, isHcJob = FALSE) {
    if (!length(process)) {
      return("Process not started")
    }
    private$validateAPIResponse(DELETE(
      url = paste0(
        private$metadata$url,
        if (isHcJob) "/hypercube/" else "/jobs/", process
      ),
      body = list(hard_kill = hardKill),
      add_headers(
        Authorization = private$authHeader,
        Timestamp = as.character(Sys.time(), usetz = TRUE)
      ),
      timeout(10L)
    ))
    return(0L)
  },
  removeJobResults = function(jID, isHcJob = FALSE) {
    tryCatch(private$validateAPIResponse(
      DELETE(
        url = paste0(
          private$metadata$url,
          if (isHcJob) "/hypercube/" else "/jobs/",
          jID, "/result"
        ),
        add_headers(
          Authorization = private$authHeader,
          Timestamp = as.character(Sys.time(), usetz = TRUE)
        ),
        timeout(private$metadata$timeout)
      )
    ),
    error = function(e) {
      warning(sprintf(
        "Problems removing results of job: '%s'. Error message: '%s'.",
        jID, conditionMessage(e)
      ), call. = FALSE)
    }
    )
    return(invisible(self))
  },
  getHcubeJobProgressRemote = function(jID) {
    jobProgress <- private$validateAPIResponse(
      GET(
        url = paste0(private$metadata$url, "/hypercube/?hypercube_token=", self$getPid(jID)),
        add_headers(
          `X-Fields` = "finished,job_count,successfully_finished",
          Authorization = private$authHeader,
          Timestamp = as.character(Sys.time(), usetz = TRUE)
        ),
        timeout(10L)
      )
    )$results[[1]]
    return(c(jobProgress$finished, jobProgress$job_count, jobProgress$successfully_finished))
  },
  getHcubeJobStatusRemote = function(pID, jID) {
    jobProgress <- tryCatch(private$getHcubeJobProgressRemote(jID),
      error = function(e) {
        errMsg <- conditionMessage(e)
        if (errMsg == 405L) {
          return(-405L)
        } else if (errMsg == 404L) {
          return(-404L)
        } else if (errMsg == -404L) {
          stop(404L, call. = FALSE)
        } else {
          stop(errMsg, call. = FALSE)
        }
      }
    )
    if (!length(jobProgress)) {
      stop("Problems determining job status. If this problem persists, please contact a system administrator!",
        call. = FALSE
      )
    }
    if (length(jobProgress) == 1L &&
      jobProgress %in% c(-404L, -405L)) {
      status <- JOBSTATUSMAP[["corrupted(noProcess)"]]
    } else if (length(jobProgress) >= 2L &&
      identical(jobProgress[[1L]], jobProgress[[2L]])) {
      status <- JOBSTATUSMAP[["completed"]]
    } else {
      status <- JOBSTATUSMAP[["running"]]
    }
    return(list(status = status, gamsRetCode = NULL))
  },
  getRemoteHcubeResults = function(resultsPath, pID) {
    return(private$validateAPIResponse(
      GET(
        url = paste0(private$metadata$url, "/hypercube/", pID, "/result"),
        write_disk(resultsPath, overwrite = TRUE),
        add_headers(
          Authorization = private$authHeader,
          Timestamp = as.character(Sys.time(), usetz = TRUE)
        ),
        timeout(36000)
      )
    ))
  },
  isEmptyString = function(string) {
    if (!length(string) || identical(string, "")) {
      return(TRUE)
    }
    return(FALSE)
  },
  buildAuthHeader = function(useTokenAuth = FALSE) {
    if (useTokenAuth) {
      return(paste0("Bearer ", private$metadata$password))
    }
    return(paste0(
      "Basic ",
      base64_encode(charToRaw(
        paste0(
          private$metadata$username,
          ":", private$metadata$password
        )
      ))
    ))
  },
  saveLoginCredentials = function(url, username, namespace, useRegistered) {
    # create token that expires in a week
    sessionToken <- private$validateAPIResponse(POST(
      url = paste0(url, "/auth/login"),
      body = list(
        expires_in = 604800,
        username = username,
        password = private$metadata$password
      ),
      add_headers(Timestamp = as.character(Sys.time(), usetz = TRUE)),
      timeout(2L)
    ))$token

    private$metadata$password <- sessionToken
    write_json(list(
      url = url,
      username = username,
      password = sessionToken,
      namespace = namespace,
      reg = useRegistered
    ),
    private$metadata$rememberMeFileName,
    auto_unbox = TRUE
    )
    return(invisible(self))
  },
  initRun = function(sid) {
    private$sid <- sid
    private$log <- character(1L)
    private$streamEntryQueueFinished <- FALSE
    private$gamsRet <- NULL
    private$fRemoteRes <- NULL
    private$fRemoteSub <- NULL
    private$jID <- NULL
    private$hardKill <- FALSE
    private$updateLog <- 0L
    private$wait <- 0L
    private$waitCnt <- 0L

    return(invisible(self))
  },
  getJobStatus = function(pID, jID = NULL, isHcJob = FALSE) {
    if (isHcJob) {
      return(private$getHcubeJobStatus(pID, jID))
    }
    stopifnot(private$remote)
    return(tryCatch(
      {
        statusInfo <- private$validateAPIResponse(
          private$getRemoteStatus(pID)
        )
        if (identical(statusInfo$status, 10L)) {
          # job finished successfully
          return(list(
            status = JOBSTATUSMAP[["completed"]],
            gamsRetCode = statusInfo$process_status
          ))
        }
        if (identical(statusInfo$status, 0L)) {
          # job queued
          return(list(
            status = JOBSTATUSMAP[["queued"]],
            gamsRetCode = NULL
          ))
        }
        if (statusInfo$status %in% c(-3, -1)) {
          # job cancelled or corrupted
          return(list(
            status = JOBSTATUSMAP[["corrupted"]],
            gamsRetCode = NULL
          ))
        }
        return(list(
          status = JOBSTATUSMAP[["running"]],
          gamsRetCode = NULL
        ))
      },
      error = function(e) {
        errMsg <- conditionMessage(e)
        if (errMsg == 405L) {
          return(list(
            status = JOBSTATUSMAP[["corrupted(noProcess)"]],
            gamsRetCode = NULL
          ))
        } else if (errMsg == 404L) {
          return(list(
            status = JOBSTATUSMAP[["corrupted(noProcess)"]],
            gamsRetCode = NULL
          ))
        } else if (errMsg == -404L) {
          stop(404L, call. = FALSE)
        } else {
          stop(errMsg, call. = FALSE)
        }
      }
    ))
  },
  getHcubeJobStatus = function(pID, jID) {
    return(private$getHcubeJobStatusRemote(pID, jID))
  },
  validateAPIResponse = function(response) {
    if (status_code(response) >= 300L) {
      stop(status_code(response),
        call. = FALSE
      )
    }
    ret <- tryCatch(
      {
        content(response,
          type = "application/json",
          encoding = "utf-8"
        )
      },
      error = function(e) {
        stop(-404L, call. = FALSE)
      }
    )

    return(ret)
  },
  testConnection = function() {
    if (!startsWith(private$metadata$url, "https://") &&
      !startsWith(private$metadata$url, "http://localhost")) {
      return(FALSE)
    }
    if (tryCatch(
      {
        if (endsWith(private$metadata$url, "/api")) {
          ret <- GET(paste0(private$metadata$url, "/version"), timeout(10L))
          if (!"gams_version" %in% names(content(ret,
            type = "application/json",
            encoding = "utf-8"
          ))) {
            stop("Not a GAMS Engine server", call. = FALSE)
          }
        } else {
          ret <- GET(paste0(private$metadata$url, "/api/version"), timeout(10L))
          if ("gams_version" %in% names(content(ret,
            type = "application/json",
            encoding = "utf-8"
          ))) {
            private$metadata$url <- paste0(private$metadata$url, "/api")
          } else {
            ret <- GET(paste0(private$metadata$url, "/version"), timeout(10L))
            if (!"gams_version" %in% names(content(ret,
              type = "application/json",
              encoding = "utf-8"
            ))) {
              stop("Not a GAMS Engine server", call. = FALSE)
            }
          }
        }
        private$apiInfo <- content(ret,
          type = "application/json",
          encoding = "utf-8"
        )
        private$apiInfo$apiVersionInt <- suppressWarnings(
          as.integer(gsub(".", "", private$apiInfo$version, fixed = TRUE))
        )[1]
        ret <- ret$url
        FALSE
      },
      error = function(e) {
        flog.debug(
          "Could not connect to provided URL. Error message: '%s'.",
          conditionMessage(e)
        )
        stop(404L, call. = FALSE)
      }
    )) {
      return(FALSE)
    }

    if (startsWith(ret, "https://") ||
      identical(ret, "http://localhost") ||
      startsWith(ret, "http://localhost:") ||
      startsWith(ret, "http://localhost/")) {
      return(TRUE)
    }
    return(FALSE)
  },
  checkAuthenticationStatus = function() {
    return(status_code(HEAD(
      paste0(private$metadata$url, "/jobs"),
      add_headers(
        Authorization = private$authHeader,
        Timestamp = as.character(Sys.time(), usetz = TRUE)
      ),
      timeout(10L)
    )))
  },
  resolveRemoteURL = function(url) {
    url <- trimws(url, "right", whitespace = "/")
    if (startsWith(url, "https://")) {
      return(url)
    }
    if (grepl("(http://)?localhost([:/].*)?$", url)) {
      if (startsWith(url, "http://")) {
        return(url)
      }
      return(paste0("http://", url))
    } else if (grepl("://", url, fixed = TRUE)) {
      stop(426, call. = FALSE)
    }
    return(paste0("https://", url))
  },
  getProcEnv = function() {
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
))

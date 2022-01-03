# R6 class for Scenario object
Scenario <- R6Class("Scenario",
  inherit = Db,
  public = list(
    initialize = function(db, sid = NULL, sname = NULL,
                          readPerm = NULL, writePerm = NULL, execPerm = NULL,
                          tags = NULL, overwrite = FALSE, isNewScen = FALSE,
                          duplicatedMetadata = NULL, uid = NULL, views = NULL,
                          attachments = NULL, scode = NULL, forceOverwrite = FALSE) {
      # Initialize scenario class
      #
      # Args:
      #   db:                R6 Database object
      #   sid:               scenario Id (optional)
      #   sname:             scenario name (optional)
      #   readPerm:          users/groups that have read permissions (optional)
      #   writePerm:         users/groups that have write permissions (optional)
      #   execPerm:          users/groups that have execute permissions (optional)
      #   tags:              vector of tags that can be specified (optional)
      #   overwrite:         logical that specifies whether data should be overwritten or appended
      #   isNewScen:         whether the scenario is not yet stored in the database
      #   duplicatedMetadata: metadata information of to apply
      #   uid:               can be used to declare owner of the scenario (default is current user)
      #   views:             views object
      #   attachments:       attachments object
      #   scode:             scenario code (optional)
      #   forceOverwrite:    Whether scenario should be overwritten even if it is locked.

      # BEGIN error checks
      stopifnot(is.R6(db), is.logical(isNewScen), length(isNewScen) == 1L)
      private$conn <- db$getConn()
      private$connectionInfo <- db$getInfo()
      if (length(uid) == 1L && is.character(uid)) {
        private$uid <- uid
        private$userAccessGroups <- uid
      } else {
        private$uid <- db$getUid()
        private$userAccessGroups <- db$getUserAccessGroups()
      }
      if (is.null(sid)) {
        stopifnot(is.character(sname), length(sname) == 1)
      } else {
        sid <- suppressWarnings(as.integer(sid))
        stopifnot(!is.na(sid), length(sid) == 1)
      }
      stopifnot(is.logical(overwrite), length(overwrite) == 1)
      # END error checks

      if (length(tags)) {
        stopifnot(is.character(tags), length(tags) >= 1)
      } else {
        tags <- character(0L)
      }
      # if permissions not explicitly set, restrict read/write access to active user
      if (length(readPerm)) {
        stopifnot(is.character(readPerm), length(readPerm) >= 1)
      } else {
        readPerm <- private$uid
      }
      if (length(writePerm)) {
        stopifnot(is.character(writePerm), length(writePerm) >= 1)
      } else {
        writePerm <- private$uid
      }
      if (length(execPerm)) {
        stopifnot(is.character(execPerm), length(execPerm) >= 1)
      } else {
        execPerm <- private$uid
      }

      private$slocktimeLimit <- db$getSlocktimeLimit()
      private$tableNameScenLocks <- dbSchema$getDbTableName("_scenLock")
      private$tags <- vector2Csv(unique(tags))
      private$readPerm <- vector2Csv(readPerm)
      private$writePerm <- vector2Csv(writePerm)
      private$execPerm <- vector2Csv(execPerm)
      private$views <- views
      private$attachments <- attachments
      private$forceOverwrite <- forceOverwrite

      savedAttachConfig <- NULL

      if (is.null(scode)) {
        private$scode <- SCODEMAP[["scen"]]
      } else {
        private$scode <- scode
      }

      if (isNewScen) {
        private$stime <- Sys.time()
        private$suid <- private$uid
        private$sname <- sname
        private$removeAllExistingAttachments <- TRUE
        if (length(duplicatedMetadata$attach)) {
          savedAttachConfig <- list(
            localAttachments = duplicatedMetadata$attach$localAttachments,
            attachmentsUpdateExec = duplicatedMetadata$attach$attachmentsUpdateExec,
            attachmentsToRemove = duplicatedMetadata$attach$attachmentsToRemove
          )
          if (length(duplicatedMetadata$attach$sidToDuplicate)) {
            private$sidToDuplicate <- as.integer(duplicatedMetadata$attach$sidToDuplicate)
            private$duplicateAttachmentsOnNextSave <- TRUE
          }
        }
        if (length(duplicatedMetadata$perm)) {
          private$readPerm <- vector2Csv(
            unique(c(
              private$uid,
              csv2Vector(duplicatedMetadata$perm$readPerm)
            ))
          )
          private$writePerm <- vector2Csv(
            unique(c(
              private$uid,
              csv2Vector(duplicatedMetadata$perm$writePerm)
            ))
          )
          private$execPerm <- vector2Csv(
            unique(c(
              private$uid,
              csv2Vector(duplicatedMetadata$perm$execPerm)
            ))
          )
        }
      } else {
        if (is.null(sid)) {
          tryCatch(
            {
              private$fetchMetadata(sname = sname, uid = private$uid)
              if (overwrite) {
                private$writeMetadata()
              }
            },
            error = function(e) {
              # scenario with name/uid combination does not exist, so create it
              private$stime <- Sys.time()
              private$suid <- private$uid
              private$sname <- sname
              private$writeMetadata()
            }
          )
        } else {
          private$fetchMetadata(sid = sid)
        }
        # set/refresh lock for scenario
        private$lock()
      }
      if (!is.null(private$attachments)) {
        private$attachments$initScenData(private$sid, private$inDataSid)
        if (!is.null(savedAttachConfig)) {
          private$attachments$setConfig(savedAttachConfig)
        }
      }
      return(invisible(self))
    },
    getSid = function() private$sid,
    getScenUid = function() private$suid,
    getScenName = function() private$sname,
    getScenTime = function() private$stime,
    getStags = function() csv2Vector(private$tags),
    getReadPerm = function() csv2Vector(private$readPerm),
    getWritePerm = function() csv2Vector(private$writePerm),
    getExecPerm = function() csv2Vector(private$execPerm),
    getLockUid = function() private$lockUid,
    getScenHash = function() {
      if (length(private$sid) && is.null(private$scenHash)) {
        # need to fetch from db as it was never set
        scenHashTmp <- self$importDataset("_scenHash",
          subsetSids = private$sid
        )
        if (length(scenHashTmp) && nrow(scenHashTmp) > 0L) {
          scenHashTmp <- scenHashTmp[["hash"]][1]
        } else {
          scenHashTmp <- character()
        }
        private$scenHash <- scenHashTmp
      }
      return(private$scenHash)
    },
    setScenHash = function(scenHash) {
      private$scenHash <- scenHash
      invisible(self)
    },
    getMetadataInfo = function(discardAttach = FALSE, discardPerm = FALSE) {
      attachmentConfig <- list()
      permissionConfig <- list()
      if (!isTRUE(discardAttach) && !is.null(private$attachments)) {
        attachmentConfig <- private$attachments$getConfig()
        attachmentConfig$sidToDuplicate <- private$sid
      }
      if (!isTRUE(discardPerm)) {
        permissionConfig <- list(
          readPerm = private$readPerm,
          writePerm = private$writePerm,
          execPerm = private$execPerm
        )
      }
      return(list(
        attach = attachmentConfig,
        perm = permissionConfig
      ))
    },
    getMetadata = function(noPermFields = TRUE) {
      # Generates dataframe containing scenario metadata
      #
      # Args:
      #   noPermFields:   do not include scenario permission fields (optional)
      #
      # Returns:
      #   dataframe with metadata

      # BEGIN error checks
      stopifnot(is.logical(noPermFields), length(noPermFields) == 1L)
      # END error checks

      if (length(private$sid)) {
        sid <- private$sid
      } else {
        sid <- NA_integer_
      }

      if (noPermFields) {
        return(tibble(
          "_sid" = sid,
          "_uid" = private$suid,
          "_sname" = private$sname,
          "_stime" = private$stime,
          "_stag" = private$tags
        ))
      }
      return(tibble(
        "_sid" = sid,
        "_uid" = private$suid,
        "_sname" = private$sname,
        "_stime" = private$stime,
        "_stag" = private$tags,
        "_accessr" = private$readPerm,
        "_accessw" = private$writePerm,
        "_accessx" = private$execPerm
      ))
    },
    resetAccessPerm = function() {
      if (private$isReadonly()) {
        stop("Db: Metadata wasn't updated as scenario is readonly (Scenario.resetAccessPerm).",
          call. = FALSE
        )
      }
      private$readPerm <- vector2Csv(private$uid)
      private$writePerm <- vector2Csv(private$uid)
      private$execPerm <- vector2Csv(private$uid)
      return(invisible(self))
    },
    save = function(datasets, msgProgress = NULL) {
      # Saves multiple dataframes to database
      #
      # Args:
      #   datasets :           dataframes to save
      #   msgProgress:         title and progress info for the progress bar (optional)
      #
      # Returns:
      #   object: invisibly returns reference to itself

      # BEGIN error checks
      if (private$isReadonly()) {
        stop("Scenario is readonly. Saving failed (Scenario.save).", call. = FALSE)
      }
      addScenIdAttach <- FALSE
      if (!length(private$sid)) {
        addScenIdAttach <- TRUE
        try(private$fetchMetadata(sname = private$sname, uid = private$uid), silent = TRUE)
        if (!private$forceOverwrite) {
          private$lock()
        }
      }
      if (length(names(datasets))) {
        symNamesScenario <- names(datasets)
      } else {
        symNamesScenario <- dbSchema$getAllSymbols()
      }
      stopifnot(is.list(datasets), identical(length(datasets), length(symNamesScenario)))
      if (!is.null(msgProgress)) {
        stopifnot(is.character(msgProgress$title), length(msgProgress$title) == 1)
        stopifnot(is.character(msgProgress$progress), length(msgProgress$progress) == 1)
        # initialize progress bar
        prog <- Progress$new()
        on.exit(prog$close())
        prog$set(message = msgProgress$title, value = 0)
        updateProgress <- NULL
        incAmount <- 1 / length(symNamesScenario)
        updateProgress <- function(detail = NULL) {
          prog$inc(amount = incAmount, detail = detail)
        }
      }
      # END error checks

      # save current time stamp
      private$stime <- Sys.time()

      if (!is.null(msgProgress)) {
        # increment progress bar
        updateProgress(detail = msgProgress$attachments)
      }
      tryCatch(
        {
          DBI::dbBegin(private$conn)
          # write scenario metadata
          private$writeMetadata()
          if (!is.null(private$attachments) && addScenIdAttach) {
            private$attachments$setSid(private$sid)
          }

          private$saveAttachmentData()

          Map(function(dataset, symNameScenario) {
            if (!is.null(dataset) && nrow(dataset)) {
              super$exportScenDataset(
                private$bindSidCol(dataset),
                symNameScenario
              )
            }
            if (!is.null(msgProgress)) {
              # increment progress bar
              updateProgress(detail = msgProgress$progress)
            }
          }, datasets, symNamesScenario)

          private$saveViewData()

          DBI::dbCommit(private$conn)

          private$saveHash()

          if (!is.na(private$inDataSid)) {
            hcubeRefCount <- self$importDataset("_scenMeta",
              tibble(
                "_scode",
                private$inDataSid + 10000L
              ),
              count = TRUE, limit = 1L
            )[[1]][1]
            if (identical(hcubeRefCount, 0L)) {
              private$deleteBySid(private$inDataSid)
            }
            private$inDataSid <- NA_integer_
          }

          private$scenSaved <- TRUE
          # refresh lock for scenario
          private$lock()
        },
        error = function(e) {
          DBI::dbRollback(private$conn)
          stop(e)
        }
      )
      invisible(self)
    },
    saveTraceData = function(traceData) {
      # Saves trace data
      #
      # Args:
      #   traceData:   data frame with trace data
      #
      # Returns:
      #   R6 object (reference to itself)

      stopifnot(length(private$sid) > 0L)
      if (private$isReadonly()) {
        stop("Scenario is readonly. Saving trace data failed (Scenario.saveTraceData).", call. = FALSE)
      }
      super$exportScenDataset(
        private$bindSidCol(traceData),
        "_scenTrc"
      )
      private$scenSaved <- TRUE
      invisible(self)
    },
    delete = function() {
      # Wrapper to deleteRows function to easily remove scenario from database
      #
      # Args:
      #
      # Returns:
      #   R6 object: reference to itself,
      #   throws exception in case of error

      # BEGIN error checks
      stopifnot(length(private$sid) > 0L)
      # END error checks
      if (private$isReadonly()) {
        stop_custom("error_scen_locked", "Scenario could not be removed as it is readonly (Scenario.delete).", call. = FALSE)
      }
      noErr <- TRUE
      private$deleteBySid(private$sid)

      private$unlock()
      flog.debug("Scenario: '%s' unlocked.", private$sid)

      private$sid <- integer(0)

      invisible(self)
    },
    updateMetadata = function(newName = character(0L), newTags = character(0L),
                              newReadPerm = character(0L), newWritePerm = character(0L),
                              newExecPerm = character(0L)) {
      # Edit scenario metadata (wrapper around db.writeMetadata method)
      #
      # Args:
      #   newName:      updated scenario name (optional)
      #   newTags:      updated scenario tags (optional)
      #   newReadPerm:  updated scenario read permissions (optional)
      #   newWritePerm: updated scenario write permissions (optional)
      #   newWritePerm: updated scenario execute permissions (optional)

      # Returns:
      #   R6 object: reference to itself,
      #   throws exception in case of error

      # BEGIN error checks
      stopifnot(is.character(newName), length(newName) <= 1L)
      if (length(newTags)) {
        stopifnot(is.character(newTags))
      }
      stopifnot(is.character(newReadPerm))
      stopifnot(is.character(newWritePerm))
      stopifnot(is.character(newExecPerm))
      # END error checks
      if (private$isReadonly() && sum(
        length(newReadPerm),
        length(newWritePerm),
        length(newExecPerm)
      ) > 0) {
        stop("Db: Metadata wasn't updated as scenario is readonly (Scenario.updateMetadata).",
          call. = FALSE
        )
      }
      if (length(newName)) {
        private$sname <- newName
      }
      if (length(newTags)) {
        private$tags <- vector2Csv(newTags)
      }
      if (length(newReadPerm)) {
        private$readPerm <- vector2Csv(newReadPerm)
      }
      if (length(newWritePerm)) {
        private$writePerm <- vector2Csv(newWritePerm)
      }
      if (length(newExecPerm)) {
        private$execPerm <- vector2Csv(newExecPerm)
      }

      invisible(self)
    },
    hasExecPerm = function() {
      # checks whether current scenario can be executed
      #
      # Args:
      #
      # Returns:
      #   logical: returns TRUE if scenario can be executed, FALSE otherwise

      if (any(private$userAccessGroups %in% csv2Vector(private$execPerm))) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    },
    saveScriptResults = function(scriptResults) {
      # save script results in database
      #
      # Args:
      #
      # Returns:
      #   R6 object (referece to self)

      stopifnot(
        inherits(scriptResults, "data.frame"),
        length(scriptResults) == 2L
      )

      scriptsSchema <- dbSchema$getDbSchema("_scenScripts")

      names(scriptResults) <- scriptsSchema$colNames[-1]

      for (i in 1:2) {
        if (!is.character(scriptResults[[i]])) {
          scriptResults[[i]] <- as.character(scriptResults[[i]])
        }
      }

      super$exportScenDataset(
        private$bindSidCol(scriptResults),
        "_scenScripts"
      )
    },
    finalize = function() {
      if (length(private$sid)) {
        if (identical(private$getUidLock(), private$uid)) {
          tryCatch(
            {
              private$unlock()
              flog.debug("Scenario: '%s' unlocked.", private$sid)
            },
            error = function(e) {
              flog.warn(
                "Scenario: '%s' could not be unlocked. Error message: '%s'",
                private$sid, conditionMessage(e)
              )
            }
          )
        }
        if (private$newScen && !private$scenSaved) {
          tryCatch(
            {
              flog.debug("Scenario was not saved. Thus, it will be removed.")
              self$delete()
            },
            error = function(e) {
              flog.warn(
                "Scenario could not be removed. Error message: '%s'",
                conditionMessage(e)
              )
            }
          )
        }
        private$sid <- integer(0L)
      }
      return(invisible(self))
    }
  ),
  active = list(
    isReadonlyOrLocked = function(x) {
      # Determines whether scenario is either readonly or locked
      #
      # Args:
      #
      # Returns:
      # logical: returns TRUE if scenario is either readonly or locked, FALSE otherwise

      # BEGIN error checks
      stopifnot(missing(x))
      # END error checks

      if (private$isLocked() || private$isReadonly()) {
        return(TRUE)
      } else {
        return(FALSE)
      }
    }
  ),
  private = list(
    sid = integer(0L),
    suid = character(0L),
    sname = character(0L),
    stime = character(0L),
    tags = character(1L),
    readPerm = character(0L),
    writePerm = character(0L),
    execPerm = character(0L),
    inDataSid = NA_integer_,
    lockUid = character(0L),
    scode = integer(1L),
    scenSaved = logical(1L),
    newScen = logical(1L),
    tableNameScenLocks = character(0L),
    traceData = tibble(),
    scenHash = NULL,
    views = NULL,
    attachments = NULL,
    duplicateAttachmentsOnNextSave = FALSE,
    removeAllExistingAttachments = FALSE,
    forceOverwrite = FALSE,
    sidToDuplicate = integer(0L),
    fetchMetadata = function(sid = NULL, sname = NULL, uid = NULL) {
      # fetches scenario metadata from database
      #
      # Args:
      #   sid:           scenario ID (optional)
      #   sname:         scenario name (optional)
      #   uid:           user ID (optional)
      #
      #   object: invisibly returns reference to itself

      # BEGIN error checks
      if (!is.null(sid)) {
        stopifnot(is.integer(sid), length(sid) == 1)
      } else {
        stopifnot(is.character(sname), length(sname) == 1)
        stopifnot(is.character(uid), length(uid) == 1)
      }
      # END error checks

      if (!is.null(sid)) {
        metadata <- self$importDataset("_scenMeta",
          subsetSids = sid, innerSepAND = FALSE
        )
        if (!nrow(metadata)) {
          stop(sprintf(
            "A scenario with ID: '%s' could not be found. This could be due to the scenario being deleted in the meantime or due to the user tampering with the app!",
            sid
          ), call. = FALSE)
        }
        if (metadata[["_scode"]][1] > 10000L) {
          # Hypercube scenario
          private$scode <- SCODEMAP[["scen"]]
          private$inDataSid <- metadata[["_scode"]] - 10000L
        }
        if (metadata[["_scode"]][1] != private$scode && is.na(private$inDataSid)) {
          flog.debug("The scenario loaded was generated from a different mode. A copy will be created (Scen.fetchMetadata)")
          private$sname <- metadata[["_sname"]][1]
          private$suid <- private$uid
          private$stime <- Sys.time()
          private$tags <- metadata[["_stag"]][1]
          traceData <- super$importDataset("_scenTrc",
            subsetSids = sid, limit = 1L
          )
          if (length(traceData) && nrow(traceData)) {
            private$traceData <- traceData[-1]
          }
          existingSids <- self$importDataset("_scenMeta",
            colNames = "_sid",
            tibble(
              c(
                "_uid",
                "_sname",
                "_scode"
              ),
              c(
                private$suid, private$sname,
                private$scode
              )
            )
          )[[1]]
          if (length(existingSids) > 0L) {
            private$sid <- existingSids[1L]
          }
          private$writeMetadata()
        } else {
          private$suid <- metadata[["_uid"]][1]
          private$sname <- metadata[["_sname"]][1]
          private$stime <- metadata[["_stime"]][1]
          private$tags <- metadata[["_stag"]][1]
          private$readPerm <- metadata[["_accessr"]][1]
          private$writePerm <- metadata[["_accessw"]][1]
          private$execPerm <- metadata[["_accessx"]][1]
          private$sid <- as.integer(sid)
        }
      } else {
        metadata <- self$importDataset(
          "_scenMeta",
          tibble(
            c(
              "_uid",
              "_sname"
            ),
            c(uid, sname)
          )
        )
        if (!nrow(metadata)) {
          stop(sprintf(
            "A scenario with name: '%s' could not be found for user: '%s'.",
            sname, uid
          ), call. = FALSE)
        }
        scenFromOtherMode <- metadata[["_scode"]] != private$scode
        if (all(scenFromOtherMode)) {
          stop("No scenario with this name exists in the current mode.",
            call. = FALSE
          )
        }

        private$suid <- uid
        private$sname <- sname
        private$stime <- Sys.time()
        sidTmp <- as.integer(metadata[["_sid"]][!scenFromOtherMode])

        private$sid <- sidTmp
        if (length(private$traceData) && nrow(private$traceData)) {
          self$saveTraceData(private$traceData)
          private$traceData <- tibble()
        }
      }

      invisible(self)
    },
    getUidLock = function() {
      # checks whether a scenario is currently locked
      #
      # Args:
      #
      # Returns:
      #   character: returns uid of user who locked scenario in case scenario is locked and NA_character_ otherwise, throws exception in case of error

      # BEGIN error checks
      stopifnot(length(private$sid) > 0L)
      # END error checks

      sid <- private$sid

      if (!DBI::dbExistsTable(private$conn, private$tableNameScenLocks)) {
        return(NA_character_)
      }
      lockData <- self$importDataset("_scenLock",
        tibble(
          "_sid",
          sid
        ),
        limit = 2L
      )
      if (!is.null(lockData) && nrow(lockData)) {
        if (nrow(lockData) > 1) {
          stop(sprintf(
            "Db: %s: More than one lock was found for the scenario. This should never happen. (Scenario.getUidLock, table: '%s', scenario: '%s').",
            private$uid, private$tableNameScenLocks, sid
          ), call. = FALSE)
        }
        if (!is.null(private$slocktimeLimit)) {
          if ((Sys.time() - as.POSIXct(lockData[["_slocktime"]])) > private$slocktimeLimit) {
            # lock has expired, so remove it
            private$unlock()
            return(NA_character_)
          } else {
            # scenario is locked and lock has not yet expired
            return(lockData[["_uid"]])
          }
        } else {
          # scenario is locked and no time limit was provided
          return(lockData[["_uid"]])
        }
      } else {
        # no lock found
        return(NA_character_)
      }
    },
    writeMetadata = function() {
      # Write scenario metadata to metadata table
      #
      # Args:
      #
      # Returns:
      #   Db class object:  invisibly returns reference to object if successful,
      #   throws exception in case of error
      if (length(private$sid)) {
        if (private$isReadonly()) {
          stop_custom("error_scen_locked", "Db: Metadata could not be overwritten as scenario is readonly (Scenario.writeMetadata).",
            call. = FALSE
          )
        }
        if (!is.na(private$inDataSid)) {
          existingSids <- self$importDataset("_scenMeta",
            colNames = "_sid",
            tibble(
              c(
                "_uid",
                "_sname",
                "_scode"
              ),
              c(
                private$suid,
                private$sname,
                private$scode
              )
            ),
            limit = 1L
          )[[1]]
          if (length(existingSids) > 0L) {
            stop_custom("error_sname_exists", "A scenario with the same name already exists.",
              call. = FALSE
            )
          }
        }
        self$deleteRows("_scenMeta", "_sid", private$sid)
        metadata <- tibble(
          `_sid` = private$sid,
          `_uid` = private$suid,
          `_sname` = private$sname,
          `_stime` = private$stime,
          `_stag` = private$tags,
          `_accessr` = private$readPerm,
          `_accessw` = private$writePerm,
          `_accessx` = private$execPerm,
          `_scode` = private$scode
        )
      } else {
        # new scenario
        private$newScen <- TRUE
        metadata <- tibble(
          `_uid` = private$suid,
          `_sname` = private$sname,
          `_stime` = private$stime,
          `_stag` = private$tags,
          `_accessr` = private$readPerm,
          `_accessw` = private$writePerm,
          `_accessx` = private$execPerm,
          `_scode` = private$scode
        )
      }
      super$writeMetadata(metadata)
      flog.debug(
        "Db: Metadata was added for scenario: '%s' (Scenario.writeMetadata).",
        private$sname
      )
      if (!length(private$sid)) {
        private$fetchMetadata(
          sname = private$sname,
          uid = private$suid
        )
      }
      invisible(self)
    },
    lock = function() {
      # adds new lock for scenario
      #
      # Args:
      #
      # Returns:
      #   Db object: invisibly returns object reference in case lock was set successfully, throws exception otherwise

      # BEGIN error checks
      if (!length(private$sid)) {
        return(invisible(self))
      }
      # END error checks
      if (private$isReadonly()) {
        flog.debug("Scenario wasn't locked as it is readonly (Scenario.lock).")
        return(invisible(self))
      }
      if (!DBI::dbExistsTable(private$conn, private$tableNameScenLocks)) {
        # in case table does not yet exist, create it
        tryCatch(
          {
            self$runQuery(dbSchema$getCreateTableQuery("_scenLock"))
            flog.debug("Db: %s: Table with locks ('%s') was created as it did not yet exist. (Scenario.lock)", private$uid, private$tableNameScenLocks)
          },
          error = function(e) {
            stop(sprintf(
              "Db: %s: An error occurred while trying to create table. (Scenario.lock, table: '%s'). Error message: %s.",
              private$uid, private$tableNameScenLocks, conditionMessage(e)
            ), call. = FALSE)
          }
        )
      }
      # check whether scenario is already locked
      uidLocked <- private$getUidLock()
      if (!is.na(uidLocked)) {
        # a lock already exists for the scenario
        if (!identical(uidLocked, private$uid)) {
          # user who has currently locked the scenario is not the same as the active user provided
          flog.debug("Scenario loaded in readonly mode as it is locked (Scenario.lock).")
          private$lockUid <- uidLocked
          return(invisible(self))
        } else {
          # user who currently has scenario locked is the same, so refresh lock
          private$unlock()
        }
      }
      lockData <- data.frame(as.character(private$uid),
        as.integer(private$sid),
        Sys.time(),
        stringsAsFactors = FALSE
      )
      colnames(lockData) <- c(
        "_uid",
        "_sid",
        "_slocktime"
      )
      lockData <- dateColToChar(private$conn, lockData)
      tryCatch(
        {
          DBI::dbWriteTable(private$conn, private$tableNameScenLocks, lockData, row.names = FALSE, append = TRUE)
          flog.debug("Db: %s: Lock was added for scenario: '%s' (Scenario.lock).", private$uid, private$sid)
        },
        error = function(e) {
          stop(sprintf(
            "Db: %s: An error occurred writing to database (Scenario.lock, table: '%s', scenario: '%s'). Error message: %s",
            private$uid, private$tableNameScenLocks, private$sid, conditionMessage(e)
          ), call. = FALSE)
        }
      )
      invisible(self)
    },
    unlock = function() {
      # removes lock for scenario
      #
      # Args:
      #
      # Returns:
      #   Db object: invisibly returns object reference in case lock was removed, throws exception in case something went wrong

      # BEGIN error checks
      if (!length(private$sid)) {
        return(invisible(self))
      }
      # END error checks

      if (DBI::dbExistsTable(private$conn, private$tableNameScenLocks)) {
        self$deleteRows("_scenLock",
          colNames = "_sid",
          values = private$sid
        )
      }
      invisible(self)
    },
    isLocked = function() {
      # Determines whether a scenario is locked by a user other than the user logged in
      #
      # Args:
      #
      # Returns:
      #   logical: returns TRUE in scenario is locked or error occurred, FALSE otherwise

      # BEGIN error checks
      if (!length(private$sid)) {
        return(FALSE)
      }
      # END error checks

      errMsg <- NULL
      tryCatch(
        {
          uidLock <- private$getUidLock()
          if (is.na(uidLock) || identical(uidLock, private$uid)) {
            # scenario is not locked or locked by current user
            return(FALSE)
          } else {
            # scenario is locked
            return(TRUE)
          }
        },
        error = function(e) {
          stop(sprintf(
            "Db: Problems locking scenario: '%s'. Error message: %s.", private$sid,
            conditionMessage(e)
          ), call. = FALSE)
        }
      )
    },
    isReadonly = function() {
      # checks whether current scenario is readOnly
      #
      # Args:
      #
      # Returns:
      #   logical: returns TRUE if scenario is readonly, FALSE otherwise
      if (length(private$lockUid)) {
        return(TRUE)
      }
      if (any(private$userAccessGroups %in% csv2Vector(private$writePerm))) {
        return(FALSE)
      } else {
        return(TRUE)
      }
    },
    bindSidCol = function(data) {
      # binds sid column to dataset
      if (!identical(names(data)[[1]], "_sid")) {
        return(bind_cols(
          `_sid` = rep.int(private$sid, nrow(data)),
          data
        ))
      }
      return(data)
    },
    saveAttachmentData = function() {
      # remove existing attachments if scenario is overwritten
      if (isTRUE(private$removeAllExistingAttachments)) {
        if (identical(private$sidToDuplicate, private$sid)) {
          # in case scenario id is identical to the one
          # where attachments should be duplicated, we do nothing
          private$duplicateAttachmentsOnNextSave <- FALSE
        } else {
          super$deleteRows("_scenAttach",
            subsetSids = private$sid
          )
        }
        private$removeAllExistingAttachments <- FALSE
      }
      if (isTRUE(private$duplicateAttachmentsOnNextSave)) {
        private$duplicateAttachments()
      }
      if (is.null(private$attachments)) {
        return(invisible(self))
      }
      attachmentOpQueue <- private$attachments$flushOpQueue()
      if (length(attachmentOpQueue$remove)) {
        # save dirty attachments
        super$deleteRows("_scenAttach", "fileName",
          attachmentOpQueue$remove,
          conditionSep = "OR", subsetSids = private$sid
        )
      }

      if (length(attachmentOpQueue$updateExec$name)) {
        fnHasExecPerm <- attachmentOpQueue$updateExec$
          name[attachmentOpQueue$updateExec$execPerm]
        fnHasNoExecPerm <- attachmentOpQueue$updateExec$
          name[!attachmentOpQueue$updateExec$execPerm]

        if (length(fnHasExecPerm)) {
          super$updateRows("_scenAttach",
            tibble("fileName", fnHasExecPerm),
            colNames = "execPerm",
            values = TRUE,
            subsetSids = private$sid, innerSepAND = FALSE
          )
        }

        if (length(fnHasNoExecPerm)) {
          super$updateRows("_scenAttach",
            tibble("fileName", fnHasNoExecPerm),
            colNames = "execPerm",
            values = FALSE,
            subsetSids = private$sid, innerSepAND = FALSE
          )
        }
      }
      if (length(attachmentOpQueue$save)) {
        super$exportScenDataset(
          private$bindSidCol(
            attachmentOpQueue$save
          ),
          "_scenAttach"
        )
      }
      return(invisible(self))
    },
    saveViewData = function() {
      if (is.null(private$views)) {
        return(invisible(self))
      }
      viewConf <- private$views$getConf()
      if (!length(viewConf) || !nrow(viewConf)) {
        return(invisible(self))
      }
      viewsSchema <- dbSchema$getDbSchema("_scenViews")
      viewConfToSave <- fixColTypes(
        viewConf %>%
          add_column(time = Sys.time()),
        substring(
          viewsSchema$colTypes,
          2
        )
      )
      names(viewConfToSave) <- viewsSchema$colNames[-1]
      super$exportScenDataset(
        private$bindSidCol(viewConfToSave),
        "_scenViews"
      )
      invisible(self)
    },
    duplicateAttachments = function() {
      stopifnot(
        is.integer(private$sidToDuplicate),
        length(private$sidToDuplicate) == 1L, !is.na(private$sidToDuplicate),
        length(private$sid) == 1L
      )
      attachSchema <- dbSchema$getDbSchema("_scenAttach")
      if (!dbExistsTable(private$conn, attachSchema$tabName)) {
        return(invisible(self))
      }
      self$runQuery(paste0(
        "INSERT INTO ",
        dbQuoteIdentifier(
          private$conn,
          attachSchema$tabName
        ),
        "(",
        paste0(dbQuoteIdentifier(
          private$conn,
          attachSchema$colNames
        ),
        collapse = ","
        ),
        ") SELECT ", private$sid, ",",
        paste0(dbQuoteIdentifier(
          private$conn,
          attachSchema$colNames[-1]
        ),
        collapse = ","
        ),
        " FROM ",
        dbQuoteIdentifier(private$conn, attachSchema$tabName),
        " WHERE ",
        dbQuoteIdentifier(private$conn, attachSchema$colNames[1]),
        "=", private$sidToDuplicate, ";"
      ))
      private$duplicateAttachmentsOnNextSave <- FALSE
      return(invisible(self))
    },
    saveHash = function() {
      # Saves scenario hash
      #
      # Returns:
      #   R6 object (reference to itself)

      stopifnot(length(private$sid) > 0L)
      if (private$isReadonly()) {
        stop("Scenario is readonly. Saving scenario hash failed (Scenario.saveHash).", call. = FALSE)
      }
      if (length(private$scenHash)) {
        super$exportScenDataset(
          private$bindSidCol(tibble(hash = private$scenHash)),
          "_scenHash"
        )
        flog.debug("Hash for scenario: '%s' saved.", private$sid)
      } else {
        self$deleteRows(
          "_scenHash",
          "_sid",
          private$sid
        )
      }
      invisible(self)
    },
    deleteBySid = function(scenId) {
      self$deleteRows(
        "_scenMeta",
        "_sid",
        scenId
      )
      self$deleteRows(
        "_scenAttach",
        "_sid",
        scenId
      )
      return(invisible(self))
    }
  )
)

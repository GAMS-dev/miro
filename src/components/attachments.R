Attachments <- R6Class("Attachments",
  inherit = ScenarioExtensions,
  public = list(
    initialize = function(db, config, workDir,
                          inputSymbols, outputSymbols,
                          tabularInputSymbols, rv = NULL, scenData = NULL) {
      private$db <- db
      private$config <- config
      private$workDir <- file.path(dirname(workDir), basename(workDir))
      private$conn <- db$getConn()
      super$initialize(inputSymbols, outputSymbols, tabularInputSymbols, rv, scenData = scenData)
    },
    setSid = function(sid) {
      private$sid <- sid
      return(invisible(self))
    },
    initScenData = function(sid, inDataSid = NA_integer_) {
      self$clear()
      private$sid <- sid
      private$attachmentData <- NULL
      if (!is.na(inDataSid)) {
        private$inDataSid <- inDataSid
      } else {
        private$inDataSid <- NULL
      }
      return(invisible(self))
    },
    getConfig = function() {
      return(list(
        localAttachments = private$localAttachments,
        attachmentsUpdateExec = private$attachmentsUpdateExec,
        attachmentsToRemove = private$attachmentsToRemove
      ))
    },
    setConfig = function(config) {
      private$localAttachments <- config$localAttachments
      private$attachmentsUpdateExec <- config$attachmentsUpdateExec
      private$attachmentsToRemove <- config$attachmentsToRemove
      return(invisible(self))
    },
    flushOpQueue = function() {
      # flushes list of operations to perform when saving scenario
      opQueue <- list(
        remove = private$attachmentsToRemove,
        save = private$getLocalData(),
        updateExec = private$attachmentsUpdateExec
      )
      private$resetOpQueue()
      return(opQueue)
    },
    getIds = function() {
      self$getMetadata()[["name"]]
    },
    getMetadata = function(scenId = NULL) {
      # Fetches list of attachments
      #
      # Args:
      #   scenId: The id of the scenario or NULL for the sandbox scenario
      #
      # Returns:
      #   tibble with columns name and execPerm
      if (!is.null(scenId)) {
        return(private$fetchDataFromDb(scenId))
      }
      attachmentNames <- basename(private$localAttachments$filePaths)
      attachmentExecPerm <- private$localAttachments$execPerm
      if (!length(private$sid)) {
        return(dplyr::arrange(tibble(
          name = attachmentNames,
          execPerm = attachmentExecPerm
        ), name))
      }
      attachmentNamesDb <- character(0L)
      attachmentExecPermDb <- logical(0L)
      if (is.null(private$attachmentData)) {
        private$attachmentData <- private$fetchDataFromDb(c(
          private$sid,
          private$inDataSid
        ))
      }
      if (length(private$attachmentData) && nrow(private$attachmentData)) {
        attachmentNamesDb <- private$attachmentData[[1]]
        attachmentsToKeep <- !attachmentNamesDb %in% attachmentNames
        attachmentNamesDb <- attachmentNamesDb[attachmentsToKeep]
        attachmentExecPermDb <- private$attachmentData[[2]][attachmentsToKeep]
        if (length(private$attachmentsToRemove)) {
          attachmentsToKeep <- !attachmentNamesDb %in% private$attachmentsToRemove
          attachmentNamesDb <- attachmentNamesDb[attachmentsToKeep]
          attachmentExecPermDb <- attachmentExecPermDb[attachmentsToKeep]
        }
        if (length(private$attachmentsUpdateExec$name)) {
          attachmentsToKeep <- !attachmentNamesDb %in% private$attachmentsUpdateExec$name
          attachmentNamesDb <- c(
            attachmentNamesDb[attachmentsToKeep],
            private$attachmentsUpdateExec$name
          )
          attachmentExecPermDb <- c(
            attachmentExecPermDb[attachmentsToKeep],
            private$attachmentsUpdateExec$execPerm
          )
        }
      }

      return(dplyr::arrange(
        tibble(
          name = c(attachmentNamesDb, attachmentNames),
          execPerm = c(attachmentExecPermDb, attachmentExecPerm)
        ),
        name
      ))
    },
    save = function(filePaths, fileNames, overwrite = TRUE) {
      # stores file(s) at given location(s)
      #
      # Args:
      #   filePaths:     character vector where to save files
      #                  either directory name or directory+filename where to store files
      #   fileNames:     character vector with names of the files to download
      #   overwrite:     Whether to overwrite existing files
      #
      # Returns:
      #   filepaths of downloaded files
      pathIsDir <- file.info(filePaths)$isdir
      fullPath <- TRUE
      if (length(filePaths) == 1L) {
        if (isTRUE(pathIsDir)) {
          fullPath <- FALSE
        } else if (isFALSE(pathIsDir) && isFALSE(overwrite)) {
          stop_custom("error_file_exists",
            "File already exists",
            call. = FALSE
          )
        } else if (is.na(pathIsDir) && is.null(fullPath) &&
          identical(
            tools::file_path_sans_ext(basename(filePaths)),
            basename(filePaths)
          )) {
          fullPath <- FALSE
        }
      } else if (isFALSE(overwrite) && any(!is.na(pathIsDir))) {
        stop_custom("error_file_exists",
          "File(s) already exist(s)",
          call. = FALSE
        )
      }
      return(self$download(filePaths, fileNames, fullPath = fullPath))
    },
    add = function(session, filePaths, fileNames = NULL, overwrite = FALSE, execPerm = NULL) {
      # Adds attachments
      #
      # Args:
      #   session:          session object
      #   filePaths:        character vector with file paths to read data from
      #   fileNames:        names of the files in case custom name should be chosen
      #   overwrite:        boolean that specifies whether existing files should
      #                     be overwritten
      #   execPerm:         vector with execute permissions
      #                     (must be NULL or logical vector same length as filePaths)
      #
      # Returns:
      #   R6 object (reference to itself)
      if (!is.null(session) && self$isReadonly(session)) {
        stop_custom("error_readonly",
          "Tried to modify attachment data from readonly tab.",
          call. = FALSE
        )
      }
      if (length(filePaths) < 1L) {
        return(invisible(self))
      }

      stopifnot(is.character(filePaths), is.logical(overwrite), length(overwrite) == 1L)

      if (is.null(execPerm)) {
        execPerm <- rep.int(TRUE, length(filePaths))
      } else {
        stopifnot(is.logical(execPerm), length(filePaths) == length(execPerm))
      }

      if (is.null(fileNames)) {
        filePaths <- unique(file.path(dirname(filePaths), basename(filePaths)))
        fileNames <- basename(filePaths)
      } else {
        stopifnot(
          is.character(fileNames),
          length(unique(fileNames)) == length(filePaths)
        )
        fileNames <- path_sanitize(fileNames)
      }

      if (length(private$config$forbiddenFNames) &&
        any(tools::file_path_sans_ext(fileNames) %in%
          private$config$forbiddenFNames)) {
        stop_custom("error_forbidden_filename",
          "Some of the filenames specified are not allowed to be used",
          call. = FALSE
        )
      }
      fileSize <- file.size(filePaths)

      if (any(is.na(fileSize))) {
        stop_custom("error_not_found",
          "Some of the files to add do not exist",
          call. = FALSE
        )
      }

      if (any(fileSize > private$config[["maxSize"]])) {
        stop_custom("error_max_size",
          "Some of the files exceed the maximum allowed attachment size",
          call. = FALSE
        )
      }
      existingFileNames <- self$getIds()

      fnToOverwrite <- fileNames %in% existingFileNames

      if (any(fnToOverwrite)) {
        if (!overwrite) {
          stop_custom("error_duplicate_files",
            "Some of the files already exist and overwrite is set to FALSE",
            call. = FALSE
          )
        }
        existingFileNames <- existingFileNames[!existingFileNames %in% fileNames]
      }

      if (length(existingFileNames) + length(filePaths) > private$config[["maxNo"]]) {
        stop_custom("error_max_no",
          "The maximum number of attachment was exceeded",
          call. = FALSE
        )
      }

      if (any(fnToOverwrite)) {
        private$.remove(fileNames[fnToOverwrite],
          removeLocal = FALSE
        )
      }

      filesNeedRelocation <- dirname(filePaths) != private$workDir | basename(filePaths) != fileNames
      if (any(filesNeedRelocation)) {
        fileNamesTmp <- vapply(seq_along(fileNames)[filesNeedRelocation], function(fnIdx) {
          if (execPerm[fnIdx]) {
            return(fileNames[fnIdx])
          }
          return(file.path("_miro_attach_", fileNames[fnIdx]))
        }, character(1L), USE.NAMES = FALSE)

        filePathsTmp <- file.path(private$workDir, fileNamesTmp)
        failedFileRelocations <- !file.move(filePaths[filesNeedRelocation], filePathsTmp)
        filePaths[filesNeedRelocation] <- filePathsTmp
        if (any(failedFileRelocations)) {
          flog.warn(
            "Some attachments could not be relocated: '%s' (Attachments.add).",
            paste(filePathsTmp[failedFileRelocations], collapse = "', '")
          )
          movedSuccessfully <- !filePaths %in% filePaths[filesNeedRelocation][failedFileRelocations]
          filePaths <- filePaths[movedSuccessfully]
          execPerm <- execPerm[movedSuccessfully]
        }
      }

      private$localAttachments$filePaths <- c(private$localAttachments$filePaths, filePaths)
      private$localAttachments$execPerm <- c(private$localAttachments$execPerm, execPerm)

      private$markUnsaved(markDirty = any(execPerm))
      lapply(names(private$updateCallbacks[["1"]]), function(symName) {
        private$updateCallbacks[["1"]][[symName]]()
      })
      invisible(self)
    },
    getData = function(scenIds = integer(0L), fileNames = NULL, includeContent = FALSE, includeSandboxScen = TRUE) {
      # Fetches attachment metadata as well as content in memory
      #
      # Args:
      #   scenIds:             scenario IDs to fetch (meta)data for (optional)
      #   fileNames:           filter what attachments to fetch (by file name) (optional)
      #   includeContent:      whether to include attachment data as well (as blob) (optional)
      #   includeSandboxScen:  whether to include data of sandbox scenario (has _sid == 0) (optional)
      #
      # Returns:
      #   tibble with columns _sid, fileName, execPerm and additionally: fileContent in case includeContent is TRUE

      colsToFetch <- c("_sid", "fileName", "execPerm")
      if (includeContent) {
        colsToFetch <- c(colsToFetch, "fileContent")
      }
      sidToFilter <- NULL
      if (includeSandboxScen) {
        if (length(private$sid) && private$sid %in% scenIds) {
          scenIdsToFetch <- scenIds
        } else {
          scenIdsToFetch <- c(private$sid, scenIds)
          sidToFilter <- private$sid
        }
      } else {
        scenIdsToFetch <- scenIds
      }
      if (length(fileNames)) {
        data <- private$db$importDataset("_scenAttach",
          tibble(rep.int("fileName", length(fileNames)), fileNames),
          colNames = colsToFetch, innerSepAND = FALSE,
          subsetSids = scenIdsToFetch
        )
      } else {
        data <- private$db$importDataset("_scenAttach",
          colNames = colsToFetch, innerSepAND = FALSE,
          subsetSids = scenIdsToFetch
        )
      }
      if (!length(data)) {
        data <- tibble(`_sid` = integer(), fileName = character(), execPerm = logical())
        if (includeContent) {
          data$fileContent <- blob::blob()
        }
      }
      data$execPerm <- as.logical(data$execPerm)
      if (!includeSandboxScen) {
        return(data)
      }
      sandboxScenData <- dplyr::filter(data, `_sid` == if (length(private$sid)) private$sid else -1L)
      sandboxScenData$`_sid` <- 0L
      if (length(private$attachmentsToRemove)) {
        sandboxScenData <- dplyr::filter(data, fileName %in% private$attachmentsToRemove)
      }
      if (length(private$attachmentsUpdateExec)) {
        fileIdsToUpdate <- match(private$attachmentsUpdateExec$name, sandboxScenData$fileName)
        execPermNeedsUpdate <- !is.na(fileIdsToUpdate)
        fileIdsToUpdate <- fileIdsToUpdate[execPermNeedsUpdate]
        sandboxScenData[fileIdsToUpdate, "execPerm"] <- private$attachmentsUpdateExec$execPerm[execPermNeedsUpdate]
      }
      if (length(private$localAttachments)) {
        localFilePaths <- private$localAttachments$filePaths
        localFileData <- tibble(
          `_sid` = vector("integer", length(localFilePaths)),
          fileName = basename(localFilePaths),
          execPerm = private$localAttachments$execPerm
        )
        if (length(fileNames)) {
          localFileData <- dplyr::filter(localFileData, fileName %in% fileNames)
          localFilePaths <- localFilePaths[basename(localFilePaths) %in% localFileData$fileName]
        }
        if (includeContent) {
          localFileSize <- file.size(localFilePaths)
          localFileData$fileContent <- blob::new_blob(lapply(
            seq_along(localFilePaths),
            function(i) {
              tryCatch(
                readBin(localFilePaths[[i]], "raw", n = localFileSize[[i]]),
                error = function(e) {
                  flog.warn(
                    "Problems reading file: '%s'. Error message: %s",
                    localFilePaths[[i]],
                    conditionMessage(e)
                  )
                  return(raw(0))
                }, warning = function(w) {
                  flog.warn(
                    "Problems reading file: '%s'. Warning message: %s",
                    localFilePaths[[i]],
                    conditionMessage(w)
                  )
                  return(raw(0))
                }
              )
            }
          ))
        }
      }
      if (length(sidToFilter)) {
        data <- dplyr::filter(data, `_sid` != sidToFilter)
      }
      return(dplyr::bind_rows(localFileData, sandboxScenData, data))
    },
    download = function(filePath, fileNames = NULL,
                        fullPath = FALSE, allExecPerm = FALSE, scenId = NULL) {
      # Fetches attachment data from db
      #
      # Args:
      #   filePath:      character vector where to save files
      #                  either directory name or
      #                  (if fullPath is TRUE) directory+filename where to store files
      #   fileNames:     character vector with names of the files to download (optional)
      #   fullPath:      whether filePath includes file name + extension or not (optional)
      #   allExecPerm:   whether to download all files with execution permission (optional)
      #   scenId:        id of scenario to download data from, NULL to use sandbox scenario (optional)
      #
      # Returns:
      #   filepaths of downloaded files

      stopifnot(
        is.character(filePath),
        is.logical(fullPath), length(fullPath) == 1L,
        is.logical(allExecPerm), length(allExecPerm) == 1L
      )

      if (length(filePath) < 1L) {
        return(invisible(self))
      }

      filePath <- file.path(
        dirname(filePath),
        basename(filePath)
      )

      if (fullPath) {
        stopifnot(!allExecPerm, length(filePath) == length(fileNames))
        fileNames <- path_sanitize(fileNames)
        fileDir <- dirname(filePath)
        filePaths <- filePath
      } else {
        stopifnot(length(filePath) == 1L)
        if (!file.exists(filePath)) {
          stop_custom("error_not_found",
            "The destination directory does not exist",
            call. = FALSE
          )
        }
        fileDir <- filePath
        filePaths <- file.path(filePath, path_sanitize(fileNames))
      }

      localPaths <- character(0L)
      remotePaths <- character(0L)
      unmodifiedData <- TRUE
      fileNamesRemote <- fileNames

      if (is.null(scenId)) {
        if (allExecPerm) {
          if (any(private$localAttachments$execPerm)) {
            localPaths <- private$localAttachments$filePaths[private$localAttachments$execPerm]
            localPathNeedsRelocation <- dirname(localPaths) != fileDir

            if (any(localPathNeedsRelocation)) {
              if (fullPath) {
                toDir <- filePaths
              } else {
                toDir <- file.path(fileDir, basename(localPaths[localPathNeedsRelocation]))
              }
              file.copy(localPaths[localPathNeedsRelocation],
                toDir,
                overwrite = TRUE
              )
            }
          }
        } else {
          stopifnot(is.character(fileNames))
          if (length(fileNames) < 1L) {
            return(invisible(self))
          }

          isLocalAttachment <- fileNames %in% basename(private$localAttachments$filePaths)
          if (any(isLocalAttachment)) {
            localPathId <- match(fileNames, basename(private$localAttachments$filePaths))
            localPathId <- localPathId[!is.na(localPathId)]

            if (length(localPathId)) {
              localPaths <- private$localAttachments$filePaths[localPathId]
              if (fullPath) {
                localPathNeedsRelocation <- localPaths != filePaths
              } else {
                localPathNeedsRelocation <- dirname(localPaths) != fileDir
              }
            } else {
              localPathNeedsRelocation <- FALSE
            }
            if (any(localPathNeedsRelocation)) {
              if (fullPath) {
                toDir <- filePaths
              } else {
                toDir <- file.path(fileDir, basename(localPaths[localPathNeedsRelocation]))
              }
              file.copy(localPaths[localPathNeedsRelocation],
                toDir,
                overwrite = TRUE
              )
            }

            if (length(localPaths) == length(filePaths)) {
              return(filePaths)
            }

            fileNames <- fileNames[!isLocalAttachment]
            filePaths <- filePaths[!isLocalAttachment]
          }
        }

        if (!length(private$sid)) {
          return(localPaths)
        }

        if (is.null(private$attachmentData)) {
          private$attachmentData <- private$fetchDataFromDb(private$sid)
        }
        fileNamesRemote <- private$attachmentData[["fileName"]]
        if (allExecPerm) {
          execPerm <- private$attachmentData[["execPerm"]]
          if (length(private$attachmentsUpdateExec$name)) {
            allExecPerm <- FALSE
            updateExecId <- match(private$attachmentsUpdateExec$name, fileNamesRemote)
            invalidExecId <- is.na(updateExecId)
            if (any(invalidExecId)) {
              flog.warn(
                "Invalid attachments marked to be updated (execPerm): %s. This should never happen!",
                paste(private$attachmentsUpdateExec$name[invalidExecId], collapse = ", ")
              )
              updateExecId <- updateExecId[!invalidExecId]
              if (length(updateExecId)) {
                execPerm[updateExecId] <- private$attachmentsUpdateExec$execPerm[!invalidExecId]
              }
            } else {
              execPerm[updateExecId] <- private$attachmentsUpdateExec$execPerm
            }
          }
          fileNamesRemote <- fileNamesRemote[execPerm == TRUE]
        } else {
          fileNamesRemote <- fileNamesRemote[fileNamesRemote %in% fileNames]
        }
        if (length(private$attachmentsToRemove)) {
          allExecPerm <- FALSE
          fileNamesRemote <- fileNamesRemote[!fileNamesRemote %in% private$attachmentsToRemove]
        }
        if (!length(fileNamesRemote)) {
          return(localPaths)
        }
      }

      data <- private$db$importDataset("_scenAttach",
        if (allExecPerm && unmodifiedData) {
          tibble("execPerm", if (inherits(private$conn, "PqConnection")) TRUE else 1)
        } else {
          tibble(rep.int("fileName", length(fileNamesRemote)), fileNamesRemote)
        },
        colNames = c("fileName", "fileContent"), innerSepAND = FALSE,
        subsetSids = if (is.null(scenId)) private$sid else scenId
      )
      if (length(data)) {
        if (allExecPerm && unmodifiedData) {
          remotePaths <- file.path(filePath, data[["fileName"]])
        } else {
          remotePaths <- filePaths[match(data[["fileName"]], fileNames)]
        }
        Map(writeBin, data[["fileContent"]], remotePaths)
        if (allExecPerm && is.null(scenId)) {
          private$localAttachmentsNotCleaned <- unique(c(
            private$localAttachmentsNotCleaned,
            remotePaths
          ))
        }
      }
      return(c(remotePaths, localPaths))
    },
    setExecPerm = function(session, fileNames, execPerm) {
      # Sets execute permission for particular attachment
      #
      # Args:
      #   session:   session object
      #   fileNames: vector if filenames
      #   execPerm:  logical vector (same length as fileNames or length 1)
      #              that specifies whether data can be executed by GAMS
      #
      # Returns:
      #   R6 object (reference to itself)
      if (!is.null(session) && self$isReadonly(session)) {
        stop_custom("error_readonly",
          "Tried to modify attachment data from readonly tab.",
          call. = FALSE
        )
      }
      if (length(fileNames) < 1L) {
        return(invisible(self))
      }
      stopifnot(
        is.character(fileNames), is.logical(execPerm),
        length(execPerm) == 1L || length(execPerm) == length(fileNames)
      )

      if (length(execPerm) != length(fileNames)) {
        execPerm <- rep.int(execPerm, length(fileNames))
      }
      flog.trace("Request to update exec permissions received (Attachments.setExecPerm).")

      localFileIds <- match(fileNames, basename(private$localAttachments$filePaths))

      if (any(is.na(localFileIds))) {
        remoteFileIds <- match(fileNames, self$getIds())
        if (any(is.na(remoteFileIds))) {
          stop_custom("error_not_found",
            sprintf(
              "Some of the files (%s) do not exist",
              paste(fileNames[is.na(remoteFileIds)])
            ),
            call. = FALSE
          )
        }
        fileNamesToUpdate <- fileNames[is.na(localFileIds)]
        execPermToUpdate <- execPerm[is.na(localFileIds)]

        updateIds <- match(fileNamesToUpdate, private$attachmentsUpdateExec$name)
        isNewUpdate <- is.na(updateIds)
        if (any(isNewUpdate)) {
          private$attachmentsUpdateExec$name <- c(
            private$attachmentsUpdateExec$name,
            fileNamesToUpdate[isNewUpdate]
          )
          private$attachmentsUpdateExec$execPerm <- c(
            private$attachmentsUpdateExec$execPerm,
            execPermToUpdate[isNewUpdate]
          )
          execPermToUpdate <- execPermToUpdate[!isNewUpdate]
        }
        if (length(execPermToUpdate)) {
          private$attachmentsUpdateExec$execPerm[updateIds[!isNewUpdate]] <- execPermToUpdate
        }
        fileNames <- fileNames[!is.na(localFileIds)]
        if (!length(fileNames)) {
          private$markUnsaved(markDirty = TRUE)
          lapply(names(private$updateCallbacks[["1"]]), function(symName) {
            private$updateCallbacks[["1"]][[symName]]()
          })
          return(invisible(self))
        }
        execPerm <- execPerm[!is.na(localFileIds)]
        localFileIds <- localFileIds[!is.na(localFileIds)]
      }

      newPaths <- vapply(seq_along(fileNames), function(i) {
        if (execPerm[i]) {
          return(file.path(private$workDir, fileNames[i]))
        }
        return(file.path(private$workDir, "_miro_attach_", fileNames[i]))
      }, character(1L), USE.NAMES = FALSE)

      hasNewExecPerm <- private$localAttachments$execPerm[localFileIds] != execPerm

      if (!all(hasNewExecPerm)) {
        newPaths <- newPaths[hasNewExecPerm]
        if (!length(newPaths)) {
          return(invisible(self))
        }
        execPerm <- execPerm[hasNewExecPerm]
        localFileIds <- localFileIds[hasNewExecPerm]
      }

      failedFileMoves <- !file.move(
        private$localAttachments$filePaths[localFileIds],
        newPaths
      )
      if (any(failedFileMoves)) {
        flog.warn(
          "Problems moving attachment(s): '%s' (Attachments.setExecPerm).",
          paste(basename(newPaths[failedFileMoves]), collapse = ", ")
        )
        newPaths <- newPaths[!failedFileMoves]
        if (!length(newPaths)) {
          return(invisible(self))
        }
        execPerm <- execPerm[!failedFileMoves]
        localFileIds <- localFileIds[!failedFileMoves]
      }

      private$localAttachments$filePaths[localFileIds] <- newPaths
      private$localAttachments$execPerm[localFileIds] <- execPerm

      private$markUnsaved(markDirty = TRUE)
      lapply(names(private$updateCallbacks[["1"]]), function(symName) {
        private$updateCallbacks[["1"]][[symName]]()
      })

      invisible(self)
    },
    remove = function(session, fileNames, removeLocal = TRUE) {
      # Removes attachments
      #
      # Args:
      #   session:     session object
      #   fileNames:   file names of attachments to remove
      #   removeLocal: whether to remove file from disk
      #
      # Returns:
      #   R6 object (reference to itself)
      if (length(fileNames) < 1L) {
        return(invisible(self))
      }

      stopifnot(
        is.character(fileNames),
        is.logical(removeLocal), length(removeLocal) == 1L
      )

      if (!is.null(session) && self$isReadonly(session)) {
        stop_custom("error_readonly",
          "Tried to modify attachment data from readonly tab.",
          call. = FALSE
        )
      }
      markDirty <- any(as.logical(self$getMetadata()[["execPerm"]]))

      private$.remove(fileNames, removeLocal)

      private$markUnsaved(markDirty = markDirty)
      lapply(names(private$updateCallbacks[["1"]]), function(symName) {
        private$updateCallbacks[["1"]][[symName]]()
      })
      return(invisible(self))
    },
    removeAll = function() {
      # Removes all attachments from scenario
      #
      # Returns:
      #   R6 object (reference to itself)

      attachmentsToRemove <- self$getIds()

      if (length(attachmentsToRemove) == 0L) {
        return(invisible(self))
      }
      return(private$.remove(attachmentsToRemove,
        removeLocal = TRUE
      ))
    },
    clear = function(cleanLocal = FALSE) {
      # clears attachments in sandbox
      private$resetOpQueue()
      private$sid <- NULL
      if (cleanLocal) {
        private$removeLocalFiles()
      }
      return(invisible(self))
    },
    removeExpired = function(fileNames, maxDuration) {
      # removes attachments that have exceeded maximum storage duration
      #
      # Args:
      #    fileNames:     file names to remove
      #    maxDuration:   maximum storage duration in days
      #
      # Returns:
      #    integer: number of rows affected
      stopifnot(
        is.character(fileNames), length(fileNames) >= 1L,
        is.integer(maxDuration), length(maxDuration) == 1L
      )

      tableNameDb <- dbSchema$getDbTableName("_scenAttach")
      if (!DBI::dbExistsTable(private$conn, tableNameDb)) {
        return(invisible(self))
      }
      expirationTime <- as.character(Sys.time() - 3600L * 24L * maxDuration,
        usetz = TRUE, tz = "GMT"
      )
      query <- paste0(
        "DELETE FROM ", dbQuoteIdentifier(private$conn, tableNameDb),
        " WHERE ", dbQuoteIdentifier(private$conn, "fileName"),
        " IN (",
        paste(dbQuoteString(private$conn, fileNames), collapse = ", "),
        ") AND ",
        dbQuoteIdentifier(private$conn, "timestamp"),
        "<",
        dbQuoteString(private$conn, expirationTime)
      )
      affectedRows <- private$db$runQuery(query)
      if (affectedRows > 0L) {
        flog.debug(
          "Db: %s attachments in table: '%s' were deleted due to surpassing the expiration date (Attachments.removeExpired).",
          affectedRows, tableNameDb
        )
      }
      return(invisible(self))
    }
  ),
  private = list(
    sid = NULL,
    inDataSid = NULL,
    db = NULL,
    conn = NULL,
    config = NULL,
    attachmentData = NULL,
    workDir = NULL,
    localAttachments = list(
      filePaths = character(0L),
      execPerm = logical(0L)
    ),
    localAttachmentsNotCleaned = character(0L),
    attachmentsToRemove = character(0L),
    attachmentsUpdateExec = list(
      name = character(0L),
      execPerm = logical(0L)
    ),
    removeLocalFiles = function() {
      if (length(private$localAttachmentsNotCleaned)) {
        removedLocalFiles <- file.remove(private$localAttachmentsNotCleaned)
        if (any(!removedLocalFiles)) {
          flog.warn(
            "Some local attachments could not be removed: '%s' (Attachments.removeLocalFiles).",
            paste(private$localAttachmentsNotCleaned[!removedLocalFiles],
              collapse = "', '"
            )
          )
        }
        private$localAttachmentsNotCleaned <- character(0L)
      }
      return(invisible(self))
    },
    resetOpQueue = function() {
      private$localAttachmentsNotCleaned <- unique(c(
        private$localAttachmentsNotCleaned,
        private$localAttachments$filePaths
      ))
      private$localAttachments <- list(
        filePaths = character(0L),
        execPerm = logical(0L)
      )
      private$attachmentsToRemove <- character(0L)
      private$attachmentsUpdateExec <- list(
        name = character(0L),
        execPerm = logical(0L)
      )
      private$attachmentData <- NULL
      return(invisible(self))
    },
    fetchDataFromDb = function(sids) {
      return(private$db$importDataset("_scenAttach",
        colNames = c("fileName", "execPerm"),
        subsetSids = sids
      ))
    },
    getLocalData = function() {
      # reads blob data and returns tibble with meta data and file content
      filePaths <- private$localAttachments$filePaths
      if (!length(filePaths)) {
        return(NULL)
      }
      fileNames <- basename(filePaths)
      fileSize <- file.size(filePaths)
      execPerm <- private$localAttachments$execPerm

      content <- blob::new_blob(lapply(
        seq_along(filePaths),
        function(i) {
          tryCatch(
            readBin(filePaths[[i]], "raw", n = fileSize[[i]]),
            error = function(e) {
              flog.warn(
                "Problems reading file: '%s'. Error message: %s",
                filePaths[[i]],
                conditionMessage(e)
              )
              return(raw(0))
            }, warning = function(w) {
              flog.warn(
                "Problems reading file: '%s'. Warning message: %s",
                filePaths[[i]],
                conditionMessage(w)
              )
              return(raw(0))
            }
          )
        }
      ))
      return(tibble(
        fileName = fileNames,
        fileExt = tools::file_ext(filePaths),
        execPerm = execPerm,
        fileContent = content,
        timestamp = as.character(Sys.time(), usetz = TRUE, tz = "GMT")
      ))
    },
    .remove = function(fileNames, removeLocal) {
      localFiles <- match(
        fileNames,
        basename(private$localAttachments$filePaths)
      )

      if (any(fileNames[is.na(localFiles)] %in% private$attachmentsToRemove)) {
        stop_custom("error_not_found",
          "Some of the files to remove do not exist",
          call. = FALSE
        )
      }
      if (length(private$attachmentData) &&
        any(!fileNames[is.na(localFiles)] %in% private$attachmentData[["fileName"]])) {
        stop_custom("error_not_found",
          "Some of the files to remove do not exist",
          call. = FALSE
        )
      }
      localFilesToRemoveId <- localFiles[!is.na(localFiles)]

      if (length(localFilesToRemoveId)) {
        if (removeLocal) {
          private$.removeLocalFiles(private$localAttachments$filePaths[localFilesToRemoveId])
        }

        private$localAttachments$filePaths <- private$localAttachments$filePaths[-localFilesToRemoveId]
        private$localAttachments$execPerm <- private$localAttachments$execPerm[-localFilesToRemoveId]

        fileNames <- fileNames[is.na(localFiles)]
      }
      if (removeLocal) {
        localFilesToRemoveId <- match(fileNames, basename(private$localAttachmentsNotCleaned))
        localFilesToRemoveId <- localFilesToRemoveId[!is.na(localFilesToRemoveId)]
        if (length(localFilesToRemoveId)) {
          private$.removeLocalFiles(private$localAttachmentsNotCleaned[localFilesToRemoveId])
        }
      }
      updatesToRemove <- match(fileNames, basename(private$attachmentsUpdateExec$name))
      updatesToRemove <- updatesToRemove[!is.na(updatesToRemove)]

      if (length(updatesToRemove)) {
        private$attachmentsUpdateExec$name <- private$
          attachmentsUpdateExec$name[-updatesToRemove]
        private$attachmentsUpdateExec$execPerm <- private$
          attachmentsUpdateExec$execPerm[-updatesToRemove]
      }
      private$attachmentsToRemove <- c(private$attachmentsToRemove, fileNames)
      invisible(self)
    },
    .removeLocalFiles = function(filePaths) {
      removedLocalFiles <- file.remove(filePaths)
      if (any(!removedLocalFiles)) {
        flog.warn(
          "Some local attachments could not be removed: '%s'.",
          paste(filePaths[!removedLocalFiles],
            collapse = "', '"
          )
        )
      }
      private$localAttachmentsNotCleaned <- private$localAttachmentsNotCleaned[!private$localAttachmentsNotCleaned %in% filePaths]
      return(invisible(self))
    }
  )
)

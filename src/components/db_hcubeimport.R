HcubeImport <- R6Class("HcubeImport",
  inherit = Db,
  public = list(
    initialize = function(db, scalarsInputName, scalarsOutputName,
                          tableNamesCanHave, tableNamesMustHave,
                          csvDelim, workDir,
                          gdxio, inputSym, outputSym, templates) {
      # R6 class to import scenarios in hcube mode
      #
      # Args:
      #   db:                      R6 database object
      #   scalarsInputName:        file name of the scalar input file
      #   scalarsOutputName:       file name of the scalar input file
      #   tableNamesCanHave:       tables that can exist in a scenario
      #   tableNamesMustHave:      tables that must exist in order
      #                            for the scenario to be valid
      #   csvDelim:                csv delimiter
      #   workDir:                 directory where temporary files are saved
      #   gdxio:                   gdxio class
      #   inputSym:                input symbols
      #   outputSym:               output symbols
      #   templates:               input/output symbol templates
      #

      # BEGIN error checks
      stopifnot(is.R6(db))
      stopifnot(is.character(scalarsInputName), length(scalarsInputName) == 1)
      stopifnot(is.character(scalarsOutputName), length(scalarsOutputName) == 1)
      stopifnot(is.character(tableNamesCanHave))
      stopifnot(is.character(tableNamesMustHave))
      stopifnot(is.character(csvDelim), length(csvDelim) == 1)
      stopifnot(is.character(workDir), length(workDir) == 1)
      stopifnot(
        is.R6(gdxio), is.character(outputSym),
        is.list(templates)
      )
      # END error checks

      private$conn <- db$getConn()
      private$uid <- db$getUid()
      private$userAccessGroups <- db$getUserAccessGroups()
      private$scalarsInputName <- tolower(scalarsInputName)
      private$scalarsOutputName <- tolower(scalarsOutputName)
      private$tableNamesMustHave <- tableNamesMustHave
      private$tableNamesToVerify <- c(tableNamesCanHave, tableNamesMustHave)
      private$csvDelim <- csvDelim
      private$workDir <- workDir
      private$includeTrc <- "_scenTrc" %in% private$tableNamesToVerify
      private$gdxio <- gdxio
      private$inputSym <- if (is.null(inputSym)) character(0L) else inputSym
      private$outputSym <- outputSym
      private$templates <- templates
      private$noScen <- NA_integer_
    },
    getScenNames = function() private$scenNames,
    getNoScen = function() private$noScen,
    unzipScenData = function(zipFilePath, extractDir) {
      # Unzips a zip archive into the extractDir folder
      #
      # Args:
      #   zipFilePath:     path to zip archive
      #   extractDir:      directory to extract
      #                    zip archive into
      #

      # BEGIN error checks
      stopifnot(length(zipFilePath) == 1)
      stopifnot(identical(tools::file_ext(zipFilePath), "zip"))
      stopifnot(is.character(extractDir), length(extractDir) == 1)
      # END error checks

      filePaths <- private$getFilePaths(zipFilePath)
      private$scenNames <- private$fetchScenNames(filePaths)
      # workaround for unzip function as path with trailing slashes is not found
      if (length(filePaths)) {
        filePaths <- utils::unzip(zipFilePath,
          exdir = gsub(
            "/?$", "",
            private$workDir
          )
        )
        if (any(is.symlink(filePaths))) {
          stop("zip archive contains symlinks.", call. = FALSE)
        }
      }

      private$filePaths <- filePaths
      private$filePaths <- lapply(
        private$scenNames,
        private$getScenFilePaths, filePaths
      )
      names(private$filePaths) <- private$scenNames
      flog.trace("%s files unzipped in: '%s'.", length(private$filePaths), private$workDir)
      invisible(self)
    },
    validateScenFiles = function() {
      # validates scenario data
      #
      # Args:
      #
      # Returns:
      #   reference to itself (importHcube R6 object)


      fileNames <- lapply(
        private$filePaths,
        private$verifyScenFiles
      )
      invalidScen <- vapply(fileNames, is.null, logical(1L), USE.NAMES = FALSE)

      private$invalidScenIds <- private$scenNames[invalidScen]
      private$filePaths[private$invalidScenIds] <- NULL

      return(private$invalidScenIds)
    },
    readAllScenData = function() {
      # Read scenario data and return them as a list of dataframes
      #
      # Args:
      #
      # Returns:
      #    R6 object (reference to itself)
      private$scenData <- lapply(private$filePaths, private$readScenData)
      invisible(self)
    },
    removeDuplicates = function() {
      # Removes the duplicated tables
      #
      # Args:
      #
      # Returns:
      #   R6 object (reference to itself)

      # BEGIN error checks
      stopifnot(is.list(private$scenData), length(private$scenData) >= 1)
      # END error checks

      if (length(private$duplicatedScenIds)) {
        private$scenData[private$duplicatedScenIds] <- NULL
      }

      invisible(self)
    },
    saveScenarios = function(hcubeTags, jobID, readPerm = private$uid,
                             writePerm = private$uid, execPerm = private$uid,
                             progressBar = NULL) {
      # Save multiple scenarios to database
      #
      # Args:
      #   hcubeTags:    character vector with tags to attach to scenario
      #   jobID:        integer (scalar), job ID of Hypercube job to import
      #   readPerm:     character vector with uids/groups that have
      #                 read permissions for scenarios
      #   writePerm:    character vector with uids/groups that have
      #                 write permissions for scenarios
      #   execPerm:     character vector with uids/groups that have
      #                 execute permissions for scenarios
      #   progressBar:  shiny pogress bar R6 object
      #
      # Returns:
      #   reference to itself (HcubeImport R6 object)

      # BEGIN error checks
      stopifnot(is.list(private$scenData), length(private$scenData) >= 1)
      jobID <- as.integer(jobID)
      stopifnot(!is.na(jobID), length(jobID) == 1L)
      stopifnot(is.character(hcubeTags), length(hcubeTags) >= 1)
      stopifnot(
        is.character(names(private$scenData)),
        length(private$scenData) >= 1
      )
      stopifnot(is.character(readPerm), length(readPerm) >= 1)
      stopifnot(is.character(writePerm), length(writePerm) >= 1)
      if (!is.null(progressBar)) {
        stopifnot(is.R6(progressBar))
      }
      # END error checks

      scenData <- private$scenData
      saveTraceFile <- as.integer(private$includeTrc)
      tableNames <- dbSchema$getAllSymbols()
      tableNames <- tableNames[!tableNames %in% ioConfig$hcubeScalars]
      if (saveTraceFile) {
        tableNames <- c(tableNames, "_scenTrc")
      }

      readPerm <- vector2Csv(readPerm)
      writePerm <- vector2Csv(writePerm)
      execPerm <- vector2Csv(execPerm)

      # export metadata to reserve scenario ids
      numberScen <- length(scenData)
      private$noScen <- numberScen
      metadataTable <- tibble(
        `_uid` = rep.int(private$uid, numberScen),
        `_sname` = names(scenData),
        `_stime` = rep.int(1, numberScen),
        `_stag` = rep.int(hcubeTags, numberScen),
        `_accessr` = rep.int(readPerm, numberScen),
        `_accessw` = rep.int(writePerm, numberScen),
        `_accessx` = rep.int(execPerm, numberScen),
        `_scode` = rep.int(jobID, numberScen)
      )
      metadataTable[[3]] <- Sys.time()

      if (inherits(private$conn, "PqConnection")) {
        query <- paste(sqlAppendTable(private$conn, dbSchema$getDbTableName("_scenMeta"),
          sqlData(private$conn, metadataTable),
          row.names = FALSE
        ), "RETURNING _sid")
        firstScenId <- as.integer(DBI::dbGetQuery(private$conn, query)[[1]][1])
      } else {
        self$writeMetadata(metadataTable)
        query <- SQL("SELECT LAST_INSERT_ROWID();")
        lasInsertedRowId <- as.integer(DBI::dbGetQuery(private$conn, query)[[1]][1])
        firstScenId <- lasInsertedRowId - numberScen + 1L
      }
      stopifnot(firstScenId >= 1L)

      if (!is.null(progressBar)) {
        progressBar$inc(amount = 0, message = sprintf("Uploading tables to database."))
      }
      if (!dbBegin(private$conn)) {
        stop("Could not start database transaction.", call. = FALSE)
      }
      tryCatch(
        {
          lapply(seq_along(tableNames), function(i) {
            tableName <- tableNames[i]
            self$exportScenDataset(bind_rows(lapply(seq_along(private$scenData), function(scenIdx) {
              add_column(
                `_sid` = firstScenId + scenIdx - 1L,
                private$scenData[[scenIdx]][[tableName]],
                .before = 1L
              )
            })), tableName, isHcJobConfig = FALSE)
            if (!is.null(progressBar)) {
              progressBar$inc(amount = 0, message = sprintf(
                "Uploading table %d of %d.",
                i, length(tableNames)
              ))
            }
          })
          if (!dbCommit(private$conn)) {
            stop_custom("error_commit", "Could not commit database transaction", call. = FALSE)
          }
        },
        error_commit = function(e) {
          stop(conditionMessage(e), call. = FALSE)
        },
        error = function(e) {
          if (!dbRollback(private$conn)) {
            self$deleteRows("_scenMeta",
              subsetSids = seq(
                firstScenId,
                firstScenId + numberScen - 1L
              )
            )
            stop("Could not roll back database transaction.", call. = FALSE)
          }
          self$deleteRows("_scenMeta",
            subsetSids = seq(
              firstScenId,
              firstScenId + numberScen - 1L
            )
          )
          stop(conditionMessage(e), call. = FALSE)
        }
      )
      invisible(self)
    },
    getScenDuplicates = function(scenNames = NULL) {
      # Fetch duplicated scenarios and return them
      #
      # Args:
      #
      #   scenNames:     scenario names to check (optional)
      #
      # Returns:
      #    dataframe with ID and tag columns

      # BEGIN error checks
      if (is.null(scenNames)) {
        stopifnot(length(private$scenNames) >= 1)
        scenNames <- private$scenNames
      } else {
        stopifnot(is.character(scenNames), length(scenNames) >= 1)
      }
      # END error checks

      previousResults <- self$importDataset("_scenMeta",
        colNames = c(
          "_sname",
          "_stag"
        )
      )
      isDuplicated <- previousResults[["_sname"]] %in% scenNames
      private$duplicatedScenIds <- scenNames[isDuplicated]
      return(previousResults[isDuplicated, ])
    },
    finalize = function() {
      NULL
    }
  ),
  private = list(
    conn = NULL,
    scenData = NULL,
    uid = character(0L),
    gdxio = NULL,
    scenNames = character(0L),
    filePaths = character(0L),
    scalarsInputName = character(0L),
    scalarsOutputName = character(0L),
    tableNamesMustHave = character(0L),
    tableNamesToVerify = character(0L),
    csvDelim = character(0L),
    workDir = character(0L),
    invalidScenIds = character(0L),
    duplicatedScenIds = character(0L),
    traceTabName = character(0L),
    includeTrc = logical(0L),
    noScen = integer(1L),
    inputSym = character(0L),
    outputSym = character(0L),
    templates = list(),
    getScenFilePaths = function(scenName, paths) {
      pathIdx <- grepl(scenName %+% "/", paths, fixed = TRUE)
      return(paths[pathIdx])
    },
    readScenData = function(filePaths) {
      scenDataNames <- lapply(tolower(filePaths), function(filePath) {
        if (endsWith(filePath, ".trc")) {
          return("_scenTrc")
        } else if (identical(basename(filePath), MIROGdxInName)) {
          return(private$inputSym)
        } else {
          return(private$outputSym)
        }
      })
      scenData <- unlist(lapply(seq_along(filePaths), function(i) {
        filePath <- filePaths[[i]]
        tryCatch(
          {
            if (endsWith(tolower(filePath), ".trc")) {
              return(list(readTraceData(filePath)[1, ]))
            }
            symNames <- scenDataNames[[i]]
            return(lapply(symNames, function(symName) {
              if (symName %in% c(scalarsFileName, scalarsOutName)) {
                colTypes <- "ccc"
              } else {
                colTypes <- dbSchema$getDbSchema(symName)$colTypes
              }
              scenData <- tryCatch(
                {
                  fixColTypes(private$gdxio$rgdx(filePath, symName), colTypes)
                },
                error = function(e) {
                  private$templates[[symName]]
                }
              )
              if (!length(scenData)) {
                return(tibble())
              }
              if (length(private$templates[[symName]])) {
                names(scenData) <- names(private$templates[[symName]])
              } else if (symName %in% c(scalarsFileName, scalarsOutName)) {
                names(scenData) <- c("scalar", "description", "value")
              }
              return(mutate(scenData, across(
                where(is.character),
                ~ replace_na(.x, replace = "")
              )))
            }))
          },
          error = function(e) {
            stop(
              sprintf(
                "Problems reading file: '%s'. Error message: %s.", filePath,
                conditionMessage(e)
              ),
              call. = FALSE
            )
          }
        )
      }), recursive = FALSE, use.names = FALSE)
      names(scenData) <- unlist(scenDataNames, use.names = FALSE, recursive = FALSE)
      return(scenData)
    },
    verifyScenFiles = function(filePaths) {
      tableNames <- unlist(
        lapply(filePaths, function(filePath) {
          if (endsWith(tolower(filePath), ".trc")) {
            return("_scenTrc")
          }
          gdxSym <- private$gdxio$getSymbols(filePath)
          return(tolower(c(
            gdxSym$sets, gdxSym$parameters,
            gdxSym$variables, gdxSym$equations
          )))
        }),
        use.names = FALSE, recursive = FALSE
      )
      verifiedIds <- match(private$tableNamesMustHave, tableNames)
      if (any(is.na(verifiedIds))) {
        flog.info(
          "The scenario misses some tables that must be included: '%s'.",
          paste(private$tableNamesMustHave[is.na(verifiedIds)], collapse = "', '")
        )
        return(NULL)
      } else {
        verifiedIds <- match(tableNames, private$tableNamesToVerify)
        if (any(is.na(verifiedIds))) {
          flog.info(
            "The scenario includes invalid datasets: '%s'.",
            paste(tableNames[is.na(verifiedIds)], collapse = "', '")
          )
          return(NULL)
        } else {
          return(tableNames)
        }
      }
    },
    getFilePaths = function(zipFilePath) {
      validFileNames <- c(MIROGdxInName, MIROGdxOutName)

      if (private$includeTrc) {
        validFileNames <- c(
          validFileNames, "_scenTrc.trc",
          paste0("_sys_trace_", modelName, ".trc")
        )
      }
      fileNamesZip <- zip_list(zipFilePath)
      fileNamesZip <- fileNamesZip[fileNamesZip$compressed_size > 0, ]$filename

      if (any(!basename(fileNamesZip) %in% validFileNames)) {
        stop("invalidFiles", call. = FALSE)
      }
      return(fileNamesZip)
    },
    fetchScenNames = function(filePaths) {
      return(unique(dirname(filePaths)))
    }
  )
)

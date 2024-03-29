InputDataInstance <- R6Class("InputDataInstance", public = list(
  initialize = function(fileExchange = c("csv", "gdx"),
                        gdxio = NULL,
                        csvDelim = ",",
                        sortedNames = character(0L),
                        activeScen = NULL,
                        attachments = NULL,
                        views = NULL) {
    if (length(gdxio)) {
      private$gdxio <- gdxio
    }
    if (is.character(sortedNames) && length(sortedNames)) {
      private$sortedNames <- sortedNames
    }
    private$fileExchange <- match.arg(fileExchange)
    private$csvDelim <- csvDelim
    private$activeScen <- activeScen
    private$attachments <- attachments
    private$views <- views
    return(invisible(self))
  },
  getDataHashes = function() private$dataHashes,
  getScalarData = function() {
    emptyScalarTibble <- tibble(scalar = character(), value = character())
    if (length(private$clArgsDf)) {
      clArgs <- private$clArgsDf[c(1, 3)]
    } else {
      clArgs <- emptyScalarTibble
    }
    inputScalars <- tryCatch(self$get(scalarsFileName)[c(1, 3)], error = function(e) {
      emptyScalarTibble
    })
    return(pivot_wider(bind_rows(clArgs, inputScalars),
      names_from = "scalar", values_from = "value"
    ))
  },
  pushClArgs = function(data) {
    if (!identical(length(data), 3L)) {
      return(invisible(self))
    }
    private$clArgsDf <- data
    isEmptyClArg <- is.na(data[[3]]) | nchar(data[[3]]) == 0L | data[[3]] %in% CLARG_MISSING_VALUES
    data[isEmptyClArg, 3] <- NA_character_
    clArgs <- sort(c(ioConfig$DDPar, ioConfig$GMSOpt))
    clArgVals <- data[[3]][match(clArgs, data[[1]])]
    names(clArgVals) <- clArgs
    private$dataHashes[paste0("__cl_", clArgs)] <- vapply(clArgs, function(clArg) {
      if (is.na(clArgVals[clArg])) {
        return(NA_character_)
      }
      if (clArg %in% ioConfig$DDPar) {
        return(paste0("--", substring(clArg, 9L), "= ", escapeGAMSCL(clArgVals[clArg])))
      }
      return(paste0(substring(clArg, 9L), "= ", escapeGAMSCL(clArgVals[clArg])))
    }, character(1L), USE.NAMES = FALSE)
    return(invisible(self))
  },
  getClArgs = function() {
    if (!length(private$dataHashes)) {
      return(character())
    }
    clArgsTmp <- unlist(private$dataHashes[startsWith(names(private$dataHashes), "__cl_")],
      use.names = FALSE
    )
    return(clArgsTmp[!is.na(clArgsTmp)])
  },
  push = function(datasetName, data) {
    stopifnot(
      is.character(datasetName),
      identical(length(datasetName), 1L),
      inherits(data, "data.frame")
    )
    private$data[[datasetName]] <- data
    if (identical(datasetName, scalarsFileName)) {
      scalarsConfig <- ioConfig$modelInRaw[[scalarsFileName]]
      scalarVals <- data[[3]][match(scalarsConfig$symnames, data[[1]])]
      private$dataHashes[scalarsConfig$symnames] <- vapply(seq_along(scalarsConfig$symnames), function(scalarId) {
        if (identical(scalarsConfig$symtypes[scalarId], "set")) {
          ddConfig <- ioConfig$modelIn[[scalarsConfig$symnames[scalarId]]]$dropdown
          if (length(ddConfig)) {
            scalarVal <- scalarVals[scalarId]
            if (length(ddConfig$aliases)) {
              symText <- ddConfig$aliases[match(scalarVal, ddConfig$choices)]
              if (identical(ddConfig$clearValue, TRUE)) {
                return(paste0(
                  "--HCUBE_SCALART_", scalarsConfig$symnames[scalarId], "= ",
                  escapeGAMSCL(symText)
                ))
              }
              scalarText <- paste0(
                " --HCUBE_SCALART_", scalarsConfig$symnames[scalarId], "= ",
                escapeGAMSCL(symText)
              )
            } else {
              scalarText <- ""
            }
          } else if (scalarsConfig$symnames[scalarId] %in% ioConfig$textOnlySymbols) {
            return(paste0(
              "--HCUBE_SCALART_", scalarsConfig$symnames[scalarId], "= ",
              escapeGAMSCL(scalarVals[scalarId])
            ))
          } else {
            scalarVal <- strsplit(scalarVals[scalarId], "||", fixed = TRUE)[[1]]
            if (length(scalarVal) > 1L) {
              scalarText <- paste0(
                " --HCUBE_SCALART_", scalarsConfig$symnames[scalarId], "= ",
                escapeGAMSCL(paste0(scalarVal[-1], collapse = "||"))
              )
            } else {
              scalarText <- ""
            }
          }
          return(paste0(
            "--HCUBE_SCALARV_", scalarsConfig$symnames[scalarId], "= ",
            if (is.na(scalarVal[1])) "" else escapeGAMSCL(scalarVal[1]), scalarText
          ))
        }
        return(paste0(
          "--HCUBE_SCALARV_", scalarsConfig$symnames[scalarId], "= ",
          scalarVals[scalarId]
        ))
      }, character(1L), USE.NAMES = FALSE)
    } else {
      private$dataHashes[[datasetName]] <- paste0(
        "--HCUBE_STATIC_", datasetName, "= ",
        digest::digest(as.list(data), algo = "md5")
      )
    }
    return(invisible(self))
  },
  generateScenHash = function() {
    dataHashesTmp <- private$dataHashes[!is.na(private$dataHashes)]
    if (!length(dataHashesTmp)) {
      return(digest::digest("", algo = "sha256", serialize = FALSE))
    }
    hashesToOrder <- startsWith(names(dataHashesTmp), "__")
    scenHashOrder <- order(names(dataHashesTmp)[hashesToOrder])
    scenHash <- paste(
      c(
        unlist(dataHashesTmp[!hashesToOrder], use.names = FALSE),
        unlist(dataHashesTmp[hashesToOrder][scenHashOrder], use.names = FALSE)
      ),
      collapse = " "
    )
    return(digest::digest(scenHash, algo = "sha256", serialize = FALSE))
  },
  addInexFile = function(workDir, modelDataFiles) {
    if (!length(modelDataFiles)) {
      return(NULL)
    }
    stopifnot(
      is.character(modelDataFiles), is.character(workDir),
      identical(length(workDir), 1L)
    )

    inexFileName <- file.path(workDir, "_inex_file_")

    jsonlite::write_json(list(files = modelDataFiles, type = "include"),
      inexFileName,
      auto_unbox = TRUE
    )

    return(inexFileName)
  },
  add = function(datasetNames, data) {
    stopifnot(identical(length(datasetNames), length(data)))
    for (i in seq_along(datasetNames)) {
      self$push(datasetNames[[i]], data[[i]])
    }
    return(invisible(self))
  },
  get = function(datasetName = NULL) {
    if (length(datasetName)) {
      stopifnot(is.character(datasetName), identical(length(datasetName), 1L))
      if (length(private$data) && datasetName %in% names(private$data)) {
        return(private$data[[datasetName]])
      }
      stop("Dataset not found.", call. = FALSE)
    }
    return(private$data)
  },
  addFilePaths = function(filePaths) {
    if (is.character(filePaths) && length(filePaths)) {
      private$filePaths <- c(private$filePaths, filePaths)
      private$dataHashes[paste0("__xattach_", basename(filePaths))] <- vapply(filePaths, function(filePath) {
        paste0("--HCUBE_STATIC_", basename(filePath), "= ", digest::digest(file = filePath, algo = "md5"))
      }, character(1L), USE.NAMES = FALSE)
    }
    return(invisible(self))
  },
  writeDisk = function(filePath, datasetName = NULL, fileName = NULL, ...) {
    stopifnot(is.character(filePath), identical(length(filePath), 1L))
    if (length(datasetName)) {
      stopifnot(is.character(datasetName), identical(datasetName, 1L))
      if (length(private$data) && datasetName %in% names(private$data)) {
        return(self)
      }
      idsToWrite <- match(datasetName, names(private$data))
    } else if (length(private$sortedNames)) {
      idsToWrite <- order(match(names(private$data), private$sortedNames))
    } else {
      idsToWrite <- seq_along(private$data)
    }
    if (private$fileExchange == "csv") {
      return(private$writeCSV(filePath, idsToWrite, ...))
    }

    return(private$writeGDX(file.path(filePath, fileName), idsToWrite, ...))
  },
  copyMiroWs = function(wsPath, jobName = NULL) {
    miroMetaDir <- file.path(wsPath, "_miro_ws_")
    if (file.exists(miroMetaDir) &&
      !identical(unlink(miroMetaDir, recursive = TRUE, force = TRUE), 0L)) {
      stop(sprintf("Could not remove (temporary) directory: %s", miroMetaDir), call. = FALSE)
    }
    if (!dir.create(miroMetaDir)) {
      stop(sprintf("Could not create (temporary) directory: %s", miroMetaDir), call. = FALSE)
    }
    generateMiroScenMeta(miroMetaDir, private$activeScen$getMetadataDf(),
      private$attachments, private$views,
      tabsetId = 1L, clArgs = private$clArgsDf, jobName = jobName
    )
    private$addDirPaths(miroMetaDir)
    return(invisible(self))
  },
  compress = function(fileName = NULL, recurse = FALSE) {
    if (!is.null(fileName)) {
      stopifnot(is.character(fileName), identical(length(fileName), 1L))
    } else {
      fileName <- tempfile(fileext = ".zip")
    }
    if (!length(private$filePaths)) {
      stop("Nothing to compress.", call. = FALSE)
    }
    if (length(private$dirPaths)) {
      filesToZip <- c(private$filePaths, private$dirPaths)
      rootPath <- unique(dirname(filesToZip))
      if (length(rootPath) > 1L) {
        stop("Zipping files and directories with different root currently not supported!", call. = FALSE)
      }
      zipr(fileName, basename(filesToZip), recurse = TRUE, compression_level = 9L, mode = "mirror", root = rootPath)
    } else {
      zipr(fileName, private$filePaths, recurse = recurse, compression_level = 9L)
    }
    return(invisible(fileName))
  }
), private = list(
  data = list(),
  clArgsDf = NULL,
  dataHashes = list(),
  gdxio = NULL,
  filePaths = character(0L),
  dirPaths = character(0L),
  fileExchange = character(1L),
  csvDelim = character(1L),
  sortedNames = character(0L),
  activeScen = NULL,
  attachments = NULL,
  views = NULL,
  writeGDX = function(filePath, idsToWrite, squeezeZeros = c("n", "y", "e")) {
    squeezeZeros <- match.arg(squeezeZeros)
    private$gdxio$wgdx(filePath, private$data[idsToWrite], squeezeZeros)
    private$filePaths <- c(private$filePaths, filePath)
    return(invisible(self))
  },
  writeCSV = function(filePath, idsToWrite) {
    for (id in idsToWrite) {
      fileName <- file.path(
        filePath,
        paste0(
          names(private$data)[[id]],
          ".csv"
        )
      )
      write_delim(private$data[[id]], fileName,
        delim = private$csvDelim, na = ""
      )
    }
    private$filePaths <- c(
      private$filePaths,
      paste0(
        filePath,
        names(private$data)[idsToWrite],
        ".csv"
      )
    )
    return(invisible(self))
  },
  addDirPaths = function(dirPaths) {
    if (is.character(dirPaths) && length(dirPaths)) {
      private$dirPaths <- c(private$dirPaths, dirPaths)
    }
    return(invisible(self))
  }
))

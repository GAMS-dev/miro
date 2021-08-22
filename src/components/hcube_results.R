HcubeResults <- R6Class("HcubeResults", public = list(
  initialize = function(db, gdxio, filePath, outputAttachments, jobMeta,
                        includeTrc = TRUE, filesMayInclude = character()){
    private$db <- db
    private$conn <- private$db$getConn()
    private$filePath <- filePath
    private$gdxio <- gdxio
    private$outputAttachments <- outputAttachments
    private$jobMeta <- jobMeta
    private$includeTrc <- includeTrc

    private$hcScalars <- private$db$importDataset("_hcScalars",
                                                  subsetSids = jobMeta[["_sid"]][1])[-1]
    if(!length(private$hcScalars) && !nrow(private$hcScalars)){
      flog.error("HcubeResults: Hypercube job configuration with id: %s could not be found. This should never happen and is likely an attempt to tamper with the app!", jobMeta[["_sid"]][1])
      stop_custom("error_not_found", "HC job configuration not found", call. = FALSE)
    }

    validFileNames <- c(outputAttachments, MIROGdxOutName)

    if(private$includeTrc){
      validFileNames <- c(validFileNames, "_scenTrc.trc")
    }
    tryCatch({
      fileNamesZip   <- zip_list(filePath)
    }, error = function(e){
      flog.error("HcubeResults: Problems validating Hypercube results zip file. Error message: %s", conditionMessage(e))
      stop_custom("error_bad_format", "Invalid Hypercube results file format", call. = FALSE)
    })
    fileNamesZip   <- fileNamesZip[fileNamesZip$compressed_size > 0, ]$filename

    invalidFiles   <- !basename(fileNamesZip) %in% c(validFileNames, filesMayInclude)
    if(any(invalidFiles)){
      flog.error("HcubeResults: The Hypercube results zip file contains invalid files: %s",
                 paste(fileNamesZip[invalidFiles], collapse = ", "))
      stop_custom("error_invalid_results", "Invalid Hypercube results file", call. = FALSE)
    }
    hashesInZip    <- sort(unique(unique(vapply(strsplit(fileNamesZip, "/"),
                                                "[[",character(1L), 1L))))
    hashesInDb     <- sort(unique(private$hcScalars[[1]]))
    if(!identical(length(hashesInDb), length(hashesInZip)) ||
       any(hashesInDb != hashesInZip)){
      flog.error("HcubeResults: The Hypercube results zip file does not match the configuration stored in the database.")
      stop_custom("error_invalid_results", "Invalid Hypercube results file", call. = FALSE)
    }
    private$scenHashes <- hashesInDb
    private$resultsValidated <- TRUE
    return(invisible(self))
  },
  extractArchive = function(session, workDir, progressCallback = NULL,
                            callbackError = NULL,
                            callbackSuccess = NULL){
    stopifnot(private$resultsValidated)
    private$exdir <- file.path(workDir, paste0(uid, "_", private$jobMeta[["_sid"]][1]))
    if(dir.exists(private$exdir) && !identical(unlink(private$exdir,), 0L)){
      flog.error("HcubeResults: Problems removing existing temporary directory: %s",
                 private$exdir)
      stop_custom("error_extraction", "Failed extracting Hypercube results archive.", call. = FALSE)
    }
    if(!dir.create(private$exdir, recursive = TRUE)){
      flog.error("HcubeResults: Problems creating temporary directory to extract Hypercube results into: %s",
                 private$exdir)
      stop_custom("error_extraction", "Failed extracting Hypercube results archive.", call. = FALSE)
    }
    private$unzipProc <- unzip_process()$new(private$filePath,
                                             exdir = private$exdir,
                                             stderr = "|")
    private$obs <- observe({
      procStat <- private$unzipProc$get_exit_status()
      if(length(procStat)){
        private$obs$destroy()
        if(identical(procStat, 0L)){
          if(length(callbackSuccess)){
            callbackSuccess()
          }
        }else{
          if(length(callbackError)){
            callbackError(procStat, private$unzipProc$read_all_error())
          }
        }
        private$unzipProc <- NULL
      }else{
        if(length(progressCallback)){
          progressCallback(self$getUnzipProgress())
        }
        invalidateLater(1000, session)
      }
    })
    return(invisible(self))
  },
  getUnzipProgress = function(){
    if(!length(private$unzipProc)){
      return(1L)
    }
    return((length(list.dirs(private$exdir, recursive = FALSE)) - 1L)/length(private$scenHashes))
  },
  interruptUnzipProc = function(){
    if(length(private$unzipProc)){
      private$unzipProc$kill()
    }
    return(invisible(self))
  },
  writeToDb = function(progressCallback = NULL){
    tryCatch({
      numberScen     <- length(private$scenHashes)
      metadataTable  <- tibble(`_uid` = rep.int(uid, numberScen),
                               `_sname` = private$scenHashes,
                               `_stime` = rep.int(Sys.time(), numberScen),
                               `_stag` = rep.int(private$jobMeta[["_stag"]][1], numberScen),
                               `_accessr` = rep.int(vector2Csv(uid), numberScen),
                               `_accessw` = rep.int(vector2Csv(uid), numberScen),
                               `_accessx` = rep.int(vector2Csv(uid), numberScen),
                               `_scode` = rep.int(private$jobMeta[["_sid"]][1], numberScen) + 10000L)

      if(inherits(private$conn, "PqConnection")){
        query <- paste(sqlAppendTable(private$conn, dbSchema$getDbTableName("_scenMeta"),
                                      sqlData(private$conn, metadataTable),
                                      row.names = FALSE), "RETURNING _sid")
        firstScenId <- as.integer(DBI::dbGetQuery(private$conn, query)[[1]][1])
      }else{
        private$db$writeMetadata(metadataTable)
        query <- SQL("SELECT LAST_INSERT_ROWID();")
        lasInsertedRowId <- as.integer(DBI::dbGetQuery(private$conn, query)[[1]][1])
        firstScenId <- lasInsertedRowId - numberScen + 1L
      }
      progressResolution <- numberScen * (length(ioConfig$modelOut) +
                                            private$includeTrc + length(private$outputAttachments))
      i <- 1L
      scenFiles <- c("_scenTrc.trc", private$outputAttachments,
                     MIROGdxOutName)
      for(scenIdx in seq_len(numberScen)){
        scenHash <- private$scenHashes[[scenIdx]]
        scenFilesExist <- setNames(file.exists(file.path(private$exdir, scenHash, scenFiles)),
                                   scenFiles)
        hcScalarsTmp <- add_column(filter(private$hcScalars, `_hash` == scenHash)[-1],
                                   description = "", .after = 1L)
        private$db$exportScenDataset(
          bind_cols(`_sid` = rep.int(firstScenId + scenIdx - 1L, nrow(hcScalarsTmp)),
                    hcScalarsTmp),
          "_scalars", isHcJobConfig = FALSE)
        if(private$includeTrc){
          if(scenFilesExist[["_scenTrc.trc"]]){
            scenData <- readTraceData(file.path(private$exdir, scenHash,
                                                "_scenTrc.trc"))[1, ]
            private$db$exportScenDataset(
              bind_cols(`_sid` = firstScenId + scenIdx - 1L, scenData),
              "_scenTrc", isHcJobConfig = FALSE)
          }
          if(length(progressCallback)){
            progressCallback(i / progressResolution)
          }
          i <- i + 1L
        }
        gdxOutPath <- file.path(private$exdir, scenHash,
                                MIROGdxOutName)
        isNewGdx <- TRUE
        if(scenFilesExist[[MIROGdxOutName]]){
          for(symName in names(ioConfig$modelOut)){
            if(identical(symName, scalarsOutName)){
              dbSchemaTmp <- list(colNames = c("scalar", "description", "value"),
                                  colTypes = "ccc")
            }else{
              dbSchemaTmp <- dbSchema$getDbSchema(symName)
            }
            scenData <- tryCatch({
              fixColTypes(private$gdxio$rgdx(gdxOutPath, symName,
                                             names = dbSchemaTmp$colNames,
                                             isNewGdx = isNewGdx),
                          dbSchemaTmp$colTypes)
            }, error = function(e){
              NULL
            })
            isNewGdx <- FALSE
            if(!length(scenData) || !nrow(scenData)){
              i <- i + 1L
              next
            }
            names(scenData) <- dbSchemaTmp$colNames
            private$db$exportScenDataset(
              bind_cols(`_sid` = rep.int(firstScenId + scenIdx - 1L,
                                         nrow(scenData)),
                        scenData) %>%
                mutate_if(is.character,
                          replace_na, replace = ""),
              symName, isHcJobConfig = FALSE)
            if(length(progressCallback)){
              progressCallback(i / progressResolution)
            }
            i <- i + 1L
          }
        }else{
          i <- i + length(ioConfig$modelOut)
        }
        for(outputAttachment in private$outputAttachments){
          if(scenFilesExist[[outputAttachment]]){
            fileSize <- file.size(file.path(private$exdir, scenHash, outputAttachment))
            if(fileSize <= attachMaxFileSize){
              private$db$exportScenDataset(
                bind_cols(`_sid` = firstScenId + scenIdx - 1L,
                          tibble(fileName = outputAttachment,
                                 fileExt = tools::file_ext(outputAttachment),
                                 execPerm = FALSE,
                                 fileContent = blob::new_blob(
                                   list(readBin(file.path(private$exdir, scenHash, outputAttachment), "raw",
                                                n = fileSize))),
                                 timestamp = as.character(Sys.time(), usetz = TRUE, tz = "GMT"))),
                "_scenAttach", isHcJobConfig = FALSE)
            }else{
              flog.warn("HcubeResults: Could not store output attachment: '%s' in database as it exceeds the maximum file size of %s bytes.",
                        outputAttachment, attachMaxFileSize)
            }
          }
          if(length(progressCallback)){
            progressCallback(i / progressResolution)
          }
          i <- i + 1L
        }
      }
      if(numberScen > 0L){
        private$db$exportScenDataset(
          tibble(`_sid` = seq(firstScenId, firstScenId + numberScen - 1L),
                 hash = private$scenHashes),
          "_scenHash", isHcJobConfig = FALSE)
      }
    }, error = function(e){
      flog.error("Problems storing Hypercube job in database. Error message: %s", conditionMessage(e))
      private$db$deleteRows("_scenMeta",
                            subsetSids = seq(firstScenId,
                                             firstScenId + numberScen - 1L))
      private$db$deleteRows("_scenAttach",
                            subsetSids = seq(firstScenId,
                                             firstScenId + numberScen - 1L))
      stop_custom("error_db", "Could not commit database transaction.", call. = FALSE)
    })
    return(invisible(self))
  }),
  private = list(
    conn = NULL,
    db = NULL,
    gdxio = NULL,
    filePath = NULL,
    jobMeta = NULL,
    scenHashes = NULL,
    includeTrc = TRUE,
    outputAttachments = NULL,
    hcScalars = NULL,
    resultsValidated = FALSE,
    exdir = NULL,
    unzipProc = NULL,
    obs = NULL
  )
)

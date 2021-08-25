BatchLoader <- R6Class("BatchLoader",
                       public = list(
                         initialize        = function(db, inputDsNamesNotToDisplay){
                           # R6 class to import scenarios in hcube mode
                           #
                           # Args:
                           #   db:                      R6 database object
                           #   inputDsNamesNotToDisplay: input datasets that are not displayed in UI
                           #

                           # BEGIN error checks
                           stopifnot(is.R6(db))
                           # END error checks

                           private$db                 <- db
                           private$conn               <- db$getConn()
                           private$inputDsNamesNotToDisplay <- inputDsNamesNotToDisplay
                           if(scalarsOutName %in% names(ioConfig$modelOut)){
                             private$scalarTables <- scalarsOutName
                           }
                           if(scalarsFileName %in% names(ioConfig$modelInRaw) ||
                              length(ioConfig$DDPar) || length(ioConfig$GMSOpt)){
                             private$scalarTables <- c(scalarsFileName, private$scalarTables)
                           }
                           return(invisible(self))
                         },
                         fetchResults = function(subsetList, colNames, limit = 1e5){
                           # Executes data from multiple tables and returns result of query
                           #
                           # Args:
                           #   subsetList:      list with dataframes to subset on
                           #                    data frames must be of the form:
                           #                    data.frame(field, val, op, table)
                           #   colNames:        named character vector with column names to select
                           #                    and names being the table to select from
                           #   limit            maximum number of rows to fetch
                           #
                           # Returns:
                           #   tibble with results from query

                           # BEGIN error checks
                           if(length(subsetList))
                             stopifnot(is.list(subsetList))
                           stopifnot(is.character(colNames), length(colNames) >= 1L)
                           stopifnot(!is.na(as.integer(limit)), length(limit) == 1L)
                           # END error checks

                           query <- private$buildQuery(subsetList = subsetList,
                                                       colNames = colNames, limit = limit)
                           tryCatch({
                             flog.trace("Running query: '%s' (BatchLoader.fetchResults).", query)
                             data <- as_tibble(DBI::dbGetQuery(private$conn, query))[, colNames]
                             flog.debug("Db: Data was imported (BatchLoader.fetchResults).")
                           }, error = function(e){
                             stop(sprintf("Db: An error occurred while querying the database (BatchLoader.fetchResults). Error message: '%s'.",
                                          conditionMessage(e)), call. = FALSE)
                           })
                           data[, colNames[!colNames %in% names(data)]] <- NA
                           if(!length(data)){
                             return(tibble())
                           }
                           tagID <- which(endsWith(names(data), "_stag"))[1L]
                           if(!is.na(tagID)){
                             if(any(nchar(data[[tagID]]) > 0)){
                               data[[tagID]] <- substr(data[[tagID]], 2L, nchar(data[[tagID]]) - 1L)
                             }
                           }
                           return(data)
                         },
                         renameScen = function(scenId, suid, newName){
                           if(isBadScenName(newName)){
                             stop_custom("error_bad_name", "Invalid scenario name", call. = FALSE)
                           }
                           scenId <- as.integer(scenId)
                           stopifnot(identical(length(scenId), 1L), !is.na(scenId))
                           if(private$db$checkSnameExists(newName, suid, checkNormalScen = TRUE)){
                             stop_custom("error_scen_exists", "Scenario exists", call. = FALSE)
                           }
                           wasUpdated <- private$db$updateRows("_scenMeta", colNames = "_sname",
                                                               values = newName,
                                                               subsetSids = scenId)
                           if(identical(wasUpdated, 0L)){
                             stop_custom("error_perm", "No write permissions for scenario",
                                         call. = FALSE)
                           }
                           return(invisible(self))
                         },
                         editScenTags = function(scenId, newTags){
                           newTagsV <- trimws(csv2Vector(newTags))
                           if(isBadScenTags(newTags, newTagsV)){
                             stop_custom("error_bad_tags", "Invalid scenario tags", call. = FALSE)
                           }
                           scenId <- as.integer(scenId)
                           stopifnot(identical(length(scenId), 1L), !is.na(scenId))
                           wasUpdated <- private$db$updateRows("_scenMeta", colNames = "_stag",
                                                               values = vector2Csv(newTagsV),
                                                               subsetSids = scenId)
                           if(identical(wasUpdated, 0L)){
                             stop_custom("error_perm", "No write permissions for scenario",
                                         call. = FALSE)
                           }
                           return(invisible(self))
                         },
                         exceedsMaxNoSolvers = function(data, attribs, maxNoGroups, exclAttrib = NULL){
                           stopifnot(inherits(data, "data.frame"))
                           stopifnot(is.character("attribs"), length(attribs) > 0L)

                           if(!inherits(private$conn, "PqConnection")){
                             names(data) <- gsub("^.+\\.", "", names(data))
                           }
                           attribs          <- gsub("^.+\\.", "", attribs)
                           exclAttrib       <- gsub("^.+\\.", "", exclAttrib)
                           sids             <- as.integer(data[[1]])
                           colIdsAttrib     <- match(attribs, names(data))
                           colIdsExclAttrib <- match(exclAttrib, names(data))

                           if(any(is.na(colIdsAttrib))){
                             stop(sprintf("Attributes: '%s' not found in data.",
                                          paste(attribs[is.na(colIdsAttrib)],
                                                collapse = "', '")),
                                  call. = FALSE)
                           }
                           if(any(is.na(colIdsExclAttrib))){
                             stop(sprintf("Attributes: '%s' not found in data.",
                                          paste(exclAttrib[is.na(colIdsExclAttrib)],
                                                collapse = "', '")),
                                  call. = FALSE)
                           }

                           attribs             <- rlang::syms(attribs)
                           groupedData         <- dplyr::group_by(data, !!!attribs)
                           groupedMetaData     <- dplyr::group_data(groupedData)
                           groupedRowIds       <- groupedMetaData[[".rows"]]
                           private$groupedSids <- lapply(groupedRowIds, function(rowIds){
                             sids[rowIds]
                           })
                           if(length(groupedRowIds) > maxNoGroups){
                             return(TRUE)
                           }

                           private$groupedNames <- lapply(private$groupedSids, function(sidGroup) {
                             vapply(sidGroup, function(sid){
                               paste(as.vector(groupedData[groupedData[[1]] == sid, -c(1, colIdsAttrib,
                                                                                       colIdsExclAttrib),
                                                           drop = FALSE]),
                                     collapse = "\\")
                             }, character(1L), USE.NAMES = FALSE)
                           })
                           private$groupLabels          <- groupedMetaData
                           private$groupLabels[".rows"] <- NULL
                           return(FALSE)
                         },
                         genPaverTraceFiles = function(workDir, exclTraceCols = NULL) {
                           stopifnot(length(private$groupedSids) > 0L)
                           stopifnot(length(private$groupLabels) > 0L)
                           stopifnot(is.character(workDir), length(workDir) == 1L)

                           groupLabels   <- vapply(seq_len(nrow(private$groupLabels)), function(i){
                             paste0(as.vector(private$groupLabels[i, ], "character"), collapse = "\\")
                           }, character(1L), USE.NAMES = FALSE)
                           lapply(seq_along(private$groupedSids), function(i){
                             fileName <- file.path(workDir, i %+% ".trc")
                             paverFile <- file(fileName, open = 'wt')
                             writeLines(paste0("* Trace Record Definition\n* GamsSolve\n",
                                               "* ", paste(setdiff(TRACE_COL_NAMES, exclTraceCols), collapse = ","),
                                               "\n*\n* SOLVER,\n* TIMELIMIT,3600\n* NODELIMIT,2100000000\n* GAPLIMIT,0"), con = paverFile)
                             close(paverFile)
                             paverData      <- private$db$importDataset("_scenTrc",
                                                                        subsetSids = private$groupedSids[[i]])
                             if(!length(paverData) ||
                                nrow(paverData) != length(private$groupedSids[[i]])){
                               stop("noTrc", call. = FALSE)
                             }
                             groupedNames   <- gsub(",", "|", private$groupedNames[[i]], fixed = TRUE)
                             paverData      <- paverData[match(private$groupedSids[[i]], paverData[[1]]), -1L]
                             paverData[[1]] <- groupedNames
                             paverData[[3]] <- rep.int(groupLabels[i], nrow(paverData))
                             if(length(exclTraceCols)){
                               paverData[, exclTraceCols] <- NULL
                             }

                             write_csv(paverData, fileName, append = TRUE)

                           })

                         },
                         genGdxFiles = function(scenIds, tmpDir, gdxio, progressBar = NULL,
                                                genScenList = FALSE){
                           stopifnot(length(scenIds) >= 1L, is.character(tmpDir),
                                     length(tmpDir) == 1L)

                           scenTableNames <- dbSchema$getAllSymbols()
                           scenTableNames <- scenTableNames[!scenTableNames %in% private$
                                                              inputDsNamesNotToDisplay]
                           noScenTables   <- length(scenTableNames)
                           noScenIds <- length(scenIds)

                           scenIdDirNameMap <- vector("list", length(scenIds))

                           if(!is.null(progressBar)){
                             noProgressSteps <- noScenTables + noScenIds + 1L
                           }
                           scenIdNameMap <- private$db$
                             importDataset("_scenMeta",
                                           subsetSids = scenIds,
                                           colNames = c("_sid",
                                                        "_sname",
                                                        "_scode"))
                           scenIdsOrdered <- scenIdNameMap[[1]]
                           sidsToFetch <- scenIdNameMap[[3]] > (SCODEMAP[["scen"]] + 10000L)
                           if(any(sidsToFetch)){
                             sidsToFetch <- c(setNames(as.character(scenIdNameMap[[3]][sidsToFetch] - 10000L),
                                                       scenIdsOrdered[sidsToFetch]),
                                              setNames(as.character(scenIdsOrdered[!sidsToFetch]),
                                                       scenIdsOrdered[!sidsToFetch]))
                           }else{
                             sidsToFetch <- setNames(as.character(scenIdsOrdered), scenIdsOrdered)
                           }
                           scenIdNameMap  <- setNames(scenIdNameMap[[2]], scenIdsOrdered)
                           if(length(private$scenIdNameMap)){
                             sidNameMapIds <- match(names(scenIdNameMap), names(private$scenIdNameMap))
                             namesNeedRemapping <- !is.na(sidNameMapIds)
                             if(any(namesNeedRemapping)){
                               scenIdNameMap[namesNeedRemapping] <- private$scenIdNameMap[sidNameMapIds[namesNeedRemapping]]
                             }
                           }

                           j <- 1L
                           dataTmp <- lapply(scenTableNames, function(tableName){
                             if(!is.null(progressBar)){
                               progressBar$inc(amount = 1/noProgressSteps,
                                               message = sprintf("Importing table %s of %d from database.",
                                                                 j, noScenTables))
                               j <<- j + 1L
                             }
                             dataDbTmp <- private$db$
                               importDataset(tableName,
                                             subsetSids = if(tableName %in% c(scalarsFileName,
                                                                              names(ioConfig$modelOut))){
                                               scenIds
                                             }else{
                                               as.integer(sidsToFetch)
                                             })
                             return(split(dataDbTmp[-1], dataDbTmp[[1L]]))
                           })
                           names(dataTmp) <- scenTableNames

                           j <- 1L

                           fileNamesTmp <- vector("list", length(scenIds))

                           for(scenId in as.character(scenIds)){
                             if(!is.null(progressBar)){
                               progressBar$inc(amount = 1/noProgressSteps,
                                               message = sprintf("Writing dataset %d of %d.",
                                                                 j, noScenIds))
                             }
                             sanitizedScenName <- sanitizeFn(scenIdNameMap[[scenId]])
                             if(!nchar(sanitizedScenName)){
                               sanitizedScenName <- "~invalid_file_name~"
                             }
                             fnCount <- 1L
                             while(sanitizedScenName %in% fileNamesTmp){
                               sanitizedScenName <- paste0(sanitizedScenName, "(", fnCount, ")")
                               fnCount <- fnCount + 1L
                             }
                             fileNamesTmp[[j]] <- sanitizedScenName
                             gdxio$wgdx(file.path(tmpDir, paste0(sanitizedScenName, ".gdx")),
                                        setNames(lapply(names(dataTmp), function(symName){
                                          if(symName %in% c(scalarsFileName, names(ioConfig$modelOut))){
                                            scenIdToFetch <- scenId
                                          }else{
                                            scenIdToFetch <- sidsToFetch[[scenId]]
                                          }
                                          if(scenIdToFetch %in% names(dataTmp[[symName]])){
                                            return(dataTmp[[symName]][[scenIdToFetch]])
                                          }
                                          return(tibble())
                                        }), names(dataTmp)))
                             j <- j + 1L
                           }
                           if(genScenList){
                             write_lines(paste0(fileNamesTmp, ".gdx"),
                                         file.path(tmpDir, "hcube_file_names.txt"))
                           }
                           if(!is.null(progressBar)){
                             progressBar$inc(amount = 1/noProgressSteps,
                                             message = "Generating zip file.")
                           }
                           return(invisible(self))
                         },
                         genCsvFiles = function(scenIds, tmpDir, progressBar = NULL){
                           stopifnot(length(scenIds) >= 1L)
                           stopifnot(is.character(tmpDir), length(tmpDir) == 1L)

                           scenTableNames <- c("_scenMeta",
                                               dbSchema$getAllSymbols(),
                                               "_scenTrc")
                           noScenTables   <- length(scenTableNames)

                           scenIdDirNameMap <- vector("list", length(scenIds))

                           if(!is.null(progressBar)){
                             noProgressSteps <- noScenTables * 2L + 1L
                           }
                           staticInputSids <- integer(0L)
                           fileNamesTmp <- vector("list", length(scenIds))
                           for(tabId in seq_along(scenTableNames)){
                             if(identical(tabId, 1L)){
                               tableName <- "_metadata_"
                             }else if(identical(tabId, length(scenTableNames))){
                               tableName <- "_trace_"
                             }else{
                               tableName <- scenTableNames[[tabId]]
                             }
                             if(tableName %in% private$inputDsNamesNotToDisplay){
                               noProgressSteps <- noProgressSteps - 2L
                               next
                             }
                             if(!is.null(progressBar)){
                               progressBar$inc(amount = 1/noProgressSteps,
                                               message = sprintf("Importing table %d of %d from database.",
                                                                 tabId, noScenTables))
                             }
                             if(scenTableNames[[tabId]] %in% c(scalarsFileName,
                                                               names(ioConfig$modelOut))){
                               subsetSids <- scenIds
                               isStaticInputDs <- FALSE
                             }else{
                               subsetSids <- c(scenIds, as.integer(staticInputSids))
                               isStaticInputDs <- TRUE
                             }
                             tableTmp <- private$db$
                               importDataset(scenTableNames[[tabId]],
                                             subsetSids = subsetSids)
                             if(identical(tableName, "_metadata_")){
                               staticInputSids <- tableTmp[["_scode"]] > (SCODEMAP[["scen"]] + 10000L)
                               if(any(staticInputSids)){
                                 staticInputSids <- setNames(as.character(tableTmp[["_scode"]][staticInputSids] - 10000L),
                                                             tableTmp[["_sid"]][staticInputSids])
                               }else{
                                 staticInputSids <- integer(0L)
                               }
                             }

                             if(length(tableTmp)){
                               tableTmp <- split(tableTmp, tableTmp[[1]])
                             }else{
                               tableTmp <- list()
                             }


                             if(!is.null(progressBar)){
                               progressBar$inc(amount = 1/noProgressSteps,
                                               message = sprintf("Writing dataset %d of %d.",
                                                                 tabId, noScenTables))
                             }
                             lapply(seq_along(tableTmp), function(i){
                               scenId   <- suppressWarnings(as.integer(tableTmp[[i]][[1L]][[1L]]))

                               if(is.na(scenId)){
                                 stop("Invalid scenario ID.", call. = FALSE)
                               }

                               if(identical(tabId, 1L)){
                                 if(length(private$scenIdNameMap)){
                                   sidNameMapId <- match(as.character(scenId), names(private$scenIdNameMap))
                                   if(!is.na(sidNameMapId)){
                                     sanitizedScenName <- sanitizeFn(private$scenIdNameMap[sidNameMapId])
                                   }else{
                                     sanitizedScenName <- sanitizeFn(tableTmp[[i]][["_sname"]][[1L]])
                                   }
                                 }else{
                                   sanitizedScenName <- sanitizeFn(tableTmp[[i]][["_sname"]][[1L]])
                                 }
                                 if(!nchar(sanitizedScenName)){
                                   sanitizedScenName <- "~invalid_file_name~"
                                 }
                                 fnCount <- 1L
                                 while(sanitizedScenName %in% fileNamesTmp){
                                   sanitizedScenName <- paste0(sanitizedScenName, "(", fnCount, ")")
                                   fnCount <- fnCount + 1L
                                 }
                                 fileNamesTmp[[i]] <<- sanitizedScenName
                                 dirNameScen <- file.path(tmpDir, sanitizedScenName)
                                 scenIdDirNameMap[[scenId]] <<- dirNameScen
                                 if(!dir.create(dirNameScen)){
                                   stop(sprintf("Temporary folder: '%s' could not be created.",
                                                dirNameScen), call. = FALSE)
                                 }
                               }else if(isStaticInputDs && length(staticInputSids)){
                                 if(scenId %in% names(staticInputSids)){
                                   # we should only use save data of input sids
                                   return()
                                 }
                                 originalSids <- match(scenId, staticInputSids)
                                 originalSids <- as.integer(names(staticInputSids)[!is.na(originalSids)])
                                 if(length(originalSids)){
                                   # scenId is static input data pointed to by one or more scenarios
                                   for(originalSid in originalSids){
                                     write_csv(tableTmp[[i]][-1L],
                                               paste0(scenIdDirNameMap[[originalSid]],
                                                      .Platform$file.sep, sanitizeFn(tableName), ".csv"))
                                   }
                                   return()
                                 }
                               }
                               write_csv(tableTmp[[i]][-1L],
                                         paste0(scenIdDirNameMap[[scenId]],
                                                .Platform$file.sep, sanitizeFn(tableName), ".csv"))
                             })
                           }
                           if(!is.null(progressBar)){
                             progressBar$inc(amount = 1/noProgressSteps,
                                             message = "Generating zip file.")
                           }
                           return(invisible(self))
                         },
                         setScenIdNameMap = function(scenIdNameMap){
                           private$scenIdNameMap <- scenIdNameMap
                           return(invisible(self))
                         },
                         finalize = function(){
                           NULL
                         }
                       ),
                       private = list(
                         db                      = NULL,
                         conn                    = NULL,
                         scalarTables            = character(0L),
                         inputDsNamesNotToDisplay = character(0L),
                         groupedData             = list(),
                         groupLabels             = list(),
                         groupedNames            = list(),
                         groupedSids             = list(),
                         scenIdNameMap           = character(),
                         buildQuery              = function(subsetList, colNames, limit){
                           escapedMetaTableName <- dbQuoteIdentifier(private$conn,
                                                                     dbSchema$getDbTableName("_scenMeta"))

                           escapedColNamesToFetch <- paste0(DBI::dbQuoteIdentifier(private$conn,
                                                                                   names(colNames)),
                                                            ".", DBI::dbQuoteIdentifier(private$conn, colNames),
                                                            collapse = ",")
                           leftJoin <- ""
                           if(length(private$scalarTables)){
                             escapedScalarsTableNames <- dbQuoteIdentifier(private$conn,
                                                                           private$scalarTables)
                             leftJoin <- paste(paste0("LEFT JOIN ",
                                                      escapedScalarsTableNames, " ON ",
                                                      escapedMetaTableName, "._sid=",
                                                      escapedScalarsTableNames, "._sid"),
                                               collapse = " ")
                           }
                           subsetRows <- paste0(escapedMetaTableName,
                                                "._scode >= ", as.integer(SCODEMAP[["scen"]]),
                                                " AND (", private$db$getAccessPermSubQuery("_accessr"), ")")
                           if(length(subsetList) > 1L || length(subsetList[[1L]])){
                             subsetRows <- paste(subsetRows, "AND (",
                                                 private$db$buildRowSubsetSubquery(subsetList,
                                                                                   " AND ",
                                                                                   " OR "), ")")
                           }
                           query     <- SQL(paste0("SELECT ", escapedColNamesToFetch, " FROM ",
                                                   escapedMetaTableName, " ", leftJoin,
                                                   " WHERE ",  subsetRows, " LIMIT ",
                                                   as.integer(limit + 1L), ";"))
                           return(query)
                         }
                       )
)

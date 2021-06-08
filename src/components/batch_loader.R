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
                                               "* ", paste(setdiff(traceColNames, exclTraceCols), collapse = ","),
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
                                                        "_sname"))
                           scenIdsOrdered <- scenIdNameMap[[1]]
                           scenIdNameMap  <- setNames(scenIdNameMap[[2]], scenIdsOrdered)
                           
                           j <- 1L
                           dataTmp <- lapply(scenTableNames, function(tableName){
                             if(!is.null(progressBar)){
                               progressBar$inc(amount = 1/noProgressSteps, 
                                               message = sprintf("Importing table %s of %d from database.", 
                                                                 j, noScenTables))
                               j <<- j + 1L
                             }
                             dataDbTmp <- private$db$importDataset(tableName, 
                                                                   subsetSids = scenIds)
                             return(split(dataDbTmp[-1], dataDbTmp[[1L]]))
                           })
                           names(dataTmp) <- scenTableNames
                           sameNameCounter   <- list()
                           
                           
                           j <- 1L
                           
                           scenNameList <- vector("character", length(scenIds))
                           
                           for(scenId in as.character(scenIds)){
                             if(!is.null(progressBar)){
                               progressBar$inc(amount = 1/noProgressSteps, 
                                               message = sprintf("Writing dataset %d of %d.", 
                                                                 j, noScenIds))
                             }
                             scenName <- scenIdNameMap[[scenId]]
                             if(is.null(sameNameCounter[[scenName]])){
                               sameNameCounter[[scenName]] <- 1L
                             }else{
                               scenName <- paste0(scenName, "_", sameNameCounter[[scenName]])
                               sameNameCounter[[scenName]] <- sameNameCounter[[scenName]] + 1L
                             }
                             scenNameList[[j]] <- paste0(scenName, ".gdx")
                             j <- j + 1L
                             gdxio$wgdx(paste0(tmpDir, .Platform$file.sep, sanitizeFn(scenName), ".gdx"), 
                                        lapply(dataTmp, function(data){
                                          if(scenId %in% names(data)){
                                            return(data[[scenId]])
                                          }
                                          return(tibble())
                                        }))
                           }
                           if(genScenList){
                             write_lines(scenNameList, file.path(tmpDir, "hcube_file_names.txt"))
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
                           sameNameCounter   <- list()
                           for(tabId in seq_along(scenTableNames)){
                             if(identical(tabId, 1L)){
                               tableName <- "_metadata_"
                             }else if(identical(tabId, length(scenTableNames))){
                               tableName <- "_trace_"
                             }else{
                               tableName <- scenTableNames[[tabId]]
                               if(startsWith(tableName, "_")){
                                 tableName <- substring(tableName, 2)
                               }
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
                             tableTmp <- private$db$importDataset(scenTableNames[[tabId]], 
                                                                  subsetSids = scenIds)
                             
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
                                 
                                 sanitizedScenName <- sanitizeFn(tableTmp[[i]][["_sname"]][[1L]])
                                 dirNameScen <- file.path(tmpDir, sanitizedScenName)
                                 
                                 if(!is.null(sameNameCounter[[sanitizedScenName]])){
                                   dirNameScen <- paste0(dirNameScen, "_", sameNameCounter[[sanitizedScenName]])
                                   sameNameCounter[[sanitizedScenName]] <<- sameNameCounter[[sanitizedScenName]] + 1L
                                 }else{
                                   sameNameCounter[[sanitizedScenName]] <<- 1L
                                 }
                                 scenIdDirNameMap[[scenId]] <<- dirNameScen
                                 if(!dir.create(dirNameScen)){
                                   stop(sprintf("Temporary folder: '%s' could not be created.", 
                                                dirNameScen), call. = FALSE)
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
                                                "._scode != ", as.integer(SCODEMAP[['hcube_jobconfig']]),
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
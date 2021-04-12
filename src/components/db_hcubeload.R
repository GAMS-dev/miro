HcubeLoad <- R6Class("HcubeLoad", 
                     public = list(
                       initialize        = function(db, scalarColNames, modelName, inputDsNamesNotToDisplay, 
                                                    scalarTables = NULL, scalarKeyTypeList = NULL, 
                                                    tableFieldSep = ".", hcubeScalars = character(0L)){
                         # R6 class to import scenarios in hcube mode
                         #
                         # Args:      
                         #   db:                      R6 database object
                         #   scalarColNames:          column names of scalar tables 
                         #                            for fields and values
                         #   modelName:               name of the currently running model
                         #   inputDsNamesNotToDisplay: input datasets that are not displayed in UI
                         #   scalarKeyTypeList:       list with keys and types for each scalar table
                         #   scalarTables:            table names of scalar tables where 
                         #                            field/value paris should be fetched from (optional)
                         #   tableFieldSep:           seperator between table and field name
                         #   hcubeScalars:            name of scalars that are transferred to 
                         #                            tables in Hypercube mode
                         #
                         
                         # BEGIN error checks
                         stopifnot(is.R6(db), is.character(modelName), length(modelName) == 1L)
                         #stopifnot(is.character(scalarColNames), length(scalarColNames) == 2L)
                         if(!is.null(scalarTables)){
                           stopifnot(is.character(scalarTables), length(scalarTables) >= 1L)
                           stopifnot(is.list(scalarKeyTypeList), length(scalarKeyTypeList) >= 1L)
                         }
                         stopifnot(is.character(tableFieldSep), length(tableFieldSep) == 1L)
                         # END error checks
                         
                         private$db                 <- db
                         private$conn               <- db$getConn()
                         private$sidCol             <- dbQuoteIdentifier(private$conn, 
                                                                         "_sid")
                         private$modelName          <- modelName
                         private$scalarColNames     <- scalarColNames
                         private$tabNameMeta        <- dbSchema$getDbTableName("_scenMeta")
                         private$scalarTables       <- scalarTables
                         private$keyTypeList        <- scalarKeyTypeList
                         private$tableFieldSep      <- tableFieldSep
                         private$inputDsNamesNotToDisplay <- inputDsNamesNotToDisplay
                         private$hcubeScalarTables  <- hcubeScalars
                         
                         if(inherits(private$conn, "PqConnection")){
                           dbExecute(private$conn, "CREATE EXTENSION IF NOT EXISTS tablefunc")
                         }
                       },
                       fetchValues = function(field, table = NULL){
                         stopifnot(is.character(field), length(field) == 1L)
                         taleField <- strsplit(field, private$tableFieldSep, 
                                               fixed = TRUE)[[1]]
                         table <- taleField[[1]]
                         field <- taleField[[2]]
                         if(is.null(table)){
                           table <- private$getTableName(fieldTableString = field)
                         }
                         stopifnot(is.character(table), length(table) == 1L)
                         if(is.null(private$values[[table]][[field]])){
                           if(table %in% private$scalarTables){
                             field <- substring(field, 2)
                             tableTmp <- private$importScalarValues(table, field)
                           }else{
                             tableTmp <- private$db$importDataset(table, 
                                                                  colNames = field, 
                                                                  distinct = TRUE)
                           }
                           if(length(tableTmp)){
                             if(is.null(private$values[[table]])){
                               private$values[[table]] <- list()
                             }
                             private$values[[table]][[field]] <- tableTmp[[1]]
                           }
                           
                         }
                         return(private$values[[table]][[field]])
                       },
                       fetchResults = function(subsetList, colNames, limit = 1e6){
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
                         innerTables <- private$scalarTables
                         
                         if(any(!vapply(c(private$tabNameMeta, innerTables), function(table){ 
                           dbExistsTable(private$conn, table)}, logical(1L), USE.NAMES = FALSE))){
                           
                           flog.info("Db: One of the tables: '%s' does not exist (HcubeLoad.fetchResults).", 
                                      paste(c(private$tabNameMeta, innerTables), collapse = ", "))
                           return(tibble())
                         }
                         if(inherits(private$conn, "PqConnection")){
                           data <- private$fetchResultsPG(subsetList = subsetList, 
                                                          colNames = colNames, limit = limit)
                         }else{
                           data <- private$fetchResultsR(subsetList = subsetList, 
                                                         colNames = colNames, limit = limit)
                         }
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
                           gdxio$wgdx(paste0(tmpDir, .Platform$file.sep, scenName, ".gdx"), 
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
                             scenId   <- tableTmp[[i]][[1L]][[1L]]
                             
                             if(identical(tabId, 1L)){
                               
                               scenName <- tableTmp[[i]][["_sname"]][[1L]]
                               
                               dirNameScen <- file.path(tmpDir, scenName)
                               
                               if(!is.null(sameNameCounter[[scenName]])){
                                 dirNameScen <- paste0(dirNameScen, "_", sameNameCounter[[scenName]])
                                 sameNameCounter[[scenName]] <<- sameNameCounter[[scenName]] + 1L
                               }else{
                                 sameNameCounter[[scenName]] <<- 1L
                               }
                               scenIdDirNameMap[[scenId]] <<- dirNameScen
                               if(!dir.create(dirNameScen)){
                                 stop(sprintf("Temporary folder: '%s' could not be created.", 
                                              dirNameScen), call. = FALSE)
                               }
                             }
                             write_csv(tableTmp[[i]][-1L], 
                                       paste0(scenIdDirNameMap[[scenId]], 
                                              .Platform$file.sep, tableName, ".csv"))
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
                       modelName               = character(1L),
                       scalarColNames          = character(0L),
                       sidCol                  = character(0L),
                       tabNameMeta             = character(0L),
                       scalarTables            = character(0L),
                       inputDsNamesNotToDisplay = character(0L),
                       tableFieldSep           = character(0L),
                       hcubeScalarTables       = character(0L),
                       keyTypeList             = NULL,
                       values                  = list(),
                       groupedData             = list(),
                       groupLabels             = list(),
                       groupedNames            = list(),
                       groupedSids             = list(),
                       fetchResultsPG          = function(subsetList, colNames, limit){
                         innerTables <- private$scalarTables
                         
                         colNamesSQL <- paste0(DBI::dbQuoteIdentifier(private$conn, names(colNames)),
                                               ".", DBI::dbQuoteIdentifier(private$conn, colNames),
                                               collapse = ",")
                         innerPivotTables <- vapply(innerTables, function(innerTable){
                           private$genPivotQuery(innerTable, private$scalarColNames[[1]], 
                                                 private$scalarColNames[[2]], 
                                                 private$keyTypeList[[innerTable]])
                         }, character(1L), USE.NAMES = FALSE)
                         innerJoin <- NULL
                         if(length(innerPivotTables)){
                           innerJoin <- paste0(" FULL JOIN (",
                                               innerPivotTables,
                                               ") AS ",
                                               dbQuoteIdentifier(private$conn, innerTables),
                                               " ON ",
                                               dbQuoteIdentifier(private$conn,
                                                                 dbSchema$getDbTableName("_scenMeta")), 
                                               "._sid=", dbQuoteIdentifier(private$conn, innerTables),
                                               "._sid")
                         }
                         
                         # fetch dataframe
                         tryCatch({
                           subsetRows <- ""
                           subsetSids <- private$db$fetchScenList(scode = SCODEMAP[['scen']],
                                                                  gt = TRUE)[[1L]]
                           subsetSidSQL <- NULL
                           if(!length(subsetSids)){
                             return(tibble())
                           }
                           subsetSidSQL <- paste0(" INNER JOIN (VALUES ",
                                                  paste("(" %+% subsetSids, 
                                                        collapse = "), "), ")) vals(_v) ON ",
                                                  dbQuoteIdentifier(private$conn, 
                                                                    private$tabNameMeta),
                                                  ".", private$sidCol, "=_v")
                           if(length(subsetList) > 1L || length(subsetList[[1L]])){
                             subsetRows <- private$db$buildRowSubsetSubquery(subsetList, " AND ", 
                                                                             " OR ")
                           }
                           sql     <- SQL(paste0("SELECT ", colNamesSQL, " FROM ", 
                                                 dbQuoteIdentifier(private$conn, private$tabNameMeta),
                                                 paste(innerJoin, collapse = " "), subsetSidSQL,
                                                 if(nchar(trimws(subsetRows))) " WHERE ", 
                                                 subsetRows, " LIMIT ?lim ;"))
                           flog.debug("Db: Data was imported (HcubeLoad.fetchResults).")
                           query   <- DBI::sqlInterpolate(private$conn, sql, lim = limit + 1L)
                           dataset <- as_tibble(DBI::dbGetQuery(private$conn, query))
                         }, error = function(e){
                           stop(sprintf("Db: An error occurred while querying the database (HcubeLoad.fetchResults)." %+%
                                          "Error message: '%s'.", conditionMessage(e)),
                                call. = FALSE)
                         })
                         if(nrow(dataset) > limit){
                           stop("maxNoRowsVio", call. = FALSE)
                         }
                         colNamesNew  <- !(colNames %in% names(dataset))
                         dataset[, colNames[colNamesNew]] <- NA
                         return(dataset[, colNames])
                       },
                       fetchResultsR           = function(subsetList, colNames, limit){
                         innerTables <- private$scalarTables
                         
                         metaData    <- private$db$fetchScenList(scode = SCODEMAP[['scen']],
                                                                 gt = TRUE)
                         if(!length(metaData) || !nrow(metaData)){
                           return(tibble())
                         }
                         names(metaData)[-1] <- paste0(private$tabNameMeta, ".", 
                                                       names(metaData)[-1])
                         sidsToFetch <- metaData[[1]]
                         
                         if(length(sidsToFetch) > limit){
                           stop("maxNoRowsVio", call. = FALSE)
                         }
                         sidColName <- names(metaData)[1]
                         tabData <- lapply(innerTables, function(innerTable){
                           data <- private$db$importDataset(innerTable, 
                                                            colNames = c(sidColName,
                                                                         private$scalarColNames),
                                                            limit = limit + 1L, 
                                                            subsetSids = sidsToFetch)
                           if(length(data) && nrow(data) > limit){
                             stop("maxNoRowsVio", call. = FALSE)
                           }
                           data <- spread(data, 2, 3, drop = FALSE, convert = TRUE)
                           names(data)[-1] <- paste0(innerTable, "._", names(data)[-1])
                           return(data)
                         })
                         dataset <- purrr::reduce(c(list(metaData), tabData), 
                                                  full_join, by = sidColName)
                         names(dataset)[1] <- paste0(private$tabNameMeta, ".", 
                                                     names(metaData)[1])
                         colNames     <- paste0(names(colNames), ".", colNames)
                         colNamesNew  <- !colNames %in% names(dataset)
                         colNamesSym  <- rlang::syms(colNames[!colNamesNew])
                         dataset      <- select(dataset, !!!colNamesSym)
                         missingVals  <- numeric(0L)
                         if(length(dataset) && nrow(dataset)){
                           missingVals <- NA_real_
                         }
                         dataset[, colNames[colNamesNew]] <- missingVals
                         if(length(subsetList) > 1L || length(subsetList[[1L]])){
                           subsetRows <- private$db$buildRowSubsetSubquery(subsetList, " & ", 
                                                                           " | ", SQL = FALSE)
                           if(nchar(trimws(subsetRows))){
                             subsetRows <- rlang::parse_expr(subsetRows)
                             dataset    <- filter(dataset, !!subsetRows)
                           }
                         }
                         return(dataset[, colNames])
                       },
                       importScalarValues      = function(table, field){
                         query <- SQL(paste0("SELECT DISTINCT ", 
                                             dbQuoteIdentifier(private$conn, private$scalarColNames[[2]]), "
                                             FROM ", dbQuoteIdentifier(private$conn, table), " WHERE ",
                                             dbQuoteIdentifier(private$conn, private$scalarColNames[[1]]), "
                                             = ", dbQuoteString(private$conn, field)))
                         as_tibble(dbGetQuery(private$conn, query))
                       },
                       genPivotQuery          = function(tableName, keyCol, valCol, keyTypeList){
                         keyCols <- vapply(keyTypeList, "[[", character(1L), "key", USE.NAMES = FALSE)
                         SQL(paste0("SELECT * FROM crosstab ('SELECT ", 
                                    dbQuoteIdentifier(private$conn, "_sid"),  ", ", 
                                    dbQuoteIdentifier(private$conn, keyCol), 
                                    ", ", dbQuoteIdentifier(private$conn, valCol), " FROM ", 
                                    dbQuoteIdentifier(private$conn, dbSchema$getDbTableName(tableName)), 
                                    " WHERE ", dbQuoteIdentifier(private$conn, keyCol), " IN ('", 
                                    paste(dbQuoteString(private$conn, keyCols), collapse = "', '"), 
                                    "') ORDER BY 1,2',$$ VALUES (", 
                                    paste(dbQuoteString(private$conn, keyCols), collapse = "), ("), ")$$) AS ", 
                                    dbQuoteIdentifier(private$conn,
                                                      paste0(tableName, "_tmp")),
                                    " (", 
                                    dbQuoteIdentifier(private$conn, "_sid"), " int, ", 
                                    private$genKeyTypeString(keyTypeList), ")"))
                       },
                       genKeyTypeString       = function(keyTypeList){
                         keyTypeList <- vapply(keyTypeList, function(keyTypeEl){
                           dbQuoteIdentifier(private$conn, "_" %+% keyTypeEl$key) %+%
                             if(keyTypeEl$type %in% c("string", "set")){
                               " varchar"
                             }else if(keyTypeEl$type %in% c("numeric", "number", "parameter")){
                               " numeric"
                             }else{
                               stop(sprintf("Invalid type: '%s'. Allowed types are: 'string, number'.",
                                            keyTypeEl$type), call. = FALSE)
                             }
                         }, character(1L), USE.NAMES = FALSE)
                         paste(keyTypeList, collapse = ", ")
                       },
                       getTableNamesScenario = function(){
                        scenTableNames <- dbSchema$getAllSymbols()
                        return(scenTableNames[!scenTableNames %in% private$hcubeScalarTables])
                       }
                     )
)
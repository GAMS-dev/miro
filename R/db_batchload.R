BatchLoad <- R6Class("BatchLoad", 
                     public = list(
                       initialize        = function(db, scalarColNames, scalarTables = NULL, 
                                                    scalarKeyTypeList = NULL, tableFieldSep = "-"){
                         # R6 class to import scenarios in batch mode
                         #
                         # Args:      
                         #   db:                      R6 database object
                         #   scalarColNames:          column names of scalar tables 
                         #                            for fields and values
                         #   scalarKeyTypeList:       list with keys and types for each scalar table
                         #   scalarTables:            table names of scalar tables where 
                         #                            field/value paris should be fetched from (optional)
                         #   tableFieldSep:           seperator between table and field name
                         #
                         
                         # BEGIN error checks
                         stopifnot(is.R6(db))
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
                                                                         db$getScenMetaColnames()['sid'])
                         private$scalarColNames     <- scalarColNames
                         private$tabNameMeta        <- db$getTableNameMetadata()
                         private$scalarTables       <- scalarTables
                         private$keyTypeList        <- scalarKeyTypeList
                         private$tableFieldSep      <- tableFieldSep
                         private$tableNameTrace     <- db$getTraceConfig()[["tabName"]]
                         private$traceColNames      <- db$getTraceConfig()[["colNames"]]
                         
                         dbExecute(private$conn, "CREATE EXTENSION IF NOT EXISTS tablefunc")
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
                         #   colNames:        named character vector with column names to select
                         #                    and names being the table to select from
                         #   limit            maximum number of rows to fetch
                         #
                         # Returns:
                         #   tibble with results from query
                         
                         # BEGIN error checks
                         stopifnot(is.list(subsetList))
                         if(!length(subsetList)){
                           return(tibble())
                         }
                         stopifnot(is.character(colNames), length(colNames) >= 1L)
                         stopifnot(is.character(names(colNames)))
                         stopifnot(!is.na(as.integer(limit)), length(limit) == 1L)
                         # END error checks
                         innerTables <- private$scalarTables
                         
                         colNames              <- colNames
                         if(any(!vapply(c(private$tabNameMeta, innerTables), function(table){ 
                           dbExistsTable(private$conn, table)}, logical(1L), USE.NAMES = FALSE))){
                           
                           flog.error("Db: One of the tables: '%s' does not exist (BatchLoad.fetchResults).", 
                                      paste(c(private$tabNameMeta, innerTables), collapse = ", "))
                           return(tibble())
                         }
                         colNames <- paste0(DBI::dbQuoteIdentifier(private$conn, names(colNames)),
                                            ".", DBI::dbQuoteIdentifier(private$conn, colNames),
                                            collapse = ",")
                         innerPivotTables <- vapply(innerTables, function(innerTable){
                           private$genPivotQuery(innerTable, private$scalarColNames[[1]], 
                                                 private$scalarColNames[[2]], 
                                                 private$keyTypeList[[innerTable]])
                         }, character(1L), USE.NAMES = FALSE)
                         innerJoin <- paste0(" INNER JOIN (", innerPivotTables,
                                             ") AS ", dbQuoteIdentifier(private$conn, innerTables),
                                             " ON ", dbQuoteIdentifier(private$conn, private$tabNameMeta), 
                                             ".", private$sidCol, "=", dbQuoteIdentifier(private$conn, innerTables),
                                             ".", private$sidCol)
                         # fetch dataframe
                         tryCatch({
                           subsetRows <- character(0L)
                           if(length(subsetList)){
                             subsetRows <- private$db$buildRowSubsetSubquery(subsetList, " AND ", 
                                                                             " OR ")
                           }
                           sql     <- SQL(paste0("SELECT ", colNames, " FROM ", 
                                                 DBI::dbQuoteIdentifier(private$conn, private$tabNameMeta),
                                                 paste(innerJoin, collapse = " "), if(length(subsetList)) " WHERE ", 
                                                 subsetRows, " LIMIT ?lim ;"))
                           flog.debug("Db: Data was imported (BatchLoad.fetchResults).")
                           query   <- DBI::sqlInterpolate(private$conn, sql, lim = limit)
                           dataset <- as_tibble(DBI::dbGetQuery(private$conn, query))
                         }, error = function(e){
                           stop(sprintf("Db: An error occurred while querying the database (BatchLoad.fetchResults)." %+%
                                          "Error message: '%s'.", e),
                                call. = FALSE)
                         })
                         
                         return(dataset)
                       },
                       exceedsMaxNoSolvers = function(data, attribs, maxNoGroups, exclAttrib = NULL){
                         stopifnot(inherits(data, "data.frame"))
                         stopifnot(is.character("attribs"), length(attribs) > 0L)
                         
                         attribs <- gsub("^.+-", "", attribs)
                         sids <- as.integer(data[[1]])
                         colIdsAttrib <- match(attribs, names(data))
                         colIdsExclAttrib <- match(exclAttrib, names(data))
                         if(any(is.na(colIdsExclAttrib))){
                           stop(sprintf("Attribute: '%s' was not found.", colIdsExclAttrib), 
                                call. = FALSE)
                         }
                           
                         attribs <- rlang::syms(attribs)
                         groupedData <- dplyr::group_by(data, !!!attribs)
                         
                         groupedRowIds       <- attr(groupedData, "indices")
                         private$groupedSids <- lapply(groupedRowIds, function(rowIds){
                           sids[rowIds + 1L]
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
                         
                         private$groupLabels <- attr(groupedData, "labels")
                         return(FALSE)
                       },
                       genPaverTraceFiles = function(workDir, exclTraceCols = NULL) {
                         stopifnot(length(private$groupedSids) > 0L)
                         stopifnot(length(private$groupLabels) > 0L)
                         stopifnot(length(private$tableNameTrace) > 0L)
                         stopifnot(is.character(workDir), length(workDir) == 1L)
                         
                         groupLabels   <- as.data.frame(lapply(seq_along(private$groupLabels), function(i) 
                           #paste0(names(groupLabels)[i], ":", as.character(groupLabels[[i]]))),
                           as.character(private$groupLabels[[i]])),
                           stringsAsFactors = FALSE)
                         groupLabels   <- vapply(seq_len(nrow(groupLabels)), function(i){
                           paste0(as.vector(groupLabels[i, ], "character"), collapse = "\\")
                         }, character(1L), USE.NAMES = FALSE)
                         lapply(seq_along(private$groupedSids), function(i){
                           fileName <- workDir %+% i %+% ".trc"
                           paverFile <- file(fileName, open = 'wt')
                           writeLines(paste0("* Trace Record Definition\n* GamsSolve\n",
                                             "* ", paste(setdiff(private$traceColNames, exclTraceCols), collapse = ","),
                                             "\n*\n* SOLVER,\n* TIMELIMIT,3600\n* NODELIMIT,2100000000\n* GAPLIMIT,0"), con = paverFile)
                           close(paverFile)
                           paverData      <- private$db$importDataset(private$tableNameTrace, 
                                                                      tibble(private$sidCol, private$groupedSids[[i]]), 
                                                                      innerSepAND = FALSE)[-1]
                           paverData[[1]] <- private$groupedNames[[i]]
                           paverData[[3]] <- rep.int(groupLabels[i], nrow(paverData))
                           if(length(exclTraceCols)){
                             paverData[, exclTraceCols] <- NULL
                           }
                           
                           write_csv(paverData, fileName, append = TRUE)
                           
                         })
                         
                       },
                       finalize = function(){
                         NULL
                       }
                     ),
                     private = list(
                       db                      = NULL,
                       conn                    = NULL,
                       scalarColNames          = character(0L),
                       sidCol                  = character(0L),
                       tabNameMeta             = character(0L),
                       scalarTables            = character(0L),
                       tableNameTrace          = character(0L),
                       traceColNames           = character(0L),
                       tableFieldSep           = character(0L),
                       keyTypeList             = NULL,
                       values                  = list(),
                       groupedData             = list(),
                       groupLabels             = list(),
                       groupedNames            = list(),
                       groupedSids             = list(),
                       filterScalars           = function(scalarTable, subsetScalars){
                         filterCondition <- private$db$buildRowSubsetSubquery(subsetScalars, " & ", 
                                                                              " | ", SQL = FALSE)
                         dplyr::filter_(scalarTable, filterCondition)
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
                                    dbQuoteIdentifier(private$conn, private$sidCol),  ", ", 
                                    dbQuoteIdentifier(private$conn, keyCol), 
                                    ", ", dbQuoteIdentifier(private$conn, valCol), " FROM ", 
                                    dbQuoteIdentifier(private$conn, tableName), 
                                    " WHERE ", dbQuoteIdentifier(private$conn, keyCol), " IN ('", 
                                    paste(dbQuoteString(private$conn, keyCols), collapse = "', '"), 
                                    "') ORDER BY 1,2',$$ VALUES (", 
                                    paste(dbQuoteString(private$conn, keyCols), collapse = "), ("), ")$$) AS ", 
                                    dbQuoteIdentifier(private$conn, tableName %+% "_tmp"), " (", 
                                    dbQuoteIdentifier(private$conn, private$sidCol), " int, ", 
                                    private$genKeyTypeString(keyTypeList), ")"))
                       },
                       genKeyTypeString       = function(keyTypeList){
                         keyTypeList <- vapply(keyTypeList, function(keyTypeEl){
                           dbQuoteIdentifier(private$conn, "_" %+% keyTypeEl$key) %+%
                             if(keyTypeEl$type %in% c("set", "string", "acronym")){
                               " varchar"
                             }else if(keyTypeEl$type %in% c("scalar", "parameter", "number")){
                               " numeric"
                             }else{
                               stop("Invalid type: ''. Allowed types are: 'string, number'.")
                             }
                         }, character(1L), USE.NAMES = FALSE)
                         paste(keyTypeList, collapse = ", ")
                       }
                     )
)
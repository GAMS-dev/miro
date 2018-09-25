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
                             table.tmp <- private$importScalarValues(table, field)
                           }else{
                             table.tmp <- private$db$importDataset(table, 
                                                                   colNames = field, 
                                                                   distinct = TRUE)
                           }
                           if(length(table.tmp)){
                             if(is.null(private$values[[table]])){
                               private$values[[table]] <- list()
                             }
                             private$values[[table]][[field]] <- table.tmp[[1]]
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
                       getNoSolvers = function(data, attribs, maxNoGroups){
                         stopifnot(inherits(data, "data.frame"))
                         stopifnot(is.character("attribs"), length(attribs) > 0L)

                         attribs <- gsub("^.+-", "", attribs)
                         sids <- data[[1]]
                         data <- data[, attribs]
                         attribs <- rlang::syms(attribs)
                         private$groupedData <- dplyr::group_by(data, !!!attribs)
                         groupedRowIds       <- attr(private$groupedData, "indices")
                         private$groupedSids <- lapply(groupedRowIds, function(rowIds){
                           as.integer(sids[rowIds + 1L])
                         })
                         return(length(groupedRowIds))
                       },
                       genPaverTraceFiles = function(workDir){
                         stopifnot(length(private$groupedSids) > 0L)
                         stopifnot(length(private$groupedData) > 0L)
                         stopifnot(length(private$tableNameTrace) > 0L)
                         stopifnot(is.character(workDir), length(workDir) == 1L)
                         
                         groupLabels   <- attr(private$groupedData, "labels")
                         groupLabels   <- as.data.frame(lapply(seq_along(groupLabels), function(i) 
                           paste0(names(groupLabels)[i], ": ", groupLabels[[i]])))
                         groupLabels   <- vapply(seq_len(nrow(groupLabels)), function(i){
                           paste0(groupLabels[i, ], collapse = ", ")
                         }, character(1L), USE.NAMES = FALSE)
                         lapply(seq_along(private$groupedSids), function(i){
                           fileName <- workDir %+% i %+% ".trc"
                           paverFile <- file(fileName, open = 'wt')
                           on.exit(close(paverFile))
                           
                           writeLines(paste0("* Trace Record Definition\n* GamsSolve\n",
"* InputFileName,ModelType,SolverName,NLP,MIP,JulianDate,Direction,NumberOfEquations,NumberOfVariables,",
"NumberOfDiscreteVariables,NumberOfNonZeros,NumberOfNonlinearNonZeros,OptionFile,ModelStatus,SolverStatus,",
"ObjectiveValue\n* ,ObjectiveValueEstimate,SolverTime,NumberOfIterations,NumberOfDomainViolations,NumberOfNodes,#User1\n",
"*\n* SOLVER,", groupLabels[i], "\n* TIMELIMIT,3600\n* NODELIMIT,2100000000\n* GAPLIMIT,0\n"), con = paverFile)
                           
                           paverData <- private$db$importDataset(private$tableNameTrace, 
                                                            tibble(private$sidCol, private$groupedSids[[i]]), 
                                                            innerSepAND = FALSE)[-1]
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
                       tableFieldSep           = character(0L),
                       keyTypeList             = NULL,
                       values                  = list(),
                       groupedData             = list(),
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
                         SQL(paste0("SELECT * FROM crosstab ('SELECT ", 
                                    dbQuoteIdentifier(private$conn, private$sidCol),  ", ", dbQuoteIdentifier(private$conn, keyCol), 
                                    ", max(", dbQuoteIdentifier(private$conn, valCol), ") FROM ", dbQuoteIdentifier(private$conn, tableName), 
                                    " GROUP BY 1,2 ORDER BY 1,2','SELECT DISTINCT ", dbQuoteIdentifier(private$conn, keyCol), " FROM ", 
                                    dbQuoteIdentifier(private$conn, tableName), " ORDER BY 1') AS ", 
                                    dbQuoteIdentifier(private$conn, tableName %+% "_tmp"), " (", 
                                    dbQuoteIdentifier(private$conn, private$sidCol), " bigint, ", private$genKeyTypeString(keyTypeList), ")"))
                       },
                       genKeyTypeString       = function(keyTypeList){
                         keyTypeList <- vapply(keyTypeList, function(keyTypeEl){
                           dbQuoteIdentifier(private$conn, "_" %+% keyTypeEl$key) %+%
                             if(identical(keyTypeEl$type, "string")){
                               " varchar"
                             }else if(identical(keyTypeEl$type, "number")){
                               " numeric"
                             }else{
                               stop("Invalid type: ''. Allowed types are: 'string, number'.")
                             }
                         }, character(1L), USE.NAMES = FALSE)
                         paste(keyTypeList, collapse = ", ")
                       }
                     )
)
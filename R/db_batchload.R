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
                       #fetchFieldValueList = function(metaColNames){
                       #  # Fetches data frame with fields and values from database
                       #  #
                       #  # Args:
                       #  #   metaColNames:            names of columns from metadata table 
                       #  #                            that should be fetched 
                       #  #
                       #  # Returns:
                       #  #   names list where names are field each holding a vector with 
                       #  #   values found in db
                       #  
                       #  # BEGIN error checks
                       #  stopifnot(is.character(metaColNames), length(metaColNames) >= 1L)
                       #  # END error checks
                       #  
                       #  fieldValueList <- NULL
                       #  scenMeta <- private$db$fetchScenList()
                       #  if(!nrow(scenMeta)){
                       #    return(NULL)
                       #  }
                       #  fieldValueList <- lapply(metaColNames, function(field){
                       #    unique(scenMeta[[field]])
                       #  })
                       #  names(fieldValueList) <- private$tabNameMeta %+% 
                       #    private$tableFieldSep %+% metaColNames
                       #  
                       #  sids <- scenMeta[[1]]
                       #  
                       #  for(table in private$scalarTables){
                       #    fieldValueListNew <- private$fetchScalarValues(table, sids)
                       #    fieldValueList    <- c(fieldValueList, fieldValueListNew)
                       #  }
                       #  return(fieldValueList)
                       #},
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
                           print(query)
                           dataset <- as_tibble(DBI::dbGetQuery(private$conn, query))
                           # scalar tables are in a bad database format (values in rows instead of columns).
                           # this is due to the fact that it provides more flexibility when GAMS model changes
                           # (schema can stay the same as new GAMS scalar is merely a new row in table)
                           # However, in order to filter table accordingly, scalar table needs to be transformed
                           #if(!is.null(subsetScalars)){
                           #  scalarTable <- bind_rows(lapply(seq_along(private$scalarTables), function(tableId){
                           #    colId <- colIdScalar + 2 * (tableId - 1)
                           #    table <- spread(dataset[, c(1, colId, colId + 1)], 2, 3)
                           #    names(table)[-1] <- private$scalarTables[tableId] %+% "-" %+% names(table)[-1]
                           #    table
                           #  }))
                           #  filteredIdsScalar <- private$filterScalars(scalarTable, subsetScalars)[[1]]
                           #}
                         }, error = function(e){
                           stop(sprintf("Db: An error occurred while querying the database (BatchLoad.fetchResults)." %+%
                                          "Error message: '%s'.", e),
                                call. = FALSE)
                         })
                         
                         return(dataset)
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
                       tableFieldSep           = character(0L),
                       keyTypeList             = NULL,
                       values                  = list(),
                       #fetchScalarValues       = function(tableName, sids){
                       #  data <- private$db$importDataset(tableName, 
                       #                                   colNames = private$scalarColNames, 
                       #                                   distinct = TRUE,
                       #                                   subsetSids = sids)
                       #  fields <- unique(data[[1]])
                       #  fieldValueList <- lapply(fields, function(field){
                       #    data[data[[1]] == field, ][[2]]
                       #  })
                       #  names(fieldValueList) <- tableName %+% private$tableFieldSep %+% fields
                       #  return(fieldValueList)
                       #},
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
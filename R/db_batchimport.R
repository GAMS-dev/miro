BatchImport <- R6Class("BatchImport", 
                       inherit = Db,
                       public = list(
                         initialize        = function(db, scalarsInputName, scalarsOutputName, 
                                                      tableNamesCanHave, tableNamesMustHave,
                                                      csvDelim, workDir){
                           # R6 class to import scenarios in batch mode
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
                           #   traceColNames:           column names of trace file
                           #
                           
                           # BEGIN error checks
                           stopifnot(is.R6(db))
                           stopifnot(is.character(scalarsInputName), length(scalarsInputName) == 1)
                           stopifnot(is.character(scalarsOutputName), length(scalarsOutputName) == 1)
                           stopifnot(is.character(tableNamesCanHave), length(tableNamesCanHave) >= 1)
                           stopifnot(is.character(tableNamesMustHave), length(tableNamesMustHave) >= 1)
                           stopifnot(is.character(csvDelim), length(csvDelim) == 1)
                           stopifnot(is.character(workDir), length(workDir) == 1)
                           stopifnot(is.character(traceColNames), length(traceColNames) >= 1)
                           # END error checks
                           
                           private$conn               <- db$getConn()
                           private$uid                <- db$getUid()
                           private$tableNamesScenario <- db$getTableNamesScenario()
                           private$tableNameMetadata  <- db$getTableNameMetadata()
                           private$scenMetaColnames   <- db$getScenMetaColnames()
                           traceConfig                <- db$getTraceConfig()
                           private$traceTabName       <- traceConfig[['tabName']]
                           private$traceColNames      <- traceConfig[['colNames']]
                           private$scalarsInputName   <- tolower(scalarsInputName)
                           private$scalarsOutputName  <- tolower(scalarsOutputName)
                           private$tableNamesMustHave <- tableNamesMustHave
                           private$tableNamesToVerify <- c(tableNamesCanHave, tableNamesMustHave)
                           private$csvDelim           <- csvDelim
                           private$workDir            <- workDir
                           private$includeTrc         <- traceConfig[['tabName']] %in% private$tableNamesToVerify
                         },
                         getScenNames      = function() private$scenNames,
                         getInvalidScenIds = function() private$invalidScenIds,
                         unzipScenData     = function(zipFilePath, extractDir){
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
                           
                           csvPaths             <- private$getCsvPaths(zipFilePath)
                           private$scenNames    <- private$fetchScenNames(csvPaths)
                           # workaround for unzip function as path with trailing slashes is not found
                           filePaths <- unzip(zipFilePath, 
                                              exdir = gsub("/?$", "", private$workDir))
                           validCsvFiles  <- grepl("\\.(csv|trc)$", filePaths, ignore.case = TRUE)
                           file.remove(filePaths[!validCsvFiles])
                           csvPaths <- filePaths[validCsvFiles]
                           
                           if(any(Sys.readlink(csvPaths) != "")){
                             stop("zip archive contains symlinks.", call. = FALSE)
                           }
                           
                           private$csvPaths <- csvPaths
                           private$csvPaths <- lapply(private$scenNames, 
                                                      private$getScenFilePaths, csvPaths)
                           names(private$csvPaths) <- private$scenNames
                           invisible(self)
                         },
                         validateScenFiles = function(){
                           # validates scenario data
                           #
                           # Args:
                           #
                           # Returns:
                           #   reference to itself (importBatch R6 object)
                           
                           
                           csvNames       <- lapply(private$csvPaths, 
                                                    private$verifyScenFiles)
                           invalidScen    <- vapply(csvNames, is.null, logical(1L), USE.NAMES = FALSE)
                           
                           private$invalidScenIds <- private$scenNames[invalidScen]
                           private$csvPaths[private$invalidScenIds] <- NULL
                           
                           invisible(self)
                         },
                         readAllScenData   = function(){
                           # Read scenario data and return them as a list of dataframes
                           #
                           # Args:
                           # 
                           # Returns:
                           #    R6 object (reference to itself)
                           private$scenData <- lapply(private$csvPaths, private$readScenData)
                           invisible(self)
                         },
                         validateScenTables = function(scalarInToVerify = NULL, scalarOutToVerify = NULL){
                           # validates scenario tables
                           #
                           # Args:
                           #   scalarInToVerify:        scalar input elements that must exist 
                           #                            in order for scenario to be valid (optional)
                           #   scalarOutToVerify:       scalar output elements that must exist 
                           #                            in order for scenario to be valid (optional)
                           #
                           # Returns:
                           #   reference to itself (importBatch R6 object)
                           
                           # BEGIN error checks
                           if(!is.null(scalarInToVerify)){
                             stopifnot(is.character(scalarInToVerify), length(scalarInToVerify) >= 1)
                           }
                           if(!is.null(scalarOutToVerify)){
                             stopifnot(is.character(scalarOutToVerify), length(scalarOutToVerify) >= 1)
                           }
                           # END error checks
                           isValidScen    <- vapply(private$scenData, 
                                                    private$validateTables,
                                                    logical(1L),
                                                    scalarInToVerify, 
                                                    scalarOutToVerify,
                                                    USE.NAMES = FALSE)
                           invalidScenTables <- names(private$scenData)[!isValidScen]
                           private$scenData[invalidScenTables] <- NULL
                           private$invalidScenIds <- c(private$invalidScenIds, 
                                                       invalidScenTables)
                           
                           invisible(self)
                         },
                         removeDuplicates  = function(){
                           # Removes the duplicated tables 
                           #
                           # Args:
                           #
                           # Returns:
                           #   R6 object (reference to itself)
                           
                           # BEGIN error checks
                           stopifnot(is.list(private$scenData), length(private$scenData) >= 1)
                           # END error checks
                           
                           if(length(private$duplicatedScenIds)){
                             private$scenData[private$duplicatedScenIds] <- NULL
                           }
                           
                           invisible(self)
                         },
                         saveScenarios     = function(batchTags, readPerm = private$uid, 
                                                      writePerm = private$uid, progressBar = NULL){
                           # Save multiple scenarios to database
                           #
                           # Args:
                           #   batchTags:    character vector with tags to attach to scenario
                           #   readPerm:     character vector with uids/groups that have 
                           #                 read permissions for scenarios
                           #   writePerm:    character vector with uids/groups that have 
                           #                 write permissions for scenarios
                           #   progressBar:  shiny pogress bar R6 object
                           # 
                           # Returns:
                           #   reference to itself (BatchImport R6 object)
                           
                           # BEGIN error checks
                           stopifnot(is.list(private$scenData), length(private$scenData) >= 1)
                           stopifnot(is.character(batchTags), length(batchTags) >= 1)
                           stopifnot(is.character(names(private$scenData)), 
                                     length(private$scenData) >= 1)
                           stopifnot(is.character(readPerm), length(readPerm) >= 1)
                           stopifnot(is.character(writePerm), length(writePerm) >= 1)
                           if(!is.null(progressBar)){
                             stopifnot(is.R6(progressBar))
                           }
                           # END error checks
                           
                           scenData         <- private$scenData
                           saveTraceFile    <- as.integer(private$includeTrc)
                           tableNames <- private$tableNamesScenario
                           if(saveTraceFile){
                             tableNames <- c(tableNames, private$traceTabName)
                           }
                           
                           scenMetaColnames <- private$scenMetaColnames
                           readPerm         <- vector2Csv(readPerm)
                           writePerm        <- vector2Csv(writePerm)
                           tableNamesRaw    <- gsub("^[^_]+_", "", tableNames)
                           tablesTmp        <- vector("list", length(tableNames) + saveTraceFile)
                           tables           <- vector("list", length(tableNames) + saveTraceFile)
                           
                           # export metadata to reserve scenario ids
                           numberScen <- length(scenData)
                           metadataTable <- data.frame(rep.int(uid, numberScen), names(scenData), 
                                                       rep.int(1, numberScen), rep.int(batchTags, numberScen), 
                                                       rep.int(readPerm, numberScen), rep.int(writePerm, numberScen),
                                                       stringsAsFactors = FALSE)
                           metadataTable[[3]] <- Sys.time()
                           names(metadataTable) <- scenMetaColnames[-1]
                           firstScenId <- self$getNextSid() + 1L
                           self$writeMetadata(metadataTable)
                           
                           # concatenate to single table first and then do bulk export to database
                           for(scenId in seq_along(scenData)){
                             for(tableId in seq_len(length(tableNamesRaw) + saveTraceFile)){
                               if(tableId > length(tableNamesRaw)){
                                 scenTableId <- match(private$traceTabName, names(scenData[[scenId]]))
                               }
                               scenTableId <- match(tableNamesRaw[tableId], names(scenData[[scenId]]))
                               if(!is.na(scenTableId) && nrow(scenData[[scenId]][[scenTableId]])){
                                 scenData[[scenId]][[scenTableId]] <- cbind(sid = firstScenId + scenId - 1,
                                                                            scenData[[scenId]][[scenTableId]])
                                 colnames(scenData[[scenId]][[scenTableId]])[1] <- scenMetaColnames[1]
                                 tablesTmp[[tableId]][[scenId]] <- scenData[[scenId]][[scenTableId]]
                               }
                             }
                             if(!is.null(progressBar) && scenId %% 10 == 0){
                               progressBar$inc(amount = 0, message = sprintf("Preparing scenario %d of %d.", 
                                                                             scenId, length(scenData)))
                             }
                           }
                           tables <- lapply(seq_along(tableNamesRaw), function(tableId){
                             if(length(tablesTmp[[tableId]])){
                               do.call(bind_rows, tablesTmp[[tableId]])
                             }
                           })
                           if(!is.null(progressBar)){
                             progressBar$inc(amount = 0, message = sprintf("Uploading tables to database."))
                           }
                           lapply(seq_along(tables), function(i){
                             self$exportScenDataset(tables[[i]], tableNames[[i]])
                             if(!is.null(progressBar)){
                               progressBar$inc(amount = 0, message = sprintf("Uploading table %d of %d.",
                                                                             i, length(tables)))
                             }
                           })
                           
                           invisible(self)
                         },
                         getScenDuplicates = function(scenNames = NULL){
                           # Fetch duplicated scenarios and return them
                           # 
                           # Args:
                           #   
                           #   scenNames:     scenario names to check (optional)
                           #
                           # Returns:
                           #    dataframe with ID and tag columns
                           
                           # BEGIN error checks
                           if(is.null(scenNames)){
                             stopifnot(length(private$scenNames) >= 1)
                             scenNames <- private$scenNames
                           }else{
                             stopifnot(is.character(scenNames), length(scenNames) >= 1)
                           }
                           # END error checks
                           
                           previousResults <- self$importDataset(private$tableNameMetadata,
                                                                 colNames = c(private$scenMetaColnames['sname'], 
                                                                              private$scenMetaColnames['stag']))
                           isDuplicated <- previousResults[[private$scenMetaColnames['sname']]] %in% scenNames
                           private$duplicatedScenIds <- scenNames[isDuplicated]
                           return(previousResults[isDuplicated, ])
                         },
                         finalize = function(){
                           NULL
                         }
                       ),
                       private = list(
                         conn                    = NULL,
                         scenData                = NULL,
                         uid                     = character(0L),
                         tableNamesScenario      = character(0L),
                         tableNameMetadata       = character(0L),
                         scenMetaColnames        = character(0L),
                         scenNames               = character(0L),
                         csvPaths                = character(0L),
                         scalarsInputName        = character(0L),
                         scalarsOutputName       = character(0L),
                         tableNamesMustHave      = character(0L),
                         tableNamesToVerify      = character(0L),
                         csvDelim                = character(0L),
                         workDir                 = character(0L),
                         invalidScenIds          = character(0L),
                         duplicatedScenIds       = character(0L),
                         traceColNames           = character(0L),
                         traceTabName            = character(0L),
                         includeTrc              = logical(0L),
                         getScenFilePaths  = function(scenName, paths){
                           csvIdx <- grepl(scenName %+% "/", paths, fixed = TRUE)
                           return(paths[csvIdx])
                         },
                         readScenData      = function(csvPaths){
                           scenData <- lapply(csvPaths, function(csvPath){
                             tryCatch({
                               if(grepl("\\.trc$", csvPath, ignore.case = TRUE)){
                                 scenData <- readTraceData(csvPath, private$traceColNames)
                               }else{
                                 scenData <- read_delim(csvPath, private$csvDelim, col_names = TRUE,
                                                        col_types = cols())
                               }
                               scenData
                             }, error = function(e){
                               stop(sprintf("Problems reading csv file: '%s'. Error message: %s.", csvPath, e),
                                    call. = FALSE)
                             })
                           })
                           names(scenData) <- gsub("\\.(csv|trc)", "", tolower(basename(csvPaths)), 
                                                   ignore.case = TRUE)
                           scenData
                         },
                         verifyScenFiles = function(csvPaths){
                           if(private$includeTrc){
                             grepEx <- "\\.(csv|trc)$"
                           }else{
                             grepEx <- "\\.csv"
                           }
                           csvNames      <- gsub(grepEx, "", basename(csvPaths), 
                                                 ignore.case = TRUE)
                           verifiedIds   <- match(private$tableNamesMustHave, csvNames)
                           if(any(is.na(verifiedIds))){
                             return(NULL)
                           }else{
                             verifiedIds   <- match(csvNames, private$tableNamesToVerify)
                             if(any(is.na(verifiedIds))){
                               return(NULL)
                             }else{
                               return(csvNames)
                             }
                           }
                         },
                         validateTables = function(scenTables, scalarInToVerify, scalarOutToVerify){
                           isInvalidTable <- vapply(seq_along(scenTables), function(tableId){
                             if(identical(names(scenTables)[tableId], private$scalarsInputName)){
                               if(any(!scalarInToVerify %in% scenTables[[tableId]][[1]])){
                                 flog.info("Missing elements in table: '%s'.", 
                                           names(scenTables)[tableId])
                                 return(TRUE)
                               }
                             }else if(identical(names(scenTables)[tableId], private$scalarsOutputName)){
                               if(any(!scalarOutToVerify %in% scenTables[[tableId]][[1]])){
                                 flog.info("Missing elements in table: '%s'.", 
                                           names(scenTables)[tableId])
                                 return(TRUE)
                               }
                             }else if(identical(names(scenTables)[tableId], private$traceTabName)){
                               if(length(scenTables[[tableId]]) != length(private$traceColNames)){
                                 flog.info("Trace file does not have %d columns.", length(private$traceColNames))
                                 return(TRUE)
                               }
                             }
                             return(FALSE)
                           }, logical(1L), USE.NAMES = FALSE)
                           if(any(isInvalidTable)){
                             return(FALSE)
                           }else{
                             return(TRUE)
                           }
                         },
                         getCsvPaths       = function(zipFilePath){
                           if(private$includeTrc){
                             grepEx <- "\\.(csv|trc)$"
                           }else{
                             grepEx <- "\\.csv$"
                           }
                           return(grep(grepEx, unzip(zipFilePath, list = TRUE)$Name, 
                                       ignore.case = TRUE, value = TRUE))
                         },
                         fetchScenNames      = function(csvPaths){
                           return(unique(dirname(csvPaths)))
                         }
                       )
)



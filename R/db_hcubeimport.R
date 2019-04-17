HcubeImport <- R6Class("HcubeImport", 
                       inherit = Db,
                       public = list(
                         initialize        = function(db, scalarsInputName, scalarsOutputName, 
                                                      tableNamesCanHave, tableNamesMustHave,
                                                      csvDelim, workDir, gmsColTypes, gmsFileHeaders,
                                                      strictmode = TRUE){
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
                           #   traceColNames:           column names of trace file
                           #   gmsColTypes:             character vector of column types per datasheet
                           #   gmsFileHeaders:          character vector of file headers per datasheet
                           #   strictmode:              logical that specifies whether strict mode is active
                           #
                           
                           # BEGIN error checks
                           stopifnot(is.R6(db))
                           stopifnot(is.character(scalarsInputName), length(scalarsInputName) == 1)
                           stopifnot(is.character(scalarsOutputName), length(scalarsOutputName) == 1)
                           stopifnot(is.character(tableNamesCanHave))
                           stopifnot(is.character(tableNamesMustHave))
                           stopifnot(is.character(csvDelim), length(csvDelim) == 1)
                           stopifnot(is.character(workDir), length(workDir) == 1)
                           stopifnot(is.character(traceColNames), length(traceColNames) >= 1)
                           stopifnot(is.character(gmsColTypes), length(gmsColTypes) >= 1)
                           stopifnot(is.list(gmsFileHeaders), length(gmsFileHeaders) >= 1)
                           stopifnot(is.logical(strictmode), length(strictmode) == 1)
                           # END error checks
                           
                           private$conn               <- db$getConn()
                           private$uid                <- db$getUid()
                           private$userAccessGroups   <- db$getUserAccessGroups()
                           private$tableNamesScenario <- db$getTableNamesScenario()
                           private$tableNameMetadata  <- db$getTableNameMetadata()
                           private$scenMetaColnames   <- db$getScenMetaColnames()
                           private$dbSchema           <- db$getDbSchema()
                           private$traceTabName       <- private$dbSchema$tabName[['_scenTrc']]
                           private$traceColNames      <- private$dbSchema$colNames[['_scenTrc']]
                           private$scalarsInputName   <- tolower(scalarsInputName)
                           private$scalarsOutputName  <- tolower(scalarsOutputName)
                           private$tableNamesMustHave <- tableNamesMustHave
                           private$tableNamesToVerify <- c(tableNamesCanHave, tableNamesMustHave)
                           private$csvDelim           <- csvDelim
                           private$workDir            <- workDir
                           private$includeTrc         <- private$traceTabName %in% private$tableNamesToVerify
                           private$gmsColTypes        <- gmsColTypes
                           private$gmsFileHeaders     <- gmsFileHeaders
                           private$strictmode         <- strictmode
                           private$noScen             <- NA_integer_
                         },
                         getScenNames      = function() private$scenNames,
                         getInvalidScenIds = function() private$invalidScenIds,
                         getNoScen         = function() private$noScen,
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
                           if(length(csvPaths)){
                             csvPaths <- utils::unzip(zipFilePath, exdir = gsub("/?$", "", private$workDir))
                             if(any(Sys.readlink(csvPaths) != "")){
                               stop("zip archive contains symlinks.", call. = FALSE)
                             }
                           }
                           
                           private$csvPaths <- csvPaths
                           private$csvPaths <- lapply(private$scenNames, 
                                                      private$getScenFilePaths, csvPaths)
                           names(private$csvPaths) <- private$scenNames
                           flog.trace("%s files unzipped in: '%s'.", length(private$csvPaths), private$workDir)
                           invisible(self)
                         },
                         validateScenFiles = function(){
                           # validates scenario data
                           #
                           # Args:
                           #
                           # Returns:
                           #   reference to itself (importHcube R6 object)
                           
                           
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
                           #   reference to itself (importHcube R6 object)
                           
                           # BEGIN error checks
                           if(length(scalarInToVerify)){
                             stopifnot(is.character(scalarInToVerify), length(scalarInToVerify) >= 1)
                           }
                           if(length(scalarOutToVerify)){
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
                         saveScenarios     = function(hcubeTags, jobID, readPerm = private$uid, 
                                                      writePerm = private$uid, execPerm = private$uid,
                                                      progressBar = NULL){
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
                           execPerm         <- vector2Csv(execPerm)
                           tableNamesRaw    <- gsub("^[^_]+_", "", tableNames)
                           tablesTmp        <- vector("list", length(tableNames) + saveTraceFile)
                           tables           <- vector("list", length(tableNames) + saveTraceFile)
                           
                           # export metadata to reserve scenario ids
                           numberScen     <- length(scenData)
                           private$noScen <- numberScen
                           metadataTable  <- tibble(rep.int(private$uid, numberScen), names(scenData), 
                                                    rep.int(1, numberScen), rep.int(hcubeTags, numberScen), 
                                                    rep.int(readPerm, numberScen), rep.int(writePerm, numberScen),
                                                    rep.int(execPerm, numberScen), rep.int(jobID, numberScen))
                           metadataTable[[3]] <- Sys.time()
                           names(metadataTable) <- scenMetaColnames[-1]
                           firstScenId <- self$getLatestSid() + 1L
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
                             if(!is.null(private$gmsFileHeaders[[tableNamesRaw[[i]]]]) && 
                                length(tables[[i]])){
                               names(tables[[i]])[-1L] <- private$gmsFileHeaders[[tableNamesRaw[[i]]]]
                             }
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
                         dbSchema                = vector("list", 3L),
                         tableNamesScenario      = character(0L),
                         tableNameMetadata       = character(0L),
                         scenMetaColnames        = character(0L),
                         scenNames               = character(0L),
                         csvPaths                = character(0L),
                         gmsColTypes             = character(0L),
                         gmsFileHeaders          = character(0L),
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
                         strictmode              = logical(1L),
                         includeTrc              = logical(0L),
                         noScen                  = integer(1L),
                         getScenFilePaths  = function(scenName, paths){
                           csvIdx <- grepl(scenName %+% "/", paths, fixed = TRUE)
                           return(paths[csvIdx])
                         },
                         readScenData      = function(csvPaths){
                           scenDataNames <- gsub("\\.(csv|trc)$", "", tolower(basename(csvPaths)), 
                                                 ignore.case = TRUE)
                           scenData <- lapply(seq_along(csvPaths), function(i){
                             csvPath <- csvPaths[[i]]
                             tryCatch({
                               if(grepl("\\.trc$", csvPath, ignore.case = TRUE)){
                                 scenData <- readTraceData(csvPath, private$traceColNames)[1, ]
                               }else{
                                 colTypes <- NULL
                                 if(!is.na(private$gmsColTypes[scenDataNames[[i]]])){
                                   colTypes <- private$gmsColTypes[[scenDataNames[[i]]]]
                                 }
                                 scenData <- read_delim(csvPath, private$csvDelim, col_names = TRUE,
                                                        col_types = cols())
                                 if(!is.null(colTypes)){
                                   scenData <- fixColTypes(scenData, colTypes)
                                 }
                                 scenData <- scenData %>% mutate_if(is.numeric , replace_na, replace = 0) %>% 
                                   replace(is.na(.), "")
                                 
                               }
                               scenData
                             }, error = function(e){
                               stop(sprintf("Problems reading csv file: '%s'. Error message: %s.", csvPath, e),
                                    call. = FALSE)
                             })
                           })
                           names(scenData) <- scenDataNames
                           scenData
                         },
                         verifyScenFiles = function(csvPaths){
                           if(private$includeTrc){
                             grepEx <- "\\.(csv|trc)$"
                           }else{
                             grepEx <- "\\.csv$"
                           }
                           csvNames      <- gsub(grepEx, "", basename(csvPaths), 
                                                 ignore.case = TRUE)
                           verifiedIds   <- match(private$tableNamesMustHave, csvNames)
                           if(any(is.na(verifiedIds))){
                             flog.info("The scenario misses some tables that must be included: '%s'.", 
                                       paste(private$tableNamesMustHave[is.na(verifiedIds)], collapse = "', '"))
                             return(NULL)
                           }else{
                             verifiedIds   <- match(tolower(csvNames), private$tableNamesToVerify)
                             if(any(is.na(verifiedIds))){
                               flog.info("The scenario includes invalid datasets: '%s'.", 
                                         paste(csvNames[is.na(verifiedIds)], collapse = "', '"))
                               return(NULL)
                             }else{
                               return(csvNames)
                             }
                           }
                         },
                         validateTables = function(scenTables, scalarInToVerify, scalarOutToVerify){
                           isInvalidTable <- vapply(seq_along(scenTables), function(tableId){
                             tableName <- tolower(names(scenTables)[tableId])
                             if(identical(tableName, private$scalarsInputName) &&
                                any(!tolower(scenTables[[tableId]][[1]]) %in% scalarInToVerify)){
                                 flog.info("Additional elements in input scalar table: '%s'.", 
                                           scenTables[[tableId]][[1]][!scenTables[[tableId]][[1]] %in% scalarInToVerify])
                                 return(TRUE)
                             }else if(identical(tableName, private$scalarsOutputName) &&
                                      (any(!tolower(scenTables[[tableId]][[1]]) %in% scalarOutToVerify) || 
                                      length(scenTables[[tableId]][[1]]) != length(scalarOutToVerify))){
                               flog.info("Missing or additional elements in output scalar table.")
                               return(TRUE)
                             }else if(identical(tableName, private$traceTabName) &&
                                      length(scenTables[[tableId]]) != length(private$traceColNames)){
                               flog.info("Trace file does not have %d columns.", 
                                         length(private$traceColNames))
                               return(TRUE)
                             }else if(!is.null(private$gmsFileHeaders[[tableName]])){
                               if(!validateHeaders(names(scenTables[[tableId]]), 
                                                  private$gmsFileHeaders[[tableName]])){
                                 flog.info("Dataset: '%s' has invalid headers ('%s'). Headers should be: '%s'.", 
                                           tableName, paste(names(scenTables[[tableId]]), collapse = "', '"), 
                                           paste(private$gmsFileHeaders[[tableName]], collapse = "', '"))
                                 if(private$strictmode){
                                   return(TRUE)
                                 }
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
                             grepEx <- "^((?!\\.\\.).)*\\.(csv|trc)$"
                           }else{
                             grepEx <- "^((?!\\.\\.).)*\\.csv$"
                           }
                           fileNamesZip   <- utils::unzip(zipFilePath, list = TRUE)
                           fileNamesZip   <- fileNamesZip[fileNamesZip$Length > 0, ]$Name
                           validFileNames <- grep(grepEx, fileNamesZip, 
                                                  ignore.case = TRUE, value = TRUE, perl = TRUE)
                           if(!identical(length(fileNamesZip), length(validFileNames))){
                             stop("invalidFiles", call. = FALSE)
                           }
                           
                           return(validFileNames)
                         },
                         fetchScenNames      = function(csvPaths){
                           return(unique(dirname(csvPaths)))
                         }
                       )
)



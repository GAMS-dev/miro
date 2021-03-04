DbMigrator <- R6::R6Class("DbMigrator", public = list(
  initialize = function(db){
    private$db <- db
    private$conn <- db$getConn()
    private$tableNamesScenario <- db$getTableNamesScenario()
    private$modelNameDb <- db$getModelNameDb()
    private$tablePrefixLen <- nchar(private$modelNameDb) + 2L
    private$dbSchema <- db$getDbSchema()
    private$orphanedTables <- private$getOrphanedTablesInternal(ioConfig$hcubeScalars)
    return(invisible(self))
  },
  getOrphanedTablesInfo = function(){
    tabInfo <- lapply(private$orphanedTables, function(tableName){
      return(private$getTableInfo(tableName))
    })
    names(tabInfo) <- vapply(private$orphanedTables, private$getSymName, character(1L), USE.NAMES = FALSE)
    return(tabInfo)
  },
  getInconsistentTablesInfo = function(){
    colNames <- private$dbSchema$colNames
    colTypes <- private$dbSchema$colTypes
    
    badTables <- lapply(private$tableNamesScenario, function(tabName){
      symName <- private$getSymName(tabName)
      tabInfo <- list(tabName = symName,
                      colNames = colNames[[symName]],
                      colTypes = colTypes[[symName]])
      confHeaders <- tabInfo$colNames
      
      if(!is.null(confHeaders)){
        if(!dbExistsTable(private$conn, tabName)){
          return(tabInfo)
        }
        currentTabInfo <- private$getTableInfo(tabName)
        tabColNames <- currentTabInfo$colNames
        tabColTypes <- currentTabInfo$colTypes
        if(!identical(colTypeVectorToString(tabColTypes), colTypes[[symName]]) ||
           any(confHeaders != tabColNames)){
          return(c(tabInfo, list(currentColNames = tabColNames,
                                 currentColTypes = tabColTypes)))
        }
      }
      return(NA)
    })
    return(badTables[!is.na(badTables)])
  },
  getTableNamesModel = function(){
    if(inherits(private$conn, "PqConnection")){
      query <- SQL(paste0("SELECT table_name FROM information_schema.tables", 
                          " WHERE table_schema='public' AND table_type='BASE TABLE'", 
                          " AND (table_name IN (", 
                          paste(dbQuoteString(private$conn, c(private$db$getTableNameMetadata(),
                                                              private$dbSchema$tabName[["_scenLock"]],
                                                              private$dbSchema$tabName[["_scenTrc"]],
                                                              private$dbSchema$tabName[["_scenAttach"]],
                                                              private$dbSchema$tabName[["_scenScripts"]],
                                                              private$dbSchema$tabName[["_jobMeta"]],
                                                              private$tableNamesScenario)),
                                collapse = ", "),
                          ") OR table_name LIKE ", 
                          dbQuoteString(private$conn, private$db$escapePattern(private$modelNameDb) %+% "\\_%"), 
                          ");"))
    }else{
      query <- SQL(paste0("SELECT name FROM sqlite_master WHERE type = 'table'",
                          " AND (name IN (", 
                          paste(dbQuoteString(private$conn, c(private$db$getTableNameMetadata(),
                                                              private$dbSchema$tabName[["_scenLock"]],
                                                              private$dbSchema$tabName[["_scenTrc"]],
                                                              private$dbSchema$tabName[["_scenAttach"]],
                                                              private$dbSchema$tabName[["_scenScripts"]],
                                                              private$dbSchema$tabName[["_jobMeta"]],
                                                              private$tableNamesScenario)),
                                collapse = ", "),
                          ") OR name LIKE ", 
                          dbQuoteString(private$conn, private$db$escapePattern(private$modelNameDb) %+% "\\_%"), " ESCAPE '\\');"))
    }
    return(dbGetQuery(private$conn, query)[[1L]])
  },
  backupDatabase = function(fileName){
    if(inherits(private$conn, "PqConnection")){
      stop("Not implemented!", call. = FALSE)
    }
    query <- SQL(paste0("VACUUM INTO ", dbQuoteString(private$conn, fileName)))
    flog.trace("Running query: %s", query)
    dbExecute(private$conn, query)
    flog.info("Database backed up to: '%s'", fileName)
    return(invisible(self))
  },
  removeTablesModel = function(tableNames = NULL){
    stopifnot(is.null(tableNames) || is.character(tableNames))
    
    removeAllTables <- FALSE
    if(!length(tableNames)){
      removeAllTables <- TRUE
      tableNames <- self$getTableNamesModel()
    }
    
    if(inherits(private$conn, "PqConnection")){
      query <- paste0("DROP TABLE IF EXISTS ",  
                      paste(dbQuoteIdentifier(private$conn, tableNames),
                            collapse = ", "), " CASCADE;")
      dbExecute(private$conn, query)
      flog.info("Database tables: '%s' deleted.", paste(tableNames, "', '"))
      return(invisible(self))
    }
    # turn foreign key usage off
    dbExecute(private$conn, "PRAGMA foreign_keys = OFF;")
    for(tableName in tableNames){
      query <- paste0("DROP TABLE IF EXISTS ",  
                      dbQuoteIdentifier(private$conn, tableName), " ;")
      dbExecute(private$conn, query)
      flog.info("Database table: '%s' deleted.", tableName)
    }
    # turn foreign key usage on again
    dbExecute(private$conn, "PRAGMA foreign_keys = ON;")
    if(removeAllTables){
      private$db$deleteRows("_sys__data_hashes", "model", private$modelNameDb)
    }
    return(invisible(self))
  },
  migrateDb = function(migrationConfig, forceRemove = FALSE, callback = NULL){
    oldTableNames <- vapply(migrationConfig, "[[", character(1L), "oldTableName", USE.NAMES = FALSE)
    migrationConfig <- migrationConfig[oldTableNames != "-"]
    oldTableNames <- oldTableNames[oldTableNames != "-"]
    
    # prefix model name
    if(length(oldTableNames)){
      oldTableNames <- paste0(private$modelNameDb, "_", oldTableNames)
      names(migrationConfig) <- paste0(private$modelNameDb, "_", names(migrationConfig))
      private$validateMigrationConfig(migrationConfig)
      newTableNames <- names(migrationConfig)
    }
    
    tablesToRemove <- !private$orphanedTables %in% oldTableNames
    
    if(any(tablesToRemove)){
      if(!forceRemove){
        stop_custom("error_data_loss", "The database migration you specified will lead to loss of data but forceRemove was not set.", call.= FALSE)
      }
      self$removeTablesModel(private$orphanedTables[tablesToRemove])
      private$orphanedTables <- private$orphanedTables[!tablesToRemove]
    }
    if(length(oldTableNames)){
      tablesToRename <- newTableNames[newTableNames != oldTableNames]
      
      newTableNamesExist <- tablesToRename %in% private$existingTables
      
      if(any(!tablesToRename[newTableNamesExist] %in% oldTableNames)){
        stop_custom("error_config", sprintf("Invalid migration config: Can't rename table(s): %s as they already exist",
                                            tablesToRename[newTableNamesExist]),
                    call. = FALSE)
      }
      
      for(tableToRename in tablesToRename[newTableNamesExist]){
        private$renameTable(paste0(private$modelNameDb, "_",
                                   migrationConfig[[tableToRename]]$oldTableName),
                            paste0(private$modelNameDb, "__",
                                   migrationConfig[[tableToRename]]$oldTableName))
        migrationConfig[[tableToRename]]$oldTableName <- paste0("_", migrationConfig[[tableToRename]]$oldTableName)
      }
      
      for(tableToRename in tablesToRename){
        private$renameTable(paste0(private$modelNameDb, "_",
                                   migrationConfig[[tableToRename]]$oldTableName),
                            tableToRename)
      }
    }
    
    lapply(names(migrationConfig), function(tableName){
      # first see if we can take shortcuts
      if(!is.null(callback)){
        callback()
      }
      currentLayout <- private$getTableInfo(tableName)
      migrationLayout <- migrationConfig[[tableName]]
      
      newColNames <- private$dbSchema$colNames[[private$getSymName(tableName)]]
      
      colsToRemove <- !currentLayout$colNames %in% migrationLayout$colNames
      if(any(colsToRemove) && !forceRemove){
        stop_custom("error_data_loss", "The database migration you specified will lead to loss of data but forceRemove was not set.", call.= FALSE)
      }
      
      if(length(migrationLayout$colNames) == length(currentLayout$colNames[!colsToRemove]) &&
         all(migrationLayout$colNames == currentLayout$colNames[!colsToRemove])){
        if(any(colsToRemove)){
          flog.debug("Removing columns: %s from table: %s",
                     paste(currentLayout$colNames[colsToRemove], collapse = ", "),
                     tableName)
          private$dropColumns(tableName, currentLayout$colNames[colsToRemove])
        }
        currentColNames <- migrationLayout$colNames
        colsToRename <- currentColNames == currentLayout$colNames & currentColNames != newColNames
        newNamesExist <- colsToRename & currentColNames %in% newColNames
        
        currentColNamesTmp <- currentColNames
        currentColNamesTmp[newNamesExist] <- paste0("_", currentColNamesTmp[newNamesExist])
        for(colIdToRename in which(newNamesExist)){
          flog.debug("Renaming columns: %s to: %s (table: %s)",
                     paste(currentColNames[colIdToRename], collapse = ", "),
                     paste(currentColNamesTmp[colIdToRename], collapse = ", "),
                     tableName)
          private$renameColumn(tableName,
                               oldName = currentColNames[colIdToRename],
                               newName = currentColNamesTmp[colIdToRename])
        }
        for(colIdToRename in which(colsToRename)){
          flog.debug("Renaming columns: %s to: %s (table: %s)",
                     paste(currentColNamesTmp[colIdToRename], collapse = ", "),
                     paste(newColNames[colIdToRename], collapse = ", "),
                     tableName)
          private$renameColumn(tableName,
                               oldName = currentColNamesTmp[colIdToRename],
                               newName = newColNames[colIdToRename])
        }
        return()
      }
      
      colsToAdd <- migrationLayout$colNames == "-"
      if(all(colsToAdd)){
        # just remove old table
        flog.debug("Removing table: %s", tableName)
        self$removeTablesModel(tableName)
        return()
      }
      
      if(any(colsToAdd) &&
         identical(sum(colsToAdd), length(newColNames) - min(which(colsToAdd)) + 1L)){
        # all columns to add are at the end
        flog.debug("Adding column(s): %s to the end of table: %s",
                   newColNames[colsToAdd], tableName)
        private$addColumns(tableName,
                           newColNames[colsToAdd],
                           private$getColTypes(tableName)[colsToAdd])
        return()
      }
      # seems we are out of luck/no shortcuts
      # need to recreate table and copy data over
      flog.debug("Remapping table: %s", tableName)
      private$remapTable(tableName, migrationLayout$colNames)
      return()
    })
  }
), private = list(
  conn = NULL,
  db = NULL,
  tableNamesScenario = NULL,
  dbSchema = NULL,
  modelNameDb = NULL,
  tablePrefixLen = NULL,
  orphanedTables = NULL,
  existingTables = NULL,
  renameIndex = function(oldName, newName){
    if(inherits(private$conn, "PqConnection")){
      query <- SQL(paste0("ALTER INDEX ",
                          DBI::dbQuoteIdentifier(private$conn, paste0("sid_index_", oldName)),
                          " RENAME TO ",
                          DBI::dbQuoteIdentifier(private$conn, paste0("sid_index_", newName))))
      flog.debug("Running query: %s", query)
      dbExecute(private$conn, query)
      return(invisible(self))
    }
    private$dropIndex(oldName)
    query <- private$getCreateIndexQuery(newName)
    flog.debug("Running query: %s", query)
    dbExecute(private$conn, query)
    return(invisible(self))
  },
  dropIndex = function(tableName){
    query <- SQL(paste0("DROP INDEX ",
                        DBI::dbQuoteIdentifier(private$conn, paste0("sid_index_", tableName))))
    flog.debug("Running query: %s", query)
    dbExecute(private$conn, query)
    return(invisible(self))
  },
  getCreateIndexQuery = function(tableName){
    return(SQL(paste0("CREATE INDEX ",
                      DBI::dbQuoteIdentifier(private$conn, paste0("sid_index_", tableName)),
                      " ON ",
                      DBI::dbQuoteIdentifier(private$conn, tableName),
                      " (",
                      DBI::dbQuoteIdentifier(private$conn, private$db$getScenMetaColnames()['sid']),
                      ");")))
  },
  getCreateTableQuery = function(tableName, colNames, colTypes){
    return(SQL(paste0("CREATE TABLE ", 
                      DBI::dbQuoteIdentifier(private$conn, tableName), 
                      " (", paste(c(DBI::dbQuoteIdentifier(private$conn,
                                                           private$db$getScenMetaColnames()['sid']),
                                    DBI::dbQuoteIdentifier(private$conn, colNames)), 
                                  c("int", colTypes), collapse = ", "),
                      ", CONSTRAINT foreign_key FOREIGN KEY (", 
                      DBI::dbQuoteIdentifier(private$conn, private$db$getScenMetaColnames()['sid']), 
                      ") REFERENCES ",
                      DBI::dbQuoteIdentifier(private$conn, private$db$getTableNameMetadata()), 
                      "(",
                      DBI::dbQuoteIdentifier(private$conn, private$db$getScenMetaColnames()['sid']), 
                      ") ON DELETE CASCADE", ");")))
  },
  getRemapTableQuery = function(tableName, colMapping){
    sidColName <- DBI::dbQuoteIdentifier(private$conn,
                                         private$db$getScenMetaColnames()['sid'])
    return(SQL(paste0("INSERT INTO ",
                      DBI::dbQuoteIdentifier(private$conn, paste0("_", tableName)),
                      " (", sidColName, ", ",
                      paste(vapply(names(colMapping), function(colNameDest){
                        DBI::dbQuoteIdentifier(private$conn, colNameDest)
                      }, character(1L), USE.NAMES = FALSE), collapse = ", "),
                      ") SELECT ", sidColName, ", ",
                      paste(vapply(colMapping, function(colNameOrigin){
                        DBI::dbQuoteIdentifier(private$conn, colNameOrigin)
                      }, character(1L), USE.NAMES = FALSE), collapse = ", "),
                      " FROM ",
                      DBI::dbQuoteIdentifier(private$conn, tableName),
                      ";")))
  },
  remapTable = function(tableName, colNames){
    newColNames <- private$dbSchema$colNames[[private$getSymName(tableName)]]
    if(!identical(length(newColNames), length(colNames))){
      stop_custom("error_config",
                  sprintf("Length of column names do not match. New columns: %s. Old columns: %s",
                          paste(newColNames, collapse = ", "),
                          paste(colNames, collapse = ", ")), call. = FALSE)
    }
    colMapping <- colNames
    names(colMapping) <- newColNames
    colMapping <- colMapping[colMapping != "-"]
    
    if(inherits(private$conn, "PqConnection")){
      return(private$remapTablePostgres(tableName, colMapping))
    }
    return(private$remapTableSQLite(tableName, colMapping))
  },
  remapTablePostgres = function(tableName, colMapping){
    colNames <- private$dbSchema$colNames[[private$getSymName(tableName)]]
    colTypes <- private$getColTypes(tableName)
    dbWithTransaction(
      private$conn,
      {
        query <- private$getCreateTableQuery(paste0("_", tableName), colNames, colTypes)
        flog.debug("Running query: %s", query)
        dbExecute(private$conn, query)
        query <- private$getRemapTableQuery(tableName, colMapping)
        flog.debug("Running query: %s", query)
        dbExecute(private$conn, query)
        query <- SQL(paste0("DROP TABLE ",
                            dbQuoteIdentifier(private$conn, tableName), " CASCADE;"))
        flog.debug("Running query: %s", query)
        dbExecute(private$conn, query)
        query <- SQL(paste0("ALTER TABLE ",
                            DBI::dbQuoteIdentifier(private$conn, paste0("_", tableName)), 
                            " RENAME TO ",
                            DBI::dbQuoteIdentifier(private$conn, tableName)))
        flog.debug("Running query: %s", query)
        dbExecute(private$conn, query)
        query <- private$getCreateIndexQuery(tableName)
        flog.debug("Running query: %s", query)
        dbExecute(private$conn, query)
      }
    )
    return(invisible(self))
  },
  remapTableSQLite = function(tableName, colMapping){
    # this basically implements the steps described on https://sqlite.org/lang_altertable.html
    query <- SQL("PRAGMA foreign_keys = OFF;")
    flog.debug("Running query: %s", query)
    dbExecute(private$conn, query)
    sidColName <- DBI::dbQuoteIdentifier(private$conn,
                                         private$db$getScenMetaColnames()['sid'])
    colNames <- private$dbSchema$colNames[[private$getSymName(tableName)]]
    colTypes <- private$getColTypes(tableName)
    
    dbWithTransaction(
      private$conn,
      {
        query <- private$getCreateTableQuery(paste0("_", tableName), colNames, colTypes)
        flog.debug("Running query: %s", query)
        dbExecute(private$conn, query)
        query <- private$getRemapTableQuery(tableName, colMapping)
        flog.debug("Running query: %s", query)
        dbExecute(private$conn, query)
        query <- SQL(paste0("DROP TABLE ",
                            dbQuoteIdentifier(private$conn, tableName)))
        flog.debug("Running query: %s", query)
        dbExecute(private$conn, query)
        query <- SQL(paste0("ALTER TABLE ",
                            DBI::dbQuoteIdentifier(private$conn, paste0("_", tableName)), 
                            " RENAME TO ",
                            DBI::dbQuoteIdentifier(private$conn, tableName)))
        flog.debug("Running query: %s", query)
        dbExecute(private$conn, query)
        query <- private$getCreateIndexQuery(tableName)
        flog.debug("Running query: %s", query)
        dbExecute(private$conn, query)
        query <- SQL("PRAGMA foreign_key_check;")
        flog.debug("Running query: %s", query)
        dbExecute(private$conn, query)
      }
    )
    query <- SQL("PRAGMA foreign_keys = ON;")
    flog.debug("Running query: %s", query)
    dbExecute(private$conn, query)
    return(invisible(self))
  },
  renameColumn = function(tableName, oldName, newName){
    query <- SQL(paste0("ALTER TABLE ",
                        DBI::dbQuoteIdentifier(private$conn, tableName), 
                        " RENAME COLUMN ",
                        DBI::dbQuoteIdentifier(private$conn, oldName), "  TO ",
                        DBI::dbQuoteIdentifier(private$conn, newName)))
    DBI::dbExecute(private$conn, query)
    return(invisible(self))
  },
  addColumns = function(tableName, colNames, colTypes){
    if(inherits(private$conn, "PqConnection")){
      query <- SQL(paste0("ALTER TABLE ",
                          DBI::dbQuoteIdentifier(private$conn, tableName),
                          paste(" ADD COLUMN",
                                DBI::dbQuoteIdentifier(private$conn, colNames),
                                colTypes, collapse = ",")))
      flog.trace("Running query: %s", query)
      DBI::dbExecute(private$conn, query)
      return(invisible(self))
    }
    for(colIdx in seq_along(colNames)){
      query <- SQL(paste("ALTER TABLE",
                         DBI::dbQuoteIdentifier(private$conn, tableName),
                         "ADD COLUMN",
                         DBI::dbQuoteIdentifier(private$conn, colNames[colIdx]),
                         colTypes[colIdx]))
      flog.trace("Running query: %s", query)
      DBI::dbExecute(private$conn, query)
    }
    return(invisible(self))
  },
  dropColumns = function(tableName, colNames){
    if(inherits(private$conn, "PqConnection")){
      query <- SQL(paste0("ALTER TABLE ",
                          DBI::dbQuoteIdentifier(private$conn, tableName), 
                          paste(" DROP COLUMN",
                                DBI::dbQuoteIdentifier(private$conn, colNames),
                                collapse = ",")))
      flog.trace("Running query: %s", query)
      DBI::dbExecute(private$conn, query)
      return(invisible(self))
    }
  },
  renameTable = function(oldName, newName){
    query <- SQL(paste0("ALTER TABLE ",
                        DBI::dbQuoteIdentifier(private$conn, oldName), 
                        " RENAME TO ",
                        DBI::dbQuoteIdentifier(private$conn, newName)))
    flog.trace("Running query: %s", query)
    DBI::dbExecute(private$conn, query)
    private$renameIndex(oldName, newName)
    return(invisible(self))
  },
  getSymName = function(tabName){
    return(substr(tabName, private$tablePrefixLen, nchar(tabName)))
  },
  getColTypes = function(tableName){
    colTypes <- strsplit(private$dbSchema$colTypes[[private$getSymName(tableName)]],
                         "", fixed = TRUE)[[1]]
    return(private$getColTypesSQL(colTypes))
  },
  getColTypesSQL = function(colTypes){
    return(vapply(colTypes, function(colType){
      if(colType %in% c("d", "numeric")){
        return("DOUBLE PRECISION")
      }
      if(colType %in% c("c", "character")){
        return("TEXT")
      }
      stop_custom("error_config", sprintf("Invalid migration config: Invalid column type: %s",
                                          colType),
                  call. = FALSE)
    }, character(1L), USE.NAMES = FALSE))
  },
  validateMigrationConfig = function(migrationConfig){
    oldTableNames <- vapply(migrationConfig, "[[", character(1L), "oldTableName", USE.NAMES = FALSE)
    oldTableNames <- paste0(private$modelNameDb, "_", oldTableNames)
    invalidOldTableNames <- !oldTableNames %in% private$existingTables
    if(any(invalidOldTableNames)){
      stop_custom("error_config", sprintf("Invalid migration config: (old) table(s): %s do not exist in database",
                                          paste(oldTableNames[invalidOldTableNames], collapse = ", ")),
                  call. = FALSE)
    }
    duplicatedOldTableNames <- duplicated(oldTableNames)
    if(any(duplicatedOldTableNames)){
      stop_custom("error_config", "Invalid migration config: Can't duplicate a table",
                  call. = FALSE)
    }
    for(i in seq_along(migrationConfig)){
      if(!names(migrationConfig)[i] %in% private$tableNamesScenario){
        stop_custom("error_config", sprintf("Invalid migration config: table: %s does not exist in db schema",
                                            names(migrationConfig)[i]),
                    call. = FALSE)
      }
      if(!identical(length(migrationConfig[[i]]$colNames),
                    length(private$dbSchema$colNames[[private$getSymName(names(migrationConfig)[i])]]))){
        stop_custom("error_config", sprintf("Invalid migration config: Length of column names invalid for table: %s.",
                                            names(migrationConfig)[i]),
                    call. = FALSE)
      }
    }
    return(invisible(self))
  },
  getOrphanedTablesInternal     = function(hcubeScalars = NULL){
    # find orphaned database tables 
    #
    # Args:
    #   hcubeScalars:        name of scalars that are transferred to 
    #                        tables in Hypercube mode
    #
    # Returns:
    #   list with names of orphaned database tables
    if(inherits(private$conn, "PqConnection")){
      query <- SQL(paste0("SELECT table_name FROM information_schema.tables", 
                          " WHERE table_schema='public' AND table_type='BASE TABLE'", 
                          " AND table_name LIKE ", 
                          dbQuoteString(private$conn, private$db$escapePattern(private$modelNameDb) %+% "\\_%"), ";"))
    }else{
      query <- SQL(paste0("SELECT name FROM sqlite_master WHERE type = 'table'",
                          " AND name LIKE ", 
                          dbQuoteString(private$conn, private$db$escapePattern(private$modelNameDb) %+% "\\_%"), " ESCAPE '\\';"))
    }
    tryCatch({
      dbTables <- dbGetQuery(private$conn, query)[[1L]]
    }, error = function(e){
      stop(sprintf("Db: An error occurred while fetching table names from database (Db.getOrphanTables). Error message: '%s'.",
                   e), call. = FALSE)
    })
    private$existingTables <- dbTables
    orphanedTables <- dbTables[!dbTables %in% private$tableNamesScenario]
    if(!is.null(hcubeScalars)){
      hcubeScalarsIdx <-  orphanedTables %in% paste0(private$modelNameDb, "_", 
                                                     hcubeScalars)
      orphanedTables <- orphanedTables[!hcubeScalarsIdx]
    }
    return(orphanedTables)
  },
  getTableInfo = function(tableName){
    if(inherits(private$conn, "PqConnection")){
      query <- SQL(paste0("SELECT ordinal_position,column_name,data_type FROM information_schema.columns WHERE table_name = ", 
                          dbQuoteString(private$conn, tableName),
                          " ORDER BY ordinal_position;"))
      tabInfo     <- dbGetQuery(private$conn, query)
      return(list(colNames = tabInfo$column_name[-1], colTypes = tabInfo$data_type[-1]))
    }
    query <- SQL(paste0("PRAGMA table_info(", 
                        dbQuoteIdentifier(private$conn, tableName), ");"))
    tabInfo     <- dbGetQuery(private$conn, query)
    return(list(colNames = tabInfo$name[-1], colTypes = tabInfo$type[-1]))
  }
))
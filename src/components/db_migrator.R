DbMigrator <- R6::R6Class("DbMigrator", public = list(
  initialize = function(db){
    private$db <- db
    private$conn <- db$getConn()
    private$symNamesScenario <- dbSchema$getAllSymbols()
    private$orphanedTables <- private$getOrphanedTablesInternal(ioConfig$hcubeScalars)
    return(invisible(self))
  },
  getOrphanedTablesInfo = function(){
    tabInfo <- lapply(private$orphanedTables, function(dbTableName){
      return(private$getTableInfo(dbTableName))
    })
    names(tabInfo) <- private$orphanedTables
    return(tabInfo)
  },
  getInconsistentTablesInfo = function(){
    badTables <- lapply(private$symNamesScenario, function(symName){
      tabInfo <- dbSchema$getDbSchema(symName)
      confHeaders <- tabInfo$colNames
      dbTableName <- tabInfo$tabName
      
      if(!is.null(confHeaders)){
        if(!dbExistsTable(private$conn, dbTableName)){
          return(tabInfo)
        }
        currentTabInfo <- private$getTableInfo(dbTableName)
        tabColNames <- currentTabInfo$colNames
        tabColTypes <- currentTabInfo$colTypes
        if(!identical(colTypeVectorToString(tabColTypes), tabInfo$colTypes) ||
           any(confHeaders != tabColNames)){
          return(c(tabInfo, list(currentColNames = tabColNames,
                                 currentColTypes = tabColTypes)))
        }
      }
      return(NA)
    })
    return(badTables[!is.na(badTables)])
  },
  getDbTableNamesModel = function(){
    if(inherits(private$conn, "PqConnection")){
      query <- paste0("SELECT table_name FROM information_schema.tables", 
                      " WHERE table_schema='",
                      dbQuoteIdentifier(private$db$getInfo()$schema),
                      "' AND table_type='BASE TABLE';")
    }else{
      query <- paste0("SELECT name FROM sqlite_master WHERE type = 'table';")
    }
    return(dbGetQuery(private$conn, SQL(query))[[1L]])
  },
  backupDatabase = function(fileName){
    if(inherits(private$conn, "PqConnection")){
      stop("Not implemented!", call. = FALSE)
    }
    private$db$runQuery(paste0("VACUUM INTO ", dbQuoteString(private$conn, fileName)))
    flog.info("Database backed up to: '%s'", fileName)
    return(invisible(self))
  },
  removeTablesModel = function(dbTableNames = NULL){
    stopifnot(is.null(dbTableNames) || is.character(dbTableNames))
    
    if(length(dbTableNames)){
      removeAllTables <- FALSE
    }else{
      removeAllTables <- TRUE
      dbTableNames <- self$getDbTableNamesModel()
    }
    
    if(inherits(private$conn, "PqConnection")){
      private$db$runQuery(paste0("DROP TABLE IF EXISTS ",  
                                 paste(dbQuoteIdentifier(private$conn, dbTableNames),
                                       collapse = ", "), " CASCADE;"))
      private$dropIndex(dbTableNames, ifExists = TRUE)
      flog.info("Database tables: '%s' deleted.", paste(dbTableNames, "', '"))
      return(invisible(self))
    }
    # turn foreign key usage off
    private$db$runQuery("PRAGMA foreign_keys = OFF;")
    for(dbTableName in dbTableNames){
      private$db$runQuery(paste0("DROP TABLE IF EXISTS ",  
                                 dbQuoteIdentifier(private$conn, dbTableName), " ;"))
      private$dropIndex(dbTableName, ifExists = TRUE)
      flog.info("Database table: '%s' deleted.", dbTableName)
    }
    # turn foreign key usage on again
    private$db$runQuery("PRAGMA foreign_keys = ON;")
    return(invisible(self))
  },
  migrateDb = function(migrationConfig, forceRemove = FALSE, callback = NULL){
    oldTableNames <- vapply(migrationConfig, "[[", character(1L), "oldTableName", USE.NAMES = FALSE)
    migrationConfig <- migrationConfig[oldTableNames != "-"]
    oldTableNames <- oldTableNames[oldTableNames != "-"]
    
    if(length(oldTableNames)){
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
        private$renameTable(migrationConfig[[tableToRename]]$oldTableName,
                            paste0("__",  migrationConfig[[tableToRename]]$oldTableName))
        migrationConfig[[tableToRename]]$oldTableName <- paste0("__", migrationConfig[[tableToRename]]$oldTableName)
      }
      
      for(tableToRename in tablesToRename){
        private$renameTable(migrationConfig[[tableToRename]]$oldTableName,
                            tableToRename)
      }
    }
    
    lapply(names(migrationConfig), function(symName){
      # first see if we can take shortcuts
      if(!is.null(callback)){
        callback()
      }
      currentLayout <- private$getTableInfo(symName)
      migrationLayout <- migrationConfig[[symName]]
      
      colsToRemove <- !currentLayout$colNames %in% migrationLayout$colNames
      if(any(colsToRemove) && !forceRemove){
        stop_custom("error_data_loss", "The database migration you specified will lead to loss of data but forceRemove was not set.", call.= FALSE)
      }
      
      newSchema <- dbSchema$getDbSchema(symName)
      dbTableName <- dbSchema$getDbTableName(symName)
      
      if(length(migrationLayout$colNames) == length(currentLayout$colNames[!colsToRemove]) &&
         all(migrationLayout$colNames == currentLayout$colNames[!colsToRemove])){
        if(any(colsToRemove)){
          flog.debug("Removing columns: %s from table: %s",
                     paste(currentLayout$colNames[colsToRemove], collapse = ", "),
                     dbTableName)
          private$dropColumns(dbTableName, currentLayout$colNames[colsToRemove])
        }
        
        currentColNames <- migrationLayout$colNames
        colsToRename <- currentColNames == currentLayout$colNames & currentColNames != newSchema$colNames
        newNamesExist <- colsToRename & currentColNames %in% newSchema$colNames
        
        currentColNamesTmp <- currentColNames
        currentColNamesTmp[newNamesExist] <- paste0("_", currentColNamesTmp[newNamesExist])
        for(colIdToRename in which(newNamesExist)){
          flog.debug("Renaming columns: %s to: %s (table: %s)",
                     paste(currentColNames[colIdToRename], collapse = ", "),
                     paste(currentColNamesTmp[colIdToRename], collapse = ", "),
                     dbTableName)
          private$renameColumn(dbTableName,
                               oldName = currentColNames[colIdToRename],
                               newName = currentColNamesTmp[colIdToRename])
        }
        for(colIdToRename in which(colsToRename)){
          flog.debug("Renaming columns: %s to: %s (table: %s)",
                     paste(currentColNamesTmp[colIdToRename], collapse = ", "),
                     paste(newSchema$colNames[colIdToRename], collapse = ", "),
                     dbTableName)
          private$renameColumn(dbTableName,
                               oldName = currentColNamesTmp[colIdToRename],
                               newName = newSchema$colNames[colIdToRename])
        }
        return()
      }
      
      colsToAdd <- migrationLayout$colNames == "-"
      if(all(colsToAdd)){
        # just remove old table
        flog.debug("Removing table: %s", dbTableName)
        self$removeTablesModel(dbTableName)
        return()
      }
      
      if(any(colsToAdd) &&
         identical(sum(colsToAdd), length(newSchema$colNames) - min(which(colsToAdd)) + 1L)){
        # all columns to add are at the end
        flog.debug("Adding column(s): %s to the end of table: %s",
                   newColNames[colsToAdd], dbTableName)
        private$addColumns(dbTableName,
                           newSchema$colNames[colsToAdd],
                           dbSchema$getColTypesSQL(newSchema$colTypes)[colsToAdd])
        return()
      }
      # seems we are out of luck/no shortcuts
      # need to recreate table and copy data over
      flog.debug("Remapping table: %s", dbTableName)
      private$remapTable(symName, migrationLayout$colNames)
      return()
    })
  }
), private = list(
  conn = NULL,
  db = NULL,
  symNamesScenario = NULL,
  orphanedTables = NULL,
  existingTables = NULL,
  renameIndex = function(oldName, newName){
    if(inherits(private$conn, "PqConnection")){
      query <- SQL(paste0("ALTER INDEX ",
                          DBI::dbQuoteIdentifier(private$conn, paste0("sid_index_", oldName)),
                          " RENAME TO ",
                          DBI::dbQuoteIdentifier(private$conn, paste0("sid_index_", newName))))
      private$db$runQuery(query)
      return(invisible(self))
    }
    private$dropIndex(oldName)
    private$db$runQuery(dbSchema$getCreateIndexQueryRaw(newName))
    return(invisible(self))
  },
  dropIndex = function(dbTableNames, ifExists = FALSE){
    private$db$runQuery(paste0("DROP INDEX ", if(ifExists) "IF EXISTS " else "",
                               dbQuoteIdentifier(private$conn,
                                                 paste(paste0("sid_index_", dbTableNames),
                                                       collapse = ", "))))
    return(invisible(self))
  },
  getRemapTableQuery = function(dbTableName, colMapping){
    sidColName <- DBI::dbQuoteIdentifier(private$conn,
                                         "_sid")
    return(paste0("INSERT INTO ",
                  DBI::dbQuoteIdentifier(private$conn, paste0("_", dbTableName)),
                  " (", sidColName, ", ",
                  paste(vapply(names(colMapping), function(colNameDest){
                    DBI::dbQuoteIdentifier(private$conn, colNameDest)
                  }, character(1L), USE.NAMES = FALSE), collapse = ", "),
                  ") SELECT ", sidColName, ", ",
                  paste(vapply(colMapping, function(colNameOrigin){
                    DBI::dbQuoteIdentifier(private$conn, colNameOrigin)
                  }, character(1L), USE.NAMES = FALSE), collapse = ", "),
                  " FROM ",
                  DBI::dbQuoteIdentifier(private$conn, dbTableName),
                  ";"))
  },
  remapTable = function(tableName, colNames){
    newColNames <- dbSchema$getDbSchema(tableName)$colNames
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
    dbTableName <- dbSchema$getDbTableName(tableName)
    dbWithTransaction(
      private$conn,
      {
        private$db$runQuery(dbSchema$getCreateTableQueryRaw(tableName,
                                                            dbTableName = paste0("_", dbTableName)))
        private$db$runQuery(private$getRemapTableQuery(dbTableName, colMapping))
        private$db$runQuery(paste0("DROP TABLE ",
                                   dbQuoteIdentifier(private$conn, dbTableName), " CASCADE;"))
        private$dropIndex(dbTableName)
        private$db$runQuery(paste0("ALTER TABLE ",
                            DBI::dbQuoteIdentifier(private$conn, paste0("_", dbTableName)), 
                            " RENAME TO ",
                            DBI::dbQuoteIdentifier(private$conn, dbTableName)))
        private$db$runQuery(dbSchema$getCreateIndexQueryRaw(dbTableName))
      }
    )
    return(invisible(self))
  },
  remapTableSQLite = function(tableName, colMapping){
    # this basically implements the steps described on https://sqlite.org/lang_altertable.html
    private$db$runQuery("PRAGMA foreign_keys = OFF;")
    sidColName <- DBI::dbQuoteIdentifier(private$conn,
                                         "_sid")
    dbTableName <- dbSchema$getDbTableName(tableName)
    
    dbWithTransaction(
      private$conn,
      {
        private$db$runQuery(dbSchema$getCreateTableQueryRaw(tableName,
                                                            dbTableName = paste0("_", dbTableName)))
        private$db$runQuery(private$getRemapTableQuery(dbTableName, colMapping))
        private$db$runQuery(paste0("DROP TABLE ",
                                   dbQuoteIdentifier(private$conn, dbTableName)))
        private$dropIndex(dbTableName)
        private$db$runQuery(paste0("ALTER TABLE ",
                                   DBI::dbQuoteIdentifier(private$conn, paste0("_", dbTableName)), 
                                   " RENAME TO ",
                                   DBI::dbQuoteIdentifier(private$conn, dbTableName)))
        private$db$runQuery(dbSchema$getCreateIndexQueryRaw(dbTableName))
        private$db$runQuery("PRAGMA foreign_key_check;")
      }
    )
    private$db$runQuery("PRAGMA foreign_keys = ON;")
    return(invisible(self))
  },
  renameColumn = function(dbTableName, oldName, newName){
    private$db$runQuery(paste0("ALTER TABLE ",
                               DBI::dbQuoteIdentifier(private$conn,
                                                      dbTableName), 
                               " RENAME COLUMN ",
                               DBI::dbQuoteIdentifier(private$conn, oldName), "  TO ",
                               DBI::dbQuoteIdentifier(private$conn, newName)))
    return(invisible(self))
  },
  addColumns = function(dbTableName, colNames, colTypes){
    if(inherits(private$conn, "PqConnection")){
      private$db$runQuery(paste0("ALTER TABLE ",
                                 DBI::dbQuoteIdentifier(private$conn, dbTableName),
                                 paste(" ADD COLUMN",
                                       DBI::dbQuoteIdentifier(private$conn, colNames),
                                       colTypes, collapse = ",")))
      return(invisible(self))
    }
    for(colIdx in seq_along(colNames)){
      private$db$runQuery(paste("ALTER TABLE",
                                DBI::dbQuoteIdentifier(private$conn, tableName),
                                "ADD COLUMN",
                                DBI::dbQuoteIdentifier(private$conn, colNames[colIdx]),
                                colTypes[colIdx]))
    }
    return(invisible(self))
  },
  dropColumns = function(dbTableName, colNames){
    if(inherits(private$conn, "PqConnection")){
      private$db$runQuery(paste0("ALTER TABLE ",
                                 DBI::dbQuoteIdentifier(private$conn, dbTableName), 
                                 paste(" DROP COLUMN",
                                       DBI::dbQuoteIdentifier(private$conn, colNames),
                                       collapse = ",")))
      return(invisible(self))
    }
  },
  renameTable = function(oldName, newName){
    private$db$runQuery(paste0("ALTER TABLE ",
                               DBI::dbQuoteIdentifier(private$conn, oldName), 
                               " RENAME TO ",
                               DBI::dbQuoteIdentifier(private$conn, newName)))
    private$renameIndex(oldName, newName)
    return(invisible(self))
  },
  validateMigrationConfig = function(migrationConfig){
    oldTableNames <- vapply(migrationConfig, "[[", character(1L), "oldTableName", USE.NAMES = FALSE)
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
      if(!names(migrationConfig)[i] %in% private$symNamesScenario){
        stop_custom("error_config", sprintf("Invalid migration config: table: %s does not exist in db schema",
                                            names(migrationConfig)[i]),
                    call. = FALSE)
      }
      newColNames <- dbSchema$getDbSchema(names(migrationConfig)[i])$colNames
      if(!identical(length(migrationConfig[[i]]$colNames),
                    length(newColNames))){
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
    dbTableNames <- self$getDbTableNamesModel()
    private$existingTables <- dbTableNames
    orphanedTables <- dbTableNames[!dbTableNames %in% dbSchema$getTableNamesCurrentSchema()]
    if(!is.null(hcubeScalars)){
      orphanedTables <- orphanedTables[!orphanedTables %in% hcubeScalars]
    }
    return(orphanedTables)
  },
  getTableInfo = function(dbTableName){
    if(inherits(private$conn, "PqConnection")){
      query <- SQL(paste0("SELECT ordinal_position,column_name,data_type FROM information_schema.columns WHERE table_name = ", 
                          dbQuoteString(private$conn, dbTableName),
                          " ORDER BY ordinal_position;"))
      tabInfo     <- dbGetQuery(private$conn, query)
      return(list(colNames = tabInfo$column_name[-1], colTypes = tabInfo$data_type[-1]))
    }
    query <- SQL(paste0("PRAGMA table_info(", 
                        dbQuoteIdentifier(private$conn, dbTableName), ");"))
    tabInfo     <- dbGetQuery(private$conn, query)
    return(list(colNames = tabInfo$name[-1], colTypes = tabInfo$type[-1]))
  }
))
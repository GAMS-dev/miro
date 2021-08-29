DbMigrator <- R6::R6Class("DbMigrator", public = list(
  initialize = function(db) {
    private$db <- db
    private$conn <- db$getConn()
    private$dbTableNames <- dbSchema$getDbTableNames()
    private$orphanedTables <- private$getOrphanedTablesInternal(c(
      "_scalars",
      ioConfig$hcubeScalars
    ))
    private$newTables <- private$dbTableNames[!private$dbTableNames %in% private$existingTables]
    return(invisible(self))
  },
  getOrphanedTablesInfo = function() {
    tabInfo <- lapply(private$orphanedTables, function(dbTableName) {
      return(private$getTableInfo(dbTableName))
    })
    names(tabInfo) <- private$orphanedTables
    return(tabInfo)
  },
  getInconsistentTablesInfo = function() {
    badTables <- lapply(private$dbTableNames, function(symName) {
      tabInfo <- dbSchema$getDbSchema(symName)
      confHeaders <- tabInfo$colNames
      dbTableName <- tabInfo$tabName
      if (!is.null(confHeaders)) {
        if (!dbExistsTable(private$conn, dbTableName)) {
          return(tabInfo)
        }
        currentTabInfo <- private$getTableInfo(dbTableName)
        tabColNames <- currentTabInfo$colNames
        tabColTypes <- currentTabInfo$colTypes
        if (!identical(colTypeVectorToString(tabColTypes), tabInfo$colTypes) ||
          any(confHeaders != tabColNames)) {
          return(c(tabInfo, list(
            currentColNames = tabColNames,
            currentColTypes = tabColTypes
          )))
        }
      }
      return(NA)
    })
    return(badTables[!is.na(badTables)])
  },
  createMissingScalarTables = function(missingViewNames = character()) {
    scalarViews <- dbSchema$getDbViews()
    for (scalarViewName in names(scalarViews)) {
      newScalarTables <- scalarViews[[scalarViewName]] %in% private$newTables
      if (any(newScalarTables)) {
        if (!dbSchema$getDbTableName("_scenMeta") %in% private$existingTables) {
          private$db$runQuery(dbSchema$getCreateTableQuery("_scenMeta"))
          private$existingTables <- c(
            private$existingTables,
            dbSchema$getDbTableName("_scenMeta")
          )
        }
        for (newScalarTable in scalarViews[[scalarViewName]][newScalarTables]) {
          private$db$runQuery(dbSchema$getCreateTableQueryRaw(newScalarTable,
            dbTableName = newScalarTable
          ))
          private$db$runQuery(dbSchema$getCreateIndexQueryRaw(newScalarTable))
          private$existingTables <- c(private$existingTables, newScalarTable)
        }
        private$dropScalarTableViews(scalarViewName)
        private$updateScalarTableViews(scalarViewName, scalarViews[[scalarViewName]])
      } else if (scalarViewName %in% missingViewNames) {
        private$updateScalarTableViews(scalarViewName, scalarViews[[scalarViewName]])
      }
    }
    return(invisible(self))
  },
  getDbTableNamesModel = function() {
    if (inherits(private$conn, "PqConnection")) {
      dbSchemaName <- private$db$getInfo()$schema
      if (!length(dbSchemaName)) {
        dbSchemaName <- "public"
      }
      query <- paste0(
        "SELECT table_name FROM information_schema.tables WHERE table_schema=",
        dbQuoteString(private$conn, dbSchemaName),
        " AND table_type='BASE TABLE';"
      )
    } else {
      query <- paste0("SELECT name FROM sqlite_master WHERE type = 'table';")
    }
    return(dbGetQuery(private$conn, SQL(query))[[1L]])
  },
  backupDatabase = function(fileName) {
    if (inherits(private$conn, "PqConnection")) {
      stop("Not implemented!", call. = FALSE)
    }
    private$db$runQuery(paste0("VACUUM INTO ", dbQuoteString(private$conn, fileName)))
    flog.info("Database backed up to: '%s'", fileName)
    return(invisible(self))
  },
  removeTablesModel = function(dbTableNames = NULL) {
    stopifnot(is.null(dbTableNames) || is.character(dbTableNames))

    if (length(dbTableNames)) {
      removeAllTables <- FALSE
    } else {
      removeAllTables <- TRUE
      dbTableNames <- self$getDbTableNamesModel()
    }

    if (inherits(private$conn, "PqConnection")) {
      private$db$runQuery(paste0(
        "DROP TABLE IF EXISTS ",
        paste(dbQuoteIdentifier(private$conn, dbTableNames),
          collapse = ", "
        ), " CASCADE;"
      ))
      flog.info("Database tables: '%s' deleted.", paste(dbTableNames, collapse = "', '"))
      return(invisible(self))
    }
    private$db$runQuery("PRAGMA foreign_keys = OFF;")
    for (dbTableName in dbTableNames) {
      private$db$runQuery(paste0(
        "DROP TABLE IF EXISTS ",
        dbQuoteIdentifier(private$conn, dbTableName), " ;"
      ))
      flog.info("Database table: '%s' deleted.", dbTableName)
    }
    private$db$runQuery("PRAGMA foreign_keys = ON;")
    return(invisible(self))
  },
  migrateDb = function(migrationConfig, forceRemove = FALSE, callback = NULL) {
    oldTableNames <- vapply(migrationConfig, "[[", character(1L), "oldTableName", USE.NAMES = FALSE)
    migrationConfig <- migrationConfig[oldTableNames != "-"]
    oldTableNames <- oldTableNames[oldTableNames != "-"]

    if (length(oldTableNames)) {
      private$validateMigrationConfig(migrationConfig)
      newTableNames <- names(migrationConfig)
    }

    tablesToRemove <- !private$orphanedTables %in% oldTableNames

    # drop views in case scalar tables get dropped/modified
    # (will be recreated at the end of the migration process)
    # TODO: Do this only if scalar tables have changed
    for (scalarViewName in c(scalarsFileName, scalarsOutName)) {
      private$dropScalarTableViews(scalarViewName)
    }
    if (any(tablesToRemove)) {
      if (!forceRemove) {
        stop_custom("error_data_loss", "The database migration you specified will lead to loss of data but forceRemove was not set.", call. = FALSE)
      }
      self$removeTablesModel(private$orphanedTables[tablesToRemove])
      private$orphanedTables <- private$orphanedTables[!tablesToRemove]
    }
    if (length(oldTableNames)) {
      tablesToRename <- newTableNames[newTableNames != oldTableNames]

      newTableNamesExist <- tablesToRename %in% private$existingTables

      if (any(!tablesToRename[newTableNamesExist] %in% oldTableNames)) {
        stop_custom("error_config", sprintf(
          "Invalid migration config: Can't rename table(s): %s as they already exist",
          tablesToRename[newTableNamesExist]
        ),
        call. = FALSE
        )
      }

      for (tableToRename in tablesToRename[newTableNamesExist]) {
        private$renameTable(
          migrationConfig[[tableToRename]]$oldTableName,
          paste0("__", migrationConfig[[tableToRename]]$oldTableName)
        )
        migrationConfig[[tableToRename]]$oldTableName <- paste0("__", migrationConfig[[tableToRename]]$oldTableName)
      }

      for (tableToRename in tablesToRename) {
        private$renameTable(
          migrationConfig[[tableToRename]]$oldTableName,
          tableToRename
        )
      }
    }
    scalarViews <- dbSchema$getDbViews()

    lapply(names(migrationConfig), function(symName) {
      # first see if we can take shortcuts
      if (!is.null(callback)) {
        callback()
      }
      currentLayout <- private$getTableInfo(symName)
      migrationLayout <- migrationConfig[[symName]]

      colsToRemove <- !currentLayout$colNames %in% migrationLayout$colNames
      if (any(colsToRemove) && !forceRemove) {
        stop_custom("error_data_loss", "The database migration you specified will lead to loss of data but forceRemove was not set.", call. = FALSE)
      }

      newSchema <- dbSchema$getDbSchema(symName)
      dbTableName <- dbSchema$getDbTableName(symName)

      if (!inherits(private$conn, "PqConnection") && any(colsToRemove)) {
        # since sqlite does not support dropping columns, we need to remap table anyway
        flog.debug("Remapping table: %s", dbTableName)
        private$remapTable(symName, migrationLayout$colNames)
        return()
      }
      if (length(migrationLayout$colNames) == length(currentLayout$colNames[!colsToRemove]) &&
        all(migrationLayout$colNames == currentLayout$colNames[!colsToRemove])) {
        if (any(colsToRemove)) {
          flog.debug(
            "Removing columns: %s from table: %s",
            paste(currentLayout$colNames[colsToRemove], collapse = ", "),
            dbTableName
          )
          private$dropColumns(dbTableName, currentLayout$colNames[colsToRemove])
          currentLayout$colNames <- currentLayout$colNames[!colsToRemove]
          currentLayout$colTypes <- currentLayout$colTypes[!colsToRemove]
        }

        currentColNames <- migrationLayout$colNames
        colsToRename <- currentColNames == currentLayout$colNames & currentColNames != newSchema$colNames
        newNamesExist <- colsToRename & currentColNames %in% newSchema$colNames

        currentColNamesTmp <- currentColNames
        currentColNamesTmp[newNamesExist] <- paste0("_", currentColNamesTmp[newNamesExist])
        for (colIdToRename in which(newNamesExist)) {
          flog.debug(
            "Renaming columns: %s to: %s (table: %s)",
            paste(currentColNames[colIdToRename], collapse = ", "),
            paste(currentColNamesTmp[colIdToRename], collapse = ", "),
            dbTableName
          )
          private$renameColumn(dbTableName,
            oldName = currentColNames[colIdToRename],
            newName = currentColNamesTmp[colIdToRename]
          )
        }
        for (colIdToRename in which(colsToRename)) {
          flog.debug(
            "Renaming columns: %s to: %s (table: %s)",
            paste(currentColNamesTmp[colIdToRename], collapse = ", "),
            paste(newSchema$colNames[colIdToRename], collapse = ", "),
            dbTableName
          )
          private$renameColumn(dbTableName,
            oldName = currentColNamesTmp[colIdToRename],
            newName = newSchema$colNames[colIdToRename]
          )
        }
        newColTypes <- tolower(dbSchema$getColTypesSQL(newSchema$colTypes))
        if (any(tolower(currentLayout$colTypes) != newColTypes)) {
          if (symName %in% unlist(scalarViews, use.names = FALSE)) {
            private$typecastScalarTable(dbTableName, newColTypes[1])
          } else {
            stop("Converting column types not supported for non-scalar tables.", call. = FALSE)
          }
        }
        return()
      }

      colsToAdd <- migrationLayout$colNames == "-"
      if (all(colsToAdd)) {
        # just remove old table
        flog.debug("Removing table: %s", dbTableName)
        self$removeTablesModel(dbTableName)
        return()
      }

      if (any(colsToAdd) &&
        identical(sum(colsToAdd), length(newSchema$colNames) - min(which(colsToAdd)) + 1L)) {
        # all columns to add are at the end
        if (any(colsToRemove)) {
          flog.debug(
            "Removing columns: %s from table: %s",
            paste(currentLayout$colNames[colsToRemove], collapse = ", "),
            dbTableName
          )
          private$dropColumns(dbTableName, currentLayout$colNames[colsToRemove])
        }
        flog.debug(
          "Adding column(s): %s to the end of table: %s",
          newSchema$colNames[colsToAdd], dbTableName
        )
        private$addColumns(
          dbTableName,
          newSchema$colNames[colsToAdd],
          dbSchema$getColTypesSQL(newSchema$colTypes)[colsToAdd]
        )
        return()
      }
      # seems we are out of luck/no shortcuts
      # need to recreate table and copy data over
      flog.debug("Remapping table: %s", dbTableName)
      private$remapTable(symName, migrationLayout$colNames)
      return()
    })
    # make sure scalar views are updated
    # TODO: Do this only if scalar tables have changed
    private$existingTables <- self$getDbTableNamesModel()
    private$newTables <- private$dbTableNames[!private$dbTableNames %in% private$existingTables]
    self$createMissingScalarTables(names(dbSchema$getDbViews()))
    return(invisible(self))
  }
), private = list(
  conn = NULL,
  db = NULL,
  dbTableNames = NULL,
  orphanedTables = NULL,
  existingTables = NULL,
  newTables = NULL,
  typecastScalarTable = function(tableName, newType = "text") {
    if (inherits(private$conn, "PqConnection")) {
      escapedTableName <- dbQuoteIdentifier(private$conn, tableName)
      private$db$runQuery(paste0(
        "ALTER TABLE ", escapedTableName,
        " ALTER COLUMN ", escapedTableName,
        " TYPE ", newType, " USING CAST(",
        escapedTableName, " AS ",
        newType, ");"
      ))
      return(invisible(self))
    }
    return(private$remapTable(tableName, tableName, castAs = newType))
  },
  renameIndex = function(oldName, newName) {
    if (inherits(private$conn, "PqConnection")) {
      query <- SQL(paste0(
        "ALTER INDEX ",
        DBI::dbQuoteIdentifier(private$conn, paste0("sid_index_", oldName)),
        " RENAME TO ",
        DBI::dbQuoteIdentifier(private$conn, paste0("sid_index_", newName))
      ))
      private$db$runQuery(query)
      return(invisible(self))
    }
    private$dropIndex(oldName)
    private$db$runQuery(dbSchema$getCreateIndexQueryRaw(newName))
    return(invisible(self))
  },
  dropIndex = function(dbTableNames, ifExists = FALSE) {
    private$db$runQuery(paste0(
      "DROP INDEX ", if (ifExists) "IF EXISTS " else "",
      dbQuoteIdentifier(
        private$conn,
        paste(paste0("sid_index_", dbTableNames),
          collapse = ", "
        )
      )
    ))
    return(invisible(self))
  },
  getRemapTableQuery = function(dbTableName, colMapping, castAs = NULL) {
    sidColName <- DBI::dbQuoteIdentifier(
      private$conn,
      "_sid"
    )
    return(paste0(
      "INSERT INTO ",
      DBI::dbQuoteIdentifier(private$conn, paste0("_", dbTableName)),
      " (", sidColName, ", ",
      paste(vapply(names(colMapping), function(colNameDest) {
        DBI::dbQuoteIdentifier(private$conn, colNameDest)
      }, character(1L), USE.NAMES = FALSE), collapse = ", "),
      ") SELECT ", sidColName, ", ",
      paste(vapply(colMapping, function(colNameOrigin) {
        if (!is.null(castAs)) {
          return(paste0(
            "CAST(",
            DBI::dbQuoteIdentifier(private$conn, colNameOrigin),
            " AS ", castAs, ")"
          ))
        }
        return(DBI::dbQuoteIdentifier(private$conn, colNameOrigin))
      }, character(1L), USE.NAMES = FALSE), collapse = ", "),
      " FROM ",
      DBI::dbQuoteIdentifier(private$conn, dbTableName),
      ";"
    ))
  },
  remapTable = function(tableName, colNames, castAs = NULL) {
    newColNames <- dbSchema$getDbSchema(tableName)$colNames
    if (!identical(length(newColNames), length(colNames))) {
      stop_custom("error_config",
        sprintf(
          "Length of column names do not match. New columns: %s. Old columns: %s",
          paste(newColNames, collapse = ", "),
          paste(colNames, collapse = ", ")
        ),
        call. = FALSE
      )
    }
    colMapping <- colNames
    names(colMapping) <- newColNames
    colMapping <- colMapping[colMapping != "-"]

    if (!length(colMapping)) {
      dbTableName <- dbSchema$getDbTableName(tableName)
      private$db$runQuery(paste0(
        "DROP TABLE IF EXISTS ",
        dbQuoteIdentifier(
          private$conn,
          dbTableName
        ), " ;"
      ))
      flog.info("Database table: '%s' deleted.", dbTableName)
      return(invisible(self))
    }

    if (inherits(private$conn, "PqConnection")) {
      return(private$remapTablePostgres(tableName, colMapping))
    }
    return(private$remapTableSQLite(tableName, colMapping, castAs = castAs))
  },
  remapTablePostgres = function(tableName, colMapping) {
    dbTableName <- dbSchema$getDbTableName(tableName)
    dbWithTransaction(
      private$conn,
      {
        private$db$runQuery(dbSchema$getCreateTableQueryRaw(tableName,
          dbTableName = paste0("_", dbTableName)
        ))
        private$db$runQuery(private$getRemapTableQuery(dbTableName, colMapping))
        private$db$runQuery(paste0(
          "DROP TABLE ",
          dbQuoteIdentifier(private$conn, dbTableName), " CASCADE;"
        ))
        private$db$runQuery(paste0(
          "ALTER TABLE ",
          DBI::dbQuoteIdentifier(private$conn, paste0("_", dbTableName)),
          " RENAME TO ",
          DBI::dbQuoteIdentifier(private$conn, dbTableName)
        ))
        private$db$runQuery(dbSchema$getCreateIndexQueryRaw(dbTableName))
      }
    )
    return(invisible(self))
  },
  remapTableSQLite = function(tableName, colMapping, castAs = NULL) {
    # this basically implements the steps described on https://sqlite.org/lang_altertable.html
    private$db$runQuery("PRAGMA foreign_keys = OFF;")
    sidColName <- DBI::dbQuoteIdentifier(
      private$conn,
      "_sid"
    )
    dbTableName <- dbSchema$getDbTableName(tableName)

    dbWithTransaction(
      private$conn,
      {
        private$db$runQuery(dbSchema$getCreateTableQueryRaw(tableName,
          dbTableName = paste0("_", dbTableName)
        ))
        private$db$runQuery(private$getRemapTableQuery(dbTableName, colMapping, castAs = castAs))
        private$db$runQuery(paste0(
          "DROP TABLE ",
          dbQuoteIdentifier(private$conn, dbTableName)
        ))
        private$db$runQuery(paste0(
          "ALTER TABLE ",
          DBI::dbQuoteIdentifier(private$conn, paste0("_", dbTableName)),
          " RENAME TO ",
          DBI::dbQuoteIdentifier(private$conn, dbTableName)
        ))
        private$db$runQuery(dbSchema$getCreateIndexQueryRaw(dbTableName))
        private$db$runQuery("PRAGMA foreign_key_check;")
      }
    )
    private$db$runQuery("PRAGMA foreign_keys = ON;")
    return(invisible(self))
  },
  renameColumn = function(dbTableName, oldName, newName) {
    private$db$runQuery(paste0(
      "ALTER TABLE ",
      DBI::dbQuoteIdentifier(
        private$conn,
        dbTableName
      ),
      " RENAME COLUMN ",
      DBI::dbQuoteIdentifier(private$conn, oldName), "  TO ",
      DBI::dbQuoteIdentifier(private$conn, newName)
    ))
    return(invisible(self))
  },
  addColumns = function(dbTableName, colNames, colTypes) {
    if (inherits(private$conn, "PqConnection")) {
      private$db$runQuery(paste0(
        "ALTER TABLE ",
        DBI::dbQuoteIdentifier(private$conn, dbTableName),
        paste(" ADD COLUMN",
          DBI::dbQuoteIdentifier(private$conn, colNames),
          colTypes,
          collapse = ","
        )
      ))
      return(invisible(self))
    }
    for (colIdx in seq_along(colNames)) {
      private$db$runQuery(paste(
        "ALTER TABLE",
        DBI::dbQuoteIdentifier(private$conn, dbTableName),
        "ADD COLUMN",
        DBI::dbQuoteIdentifier(private$conn, colNames[colIdx]),
        colTypes[colIdx]
      ))
    }
    return(invisible(self))
  },
  dropColumns = function(dbTableName, colNames) {
    if (inherits(private$conn, "PqConnection")) {
      private$db$runQuery(paste0(
        "ALTER TABLE ",
        DBI::dbQuoteIdentifier(private$conn, dbTableName),
        paste(" DROP COLUMN",
          DBI::dbQuoteIdentifier(private$conn, colNames),
          collapse = ","
        )
      ))
      return(invisible(self))
    }
    stop("dropping columns not supported in sqlite", call. = FALSE)
  },
  renameTable = function(oldName, newName) {
    private$db$runQuery(paste0(
      "ALTER TABLE ",
      DBI::dbQuoteIdentifier(private$conn, oldName),
      " RENAME TO ",
      DBI::dbQuoteIdentifier(private$conn, newName)
    ))
    private$renameIndex(oldName, newName)
    return(invisible(self))
  },
  validateMigrationConfig = function(migrationConfig) {
    oldTableNames <- vapply(migrationConfig, "[[", character(1L), "oldTableName", USE.NAMES = FALSE)
    invalidOldTableNames <- !oldTableNames %in% private$existingTables
    if (any(invalidOldTableNames)) {
      stop_custom("error_config", sprintf(
        "Invalid migration config: (old) table(s): %s do not exist in database",
        paste(oldTableNames[invalidOldTableNames], collapse = ", ")
      ),
      call. = FALSE
      )
    }
    duplicatedOldTableNames <- duplicated(oldTableNames)
    if (any(duplicatedOldTableNames)) {
      stop_custom("error_config", "Invalid migration config: Can't duplicate a table",
        call. = FALSE
      )
    }
    for (i in seq_along(migrationConfig)) {
      if (!names(migrationConfig)[i] %in% private$dbTableNames) {
        stop_custom("error_config", sprintf(
          "Invalid migration config: table: %s does not exist in db schema",
          names(migrationConfig)[i]
        ),
        call. = FALSE
        )
      }
      newColNames <- dbSchema$getDbSchema(names(migrationConfig)[i])$colNames
      if (!identical(
        length(migrationConfig[[i]]$colNames),
        length(newColNames)
      )) {
        stop_custom("error_config", sprintf(
          "Invalid migration config: Length of column names invalid for table: %s.",
          names(migrationConfig)[i]
        ),
        call. = FALSE
        )
      }
    }
    return(invisible(self))
  },
  getOrphanedTablesInternal = function(hcubeScalars = NULL) {
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
    orphanedTables <- orphanedTables[!startsWith(orphanedTables, "_hc_")]
    return(orphanedTables)
  },
  getTableInfo = function(dbTableName) {
    if (inherits(private$conn, "PqConnection")) {
      query <- SQL(paste0(
        "SELECT ordinal_position,column_name,data_type FROM information_schema.columns WHERE table_name =",
        dbQuoteString(private$conn, dbTableName),
        " AND table_schema=",
        dbQuoteString(private$conn, private$db$getInfo()$schema),
        "ORDER BY ordinal_position;"
      ))
      tabInfo <- dbGetQuery(private$conn, query)
      return(list(colNames = tabInfo$column_name[-1], colTypes = tabInfo$data_type[-1]))
    }
    query <- SQL(paste0(
      "PRAGMA table_info(",
      dbQuoteIdentifier(private$conn, dbTableName), ");"
    ))
    tabInfo <- dbGetQuery(private$conn, query)
    return(list(colNames = tabInfo$name[-1], colTypes = tabInfo$type[-1]))
  },
  dropScalarTableViews = function(tableName) {
    private$db$runQuery(dbSchema$getDropScalarTriggerQuery(tableName))
    private$db$runQuery(SQL(paste0(
      "DROP VIEW IF EXISTS ",
      dbQuoteIdentifier(private$conn, tableName)
    )))
    return(invisible(self))
  },
  updateScalarTableViews = function(tableName, scalars) {
    private$db$runQuery(dbSchema$getCreateScalarViewQuery(tableName, scalars))
    if (inherits(private$conn, "PqConnection")) {
      private$db$runQuery(dbSchema$getCreateScalarViewTriggerFnQuery(tableName, scalars))
    }
    private$db$runQuery(dbSchema$getCreateScalarViewTriggerQuery(tableName, scalars))
    return(invisible(self))
  }
))

migrateMiroDatabase <- function(oldPath, newPath) {
  flog.info("Migrating your MIRO database to new schema...")
  isWindows <- identical(tolower(Sys.info()[["sysname"]]), "windows")
  if (isWindows) {
    pb <- winProgressBar(
      title = "Migrating MIRO database", label = "Preparing migration",
      min = 0, max = 1, initial = 0, width = 300
    )
  } else {
    pb <- txtProgressBar(min = 0, max = 1, initial = 0)
  }
  on.exit(close(pb))
  tableBelongsToModel <- function(dbTableName, appId) {
    return(startsWith(dbTableName, paste0(gsub("_", "", appId, fixed = TRUE), "_")) ||
      (startsWith(dbTableName, "_sys_") &&
        endsWith(dbTableName, paste0("_", appId))))
  }

  conn <- DBI::dbConnect(
    drv = RSQLite::SQLite(),
    dbname = oldPath, bigint = "integer"
  )
  tryCatch(
    {
      allDbTables <- dbGetQuery(
        conn,
        SQL("SELECT name FROM sqlite_master WHERE type = 'table';")
      )[[1L]]
    },
    finally = {
      DBI::dbDisconnect(conn)
    }
  )
  # scalars need to be migrated last..
  isScalarTable <- endsWith(allDbTables, "_scalars") | endsWith(allDbTables, "_scalars_out")
  allDbTables <- c(allDbTables[!isScalarTable], allDbTables[isScalarTable])
  appIdsWithData <- vapply(allDbTables,
    function(dbTableName) {
      if (!startsWith(dbTableName, "_sys_metadata_")) {
        return(NA_character_)
      }
      return(substring(dbTableName, nchar("_sys_metadata_") + 1L))
    }, character(1L),
    USE.NAMES = FALSE
  )
  appIdsWithData <- appIdsWithData[!is.na(appIdsWithData)]
  i <- 1L
  for (appIdWithData in appIdsWithData) {
    if (isWindows) {
      setWinProgressBar(pb, i / (length(appIdsWithData) + 1L),
        label = sprintf("Migrating data for app: '%s'", appIdWithData)
      )
    } else {
      setTxtProgressBar(pb, value = i / (length(appIdsWithData) + 1L))
    }
    i <- i + 1L
    newPathFull <- file.path(newPath, paste0(appIdWithData, ".sqlite3"))
    if (file.exists(newPathFull)) {
      flog.warn(
        "Could not migrate data for app: '%s' as the file: '%s' already exists",
        appIdWithData, newPathFull
      )
      next
    }
    if (!file.copy(oldPath, newPathFull)) {
      stop(sprintf(
        "Could not copy: '%s' to: '%s'. Check access permissions.",
        oldPath, newPathFull
      ), call. = FALSE)
    }
    tablesBelongToModel <- vapply(allDbTables, tableBelongsToModel,
      logical(1L),
      USE.NAMES = FALSE, appIdWithData
    )
    conn <- DBI::dbConnect(
      drv = RSQLite::SQLite(),
      dbname = newPathFull, bigint = "integer"
    )
    tryCatch(
      {
        dbExecute(conn, SQL("PRAGMA foreign_keys = ON;"))
        for (dbTableToRemove in allDbTables[!tablesBelongToModel]) {
          dbExecute(conn, SQL(paste0("DROP TABLE IF EXISTS ", dbQuoteIdentifier(conn, dbTableToRemove), " ;")))
        }
        appTablePrefixLen <- nchar(gsub("_", "", appIdWithData, fixed = TRUE)) + 1L
        for (dbTableToRename in allDbTables[tablesBelongToModel]) {
          if (startsWith(dbTableToRename, "_sys_") &&
            endsWith(dbTableToRename, paste0("_", appIdWithData))) {
            newTableName <- substring(
              dbTableToRename, 1L,
              nchar(dbTableToRename) - nchar(appIdWithData)
            )
          } else {
            newTableName <- substring(dbTableToRename, appTablePrefixLen + 1L, nchar(dbTableToRename))
          }
          if (newTableName %in% c("_scalars", "_scalars_out")) {
            scalarsToMigrate <- dbGetQuery(
              conn,
              paste0(
                "SELECT DISTINCT scalar FROM ",
                dbQuoteIdentifier(conn, dbTableToRename)
              )
            )
            if (!length(scalarsToMigrate) || !length(scalarsToMigrate[[1]])) {
              dbExecute(conn, SQL(paste0("DROP TABLE IF EXISTS ", dbQuoteIdentifier(conn, dbTableToRename), " ;")))
              next
            }
            sidsHcConfig <- dbGetQuery(conn, paste0(
              "SELECT _sid FROM ",
              dbQuoteIdentifier(conn, "_sys_metadata_"),
              " WHERE _scode=-1"
            ))
            if (length(sidsHcConfig) && length(sidsHcConfig[[1]]) && !dbExistsTable(conn, "_hc__scalars")) {
              # copy Hypercube scalars to new _hc__scalars table
              dbExecute(
                conn,
                "CREATE TABLE _hc__scalars (_sid INTEGER,scalar TEXT,description TEXT,value TEXT, CONSTRAINT foreign_key FOREIGN KEY (_sid) REFERENCES _sys_metadata_(_sid) ON DELETE CASCADE);"
              )
              dbExecute(conn, SQL(paste0(
                "INSERT INTO _hc__scalars (_sid,scalar,description,value) SELECT _sid,scalar,description,value FROM ",
                dbQuoteIdentifier(conn, dbTableToRename), " WHERE _sid IN (",
                paste(DBI::dbQuoteLiteral(conn, sidsHcConfig[[1]]), collapse = ","), ")"
              )))
            }
            scalarsToMigrate <- gsub("$", "_", scalarsToMigrate[[1]], fixed = TRUE)
            for (scalarTableName in scalarsToMigrate) {
              if (dbExistsTable(conn, scalarTableName)) {
                # is Hypercube scalar table
                dbExecute(conn, SQL(paste0(
                  "ALTER TABLE ",
                  DBI::dbQuoteIdentifier(conn, scalarTableName),
                  " RENAME TO ",
                  DBI::dbQuoteIdentifier(conn, paste0("_hc_", scalarTableName))
                )))
                dbExecute(conn, SQL(paste0(
                  "DROP INDEX ",
                  DBI::dbQuoteIdentifier(conn, paste0("sid_index_", scalarTableName))
                )))
                dbExecute(conn, paste0(
                  "CREATE INDEX ",
                  dbQuoteIdentifier(conn, paste0("sid_index__hc_", scalarTableName)),
                  " ON ",
                  dbQuoteIdentifier(conn, paste0("_hc_", scalarTableName)),
                  " (",
                  dbQuoteIdentifier(conn, "_sid"),
                  ");"
                ))
              }
              dbExecute(
                conn,
                paste0(
                  "CREATE TABLE ",
                  dbQuoteIdentifier(conn, scalarTableName),
                  " (_sid INTEGER,", dbQuoteIdentifier(conn, scalarTableName),
                  " TEXT, CONSTRAINT foreign_key FOREIGN KEY (_sid) REFERENCES ",
                  "_sys_metadata_(_sid) ON DELETE CASCADE);"
                )
              )
              dbExecute(conn, paste0(
                "CREATE INDEX ",
                dbQuoteIdentifier(conn, paste0("sid_index_", scalarTableName)),
                " ON ",
                dbQuoteIdentifier(conn, scalarTableName),
                " (",
                dbQuoteIdentifier(conn, "_sid"),
                ");"
              ))
              dbExecute(conn, SQL(paste0(
                "INSERT INTO ", dbQuoteIdentifier(conn, scalarTableName),
                " (_sid,", dbQuoteIdentifier(conn, scalarTableName),
                ") SELECT _sid,value FROM ",
                dbQuoteIdentifier(conn, dbTableToRename), " WHERE scalar=",
                dbQuoteString(conn, scalarTableName)
              )))
            }
            dbExecute(conn, SQL(paste0("DROP TABLE IF EXISTS ", dbQuoteIdentifier(conn, dbTableToRename), " ;")))
            escapedScalarNames <- DBI::dbQuoteIdentifier(conn, scalarsToMigrate)
            dbExecute(conn, paste0(
              "CREATE VIEW ", dbQuoteIdentifier(conn, newTableName), " AS SELECT ",
              "_sys_metadata_._sid,",
              paste(escapedScalarNames, collapse = ","), " FROM _sys_metadata_ ",
              paste(paste0(
                "LEFT JOIN ", escapedScalarNames, " ON ",
                "_sys_metadata_._sid=", escapedScalarNames, "._sid"
              ),
              collapse = " "
              )
            ))
            dbExecute(conn, paste0(
              "CREATE TRIGGER ", dbQuoteIdentifier(conn, paste0(newTableName, "_insert")),
              " INSTEAD OF INSERT ON ", dbQuoteIdentifier(conn, newTableName),
              " BEGIN ", paste(paste0(
                "INSERT INTO", escapedScalarNames, "(_sid,",
                escapedScalarNames, ") VALUES (NEW._sid,NEW.",
                escapedScalarNames, ");"
              ), collapse = " "), " END"
            ))
            next
          }
          dbExecute(conn, SQL(paste0(
            "ALTER TABLE ",
            DBI::dbQuoteIdentifier(conn, dbTableToRename),
            " RENAME TO ",
            DBI::dbQuoteIdentifier(conn, newTableName)
          )))
          dbExecute(conn, paste0(
            "DROP INDEX IF EXISTS ",
            dbQuoteIdentifier(conn, paste0("sid_index_", dbTableToRename))
          ))
          dbExecute(conn, paste0(
            "CREATE INDEX ",
            dbQuoteIdentifier(conn, paste0("sid_index_", newTableName)),
            " ON ",
            dbQuoteIdentifier(conn, newTableName),
            " (",
            dbQuoteIdentifier(conn, "_sid"),
            ");"
          ))
        }
        dbExecute(conn, "VACUUM")
      },
      finally = {
        DBI::dbDisconnect(conn)
      }
    )
  }
  if (file.move(oldPath, file.path(dirname(oldPath), "bk_miro.sqlite3"))) {
    on.exit(flog.info("Backup of old database was stored in: '%s'. Delete manually to free space.", file.path(dirname(oldPath), "bk_miro.sqlite3")),
      add = TRUE
    )
  } else {
    on.exit(flog.warn("Could not move: '%s' to: '%s", oldPath, file.path(dirname(oldPath), "bk_miro.sqlite3")),
      add = TRUE
    )
  }
}

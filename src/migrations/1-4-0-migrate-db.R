migrateMiroDatabase <- function(oldPath, newPath){
  flog.info("Migrating your MIRO database to new schema...")
  isWindows <- identical(tolower(Sys.info()[["sysname"]]), "windows")
  if(isWindows){
    pb <- winProgressBar(title = "Migrating MIRO database", label = "Preparing migration",
                         min = 0, max = 1, initial = 0, width = 300)
  }else{
    pb <- txtProgressBar(min = 0, max = 1, initial = 0)
  }
  on.exit(close(pb))
  tableBelongsToModel <- function(dbTableName, appId){
    return(startsWith(dbTableName, paste0(gsub("_", "", appId, fixed = TRUE), "_"))
           || (startsWith(dbTableName, "_sys_")
               && endsWith(dbTableName, paste0("_", appId))))
  }
  
  conn <- DBI::dbConnect(drv = RSQLite::SQLite(),
                         dbname = oldPath, bigint = "integer")
  tryCatch({
    allDbTables <- dbGetQuery(conn,
                              SQL("SELECT name FROM sqlite_master WHERE type = 'table';"))[[1L]]
  }, finally = {
    DBI::dbDisconnect(conn)
  })
  appIdsWithData <- vapply(allDbTables,
                           function(dbTableName){
                             if(!startsWith(dbTableName, "_sys_metadata_")){
                               return(NA_character_)
                             }
                             return(substring(dbTableName, nchar("_sys_metadata_") + 1L))
                           }, character(1L), USE.NAMES = FALSE)
  appIdsWithData <- appIdsWithData[!is.na(appIdsWithData)]
  i <- 1L
  for(appIdWithData in appIdsWithData){
    if(isWindows){
      setWinProgressBar(pb, i/(length(appIdsWithData) + 1L),
                        label = sprintf("Migrating data for app: '%s'", appIdWithData))
    }else{
      setTxtProgressBar(pb, value = i/(length(appIdsWithData) + 1L))
    }
    i <- i + 1L
    newPathFull <- file.path(newPath, paste0(appIdWithData, ".sqlite3"))
    if(file.exists(newPathFull)){
      flog.warn("Could not migrate data for app: '%s' as the file: '%s' already exists",
                appIdWithData, newPathFull)
      next
    }
    if(!file.copy(oldPath, newPathFull)){
      stop(sprintf("Could not copy: '%s' to: '%s'. Check access permissions.",
                   oldPath, newPathFull), call. = FALSE)
    }
    tablesBelongToModel <- vapply(allDbTables, tableBelongsToModel,
                                  logical(1L), USE.NAMES = FALSE, appIdWithData)
    conn <- DBI::dbConnect(drv = RSQLite::SQLite(),
                           dbname = newPathFull, bigint = "integer")
    tryCatch({
      dbExecute(conn, SQL("PRAGMA foreign_keys = ON;"))
      for(dbTableToRemove in allDbTables[!tablesBelongToModel]){
        dbExecute(conn, SQL(paste0("DROP TABLE IF EXISTS ", dbQuoteIdentifier(conn, dbTableToRemove), " ;")))
      }
      appTablePrefixLen <- nchar(gsub("_", "", appIdWithData, fixed = TRUE)) + 1L
      for(dbTableToRename in allDbTables[tablesBelongToModel]){
        if(startsWith(dbTableToRename, "_sys_")
           && endsWith(dbTableToRename, paste0("_", appIdWithData))){
          newTableName <- substring(dbTableToRename, 1L,
                                    nchar(dbTableToRename) - nchar(appIdWithData))
        }else{
          newTableName <- substring(dbTableToRename, appTablePrefixLen + 1L, nchar(dbTableToRename))
        }
        dbExecute(conn, SQL(paste0("ALTER TABLE ",
                                   DBI::dbQuoteIdentifier(conn, dbTableToRename), 
                                   " RENAME TO ",
                                   DBI::dbQuoteIdentifier(conn, newTableName))))
      }
      dbExecute(conn, "VACUUM")
    }, finally = {
      DBI::dbDisconnect(conn)
    })
  }
  if(file.move(oldPath, file.path(dirname(oldPath), "bk_miro.sqlite3"))){
    on.exit(flog.info("Backup of old database was stored in: '%s'. Delete manually to free space.", file.path(dirname(oldPath), "bk_miro.sqlite3")),
            add = TRUE)
  }else{
    on.exit(flog.warn("Could not move: '%s' to: '%s", oldPath, file.path(dirname(oldPath), "bk_miro.sqlite3")),
            add = TRUE)
  }
}

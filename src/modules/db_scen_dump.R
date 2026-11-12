# dump sandbox scenario to database
#
# These helpers are needed both by the save scenario module and by the AI
# assistant. The AI assistant is also available in readonly mode, where
# `db_scen_save.R` is not sourced, so they live in their own module.

saveScenRefreshScenData <- function(saveOutput) {
  return(tryCatch(
    {
      if (!saveOutput) {
        scenData$clearSandbox()
      }
      scenData$loadSandbox(
        getInputDataFromSandbox(),
        if (length(modelInFileNames)) modelInFileNames else character(),
        activeScen$getMetadataDf()
      )
      FALSE
    },
    no_data = function(e) {
      flog.error(conditionMessage(e))
      showErrorMsg(lang$errMsg$GAMSInput$title, conditionMessage(e))
      return(TRUE)
    },
    error = function(e) {
      flog.error("Unexpected error while fetching input data from sandbox. Error message: '%s'", conditionMessage(e))
      showErrorMsg(lang$errMsg$GAMSInput$title, lang$errMsg$unknownError)
      return(TRUE)
    }
  ))
}
saveScenDumpTempToDb <- function(scenName = lang$nav$dialogNewScen$newScenName, scenTags = NULL, msgProgress = lang$progressBar$saveScenDb) {
  # dumps current sandbox to database (required by AI assistant)
  if (saveScenRefreshScenData(saveOutput = TRUE)) {
    stop("Problems updating scenData", call. = FALSE)
  }
  # the name is unique per session so that multiple sessions of the same user do not share a snapshot
  tempScen <- Scenario$new(
    db = db, sname = paste0(scenName, "_", session$token),
    tags = scenTags, overwrite = TRUE,
    isNewScen = TRUE, scode = SCODEMAP[["sb_cache"]]
  )
  tempScen$save(scenData$get("sb", includeHiddenScalars = TRUE), msgProgress = msgProgress)
  return(tempScen)
}
removeStaleTempScensFromDb <- function(maxAgeHours = 24L) {
  # snapshots are removed when the AI assistant is closed, but not when the session crashes
  conn <- db$getConn()
  metaTable <- dbSchema$getDbTableName("_scenMeta")
  if (!DBI::dbExistsTable(conn, metaTable)) {
    return(invisible())
  }
  cutoff <- Sys.time() - 3600 * maxAgeHours
  # SQLite stores timestamps as local time strings (see dateColToChar)
  cutoff <- if (inherits(conn, "PqConnection")) format(cutoff, "%Y-%m-%d %H:%M:%S%z") else as.character(cutoff)
  affectedRows <- db$runQuery(DBI::sqlInterpolate(conn,
    paste0(
      "DELETE FROM ", DBI::dbQuoteIdentifier(conn, metaTable),
      " WHERE ", DBI::dbQuoteIdentifier(conn, "_uid"), " = ?uid AND ",
      DBI::dbQuoteIdentifier(conn, "_scode"), " = ?scode AND ",
      DBI::dbQuoteIdentifier(conn, "_stime"), " < ?cutoff"
    ),
    uid = db$getUid(), scode = SCODEMAP[["sb_cache"]], cutoff = cutoff
  ))
  if (affectedRows > 0L) {
    flog.debug("Removed %d stale temporary scenario(s).", affectedRows)
  }
  return(invisible())
}
tryCatch(removeStaleTempScensFromDb(), error = function(err) {
  flog.warn("Problems removing stale temporary scenarios. Error message: %s", conditionMessage(err))
})

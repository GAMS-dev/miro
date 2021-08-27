removeDbTablesServer("removeAllButton",
  errorContainerId = "unknownError",
  successContainerId = "removeSuccess"
)

output$btDownloadBackup <- downloadHandler(
  filename = function() {
    paste0("miro_backup_", strftime(Sys.Date(), "%Y_%m_%d"), ".sqlite3")
  },
  content = function(file) {
    prog <- Progress$new()
    on.exit(prog$close(), add = TRUE)
    prog$set(message = lang$adminMode$database$backupProgress, value = 0.2)
    dbMigrator$backupDatabase(file)
  },
  contentType = "application/x-sqlite3"
)

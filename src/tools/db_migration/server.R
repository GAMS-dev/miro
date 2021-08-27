serverDbMig <- function(session, input, output) {
  returnCode <- dbMigrationServer("migration", inconsistentTablesInfo, orphanedTablesInfo,
    standalone = TRUE
  )

  successTimeoutExpired <- FALSE
  observe({
    if (length(returnCode())) {
      if (identical(returnCode(), 0L)) {
        invalidateLater(4000)
        if (successTimeoutExpired) {
          stopApp(returnCode())
        } else {
          successTimeoutExpired <<- TRUE
        }
      } else {
        stopApp(returnCode())
      }
    }
  })
  session$onSessionEnded(function() {
    stopApp(1L)
  })
}

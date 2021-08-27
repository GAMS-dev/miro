# rendering output tables and graphs
renderOutputData <- function() {
  progress <- Progress$new()
  on.exit(progress$close())
  progress$set(
    message = lang$progressBar$renderOutput$title, value = 0.5,
    detail = paste0(lang$progressBar$renderOutput$progress, 1)
  )
  if (!is.null(dynamicUILoaded$dynamicTabsets[["tab_1"]])) {
    dynamicUILoaded$dynamicTabsets[["tab_1"]][["content"]][] <<- FALSE
  }
  loadDynamicTabContent(session, 1L,
    getSheetnamesByTabsetId(1L),
    initEnv = TRUE
  )
}

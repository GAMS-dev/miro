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
  tempScen <- Scenario$new(
    db = db, sname = scenName,
    tags = scenTags, overwrite = TRUE,
    isNewScen = TRUE, scode = SCODEMAP[["sb_cache"]]
  )
  tempScen$save(scenData$get("sb", includeHiddenScalars = TRUE), msgProgress = msgProgress)
  return(tempScen)
}

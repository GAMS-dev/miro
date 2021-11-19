scenMetaTibbleToJSON <- function(metadata) {
  metaTmp <- metadata[, c("_sname", "_stag", "_uid", "_stime", "_accessr", "_accessw", "_accessx")]
  for (colId in c("_stag", "_accessr", "_accessw", "_accessx")) {
    metaTmp[[colId]] <- stringi::stri_split_fixed(metaTmp[[colId]], ",", omit_empty = TRUE)
  }
  metaTmp[["_stime"]] <- paste0(metaTmp[["_stime"]], "Z")
  names(metaTmp) <- c("name", "tags", "owner", "last_modified", "read_perm", "write_perm", "exec_perm")
  return(toJSON(metaTmp, dataframe = "rows"))
}

deleteMIROScenario <- function(db, uid) {
  stdin <- file("stdin")
  on.exit(close(stdin))
  metadata <- fromJSON(suppressWarnings(readLines(stdin)))
  scenOwner <- trimws(metadata[["deleteScenOwner"]])
  if (!length(scenOwner) || identical(scenOwner, "")) {
    scenOwner <- uid
  }
  if (!db$checkSnameExists(metadata[["deleteScenName"]], scenOwner)) {
    stop_custom("error_not_found", "Scenario does not exist", call. = FALSE)
  }
  scen <- Scenario$new(
    db = db, sname = metadata[["deleteScenName"]],
    uid = scenOwner
  )
  scen$delete()
}

downloadMIROScenario <- function(uid, excelConfig = NULL) {
  xlsio <- XlsIO$new()
  stdin <- file("stdin")
  on.exit(close(stdin))
  metadata <- fromJSON(suppressWarnings(readLines(stdin)))
  exportFileType <- metadata[["downloadFileType"]]
  scenName <- metadata[["downloadScenName"]]
  scenOwner <- trimws(metadata[["downloadScenOwner"]])
  if (!length(scenOwner) || identical(scenOwner, "")) {
    scenOwner <- uid
  }
  scenMetaTmp <- db$importDataset(
    tableName = "_scenMeta",
    tibble(c("_sname", "_uid"), c(scenName, scenOwner))
  )
  if (!length(scenMetaTmp) || identical(nrow(scenMetaTmp), 0L)) {
    write("merr:::404:::Scenario not found", stderr())
    quit("no", 1L)
  }
  if (nrow(scenMetaTmp) > 1L) {
    write("merr:::500:::Scenario could not be uniquely identified", stderr())
    quit("no", 1L)
  }
  sidToDownload <- scenMetaTmp[[1L]][[1L]]
  file <- metadata[["downloadPath"]]
  refId <- "sb"
  tabsetId <- 1L
  scenData <- ScenData$new(
    db = db,
    scenDataTemplate = scenDataTemplate,
    hiddenOutputScalars = config$hiddenOutputScalars
  )
  scenData$load(sidToDownload,
    refId = refId,
    showProgress = FALSE
  )
  views <- Views$new(
    names(modelIn),
    names(modelOut),
    ioConfig$inputDsNamesBase
  )
  attachments <- Attachments$new(
    db, list(
      maxSize = attachMaxFileSize, maxNo = attachMaxNo,
      forbiddenFNames = c(
        if (identical(config$fileExchange, "gdx")) {
          c(MIROGdxInName, MIROGdxOutName)
        } else {
          paste0(c(names(modelOut), inputDsNames), ".csv")
        },
        paste0(modelNameRaw, c(".log", ".lst"))
      )
    ),
    tempdir(),
    names(modelIn),
    names(modelOut),
    ioConfig$inputDsNamesBase
  )
  views$loadConf(
    db$importDataset(
      tableName = "_scenViews",
      subsetSids = sidToDownload
    ), TRUE,
    tabsetId, sidToDownload
  )
  data <- scenData$get(refId, includeHiddenScalars = TRUE)
  if (scalarsOutName %in% names(data)) {
    data[[scalarsOutName]] <- scenData$getScalars(refId, outputScalarsOnly = TRUE)
  }
  exportScenario(file, data, exportFileType, refId, tabsetId, attachments, views, scenData, xlsio,
    excelConfig = excelConfig
  )
}

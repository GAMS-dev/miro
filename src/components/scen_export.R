exportScenario <- function(file, data, exportFileType, refId, tabsetId, attachments, views, scenData, xlsio, suppressRemoveModal = FALSE, session = NULL, excelConfig = NULL) {
  interactiveMode <- !is.null(session)
  if (interactiveMode) {
    prog <- Progress$new()
    on.exit(suppressWarnings(prog$close()))
    prog$set(message = lang$progressBar$exportScen$title, value = 0.1)
    prog$set(value = 0.2)
    noDatasets <- length(data)
  }
  if (identical(exportFileType, "gdx")) {
    return(tryCatch(
      {
        gdxio$wgdx(file, data, squeezeZeros = "n")
        if (interactiveMode && !suppressRemoveModal) {
          removeModal()
        }
      },
      error_duplicate_records = function(e) {
        flog.info("Duplicate records found when writing GDX file: %s", conditionMessage(e))
        if (interactiveMode) {
          showElReplaceTxt(session, "#scenExportError", conditionMessage(e))
          downloadHandlerError(file, conditionMessage(e))
        } else {
          write(sprintf(
            "merr:::422:::Duplicate records found when writing GDX file (in miroscen file): %s",
            conditionMessage(e)
          ), stderr())
          quit("no", 1L)
        }
      },
      error = function(e) {
        flog.info("Unexpected error while creating gdx file for export: %s", conditionMessage(e))
        if (interactiveMode) {
          showElReplaceTxt(session, "#scenExportError", lang$errMsg$unknownError)
          downloadHandlerError(file, lang$errMsg$unknownError)
        } else {
          write("merr:::500:::Unexpected error while creating gdx file for export.", stderr())
          quit("no", 1L)
        }
      }
    ))
  }
  if (identical(exportFileType, "miroscen")) {
    return(tryCatch(
      {
        generateMiroScen(
          file, scenData$getById("meta", refId = refId, drop = TRUE),
          data, attachments, views, tabsetId
        )
        if (interactiveMode && !suppressRemoveModal) {
          removeModal()
        }
      },
      error_duplicate_records = function(e) {
        flog.info(
          "Duplicate records found when writing GDX file (in miroscen file): %s",
          conditionMessage(e)
        )
        if (interactiveMode) {
          showElReplaceTxt(session, "#scenExportError", conditionMessage(e))
          downloadHandlerError(file, conditionMessage(e))
        } else {
          write(sprintf(
            "merr:::422:::Duplicate records found when writing GDX file (in miroscen file): %s",
            conditionMessage(e)
          ), stderr())
          quit("no", 1L)
        }
      },
      error = function(e) {
        flog.info("Unexpected error while creating miroscen file for export: %s", conditionMessage(e))
        if (interactiveMode) {
          showElReplaceTxt(session, "#scenExportError", lang$errMsg$unknownError)
          downloadHandlerError(file, lang$errMsg$unknownError)
        } else {
          write("merr:::500:::Unexpected error while creating miroscen file for export.", stderr())
          quit("no", 1L)
        }
      }
    ))
  }
  if (identical(exportFileType, "csv")) {
    if (interactiveMode && !suppressRemoveModal) {
      removeModal()
    }
    if (length(data) == 0L) {
      return(readr::write_csv(tibble(), file))
    } else if (length(data) == 1L) {
      return(readr::write_csv(data[[1L]], file))
    }
    tmpDir <- file.path(tempdir(), paste0(uid, "_exp_tmp_dir"))
    if (file.exists(tmpDir) && !identical(unlink(tmpDir, recursive = TRUE), 0L)) {
      flog.error("Could not remove temporary directory: '%s'.", tmpDir)
      if (interactiveMode) {
        showElReplaceTxt(session, "#scenExportError", lang$errMsg$unknownError)
        downloadHandlerError(file, "Directory could not be removed")
      } else {
        write("merr:::500:::Unexpected error while creating csv file for export.", stderr())
        quit("no", 1L)
      }
    }
    if (!dir.create(tmpDir, recursive = TRUE)) {
      flog.error("Could not create temporary directory: '%s'.", tmpDir)
      if (interactiveMode) {
        showElReplaceTxt(session, "#scenExportError", lang$errMsg$unknownError)
        downloadHandlerError(file, "Directory could not be created")
      } else {
        write("merr:::500:::Unexpected error while creating csv file for export.", stderr())
        quit("no", 1L)
      }
    }
    on.exit(unlink(tmpDir, recursive = TRUE), add = TRUE)
    for (i in seq_along(data)) {
      dsName <- names(data)[i]
      if (interactiveMode) {
        prog$inc(amount = 0.8 / noDatasets, detail = sprintf(lang$progressBar$exportScen$exportDs, i, noDatasets))
      }
      readr::write_csv(data[[dsName]], file.path(tmpDir, paste0(dsName, ".csv")))
    }
    return(suppressWarnings(zip::zipr(file, list.files(tmpDir, full.names = TRUE),
      recurse = FALSE, include_directories = FALSE
    )))
  }
  return(tryCatch(
    {
      xlsio$write(file, data, scenData$getById("meta", refId = refId, drop = TRUE),
        includeMetadataSheet = excelConfig$includeMeta,
        includeEmptySheets = excelConfig$includeEmpty
      )
      if (interactiveMode && !suppressRemoveModal) {
        removeModal()
      }
    },
    error = function(e) {
      flog.info("Unexpected error while creating xlsx file for export: %s", conditionMessage(e))
      if (interactiveMode) {
        showElReplaceTxt(session, "#scenExportError", lang$errMsg$unknownError)
        downloadHandlerError(file, lang$errMsg$unknownError)
      } else {
        write("merr:::500:::Unexpected error while creating xlsx file for export.", stderr())
        quit("no", 1L)
      }
    }
  ))
}

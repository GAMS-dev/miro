exportScenario <- function(file, data, exportFileTypeId, refId, tabsetId, attachments, views, scenData, xlsio, suppressRemoveModal = FALSE, session = NULL, excelConfig = NULL,
                           customDataIO = NULL, sandboxScenario = NULL) {
  interactiveMode <- !is.null(session)
  if (interactiveMode) {
    prog <- Progress$new()
    on.exit(suppressWarnings(prog$close()))
    prog$set(message = lang$progressBar$exportScen$title, value = 0.1)
    prog$set(value = 0.2)
    noDatasets <- length(data)
  }
  if (identical(exportFileTypeId, "gdx")) {
    return(tryCatch(
      {
        gdxio$wgdx(file, data, squeezeZeros = "n")
        if (interactiveMode) {
          if (!suppressRemoveModal) {
            removeModal()
          }
          showNotification(sprintf(
            lang$progressBar$exportScen$success,
            "GDX"
          ))
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
  if (identical(exportFileTypeId, "miroscen")) {
    return(tryCatch(
      {
        generateMiroScen(
          file, scenData$getById("meta", refId = refId, drop = TRUE),
          data, attachments, views, tabsetId
        )
        if (interactiveMode) {
          if (!suppressRemoveModal) {
            removeModal()
          }
          showNotification(sprintf(
            lang$progressBar$exportScen$success,
            "MIROSCEN"
          ))
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
  if (identical(exportFileTypeId, "csv")) {
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
    suppressWarnings(zip::zipr(file, list.files(tmpDir, full.names = TRUE),
      recurse = FALSE, include_directories = FALSE
    ))
    if (interactiveMode) {
      if (!suppressRemoveModal) {
        removeModal()
      }
      showNotification(sprintf(
        lang$progressBar$exportScen$success,
        "CSV"
      ))
    }
    return()
  }
  if (is.integer(exportFileTypeId)) {
    # custom export function
    return(tryCatch(
      {
        customDataIO$
          write(data, path = file, sandboxScenario = sandboxScenario)
        if (interactiveMode) {
          if (!suppressRemoveModal) {
            removeModal()
          }
          showNotification(sprintf(
            lang$progressBar$exportScen$success,
            customDataIO$getLabel()
          ))
        }
      },
      error_custom = function(e) {
        flog.debug(
          "Custom exporter: %s reported a custom error: %s",
          customDataIO$getLabel(),
          conditionMessage(e)
        )
        if (interactiveMode) {
          showElReplaceTxt(session, "#scenExportError", conditionMessage(e))
          downloadHandlerError(file, conditionMessage(e))
        } else {
          write(paste0("merr:::400:::", conditionMessage(e)), stderr())
          quit("no", 1L)
        }
      },
      error = function(e) {
        flog.info(
          "Unexpected error while executing the custom export function: %s. Error message: %s",
          customDataIO$getLabel(),
          conditionMessage(e)
        )
        if (interactiveMode) {
          showElReplaceTxt(session, "#scenExportError", lang$errMsg$unknownError)
          downloadHandlerError(file, lang$errMsg$unknownError)
        } else {
          write("merr:::500:::Unexpected error while executing the custom export function.", stderr())
          quit("no", 1L)
        }
      }
    ))
  }
  return(tryCatch(
    {
      xlsio$write(file, data, scenData$getById("meta", refId = refId, drop = TRUE),
        includeMetadataSheet = excelConfig$includeMeta,
        includeEmptySheets = excelConfig$includeEmpty
      )
      if (interactiveMode) {
        if (!suppressRemoveModal) {
          removeModal()
        }
        showNotification(sprintf(
          lang$progressBar$exportScen$success,
          "Excel"
        ))
      }
    },
    error = function(e) {
      if (grepl("row or column index out of rang", conditionMessage(e), fixed = TRUE)) {
        flog.info("Data exceeds maximum number of rows and/or columns supported by Excel: %s", conditionMessage(e))
        errMsgInteractive <- lang$errMsg$xlsio$errors$maxDimExceeded
        errMsg <- "Data exceeds maximum number of rows and/or columns supported by Excel."
      } else {
        flog.info("Unexpected error while creating xlsx file for export: %s", conditionMessage(e))
        errMsgInteractive <- lang$errMsg$unknownError
        errMsg <- "Unexpected error while creating xlsx file for export."
      }
      if (interactiveMode) {
        showElReplaceTxt(session, "#scenExportError", errMsgInteractive)
        downloadHandlerError(file, errMsgInteractive)
      } else {
        write(paste0("merr:::500:::", errMsg), stderr())
        quit("no", 1L)
      }
    }
  ))
}

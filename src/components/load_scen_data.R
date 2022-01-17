loadScenData <- function(metaData, workDir,
                         templates, method = "csv", csvDelim = ",",
                         fileName = character(0L), DDPar = character(0L), GMSOpt = character(0L),
                         dfClArgs = NULL, xlsio = NULL, csvio = NULL) {
  ret <- list(tabular = NULL, scalar = NULL, errors = character())
  loadDataErrors <- CharArray$new()
  if (length(method) == 1L && method %in% c("xls", "gdx", "scsv")) {
    if (identical(method, "scsv") && is.null(csvio)) {
      return(ret)
    }
    dataFilePath <- file.path(workDir, fileName)
    if (!file.exists(dataFilePath)) {
      stop(sprintf("File: '%s' could not be found.", dataFilePath), call. = FALSE)
    }
  } else if (!identical(method, "csv")) {
    stop("Method is not suported for loading data.", call. = FALSE)
  }
  # read scalar data in case it exists
  isNewGdx <- TRUE
  # fetch results from csv files
  lapply(seq_along(metaData), function(i) {
    tryCatch(
      {
        switch(method,
          scsv = {
            if (tryCatch(
              {
                ret$tabular[[i]] <<- csvio$read(dataFilePath, names(metaData)[[i]])
                FALSE
              },
              error_notfound = function(e) {
                ret$tabular[[i]] <<- templates[[i]]
                return(TRUE)
              }
            )) {
              return()
            }
          },
          csv = {
            if (file.exists(file.path(workDir, names(metaData)[[i]] %+% ".csv"))) {
              ret$tabular[[i]] <<- read_delim(file.path(workDir, names(metaData)[[i]] %+% ".csv"),
                csvDelim,
                col_types = metaData[[i]]$colTypes,
                col_names = TRUE
              )
            } else {
              ret$tabular[[i]] <<- templates[[i]]
            }
            if (identical(names(metaData)[[i]], scalarsFileName)) {
              if (length(c(DDPar, GMSOpt)) > 0 &&
                (length(ret$tabular[[i]]) == 0L || length(ret$tabular[[i]]) == 3L) &&
                file.exists(file.path(workDir, modelName %+% ".pf"))) {
                pfFileContent <- loadPfFileContent(read_lines(
                  file.path(workDir, modelName %+% ".pf")
                ), GMSOpt, DDPar)
                if (length(pfFileContent) == 3L) {
                  if (length(ret$tabular[[i]])) {
                    names(pfFileContent) <- names(ret$tabular[[i]])
                  }
                  ret$tabular[[i]] <- bind_rows(ret$tabular[[i]], pfFileContent)
                }
              }
            }
          },
          xls = {
            if (tryCatch(
              {
                ret$tabular[[i]] <<- xlsio$read(dataFilePath, names(metaData)[[i]])
                FALSE
              },
              error_no_data = function(e) {
                flog.debug("Symbol: %s contains no data", names(metaData)[[i]])
                ret$tabular[[i]] <<- templates[[i]]
                return(TRUE)
              },
              error_notfound = function(e) {
                flog.debug("Symbol: %s not found in Excel spreadsheet", names(metaData)[[i]])
                ret$tabular[[i]] <<- templates[[i]]
                return(TRUE)
              },
              error_parse_config = function(e) {
                flog.info(
                  "Symbol: %s : Problems parsing index from Excel file. Error message: %s",
                  names(metaData)[[i]], conditionMessage(e)
                )
                loadDataErrors$push(conditionMessage(e))
                ret$tabular[[i]] <<- templates[[i]]
                return(TRUE)
              },
              error_data = function(e) {
                flog.info(
                  "Symbol: %s : Problems reading data. Error message: %s",
                  names(metaData)[[i]], conditionMessage(e)
                )
                loadDataErrors$push(conditionMessage(e))
                ret$tabular[[i]] <<- templates[[i]]
                return(TRUE)
              }
            )) {
              return()
            }
          },
          gdx = {
            tryCatch(
              {
                ret$tabular[[i]] <<- gdxio$rgdx(dataFilePath, names(metaData)[[i]],
                  names = names(metaData[[i]]$headers),
                  isNewGdx = isNewGdx
                )
                isNewGdx <<- FALSE
                if (!inherits(ret$tabular[[i]], "data.frame")) {
                  ret$tabular[[i]] <<- ddToTibble(ret$tabular[[i]], metaData[[i]])
                }
                if (!length(ret$tabular[[i]])) {
                  ret$tabular[[i]] <<- templates[[i]]
                }
              },
              error = function(e) {
                if (grepl("Compression library not found",
                  conditionMessage(e),
                  fixed = TRUE
                )) {
                  stop("Compressed GDX is not supported. Please remove the GDXCOMPRESS environment variable.")
                }
                ret$tabular[[i]] <<- templates[[i]]
              }
            )
          }
        )
      },
      error = function(e) {
        stop(sprintf(
          "Model file: '%s' could not be read. Error message: %s",
          names(metaData)[[i]], conditionMessage(e)
        ), call. = FALSE)
      }
    )
    if (!identical(length(ret$tabular[[i]]), length(metaData[[i]]$headers))) {
      if (identical(metaData[[i]]$symtype, "set") &&
        length(metaData[[i]]$headers) == 1L &&
        length(ret$tabular[[i]]) == 2L) {
        # multi-dropdown with single column
        ret$tabular[[i]] <<- ret$tabular[[i]][-2]
      } else {
        flog.warn("Invalid data attempted to be read (number of headers of table does not match io_config schema).")
        stop(sprintf(lang$errMsg$GAMSOutput$badOutputData, names(metaData)[i]), call. = FALSE)
      }
    }
    ret$tabular[[i]] <<- fixColTypes(ret$tabular[[i]], metaData[[i]]$colTypes)
    names(ret$tabular[[i]]) <<- names(metaData[[i]]$headers)
    if (identical(names(metaData)[[i]], scalarsFileName)) {
      if (length(dfClArgs) == 3L) {
        ret$tabular[[i]] <<- bind_rows(ret$tabular[[i]], setNames(dfClArgs, names(metaData[[i]]$headers)))
      }
    } else {
      ret$tabular[[i]] <<- ret$tabular[[i]] %>% mutate(across(
        where(is.character),
        ~ replace_na(.x, replace = "")
      ))
    }
    if (!hasValidHeaderTypes(ret$tabular[[i]], metaData[[i]]$colTypes)) {
      flog.warn(
        "Dataset: '%s' has invalid header types ('%s'). Header types should be: '%s'.",
        names(metaData)[i], paste(vapply(ret$tabular[[i]], function(el) {
          return(class(el)[[1L]])
        },
        character(1L),
        USE.NAMES = FALSE
        ), collapse = "', '"),
        metaData[[i]]$colTypes
      )
      stop(sprintf(lang$errMsg$GAMSOutput$badOutputData, names(metaData)[i]), call. = FALSE)
    }
  })
  ret$errors <- loadDataErrors$get()
  return(ret)
}

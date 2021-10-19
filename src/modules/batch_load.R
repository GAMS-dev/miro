hcubeRemoveConfirmed <- FALSE

batchLoadData <- tibble()

inputType <- list(text = c("_uid", "_sname"), date = "_stime", csv = "_stag")
scalarFields <- NULL
scalarKeyTypeList <- list()
modelInSorted <- sort(names(modelIn))
modelOutSorted <- sort(modelOut[[scalarsOutName]]$symnames)

appendInputTypeList <- function(scalarsTabName) {
  inputType$text <<- c(inputType$text, vapply(scalarKeyTypeList[[scalarsTabName]],
    function(el) if (el$type %in% c("set", "string", "acronym")) el$key else NA_character_,
    character(1L),
    USE.NAMES = FALSE
  ))
  inputType$text <<- inputType$text[!is.na(inputType$text)]
  inputType$number <<- c(inputType$number, vapply(scalarKeyTypeList[[scalarsTabName]],
    function(el) if (el$type %in% c("scalar", "parameter", "number")) el$key else NA_character_,
    character(1L),
    USE.NAMES = FALSE
  ))
  inputType$number <<- inputType$number[!is.na(inputType$number)]
}

k <- 1L
for (j in seq_along(modelIn)) {
  i <- match(modelInSorted[[j]], names(modelIn))
  if (identical(modelIn[[i]]$symtype, "set")) {
    next
  }
  if (!is.null(modelIn[[i]]$daterange)) {
    scalarKeyTypeList[[scalarsFileName]][[k]] <- list(
      key = paste0(names(modelIn)[[i]], "_lo"),
      type = "string",
      alias = paste0(modelInAlias[[i]], " (lower)")
    )
    scalarKeyTypeList[[scalarsFileName]][[k + 1L]] <- list(
      key = paste0(names(modelIn)[[i]], "_up"),
      type = "string",
      alias = paste0(modelInAlias[[i]], " (upper)")
    )
    k <- k + 2L
  } else if (length(modelIn[[i]]$slider$default) > 1L) {
    # slider range
    scalarKeyTypeList[[scalarsFileName]][[k]] <- list(
      key = paste0(names(modelIn)[[i]], "_lo"),
      type = "number",
      alias = paste0(modelInAlias[[i]], " (lower)")
    )
    scalarKeyTypeList[[scalarsFileName]][[k + 1L]] <- list(
      key = paste0(names(modelIn)[[i]], "_up"),
      type = "number",
      alias = paste0(modelInAlias[[i]], " (upper)")
    )
    k <- k + 2L
  } else if (modelIn[[i]]$type %in% c("slider", "checkbox", "numericinput") ||
    identical(modelIn[[i]]$dropdown$checkbox, TRUE)) {
    scalarKeyTypeList[[scalarsFileName]][[k]] <- list(
      key = names(modelIn)[[i]],
      type = "number",
      alias = modelInAlias[[i]]
    )
    k <- k + 1L
  } else if (modelIn[[i]]$type %in% c("dropdown", "dropdowne", "date", "textinput") &&
    !isFALSE(modelIn[[i]]$dropdown$single)) {
    scalarKeyTypeList[[scalarsFileName]][[k]] <- list(
      key = names(modelIn)[[i]],
      type = "string",
      alias = modelInAlias[[i]]
    )
    k <- k + 1L
  }
}

if (length(modelIn[[scalarsFileName]])) {
  scalarKeyTypeList[[scalarsFileName]] <- c(
    scalarKeyTypeList[[scalarsFileName]],
    lapply(seq_along(modelIn[[scalarsFileName]]$symnames), function(i) {
      list(
        key = modelIn[[scalarsFileName]]$symnames[[i]],
        type = modelIn[[scalarsFileName]]$symtypes[[i]],
        alias = modelIn[[scalarsFileName]]$symtext[[i]]
      )
    })
  )
}

if (length(scalarKeyTypeList[[scalarsFileName]])) {
  appendInputTypeList(scalarsFileName)
  scalarFields <- paste0(
    scalarsFileName, ".",
    vapply(scalarKeyTypeList[[scalarsFileName]],
      "[[", character(1L), "key",
      USE.NAMES = FALSE
    )
  )
  names(scalarFields) <- vapply(scalarKeyTypeList[[scalarsFileName]],
    "[[", character(1L), "alias",
    USE.NAMES = FALSE
  )
}
scalarOutFields <- NULL
if (length(modelOut[[scalarsOutName]])) {
  scalarKeyTypeList[[scalarsOutName]] <- lapply(seq_along(modelOut[[scalarsOutName]]$symnames), function(i) {
    list(
      key = modelOut[[scalarsOutName]]$symnames[[i]],
      type = modelOut[[scalarsOutName]]$symtypes[[i]],
      alias = modelOut[[scalarsOutName]]$symtext[[i]]
    )
  })
  scalarKeyTypeList[[scalarsOutName]] <- scalarKeyTypeList[[scalarsOutName]][order(vapply(scalarKeyTypeList[[scalarsOutName]],
    "[[", character(1L), "key",
    USE.NAMES = FALSE
  ))]
  appendInputTypeList(scalarsOutName)
  scalarOutFields <- scalarsOutName %+% "." %+%
    vapply(scalarKeyTypeList[[scalarsOutName]], "[[", character(1L), "key", USE.NAMES = FALSE)
  names(scalarOutFields) <- vapply(scalarKeyTypeList[[scalarsOutName]],
    "[[", character(1L), "alias",
    USE.NAMES = FALSE
  )
  scalarFields <- c(scalarFields, scalarOutFields)
}

batchLoader <- BatchLoader$new(db, inputDsNamesNotToDisplay)
batchLoadFilters <- c("", paste0(
  dbSchema$getDbTableName("_scenMeta"), ".",
  c("_uid", "_sname", "_stime", "_stag")
))
names(batchLoadFilters) <- c(
  "", lang$nav$queryBuilder$metaColAliases$uid,
  lang$nav$queryBuilder$metaColAliases$sname,
  lang$nav$queryBuilder$metaColAliases$stime,
  lang$nav$queryBuilder$metaColAliases$stag
)
batchLoadFilters <- c(batchLoadFilters, scalarFields)

maxNumBlocks <- 5L
activeBlocks <- vector("logical", maxNumBlocks)
activeLines <- vector("logical", maxNumBlocks^2)
fieldsSelected <- vector("character", maxNumBlocks^2)
exclAttribChoices <- c(batchLoadFilters[2:4], scalarOutFields)
if (length(vapply(scalarKeyTypeList[[scalarsOutName]],
  "[[", character(1L), "key",
  USE.NAMES = FALSE
))) {
  names(exclAttribChoices)[4:length(exclAttribChoices)] <- vapply(scalarKeyTypeList[[scalarsOutName]],
    "[[", character(1L), "alias",
    USE.NAMES = FALSE
  )
}

observeEvent(input$btNewBlock,
  {
    if (all(activeBlocks)) {
      return(NULL)
    }
    blockIdx <- which.min(activeBlocks)
    activeBlocks[blockIdx] <<- TRUE
    addQueryBuilderBlock(id = blockIdx, choices = batchLoadFilters)
  },
  ignoreNULL = FALSE
)

queryBuilderRemoveBlock <- function(blockIdx) {
  activeBlocks[blockIdx] <<- FALSE
  activeLines[(blockIdx - 1) * maxNumBlocks + 1] <<- FALSE
  removeUI(selector = paste0("#block", blockIdx))
}

lapply(seq_len(maxNumBlocks), function(blockIdx) {
  if (blockIdx > 1L) {
    observeEvent(input[[paste0("btRemoveBlock", blockIdx)]], {
      queryBuilderRemoveBlock(blockIdx)
    })
  }
  blockOffset <- (blockIdx - 1L) * maxNumBlocks
  lapply(seq_len(maxNumBlocks), function(lineIdx) {
    observeEvent(input[[paste0("btRemoveLine", blockIdx, "_", lineIdx)]], {
      activeLines[lineIdx + blockOffset] <<- FALSE
      removeUI(selector = paste0("#line", blockIdx, "_", lineIdx))
      if (blockIdx > 1L &&
        !any(activeLines[seq(blockOffset + 1L, blockOffset + maxNumBlocks)])) {
        queryBuilderRemoveBlock(blockIdx)
      }
    })
  })
  observeEvent(input[[paste0("newLine_", blockIdx)]], {
    if (all(activeLines[seq(blockOffset + 1L, blockOffset + maxNumBlocks)]) ||
      input[[paste0("newLine_", blockIdx)]] == "") {
      return()
    }
    lineIdxToAssign <- which.min(activeLines[seq(
      blockOffset + 1L,
      blockOffset + maxNumBlocks
    )])
    activeLines[blockOffset + lineIdxToAssign] <<- TRUE

    fieldSelected <- input[[paste0("newLine_", blockIdx)]]

    fieldId <- match(fieldSelected, batchLoadFilters)
    if (length(fieldId) != 1L || is.na(fieldId)) {
      flog.error(
        "Invalid field selected in query builder: '%s'. This is likely an attempt to tamper with the app!",
        fieldSelected
      )
      stop()
    }
    label <- names(batchLoadFilters)[fieldId]

    fieldsSelected[blockOffset + lineIdxToAssign] <<- fieldSelected
    field <- strsplit(fieldSelected, ".", fixed = TRUE)[[1]][[2]]

    if (field %in% inputType[["text"]]) {
      ui <- generateLine(blockIdx, lineIdxToAssign, "text", label)
    } else if (field %in% inputType[["date"]]) {
      ui <- generateLine(blockIdx, lineIdxToAssign, "date", label)
    } else if (field %in% inputType[["csv"]]) {
      ui <- generateLine(blockIdx, lineIdxToAssign, "csv", label)
    } else {
      ui <- generateLine(blockIdx, lineIdxToAssign, "number", label)
    }
    insertUI(
      selector = paste0("#blockContent", blockIdx),
      where = "beforeEnd",
      ui = ui
    )
    updateSelectInput(session, paste0("newLine_", blockIdx), selected = "")
  })
})
observeEvent(input$btSendQuery, {
  flog.trace("Batch Load: Button to send new query clicked.")
  disableEl(session, "#btSendQuery")
  showEl(session, "#hyperQueryLoad")
  on.exit(hideEl(session, "#hyperQueryLoad"))
  on.exit(enableEl(session, "#btSendQuery"), add = TRUE)

  i <- 1L
  subsetCoditions <- NULL
  for (blockIdx in which(activeBlocks)) {
    j <- 1L
    table <- NULL
    field <- NULL
    val <- NULL
    op <- NULL

    blockOffset <- (blockIdx - 1L) * maxNumBlocks
    for (lineIdx in which(activeLines[seq(
      blockOffset + 1L,
      blockOffset + maxNumBlocks
    )])) {
      tableField <- strsplit(fieldsSelected[blockOffset + lineIdx],
        ".",
        fixed = TRUE
      )[[1]]
      table[j] <- tableField[[1]]
      field[j] <- tableField[[2]]
      opUntrusted <- as.character(input[[paste0("op_", blockIdx, "_", lineIdx)]])
      filterVal <- input[[paste0("val_", blockIdx, "_", lineIdx)]]

      if (field[j] %in% inputType[["text"]]) {
        validOperators <- c("%LIKE%", "%NOTLIKE%", "LIKE%", "%LIKE", "=", "!=", "%EXIST", "%NOTEXIST")
      } else if (field[j] %in% inputType[["date"]]) {
        validOperators <- "BETWEEN"
      } else if (field[j] %in% inputType[["csv"]]) {
        validOperators <- c(
          "%LIKE%", "%NOTLIKE%", ",LIKE%",
          "%LIKE,", "%,LIKE,%", "%,NOTLIKE,%"
        )
        if (grepl(",", filterVal[1], fixed = TRUE)) {
          fieldId <- match(fieldsSelected[blockOffset + lineIdx], batchLoadFilters)
          showHideEl(
            session, "#queryBuilderError", 4000L,
            sprintf(
              lang$nav$queryBuilder$invalidCsvFilter,
              if (is.na(fieldId)) batchLoadFilters[j] else names(batchLoadFilters)[fieldId]
            )
          )
          return()
        }
      } else {
        validOperators <- c("=", "<", ">", "<=", ">=", "!=")
      }
      if (length(opUntrusted) != 1L || !opUntrusted %in% validOperators) {
        flog.error(
          "Invalid operator: '%s' received for field: '%s'. This is likely an attempt to tamper with the app!",
          opUntrusted, field[j]
        )
        return()
      }
      op[j] <- opUntrusted
      filterValEscaped <- db$escapePatternPivot(filterVal)
      if (grepl("%", op[j])[[1]]) {
        switch(op[j],
          "%LIKE" = {
            val[j] <- paste0("%", filterValEscaped)

            op[j] <- "LIKE"
          },
          "LIKE%" = {
            val[j] <- paste0(filterValEscaped, "%")
            op[j] <- "LIKE"
          },
          "%LIKE%" = {
            val[j] <- paste0("%", filterValEscaped, "%")
            op[j] <- "LIKE"
          },
          "%NOTLIKE%" = {
            val[j] <- paste0("%", filterValEscaped, "%")
            op[j] <- "NOT LIKE"
          },
          ",LIKE%" = {
            val[j] <- paste0("%,", filterValEscaped, "%")
            op[j] <- "LIKE"
          },
          "%LIKE," = {
            val[j] <- paste0("%", filterValEscaped, ",%")
            op[j] <- "LIKE"
          },
          "%,LIKE,%" = {
            val[j] <- paste0("%,", filterValEscaped, ",%")
            op[j] <- "LIKE"
          },
          "%,NOTLIKE,%" = {
            val[j] <- paste0("%,", filterValEscaped, ",%")
            op[j] <- "NOT LIKE"
          },
          "%EXIST" = {
            val[j] <- NA
            op[j] <- "!="
          },
          "%NOTEXIST" = {
            val[j] <- NA
            op[j] <- "="
          }
        )
      } else if (identical(op[j], "BETWEEN")) {
        table[j + 1] <- tableField[[1]]
        field[j + 1] <- tableField[[2]]
        val[j] <- as.character(filterVal[[1]])
        val[j + 1] <- as.character(filterVal[[2]] + 1)
        op[j] <- ">="
        op[j + 1] <- "<"
        j <- j + 2L
        next
      } else {
        val[j] <- filterVal
      }
      j <- j + 1L
    }
    if (length(field)) {
      subsetCoditions[[i]] <- tibble(field, val, op, table)
    } else {
      subsetCoditions[[i]] <- tibble()
    }
    i <- i + 1L
  }
  colsToFetch <- strsplit(batchLoadFilters[-1], ".", fixed = TRUE)
  colN <- c("_sid", vapply(colsToFetch,
    "[[",
    FUN.VALUE = "character", 2,
    USE.NAMES = FALSE
  ))
  names(colN) <- c("_sys_metadata_", vapply(colsToFetch, "[[",
    FUN.VALUE = "character", 1,
    USE.NAMES = FALSE
  ))
  tryCatch(
    {
      batchLoadData <<- batchLoader$fetchResults(subsetCoditions,
        colNames = colN,
        limit = hcubeLoadMaxScen
      )
      rv$updateBatchLoadData <- rv$updateBatchLoadData + 1L
    },
    error = function(e) {
      if (identical(conditionMessage(e), "maxNoRowsVio")) {
        errMsg <- sprintf(
          "Your query results in too many scenarios to be fetched from the database. The maximum number of scenarios to be fetched is: %d. Please narrow your search.",
          hcubeLoadMaxScen
        )
      } else {
        errMsg <- "An error occurred while executing the database query. " %+%
          "Please try again or contact the system administrator in case this problem persists."
      }
      showErrorMsg("Error fetching data", errMsg)
      flog.warn("Problems executing hcubeLoad query. Error message: %s.", conditionMessage(e))
    }
  )
})

batchLoadResultsProxy <- dataTableProxy("batchLoadResults")
observeEvent(input$batchLoadResults_cell_edit, {
  newValInfo <- input$batchLoadResults_cell_edit
  scenInfo <- batchLoadData[newValInfo$row, c(1, 2)]
  if (tryCatch(
    {
      if (identical(newValInfo$col, 1L)) {
        flog.trace(
          "Batch Load: New scenario name for sid: %s entered: %s",
          scenInfo[["_sid"]][1], newValInfo$value
        )
        batchLoader$renameScen(
          scenInfo[["_sid"]][1],
          scenInfo[["_uid"]][1],
          newValInfo$value
        )
      } else if (identical(newValInfo$col, 3L)) {
        flog.trace(
          "Batch Load: New scenario tag(s) for sid: %s entered: %s",
          scenInfo[["_sid"]][1], newValInfo$value
        )
        batchLoader$editScenTags(
          scenInfo[["_sid"]][1],
          newValInfo$value
        )
      } else {
        stop("Invalid column edited. This should never happen and is likely an attempt to tamper with the app!",
          call. = FALSE
        )
      }
      batchLoadData[newValInfo$row, newValInfo$col + 2L] <<- newValInfo$value
      replaceData(batchLoadResultsProxy,
        batchLoadData[, -1],
        resetPaging = FALSE, rownames = FALSE
      )
      FALSE
    },
    error_bad_name = function(e) {
      flog.debug("Invalid scenario name entered")
      showHideEl(
        session, "#queryBuilderError", 4000L,
        lang$nav$dialogEditMeta$badName
      )
      return(TRUE)
    },
    error_bad_tags = function(e) {
      flog.debug("Invalid scenario tags entered")
      showHideEl(
        session, "#queryBuilderError", 4000L,
        lang$nav$dialogEditMeta$badTags
      )
      return(TRUE)
    },
    error_scen_exists = function(e) {
      flog.debug("Scenario with same name already exists")
      showHideEl(
        session, "#queryBuilderError", 4000L,
        lang$nav$dialogEditMeta$scenExists
      )
      return(TRUE)
    },
    error_perm = function(e) {
      flog.debug("No write permissions to rename scenario")
      showHideEl(
        session, "#queryBuilderError", 4000L,
        lang$errMsg$permErr
      )
      return(TRUE)
    },
    error = function(e) {
      flog.error(
        "Unexpected error while renaming scenario/editing scenario tags. Error message: '%s'",
        conditionMessage(e)
      )
      showHideEl(
        session, "#queryBuilderError", 4000L,
        lang$errMsg$unknownError
      )
      return(TRUE)
    }
  )) {
    reloadData(batchLoadResultsProxy, resetPaging = FALSE)
  }
})

output$batchLoadResults <- renderDataTable(
  if (rv$updateBatchLoadData > 0L) {
    if (length(batchLoadData) && nrow(batchLoadData)) {
      showEl(session, "#batchLoadButtons")
      hideEl(session, "#batchLoadNoData")
    } else {
      showEl(session, "#batchLoadNoData")
      hideEl(session, "#batchLoadButtons")
      return()
    }
    data <- batchLoadData[, -1]
    dtObj <- datatable(
      data,
      filter = "bottom", colnames = names(batchLoadFilters)[-1], rownames = FALSE,
      editable = list(target = "cell", disable = list(columns = seq_along(data)[-c(2L, 4L)] - 1L)),
      options = list(scrollX = TRUE, columnDefs = list(list(
        targets = "_all",
        render = JS(
          "function(data, type, row, meta) {",
          "return type === 'display' && data != null && data.length > 20 ?",
          "'<span class=\"dt-allow-click-event\" title=\"' + data + '\">' + data.substr(0, 20) + '...</span>' : data;",
          "}"
        )
      )))
    ) %>%
      formatDate(3L, method = "toLocaleString")
    if (length(data) > 4L) {
      numericCols <- seq(5L, length(data))[vapply(data[, seq(5L, length(data))],
        is.numeric, logical(1L),
        USE.NAMES = FALSE
      )]
      if (!length(numericCols)) {
        return(dtObj)
      }
      return(formatRound(dtObj, numericCols, digits = roundPrecision))
    }
    return(dtObj)
  }
)

observeEvent(input$batchLoadSelected, {
  flog.debug("Button to load selected scenarios (Batch load) clicked.")
  if (!length(input$batchLoadResults_rows_selected)) {
    showHideEl(session, "#queryBuilderError", 4000L, lang$nav$scen$noScen)
    return()
  }
  hcubeRemoveConfirmed <<- FALSE
  sidsToLoad <<- as.integer(batchLoadData[[1]][input$batchLoadResults_rows_selected])
  showBatchLoadDialog(length(sidsToLoad), batchLoadFilters, maxSolversPaver,
    exclAttribChoices = exclAttribChoices,
    customScripts = config$scripts$hcube,
    colNamesForNaming = setNames(names(batchLoadData)[-1], names(batchLoadFilters)[-1])
  )
})
observeEvent(input$batchLoadCurrent, {
  flog.debug("Button to load current page of scenarios (Batch load) clicked.")
  if (is.null(input$batchLoadResults_rows_current)) {
    return()
  }
  hcubeRemoveConfirmed <<- FALSE
  sidsToLoad <<- as.integer(batchLoadData[[1]][input$batchLoadResults_rows_current])
  showBatchLoadDialog(length(sidsToLoad), batchLoadFilters, maxSolversPaver,
    exclAttribChoices = exclAttribChoices,
    customScripts = config$scripts$hcube,
    colNamesForNaming = setNames(names(batchLoadData)[-1], names(batchLoadFilters)[-1])
  )
})
observeEvent(input$batchLoadAll, {
  flog.debug("Button to load all scenarios (Batch load) clicked.")
  if (!length(batchLoadData) || !nrow(batchLoadData)) {
    return()
  }
  hcubeRemoveConfirmed <<- FALSE
  sidsToLoad <<- as.integer(batchLoadData[[1]])
  showBatchLoadDialog(length(sidsToLoad), batchLoadFilters, maxSolversPaver,
    exclAttribChoices = exclAttribChoices,
    customScripts = config$scripts$hcube,
    colNamesForNaming = setNames(names(batchLoadData)[-1], names(batchLoadFilters)[-1])
  )
})
downloadBatchData <- function(file, sids, type) {
  if (!length(sids)) {
    flog.warn("No scenario IDs to download could be found.")
    return(downloadHandlerError(file))
  }
  if (length(sids) > hcubeLoadMaxScen) {
    flog.warn("Maximum number of scenarios to download is exceeded.")
    return(downloadHandlerError(file))
  }
  if (length(input$batchCompareNameCols)) {
    if (tryCatch(
      {
        mapColIds <- match(input$batchCompareNameCols, names(batchLoadData))
        if (any(is.na(mapColIds))) {
          stop(sprintf(
            "Setting scenIdNameMap: Invalid column(s): %s. This is likely an attempt to tamper with the app!",
            paste(input$batchCompareNameCols[is.na(mapColIds)], collapse = ", ")
          ))
        }
        rowIdsToPick <- match(sidsToLoad, batchLoadData[[1]])
        if (any(is.na(rowIdsToPick))) {
          stop(sprintf(
            "Setting scenIdNameMap: Invalid sid(s): %s. This is likely an attempt to tamper with the app!",
            paste(sidsToLoad[is.na(rowIdsToPick)], collapse = ", ")
          ))
        }
        batchLoader$setScenIdNameMap(setNames(unite(batchLoadData[rowIdsToPick, mapColIds],
          "name",
          sep = "_"
        )[[1]], as.character(sidsToLoad)))
        FALSE
      },
      error = function(e) {
        flog.error(
          "Unexpected error while setting scenIdNameMap. Error message: %s",
          conditionMessage(e)
        )
        return(TRUE)
      }
    )) {
      return(downloadHandlerError(file))
    }
  } else {
    batchLoader$setScenIdNameMap(character())
  }
  tmpDir <- file.path(tempdir(), paste0("dl_batch_", uid))
  on.exit(unlink(tmpDir, recursive = TRUE, force = TRUE), add = TRUE)
  if (dir.exists(tmpDir)) {
    unlink(tmpDir, recursive = TRUE, force = TRUE)
  }
  if (!dir.create(tmpDir)) {
    flog.error("Temporary folder could not be created")
    return(downloadHandlerError(file))
  }
  prog <- Progress$new()
  on.exit(prog$close(), add = TRUE)
  prog$set(message = lang$nav$dialogHcube$waitDialog$title, value = 0)
  updateProgress <- function(incAmount, detail = NULL) {
    prog$inc(amount = incAmount, detail = detail)
  }
  tryCatch(
    {
      if (identical(type, "csv")) {
        batchLoader$genCsvFiles(sidsToLoad, tmpDir, prog)
      } else {
        batchLoader$genGdxFiles(sidsToLoad, tmpDir, gdxio, prog)
      }
      return(zipr(file, list.files(tmpDir, full.names = TRUE),
        compression_level = 6
      ))
    },
    error = function(e) {
      flog.error(sprintf(
        "Problems generating the data archive for download. Error message: %s",
        conditionMessage(e)
      ))
    }
  )
  return(downloadHandlerError(file))
}
output$btBatchDownloadGDX <- downloadHandler(
  filename = function() {
    paste0(modelName, "_data.zip")
  },
  content = function(file) {
    flog.debug("Button to download batch of scenarios (GDX) clicked.")
    return(downloadBatchData(file, sidsToLoad, "gdx"))
  },
  contentType = "application/zip"
)
output$btBatchDownloadCSV <- downloadHandler(
  filename = function() {
    paste0(modelName, "_data.zip")
  },
  content = function(file) {
    flog.debug("Button to download batch of scenarios (CSV) clicked.")
    return(downloadBatchData(file, sidsToLoad, "csv"))
  },
  contentType = "application/zip"
)

observeEvent(input$btBatchRemove, {
  if (hcubeRemoveConfirmed) {
    errMsg <- NULL
    disableEl(session, "#btBatchRemove")
    affectedRows <- 0L
    tryCatch(affectedRows <- db$deleteRows("_scenMeta", subsetSids = sidsToLoad),
      error = function(e) {
        flog.error(
          "Problems removing scenarios. Error message: %s",
          conditionMessage(e)
        )
        errMsg <<- TRUE
      }
    )
    if (!is.null(errMsg) || affectedRows < length(sidsToLoad)) {
      showHideEl(session, "#batchRemoveError", 4000L)
      return(NULL)
    }
    showHideEl(session, "#batchRemoveSuccess", 2000L)
    batchLoadData <<- batchLoadData[!batchLoadData[[1]] %in% sidsToLoad, ]
    rv$updateBatchLoadData <- rv$updateBatchLoadData + 1L
    hideEl(session, ".batch-load-content")
    hideModal(session, 2L)
  } else {
    hideEl(session, ".batch-load-content")
    showEl(session, ".batch-load-remove-content")
    hcubeRemoveConfirmed <<- TRUE
  }
})

observeEvent(input$btBatchLoadCancel, {
  flog.trace("Close batch load dialog button clicked.")
  if (length(config$scripts$hcube) &&
    any(scriptOutput$isRunning())) {
    for (script in config$scripts$hcube) {
      if (scriptOutput$isRunning(script$id)) {
        scriptOutput$interrupt(script$id)
      }
    }
  }
  removeModal()
})

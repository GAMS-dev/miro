hcubeRemoveConfirmed <- FALSE

inputType <- list(text = "_uid", date = c("_stime"), csv = "_stag")
keysRaw   <- NULL
scalarFields  <- NULL
scalarTables  <- NULL
scalarKeyTypeList <- list()
scalarsTabNameIn  <- gsub("_", "", modelName, fixed = TRUE) %+% "_" %+% scalarsFileName
scalarsTabNameOut <- gsub("_", "", modelName, fixed = TRUE) %+% "_" %+% scalarsOutName
modelInSorted <- sort(names(modelIn))
modelOutSorted <- sort(modelOut[[scalarsTabNameOut]]$symnames)

appendInputTypeList <- function(scalarsTabName){
  inputType$text <<- c(inputType$text, vapply(scalarKeyTypeList[[scalarsTabName]], 
                                             function(el) if(el$type %in% c("set", "string", "acronym")) "_" %+% el$key else NA_character_, 
                                             character(1L), USE.NAMES = FALSE))
  inputType$text <<- inputType$text[!is.na(inputType$text)]
  inputType$number <<- c(inputType$number, vapply(scalarKeyTypeList[[scalarsTabName]], 
                                                 function(el) if(el$type %in% c("scalar", "parameter", "number")) "_" %+% el$key else NA_character_, 
                                                 character(1L), USE.NAMES = FALSE))
  inputType$number <<- inputType$number[!is.na(inputType$number)]
}

k <- 1L
for(j in seq_along(modelIn)){
  i <- match(modelInSorted[[j]], names(modelIn))
  if(!is.null(modelIn[[i]]$daterange)){
    scalarKeyTypeList[[scalarsTabNameIn]][[k]] <- list(key = names(modelIn)[[i]] %+% "_lo", type = "string", alias = modelInAlias[[i]] %+% " (lower)")
    scalarKeyTypeList[[scalarsTabNameIn]][[k + 1L]] <- list(key = names(modelIn)[[i]] %+% "_up", type = "string", alias = modelInAlias[[i]] %+% " (upper)")
    k <- k + 2L
  }else if(!is.null(modelIn[[i]]$slider) && identical(modelIn[[i]]$slider$double, TRUE)){
    scalarKeyTypeList[[scalarsTabNameIn]][[k]] <- list(key = names(modelIn)[[i]] %+% "_lo", type = "number", alias = modelInAlias[[i]] %+% " (lower)")
    scalarKeyTypeList[[scalarsTabNameIn]][[k + 1L]] <- list(key = names(modelIn)[[i]] %+% "_up", type = "number", alias = modelInAlias[[i]] %+% " (upper)")
    k <- k + 2L
  }else if(!is.null(modelIn[[i]]$daterange)){
    scalarKeyTypeList[[scalarsTabNameIn]][[k]] <- list(key = names(modelIn)[[i]] %+% "_lo", type = "string", alias = modelInAlias[[i]] %+% " (lower)")
    scalarKeyTypeList[[scalarsTabNameIn]][[k + 1L]] <- list(key = names(modelIn)[[i]] %+% "_up", type = "string", alias = modelInAlias[[i]] %+% " (upper)")
    k <- k + 2L
  }else if(modelIn[[i]]$type %in% c("slider", "checkbox", "numericinput") || identical(modelIn[[i]]$dropdown$checkbox, TRUE)){
    scalarKeyTypeList[[scalarsTabNameIn]][[k]] <- list(key = names(modelIn)[[i]], type = "number", alias = modelInAlias[[i]])
    k <- k + 1L
  }else if(modelIn[[i]]$type %in% c("dropdown", "dropdowne", "date", "textinput") && 
           !identical(modelIn[[i]]$dropdown$single, FALSE)){
    scalarKeyTypeList[[scalarsTabNameIn]][[k]] <- list(key = names(modelIn)[[i]], type = "string", alias = modelInAlias[[i]])
    k <- k + 1L
  }
}

if(length(modelIn[[scalarsFileName]])){
  scalarKeyTypeList[[scalarsTabNameIn]] <- c(scalarKeyTypeList[[scalarsTabNameIn]], 
                                             lapply(seq_along(modelIn[[scalarsFileName]]$symnames), function(i){
                                               list(key = modelIn[[scalarsFileName]]$symnames[[i]], 
                                                   type = modelIn[[scalarsFileName]]$symtypes[[i]], 
                                                   alias = modelIn[[scalarsFileName]]$symtext[[i]])
                                             }))
}

if(length(scalarKeyTypeList[[scalarsTabNameIn]])){
  appendInputTypeList(scalarsTabNameIn)
  scalarFields        <- scalarsTabNameIn %+% "._" %+% 
    vapply(scalarKeyTypeList[[scalarsTabNameIn]], "[[", character(1L), "key", USE.NAMES = FALSE)
  names(scalarFields) <- vapply(scalarKeyTypeList[[scalarsTabNameIn]],
                                "[[", character(1L), "alias", USE.NAMES = FALSE)
  scalarTables <- scalarsTabNameIn
}
scalarOutFields <- NULL
if(length(modelOut[[scalarsOutName]])){
  scalarKeyTypeList[[scalarsTabNameOut]] <- lapply(seq_along(modelOut[[scalarsOutName]]$symnames), function(i){
    list(key = modelOut[[scalarsOutName]]$symnames[[i]], 
         type = modelOut[[scalarsOutName]]$symtypes[[i]], 
         alias = modelOut[[scalarsOutName]]$symtext[[i]])
  })
  scalarKeyTypeList[[scalarsTabNameOut]] <- scalarKeyTypeList[[scalarsTabNameOut]][order(vapply(scalarKeyTypeList[[scalarsTabNameOut]], 
                                                                                                "[[", character(1L), "key", USE.NAMES = FALSE))]
  appendInputTypeList(scalarsTabNameOut)
  scalarOutFields        <- scalarsTabNameOut %+% "._" %+% 
                             vapply(scalarKeyTypeList[[scalarsTabNameOut]], "[[", character(1L), "key", USE.NAMES = FALSE)
  names(scalarOutFields) <- vapply(scalarKeyTypeList[[scalarsTabNameOut]],
                                   "[[", character(1L), "alias", USE.NAMES = FALSE)
  scalarFields <- c(scalarFields, scalarOutFields)
  scalarTables <- c(scalarTables, scalarsTabNameOut)
}

hcubeLoad <- HcubeLoad$new(db, scalarsFileHeaders[c(1, 3)], modelName,
                           inputDsNamesNotToDisplay,
                           scalarTables, scalarKeyTypeList)
metaCols <- db$getScenMetaColnames()
fields <- c("", scenMetadataTable %+% "." %+% metaCols[c("uid", "stime", "stag")])
names(fields) <- c("", lang$nav$hcubeLoad$metaColAliases$uid, 
                   lang$nav$hcubeLoad$metaColAliases$stime,
                   lang$nav$hcubeLoad$metaColAliases$stag)
fields <- c(fields, scalarFields)

maxNumBlocks   <- 5L
activeBlocks   <- vector("logical", maxNumBlocks)
activeLines    <- vector("logical", maxNumBlocks^2)
fieldsSelected <- vector("character", maxNumBlocks^2)
tmpOutputKeys  <- vapply(scalarKeyTypeList[[scalarsTabNameOut]], 
                         "[[", character(1L), "key", USE.NAMES = FALSE)
exclAttribChoices <- c(fields[2:4], scalarOutFields)
if(length(tmpOutputKeys))
  names(exclAttribChoices)[4:length(exclAttribChoices)] <- vapply(scalarKeyTypeList[[scalarsTabNameOut]],
                                                                  "[[", character(1L), "alias", USE.NAMES = FALSE)
rm(tmpOutputKeys)
hideEl(session, "#hcubeLoadButtons")

observeEvent(input$btNewBlock, {
  if(all(activeBlocks)){
    return(NULL)
  }
  i <- which.min(activeBlocks)
  activeBlocks[i] <<- TRUE
  addHcubeLoadBlock(id = i, choices = fields)
}, ignoreNULL = FALSE)
lapply(2:maxNumBlocks, function(i){
  observeEvent(input[["btRemoveBlock" %+% i]], {
    removeBlock(i)
  })
})
lapply(seq_len(maxNumBlocks), function(i){
  lapply(seq_len(maxNumBlocks), function(j){
    observeEvent(input[["btRemoveLine" %+% i %+% "_" %+% j]], {
      activeLines[j + (i - 1) * maxNumBlocks]  <<- FALSE
      removeUI(selector = "#line" %+% i %+% "_" %+% j)
      if(i > 1 && noLinesInBlock(i)){
        removeBlock(i)
      }
    })
  })
  observeEvent(input[["newLine_" %+% i]], {
    if(all(activeLines[((i - 1) * maxNumBlocks + 1):
                       ((i - 1) * maxNumBlocks + maxNumBlocks)]) ||
       input[["newLine_" %+% i]] == ""){
      return(NULL)
    }
    j <- which.min(activeLines[((i - 1) * maxNumBlocks + 1):
                                 ((i - 1) * maxNumBlocks + maxNumBlocks)])
    activeLines[j + (i - 1) * maxNumBlocks]  <<- TRUE
    label <- names(fields)[match(input[["newLine_" %+% i]], fields)]
    
    fieldsSelected[j + (i - 1) * maxNumBlocks] <<- input[["newLine_" %+% i]]
    field <- strsplit(input[["newLine_" %+% i]], ".", fixed = TRUE)[[1]][[2]]
    
    if(field %in% inputType[['text']]){
      ui <- generateLine(i, j, "text", label)
    }else if(field %in% inputType[['date']]){
      ui <- generateLine(i, j, "date", label)
    }else if(field %in% inputType[['csv']]){
      ui <- generateLine(i, j, "csv", label)
    }else{
      ui <- generateLine(i, j, "number", label)
    }
    insertUI(
      selector = "#blockContent" %+% i,
      where = "beforeEnd",
      ui = ui
    )
    updateSelectInput(session, "newLine_" %+% i, selected = "")
  })
})
observeEvent(input$btSendQuery, {
  disableEl(session, "#btSendQuery")
  showEl(session, "#hyperQueryLoad")
  a <- 1L
  c <- 1L
  subsetCoditions <- NULL
  subsetScalars   <- NULL
  lapply(seq_len(maxNumBlocks), function(i){
    if(!activeBlocks[i]){
      return(NULL)
    }
    b <- 1L
    table       <- NULL
    field       <- NULL
    val         <- NULL
    op          <- NULL
    lapply(seq_len(maxNumBlocks), function(j){
      if(!activeLines[j + (i - 1) * maxNumBlocks]){
        return(NULL)
      }
      tableField <- strsplit(fieldsSelected[j + (i - 1) * maxNumBlocks], 
                             ".", fixed = TRUE)[[1]]
      table[b]  <<- tableField[[1]]
      field[b]  <<- tableField[[2]]
      op[b]     <<- input[["op_" %+% i %+% "_" %+% j]]
      if(grepl("%", op[b])[[1]] && inherits(db$getConn(), "PostgreSQL")){
        switch(op[b],
               "%LIKE" = {
                 val[b] <<- "%" %+% 
                   db$escapePatternPivot(input[["val_" %+% i %+% "_" %+% j]])
                 op[b]  <<- "LIKE"
               },
               "LIKE%" = {
                 val[b] <<- db$escapePatternPivot(input[["val_" %+% i %+% "_" %+% j]]) %+% "%"
                 op[b]  <<- "LIKE"
               },
               "%LIKE%" = {
                 val[b] <<- "%" %+% 
                   db$escapePatternPivot(input[["val_" %+% i %+% "_" %+% j]]) %+% "%"
                 op[b]  <<- "LIKE"
               },
               "%NOTLIKE%" = {
                 val[b] <<- "%" %+% 
                   db$escapePatternPivot(input[["val_" %+% i %+% "_" %+% j]]) %+% "%"
                 op[b]  <<- "NOT LIKE"
               },
               ",LIKE%" = {
                 val[b] <<- "%," %+% db$escapePatternPivot(input[["val_" %+% i %+% "_" %+% j]]) %+% "%"
                 op[b]  <<- "LIKE"
               },
               "%LIKE," = {
                 val[b] <<- "%" %+% 
                   db$escapePatternPivot(input[["val_" %+% i %+% "_" %+% j]]) %+% ",%"
                 op[b]  <<- "LIKE"
               },
               "%,LIKE,%" = {
                 val[b] <<- "%," %+% 
                   db$escapePatternPivot(input[["val_" %+% i %+% "_" %+% j]]) %+% ",%"
                 op[b]  <<- "LIKE"
               },
               "%,NOTLIKE,%" = {
                 val[b] <<- "%," %+% 
                   db$escapePatternPivot(input[["val_" %+% i %+% "_" %+% j]]) %+% ",%"
                 op[b]  <<- "NOT LIKE"
               })
      }else if(identical(op[b], "BETWEEN")){
        table[b + 1] <<- tableField[[1]]
        field[b + 1] <<- tableField[[2]]
        val[b]     <<- as.character(input[["val_" %+% i %+% "_" %+% j]][[1]])
        val[b + 1] <<- as.character(input[["val_" %+% i %+% "_" %+% j]][[2]] + 1)
        op[b]      <<- ">="
        op[b + 1]  <<- "<"
        b <<- b + 2L
        return(NULL)
      }else{
        val[b] <<- input[["val_" %+% i %+% "_" %+% j]]
      }
      b <<- b + 1L
    })
    if(length(field)){
      subsetCoditions[[a]] <<- tibble(field, val, op, table)
    }else{
      subsetCoditions[[a]] <<- tibble()
    }
    a <<- a + 1L
  })
  colsToFetch <- strsplit(fields[-1], ".", fixed = TRUE)
  colN <- c(sidIdentifier, vapply(colsToFetch, 
                                   '[[', FUN.VALUE = "character", 2, 
                                   USE.NAMES = FALSE))
  names(colN) <- c(scenMetadataTable, vapply(colsToFetch, '[[', 
                                               FUN.VALUE = "character", 1,
                                               USE.NAMES = FALSE))
  tryCatch({
    rv$fetchedScenarios <- hcubeLoad$fetchResults(subsetCoditions, colNames = colN, limit = hcubeLoadMaxScen)
  }, error = function(e){
    if(identical(conditionMessage(e), "maxNoRowsVio")){
      errMsg <- sprintf("Your query results in too many scenarios to be fetched from the database. The maximum number of scenarios to be fetched is: %d. Please narrow your search.", 
                        hcubeLoadMaxScen)
    }else{
      errMsg <- "An error occurred while executing the database query. " %+%
        "Please try again or contact the system administrator in case this problem persists."
    }
    showErrorMsg("Error fetching data", errMsg)
    flog.warn("Problems executing hcubeLoad query. Error message: %s.", e)
  })
  if(length(isolate(rv$fetchedScenarios)) && nrow(isolate(rv$fetchedScenarios))){
    showEl(session, "#hcubeLoadButtons")
    hideEl(session, "#hcubeLoadNoData")
  }else{
    showEl(session, "#hcubeLoadNoData")
    hideEl(session, "#hcubeLoadButtons")
  }
  hideEl(session, "#hyperQueryLoad")
  enableEl(session, "#btSendQuery")
})

if("DT" %in% (.packages())){
  output$hcubeLoadResults <- renderDataTable(
    if(length(rv$fetchedScenarios) && nrow(rv$fetchedScenarios)){
      data <- rv$fetchedScenarios[, -1]
      datatable(
        data, filter = "bottom", colnames = names(fields)[-1], rownames = FALSE) %>%
      formatDate(2L,  method = "toLocaleString") %>%
        formatRound(seq(4, length(data))[vapply(data[, seq(4, length(data))], is.numeric, logical(1L), USE.NAMES = FALSE)], 
                    digits = roundPrecision)
    }
  )
}else{
  output$hcubeLoadResults <- renderDataTable({
    if(length(rv$fetchedScenarios) && nrow(rv$fetchedScenarios)){
      rv$fetchedScenarios[, -1]
    }
  }, options = list(filter = "bottom", colnames = names(fields)[-1], rownames = FALSE))
}

observeEvent(input$btShowHash, {
  flog.debug("Button to show hash of selected scenario (Hypercube load) clicked.")
  selectedRows <- isolate(input$hcubeLoadResults_rows_selected)
  if(!length(selectedRows) || length(selectedRows) > 1L){
    showHideEl(session, "#showHashOnlyOne", 4000L)
    return()
  }
  noErr <- TRUE
  tryCatch(
    hashValue <- db$importDataset(db$getDbSchema()$tabName[['_scenMeta']], colNames = snameIdentifier, 
                                  tibble(scodeIdentifier, SCODEMAP[['scen']], ">"),
                                  subsetSids = rv$fetchedScenarios[[1]][selectedRows])[[1]]
  , error = function(e){
    flog.error("Problems fetching hash value from database. Error message: '%s'.", e)
    showHideEl(session, "#showHashError", 4000L)
    noErr <<- FALSE
  })
  if(!noErr){
    return()
  }
  if(!length(hashValue)){
    showHideEl(session, "#showNoHashError", 4000L)
    return()
  }
  showHashDialog(hashValue)
})

observeEvent(input$hcubeLoadSelected, {
  flog.debug("Button to load selected scenarios (Hypercube load) clicked.")
  if(!length(input$hcubeLoadResults_rows_selected)){
    return()
  }
  hcubeRemoveConfirmed <<- FALSE
  sidsToLoad <<- as.integer(rv$fetchedScenarios[[1]][input$hcubeLoadResults_rows_selected])
  showHcubeLoadMethodDialog(length(sidsToLoad), fields, maxSolversPaver, maxConcurentLoad,
                            hasRemovePerm = TRUE, exclAttribChoices = exclAttribChoices,
                            customScripts = config$scripts$hcube)
})
observeEvent(input$hcubeLoadCurrent, {
  flog.debug("Button to load current page of scenarios (Hypercube load) clicked.")
  if(is.null(input$hcubeLoadResults_rows_current)){
    return()
  }
  hcubeRemoveConfirmed <<- FALSE
  sidsToLoad <<- as.integer(rv$fetchedScenarios[[1]][input$hcubeLoadResults_rows_current])
  showHcubeLoadMethodDialog(length(sidsToLoad), fields, maxSolversPaver, maxConcurentLoad,
                            hasRemovePerm = TRUE, exclAttribChoices = exclAttribChoices,
                            customScripts = config$scripts$hcube)
})
observeEvent(input$hcubeLoadAll, {
  flog.debug("Button to load all scenarios (Hypercube load) clicked.")
  if(!length(rv$fetchedScenarios) || !nrow(rv$fetchedScenarios)){
    return()
  }
  hcubeRemoveConfirmed <<- FALSE
  sidsToLoad     <<- as.integer(rv$fetchedScenarios[[1]])
  showHcubeLoadMethodDialog(length(sidsToLoad), fields, maxSolversPaver, maxConcurentLoad, 
                            hasRemovePerm = TRUE, exclAttribChoices = exclAttribChoices,
                            customScripts = config$scripts$hcube)
})

output$btHcubeDownloadConfirm <- downloadHandler(
  filename = function() {
    tolower(modelName) %+% "_data.zip"
  },
  content = function(file) {
    flog.debug("Button to download Hypercube job files clicked.")
    
    if(!length(sidsToLoad)){
      flog.warn("No scenario IDs to download could be found.")
      return(downloadHandlerError(file))
    }
    if(length(sidsToLoad) > hcubeLoadMaxScen){
      flog.warn("Maximum number of scenarios to download is exceeded.")
      return(downloadHandlerError(file))
    }
    tmpDir      <- file.path(tempdir(), "scenDL")
    on.exit(unlink(tmpDir, recursive = TRUE, force = TRUE), add = TRUE)
    if(dir.exists(tmpDir)){
      unlink(tmpDir, recursive = TRUE, force = TRUE)
    }
    if(!dir.create(tmpDir)){
      flog.error("Temporary folder could not be created")
      return(downloadHandlerError(file))
    }
    
    prog <- Progress$new()
    on.exit(prog$close(), add = TRUE)
    prog$set(message = lang$nav$dialogHcube$waitDialog$title, value = 0)
    updateProgress <- function(incAmount, detail = NULL) {
      prog$inc(amount = incAmount, detail = detail)
    }
    tryCatch({
      if(identical(isolate(input$selExportFiletype), "csv")){
        hcubeLoad$genCsvFiles(sidsToLoad, tmpDir, prog)
      }else{
        hcubeLoad$genGdxFiles(sidsToLoad, tmpDir, gdxio, prog)
      }
      return(zipr(file, list.files(tmpDir, full.names = TRUE), 
                  compression_level = 6))
    }, error = function(e){
      flog.error(e)
    })
    return(downloadHandlerError(file))
  },
  contentType = "application/zip")

noLinesInBlock <- function(blockId){
  if(any(activeLines[((blockId - 1) * maxNumBlocks + 1):
                     ((blockId - 1) * maxNumBlocks + maxNumBlocks)])){
    return(FALSE)
  }else{
    return(TRUE)
  }
}
removeBlock <- function(blockId){
  activeBlocks[blockId] <<- FALSE
  activeLines[(blockId - 1) * maxNumBlocks + 1]  <<- FALSE
  removeUI(selector = "#block" %+% blockId)
}

observeEvent(input$btHcubeRemove, {
  if(hcubeRemoveConfirmed){
    errMsg <- NULL
    disableEl(session, "#btHcubeRemove")
    affectedRows <- 0L
    tryCatch(affectedRows <- db$deleteRows(db$getTableNameMetadata(), subsetSids = sidsToLoad), 
             error = function(e){
      flog.error("Problems removing Hypercube scenarios. Error message: %s", e)
      errMsg <<- TRUE
    })
    if(!is.null(errMsg) || affectedRows < sidsToLoad){
      showHideEl(session, "#hcubeRemoveError", 4000L)
      return(NULL)
    }
    showHideEl(session, "#hcubeRemoveSuccess", 2000L)
    hideModal(session, 2L)
  }else{
    hideEl(session, "#btHcubeLoad")
    hideEl(session, "#hcubeLoadMethod")
    hideEl(session, "#btPaverConfig")
    hideEl(session, "#btHcubeDownload")
    showEl(session, "#hcubeRemoveConfirm")
    hcubeRemoveConfirmed <<- TRUE
  }
})

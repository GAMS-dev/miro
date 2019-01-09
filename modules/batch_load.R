hasBatchRemovePermission <- FALSE
batchRemoveConfirmed <- FALSE

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
  }else if(modelIn[[i]]$type %in% c("slider", "checkbox") || identical(modelIn[[i]]$dropdown$checkbox, TRUE)){
    scalarKeyTypeList[[scalarsTabNameIn]][[k]] <- list(key = names(modelIn)[[i]], type = "number", alias = modelInAlias[[i]])
    k <- k + 1L
  }else if(modelIn[[i]]$type %in% c("dropdown", "dropdowne", "date", "daterange")){
    scalarKeyTypeList[[scalarsTabNameIn]][[k]] <- list(key = names(modelIn)[[i]], type = "string", alias = modelInAlias[[i]])
    k <- k + 1L
  }
}

if(length(modelIn[[scalarsFileName]])){
  scalarKeyTypeList[[scalarsTabNameIn]] <- c(scalarKeyTypeList[[scalarsTabNameIn]], 
                                             lapply(seq_along(modelIn[[scalarsFileName]]$symnames), function(i){
                                               list(key = modelOut[[scalarsOutName]]$symnames[[i]], 
                                                   type = modelOut[[scalarsOutName]]$symtypes[[i]], 
                                                   alias = modelOut[[scalarsOutName]]$symtext[[i]])
                                             }))
}

if(length(scalarKeyTypeList[[scalarsTabNameIn]])){
  appendInputTypeList(scalarsTabNameIn)
  scalarFields        <- scalarsTabNameIn %+% "-_" %+% 
    vapply(scalarKeyTypeList[[scalarsTabNameIn]], "[[", character(1L), "key", USE.NAMES = FALSE)
  names(scalarFields) <- vapply(scalarKeyTypeList[[scalarsTabNameIn]],
                                "[[", character(1L), "alias", USE.NAMES = FALSE)
  scalarTables <- scalarsTabNameIn
}
if(length(modelOut[[scalarsOutName]])){
  scalarKeyTypeList[[scalarsTabNameOut]] <- lapply(seq_along(modelOut[[scalarsOutName]]$symnames), function(i){
    list(key = modelOut[[scalarsOutName]]$symnames[[i]], 
         type = modelOut[[scalarsOutName]]$symtypes[[i]], 
         alias = modelOut[[scalarsOutName]]$symtext[[i]])
  })
  scalarKeyTypeList[[scalarsTabNameOut]] <- scalarKeyTypeList[[scalarsTabNameOut]][order(vapply(scalarKeyTypeList[[scalarsTabNameOut]], 
                                                                                                "[[", character(1L), "key", USE.NAMES = FALSE))]
  appendInputTypeList(scalarsTabNameOut)
  scalarOutFields        <- scalarsTabNameOut %+% "-_" %+% 
                             vapply(scalarKeyTypeList[[scalarsTabNameOut]], "[[", character(1L), "key", USE.NAMES = FALSE)
  names(scalarOutFields) <- vapply(scalarKeyTypeList[[scalarsTabNameOut]],
                                   "[[", character(1L), "alias", USE.NAMES = FALSE)
  scalarFields <- c(scalarFields, scalarOutFields)
  scalarTables <- c(scalarTables, scalarsTabNameOut)
}

batchLoad <- BatchLoad$new(db, scalarsFileHeaders[c(1, 3)],
                           scalarTables, scalarKeyTypeList)
metaCols <- db$getScenMetaColnames()
fields <- c("", scenMetadataTable %+% "-" %+% metaCols[c("uid", "stime", "stag")])
names(fields) <- c("", "Owner", "Date of creation", "Batch tags")
fields <- c(fields, scalarFields)

maxNumBlocks   <- 5L
activeBlocks   <- vector("logical", maxNumBlocks)
activeLines    <- vector("logical", maxNumBlocks^2)
fieldsSelected <- vector("character", maxNumBlocks^2)

hideEl(session, "#batchLoadButtons")

generateLine <- function(i, j, type, label){
  tags$div(id = "line" %+% i %+% "_" %+% j, class = "itemLine",
           tags$div(class = "itemName", helpText(label)),
           tags$div(class = "itemScenDrop", switch(type,
                           number = {
                             selectInput("op_" %+% i %+% "_" %+% j, label=NULL, 
                                         choices = c('=', '<', '>', '<=', ">=", '!='))
                           },
                           text = {
                             selectInput("op_" %+% i %+% "_" %+% j, label=NULL, 
                                         choices = c(contains = "%LIKE%", 
                                                     "doesn't contain" = "%NOTLIKE%",
                                                     "starts with" = "LIKE%",
                                                     "ends with" = "%LIKE",
                                                     is = "LIKE",
                                                     "is not" = "NOT LIKE"), selected = "LIKE")
                           },
                           csv = {
                             selectInput("op_" %+% i %+% "_" %+% j, label=NULL, 
                                         choices = c(contains = "%LIKE%", 
                                                     "doesn't contain" = "%NOTLIKE%",
                                                     "starts with" = ",LIKE%",
                                                     "ends with" = "%LIKE,",
                                                     is = "%,LIKE,%",
                                                     "is not" = "%,NOTLIKE,%"), selected = "%,LIKE,%")
                           },
                           date = {
                             selectInput("op_" %+% i %+% "_" %+% j, label=NULL, 
                                         choices = c(between = "%BETWEEN%"))
                           })
           ),
           tags$div(class = "itemSearchCrit",
                    switch(type,
                           number = {
                             numericInput("val_" %+% i %+% "_" %+% j, label=NULL)
                           },
                           date = {
                             dateRangeInput("val_" %+% i %+% "_" %+% j, label=NULL)
                           }, 
                           {
                             textInput("val_" %+% i %+% "_" %+% j, label=NULL)
                           })
                    
           ),
           tags$div(class = "itemDelete",
             actionButton("btRemoveLine" %+% i %+% "_" %+% j, label = "-", style = "background-color: #fff;")
           )
  )
}

observeEvent(input$btNewBlock, {
  if(all(activeBlocks)){
    return(NULL)
  }
  i <- which.min(activeBlocks)
  activeBlocks[i] <<- TRUE
  insertUI(
    selector = "#selectorsWrapper",
    where = "beforeEnd",
    ui = tags$div(id = "block" %+% i,
                  tags$div(id = "blockContent" %+% i, class = "grid-container",
                           if(i > 1L){
                             tags$hr()
                           }
                  ),
                  tags$div(class = "itemAndDrop",
                     tags$div(class = "itemAND", 
                              tags$span(style = "display:inline-block; vertical-align:middle; line-height: 70px;", 'AND')),
                     tags$div(class = "itemDropdown", 
                              selectInput("newLine_" %+% i, "", choices = fields)
                              ),
                     if(i > 1L){
                       tags$div(class = "itemDelete",
                                actionButton("btRemoveBlock" %+% i, label = "-", style = "background-color: #fff;"))
                     }
                  )
    )
  )
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
    field <- strsplit(input[["newLine_" %+% i]], "-", fixed = TRUE)[[1]][[2]]
    
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
  showEl(session, "#loadDiv")
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
                             "-", fixed = TRUE)[[1]]
      field[b]  <<- tableField[[2]]
      table[b]  <<- tableField[[1]]
      op[b]     <<- input[["op_" %+% i %+% "_" %+% j]]
      if(grepl("%", op[b])[[1]]){
        switch(op[b],
               "%LIKE" = {
                 val[b] <<- "%" %+% 
                   db$escapePattern(input[["val_" %+% i %+% "_" %+% j]])
                 op[b]  <<- "LIKE"
               },
               "LIKE%" = {
                 val[b] <<- db$escapePattern(input[["val_" %+% i %+% "_" %+% j]]) %+% "%"
                 op[b]  <<- "LIKE"
               },
               "%LIKE%" = {
                 val[b] <<- "%" %+% 
                   db$escapePattern(input[["val_" %+% i %+% "_" %+% j]]) %+% "%"
                 op[b]  <<- "LIKE"
               },
               "%NOTLIKE%" = {
                 val[b] <<- "%" %+% 
                   db$escapePattern(input[["val_" %+% i %+% "_" %+% j]]) %+% "%"
                 op[b]  <<- "NOT LIKE"
               },
               "%BETWEEN%" = {
                 table[b + 1] <<- tableField[[1]]
                 field[b + 1] <<- tableField[[2]]
                 val[b]     <<- as.character(input[["val_" %+% i %+% "_" %+% j]][[1]])
                 val[b + 1] <<- as.character(input[["val_" %+% i %+% "_" %+% j]][[2]] + 1)
                 op[b]      <<- ">="
                 op[b + 1]  <<- "<"
                 b <<- b + 2L
                 return(NULL)
               },
               ",LIKE%" = {
                 val[b] <<- "," %+% db$escapePattern(input[["val_" %+% i %+% "_" %+% j]]) %+% "%"
                 op[b]  <<- "LIKE"
               },
               "%LIKE," = {
                 val[b] <<- "%" %+% 
                   db$escapePattern(input[["val_" %+% i %+% "_" %+% j]]) %+% ","
                 op[b]  <<- "LIKE"
               },
               "%,LIKE,%" = {
                 val[b] <<- "%," %+% 
                   db$escapePattern(input[["val_" %+% i %+% "_" %+% j]]) %+% ",%"
                 op[b]  <<- "LIKE"
               },
               "%,NOTLIKE,%" = {
                 val[b] <<- "%," %+% 
                   db$escapePattern(input[["val_" %+% i %+% "_" %+% j]]) %+% ",%"
                 op[b]  <<- "NOT LIKE"
               })
      }else{
        val[b] <<- input[["val_" %+% i %+% "_" %+% j]]
      }
      b <<- b + 1L
    })
    if(length(field)){
      subsetCoditions[[a]] <<- tibble(field, val, op, table)
    }else{
      subsetCoditions[[a]] <<- tibble(field = sidIdentifier, val = 0L, op = ">", 
                                      table = scenMetadataTable)
    }
    a <<- a + 1L
  })
  colsToFetch <- strsplit(fields[-1], "-", fixed = TRUE)
  colN <- c(sidIdentifier, vapply(colsToFetch, 
                                   '[[', FUN.VALUE = "character", 2, 
                                   USE.NAMES = FALSE))
  names(colN) <- c(scenMetadataTable, vapply(colsToFetch, '[[', 
                                               FUN.VALUE = "character", 1,
                                               USE.NAMES = FALSE))
  tryCatch({
    rv$fetchedScenarios <- batchLoad$fetchResults(subsetCoditions, colNames = colN, limit = batchLoadMaxScen)
  }, error = function(e){
    if(identical(conditionMessage(e), "maxNoRowsVio")){
      errMsg <- sprintf("Your query results in too many scenarios to be fetched from the database. The maximum number of scenarios to be fetched is: %d. Please narrow your search.", 
                        batchLoadMaxScen)
    }else{
      errMsg <- "An error occurred while executing the database query. " %+%
        "Please try again or contact the system administrator in case this problem persists."
    }
    showErrorMsg("Error fetching data", errMsg)
    flog.warn("Problems executing batchLoad query. Error message: %s.", e)
  })
  if(length(isolate(rv$fetchedScenarios)) && nrow(isolate(rv$fetchedScenarios))){
    showEl(session, "#batchLoadButtons")
    hideEl(session, "#batchLoadNoData")
  }else{
    showEl(session, "#batchLoadNoData")
    hideEl(session, "#batchLoadButtons")
  }
  hideEl(session, "#loadDiv")
  enableEl(session, "#btSendQuery")
})
if("DT" %in% (.packages())){
  output$batchLoadResults <- renderDataTable({
    if(length(rv$fetchedScenarios) && nrow(rv$fetchedScenarios)){
      rv$fetchedScenarios[, -1]
    }
  }, filter = "bottom", colnames = names(fields)[-1], rownames = FALSE)
}else{
  output$batchLoadResults <- renderDataTable({
    if(length(rv$fetchedScenarios) && nrow(rv$fetchedScenarios)){
      rv$fetchedScenarios[, -1]
    }
  }, options = list(filter = "bottom", colnames = names(fields)[-1], rownames = FALSE))
}

observeEvent(input$batchLoadSelected, {
  flog.debug("Button to load selected scenarios (batch load) clicked.")
  if(is.null(input$batchLoadResults_rows_selected)){
    return(NULL)
  }
  batchRemoveConfirmed <<- FALSE
  sidsToLoad <<- as.integer(rv$fetchedScenarios[[1]][input$batchLoadResults_rows_selected])
  showBatchLoadMethodDialog(length(sidsToLoad), fields, maxSolversPaver, maxConcurentLoad,
                            hasRemovePerm = hasBatchRemovePermission)
})
observeEvent(input$batchLoadCurrent, {
  flog.debug("Button to load current page of scenarios (batch load) clicked.")
  if(is.null(input$batchLoadResults_rows_current)){
    return(NULL)
  }
  batchRemoveConfirmed <<- FALSE
  sidsToLoad <<- as.integer(rv$fetchedScenarios[[1]][input$batchLoadResults_rows_current])
  showBatchLoadMethodDialog(length(sidsToLoad), fields, maxSolversPaver, maxConcurentLoad,
                            hasRemovePerm = hasBatchRemovePermission)
})
observeEvent(input$batchLoadAll, {
  flog.debug("Button to load all scenarios (batch load) clicked.")
  if(!length(rv$fetchedScenarios) || !nrow(rv$fetchedScenarios)){
    return(NULL)
  }
  batchRemoveConfirmed <<- FALSE
  sidsToLoad     <<- as.integer(rv$fetchedScenarios[[1]])
  showBatchLoadMethodDialog(length(sidsToLoad), fields, maxSolversPaver, maxConcurentLoad, 
                            hasRemovePerm = hasBatchRemovePermission)
})

output$btBatchDownload <- downloadHandler(
  filename = function() {
    tolower(modelName) %+% "_data.zip"
  },
  content = function(file) {
    flog.debug("Button to download batch files clicked.")
    
    if(!length(sidsToLoad)){
      flog.warn("No scenario IDs to download could be found.")
      return(downloadHandlerError(file))
    }
    if(length(sidsToLoad) > batchLoadMaxScen){
      flog.warn("Maximum number of scenarios to download was exceeded.")
      return(downloadHandlerError(file))
    }
    wd <- getwd()
    on.exit(setwd(wd), add = TRUE)
    tmpDir      <- tempdir() %+% .Platform$file.sep %+% "scenDL"
    on.exit(unlink(tmpDir, recursive = TRUE, force = TRUE), add = TRUE)
    if(dir.exists(tmpDir)){
      unlink(tmpDir, recursive = TRUE, force = TRUE)
      Sys.sleep(0.5)
    }
    if(!dir.create(tmpDir)){
      flog.error("Temporary folder could not be created")
      return(downloadHandlerError(file))
    }
    
    
    setwd(tmpDir)
    prog <- Progress$new()
    on.exit(prog$close(), add = TRUE)
    prog$set(message = lang$nav$dialogBatch$waitDialog$title, value = 0)
    updateProgress <- function(incAmount, detail = NULL) {
      prog$inc(amount = incAmount, detail = detail)
    }
    tryCatch({
      batchLoad$genCsvFiles(sidsToLoad, tmpDir, prog)
      return(zip(file, list.files(recursive = TRUE), compression_level = 6))
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

observeEvent(input$btBatchRemove, {
  req(hasBatchRemovePermission)
  if(batchRemoveConfirmed){
    errMsg <- NULL
    disableEl(session, "#btBatchRemove")
    tryCatch(db$deleteRows(db$getTableNameMetadata(), subsetSids = sidsToLoad), error = function(e){
      flog.error("Problems removing batch scenarios. Error message: %s", e)
      errMsg <<- TRUE
    })
    if(!is.null(errMsg)){
      showEl(session, "#batchRemoveError")
      return(NULL)
    }
    hideEl(session, "#batchRemoveConfirm")
    showEl(session, "#batchRemoveSuccess")
    hideModal(session, 1L)
  }else{
    hideEl(session, "#btBatchLoad")
    hideEl(session, "#batchLoadMethod")
    hideEl(session, "#btPaverConfig")
    hideEl(session, "#btBatchDownload")
    showEl(session, "#batchRemoveConfirm")
    batchRemoveConfirmed <<- TRUE
  }
})

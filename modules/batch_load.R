inputType <- list(text = c("_stag", "_uid"), date = c("_stime"))
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

scalarKeyTypeList[[scalarsTabNameIn]] <- lapply(seq_along(modelIn), function(j){
  i <- match(modelInSorted[[j]], names(modelIn))
  if(modelIn[[i]]$type %in% c("slider", "checkbox") || identical(modelIn[[i]]$dropdown$checkbox, TRUE)){
    list(key = names(modelIn)[[i]], type = "number", alias = modelInAlias[[i]])
  }else if(modelIn[[i]]$type %in% c("dropdown", "dropdowne", "date", "daterange")){
    list(key = names(modelIn)[[i]], type = "string", alias = modelInAlias[[i]])
  }else{
    NA
  }
})
if(length(modelIn[[scalarsFileName]])){
  scalarKeyTypeList[[scalarsTabNameIn]] <- c(scalarKeyTypeList[[scalarsTabNameIn]], 
                                             lapply(seq_along(modelIn[[scalarsFileName]]$symnames), function(i){
                                               list(key = modelOut[[scalarsOutName]]$symnames[[i]], 
                                                   type = modelOut[[scalarsOutName]]$symtypes[[i]], 
                                                   alias = modelOut[[scalarsOutName]]$symtext[[i]])
                                             }))
}
scalarKeyTypeList[[scalarsTabNameIn]]   <- 
  scalarKeyTypeList[[scalarsTabNameIn]][!is.na(scalarKeyTypeList[[scalarsTabNameIn]])]
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

generateLine <- function(i, j, type, label, values = NULL){
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
                           date = {
                             selectInput("op_" %+% i %+% "_" %+% j, label=NULL, 
                                         choices = c(between = "%BETWEEN%"))
                           })
           ),
           tags$div(class = "itemSearchCrit",
                    switch(type,
                           number = {
                             numericInput("val_" %+% i %+% "_" %+% j, label=NULL, value = values[[1]])
                           },
                           text = {
                             if(length(values) > 20){
                               textInput("val_" %+% i %+% "_" %+% j, label=NULL)
                             }else{
                               selectInput("val_" %+% i %+% "_" %+% j, label=NULL, choices = values)
                             }
                           },
                           date = {
                             dateRangeInput("val_" %+% i %+% "_" %+% j, label=NULL)
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
      values <- batchLoad$fetchValues(input[["newLine_" %+% i]])
      ui <- generateLine(i, j, "text", label, values)
    }else if(field %in% inputType[['date']]){
      ui <- generateLine(i, j, "date", label)
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
                                   '[[', FUN.VALUE = "characer", 2, 
                                   USE.NAMES = FALSE))
  names(colN) <- c(scenMetadataTable, vapply(colsToFetch, '[[', 
                                               FUN.VALUE = "characer", 1,
                                               USE.NAMES = FALSE))
  tryCatch({
    rv$fetchedScenarios <- batchLoad$fetchResults(subsetCoditions, colNames = colN, limit = maxRecordsBatchLoad)
  }, error = function(e){
    errMsg <- "An error occurred while executing the database query. " %+%
      "Please try again or contact the system administrator in case this problem persists."
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
output$batchLoadResults <- renderDataTable({
  if(length(rv$fetchedScenarios) && nrow(rv$fetchedScenarios)){
    rv$fetchedScenarios[, -1]
  }
}, filter = "bottom", colnames = names(fields)[-1], rownames = FALSE)

observeEvent(input$batchLoadSelected, {
  if(is.null(input$batchLoadResults_rows_selected)){
    return(NULL)
  }
  sidsToLoad <<- as.integer(rv$fetchedScenarios[[1]][input$batchLoadResults_rows_selected])
  showBatchLoadMethodDialog(fields, maxSolversPaver, maxConcurentLoad)
})
observeEvent(input$batchLoadCurrent, {
  if(is.null(input$batchLoadResults_rows_current)){
    return(NULL)
  }
  sidsToLoad <<- as.integer(rv$fetchedScenarios[[1]][input$batchLoadResults_rows_current])
  showBatchLoadMethodDialog(fields, maxSolversPaver, maxConcurentLoad)
})
observeEvent(input$batchLoadAll, {
  if(!length(rv$fetchedScenarios) || !nrow(rv$fetchedScenarios)){
    return(NULL)
  }
  sidsToLoad     <<- as.integer(rv$fetchedScenarios[[1]])
  showBatchLoadMethodDialog(fields, maxSolversPaver, maxConcurentLoad)
})

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
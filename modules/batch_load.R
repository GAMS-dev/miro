inputType <- list(text = c("_stag", "_uid"), date = c("_stime"))
keysRaw   <- NULL
scalarKeyTypeList <- list()
scalarsTabNameIn  <- modelName %+% "_" %+% scalarsFileName
scalarsTabNameOut <- modelName %+% "_" %+% scalarsOutName
scalarKeyTypeList[[scalarsTabNameIn]] <- lapply(seq_along(modelIn), function(i){
  print(names(modelIn)[i] == scalarsFileName)
  if(modelIn[[i]]$type %in% c("slider", "checkbox") || identical(modelIn[[i]]$dropdown$checkbox, TRUE)){
    list(key = names(modelIn)[[i]], type = "number", alias = modelInAlias[[i]])
  }else if(modelIn[[i]]$type %in% c("dropdown", "dropdowne", "date", "daterange")){
    list(key = names(modelIn)[[i]], type = "string", alias = modelInAlias[[i]])
  }else if(names(modelIn)[i] %in% c(scalarsFileName, scalarsOutName)){
    aliasTypeVector <- vapply(modelIn[[i]]$content, function(j){
      return()
    })
    #list(key = names(modeln[[i]]$content), type = modeln[[i]]$content, alias = modelIn[[i]]$alias)
  }else{
    NA
  }
})
scalarKeyTypeList[[scalarsTabNameIn]]   <- 
  scalarKeyTypeList[[scalarsTabNameIn]][!is.na(scalarKeyTypeList[[scalarsTabNameIn]])]
scalarFields        <- scalarsTabNameIn %+% "-" %+% "_" %+% 
  unlist(lapply(scalarKeyTypeList[[scalarsTabNameIn]], "[[", "key"))
names(scalarFields) <- unlist(lapply(scalarKeyTypeList[[scalarsTabNameIn]],
                                     "[[", "alias"))
batchLoad <- BatchLoad$new(db, scalarsFileHeaders[c(1, 3)],
                           scalarsTabNameIn, scalarKeyTypeList)

#tables <- strsplit(names(fieldValueList), "-", fixed = TRUE)
#tables <- vapply(tables, "[[", character(1L), 2, USE.NAMES = FALSE)
#fields <- names(fieldValueList)
metaCols <- db$getScenMetaColnames()
fields <- c("", scenMetadataTable %+% "-" %+% metaCols[c("uid", "stime", "stag")])
names(fields) <- c("", "Owner", "Date of creation", "Batch tags")
fields <- c(fields, scalarFields)
#names(fields) <- tables
#rm(tables)

maxNumBlocks   <- 5L
activeBlocks   <- vector("logical", maxNumBlocks)
activeLines    <- vector("logical", maxNumBlocks^2)
fieldsSelected <- vector("character", maxNumBlocks^2)

shinyjs::hide("batchLoadButtons")

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
                                                     "is not" = "NOT LIKE"), selected = "%LIKE%")
                           },
                           date = {
                             selectInput("op_" %+% i %+% "_" %+% j, label=NULL, 
                                         choices = c(between = "%BETWEEN%"))
                           })
           ),
           tags$div(class = "itemSearchCrit",
                    switch(type,
                           number = {
                             if(length(values) > 20){
                               numericInput("val_" %+% i %+% "_" %+% j, label=NULL, value = values[[1]])
                             }else{
                               selectInput("val_" %+% i %+% "_" %+% j, label=NULL, choices = values)
                             }
                           },
                           text = {
                             textInput("val_" %+% i %+% "_" %+% j, label=NULL)
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
  #j <- which.min(activeLines[((i - 1) * maxNumBlocks + 1):
  #                             ((i - 1) * maxNumBlocks + maxNumBlocks)])
  #activeLines[j + (i - 1) * maxNumBlocks]  <<- TRUE
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
    #observeEvent(input[["field_" %+% i %+% "_" %+% j]], {
    #  values <- batchLoad$fetchValues(input[["field_" %+% i %+% "_" %+% j]])
    #  updateSelectInput(session, "val_" %+% i %+% "_" %+% j, 
    #                    choices = values)
    #})
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
    }else{
      values <- batchLoad$fetchValues(input[["newLine_" %+% i]])
      ui <- generateLine(i, j, "number", label, values)
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
  disable("btSendQuery")
  shinyjs::show("loadDiv")
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
    rv$fetchedScenarios <- batchLoad$fetchResults(subsetCoditions, colNames = colN)
  }, error = function(e){
    errMsg <- "An error occurred while executing the database query. " %+%
      "Please try again or contact the system administrator in case this problem persists."
    showErrorMsg("Error fetching data", errMsg)
    flog.warn("Problems executing batchLoad query. Error message: %s.", e)
  })
  
  hide("loadDiv")
  shinyjs::show("batchLoadButtons")
  enable("btSendQuery")
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
  sidsToLoad <<- rv$fetchedScenarios[[1]][input$batchLoadResults_rows_selected]
  showBatchLoadMethodDialog()
})
observeEvent(input$batchLoadCurrent, {
  if(is.null(input$batchLoadResults_rows_current)){
    return(NULL)
  }
  sidsToLoad <<- rv$fetchedScenarios[[1]][input$batchLoadResults_rows_current]
  showBatchLoadMethodDialog()
})
observeEvent(input$batchLoadAll, {
  if(!length(rv$fetchedScenarios) || !nrow(rv$fetchedScenarios)){
    return(NULL)
  }
  sidsToLoad     <<- rv$fetchedScenarios[[1]]
  showBatchLoadMethodDialog()
})
showBatchLoadMethodDialog <- function(){
  showModal(modalDialog(
    title = "Select scenario comparison method",
    if(length(sidsToLoad) <= maxConcurentLoad){
      "Which method would you like to use in order to compare your scenarios?"
    }else{
      "You may only select up to: %d scenarios to load into interactive mode at once." %+% 
" Select less scenarios or use the Paver module to compare."
    },
    footer = tagList(
      modalButton("Cancel"),
      actionButton("btPaver", label = "Paver"),
      if(length(sidsToLoad) <= maxConcurentLoad)
        actionButton("btBatchLoad", label = "Interactive mode")
    ),
    fade = TRUE, easyClose = TRUE
  ))
}

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
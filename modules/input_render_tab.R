# render tabular datasets
proxy <- vector("list", length(modelIn))

getInputDataset <- function(id){
  if((!is.null(isolate(input[["in_" %+% id]])) && hotInit[[id]]) ||
     (length(tableContent[[id]]))){
    if(length(colsWithDep[[id]])){
      if(!isEmptyInput[id]){
        if(modelIn[[id]]$type == "hot"){
          inputData <- bind_rows(hot_to_r(isolate(input[["in_" %+% id]])), 
                                 modelInputData[[id]])
        }else{
          inputData <- bind_rows(tableContent[[id]], 
                                 modelInputData[[id]])
        }
      }else{
        inputData <- modelInputData[[id]]
      }
    }else{
      if(!isEmptyInput[id]){
        if(modelIn[[id]]$type == "hot"){
          inputData <- hot_to_r(isolate(input[["in_" %+% id]]))
        }else{
          inputData <- tableContent[[id]]
        }
      }else{
        inputData <- modelInTemplate[[id]]
      }
    }
  }else if(!is.null(modelInputData[[id]])){
    # tab was never activated, so shiny does not update handsontable thus it is 
    # empty although data was loaded
    inputData <- modelInputData[[id]]
  }else{
    stop("No input data found.", call. = FALSE)
  }
  return(inputData)
}

lapply(modelInTabularData, function(sheet){
  # get input element id of dataset
  i <- match(sheet, tolower(names(modelIn)))[[1]]
  
  observeEvent(input[["btGraphIn" %+% i]], {
    if(identical(modelIn[[i]]$type, "hot")){
      data <- hot_to_r(input[["in_" %+% i]])
    }else{
      data <- tableContent[[i]]
    }
    toggleEl(session, "#graph-in_" %+% i)
    toggleEl(session, "#data-in_" %+% i)
    errMsg <- NULL
    tryCatch({
      callModule(renderData, "in_" %+% i, 
                 type = configGraphsIn[[i]]$outType, 
                 data = data,
                 dtOptions = config$datatable, 
                 graphOptions = configGraphsIn[[i]]$graph, 
                 pivotOptions = configGraphsIn[[i]]$pivottable, 
                 customOptions = configGraphsIn[[i]]$options,
                 roundPrecision = roundPrecision, modelDir = modelDir)
    }, error = function(e) {
      flog.error("Problems rendering output charts and/or tables for dataset: '%s'. Error message: %s.", 
                 modelInAlias[i], e)
      errMsg <<- sprintf(lang$errMsg$renderGraph$desc, modelInAlias[i])
    })
    showErrorMsg(lang$errMsg$renderGraph$title, errMsg)
  })
  
  switch(modelIn[[i]]$type,
         hot = {
           if(length(colsWithDep[[i]])){
             dataModelIn[[i]] <- reactive({
               hotInit[[i]] <<- TRUE
               # make sure data will be updated when old data is overwritten
               rv[["in_" %+% i]]
               if(isEmptyInput[i]){
                 data <- modelInputData[[i]]
               }else{
                 # save changes made in handsontable
                 if(!is.null(isolate(input[["in_" %+% i]]))){
                   hotInput[[i]] <<- as_tibble(hot_to_r(isolate(input[["in_" %+% i]])))
                 }
                 tryCatch({
                   data <- bind_rows(hotInput[[i]], modelInputData[[i]])
                 }, error = function(e){
                   if(debugMode){
                     errMsg <<- paste(errMsg, paste(lang$errMsg$dataError$desc,
                                                    e, sep = "\n"), sep = "\n")
                   }else{
                     errMsg <<- paste(errMsg, lang$errMsg$dataError$desc, sep = "\n")
                   }
                 })
                 modelInputData[[i]] <<- data
               }
               for(iDep in seq_along(colsWithDep[[i]])){
                 # get id of element (e.g. dropdown menu) that causes backward dependency
                 id  <- colsWithDep[[i]][[iDep]]
                 # in case nothing was selected in dropdown menu, skip this iteration
                 if(is.null(input[["dropdown_" %+% id]]) || input[["dropdown_" %+% id]] %in% c("","_")){
                   next
                 }
                 # get column name with dependency
                 col <- names(colsWithDep[[i]])[[iDep]]
                 # filter data frame
                 data <- data[data[[col]] %in% input[["dropdown_" %+% id]], ]
               }
               modelInputData[[i]] <<- anti_join(modelInputData[[i]], data, by = idsIn[[i]])
               if(!nrow(data)){
                 data[1, ] <- ""
                 disableEl(session, paste0("#btGraphIn", i))
                 isEmptyInput[i] <<- TRUE
               }else{
                 enableEl(session, paste0("#btGraphIn", i))
                 isEmptyInput[i] <<- FALSE
               }
               return(data)
             })
           }else{
             dataModelIn[[i]] <- reactive({
               rv[["in_" %+% i]]
               hotInit[[i]] <<- TRUE
               if(!nrow(modelInputData[[i]])){
                 modelInputData[[i]][1, ] <<- ""
                 disableEl(session, paste0("#btGraphIn", i))
                 isEmptyInput[i] <<- TRUE
               }else{
                 enableEl(session, paste0("#btGraphIn", i))
                 isEmptyInput[i] <<- FALSE
               }
               return(modelInputData[[i]])
             })
             
           }
           # rendering handsontables for input data 
           output[[paste0("in_", i)]] <- renderRHandsontable({
             noCheck[i] <<- TRUE
             ht <- rhandsontable(dataModelIn[[i]](), height = hotOptions$height, 
                                 width = hotOptions$width, search = hotOptions$search, 
                                 readOnly = modelIn[[i]]$readonly, selectCallback = TRUE)
             ht <- hot_table(ht, contextMenu = hotOptions$contextMenu$enabled, 
                             highlightCol = hotOptions$highlightCol, 
                             highlightRow = hotOptions$highlightRow,
                             rowHeaderWidth = hotOptions$rowHeaderWidth, 
                             enableComments = hotOptions$enableComments, 
                             stretchH = hotOptions$stretchH,
                             overflow = hotOptions$overflow)
             ht <- hot_context_menu(ht, allowRowEdit = hotOptions$contextMenu$allowRowEdit, 
                                    allowColEdit = hotOptions$contextMenu$allowColEdit, 
                                    allowReadOnly = hotOptions$contextMenu$allowReadOnly, 
                                    allowComments = hotOptions$contextMenu$allowComments)
             ht <- hot_cols(ht, columnSorting = hotOptions$columnSorting, 
                            manualColumnMove = hotOptions$manualColumnMove, 
                            manualColumnResize = hotOptions$manualColumnResize, 
                            colWidths = hotOptions$colWidths, 
                            fixedColumnsLeft = hotOptions$fixedColumnsLeft)
             
             # check for readonly columns
             colsReadonly <- vapply(seq_along(modelIn[[i]]$headers), function(j){
               if(identical(modelIn[[i]]$headers[[j]]$readonly, TRUE)){
                 names(modelIn[[i]]$headers)[[j]]
               }else{
                 NA_character_
               }
             }, character(1L), USE.NAMES = FALSE)
             colsReadonly <- colsReadonly[!is.na(colsReadonly)]
             if(length(colsReadonly)){
               return(hot_col(ht, colsReadonly, readOnly = TRUE))
             }else{
               return(ht)
             }
           })
         },
         dt = {
           if(length(colsWithDep[[i]])){
             dataModelIn[[i]] <- reactive({
               # make sure data will be updated when old data is overwritten
               rv[["in_" %+% i]]
               if(isEmptyInput[i]){
                 data <- modelInputData[[i]]
               }else{
                 tryCatch({
                   data <- bind_rows(tableContent[[i]], modelInputData[[i]])
                 }, error = function(e){
                   if(debugMode){
                     errMsg <<- paste(errMsg, paste(lang$errMsg$dataError$desc, e, sep = "\n"), 
                                      sep = "\n")
                   }else{
                     errMsg <<- paste(errMsg, lang$errMsg$dataError$desc, sep = "\n")
                   }
                 })
                 modelInputData[[i]] <<- data
               }
               
               for(idDep in seq_along(colsWithDep[[i]])){
                 # get id of element (e.g. dropdown menu) that causes backward dependency
                 id  <- colsWithDep[[i]][[idDep]]
                 # in case nothing was selected in dropdown menu, skip this iteration
                 if(is.null(input[["dropdown_" %+% id]]) || 
                    input[["dropdown_" %+% id]] %in% c("","_")){
                   next
                 }
                 # get column name with dependency
                 col <- names(colsWithDep[[1]])[[idDep]]
                 # filter data frame
                 data <- data[data[[col]] %in% input[["dropdown_" %+% id]], ]
               }
               modelInputData[[i]] <<- anti_join(modelInputData[[i]], 
                                                 data, by = idsIn[[i]])
               if(!nrow(data)){
                 # disable graph button as no data was loaded
                 disableEl(session, "#btGraphIn" %+% i)
                 isEmptyInput[i] <<- TRUE
               }else{
                 enableEl(session, "#btGraphIn" %+% i)
                 isEmptyInput[i] <<- FALSE
               }
               tableContent[[i]] <<- data
               return(data)
             })
           }else{
             modelInputData[[i]] <- reactive({
               rv[["in_" %+% i]]
               if(!nrow(modelInputData[[i]])){
                 # disable graph button as no data was loaded
                 disableEl(session, "#btGraphIn" %+% i)
                 isEmptyInput[i] <<- TRUE
               }else{
                 enableEl(session, "#btGraphIn" %+% i)
                 isEmptyInput[i] <<- FALSE
               }
               tableContent[[i]] <<- modelInputData[[i]]
               return(tableContent[[i]])
             })
             
           }
           output[["in_" %+% i]] <- renderDT({
             errMsg <- NULL
             tryCatch({
               dt <- do.call(datatable, c(list(dataModelIn[[i]](), 
                                               editable = if(identical(modelIn[[i]]$readonly, 
                                                                       TRUE))
                                                 FALSE else TRUE),
                                          config$datatable)) %>%
                 formatRound(seq_along(data), roundPrecision)
             }, error = function(e){
               flog.error("Problems rendering table for input dataset: %s. Error message: %s.",
                          modelInAlias[[i]], e) 
               errMsg <<- sprintf(lang$errMsg$renderTable$desc, modelInAlias[i])
             })
             if(is.null(showErrorMsg(lang$errMsg$renderTable$title, errMsg))){
               return(modelInputData[[i]]())
             }
             return(dt)
           })
           proxy[[i]] <<- dataTableProxy("in_" %+% i)
           
           observeEvent(input[[paste0("in_", i, "_cell_edit")]], {
             rownames <- config$datatable$rownames
             info <- input[[paste0("in_", i, "_cell_edit")]]
             row <- info$row
             if(rownames){
               col <- info$col
               if(col < 1){
                 return()
               }
             }else{
               col <- info$col + 1L
             }
             val <- info$value
             tableContent[[i]][row, col] <<- suppressWarnings(coerceValue(val, 
                                                                          tableContent[[i]][[col]][row]))
             replaceData(proxy[[i]], tableContent[[i]], resetPaging = FALSE, rownames = rownames)
           })
         }
  )
})
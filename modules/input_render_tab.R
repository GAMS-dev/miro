# render tabular datasets
proxy <- vector("list", length(modelIn))

getInputDataset <- function(id){
  if((!is.null(isolate(input[["in_" %+% id]])) && hotInit[[id]]) ||
     (length(tableContent[[id]]))){
    if(length(colsWithDep[[id]])){
      if(!isEmptyInput[id]){
        if(modelIn[[id]]$type == "hot"){
          dataTmp <- hot_to_r(isolate(input[["in_" %+% id]]))
          if(!length(dataTmp) || identical(nrow(dataTmp), 1L) &&
             identical(dataTmp[[1L]][1], ""))
            return(bind_rows(modelInputData[[id]], 
                             modelInputDataVisible[[id]]))
          return(bind_rows(dataTmp, modelInputData[[id]]))
        }
        return(bind_rows(tableContent[[id]], 
                               modelInputData[[id]]))
      }
      return(modelInputData[[id]])
    }
    if(!isEmptyInput[id]){
      if(modelIn[[id]]$type == "hot"){
        return(hot_to_r(isolate(input[["in_" %+% id]])))
      }
      return(tableContent[[id]])
    }
    return(modelInTemplate[[id]])
  }else if(!is.null(modelInputData[[id]])){
    # tab was never activated, so shiny does not update handsontable thus it is 
    # empty although data was loaded
    return(modelInputData[[id]])
  }
  stop("No input data found.", call. = FALSE)
}
observeEvent(input$inputTabset, {
  i <- as.integer(strsplit(isolate(input$inputTabset), "_")[[1]][2])
  if(is.null(configGraphsIn[[i]]) || isEmptyInput[i]){
    disableEl(session, "#btGraphIn")
  }else{
    enableEl(session, "#btGraphIn")
  }
})
observeEvent(input$btGraphIn, {
  i <- as.integer(strsplit(isolate(input$inputTabset), "_")[[1]][2])
  if(length(inputTabs[[i]]) > 1L){
    j <- as.integer(strsplit(isolate(input[[paste0("inputTabset", i)]]), "_")[[1]][2])
    i <- inputTabs[[i]][j]
  }else{
    i <- inputTabs[[i]][1]
  }
  flog.debug("Graph view for model input in sheet: %d activated.", i)
  if(is.null(configGraphsIn[[i]])){
    return()
  }else if(identical(modelIn[[i]]$type, "hot")){
    data <- hot_to_r(input[["in_" %+% i]])
  }else if(identical(modelIn[[i]]$type, "dt")){
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
lapply(modelInTabularData, function(sheet){
  # get input element id of dataset
  i <- match(sheet, tolower(names(modelIn)))[[1]]
  
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
                   flog.warn(paste0(lang$errMsg$dataError$desc,e))
                   errMsg <<- paste(errMsg, lang$errMsg$dataError$desc, sep = "\n")
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
                 disableEl(session, "#btGraphIn")
                 isEmptyInput[i] <<- TRUE
               }else{
                 enableEl(session, "#btGraphIn")
                 isEmptyInput[i] <<- FALSE
               }
               modelInputDataVisible[[i]] <<- data
               return(data)
             })
           }else{
             dataModelIn[[i]] <- reactive({
               rv[["in_" %+% i]]
               hotInit[[i]] <<- TRUE
               if(!nrow(modelInputData[[i]])){
                 modelInputData[[i]][1, ] <<- ""
                 disableEl(session, "#btGraphIn")
                 isEmptyInput[i] <<- TRUE
               }else{
                 enableEl(session, "#btGraphIn")
                 isEmptyInput[i] <<- FALSE
               }
               return(modelInputData[[i]])
             })
             
           }
           # rendering handsontables for input data 
           output[[paste0("in_", i)]] <- renderRHandsontable({
             noCheck[i] <<- TRUE
             ht <- rhandsontable(dataModelIn[[i]](), height = hotOptions$height, 
                                 colHeaders = attr(modelInTemplate[[i]], "aliases"),
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
                 modelIn[[i]]$headers[[j]]$alias
               }else{
                 NA_character_
               }
             }, character(1L), USE.NAMES = FALSE)
             colsReadonly <- colsReadonly[!is.na(colsReadonly)]
             if(length(colsReadonly)){
               ht <- hot_col(ht, colsReadonly, readOnly = TRUE)
             }
             if(identical(modelIn[[i]]$heatmap, TRUE))
               return(hot_heatmap(ht))
             return(ht)
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
                   flog.warn(paste0(lang$errMsg$dataError$desc, e))
                   errMsg <<- paste(errMsg, lang$errMsg$dataError$desc, sep = "\n")
                 })
                 modelInputData[[i]] <<- data
               }
               
               for(idDep in seq_along(colsWithDep[[i]])){
                 # get id of element (e.g. dropdown menu) that causes backward dependency
                 id  <- colsWithDep[[i]][[idDep]]
                 # in case nothing was selected in dropdown menu, skip this iteration
                 if(is.null(input[["dropdown_" %+% id]]) || 
                    input[["dropdown_" %+% id]] %in% c("", "_")){
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
                 disableEl(session, "#btGraphIn")
                 isEmptyInput[i] <<- TRUE
               }else{
                 enableEl(session, "#btGraphIn")
                 isEmptyInput[i] <<- FALSE
               }
               tableContent[[i]] <<- data
               return(data)
             })
           }else{
             dataModelIn[[i]] <- reactive({
               rv[["in_" %+% i]]
               if(!length(modelInputData[[i]]) || nrow(modelInputData[[i]]) < 1L){
                 # disable graph button as no data was loaded
                 disableEl(session, "#btGraphIn")
                 isEmptyInput[i] <<- TRUE
               }else{
                 enableEl(session, "#btGraphIn")
                 isEmptyInput[i] <<- FALSE
               }
               tableContent[[i]] <<- modelInputData[[i]]
               return(tableContent[[i]])
             })
             
           }
           output[["in_" %+% i]] <- renderDT({
             errMsg <- NULL
             tryCatch({
               dtOptions <- list(editable = !identical(modelIn[[i]]$readonly, 
                                                       TRUE),
                                 colnames = attr(modelInTemplate[[i]], "aliases"))
               
               dt <- renderDTable(dataModelIn[[i]](), dtOptions, 
                                  roundPrecision = roundPrecision, render = FALSE)
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
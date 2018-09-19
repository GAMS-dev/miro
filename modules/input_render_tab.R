# render tabular datasets

lapply(modelInTabularData, function(sheet){
  # get input element id of dataset
  i <- match(sheet, tolower(names(modelIn)))[[1]]
  switch(modelIn[[i]]$type,
         hot = {
           # l <- match(tolower(names(modelIn)[[i]]), names(modelInToImport))[[1]]
           if(length(colsWithDep[[i]])){
             dataModelIn[[i]] <- reactive({
               hotInit[[i]] <<- TRUE
               # make sure data will be updated when old data is overwritten
               rv[[paste0("in_", i)]]
               if(isEmptyInput[i]){
                 data <- modelInputData[[i]]
               }else{
                 # save changes made in handsontable
                 if(!is.null(isolate(input[[paste0("in_", i)]]))){
                   hotInput[[i]] <<- as_tibble(hot_to_r(isolate(input[[paste0("in_", i)]])))
                 }
                 tryCatch({
                   data <- bind_rows(hotInput[[i]], modelInputData[[i]])
                 }, error = function(e){
                   if(debugMode){
                     errMsg <<- paste(errMsg, paste(lang$errMsg$dataError$desc, e, sep = "\n"), sep = "\n")
                   }else{
                     errMsg <<- paste(errMsg, lang$errMsg$dataError$desc, sep = "\n")
                   }
                 })
                 modelInputData[[i]] <<- data
               }
               
               for(i.dep in seq_along(colsWithDep[[i]])){
                 # get id of element (e.g. dropdown menu) that causes backward dependency
                 id  <- colsWithDep[[i]][[i.dep]]
                 # in case nothing was selected in dropdown menu, skip this iteration
                 if(is.null(input[["dropdown_" %+% id]]) || input[["dropdown_" %+% id]] %in% c("","_")){
                   next
                 }
                 # get column name with dependency
                 col <- names(colsWithDep[[1]])[[i.dep]]
                 # filter data frame
                 data <- data[data[[col]] %in% input[["dropdown_" %+% id]], ]
               }
               modelInputData[[i]] <<- anti_join(modelInputData[[i]], data, by = idsIn[[i]])
               if(!nrow(data)){
                 data[1, ] <- ""
                 # disable graph button as no data was loaded
                 shinyjs::disable(paste0("btGraphIn", i))
                 isEmptyInput[i] <<- TRUE
               }else{
                 shinyjs::enable(paste0("btGraphIn", i))
                 isEmptyInput[i] <<- FALSE
                 #rv$datasetsImported[l] <<- TRUE
               }
               # do not set unsaved flag when data was updated from other input element automatically
               #noCheck[i] <<- TRUE
               return(data)
             })
           }else{
             dataModelIn[[i]] <- reactive({
               rv[[paste0("in_", i)]]
               if(!hotInit[[i]]){
                 # do not set unsaved flag when table has not yet been initialised
                 #noCheck[i] <<- TRUE
               }
               hotInit[[i]] <<- TRUE
               if(!nrow(modelInputData[[i]])){
                 modelInputData[[i]][1, ] <<- ""
                 # disable graph button as no data was loaded
                 shinyjs::disable(paste0("btGraphIn", i))
                 isEmptyInput[i] <<- TRUE
               }else{
                 shinyjs::enable(paste0("btGraphIn", i))
                 #rv$datasetsImported[l] <<- TRUE
                 isEmptyInput[i] <<- FALSE
               }
               return(modelInputData[[i]])
             })
             
           }
           
           observeEvent(input[[paste0("btGraphIn", i)]], {
             shinyjs::toggle(paste0("graph-in_", i))
             shinyjs::toggle(paste0("data-in_", i))
             errMsg <- NULL
             tryCatch({
               callModule(renderData, "in_" %+% i, type = configGraphsIn[[i]]$outType, data = rhandsontable::hot_to_r(input[["in_" %+% i]]),
                          dt.options = config$datatable, graph.options = configGraphsIn[[i]]$graph, 
                          pivot.options = configGraphsIn[[i]]$pivottable, custom.options = configGraphsIn[[i]]$options,
                          roundPrecision = roundPrecision, modelDir = modelDir)
             }, error = function(e) {
               flog.error("Problems rendering output charts and/or tables for dataset: '%s'. Error message: %s.", modelInAlias[i], e)
               errMsg <<- sprintf(lang$errMsg$renderGraph$desc, modelInAlias[i])
             })
             showErrorMsg(lang$errMsg$renderGraph$title, errMsg)
           })
           # rendering handsontables for input data 
           output[[paste0("in_", i)]] <- renderRHandsontable({
             noCheck[i] <<- TRUE
             ht <- rhandsontable(dataModelIn[[i]](), height = hotOptions$height, 
                                 width = hotOptions$width, search = hotOptions$search, 
                                 readOnly = modelIn[[i]]$readonly, selectCallback = TRUE)
             ht <- hot_table(ht, contextMenu = hotOptions$contextMenu$enabled, highlightCol = hotOptions$highlightCol, highlightRow = hotOptions$highlightRow,
                             rowHeaderWidth = hotOptions$rowHeaderWidth, enableComments = hotOptions$enableComments, stretchH = hotOptions$stretchH,
                             overflow = hotOptions$overflow)
             ht <- hot_context_menu(ht, allowRowEdit = hotOptions$contextMenu$allowRowEdit, allowColEdit = hotOptions$contextMenu$allowColEdit, 
                                    allowReadOnly = hotOptions$contextMenu$allowReadOnly, allowComments = hotOptions$contextMenu$allowComments)
             ht <- hot_cols(ht, columnSorting = hotOptions$columnSorting, manualColumnMove = hotOptions$manualColumnMove, 
                            manualColumnResize = hotOptions$manualColumnResize, colWidths = hotOptions$colWidths, fixedColumnsLeft = hotOptions$fixedColumnsLeft)
             
             # check for readonly columns
             cols.readonly <- vapply(seq_along(modelIn[[i]]$headers), function(j){
               if(identical(modelIn[[i]]$headers[[j]]$readonly, TRUE)){
                 names(modelIn[[i]]$headers)[[j]]
               }else{
                 NA_character_
               }
             }, character(1L), USE.NAMES = FALSE)
             cols.readonly <- cols.readonly[!is.na(cols.readonly)]
             if(length(cols.readonly)){
               return(hot_col(ht, cols.readonly, readOnly = TRUE))
             }else{
               return(ht)
             }
           })
         }
  )
})
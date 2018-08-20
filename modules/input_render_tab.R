# render tabular datasets

lapply(modelIn.tabular.data, function(sheet){
  # get input element id of dataset
  i <- match(sheet, tolower(names(modelIn)))[[1]]
  switch(modelIn[[i]]$type,
         hot = {
           # l <- match(tolower(names(modelIn)[[i]]), names(modelIn.to.import))[[1]]
           if(length(cols.with.dep[[i]])){
             data.modelIn[[i]] <- reactive({
               hot.init[[i]] <<- T
               # make sure data will be updated when old data is overwritten
               rv[[paste0("in_", i)]]
               if(is.empty.input[i]){
                 data <- model.input.data[[i]]
               }else{
                 # save changes made in handsontable
                 if(!is.null(isolate(input[[paste0("in_", i)]]))){
                   hot.input[[i]] <<- as_tibble(rhandsontable::hot_to_r(isolate(input[[paste0("in_", i)]])))
                 }
                 tryCatch({
                   data <- dplyr::bind_rows(hot.input[[i]], model.input.data[[i]])
                 }, error = function(e){
                   if(debug.mode){
                     errMsg <<- paste(errMsg, paste(lang$errMsg$dataError$desc, e, sep = "\n"), sep = "\n")
                   }else{
                     errMsg <<- paste(errMsg, lang$errMsg$dataError$desc, sep = "\n")
                   }
                 })
                 model.input.data[[i]] <<- data
               }
               
               for(i.dep in seq_along(cols.with.dep[[i]])){
                 # get id of element (e.g. dropdown menu) that causes backward dependency
                 id  <- cols.with.dep[[i]][[i.dep]]
                 # in case nothing was selected in dropdown menu, skip this iteration
                 if(is.null(input[[paste0("dropdown_", id)]]) || input[[paste0("dropdown_", id)]] %in% c("","_")){
                   next
                 }
                 # get column name with dependency
                 col <- names(cols.with.dep[[1]])[[i.dep]]
                 # filter data frame
                 data <- data[data[[col]] %in% input[[paste0("dropdown_", id)]], ]
               }
               model.input.data[[i]] <<- dplyr::anti_join(model.input.data[[i]], data, by = ids.in[[i]])
               if(!nrow(data)){
                 data[1, ] <- ""
                 # disable graph button as no data was loaded
                 shinyjs::disable(paste0("btGraphIn", i))
                 is.empty.input[i] <<- TRUE
               }else{
                 shinyjs::enable(paste0("btGraphIn", i))
                 is.empty.input[i] <<- FALSE
                 #rv$datasets.imported[l] <<- TRUE
               }
               # do not set unsaved flag when data was updated from other input element automatically
               no.check[i] <<- TRUE
               return(data)
             })
           }else{
             data.modelIn[[i]] <- reactive({
               rv[[paste0("in_", i)]]
               if(!hot.init[[i]]){
                 # do not set unsaved flag when table has not yet been initialised
                 no.check[i] <<- TRUE
               }
               hot.init[[i]] <<- TRUE
               if(!nrow(model.input.data[[i]])){
                 model.input.data[[i]][1, ] <<- ""
                 # disable graph button as no data was loaded
                 shinyjs::disable(paste0("btGraphIn", i))
                 is.empty.input[i] <<- TRUE
               }else{
                 shinyjs::enable(paste0("btGraphIn", i))
                 #rv$datasets.imported[l] <<- TRUE
                 is.empty.input[i] <<- FALSE
               }
               return(model.input.data[[i]])
             })
             
           }
           
           observeEvent(input[[paste0("btGraphIn", i)]], {
             shinyjs::toggle(paste0("graph-in_", i))
             shinyjs::toggle(paste0("data-in_", i))
             errMsg <- NULL
             tryCatch({
               callModule(modelOutput, "in_" %+% i, type = config.graphs.in[[i]]$outType, data = rhandsontable::hot_to_r(input[["in_" %+% i]]),
                          dt.options = config$datatable, graph.options = config.graphs.in[[i]]$graph, 
                          pivot.options = config.graphs.in[[i]]$pivottable, custom.options = config.graphs.in[[i]]$options,
                          roundPrecision = roundPrecision, modelDir = modelDir)
             }, error = function(e) {
               flog.error("Problems rendering output charts and/or tables for dataset: '%s'. Error message: %s.", modelIn.alias[i], e)
               errMsg <<- sprintf(lang$errMsg$renderGraph$desc, modelIn.alias[i])
             })
             showErrorMsg(lang$errMsg$renderGraph$title, errMsg)
           })
           # rendering handsontables for input data 
           output[[paste0("in_", i)]] <- renderRHandsontable({
             ht <- rhandsontable(data.modelIn[[i]](), height = hot.options$height, width = hot.options$width, search = hot.options$search, readOnly = modelIn[[i]]$readonly)
             ht <- hot_table(ht, contextMenu = hot.options$contextMenu$enabled, highlightCol = hot.options$highlightCol, highlightRow = hot.options$highlightRow,
                             rowHeaderWidth = hot.options$rowHeaderWidth, enableComments = hot.options$enableComments, stretchH = hot.options$stretchH,
                             overflow = hot.options$overflow)
             ht <- hot_context_menu(ht, allowRowEdit = hot.options$contextMenu$allowRowEdit, allowColEdit = hot.options$contextMenu$allowColEdit, 
                                    allowReadOnly = hot.options$contextMenu$allowReadOnly, allowComments = hot.options$contextMenu$allowComments)
             ht <- hot_cols(ht, columnSorting = hot.options$columnSorting, manualColumnMove = hot.options$manualColumnMove, 
                            manualColumnResize = hot.options$manualColumnResize, colWidths = hot.options$colWidths, fixedColumnsLeft = hot.options$fixedColumnsLeft)
             
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
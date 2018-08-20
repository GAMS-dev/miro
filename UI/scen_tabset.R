getScenTabData <- function(sheetName){
  # returns list with data relevant for rendering scenario tabsets
  tabData       <- NULL
  tabData$tabId <- match(sheetName, scen.table.names.to.display)[1]
  i <- match(tolower(sheetName), tolower(input.ds.names))
  if(is.na(i)){
    i <- match(tolower(sheetName), names(modelOut))
    if(is.na(i)){
      stop(sprintf("Sheet: %s could not be rendered as it was not found in either list of input or output sheets.", sheetName), call. = FALSE)
    }else{
      # sheet is output sheet
      tabData$sheetName     <- modelOut.alias[i]
      tabData$tooltip       <- lang$nav$scen$tooltips$outputSheet
      tabData$graphConfig   <- config.graphs.out[[i]]
    }
  }else{
    # sheet is input sheet
    if(is.null(config.graphs.in[[i]]$outType)){
      config.graphs.in[[i]]$outType <- "datatable"
    }
    tabData$graphConfig   <- config.graphs.in[[i]]
    tabData$tooltip       <- lang$nav$scen$tooltips$inputSheet
    if(input.ds.names[i] == scalars.file.name){
      tabData$sheetName <- lang$nav$scen$scalarsAlias
    }else{
      tabData$sheetName <- modelIn.alias[match(input.ds.names[i], names(modelIn))[1]]
    }
  }
  # get data index
  tabData$scenTableId <- match(tolower(paste0(modelName, "_", sheetName)), tolower(scen.table.names))
  if(is.na(tabData$scenTableId)){
    stop(sprintf("Data for sheet: '%s' could not be found. If this problem persists, please contact the system administrator.", sheetName), call. = FALSE)
  }
  return(tabData)
}
generateScenarioTabset <- function(scenId, noData = vector("logical", length(scen.table.names.to.display)), 
                                   noDataTxt = lang$nav$outputScreen$boxResults$noData, scenCounter = scenId){
  errMsg <- NULL
  scenTabset <- do.call(tabBox, c(id = paste0("content.scen_", scenId), width = 12, 
                                  lapply(scen.table.names.to.display, function(sheetName) {
                                    # get sheet configuration information
                                    tabData <- getScenTabData(sheetName)
                                    
                                    
                                    tabPanel(value = "content.scen_" %+% scenId %+% "_" %+% tabData$tabId,
                                             title = span(tabData$sheetName, title = tabData$tooltip), 
                                             tags$div(class="space"),
                                             if(noData[tabData$tabId]){
                                               tags$div(class = "out-no-data", lang$nav$outputScreen$boxResults$noData)
                                             }else{
                                               tagList(
                                                 # loading animation
                                                 #if(config.graphs$outType %in% c("dtGraph") && config.graphs$graph$tool == "plotly"){
                                                 #  tags$img(src = "load.gif", class = "loading-input-r")
                                                 #}else if (config.graphs$outType == "graph" && config.graphs$graph$tool == "plotly"){
                                                 #  tags$img(src = "load.gif", class = "loading-input")
                                                 #},
                                                 tags$div(id= paste0("scenGraph_", scenId, "_", tabData$tabId), class = "render-output", 
                                                          style = if(!is.null(tabData$graphConfig$height)) sprintf("min-height: %s;", add.css.dim(tabData$graphConfig$height, 5)),{
                                                            tryCatch({
                                                              renderDataUI("tab_" %+% scenCounter %+% "_" %+% tabData$tabId, type = tabData$graphConfig$outType, 
                                                                            graph.tool = tabData$graphConfig$graph$tool, custom.options = tabData$graphConfig$options,
                                                                            height = tabData$graphConfig$height, modelDir = modelDir, 
                                                                            no.data.txt = noDataTxt)
                                                            }, error = function(e) {
                                                              flog.error("Problems rendering UI elements for scenario dataset: '%s'. Error message: %s.", tabData$sheetName, e)
                                                              errMsg <<- paste(errMsg, sprintf(lang$errMsg$renderTable$desc, tabData$sheetName), sep = "\n")
                                                            })
                                                          }),
                                                 shinyjs::hidden(
                                                   tags$div(id= paste0("scenTable_", scenId, "_", tabData$tabId), class = "render-output",{
                                                     tryCatch({
                                                       renderDataUI("table_tab_" %+% scenCounter %+% "_" %+% tabData$tabId, type = "datatable",
                                                                     no.data.txt = noDataTxt)
                                                     }, error = function(e) {
                                                       flog.error("Problems rendering table for scenario dataset: '%s'. Error message: %s.", tabData$sheetName, e)
                                                       errMsg <<- paste(errMsg, sprintf(lang$errMsg$renderTable$desc, tabData$sheetName), sep = "\n")
                                                     })
                                                   })
                                                 )
                                               )
                                             },
                                             tags$div(class="space")
                                    )
                                  })))
  if(!is.null(errMsg)){
    stop(errMsg, call. = FALSE)
  }else{
    return(scenTabset)
  }
}
generateScenarioTabsetMulti <- function(scenId, noData = vector("logical", length(scen.table.names.to.display)), scenCounter = scenId){
  errMsg <- NULL
  tryCatch({
    scenTabset <- generateScenarioTabset(scenId, noData, noDataTxt = NULL, scenCounter = scenCounter)
  }, error = function(e){
    flog.error("Problems generating scenario tabset (multi comparison mode). Error message: %s.", e)
    errMsg <<- as.character(e)
  })
  if(is.null(showErrorMsg(lang$errMsg$renderGraph$title, errMsg))){
    return(NULL)
  }
  tabPanel(textOutput("title_" %+% scenId, inline = TRUE), value = "scen_" %+% scenId %+% "_",
           tags$div(class="scen-header", 
                    tags$div(class = "scen-date-wrapper", textOutput("date_" %+% scenId, inline = TRUE)),
                    tags$div(class = "scen-buttons-wrapper",
                             tags$div(title = lang$nav$scen$tooltips$btExport, class = "scen-button-tt",
                                      downloadButton(outputId = paste0("export_", scenId), label = "", 
                                                     class="scen-button")
                             ),
                             tags$div(title = lang$nav$scen$tooltips$btTableView, class = "scen-button-tt",
                                      actionButton(paste0("table_", scenId), icon("table"), 
                                                   class="scen-button")
                             ),
                             tags$div(title = lang$nav$scen$tooltips$btClose, class = "scen-button-tt",
                                      actionButton(paste0("btClose_", scenId), icon("times"), 
                                                   class="scen-button")
                                      
                             )
                    )
                    
           ),
           fluidRow(
             scenTabset
           )
  )
}
generateScenarioTabsetSplit <- function(scenId){
  errMsg <- NULL
  tryCatch({
    scenTabset <- generateScenarioTabset(scenId)
  }, error = function(e){
    flog.error("Problems generating scenario tabset (split comparison mode). Error message: %s.", e)
    errMsg <<- as.character(e)
  })
  if(is.null(showErrorMsg(lang$errMsg$renderGraph$title, errMsg))){
    return(NULL)
  }
  tagList(
    tags$div(class="scen-header", 
             tags$div(class = "scen-date-wrapper", textOutput("date_" %+% scenId, inline = TRUE)),
             tags$div(class = "scen-buttons-wrapper",
                      tags$div(title = lang$nav$scen$tooltips$btExport, class = "scen-button-tt",
                               downloadButton(outputId = paste0("export_", scenId), label = "", 
                                              class="scen-button")
                      ),
                      tags$div(title = lang$nav$scen$tooltips$btTableView, class = "scen-button-tt",
                               actionButton(paste0("table_", scenId), icon("table"), 
                                            class="scen-button")
                      )
             )
             
    ),
    tags$div(style = "margin-top: 10px;",
             scenTabset
    )
  )
}
getScenTabData <- function(sheetName){
  # returns list with data relevant for rendering scenario tabsets
  tabData       <- NULL
  tabData$tabId <- match(sheetName, scenTableNamesToDisplay)[1]
  i <- match(tolower(sheetName), tolower(inputDsNames))
  if(is.na(i)){
    i <- match(tolower(sheetName), names(modelOut))
    if(is.na(i)){
      stop(sprintf("Sheet: %s could not be rendered as it was not found in either list of input or output sheets.", sheetName), call. = FALSE)
    }else{
      # sheet is output sheet
      tabData$sheetName     <- modelOutAlias[i]
      tabData$tooltip       <- lang$nav$scen$tooltips$outputSheet
      tabData$graphConfig   <- configGraphsOut[[i]]
      tabData$headerAliases <- attr(modelOutTemplate[[i]], "aliases")
    }
  }else{
    # sheet is input sheet
    if(is.null(configGraphsIn[[i]]$outType)){
      configGraphsIn[[i]]$outType <- "pivot"
      # prepopulate scalar table
      if(inputDsNames[i] == scalarsFileName && !length(configGraphsIn[[i]]$pivottable)){
        configGraphsIn[[i]]$pivottable$rows <- c("Scalar")
        configGraphsIn[[i]]$pivottable$aggregatorName <- "Sum"
        configGraphsIn[[i]]$pivottable$vals <- "Value"
      }
    }
    tabData$graphConfig   <- configGraphsIn[[i]]
    tabData$tooltip       <- lang$nav$scen$tooltips$inputSheet
    tabData$headerAliases <- attr(modelInTemplate[[i]], "aliases")
    if(inputDsNames[i] == scalarsFileName){
      tabData$sheetName <- config$scalarAliases$inputScalars
      tabData$headerAliases <- scalarsFileHeaders
    }else{
      tabData$sheetName <- modelInAlias[match(inputDsNames[i], names(modelIn))[1]]
      tabData$headerAliases <- attr(modelInTemplate[[i]], "aliases")
    }
  }
  # get data index
  tabData$scenTableId <- match(tolower(paste0(gsub("_", "", modelName, fixed = TRUE),
                                              "_", sheetName)), tolower(scenTableNames))
  if(is.na(tabData$scenTableId)){
    stop(sprintf("Data for sheet: '%s' could not be found. If this problem persists, please contact the system administrator.", 
                 sheetName), call. = FALSE)
  }
  return(tabData)
}
generateScenarioTabset <- function(scenId, noData = vector("logical", length(scenTableNamesToDisplay)), 
                                   noDataTxt = lang$nav$outputScreen$boxResults$noData, scenCounter = scenId){
  errMsg <- NULL
  scenTabset <- do.call(tabBox, c(id = paste0("contentScen_", scenId), width = 12, 
                                  lapply(scenTableNamesToDisplay, function(sheetName) {
                                    # get sheet configuration information
                                    tabData <- getScenTabData(sheetName)
                                    
                                    tabPanel(value = "contentScen_" %+% scenId %+% "_" %+% tabData$tabId,
                                             title = span(tabData$sheetName, title = tabData$tooltip), 
                                             tags$div(class="space"),
                                             if(noData[tabData$tabId]){
                                               tags$div(class = "out-no-data", lang$nav$outputScreen$boxResults$noData)
                                             }else{
                                               tagList(
                                                 tags$div(id= paste0("scenGraph_", scenId, "_", tabData$tabId), class = "render-output", 
                                                          style = if(!is.null(tabData$graphConfig$height)) sprintf("min-height: %s;", 
                                                                                                                   addCssDim(tabData$graphConfig$height, 5)),{
                                                            tryCatch({
                                                              renderDataUI("tab_" %+% scenCounter %+% "_" %+% tabData$tabId, 
                                                                           type = tabData$graphConfig$outType, 
                                                                            graphTool = tabData$graphConfig$graph$tool, 
                                                                           customOptions = tabData$graphConfig$options,
                                                                            height = tabData$graphConfig$height, modelDir = modelDir, 
                                                                            noDataTxt = noDataTxt)
                                                            }, error = function(e) {
                                                              flog.error("Problems rendering UI elements for scenario dataset: '%s'. Error message: %s.", 
                                                                         tabData$sheetName, e)
                                                              errMsg <<- paste(errMsg, sprintf(lang$errMsg$renderTable$desc, tabData$sheetName), sep = "\n")
                                                            })
                                                          }),
                                               tags$div(id= paste0("scenTable_", scenId, "_", tabData$tabId), style = "display:none;",
                                                        class = "render-output",{
                                                 tryCatch({
                                                   renderDataUI("table_tab_" %+% scenCounter %+% "_" %+% tabData$tabId, type = "datatable",
                                                                 noDataTxt = noDataTxt)
                                                 }, error = function(e) {
                                                   flog.error("Problems rendering table for scenario dataset: '%s'. Error message: %s.", tabData$sheetName, e)
                                                   errMsg <<- paste(errMsg, sprintf(lang$errMsg$renderTable$desc, tabData$sheetName), sep = "\n")
                                                 })
                                               })
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
generateScenarioTabsetMulti <- function(scenId, noData = vector("logical", length(scenTableNamesToDisplay)), scenCounter = scenId){
  tryCatch({
    scenTabset <- generateScenarioTabset(scenId, noData, noDataTxt = NULL, scenCounter = scenCounter)
  }, error = function(e){
    flog.error("Problems generating scenario tabset (multi comparison mode). Error message: %s.", e)
    stop(conditionMessage(e))
  })

  tabPanel(textOutput("title_" %+% scenId, inline = TRUE), value = "scen_" %+% scenId %+% "_",
           tags$div(class="scen-header", 
                    tags$div(class = "scen-date-wrapper", textOutput("date_" %+% scenId, inline = TRUE)),
                    tags$div(class = "scen-buttons-wrapper",
                             tags$div(title = lang$nav$scen$tooltips$btExport, class = "scen-button-tt",
                                      HTML(paste0('<button type="button" class="btn btn-default scen-button" 
onclick="Shiny.setInputValue(\'btExportScen\', ', scenId, ', {priority: \'event\'})"><i class="fas fa-download"></i></button>'))
                             ),
                             tags$div(title = lang$nav$scen$tooltips$btTableView, class = "scen-button-tt",
                                      actionButton(paste0("table_", scenId), icon("chart-bar"), 
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
  tryCatch({
    scenTabset <- generateScenarioTabset(scenId)
  }, error = function(e){
    flog.error("Problems generating scenario tabset (split comparison mode). Error message: %s.", e)
    stop(conditionMessage(e))
  })
  tagList(
    tags$div(class="scen-header", 
             tags$div(class = "scen-date-wrapper", textOutput("date_" %+% scenId, inline = TRUE)),
             tags$div(class = "scen-buttons-wrapper",
                      tags$div(title = lang$nav$scen$tooltips$btExport, class = "scen-button-tt",
                               HTML(paste0('<button type="button" class="btn btn-default scen-button" 
onclick="Shiny.setInputValue(\'btExportScen\', ', scenId, ', {priority: \'event\'})"><i class="fas fa-download"></i></button>'))
                      ),
                      tags$div(title = lang$nav$scen$tooltips$btTableView, class = "scen-button-tt",
                               actionButton(paste0("table_", scenId), icon("chart-bar"), 
                                            class="scen-button")
                      )
             )
             
    ),
    tags$div(style = "margin-top: 10px;",
             scenTabset
    )
  )
}
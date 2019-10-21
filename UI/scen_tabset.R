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
    if(identical(inputDsNames[[i]], scalarsFileName) && 
       !scalarsFileName %in% names(modelIn)){
      tabData$sheetName     <- lang$nav$scalarAliases$scalars
      tabData$headerAliases <- c(lang$nav$scalarAliases$cols$name,
                                 lang$nav$scalarAliases$cols$desc,
                                 lang$nav$scalarAliases$cols$value)
      tabData$graphConfig$outType <- "pivot"
      tabData$graphConfig$pivottable$rows <- scalarsFileHeaders[1]
      tabData$graphConfig$pivottable$aggregatorName <- "Sum"
      tabData$graphConfig$pivottable$vals <- scalarsFileHeaders[3]
    }else{
      tabData$sheetName     <- modelInAlias[match(inputDsNames[i], names(modelIn))[1]]
      tabData$graphConfig   <- configGraphsIn[[i]]
      tabData$headerAliases <- attr(modelInTemplate[[i]], "aliases")
    }
    tabData$tooltip       <- lang$nav$scen$tooltips$inputSheet
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
                                   noDataTxt = lang$nav$outputScreen$boxResults$noData, scenCounter = scenId,
                                   createdDynamically = FALSE){
  errMsg <- NULL
  noDataDiv <- tags$div(class = "out-no-data", lang$nav$outputScreen$boxResults$noData)
  scenTabset <- MIROtabBox(id = paste0("contentScen_", scenId), noTabsGrouped = length(outputTabs),
                           btCollapsedTabs = lang$nav$inputScreen$btCollapsedTabs, 
                                  lapply(seq_len(length(outputTabs) + length(scenInputTabs)), 
                                  function(groupId) {
                                    if(groupId > length(outputTabs)){
                                      # input dataset
                                      tabSheetIds <- scenInputTabs[[groupId - length(outputTabs)]]
                                      tabTitles   <- scenInputTabTitles[[groupId - length(outputTabs)]]
                                      tooltip     <- lang$nav$scen$tooltips$inputSheet
                                      isOutputGroup <- FALSE
                                    }else{
                                      tabSheetIds   <- outputTabs[[groupId]]
                                      tabTitles     <- outputTabTitles[[groupId]]
                                      tooltip       <- lang$nav$scen$tooltips$outputSheet
                                      isOutputGroup <- TRUE
                                    }
                                    content <- lapply(tabSheetIds, function(sheetId){
                                      if(isOutputGroup){
                                        sheetName <- names(modelOut)[sheetId]
                                        graphConfig <- configGraphsOut[[sheetId]]
                                      }else if(identical(sheetId, 0L)){
                                        sheetName <- scalarsFileName
                                        graphConfig <- getScenTabData(scalarsFileName)
                                        graphConfig <- graphConfig$graphConfig
                                      }else{
                                        sheetName <- names(modelIn)[sheetId]
                                        graphConfig <- configGraphsIn[[sheetId]]
                                      }
                                      tabContent <- NULL
                                      tabId <- match(sheetName, 
                                                     scenTableNamesToDisplay)
                                      if(is.na(tabId)){
                                        flog.error("Invalid dataset! Please contact GAMS!")
                                        tabContent <- noDataDiv
                                      }
                                      
                                      if(is.null(tabContent)){
                                        if(noData[tabId]){
                                          tabContent <- noDataDiv
                                        }else{
                                          tabContent <- tagList(
                                            tags$div(id= paste0("scenGraph_", scenId, "_", tabId), 
                                                     class = "render-output", 
                                                     style = if(!is.null(graphConfig$height)) 
                                                       sprintf("min-height: %s;", addCssDim(graphConfig$height, 5)),{
                                                         tryCatch({
                                                           renderDataUI(paste0("tab_", scenCounter,
                                                                               "_", tabId), 
                                                                        type = graphConfig$outType, 
                                                                        graphTool = graphConfig$graph$tool, 
                                                                        customOptions = graphConfig$options,
                                                                        filterOptions = graphConfig$graph$filter,
                                                                        height = graphConfig$height, modelDir = modelDir, 
                                                                        noDataTxt = noDataTxt, 
                                                                        createdDynamically = createdDynamically)
                                                         }, error = function(e) {
                                                           flog.error("Problems rendering UI elements for scenario dataset: '%s'. Error message: %s.", 
                                                                      sheetName, e)
                                                           errMsg <<- paste(errMsg, sprintf(lang$errMsg$renderTable$desc, 
                                                                                            sheetName), sep = "\n")
                                                         })
                                                       }),
                                            tags$div(id= paste0("scenTable_", scenId, "_", tabId), 
                                                     style = "display:none;",
                                                     class = "render-output",{
                                                       tryCatch({
                                                         renderDataUI(paste0("table_tab_", scenCounter, "_",
                                                                             tabId), type = "datatable",
                                                                      noDataTxt = noDataTxt)
                                                       }, error = function(e) {
                                                         flog.error("Problems rendering table for scenario dataset: '%s'. Error message: %s.", sheetName, e)
                                                         errMsg <<- paste(errMsg, sprintf(lang$errMsg$renderTable$desc, sheetName), sep = "\n")
                                                       })
                                                     })
                                          )
                                        }
                                      }
                                      
                                      if(length(tabTitles) > 1L){
                                        titleId <- match(sheetId, tabSheetIds) + 1L
                                        return(tabPanel(
                                          title = tabTitles[titleId],
                                          value = paste0("contentScen_", scenId, "_", 
                                                         groupId, "_", titleId - 1L),
                                          tags$div(class="space"),
                                          tabContent,
                                          tags$div(class="space")
                                        ))
                                      }
                                      return(tabContent)
                                    })
                                    
                                    return(tabPanel(
                                      title = span(tabTitles[1], title = tooltip),
                                      value = paste0("contentScen_", scenId, 
                                                     "_", groupId),
                                      if(length(tabTitles) > 1L){
                                        MIROtabsetPanel(content,
                                                        id = paste0("contentScen_", scenId,
                                                                    "_", groupId),
                                                        noTabsGrouped = if(isOutputGroup) 
                                                          length(tabTitles) else 0L)
                                      }else{
                                        tagList(tags$div(class="space"), 
                                                content,
                                                tags$div(class="space"))
                                      }
                                    ))
                                  }))
  if(!is.null(errMsg)){
    stop(errMsg, call. = FALSE)
  }else{
    return(scenTabset)
  }
}
generateScenarioTabsetMulti <- function(scenId, noData = vector("logical", length(scenTableNamesToDisplay)), 
                                        scenCounter = scenId){
  tryCatch({
    scenTabset <- generateScenarioTabset(scenId, noData, noDataTxt = NULL, scenCounter = scenCounter,
                                         createdDynamically = TRUE)
  }, error = function(e){
    flog.error("Problems generating scenario tabset (multi comparison mode). Error message: %s.", e)
    stop(conditionMessage(e))
  })

  tabPanel(uiOutput("title_" %+% scenId, inline = TRUE), value = "scen_" %+% scenId %+% "_",
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
           tags$div(class="small-space"),
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
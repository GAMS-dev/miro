getScenTabData <- function(sheetName){
  # returns list with data relevant for rendering scenario tabsets
  tabData       <- NULL
  tabData$tabId <- match(sheetName, scenTableNamesToDisplay)[1]
  i <- match(tolower(sheetName), tolower(inputDsNames))
  if(is.na(i)){
    i <- match(tolower(sheetName), names(modelOut))
    if(is.na(i)){
      stop(sprintf("Sheet: %s could not be rendered as it was not found in either list of input or output sheets.",
                   sheetName), call. = FALSE)
    }else{
      # sheet is output sheet
      tabData$sheetName     <- modelOutAlias[i]
      tabData$tooltip       <- lang$nav$scen$tooltips$outputSheet
      tabData$graphConfig   <- configGraphsOut[[i]]
      tabData$headerAliases <- attr(modelOutTemplate[[i]], "aliases")
    }
    tabData$scenTableId <- i
  }else{
    # sheet is input sheet
    if(identical(inputDsNames[[i]], scalarsFileName) &&
       !scalarsFileName %in% names(modelIn)){
      tabData$sheetName     <- lang$nav$scalarAliases$scalars
      tabData$headerAliases <- c(lang$nav$scalarAliases$cols$name,
                                 lang$nav$scalarAliases$cols$desc,
                                 lang$nav$scalarAliases$cols$value)
      if(scalarsFileName %in% names(modelInRaw)){
        tabData$graphConfig$outType <- "valuebox"
        tabData$graphConfig$options$count <- length(modelInRaw[[scalarsFileName]]$symnames)
      }else{
        tabData$graphConfig$outType <- "datatable"
      }
    }else{
      modelInId             <- match(inputDsNames[i], names(modelIn))[1]
      tabData$sheetName     <- modelInAlias[modelInId]
      tabData$graphConfig   <- configGraphsIn[[modelInId]]
      tabData$headerAliases <- attr(modelInTemplate[[modelInId]], "aliases")
    }
    tabData$tooltip       <- lang$nav$scen$tooltips$inputSheet
    tabData$scenTableId   <- length(modelOut) + i
  }
  if(is.na(tabData$scenTableId)){
    stop(sprintf("Data for sheet: '%s' could not be found. If this problem persists, please contact the system administrator.",
                 sheetName), call. = FALSE)
  }
  return(tabData)
}
generateScenarioTabset <- function(scenId, createdDynamically = FALSE, pivotCompare = FALSE){
  errMsg <- NULL
  noDataDiv <- tags$div(class = "out-no-data", lang$nav$outputScreen$boxResults$noData)
  scenTabContent <- lapply(seq_len(length(outputTabs) + length(scenInputTabs)),
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
                                 tabContent <- tagList(
                                   tags$div(id = paste0("scenGraph_", scenId, "_", tabId),
                                            class = "render-output",
                                            style = if(!is.null(graphConfig$height))
                                              sprintf("min-height: %s;", addCssDim(graphConfig$height, 5))),
                                   if(!pivotCompare){
                                     tags$div(id= paste0("scenTable_", scenId, "_", tabId),
                                              style = "display:none;",
                                              class = "render-output")
                                   }
                                 )
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
                               if(length(tabSheetIds) > 1L){
                                 return(column(width = 6L,
                                               tabContent))
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
                                         if(length(tabSheetIds) > 1L){
                                           fluidRow(content)
                                         }else{
                                           content
                                         },
                                         tags$div(class="space"))
                               }
                             ))
                           })
  if(!pivotCompare && length(config$scripts$base)){
    scenTabContent <- c(
      scenTabContent,
      lapply(seq_along(config$scripts$base), function(scriptId){
        tabPanel(
          title = config$scripts$base[[scriptId]]$tabTitle,
          value = paste0("contentScen_", scenId,
                         "_", length(outputTabs) + length(scenInputTabs) + scriptId),
          tagList(tags$div(class="small-space"),
                  tags$div(class="space"),
                  tags$div(id = paste0("scenScript_", scenId, "_", scriptId, "_noData"),
                           class = "out-no-data", lang$nav$outputScreen$boxResults$noData),
                  tags$iframe(id = paste0("scenScript_", scenId, "_", scriptId),
                              class = "script-output"),
                  tags$div(class="space"))
        )}))
  }
  scenTabset <- MIROtabBox(id = paste0("contentScen_", scenId), noTabsGrouped = length(outputTabs),
                           btCollapsedTabs = lang$nav$inputScreen$btCollapsedTabs,
                           scenTabContent)
  if(!is.null(errMsg)){
    stop(errMsg, call. = FALSE)
  }else{
    return(scenTabset)
  }
}
generateScenarioTabsetMulti <- function(scenId){
  tryCatch({
    scenTabset <- generateScenarioTabset(scenId, createdDynamically = TRUE)
  }, error = function(e){
    flog.error("Problems generating scenario tabset (multi comparison mode). Error message: %s.",
               conditionMessage(e))
    stop(conditionMessage(e))
  })

  tabPanel(tags$span(id = paste0("cmpScenTitle_", scenId)), value = paste0("scen_", scenId, "_"),
           tags$div(class="scen-header",
                    tags$div(class = "scen-date-wrapper",
                             tags$span(id = paste0("cmpScenDate_", scenId))),
                    tags$div(class = "scen-buttons-wrapper",
                             tags$div(title = lang$nav$scen$tooltips$btRefresh, class = "scen-button-tt",
                                      id = paste0("refreshSandbox_", scenId),
                                      tags$button(class = "btn btn-default scen-button",
                                                  type = "button",
                                                  onclick = paste0("Shiny.setInputValue('btRefreshComp',",
                                                                   scenId, ",{priority:'event'})"),
                                                  tags$i(class = "fas fa-sync-alt",
                                                         role = "presentation",
                                                         `aria-label` = lang$nav$scen$tooltips$btRefresh))
                             ),
                             tags$div(title = lang$nav$scen$tooltips$btExport, class = "scen-button-tt",
                                      HTML(paste0('<button type="button" class="btn btn-default scen-button"
onclick="Shiny.setInputValue(\'btExportScen\', ', scenId, ', {priority: \'event\'})"><i class="fas fa-download" role="presentation" aria-label="',
lang$nav$scen$tooltips$btExport, '"></i></button>'))
                             ),
tags$div(title = lang$nav$scen$tooltips$btTableView, class = "scen-button-tt",
         tags$button(class = "btn btn-default scen-button",
                     id = paste0("btScenTableView", scenId), type = "button",
                     onclick = paste0("Shiny.setInputValue('btScenTableView',",
                                      scenId, ",{priority:'event'})"),
                     tags$i(class = "fa fa-chart-bar",
                            role = "presentation",
                            `aria-label` = lang$nav$scen$tooltips$btTableView))
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
    flog.error("Problems generating scenario tabset (split comparison mode). Error message: %s.",
               conditionMessage(e))
    stop(conditionMessage(e))
  })
  tagList(
    tags$div(class="scen-header",
             tags$div(class = "scen-date-wrapper",
                      tags$span(id = paste0("cmpScenDate_", scenId))),
             tags$div(class = "scen-buttons-wrapper",
                      tags$div(title = lang$nav$scen$tooltips$btRefresh, class = "scen-button-tt",
                               id = paste0("refreshSandbox_", scenId),
                               tags$button(class = "btn btn-default scen-button",
                                           type = "button",
                                           onclick = paste0("Shiny.setInputValue('btRefreshComp',",
                                                            scenId, ",{priority:'event'})"),
                                           tags$i(class = "fas fa-sync-alt",
                                                  role = "presentation",
                                                  `aria-label` = lang$nav$scen$tooltips$btRefresh))
                      ),
                      tags$div(title = lang$nav$scen$tooltips$btExport, class = "scen-button-tt",
                               HTML(paste0('<button type="button" class="btn btn-default scen-button"
onclick="Shiny.setInputValue(\'btExportScen\', ', scenId, ', {priority: \'event\'})"><i class="fas fa-download" role="presentation" aria-label="',
lang$nav$scen$tooltips$btExport, '"></i></button>'))
                      ),
tags$div(title = lang$nav$scen$tooltips$btTableView, class = "scen-button-tt",
         tags$button(class = "btn btn-default scen-button",
                     id = paste0("btScenTableView", scenId), type = "button",
                     onclick = paste0("Shiny.setInputValue('btScenTableView',",
                                      scenId, ",{priority:'event'})"),
                     tags$i(class = "fa fa-chart-bar",
                            role = "presentation",
                            `aria-label` = lang$nav$scen$tooltips$btTableView))
)
             )

    ),
tags$div(style = "margin-top: 10px;",
         scenTabset
)
  )
}
generateScenarioTabsetPivot <- function(hcubeMode = FALSE){
  fluidRow(
    tags$div(id = "scen-pivot-view", style = if(!identical(config$defCompMode, "pivot")) "display:none;",
             box(width = 12L, solidHeader = TRUE, status="primary", title =
                   tagList(tags$button(title = lang$nav$scen$tooltips$btAddPivot,
                                       class = "btn btn-default bt-icon action-button",
                                       onclick = "Shiny.setInputValue('btLoadScen',1,{priority: 'event'})",
                                       tags$i(class = "fas fa-folder-plus",
                                              `aria-label` = lang$nav$scen$tooltips$btAddPivot)),
                           tags$button(title = lang$nav$scen$tooltips$btRefresh,
                                       id = "btClosePivotComp",
                                       disabled = "true",
                                       style = "margin-left:10px",
                                       class = "btn btn-default bt-icon action-button",
                                       onclick = "Shiny.setInputValue('btRefreshComp',0,{priority: 'event'})",
                                       tags$i(class = "fas fa-sync-alt",
                                              `aria-label` = lang$nav$scen$tooltips$btRefresh)),
                           tags$div(style = "float:right;", title = lang$nav$scen$tooltips$btClosePivot,
                                    actionButton(inputId = "btScenPivot_close",
                                                 class = "bt-icon",
                                                 icon = icon("times"),
                                                 label = NULL))),
                 tags$div(id = "pivotCompBtWrapper", class = "no-scen", lang$nav$scen$noScen,
                          tags$div(style = "margin: 10px;",
                                   tags$button(class = "btn btn-default action-button",
                                               type = "button",
                                               onclick = "Shiny.setInputValue('btLoadScen',1,{priority: 'event'})",
                                               lang$nav$scen$btLoad))
                 ),
                 tags$div(id = "pivotCompScenWrapper", style = "margin-top: 10px;", style = "display:none")
             )
    )
  )
}

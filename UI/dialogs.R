showReadonlyDialog <- function(){
  showModal(modalDialog(
    title = lang$nav$dialogReadonly$title,
    lang$nav$dialogReadonly$desc,
    footer = tagList(
      modalButton(lang$nav$dialogReadonly$cancelButton),
      actionButton("btSaveReadonly", label = lang$nav$dialogReadonly$okButton, class = "btHighlight1")),
    fade = TRUE, easyClose = FALSE
  ))
}

showNewScenDialog <- function(tmpScenName){
  showModal(modalDialog(
    title = lang$nav$dialogNewScen$title,
    tags$div(id = "scenNameWrapper", 
             textInput("scenName", lang$nav$dialogNewScen$desc,
                       value = tmpScenName),
             selectizeInput("newScenTags", lang$nav$dialogNewScen$tags, c(),
                            multiple = TRUE, options = list(
                              'create' = TRUE,
                              'persist' = FALSE)
             )),
    tags$div(id = "badScenarioName", class = "errMsg", style = "display:none;", 
             lang$nav$dialogNewScen$badName),
    tags$div(id = "scenarioExits", class = "errMsg", style = "display:none;", 
             lang$nav$dialogNewScen$scenExits),
    footer = tagList(
      tags$div(id = "dialogSaveInit",
               modalButton(lang$nav$dialogNewScen$cancelButton),
               actionButton("btCheckName", lang$nav$dialogNewScen$okButton, 
                            class = "btHighlight1")
      ),
      tags$div(id = "dialogSaveConfirm", style = "display:none;",
               actionButton("btNewName", lang$nav$dialogNewScen$btNewName),
               actionButton("btSaveConfirm", lang$nav$dialogNewScen$btOverride, 
                            class = "btHighlight1")
      )
    ),
    fade = TRUE, easyClose = FALSE))
}

showCloseScenDialog <- function(scenId){
  showModal(modalDialog(
    title = lang$nav$dialogCloseScen$title,
    lang$nav$dialogCloseScen$desc,
    footer = tagList(
      modalButton(lang$nav$dialogCloseScen$cancelButton),
      actionButton("btCloseFinal_" %+% scenId, lang$nav$dialogCloseScen$okButton, class = "btHighlight1")),
    fade=FALSE, easyClose=FALSE))
}

showRemoveActiveScenFromUIDialog <- function(){
  showModal(modalDialog(
    title = lang$nav$dialogRemoveScen$title,
    lang$nav$dialogRemoveScen$desc,
    footer = tagList(
      modalButton(lang$nav$dialogRemoveScen$cancelButton),
      actionButton("btRemoveConfirm", label = lang$nav$dialogRemoveScen$okButton, class = "btHighlight1")),
    fade=TRUE, easyClose=FALSE))
}

showRemoveDeletedScenFromUIDialog <- function(){
  showModal(modalDialog(
    title = lang$nav$dialogDeleteScen$removeFromUI$title,
    lang$nav$dialogDeleteScen$removeFromUI$desc,
    footer = tagList(
      modalButton(lang$nav$dialogDeleteScen$removeFromUI$cancelButton),
      actionButton("btRemoveDeletedConfirm", label = lang$nav$dialogDeleteScen$removeFromUI$okButton, class = "btHighlight1")),
    fade=TRUE, easyClose=FALSE))
}

showDeleteScenDialog <- function(){
  showModal(modalDialog(
    title = lang$nav$dialogDeleteScen$title,
    tags$div(id = "deleteScen_db",
             lang$nav$dialogDeleteScen$desc
             ),
    tags$div(id = "deleteScen_ui", style = "display:none;",
             lang$nav$dialogDeleteScen$removeFromUI$desc),
    footer = tagList(
      modalButton(lang$nav$dialogDeleteScen$cancelButton),
      actionButton("btDeleteConfirm", lang$nav$dialogDeleteScen$okButton, class = "btHighlight1"),
      actionButton("btRemoveDeletedConfirm", 
                  label = lang$nav$dialogDeleteScen$removeFromUI$okButton, 
                  class = "btHighlight1", style = "display:none;")),
    fade=TRUE, easyClose=FALSE))
}

showRemoveExistingOutputDataDialog <- function(){
  showModal(modalDialog(
    title = lang$nav$dialogExistingOutput$title,
    lang$nav$dialogExistingOutput$desc,
    footer = tagList(
      modalButton(lang$nav$dialogExistingOutput$cancelButton),
      actionButton("btSaveOutput", label = lang$nav$dialogExistingOutput$saveOutputButton),
      actionButton("btRemoveOutput", label = lang$nav$dialogExistingOutput$discardOutputButton, class = "btHighlight1")),
    fade = TRUE, easyClose = FALSE))
}
showLoadDataDialog <- function(scenMetadata, noDataInUI = FALSE, dbTagList = NULL){
  tabLoadFromLocalFile <- tabPanel(lang$nav$dialogImport$tabLocal, value = "tb_importData_local",
                                   tags$div(class = "space"),
                                   tags$div(id = "loadLocal_content",
                                            fluidRow(
                                              column(12,
                                                     fileInput("localInput", lang$nav$dialogImport$descLocal, width = "100%",
                                                               multiple = FALSE,
                                                               accept = c("application/vnd.ms-excel", 
                                                                          "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", 
                                                                          ".xlsx")),
                                                     if(noDataInUI){
                                                       tagList(
                                                         tags$div(id = "local_badScenName", style = "display:none;", 
                                                                         class = "errMsg", 
                                                                         lang$nav$dialogImport$badScenName),
                                                         textInput("local_newScenName", 
                                                                   lang$nav$dialogImport$newScenName)
                                                       )
                                                     }
                                              )
                                            ),
                                            fluidRow(
                                              div(class= "choose-input", 
                                                  column(6,
                                                         tags$label(class = "checkbox-material flex-design", 
                                                                    'for'= "cbSelectManuallyLoc", 
                                                                    checkboxInput("cbSelectManuallyLoc", "", F), 
                                                                    lang$nav$dialogImport$cbSelectManually)
                                                  ),
                                                  column(6,
                                                         conditionalPanel(
                                                           condition = "input.cbSelectManuallyLoc == true",
                                                           selectInput("selInputDataLoc", lang$nav$dialogImport$selInputData, 
                                                                       setNames(as.list(names(modelInToImport)), 
                                                                                modelInToImportAlias), 
                                                                       multiple = TRUE, width = "100%")
                                                         )
                                                  )
                                              )
                                            ),
                                            fluidRow(
                                              tags$div(style = "text-align: center;",
                                                       tagAppendAttributes(
                                                         actionButton("btCheckSnameLocal", 
                                                                      lang$nav$dialogImport$okButton, 
                                                                      class = "btHighlight1"), disabled = ""
                                                       )
                                              )
                                            )
                                   ),
                                   if(config$activateModules$scenario){
                                     tags$div(id = "loadLocal_scenNameExists", style = "display:none;",
                                              fluidRow(
                                                tags$div(class = "errMsg",
                                                         lang$nav$dialogImport$scenNameExists
                                                )
                                              ),
                                              fluidRow(
                                                tags$div(style = "text-align: center;",
                                                         actionButton("btOverrideLocal", 
                                                                      lang$nav$dialogImport$overrideButton),
                                                         actionButton("btNewNameLocal", 
                                                                      lang$nav$dialogImport$newNameButton, 
                                                                      class = "btHighlight1")
                                                )
                                              )
                                     )
                                     },
                                   icon = icon("file"))
  
  if(config$activateModules$scenario){
    tabLoadFromDb <- tabPanel(lang$nav$dialogImport$tabDatabase, value = "tb_importData_remote",
                              fluidRow(
                                column(12,
                                       if(is.null(nrow(scenMetadata)) || !nrow(scenMetadata)){
                                         lang$nav$dialogLoadScen$descNoScen
                                       }else{
                                         list(
                                           tags$div(class = "space"),
                                           selectInput("selLoadScen", lang$nav$dialogLoadScen$selLoadScen, 
                                                       db$formatScenList(scenMetadata, stimeIdentifier, desc = TRUE), 
                                                       multiple = FALSE, width = "100%"),
                                           if(length(dbTagList)){
                                             selectInput("selLoadScenTags", lang$nav$dialogLoadScen$selTags, 
                                                         dbTagList, 
                                                         multiple = TRUE, width = "100%")
                                           },
                                           tags$div(
                                             lang$nav$dialogLoadScen$sortBy,
                                             actionButton("btSortName", label = lang$nav$dialogLoadScen$btSortNameASC, 
                                                          icon = icon("sort-by-alphabet", lib = "glyphicon"), 
                                                          class = "scen-sort-by"), 
                                             actionButton("btSortTime", label = lang$nav$dialogLoadScen$btSortTimeASC, 
                                                          icon = icon("sort-by-order", lib = "glyphicon"), 
                                                          class = "scen-sort-by")
                                           ),
                                           tags$div(class = "small-space"),
                                           tags$div(style = "text-align: center;",
                                                    actionButton("btLoadScenConfirm", 
                                                                 lang$nav$dialogLoadScen$okButton, 
                                                                 class = "btHighlight1")
                                           )
                                           
                                         )
                                       }
                                )
                              ),
                              icon = icon("database")
    )
  }
  showModal(modalDialog(
    title = lang$nav$dialogImport$title,
    tags$div(id = "importDataTabset",
             if(config$activateModules$scenario){
               tabBox(width = 12, id = "tb_importData", tabLoadFromDb, tabLoadFromLocalFile)
             }else{
               tabBox(width = 12, id = "tb_importData", tabLoadFromLocalFile)
             }
    ),
    tags$div(id = "importDataOverride", style = "display:none;",
             lang$nav$dialogImport$descOverrideInput
    ), footer = {
      tagList(
        modalButton(lang$nav$dialogImport$cancelButton),
        actionButton("btOverrideInput", label = lang$nav$dialogImport$okButton, 
                     class = "btHighlight1", style = "display:none;"),
        actionButton("btOverrideScen", label = lang$nav$dialogImport$okButton, 
                     class = "btHighlight1", style = "display:none;")
      )
    }
  ))
}
showLoadScenDialog <- function(dbScenList, uiScenList, isInSplitView, noDBPanel = FALSE, dbTagList = NULL){
  tabPanelUI <- NULL
  tabPanelDB <- NULL
  if(isInSplitView && length(uiScenList)){
    tabPanelUI <- tabPanel(lang$nav$dialogLoadScen$tabUI, icon = icon("file"),
                           value = "loadScenUI",
                           tags$div(class = "space"),
                           selectInput("selLoadScenUI", lang$nav$dialogLoadScen$selLoadScen, 
                                       uiScenList, 
                                       multiple = FALSE, width = "100%")
    )
  }
  if(!noDBPanel){
    tabPanelDB <- tabPanel(lang$nav$dialogLoadScen$tabDB, icon = icon("database"),
                           value = "loadScenDb",
                           tags$div(class = "space"),
                           selectInput("selLoadScen", lang$nav$dialogLoadScen$selLoadScen, 
                                       dbScenList, 
                                       multiple = if(isInSplitView) FALSE else TRUE, width = "100%"),
                           if(length(dbTagList)){
                             selectInput("selLoadScenTags", lang$nav$dialogLoadScen$selTags, 
                                         dbTagList, 
                                         multiple = TRUE, width = "100%")
                           },
                           tags$div(class = "space"),
                           tags$div(
                             lang$nav$dialogLoadScen$sortBy,
                             actionButton("btSortName", label = lang$nav$dialogLoadScen$btSortNameASC, 
                                          icon = icon("sort-by-alphabet", lib = "glyphicon"), class = "scen-sort-by"), 
                             actionButton("btSortTime", label = lang$nav$dialogLoadScen$btSortTimeASC, 
                                          icon = icon("sort-by-order", lib = "glyphicon"), class = "scen-sort-by")
                           )
    )
  }
  showModal(modalDialog(
    title = lang$nav$dialogLoadScen$title,
    if(is.null(tabPanelUI)){
      tabsetPanel(id = "tabsetLoadScen",
                  tabPanelDB
      )
    }else if(is.null(tabPanelDB)){
      tabsetPanel(id = "tabsetLoadScen",
                  tabPanelUI
      )
    }else{
      tabsetPanel(id = "tabsetLoadScen",
                  tabPanelDB, tabPanelUI
      )
    },
    footer = tagList(
      modalButton(lang$nav$dialogLoadScen$cancelButton),
      actionButton("btLoadScenConfirm", lang$nav$dialogLoadScen$okButton, 
                   class = "btHighlight1")
      ),
    fade = TRUE, easyClose = FALSE
  ))
  addClassEl(session, "#btSortTime", "scen-sort-by-selected")
}
showEditMetaDialog <- function(metadata, sharedScen = FALSE, ugroups = character(0L)){
  scenTags <- csv2Vector(metadata[["stag"]][[1]])
  showModal(modalDialog(
    title = lang$nav$dialogEditMeta$title,
    tags$div(class = "space"),
    tags$div(class = "errMsg", id = "editMetaBadName", style = "display:none;",
             lang$nav$dialogNewScen$badName),
    tags$div(class = "errMsg", id = "editMetaNameExists", style = "display:none;",
             lang$nav$dialogNewScen$scenExits),
    tags$div(class = "errMsg", id = "editMetaError", style = "display:none;", 
             lang$nav$dialogEditMeta$errMsg),
    tags$div(id = "editMetaSuccess", style = "display:none;", 
             lang$nav$dialogEditMeta$success),
    tags$div(id = "editMetaUI",
      textInput("editMetaName", lang$nav$dialogEditMeta$newName, 
                value = metadata[["sname"]][[1]]),
      selectizeInput("editMetaTags", lang$nav$dialogEditMeta$newTags, 
                     scenTags, selected = scenTags,
                     multiple = TRUE, options = list(
                       'create' = TRUE,
                       'persist' = FALSE)
      ),
      if(sharedScen && length(ugroups)){
        readPerm  <- csv2Vector(metadata[["readPerm"]][[1]])
        writePerm <- csv2Vector(metadata[["writePerm"]][[1]])
        ugroups   <- csv2Vector(ugroups)
        tagList(
          selectInput("editMetaReadPerm", lang$nav$dialogEditMeta$readPerm, 
                      ugroups, selected = csv2Vector(metadata[["writePerm"]][[1]]),
                      multiple = TRUE, options = list(
                        'create' = TRUE,
                        'persist' = FALSE)),
          selectInput("editMetaWritePerm", lang$nav$dialogEditMeta$writePerm, 
                      ugroups, selected = writePerm,
                      multiple = TRUE, options = list(
                        'create' = TRUE,
                        'persist' = FALSE))
        )
      }
    ),
    footer = tagList(
      modalButton(lang$nav$dialogEditMeta$cancelButton),
      actionButton("btUpdateMeta", lang$nav$dialogEditMeta$okButton, 
                   class = "btHighlight1")
    ),
    fade = TRUE, easyClose = FALSE
  ))
}
######## BATCH MODE

showBatchLoadMethodDialog <- function(attribs = NULL, maxSolversPaver = "", maxConcurentLoad = 0L){
  showModal(modalDialog(
    title = lang$nav$batchMode$configPaverDialog$title,
    tags$div(id="batchLoadMethod",
             if(length(sidsToLoad) <= maxConcurentLoad){
               lang$nav$batchMode$configPaverDialog$selectMethod
             }else{
               sprintf(lang$nav$batchMode$configPaverDialog$maxScenWarning1, maxConcurentLoad) %+% 
               lang$nav$batchMode$configPaverDialog$maxScenWarning2
             }
    ),
    tags$div(id="configPaver", style = "display:none;",
            lang$nav$batchMode$configPaverDialog$desc,
            tags$div(id = "configPaverMaxSolversErr", style = "display:none;", class = "errMsg",
                     sprintf(lang$nav$batchMode$configPaverDialog$tooManySolvers, 
                             maxSolversPaver)),
            selectInput("selPaverAttribs", lang$nav$batchMode$configPaverDialog$selAttribs, 
                         attribs, multiple = TRUE, width = "100%")
    ),
    tags$div(id="deleteTrace", style = "display:none;",
                    lang$nav$batchMode$configPaverDialog$delTrace
    ),
    footer = tagList(
      modalButton(lang$nav$batchMode$configPaverDialog$cancelButton),
      actionButton("btPaverConfig", lang$nav$batchMode$configPaverDialog$paverButton,
                   class = "btHighlight1"),
      actionButton("btPaver", lang$nav$batchMode$configPaverDialog$runButton, 
                   class = "btHighlight1", style = "display:none;"),
      if(length(sidsToLoad) <= maxConcurentLoad)
        actionButton("btBatchLoad", lang$nav$batchMode$configPaverDialog$interactiveButton)
    ),
    fade = TRUE, easyClose = FALSE
  ))
}
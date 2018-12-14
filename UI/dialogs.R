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
               actionButton("btSaveConfirm", lang$nav$dialogNewScen$btOverwrite, 
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
                                                         actionButton("btOverwriteLocal", 
                                                                      lang$nav$dialogImport$overwriteButton),
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
    tags$div(id = "importDataOverwrite", style = "display:none;",
             lang$nav$dialogImport$descOverwriteInput
    ), footer = {
      tagList(
        modalButton(lang$nav$dialogImport$cancelButton),
        actionButton("btOverwriteInput", label = lang$nav$dialogImport$okButton, 
                     class = "btHighlight1", style = "display:none;"),
        actionButton("btOverwriteScen", label = lang$nav$dialogImport$okButton, 
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
showEditMetaDialog <- function(metadata, sharedScen = FALSE, 
                               ugroups = character(0L), 
                               allowAttachments = FALSE, 
                               attachmentMetadata = character(0L), attachAllowExec = FALSE){
  scenTags <- csv2Vector(metadata[["stag"]][[1]])
  showModal(modalDialog(
    title = lang$nav$dialogEditMeta$title,
    tags$div(class = "gmsalert gmsalert-success", id = "attachSuccess", 
             lang$nav$dialogEditMeta$attachSuccess),
    tags$div(class = "gmsalert gmsalert-error", id = "editMetaBadName", 
             lang$nav$dialogNewScen$badName),
    tags$div(class = "gmsalert gmsalert-error", id = "editMetaNameExists",
             lang$nav$dialogNewScen$scenExits),
    tags$div(class = "gmsalert gmsalert-error", id = "editMetaError", 
             lang$nav$dialogEditMeta$errMsg),
    tags$div(class = "gmsalert gmsalert-error", id = "attachMaxNoError", 
             lang$nav$dialogEditMeta$attachMaxNoError),
    tags$div(class = "gmsalert gmsalert-error", id = "attachMaxSizeError", 
             lang$nav$dialogEditMeta$attachMaxSizeError),
    tags$div(class = "gmsalert gmsalert-error", id = "attachDuplicateError", 
             lang$nav$dialogEditMeta$attachDuplicateError),
    tags$div(class = "gmsalert gmsalert-error", id = "attachForbiddenFnameError", 
             lang$nav$dialogEditMeta$attachForbiddenFnameError),
    tags$div(class = "gmsalert gmsalert-error", id = "attachUnknownError", 
             lang$nav$dialogEditMeta$attachUnknownError),
    tags$div(class = "space"),
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
      },
      if(allowAttachments){
        tagList(
          tags$div(class = "labelClass", lang$nav$dialogEditMeta$attachmentsLabel),
          fileInput("file_addAttachments", lang$nav$dialogEditMeta$attachmentsAdd, multiple = TRUE),
          if(length(attachmentMetadata[["name"]])){
            lapply(seq_along(attachmentMetadata[["name"]]), function(i){
              tags$div(class = "row attachment-line", 
                       column(width = 6, 
                              HTML(paste0('<button class="btn btn-default btIcon" id="btRemoveAttachment_', i,
                                          '" type="button" onclick="removeAttachment(', i, ')"><i class="fa fa-times-circle"></i></button>')), 
                              downloadLink("downloadAttachment_" %+% i, attachmentMetadata[["name"]][[i]])
                              ),
                       if(attachAllowExec){
                         column(width = 6,
                                HTML(paste0('<div class="form-group shiny-input-container"><div class="checkbox"><label><input type="checkbox" onchange="Shiny.setInputValue(\'execPermAttachment_', 
                                            i, '\', $(this).is(\':checked\'));"', if(attachmentMetadata[["execPerm"]][[i]]) 'checked="checked"', '><span>', 
                                            lang$nav$dialogEditMeta$attachmentsExecPerm, '</span></label></div></div>'))
                                )
                       }
                    )
            })
          },
          tags$div(id = "endAttachList", class = "small-space"),
          genSpinner(id = "addAttachLoading", hidden = TRUE, absolute = FALSE)
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

showBatchLoadMethodDialog <- function(noScenSelected, attribs = NULL, maxSolversPaver = "", 
                                      maxConcurentLoad = 0L, hasRemovePerm = FALSE){
  showModal(modalDialog(
    title = list(lang$nav$batchMode$configPaverDialog$title, 
                 HTML('<button type="button" class="close" data-dismiss="modal"><span aria-hidden="true">&times;</span><span class="sr-only">Close</span></button>')),
    if(hasRemovePerm){
      tagList(
        tags$div(id = "batchRemoveConfirm", style = "display:none;",
                 sprintf(lang$nav$batchMode$configPaverDialog$removeConfirm, noScenSelected)
        ),
        tags$div(id = "batchRemoveSuccess", style = "display:none",
                 lang$nav$batchMode$configPaverDialog$removeSuccess),
        tags$div(id = "batchRemoveError", class = "errMsg", style = "display:none",
                 lang$nav$batchMode$configPaverDialog$removeError)
      )
    },
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
      if(hasRemovePerm){
        actionButton("btBatchRemove", lang$nav$batchMode$configPaverDialog$removeButton,
                     class = "btRemove")
      },
      tags$a(id="btBatchDownload", class='btn btn-default shiny-download-link',
             href='', target='_blank', download=NA, lang$nav$batchMode$configPaverDialog$downloadButton),
      actionButton("btPaverConfig", lang$nav$batchMode$configPaverDialog$paverButton),
      actionButton("btPaver", lang$nav$batchMode$configPaverDialog$runButton, 
                   class = "btHighlight1", style = "display:none;"),
      if(length(sidsToLoad) <= maxConcurentLoad)
        actionButton("btBatchLoad", lang$nav$batchMode$configPaverDialog$interactiveButton)
    ),
    fade = TRUE, easyClose = FALSE
  ))
}

showInvalidScenIdsDialog <- function(invalidScenIds){
  showModal(modalDialog(
    title = lang$nav$batchMode$invalidScenDialog$title,
    sprintf(lang$nav$batchMode$invalidScenDialog$desc, 
            length(invalidScenIds), paste(invalidScenIds, collapse = ", ")),
    footer = tagList(
      modalButton(lang$nav$batchMode$invalidScenDialog$cancelButton),
      actionButton("btBatchImportInvalid", label = lang$nav$batchMode$invalidScenDialog$okButton)
    ),
    fade = TRUE, easyClose = FALSE
  ))
}
showDuplicatedScenDialog <- function(noDupScen, dupScenTags, noScen){
  showModal(modalDialog(
    title = lang$nav$batchMode$duplicatedScenDialog$title,
    if(noScen == noDupScen){
      sprintf(lang$nav$batchMode$duplicatedScenDialog$allDuplicated, dupScenTags)
    }else{
      sprintf(lang$nav$batchMode$duplicatedScenDialog$someDuplicated, noDupScen, dupScenTags)
    },
    footer = tagList(
      modalButton(lang$nav$batchMode$duplicatedScenDialog$cancelButton),
      if(noScen != noDupScen){
        actionButton("btBatchImportNew", label = lang$nav$batchMode$duplicatedScenDialog$importNewButton)
      },
      actionButton("btBatchImportAll", label = lang$nav$batchMode$duplicatedScenDialog$importAllButton)
    ),
    fade = TRUE, easyClose = FALSE
  ))
}
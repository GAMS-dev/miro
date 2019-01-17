showReadonlyDialog <- function(){
  showModal(modalDialog(
    title = lang$nav$dialogReadonly$title,
    lang$nav$dialogReadonly$desc,
    footer = tagList(
      modalButton(lang$nav$dialogReadonly$cancelButton),
      actionButton("btSaveReadonly", label = lang$nav$dialogReadonly$okButton, 
                   class = "bt-highlight-1 bt-gms-confirm")),
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
    tags$div(id = "badScenarioName", class = "err-msg", style = "display:none;", 
             lang$nav$dialogNewScen$badName),
    tags$div(id = "scenarioExits", class = "err-msg", style = "display:none;", 
             lang$nav$dialogNewScen$scenExits),
    footer = tagList(
      tags$div(id = "dialogSaveInit",
               modalButton(lang$nav$dialogNewScen$cancelButton),
               actionButton("btCheckName", lang$nav$dialogNewScen$okButton, 
                            class = "bt-highlight-1 bt-gms-confirm")
      ),
      tags$div(id = "dialogSaveConfirm", style = "display:none;",
               actionButton("btNewName", lang$nav$dialogNewScen$btNewName),
               actionButton("btSaveConfirm", lang$nav$dialogNewScen$btOverwrite, 
                            class = "bt-highlight-1 bt-gms-confirm")
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
      actionButton("btCloseFinal_" %+% scenId, lang$nav$dialogCloseScen$okButton, 
                   class = "bt-highlight-1 bt-gms-confirm")),
    fade=FALSE, easyClose=FALSE))
}

showRemoveActiveScenFromUIDialog <- function(){
  showModal(modalDialog(
    title = lang$nav$dialogRemoveScen$title,
    lang$nav$dialogRemoveScen$desc,
    footer = tagList(
      modalButton(lang$nav$dialogRemoveScen$cancelButton),
      actionButton("btRemoveConfirm", label = lang$nav$dialogRemoveScen$okButton, 
                   class = "bt-highlight-1 bt-gms-confirm")),
    fade=TRUE, easyClose=FALSE))
}

showRemoveDeletedScenFromUIDialog <- function(){
  showModal(modalDialog(
    title = lang$nav$dialogDeleteScen$removeFromUI$title,
    lang$nav$dialogDeleteScen$removeFromUI$desc,
    footer = tagList(
      modalButton(lang$nav$dialogDeleteScen$removeFromUI$cancelButton),
      actionButton("btRemoveDeletedConfirm", 
                   label = lang$nav$dialogDeleteScen$removeFromUI$okButton, 
                   class = "bt-highlight-1 bt-gms-confirm")),
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
      actionButton("btDeleteConfirm", lang$nav$dialogDeleteScen$okButton, 
                   class = "bt-highlight-1 bt-gms-confirm"),
      actionButton("btRemoveDeletedConfirm", 
                  label = lang$nav$dialogDeleteScen$removeFromUI$okButton, 
                  class = "bt-highlight-1 bt-gms-confirm", style = "display:none;")),
    fade=TRUE, easyClose=FALSE))
}

showRemoveExistingOutputDataDialog <- function(){
  showModal(modalDialog(
    title = lang$nav$dialogExistingOutput$title,
    lang$nav$dialogExistingOutput$desc,
    footer = tagList(
      modalButton(lang$nav$dialogExistingOutput$cancelButton),
      actionButton("btSaveOutput", label = lang$nav$dialogExistingOutput$saveOutputButton),
      actionButton("btRemoveOutput", label = lang$nav$dialogExistingOutput$discardOutputButton, 
                   class = "bt-highlight-1 bt-gms-confirm")),
    fade = TRUE, easyClose = FALSE))
}
showLoadDataDialog <- function(scenMetadata, noDataInUI = FALSE, dbTagList = NULL){
  tabLoadFromLocalFile <- tabPanel(lang$nav$dialogImport$tabLocal, value = "tb_importData_local",
                                   tags$div(class = "space"),
                                   tags$div(id = "loadLocal_content",
                                            fluidRow(
                                              column(12,
                                                     fileInput("localInput", lang$nav$dialogImport$descLocal, 
                                                               width = "100%",
                                                               multiple = FALSE,
                                                               accept = c("application/vnd.ms-excel", 
                                                                          "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", 
                                                                          ".xlsx")),
                                                     if(noDataInUI){
                                                       tagList(
                                                         tags$div(id = "local_badScenName", style = "display:none;", 
                                                                         class = "err-msg", 
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
                                                                      class = "bt-highlight-1 bt-gms-confirm"), 
                                                         disabled = ""
                                                       )
                                              )
                                            )
                                   ),
                                   if(config$activateModules$scenario){
                                     tags$div(id = "loadLocal_scenNameExists", style = "display:none;",
                                              fluidRow(
                                                tags$div(class = "err-msg",
                                                         lang$nav$dialogImport$scenNameExists
                                                )
                                              ),
                                              fluidRow(
                                                tags$div(style = "text-align: center;",
                                                         actionButton("btOverwriteLocal", 
                                                                      lang$nav$dialogImport$overwriteButton),
                                                         actionButton("btNewNameLocal", 
                                                                      lang$nav$dialogImport$newNameButton, 
                                                                      class = "bt-highlight-1 bt-gms-confirm")
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
                                                                 class = "bt-highlight-1 bt-gms-confirm")
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
                     class = "bt-highlight-1 bt-gms-confirm", style = "display:none;"),
        actionButton("btOverwriteScen", label = lang$nav$dialogImport$okButton, 
                     class = "bt-highlight-1 bt-gms-confirm", style = "display:none;")
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
                   class = "bt-highlight-1 bt-gms-confirm")
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
          tags$div(class = "label-class", lang$nav$dialogEditMeta$attachmentsLabel),
          fileInput("file_addAttachments", lang$nav$dialogEditMeta$attachmentsAdd, multiple = TRUE),
          if(length(attachmentMetadata[["name"]])){
            lapply(seq_along(attachmentMetadata[["name"]]), function(i){
              tags$div(class = "row attachment-line", 
                       column(width = 6, 
                              HTML(paste0('<button class="btn btn-default bt-icon" id="btRemoveAttachment_', i,
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
                   class = "bt-highlight-1 bt-gms-confirm")
    ),
    fade = TRUE, easyClose = FALSE
  ))
}
######## HYPERCUBE MODE
showHcubeSubmitDialog <- function(noIdsToSolve, noIdsExist){
  showModal(modalDialog(
    tags$div(class = "gmsalert gmsalert-success", id = "hcubeSubmitSuccess",
             lang$nav$dialogHcube$success),
    tags$div(class = "gmsalert gmsalert-error", id = "hcubeSubmitWait",
             lang$nav$dialogHcube$waitTime),
    tags$div(class = "gmsalert gmsalert-error", id = "hcubeSubmitUnknownError",
             lang$nav$dialogHcube$unknownError),
    tags$div(paste0(sprintf(lang$nav$dialogHcube$desc, noIdsToSolve, 
                     noIdsExist), if(!identical(noIdsExist, noIdsToSolve)) 
                       lang$nav$dialogHcube$descSolveAgain)),
    tags$div(class = "small-space"),
    conditionalPanel(
      condition = "input.hcubeSolve_dl == 0",
      selectizeInput("newHcubeTags", lang$nav$dialogHcube$newTags, c(),
                     multiple = TRUE, options = list(
                       'create' = TRUE,
                       'persist' = FALSE))
    ),
    title = lang$nav$dialogHcube$title,
    footer = tagList(
      tags$div(style = "text-align:left;", 
               tags$i(class="fas fa-arrow-down", 
                      onclick = "$(this).next().slideToggle();$(this).toggleClass('fa-arrow-up');$(this).toggleClass('fa-arrow-down');", 
                      style = "cursor: pointer;"),
               tags$div(style = "display:none;",
                        tags$label(class = "cb-label", "for" = "hcubeSolve_dl", 
                                   lang$nav$dialogHcube$manualSwitch), 
                        tags$div(
                          tags$label(class = "checkbox-material", "for" = "hcubeSolve_dl", 
                                     checkboxInput("hcubeSolve_dl", label = NULL))
                        )
               )
      ),
      conditionalPanel(
        condition = "input.hcubeSolve_dl == 1",
        modalButton(lang$nav$dialogHcube$cancelButton),
        tags$a(id="btHcubeAll_dl", class='btn btn-default shiny-download-link',
               href='', target='_blank', download=NA, lang$nav$dialogHcube$processAllButton),
        if(noIdsExist > 0L && !identical(noIdsExist, noIdsToSolve)){
          tags$a(id="btHcubeNew_dl", class='btn btn-default shiny-download-link bt-highlight-1',
                 href='', target='_blank', download=NA, lang$nav$dialogHcube$processUnsolvedButton)
        }
      ),
      conditionalPanel(
        condition = "input.hcubeSolve_dl == 0",
        modalButton(lang$nav$dialogHcube$cancelButton),
        actionButton("btHcubeAll", lang$nav$dialogHcube$processAllButton),
        if(noIdsExist > 0L && !identical(noIdsExist, noIdsToSolve)){
          actionButton("btHcubeNew", lang$nav$dialogHcube$processUnsolvedButton,
                       class='bt-highlight-1 bt-gms-confirm')
        }
      )),
    fade = TRUE, easyClose = TRUE))
}
showInvalidScenIdsDialog <- function(invalidScenIds){
  showModal(modalDialog(
    title = lang$nav$hcubeMode$invalidScenDialog$title,
    sprintf(lang$nav$hcubeMode$invalidScenDialog$desc, 
            length(invalidScenIds), paste(invalidScenIds, collapse = ", ")),
    footer = tagList(
      modalButton(lang$nav$hcubeMode$invalidScenDialog$cancelButton),
      actionButton("btHcubeImportInvalid", label = lang$nav$hcubeMode$invalidScenDialog$okButton)
    ),
    fade = TRUE, easyClose = FALSE
  ))
}
showDuplicatedScenDialog <- function(noDupScen, dupScenTags, noScen){
  showModal(modalDialog(
    title = lang$nav$hcubeMode$duplicatedScenDialog$title,
    if(noScen == noDupScen){
      sprintf(lang$nav$hcubeMode$duplicatedScenDialog$allDuplicated, dupScenTags)
    }else{
      sprintf(lang$nav$hcubeMode$duplicatedScenDialog$someDuplicated, noDupScen, dupScenTags)
    },
    footer = tagList(
      modalButton(lang$nav$hcubeMode$duplicatedScenDialog$cancelButton),
      if(noScen != noDupScen){
        actionButton("btHcubeImportNew", label = lang$nav$hcubeMode$duplicatedScenDialog$importNewButton)
      },
      actionButton("btHcubeImportAll", label = lang$nav$hcubeMode$duplicatedScenDialog$importAllButton)
    ),
    fade = TRUE, easyClose = FALSE
  ))
}
# Hypercube analyze module
showHcubeLoadMethodDialog <- function(noScenSelected, attribs = NULL, maxSolversPaver = "", 
                                      maxConcurentLoad = 0L, hasRemovePerm = FALSE){
  showModal(modalDialog(
    title = list(lang$nav$hcubeMode$configPaverDialog$title, 
                 HTML('<button type="button" class="close" data-dismiss="modal"><span aria-hidden="true">&times;</span><span class="sr-only">Close</span></button>')),
    if(hasRemovePerm){
      tagList(
        tags$div(id = "hcubeRemoveConfirm", style = "display:none;",
                 sprintf(lang$nav$hcubeMode$configPaverDialog$removeConfirm, noScenSelected)
        ),
        tags$div(id = "hcubeRemoveSuccess", style = "display:none",
                 lang$nav$hcubeMode$configPaverDialog$removeSuccess),
        tags$div(id = "hcubeRemoveError", class = "err-msg", style = "display:none",
                 lang$nav$hcubeMode$configPaverDialog$removeError)
      )
    },
    tags$div(id="hcubeLoadMethod",
             if(length(sidsToLoad) <= maxConcurentLoad){
               lang$nav$hcubeMode$configPaverDialog$selectMethod
             }else{
               sprintf(lang$nav$hcubeMode$configPaverDialog$maxScenWarning1, maxConcurentLoad) %+% 
               lang$nav$hcubeMode$configPaverDialog$maxScenWarning2
             }
    ),
    tags$div(id="configPaver", style = "display:none;",
            lang$nav$hcubeMode$configPaverDialog$desc,
            tags$div(id = "configPaverMaxSolversErr", style = "display:none;", class = "err-msg",
                     sprintf(lang$nav$hcubeMode$configPaverDialog$tooManySolvers, 
                             maxSolversPaver)),
            selectInput("selPaverAttribs", lang$nav$hcubeMode$configPaverDialog$selAttribs, 
                         attribs, multiple = TRUE, width = "100%")
    ),
    tags$div(id="deleteTrace", style = "display:none;",
                    lang$nav$hcubeMode$configPaverDialog$delTrace
    ),
    footer = tagList(
      if(hasRemovePerm){
        actionButton("btHcubeRemove", lang$nav$hcubeMode$configPaverDialog$removeButton,
                     class = "bt-remove")
      },
      tags$a(id="btHcubeDownload", class='btn btn-default shiny-download-link',
             href='', target='_blank', download=NA, lang$nav$hcubeMode$configPaverDialog$downloadButton),
      actionButton("btPaverConfig", lang$nav$hcubeMode$configPaverDialog$paverButton),
      actionButton("btPaver", lang$nav$hcubeMode$configPaverDialog$runButton, 
                   class = "bt-highlight-1 bt-gms-confirm", style = "display:none;"),
      if(length(sidsToLoad) <= maxConcurentLoad)
        actionButton("btHcubeLoad", lang$nav$hcubeMode$configPaverDialog$interactiveButton)
    ),
    fade = TRUE, easyClose = FALSE
  ))
}
# Hypercube job import module
showManualJobImportDialog <- function(){
   showModal(modalDialog(
     title = lang$nav$hcubeMode$manualJobImportDialog$title,
     tags$div(
       tags$div(
         fileInput("hcubeImport", label = lang$nav$hcubeMode$manualJobImportDialog$uploadZip)
       ),
       tags$div(
         selectizeInput("hcubeTags", 
                        lang$nav$hcubeMode$manualJobImportDialog$hcubeTags, c(),
                        multiple = TRUE, options = list(
                          'create' = TRUE,
                          'persist' = FALSE)
         )
       )
     ),
     footer = tagList(
       modalButton(lang$nav$hcubeMode$manualJobImportDialog$cancelButton),
       actionButton("btUploadHcube", label = lang$nav$hcubeMode$manualJobImportDialog$uploadButton, 
                    class = "bt-highlight-1 bt-gms-confirm")
     ),
     fade = TRUE, easyClose = TRUE
   ))
}
getHypercubeJobsTable <- function(hcubeMeta, jobHist = FALSE){
  if(!inherits(hcubeMeta, "data.frame")){
    content <- tags$div(class = "err-msg", 
                        lang$nav$hcubeMode$importJobsDialog$unknownError
    )
  }else if(length(hcubeMeta) && nrow(hcubeMeta)){
    content <- tags$table(class = "cJob-wrapper",
                          tags$tr(
                            tags$th(lang$nav$hcubeMode$importJobsDialog$header$owner),
                            tags$th(lang$nav$hcubeMode$importJobsDialog$header$date),
                            tags$th(lang$nav$hcubeMode$importJobsDialog$header$tags),
                            tags$th(lang$nav$hcubeMode$importJobsDialog$header$status),
                            if(!jobHist)
                              tags$th(lang$nav$hcubeMode$importJobsDialog$header$action)
                          ),
                          do.call("tagList", lapply(seq_len(nrow(hcubeMeta)), function(i){
                            jStatus <- strsplit(hcubeMeta[[3]][i], "_", fixed = TRUE)[[1]][2]
                            jID     <- hcubeMeta[[1]][i]
                            
                            tags$tr(
                              tags$td(hcubeMeta[[2]][i]),
                              tags$td(hcubeMeta[[4]][i]),
                              tags$td(
                                if(jobHist){
                                  substr(hcubeMeta[[5]][i], 2, nchar(hcubeMeta[[5]][i]) - 1L)
                                }else{
                                  jTags   <- csv2Vector(hcubeMeta[[5]][i])
                                  selectizeInput("jTag_" %+% jID, label = NULL, choices = jTags,
                                                 selected = jTags,
                                                 multiple = TRUE, options = list(
                                                   'create' = TRUE,
                                                   'persist' = FALSE))
                                }
                              ),
                              if(startsWith(jStatus, "corrupted")){
                                jStatus <- strsplit(jStatus, "(", fixed = TRUE)[[1L]]
                                tags$td(class = "ttip", jStatus[1L], tags$span(
                                  if(startsWith(jStatus[2L], "noDir")) 
                                    lang$nav$hcubeMode$importJobsDialog$ttips$corruptedNoDir
                                  else if(startsWith(jStatus[2L], "noProcess"))
                                    lang$nav$hcubeMode$importJobsDialog$ttips$corruptedNoProcess))
                              }else if(startsWith(jStatus, "discarded")){
                                jStatus <- strsplit(jStatus, "(", fixed = TRUE)[[1L]]
                                tags$td(class = "ttip", jStatus[1L], tags$span(
                                  if(startsWith(jStatus[2L], "corrupted")) 
                                    lang$nav$hcubeMode$importJobsDialog$ttips$discardedCorrupted
                                  else if(startsWith(jStatus[2L], "running"))
                                    lang$nav$hcubeMode$importJobsDialog$ttips$discardedActive))
                              }else{
                                tags$td(jStatus)
                              },
                              if(!jobHist){
                                tags$td(
                                  if(identical(jStatus, "completed")){
                                    tagList(
                                      HTML(paste0('<button id="jImport_', jID, '" type="button" class="btn btn-default" onclick="confirmModalShow(\'', 
                                                  lang$nav$hcubeMode$importJobsDialog$importConfirm$title, '\', \'', 
                                                  lang$nav$hcubeMode$importJobsDialog$importConfirm$desc, '\', \'', 
                                                  lang$nav$hcubeMode$importJobsDialog$importConfirm$cancelButton, '\', \'', 
                                                  lang$nav$hcubeMode$importJobsDialog$importConfirm$confirmButton, 
                                                  '\', \'importHypercubeJob(', jID, 
                                                  ')\')">', lang$nav$hcubeMode$importJobsDialog$buttons$import, '</button>'))
                                    )
                                  },
                                  HTML(paste0('<button type="button" class="btn btn-default" onclick="showHypercubeLog(', jID, ')">', 
                                              lang$nav$hcubeMode$importJobsDialog$buttons$log, '</button>
                                   <button type="button" class="btn btn-default" onclick="confirmModalShow(\'', 
                                              lang$nav$hcubeMode$importJobsDialog$discardConfirm$title, '\', \'', 
                                              lang$nav$hcubeMode$importJobsDialog$discardConfirm$desc, '\', \'', 
                                              lang$nav$hcubeMode$importJobsDialog$discardConfirm$cancelButton, '\', \'', 
                                              lang$nav$hcubeMode$importJobsDialog$discardConfirm$confirmButton, 
                                              '\', \'discardHypercubeJob(', jID, 
                                              ')\')">', lang$nav$hcubeMode$importJobsDialog$buttons$discard, '</button>'))
                                )
                              }
                            )
                          }))
    )
  }else{
    content <- tags$div(style = "padding:20px;text-align:center;",
                        if(jobHist)
                          lang$nav$hcubeMode$importJobsDialog$noJobsHist
                        else
                          lang$nav$hcubeMode$importJobsDialog$noJobs
    )
  }
  if(jobHist){
    return(getHypercubeJobsTableSkeleton(content = content))
  }else{
    return(content)
  }
}
showJobsCompletedDialog <- function(){
  showModal(modalDialog(
    title = lang$nav$hcubeMode$jobsCompletedDialog$title,
    lang$nav$hcubeMode$jobsCompletedDialog$desc,
  fade = TRUE, easyClose = TRUE))
}
showJobHistoryDialog <- function(jobMeta){
  showModal(modalDialog(
    title = lang$nav$hcubeMode$importJobsDialog$histTitle,
    getHypercubeJobsTable(jobMeta, jobHist = TRUE),
    fade = TRUE, easyClose = TRUE, size = "l"
  ))
}
showHypercubeLogFileDialog <- function(logContent){
  showModal(modalDialog(
    title = lang$nav$hcubeMode$showLogFileDialog$title,
    tags$pre(style = "max-height:500px;max-height:70vh;overflow:auto;",
      if(length(logContent) && nchar(logContent)){
        logContent
      }else{
        lang$nav$hcubeMode$showLogFileDialog$noContent
      }
    ),
    fade = TRUE, easyClose = TRUE
  ))
}
# Hypercube load module
generateLine <- function(i, j, type, label){
  tags$div(id = "line" %+% i %+% "_" %+% j, class = "item-line",
           tags$div(class = "item-name", helpText(label)),
           tags$div(class = "item-scen-drop", switch(type,
                                                     number = {
                                                       selectInput("op_" %+% i %+% "_" %+% j, label=NULL, 
                                                                   choices = c('=', '<', '>', '<=', ">=", '!='))
                                                     },
                                                     text = {
                                                       selectInput("op_" %+% i %+% "_" %+% j, label=NULL, 
                                                                   choices = c(contains = "%LIKE%", 
                                                                               "doesn't contain" = "%NOTLIKE%",
                                                                               "starts with" = "LIKE%",
                                                                               "ends with" = "%LIKE",
                                                                               is = "=",
                                                                               "is not" = "!="), 
                                                                   selected = "=")
                                                     },
                                                     csv = {
                                                       selectInput("op_" %+% i %+% "_" %+% j, label=NULL, 
                                                                   choices = c(contains = "%LIKE%", 
                                                                               "doesn't contain" = "%NOTLIKE%",
                                                                               "starts with" = ",LIKE%",
                                                                               "ends with" = "%LIKE,",
                                                                               is = "%,LIKE,%",
                                                                               "is not" = "%,NOTLIKE,%"), 
                                                                   selected = "%,LIKE,%")
                                                     },
                                                     date = {
                                                       selectInput("op_" %+% i %+% "_" %+% j, label=NULL, 
                                                                   choices = c(between = "BETWEEN"))
                                                     })
           ),
           tags$div(class = "item-search-crit",
                    switch(type,
                           number = {
                             numericInput("val_" %+% i %+% "_" %+% j, label=NULL, 
                                          value = 0L)
                           },
                           date = {
                             dateRangeInput("val_" %+% i %+% "_" %+% j, label=NULL)
                           }, 
                           {
                             textInput("val_" %+% i %+% "_" %+% j, label=NULL)
                           })
                    
           ),
           tags$div(class = "item-delete",
                    actionButton("btRemoveLine" %+% i %+% "_" %+% j, label = "-", 
                                 style = "background-color: #fff;")
           )
  )
}
addHcubeLoadBlock <- function(id, choices){
  insertUI(
    selector = "#selectorsWrapper",
    where = "beforeEnd",
    ui = tags$div(id = "block" %+% id,
                  tags$div(id = "blockContent" %+% id, 
                           class = "grid-container",
                           if(id > 1L){
                             tags$hr()
                           }
                  ),
                  tags$div(class = "item-add-block",
                           tags$div(class = "item-and", 
                                    tags$span(style = "display:inline-block;vertical-align:middle;line-height: 70px;", 
                                              'AND')),
                           tags$div(class = "item-dropdown", 
                                    selectInput("newLine_" %+% id, "", 
                                                choices = fields)
                           ),
                           if(id > 1L){
                             tags$div(class = "item-delete",
                                      actionButton("btRemoveBlock" %+% id, label = "-", 
                                                   style = "background-color: #fff;"))
                           }
                  )
    )
  )
}

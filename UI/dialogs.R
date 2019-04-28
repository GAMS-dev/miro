showReadonlyDialog <- function(){
  if(config$activateModules$hcubeMode){
    modeDescriptor <- "dialogReadonly"
  }else{
    modeDescriptor <- "dialogReadonlyHC"
  }
  showModal(modalDialog(
    title = lang$nav[[modeDescriptor]]$title,
    lang$nav[[modeDescriptor]]$desc,
    footer = tagList(
      modalButton(lang$nav[[modeDescriptor]]$cancelButton),
      actionButton("btSaveReadonly", label = lang$nav[[modeDescriptor]]$okButton, 
                   class = "bt-highlight-1 bt-gms-confirm")),
    fade = TRUE, easyClose = FALSE
  ))
}

showNewScenDialog <- function(tmpScenName){
  if(config$activateModules$hcubeMode){
    modeDescriptor <- "dialogNewHCJob"
  }else{
    modeDescriptor <- "dialogNewScen"
  }
  showModal(modalDialog(
    title = lang$nav[[modeDescriptor]]$title,
    tags$div(id = "scenNameWrapper", 
             textInput("scenName", lang$nav[[modeDescriptor]]$desc,
                       value = tmpScenName),
             selectizeInput("newScenTags", lang$nav[[modeDescriptor]]$tags, c(),
                            multiple = TRUE, options = list(
                              'create' = TRUE,
                              'persist' = FALSE)
             )),
    tags$div(id = "badScenarioName", class = "gmsalert gmsalert-error", 
             lang$nav[[modeDescriptor]]$badName),
    tags$div(id = "scenarioExits", class = "err-msg", style = "display:none;", 
             lang$nav[[modeDescriptor]]$scenExits),
    footer = tagList(
      tags$div(id = "dialogSaveInit",
               modalButton(lang$nav[[modeDescriptor]]$cancelButton),
               actionButton("btCheckName", lang$nav[[modeDescriptor]]$okButton, 
                            class = "bt-highlight-1 bt-gms-confirm")
      ),
      tags$div(id = "dialogSaveConfirm", style = "display:none;",
               actionButton("btNewName", lang$nav[[modeDescriptor]]$btNewName),
               actionButton("btSaveConfirm", lang$nav[[modeDescriptor]]$btOverwrite, 
                            class = "bt-highlight-1 bt-gms-confirm")
      )
    ),
    fade = TRUE, easyClose = FALSE))
}

showCloseScenDialog <- function(scenId){
  if(config$activateModules$hcubeMode){
    modeDescriptor <- "dialogRemoveHCJob"
  }else{
    modeDescriptor <- "dialogCloseScen"
  }
  showModal(modalDialog(
    title = lang$nav[[modeDescriptor]]$title,
    lang$nav[[modeDescriptor]]$desc,
    footer = tagList(
      modalButton(lang$nav[[modeDescriptor]]$cancelButton),
      actionButton("btCloseFinal_" %+% scenId, lang$nav[[modeDescriptor]]$okButton, 
                   class = "bt-highlight-1 bt-gms-confirm")),
    fade=FALSE, easyClose=FALSE))
}

showDeleteScenDialog <- function(){
  if(config$activateModules$hcubeMode){
    modeDescriptor <- "dialogDeleteHCJob"
  }else{
    modeDescriptor <- "dialogDeleteScen"
  }
  showModal(modalDialog(
    title = lang$nav[[modeDescriptor]]$title,
    tags$div(id = "deleteScen_db",
             lang$nav[[modeDescriptor]]$desc
             ),
    tags$div(id = "deleteScen_ui", style = "display:none;",
             lang$nav[[modeDescriptor]]$removeFromUI$desc),
    footer = tagList(
      modalButton(lang$nav[[modeDescriptor]]$cancelButton),
      actionButton("btDeleteConfirm", lang$nav[[modeDescriptor]]$okButton, 
                   class = "bt-highlight-1 bt-gms-confirm"),
      actionButton("btRemoveDeletedConfirm", 
                  label = lang$nav[[modeDescriptor]]$removeFromUI$okButton, 
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
getLoadDbPanel <- function(id, title, scenList, tagList, iconName, modeDescriptor, async = FALSE){
  if(identical(id, "remote"))
    suffixInputs <- ""
  else
    suffixInputs <- "_base"
  content <- tagList(
    tags$div(class = "space"),
    selectInput("selLoadScen" %+% suffixInputs, lang$nav$dialogLoadScen$selLoadScen, 
                scenList, 
                multiple = FALSE, width = "100%"),
    if(length(tagList) || async){
      tags$div(id = "selLoadScenTagsDiv" %+% suffixInputs, 
               selectInput("selLoadScenTags" %+% suffixInputs, lang$nav$dialogLoadScen$selTags, 
                           tagList, 
                           multiple = TRUE, width = "100%")
      )
    },
    tags$div(
      lang$nav$dialogLoadScen$sortBy,
      actionButton("btSortName" %+% suffixInputs, label = lang$nav$dialogLoadScen$btSortNameASC, 
                   icon = icon("sort-by-alphabet", lib = "glyphicon"), 
                   class = "scen-sort-by"), 
      actionButton("btSortTime" %+% suffixInputs, label = lang$nav$dialogLoadScen$btSortTimeASC, 
                   icon = icon("sort-by-order", lib = "glyphicon"), 
                   class = "scen-sort-by scen-sort-by-selected")
    ),
    tags$div(class = "small-space"),
    tags$div(style = "text-align: center;",
             HTML(paste0('<button type="button" id="btLoadScenConfirm2" class="btn btn-default bt-highlight-1 bt-gms-confirm" 
onclick="Shiny.setInputValue(\'btLoadScenConfirm\', 1, {priority: \'event\'})">', lang$nav$dialogLoadScen$okButton, '</button>'))
    )
  )
  tabPanel(title, value = "tb_importData_" %+% id,
           tags$div(id = "loadData_content" %+% suffixInputs,
             fluidRow(
               column(12,
                      if(async){
                        tagList(
                          genSpinner("importDataDbSpinner"),
                          tags$div(id = "importDataDbUnknownError", style = "display:none;",
                                   lang$errMsg$unknownError), 
                          tags$div(id = "importDataDbNoContent", style = "display:none;", 
                                   lang$nav$dialogLoadScen$descNoScen),
                          tags$div(id = "importDataDbContent", style = "display:none;",
                                   content)
                        )
                      }else{
                        if(!length(scenList)){
                          lang$nav$dialogLoadScen$descNoScen
                        }else{
                          content
                        }
                      }
               )
             )
           ),
           if(!identical(id, "remote") && config$activateModules$scenario){
             tags$div(id = "loadBase_scenNameExists", style = "display:none;",
                      fluidRow(
                        tags$div(id = "loadBase_snameExistsMsg",
                                 tags$div(class = "err-msg",
                                          lang$nav[[modeDescriptor]]$scenNameExists
                                 )
                        ),
                        tags$div(id="loadBase_newName", style = "display:none;",
                                 textInput("base_newScenName", 
                                           lang$nav[[modeDescriptor]]$newScenName,
                                           width = "100%")
                          
                        )
                      ),
                      HTML(paste0(
                        '<div style="text-align: center;">
                        <div id="base-overwrite-container">
                          <button class="btn btn-default action-button" type="button" 
                                  onclick="Shiny.setInputValue(\'btOverwriteScen\', 1, 
                                                               {priority: \'event\'})">',
                        htmltools::htmlEscape(lang$nav[[modeDescriptor]]$overwriteButton), '</button>
                          <button class="btn btn-default action-button bt-highlight-1 bt-gms-confirm" 
                        type="button" onclick="showNewNameBaseDialog()">',
                        htmltools::htmlEscape(lang$nav[[modeDescriptor]]$newNameButton), '</button>
                        </div>
                        <button id="btCheckSnameBase" class="btn btn-default bt-highlight-1 
                        bt-gms-confirm" type="button" style="display:none;" 
                        onclick="validateSname(\'#base_newScenName\', \'btCheckSnameBaseConfirm\')">', 
                        htmltools::htmlEscape(lang$nav[[modeDescriptor]]$okButton), '</button>
                        </div>'
                      ))
             )
           },
           icon = icon(iconName)
  )
}
showLoadDataDialog <- function(scenListDb, noDataInUI = FALSE, dbTagList = NULL){
  if(config$activateModules$hcubeMode){
    modeDescriptor <- "dialogImportHC"
  }else{
    modeDescriptor <- "dialogImport"
  }
  tabLoadFromLocalFile <- tabPanel(lang$nav[[modeDescriptor]]$tabLocal, value = "tb_importData_local",
                                   tags$div(class = "space"),
                                   tags$div(id = "loadLocal_content",
                                            fluidRow(
                                              column(12,
                                                     fileInput("localInput", lang$nav[[modeDescriptor]]$descLocal, 
                                                               width = "100%",
                                                               multiple = FALSE,
                                                               accept = c("application/vnd.ms-excel", 
                                                                          "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", 
                                                                          ".xlsx",
                                                                          ".gdx")),
                                                     if(noDataInUI){
                                                       tagList(
                                                         tags$div(id = "local_badScenName",
                                                                  class = "gmsalert gmsalert-error", 
                                                                  lang$nav[[modeDescriptor]]$badScenName),
                                                         textInput("local_newScenName", 
                                                                   lang$nav[[modeDescriptor]]$newScenName,
                                                                   width = "100%")
                                                       )
                                                     }
                                              )
                                            ),
                                            fluidRow(
                                              div(class= "choose-input", 
                                                  column(6,
                                                         tags$label(class = "checkbox-material flex-design", 
                                                                    'for'= "cbSelectManuallyLoc", 
                                                                    checkboxInput("cbSelectManuallyLoc", "", FALSE), 
                                                                    lang$nav[[modeDescriptor]]$cbSelectManually)
                                                  ),
                                                  column(6,
                                                         conditionalPanel(
                                                           condition = "input.cbSelectManuallyLoc == true",
                                                           selectInput("selInputDataLoc", lang$nav[[modeDescriptor]]$selInputData, 
                                                                       setNames(as.list(names(modelInToImport)), 
                                                                                modelInToImportAlias), 
                                                                       multiple = TRUE, width = "100%")
                                                         )
                                                  )
                                              )
                                            ),
                                            fluidRow(
                                              tags$div(style = "text-align: center;",
                                                       HTML(paste0('<button id="btCheckSnameLocal" class="btn btn-default bt-highlight-1 bt-gms-confirm" 
type="button" onclick="validateSname(\'#local_newScenName\')" disabled>', htmltools::htmlEscape(lang$nav[[modeDescriptor]]$okButton), 
                                                                   '</button>'))
                                              )
                                            )
                                   ),
                                   if(config$activateModules$scenario){
                                     tags$div(id = "loadLocal_scenNameExists", style = "display:none;",
                                              fluidRow(
                                                tags$div(class = "err-msg",
                                                         lang$nav[[modeDescriptor]]$scenNameExists
                                                )
                                              ),
                                              fluidRow(
                                                tags$div(style = "text-align: center;",
                                                         actionButton("btOverwriteLocal", 
                                                                      lang$nav[[modeDescriptor]]$overwriteButton),
                                                         actionButton("btNewNameLocal", 
                                                                      lang$nav[[modeDescriptor]]$newNameButton, 
                                                                      class = "bt-highlight-1 bt-gms-confirm")
                                                )
                                              )
                                     )
                                     },
                                   icon = icon("file"))
  
  if(config$activateModules$scenario){
    tabLoadFromDb <- getLoadDbPanel(id = "remote", 
                                    title = lang$nav[[modeDescriptor]]$tabDatabase, 
                                    scenList = scenListDb, tagList = dbTagList,
                                    iconName = if(config$activateModules$hcube) "cube" else "database",
                                    modeDescriptor = modeDescriptor)
    if(config$activateModules$hcubeMode){
      tabLoadFromBase <- getLoadDbPanel(id = "base", 
                                        title = lang$nav[[modeDescriptor]]$tabBase, 
                                        scenList = character(0L), tagList = character(0L),
                                        iconName = "database", 
                                        modeDescriptor = modeDescriptor, async = TRUE)
    }else{
      tabLoadFromHcube <- tabPanel(lang$nav[[modeDescriptor]]$tabHcube, value = "tb_importData_hcube",
                                   fluidRow(
                                     column(12,
                                            tags$div(class = "space"),
                                            tags$div(
                                              lang$nav[[modeDescriptor]]$hcubeHashDesc,
                                              HTML(paste0('<div class="small-space"></div>
<input class="form-control" id="hcHashLookup" style="width:95%;font-size:10pt;"/>
                                                         <div class="space"></div>')),
                                              textInput("hcube_newScenName", 
                                                        lang$nav[[modeDescriptor]]$newScenName,
                                                        width = "95%"),
                                              HTML(paste0('<div class="small-space"></div>
                                                           <div style="text-align:center;">
                                                              <button class="btn btn-default bt-highlight-1" type="button" 
                                                                      onclick="validateHcubeHash()">',
                                                          htmltools::htmlEscape(lang$nav[[modeDescriptor]]$hcubeHashButton), 
                                                          '</button></div>')),
                                              genSpinner("hcHashLookup_load", absolute = TRUE, hidden = TRUE),
                                              tags$div(style = "max-height: 500px;overflow:auto;",
                                                       uiOutput("hcHashLookupResults")
                                              )
                                            )
                                     )
                                   ),
                                   icon = icon("cube")
      )
    }
  }
  showModal(modalDialog(
    title = lang$nav[[modeDescriptor]]$title,
    tags$div(id = "importScenMaxNoScen", class = "gmsalert gmsalert-error", 
             lang$nav$dialogLoadScen$maxNoScenExceeded),
    tags$div(id = "importScenNoHcubeScen", class = "gmsalert gmsalert-error", 
             lang$nav$dialogImport$hcubeHashNoMatch),
    tags$div(id = "importScenSnameExistsErr", class = "gmsalert gmsalert-error", 
             lang$nav$dialogImport$scenNameExistsErr),
    tags$div(id = "importScenError", class = "gmsalert gmsalert-error", 
             lang$errMsg$unknownError),
    tags$div(id = "importDataTabset",
             if(config$activateModules$scenario){
               if(config$activateModules$hcubeMode)
                 tabBox(width = 12, id = "tb_importData", 
                        tabLoadFromDb, tabLoadFromLocalFile, tabLoadFromBase)
               else
                 tabBox(width = 12, id = "tb_importData", 
                        tabLoadFromDb, tabLoadFromLocalFile, tabLoadFromHcube)
             }else{
               tabBox(width = 12, id = "tb_importData", tabLoadFromLocalFile)
             }
    ),
    tags$div(id = "importDataOverwrite", style = "display:none;",
             lang$nav[[modeDescriptor]]$descOverwriteInput
    ), footer = {
      tagList(
        modalButton(lang$nav[[modeDescriptor]]$cancelButton),
        actionButton("btOverwriteInput", label = lang$nav[[modeDescriptor]]$okButton, 
                     class = "bt-highlight-1 bt-gms-confirm", style = "display:none;"),
        actionButton("btOverwriteScen", label = lang$nav[[modeDescriptor]]$okButton, 
                     class = "bt-highlight-1 bt-gms-confirm", style = "display:none;")
      )
    }
  ))
}
getHcubeHashLookupTable <- function(hashLookupResults){
  if(!inherits(hashLookupResults, "data.frame")){
    tags$div(class = "err-msg", 
             lang$errMsg$unknownError
    )
  }else{
    tags$table(class = "cJob-wrapper",
               tags$tr(
                 tags$th(lang$nav$hcubeMode$importJobsDialog$header$tags),
                 tags$th(lang$nav$hcubeMode$importJobsDialog$header$date)
               ),
               do.call("tagList", lapply(seq_len(nrow(hashLookupResults)), function(i){
                 tags$tr(onclick = paste0("hcHashImport(", hashLookupResults[[1]][i], ")"),
                         tags$td(substr(hashLookupResults[[2]][i], 2, 
                                        nchar(hashLookupResults[[2]][i]) - 1L)),
                         tags$td(hashLookupResults[[3]][i])
                 )
               }))
    )
  }
}
showLoadScenDialog <- function(dbScenList, uiScenList, isInSplitView, noDBPanel = FALSE, 
                               dbTagList = NULL){
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
  if(!noDBPanel && length(dbScenList)){
    tabPanelDB <- tabPanel(lang$nav$dialogLoadScen$tabDB, icon = icon("database"),
                           value = "loadScenDb",
                           tags$div(class = "space"),
                           selectInput("selLoadScen", lang$nav$dialogLoadScen$selLoadScen, 
                                       dbScenList, 
                                       selected = if(isInSplitView) NULL else unname(uiScenList),
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
                                          icon = icon("sort-by-alphabet", lib = "glyphicon"), 
                                          class = "scen-sort-by"), 
                             actionButton("btSortTime", label = lang$nav$dialogLoadScen$btSortTimeASC, 
                                          icon = icon("sort-by-order", lib = "glyphicon"), 
                                          class = "scen-sort-by scen-sort-by-selected")
                           )
    )
  }
  showModal(modalDialog(
    title = lang$nav$dialogLoadScen$title,
    tags$div(id = "importScenMaxNoScen", class = "gmsalert gmsalert-error", 
             lang$nav$dialogLoadScen$maxNoScenExceeded),
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
}
showEditMetaDialog <- function(metadata, sharedScen = FALSE, 
                               ugroups = character(0L), 
                               allowAttachments = FALSE, 
                               attachmentMetadata = character(0L), attachAllowExec = FALSE){
  if(config$activateModules$hcubeMode){
    modeDescriptor <- "dialogEditMetaHC"
  }else{
    modeDescriptor <- "dialogEditMeta"
  }
  scenTags <- csv2Vector(metadata[["stag"]][[1]])
  showModal(modalDialog(
    title = lang$nav[[modeDescriptor]]$title,
    tags$div(class = "gmsalert gmsalert-success", id = "attachSuccess", 
             lang$nav[[modeDescriptor]]$attachSuccess),
    tags$div(class = "gmsalert gmsalert-error", id = "editMetaBadName", 
             lang$nav[[modeDescriptor]]$badName),
    tags$div(class = "gmsalert gmsalert-error", id = "editMetaNameExists",
             lang$nav[[modeDescriptor]]$scenExits),
    tags$div(class = "gmsalert gmsalert-error", id = "editMetaError", 
             lang$nav[[modeDescriptor]]$errMsg),
    tags$div(class = "gmsalert gmsalert-error", id = "attachMaxNoError", 
             lang$nav[[modeDescriptor]]$attachMaxNoError),
    tags$div(class = "gmsalert gmsalert-error", id = "attachMaxSizeError", 
             lang$nav[[modeDescriptor]]$attachMaxSizeError),
    tags$div(class = "gmsalert gmsalert-error", id = "attachDuplicateError", 
             lang$nav[[modeDescriptor]]$attachDuplicateError),
    tags$div(class = "gmsalert gmsalert-error", id = "attachForbiddenFnameError", 
             lang$nav[[modeDescriptor]]$attachForbiddenFnameError),
    tags$div(class = "gmsalert gmsalert-error", id = "attachRO", 
             lang$errMsg$permErr),
    tags$div(class = "gmsalert gmsalert-error", id = "attachUnknownError", 
             lang$errMsg$unknownError),
    tags$div(class = "space"),
    tags$div(id = "editMetaSuccess", style = "display:none;", 
             lang$nav[[modeDescriptor]]$success),
    tags$div(id = "editMetaUI",
      textInput("editMetaName", lang$nav[[modeDescriptor]]$newName, 
                value = metadata[["sname"]][[1]]),
      selectizeInput("editMetaTags", lang$nav[[modeDescriptor]]$newTags, 
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
          selectizeInput("editMetaReadPerm", lang$nav[[modeDescriptor]]$readPerm, 
                         ugroups, selected = csv2Vector(metadata[["writePerm"]][[1]]),
                         multiple = TRUE, options = list(
                           'create' = TRUE,
                           'persist' = FALSE)),
          selectizeInput("editMetaWritePerm", lang$nav[[modeDescriptor]]$writePerm, 
                         ugroups, selected = writePerm,
                         multiple = TRUE, options = list(
                           'create' = TRUE,
                           'persist' = FALSE))
        )
      },
      if(allowAttachments){
        tagList(
          tags$div(class = "label-class", lang$nav[[modeDescriptor]]$attachmentsLabel),
          fileInput("file_addAttachments", lang$nav[[modeDescriptor]]$attachmentsAdd, multiple = TRUE),
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
                                            lang$nav[[modeDescriptor]]$attachmentsExecPerm, '</span></label></div></div>'))
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
      modalButton(lang$nav[[modeDescriptor]]$cancelButton),
      actionButton("btUpdateMeta", lang$nav[[modeDescriptor]]$okButton, 
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
             lang$errMsg$unknownError),
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
                                      maxConcurentLoad = 0L, hasRemovePerm = FALSE, exclAttribChoices = NULL){
  showModal(modalDialog(
    title = list(lang$nav$hcubeMode$configPaverDialog$title, 
                 HTML('<button type="button" class="close" data-dismiss="modal"><span aria-hidden="true">&times;</span><span class="sr-only">Close</span></button>')),
    tags$div(class = "gmsalert gmsalert-error", id = "paverRunNoTrc", 
             lang$nav$hcubeMode$configPaverDialog$noTrc),
    tags$div(class = "gmsalert gmsalert-error", id = "paverRunUnknownError", 
             lang$errMsg$unknownError),
    tags$div(class = "gmsalert gmsalert-error", id = "configPaverMaxSolversErr", 
             sprintf(lang$nav$hcubeMode$configPaverDialog$tooManySolvers, 
                     maxSolversPaver)),
    if(hasRemovePerm){
      tagList(
        tags$div(class = "gmsalert gmsalert-success", id = "hcubeRemoveSuccess",
                 lang$nav$hcubeMode$configPaverDialog$removeSuccess),
        tags$div(class = "gmsalert gmsalert-error", id = "hcubeRemoveError",
                 lang$errMsg$unknownError),
        tags$div(id = "hcubeRemoveConfirm", style = "display:none;",
                 sprintf(lang$nav$hcubeMode$configPaverDialog$removeConfirm, noScenSelected)
        )
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
            selectInput("selPaverAttribs", lang$nav$hcubeMode$configPaverDialog$selAttribs, 
                         attribs, multiple = TRUE, width = "100%")
    ),
    tags$div(id="deleteTrace", style = "display:none;",
                    lang$nav$hcubeMode$configPaverDialog$delTrace
    ),
    footer = tagList(
      tags$div(style = "text-align:left;display:none;", id = "paverExclAttribContainer",
               tags$i(class="fas fa-arrow-down", 
                      onclick = "$(this).next().slideToggle();$(this).toggleClass('fa-arrow-up');$(this).toggleClass('fa-arrow-down');", 
                      style = "cursor: pointer;"),
               tags$div(style = "display:none;",
                        selectInput("paverExclAttrib", label = lang$nav$hcubeMode$configPaverDialog$selIgnoreAttribs, 
                                    choices = attribs, selected = exclAttribChoices, multiple = TRUE),
                        selectizeInput("paverClArgs", lang$nav$hcubeMode$configPaverDialog$selClArgs, c(),
                                       multiple = TRUE, options = list(
                                         'create' = TRUE,
                                         'persist' = FALSE))
               )
      ),
      if(hasRemovePerm){
        actionButton("btHcubeRemove", 
                     lang$nav$hcubeMode$configPaverDialog$removeButton,
                     class = "bt-remove")
      },
      tags$a(id="btHcubeDownload", class='btn btn-default shiny-download-link',
             href='', target='_blank', download=NA, lang$nav$hcubeMode$configPaverDialog$downloadButton),
      tagAppendAttributes(actionButton("btPaverConfig", lang$nav$hcubeMode$configPaverDialog$paverButton),
                          onclick = "$('#paverExclAttribContainer').show();$('#configPaver').show();
$('#btPaver').show();$('#btHcubeLoad').hide();$('#hcubeLoadMethod').hide();$('#btPaverConfig').hide();
                          $('#btHcubeDownload').hide();$('#btHcubeRemove').hide();"),
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
     tags$div(class = "gmsalert gmsalert-error", id = "manHcubeImportUnknownError", 
              lang$errMsg$unknownError),
     tags$div(
       tags$div(
         fileInput("hcubeImport", 
                   label = lang$nav$hcubeMode$manualJobImportDialog$uploadZip,
                   accept = c(".zip", "application/zip", 
                              "application/octet-stream", 
                              "application/x-zip-compressed", 
                              "multipart/x-zip"))
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
                        lang$errMsg$unknownError
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
                                    lang$nav$hcubeMode$importJobsDialog$ttips$corruptedNoProcess
                                  else if(startsWith(jStatus[2L], "man"))
                                    lang$nav$hcubeMode$importJobsDialog$ttips$corruptedManual))
                              }else if(startsWith(jStatus, "discarded")){
                                jStatus <- strsplit(jStatus, "(", fixed = TRUE)[[1L]]
                                if(length(jStatus) < 2L){
                                  tags$td(jStatus)
                                }else{
                                  tags$td(class = "ttip", jStatus[1L], tags$span(
                                    if(startsWith(jStatus[2L], "corrupted")) 
                                      lang$nav$hcubeMode$importJobsDialog$ttips$discardedCorrupted
                                    else if(startsWith(jStatus[2L], "running"))
                                      lang$nav$hcubeMode$importJobsDialog$ttips$discardedActive
                                    else if(startsWith(jStatus[2L], "scheduled"))
                                      lang$nav$hcubeMode$importJobsDialog$ttips$discardedScheduled
                                    else if(startsWith(jStatus[2L], "completed"))
                                      lang$nav$hcubeMode$importJobsDialog$ttips$discardedCompleted))
                                }
                              }else if(startsWith(jStatus, "imported")){
                                jStatus <- strsplit(jStatus, "(", fixed = TRUE)[[1L]]
                                if(identical(length(jStatus), 2L) && startsWith(jStatus[2L], "man"))
                                  tags$td(class = "ttip", jStatus[1L], tags$span(
                                    lang$nav$hcubeMode$importJobsDialog$ttips$importedManual))
                                else
                                  tags$td(jStatus)
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
                                              lang$nav$hcubeLoad$andButton)),
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
showHashDialog <- function(hash){
  showModal(modalDialog(
    title = lang$nav$hcubeLoad$showHashDialog$title,
    tags$div(
      lang$nav$hcubeLoad$showHashDialog$desc,
      tags$div(
        HTML(paste0('<input onClick="this.setSelectionRange(0, this.value.length)" value="', 
                    htmltools::htmlEscape(hash), '" class="form-control" style="width:490px;font-size:10pt;"/>'))
      )
    ),
    fade = TRUE, easyClose = TRUE
  ))
}

showReadonlyDialog <- function(){
  if(config$activateModules$hcubeMode){
    modeDescriptor <- "dialogReadonlyHC"
  }else{
    modeDescriptor <- "dialogReadonly"
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

showLoginDialog <- function(cred, forwardOnSuccess = NULL){
  showModal(modalDialog(
    title = lang$nav$dialogRemoteLogin$title,
    tags$div(class = "gmsalert gmsalert-error", id = "remoteLoginHostNotFound", 
             lang$nav$dialogRemoteLogin$hostNotFound),
    tags$div(class = "gmsalert gmsalert-error", id = "remoteLoginInvalidCred", 
             lang$nav$dialogRemoteLogin$invalidCred),
    tags$div(class = "gmsalert gmsalert-error", id = "remoteLoginNsNotFound", 
             lang$nav$dialogRemoteLogin$nsNotFound),
    tags$div(class = "gmsalert gmsalert-error", id = "remoteLoginInsuffPerm", 
             lang$nav$dialogRemoteLogin$insuffPerm),
    tags$div(class = "gmsalert gmsalert-error", id = "remoteLoginInvalidProt", 
             lang$nav$dialogRemoteLogin$invalidProtocol),
    lang$nav$dialogRemoteLogin$desc,
    div(class = "space"),
    textInput("remoteCredUrl", lang$nav$dialogRemoteLogin$url, cred$url),
    textInput("remoteCredUser", lang$nav$dialogRemoteLogin$username, cred$user),
    passwordInput("remoteCredPass", lang$nav$dialogRemoteLogin$password),
    textInput("remoteCredNs", lang$nav$dialogRemoteLogin$namespace, cred$ns),
    checkboxInput_MIRO("remoteCredReg", lang$nav$dialogRemoteLogin$useRegistered, cred$reg),
    checkboxInput_MIRO("remoteCredRemember", lang$nav$dialogRemoteLogin$remember),
    footer = tagList(
      modalButton(lang$nav$dialogRemoteLogin$cancelButton),
      if(length(forwardOnSuccess)){
        tags$button(class = "btn btn-default bt-highlight-1 bt-gms-confirm",
                    type = "button", onclick = paste0("Shiny.setInputValue('btSaveCredentials', '", 
                    forwardOnSuccess,"', {priority:'event'})"),
                    lang$nav$dialogRemoteLogin$okButton)
      }else{
        actionButton("btSaveCredentials", label = lang$nav$dialogRemoteLogin$okButton, 
                     class = "bt-highlight-1 bt-gms-confirm")
      }),
    fade = TRUE, easyClose = FALSE
  ))
}

showNewScenDialog <- function(tmpScenName = NULL, forwardTo = "btSaveConfirm", 
                              scenTags = character(0L)){
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
             selectizeInput("newScenTags", lang$nav[[modeDescriptor]]$tags, scenTags, 
                            selected = scenTags,
                            multiple = TRUE, options = list(
                              'create' = TRUE,
                              'persist' = FALSE)
             )),
    tags$div(id = "badScenarioName", class = "gmsalert gmsalert-error", 
             lang$nav[[modeDescriptor]]$badName),
    tags$div(id = "scenarioExits", class = "err-msg", style = "display:none;", 
             lang$nav[[modeDescriptor]]$scenExists),
    footer = tagList(
      tags$div(id = "dialogSaveInit",
               modalButton(lang$nav[[modeDescriptor]]$cancelButton),
               tags$button(class = "btn btn-default bt-highlight-1 bt-gms-confirm",
                           type = "button", onclick = paste0("Shiny.setInputValue('btCheckName', '", 
                                                             forwardTo, "', {priority:'event'})"),
                           lang$nav[[modeDescriptor]]$okButton)
      ),
      tags$div(id = "dialogSaveConfirm", style = "display:none;",
               actionButton("btNewName", lang$nav[[modeDescriptor]]$btNewName),
               actionButton(forwardTo, lang$nav[[modeDescriptor]]$btOverwrite, 
                            class = "bt-highlight-1 bt-gms-confirm")
      )
    ),
    fade = TRUE, easyClose = FALSE))
}

showRemoveScenDialog <- function(forwardTo){
  showModal(modalDialog(
    title = lang$nav$dialogRemoveScen$title,
    lang$nav$dialogRemoveScen$desc,
    footer = tagList(
      modalButton(lang$nav$dialogRemoveScen$cancelButton),
      tags$button(class = "btn btn-default bt-highlight-1 bt-gms-confirm",
                  type = "button", onclick = paste0("Shiny.setInputValue('", forwardTo, "',1,",
                                                    "{priority:'event'})"),
                  lang$nav$dialogRemoveScen$okButton)
    ), fade = TRUE, easyClose = FALSE))
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
                        type="button" onclick="Miro.showNewNameBaseDialog()">',
                        htmltools::htmlEscape(lang$nav[[modeDescriptor]]$newNameButton), '</button>
                        </div>
                        <button id="btCheckSnameBase" class="btn btn-default bt-highlight-1 
                        bt-gms-confirm" type="button" style="display:none;" 
                        onclick="Miro.validateSname(\'#base_newScenName\', \'btCheckSnameBaseConfirm\')">', 
                        htmltools::htmlEscape(lang$nav[[modeDescriptor]]$okButton), '</button>
                        </div>'
                      ))
             )
           },
           icon = icon(iconName)
  )
}
showLoadDataDialog <- function(scenListDb, dbTagList = NULL){
  if(config$activateModules$hcubeMode){
    modeDescriptor <- "dialogImportHC"
  }else{
    modeDescriptor <- "dialogImport"
  }
  tabLoadFromDb <- NULL
  tabLoadFromLocalFile <- NULL
  tabLoadFromExternalSource <- NULL
  tabLoadFromBase <- NULL 
  tabLoadFromHcube <- NULL 

  if(length(externalInputConfig)){
    tabLoadFromExternalSource <- tabPanel(lang$nav$dialogImport$tabExternal, vale = "tb_importData_external",
                                          tags$div(class = "space"),
                                          fluidRow(
                                            selectInput("selExternalSource", lang$nav$dialogImport$selExternalSource, 
                                                        names(externalInputConfig), 
                                                        multiple = FALSE, width = "100%")
                                          ),
                                          fluidRow(
                                            div(class= "choose-input", 
                                                column(6,
                                                       tags$label(class = "checkbox-material flex-design", 
                                                                  'for'= "cbSelectManuallyExt", 
                                                                  checkboxInput("cbSelectManuallyExt", "", FALSE), 
                                                                  lang$nav[[modeDescriptor]]$cbSelectManually)
                                                ),
                                                column(6,
                                                       conditionalPanel(
                                                         condition = "input.cbSelectManuallyExt === true",
                                                         selectInput("selInputDataExt", lang$nav[[modeDescriptor]]$selInputData, 
                                                                     setNames(names(modelInToImport), 
                                                                              modelInToImportAlias), 
                                                                     multiple = TRUE, width = "100%")
                                                       )
                                                )
                                            )
                                          ),
                                          fluidRow(
                                            tags$div(style = "text-align: center;",
                                                     actionButton("btImportExternal", class = "bt-highlight-1 bt-gms-confirm", 
                                                                  lang$nav[[modeDescriptor]]$okButton)
                                            )
                                          ), icon = icon("external-link-alt"))
  }
  if(config$activateModule$loadLocal){
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
                                                                            "application/zip", "text/csv", ".xlsx", ".csv", ".xls", ".zip",
                                                                            ".gdx"))
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
                                                             condition = "input.cbSelectManuallyLoc === true",
                                                             selectInput("selInputDataLoc", lang$nav[[modeDescriptor]]$selInputData, 
                                                                         setNames(names(modelInToImport), 
                                                                                  modelInToImportAlias), 
                                                                         multiple = TRUE, width = "100%")
                                                           )
                                                    )
                                                )
                                              ),
                                              fluidRow(
                                                tags$div(style = "text-align: center;",
                                                         actionButton("btImportLocal", class = "bt-highlight-1 bt-gms-confirm", 
                                                                      lang$nav[[modeDescriptor]]$okButton)
                                                )
                                              )
                                     ),
                                     icon = icon("file"))
  }
  
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
                                                                      onclick="Miro.validateHcubeHash()">',
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
  
  loadDataTabs <- list(tabLoadFromDb, tabLoadFromLocalFile, tabLoadFromExternalSource, 
                       tabLoadFromBase, tabLoadFromHcube)
  loadDataTabs <- loadDataTabs[!vapply(loadDataTabs, is.null, logical(1L), USE.NAMES = FALSE)]
  
  showModal(modalDialog(
    title = lang$nav[[modeDescriptor]]$title,
    fluidRow(
      tags$div(id = "importScenMaxNoScen", class = "gmsalert gmsalert-error", 
               lang$nav$dialogLoadScen$maxNoScenExceeded),
      tags$div(id = "importScenNoHcubeScen", class = "gmsalert gmsalert-error", 
               lang$nav$dialogImport$hcubeHashNoMatch),
      tags$div(id = "importScenSnameExistsErr", class = "gmsalert gmsalert-error", 
               lang$nav$dialogImport$scenNameExistsErr),
      tags$div(id = "importSceNoDsSelected", class = "gmsalert gmsalert-error", 
              lang$nav$dialogLoadScen$noDsSelected),
      tags$div(id = "importScenInvalidFile", class = "gmsalert gmsalert-error", 
               lang$nav$dialogLoadScen$invalidFile),
      tags$div(id = "importScenError", class = "gmsalert gmsalert-error", 
               lang$errMsg$unknownError),
      tags$div(id = "importDataTabset",
               do.call(tabBox, c(list(width = 12, id = "tb_importData"), loadDataTabs))
      ),
      tags$div(id = "importDataOverwrite", class = "col-sm-12", style = "display:none;",
               lang$nav[[modeDescriptor]]$descOverwriteInput
      )
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
                 tags$th(lang$nav$importJobsDialog$header$tags),
                 tags$th(lang$nav$importJobsDialog$header$date)
               ),
               do.call("tagList", lapply(seq_len(nrow(hashLookupResults)), function(i){
                 tags$tr(onclick = paste0("Miro.hcHashImport(", hashLookupResults[[1]][i], ")"),
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
  langData <- lang$nav[[modeDescriptor]]
  
  scenTags <- csv2Vector(metadata[["stag"]][[1]])
  
  contentAccessPerm <- NULL
  
  content <- tagList(
    tags$div(class = "gmsalert gmsalert-error", id = "editMetaBadName", 
             langData$badName),
    tags$div(class = "gmsalert gmsalert-error", id = "editMetaNameExists",
             langData$scenExists),
    tags$div(class = "space"),
             textInput("editMetaName", langData$newName, 
                       value = metadata[["sname"]][[1]]),
             selectizeInput("editMetaTags", langData$newTags, 
                            scenTags, selected = scenTags,
                            multiple = TRUE, options = list(
                              'create' = TRUE,
                              'persist' = FALSE)
             )
  )
  writePerm <- csv2Vector(metadata[["writePerm"]][[1]])
  if(sharedScen && length(ugroups) && any(ugroups %in% writePerm)){
    readPerm  <- csv2Vector(metadata[["readPerm"]][[1]])
    execPerm <- csv2Vector(metadata[["execPerm"]][[1]])
    
    contentAccessPerm <- tabPanel(langData$categoryAccessPerm, 
                                  tags$div(class = "gmsalert gmsalert-error", id = "editMetaEmptyPerm",
                                           langData$emptyPerm),
                                  tags$div(class = "gmsalert gmsalert-error", id = "editMetaIncapOwner",
                                           langData$incapOwner),
                                  tags$div(class = "gmsalert gmsalert-error", id = "editAccessRightsError", 
                                           langData$errMsg),
                                  tags$div(class = "space"),
        selectizeInput("editMetaReadPerm", lang$nav$excelExport$metadataSheet$readPerm, 
                       unique(c(readPerm, ugroups)), selected = readPerm,
                       multiple = TRUE),
        selectizeInput("editMetaWritePerm", lang$nav$excelExport$metadataSheet$writePerm, 
                       unique(c(writePerm, ugroups)), selected = writePerm,
                       multiple = TRUE),
        selectizeInput("editMetaExecPerm", lang$nav$excelExport$metadataSheet$execPerm, 
                       unique(c(execPerm, ugroups)), selected = execPerm,
                       multiple = TRUE)
      )
  }
  if(allowAttachments){
    contentAttachments <- tabPanel(langData$categoryAttachments,
                                   tags$div(class = "gmsalert gmsalert-success", id = "attachSuccess", 
                                            langData$attachSuccess),
                                   tags$div(class = "gmsalert gmsalert-error", id = "attachMaxNoError", 
                                            langData$attachMaxNoError),
                                   tags$div(class = "gmsalert gmsalert-error", id = "attachMaxSizeError", 
                                            langData$attachMaxSizeError),
                                   tags$div(class = "gmsalert gmsalert-error", id = "attachDuplicateError", 
                                            langData$attachDuplicateError),
                                   tags$div(class = "gmsalert gmsalert-error", id = "attachForbiddenFnameError", 
                                            langData$attachForbiddenFnameError),
                                   tags$div(class = "gmsalert gmsalert-error", id = "attachRO", 
                                            lang$errMsg$permErr),
                                   tags$div(class = "gmsalert gmsalert-error", id = "attachUnknownError", 
                                            lang$errMsg$unknownError),
                                   tags$div(class = "space"),
                                   downloadLink("downloadAttachmentData", "", style = "visibility:hidden;"),
                                   fileInput("file_addAttachments", langData$attachmentsAdd, multiple = TRUE),
                                   if(length(attachmentMetadata[["name"]])){
                                     lapply(seq_along(attachmentMetadata[["name"]]), function(i){
                                       tags$div(class = "row attachment-line", 
                                                column(width = 6, 
                                                       HTML(paste0('<button class="btn btn-default bt-icon" id="btRemoveAttachment_', i,
                                                                   '" type="button" onclick="Miro.removeAttachment(', i, ')"><i class="fa fa-times-circle"></i></button>
                                                                   <a href="#" onclick="Miro.downloadAttachment(', i, ')">', 
                                                                   htmltools::htmlEscape(attachmentMetadata[["name"]][[i]]), '</a>'))
                                                ),
                                                if(attachAllowExec){
                                                  column(width = 6,
                                                         HTML(paste0('<div class="form-group shiny-input-container"><div class="checkbox"><label><input type="checkbox" onchange="Shiny.setInputValue(\'execPermAttachment_', 
                                                                     i, '\', $(this).is(\':checked\'));"', if(attachmentMetadata[["execPerm"]][[i]]) 'checked="checked"', '><span>', 
                                                                     langData$attachmentsExecPerm, '</span></label></div></div>'))
                                                  )
                                                }
                                       )
                                     })
                                   },
                                   tags$div(id = "endAttachList", class = "small-space"),
                                   genSpinner(id = "addAttachLoading", hidden = TRUE, absolute = FALSE)
    )
    contentList <- list(tabPanel(langData$categoryGeneral, 
                                 content), contentAttachments)
    if(length(contentAccessPerm)){
      contentList[[3L]] <- contentAccessPerm
    }
    content <- do.call(tabsetPanel, contentList)
  }else if(length(contentAccessPerm)){
    content <- tabsetPanel(tabPanel(langData$categoryGeneral, 
                                    content),
                           contentAccessPerm)
  }
  showModal(modalDialog(
    title = langData$title,
    tags$div(
      tags$div(class = "gmsalert gmsalert-error", id = "editMetaError", 
               langData$errMsg),
      tags$div(id = "editMetaSuccess", style = "display:none;", 
               langData$success),
      tags$div(id = "editMetaUI",
               content)
    ),
    footer = tagList(
      modalButton(langData$cancelButton),
      actionButton("btUpdateMeta", langData$okButton, 
                   class = "bt-highlight-1 bt-gms-confirm")
    ),
    fade = TRUE, easyClose = FALSE
  ))
}
showScenExportDialog <- function(id, exportTypes){
  showModal(modalDialog(
    title = lang$nav$dialogExportScen$title,
    tags$div(class = "gmsalert gmsalert-error", id = "exportNoDsSelected", 
             lang$nav$dialogExportScen$noDsSelected),
    selectInput("exportFileType", lang$nav$dialogExportScen$desc, exportTypes),
    div(class= "choose-input", 
        column(6,
               tags$label(class = "checkbox-material flex-design", 
                          'for'= "cbSelectManuallyExp", 
                          checkboxInput("cbSelectManuallyExp", "", FALSE), 
                          lang$nav$dialogExportScen$cbSelectManually)
        ),
        column(6,
               conditionalPanel(
                 condition = "input.cbSelectManuallyExp === true",
                 selectInput("selDataToExport", lang$nav$dialogExportScen$selDatasets, 
                             setNames(c(inputDsNames, names(modelOut)), 
                                      c(inputDsAliases, modelOutAlias)), 
                             multiple = TRUE, width = "100%")
               )
        )
    ),
    footer = tagList(
      modalButton(lang$nav$dialogExportScen$cancelButton),
      downloadButton(paste0("export_", id), lang$nav$dialogExportScen$okButton,  
                     class = "bt-highlight-1 bt-gms-confirm file-export"),
      actionButton(paste0("remote_export_", id), lang$nav$dialogExportScen$okButton,  
                   class = "bt-highlight-1 bt-gms-confirm remote-export", 
                   style = "display: none;")
      ), fade = TRUE, easyClose = TRUE
  ))
}
showJobSubmissionDialog <- function(jobName = ""){
  showModal(modalDialog(
    tags$div(class = "gmsalert gmsalert-success", style = "position:relative;",
             id = "jobSubmitSuccess",
             lang$nav$dialogJobSubmission$successMsg),
    tags$div(class = "gmsalert gmsalert-error", id = "jobSubmitBadName",
             lang$nav$dialogJobSubmission$badName),
    tags$div(class = "gmsalert gmsalert-error", id = "jobSubmitUnknownHost",
             style = "position:relative;", 
             lang$nav$dialogRemoteLogin$hostNotFound),
    tags$div(class = "gmsalert gmsalert-error", id = "jobSubmitUnauthorized",
             style = "position:relative;", 
             lang$nav$dialogRemoteLogin$invalidCred),
    tags$div(class = "gmsalert gmsalert-error", id = "jobSubmitUnknownError",
             lang$errMsg$unknownError),
    tags$div(id = "jobSubmissionLoad", style = "display:none;text-align:center;",
             lang$nav$dialogJobSubmission$descWait,
             tags$div(class = "space"),
             genSpinner(hidden = FALSE, absolute = FALSE)),
    tags$div(id = "jobSubmissionWrapper", 
             lang$nav$dialogJobSubmission$desc,
             tags$div(class = "space"),
             textInput("jobSubmissionName", lang$nav$dialogJobSubmission$jobName, jobName),
             tags$div(class = "small-space")
    ),
    title = lang$nav$dialogJobSubmission$title,
    footer = tagList(
      modalButton(lang$nav$dialogJobSubmission$cancelButton),
      actionButton("btSubmitAsyncJob", lang$nav$dialogJobSubmission$okButton,
                   class='bt-highlight-1 bt-gms-confirm')),
    fade = TRUE, easyClose = TRUE))
}
######## HYPERCUBE MODE
showHcubeSubmitDialog <- function(noIdsToSolve, noIdsExist){
  showModal(modalDialog(
    tags$div(class = "gmsalert gmsalert-success", style = "position:relative;", 
             id = "hcubeSubmitSuccess",
             lang$nav$dialogHcube$successMsg),
    tags$div(class = "gmsalert gmsalert-error", id = "hcubeSubmitUnknownHost",
             style = "position:relative;", 
             lang$nav$dialogRemoteLogin$hostNotFound),
    tags$div(class = "gmsalert gmsalert-error", id = "hcubeSubmitUnauthorized",
             style = "position:relative;", 
             lang$nav$dialogRemoteLogin$invalidCred),
    tags$div(class = "gmsalert gmsalert-error", id = "hcubeSubmitWait",
             style = "position:relative;", 
             lang$nav$dialogHcube$waitTime),
    tags$div(class = "gmsalert gmsalert-error", id = "hcubeSubmitUnknownError",
             style = "position:relative;", 
             lang$errMsg$unknownError),
    tags$div(id = "jobSubmissionLoad", style = "display:none;text-align:center;",
             lang$nav$dialogHcube$descJobSubmission,
             tags$div(class = "space"),
             genSpinner(hidden = FALSE, absolute = FALSE)),
    tags$div(id = "jobSubmissionWrapper", 
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
             )
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
showNewCompletedJobsDialog <- function(hcubeMode = FALSE){
  showModal(modalDialog(
    title = lang$nav$newCompletedJobsDialog$title,
    lang$nav$newCompletedJobsDialog$desc,
    footer = tagList(
      modalButton(lang$nav$newCompletedJobsDialog$cancelButton),
      tags$button(class = "btn btn-default bt-highlight-1 bt-gms-confirm",
                  type = "button", onclick = paste0("Miro.showJobsDialog(", 
                                                    if(hcubeMode) "true" else "false", ")"),
                  lang$nav$newCompletedJobsDialog$okButton)
    ),
    fade = TRUE, easyClose = FALSE
  ))
}
getJobsTable <- function(hcubeMeta, jobHist = FALSE, hcubeMode = TRUE, showLogFileDialog = TRUE){
  if(!inherits(hcubeMeta, "data.frame")){
    content <- tags$div(class = "err-msg", 
                        lang$errMsg$unknownError
    )
  }else if(length(hcubeMeta) && nrow(hcubeMeta)){
    jobIds     <- hcubeMeta[[1]]
    jobOwners  <- hcubeMeta[[2]]
    jobTimes   <- hcubeMeta[[4]]
    jStatuses  <- hcubeMeta[[3]]
    content <- tags$table(class = "cJob-wrapper",
                          tags$tr(
                            tags$th(lang$nav$importJobsDialog$header$owner),
                            tags$th(lang$nav$importJobsDialog$header$date),
                            if(hcubeMode)
                              tags$th(lang$nav$importJobsDialog$header$tags)
                            else
                              tags$th(lang$nav$importJobsDialog$header$name),
                            tags$th(lang$nav$importJobsDialog$header$status),
                            if(!jobHist)
                              tags$th(lang$nav$importJobsDialog$header$action)
                          ),
                          do.call("tagList", lapply(seq_len(nrow(hcubeMeta)), function(i){
                            jID     <- jobIds[i]
                            jStatus <- jStatuses[i]
                            tags$tr(
                              tags$td(jobOwners[i]),
                              tags$td(jobTimes[i]),
                              if(hcubeMode)
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
                                )
                              else
                                tags$td(
                                  hcubeMeta[[10]][i]
                                ),
                              if(identical(jStatus, JOBSTATUSMAP[['running']])){
                                tags$td(lang$nav$importJobsDialog$status$running)
                              }else if(identical(jStatus, JOBSTATUSMAP[['completed']])){
                                tags$td(tags$div(lang$nav$importJobsDialog$status$completed), 
                                        tags$div(class = "progress", 
                                                 style = "display:none;margin-bottom:0;border:1px solid black;",
                                                 id = paste0("jobImportDlProgressWrapper_", jID),
                                                 tags$div(class = "progress-bar progress-bar-striped active", 
                                                          id = paste0("jobImportDlProgress_", jID),
                                                          role = "progressbar", `aria-valuenow` = 5L, 
                                                          `aria-valuemin` = 0, `aria-valuemax` = 100, 
                                                          style = paste0("width:", 5L, "%;"),
                                                          lang$nav$importJobsDialog$status$downloading)
                                        ))
                              }else if(identical(jStatus, JOBSTATUSMAP[['downloaded']])){
                                tags$td(lang$nav$importJobsDialog$status$downloaded)
                              }else if(identical(jStatus, JOBSTATUSMAP[['corrupted']])){
                                tags$td(lang$nav$importJobsDialog$status$corrupted)
                              }else if(identical(jStatus, JOBSTATUSMAP[['corrupted(noDir)']])){
                                tags$td(class = "ttip", lang$nav$importJobsDialog$status$corrupted, 
                                        tags$span(
                                          lang$nav$importJobsDialog$ttips$corruptedNoDir))
                              }else if(identical(jStatus, JOBSTATUSMAP[['corrupted(noProcess)']])){
                                tags$td(class = "ttip", lang$nav$importJobsDialog$status$corrupted, 
                                        tags$span(
                                          lang$nav$importJobsDialog$ttips$corruptedNoProcess))
                              }else if(identical(jStatus, JOBSTATUSMAP[['corrupted(man)']])){
                                tags$td(class = "ttip", lang$nav$importJobsDialog$status$corrupted, 
                                        tags$span(
                                          lang$nav$importJobsDialog$ttips$corruptedManual))
                              }else if(identical(jStatus, JOBSTATUSMAP[['discarded']])){
                                tags$td(lang$nav$importJobsDialog$status$discarded)
                              }else if(identical(jStatus, JOBSTATUSMAP[['discarded(corrupted)']])){
                                tags$td(class = "ttip", lang$nav$importJobsDialog$status$discarded, 
                                        tags$span(
                                          lang$nav$importJobsDialog$ttips$discardedCorrupted))
                              }else if(identical(jStatus, JOBSTATUSMAP[['discarded(scheduled)']])){
                                tags$td(class = "ttip", lang$nav$importJobsDialog$status$discarded, 
                                        tags$span(
                                          lang$nav$importJobsDialog$ttips$discardedScheduled))
                              }else if(identical(jStatus, JOBSTATUSMAP[['discarded(running)']])){
                                tags$td(class = "ttip", lang$nav$importJobsDialog$status$discarded, 
                                        tags$span(
                                          lang$nav$importJobsDialog$ttips$discardedActive))
                              }else if(identical(jStatus, JOBSTATUSMAP[['discarded(completed)']])){
                                tags$td(class = "ttip", lang$nav$importJobsDialog$status$discarded, 
                                        tags$span(
                                          lang$nav$importJobsDialog$ttips$discardedCompleted))
                              }else if(identical(jStatus, JOBSTATUSMAP[['imported']])){
                                tags$td("imported")
                              }else if(identical(jStatus, JOBSTATUSMAP[['imported(man)']])){
                                tags$td(class = "ttip", lang$nav$importJobsDialog$status$discarded,
                                        tags$span(
                                          lang$nav$importJobsDialog$ttips$importedManual))
                              }else{
                                tags$td(jStatus)
                              },
                              if(!jobHist){
                                tags$td(
                                  if(jStatus %in% c(JOBSTATUSMAP[['completed']],
                                                    JOBSTATUSMAP[['downloaded']])){
                                    tagList(
                                      tags$button(class = "btn btn-default",
                                                  style = if(identical(jStatus, JOBSTATUSMAP[['completed']]))
                                                    "" else "display:none;",
                                                  id = paste0("btDownloadJob_", jID),
                                                  onclick = paste0("Shiny.setInputValue('downloadJobData',", 
                                                                   jID, ",{priority:\'event\'});"),
                                                  lang$nav$importJobsDialog$buttons$download),
                                      tags$button(class = "btn btn-default", 
                                                  style = if(identical(jStatus, JOBSTATUSMAP[['completed']]))
                                                    "display:none" else "",
                                                  id = paste0("btImportJob_", jID),
                                                  onclick = paste0("Shiny.setInputValue('importJob',", 
                                                                   jID, ",{priority:\'event\'});"),
                                                  lang$nav$importJobsDialog$buttons$import),
                                      if(!hcubeMode && showLogFileDialog)
                                        tags$button(class = "btn btn-default", 
                                                    onclick = paste0("Shiny.setInputValue('showJobLog', '", 
                                                                     jID, "',{priority:\'event\'});"),
                                                    lang$nav$importJobsDialog$buttons$log)
                                    )
                                  },
                                  if(hcubeMode && identical(jStatus, JOBSTATUSMAP[['running']]))
                                    tags$button(class = "btn btn-default", 
                                                onclick = paste0("Shiny.setInputValue('showJobProgress', '", 
                                                                 jID, "',{priority:\'event\'});"),
                                                lang$nav$importJobsDialog$buttons$progress),
                                  tags$button(class = "btn btn-default", 
                                              onclick = paste0("Miro.confirmModalShow('",
                                              lang$nav$importJobsDialog$discardConfirm$title, "', '", 
                                              lang$nav$importJobsDialog$discardConfirm$desc, "', '", 
                                              lang$nav$importJobsDialog$discardConfirm$cancelButton, "', '",  
                                              lang$nav$importJobsDialog$discardConfirm$confirmButton, 
                                              "', 'Shiny.setInputValue(\\'discardJob\\',", jID, 
                                              ",{priority:\\'event\\'})')"),
                                              lang$nav$importJobsDialog$buttons$discard)
                                )
                              }
                            )
                          }))
    )
  }else{
    content <- tags$div(style = "padding:20px;text-align:center;",
                        if(jobHist)
                          lang$nav$importJobsDialog$noJobsHist
                        else
                          lang$nav$importJobsDialog$noJobs
    )
  }
  if(jobHist){
    return(getJobsTableSkeleton(content = content))
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
showJobHistoryDialog <- function(jobMeta, hcubeMode = TRUE){
  showModal(modalDialog(
    title = lang$nav$importJobsDialog$histTitle,
    getJobsTable(jobMeta, jobHist = TRUE, hcubeMode = hcubeMode),
    fade = TRUE, easyClose = TRUE, size = "l"
  ))
}
showJobLogFileDialog <- function(jID){
  logTabsetList <- list()
  if(config$activateModules$logFile){
    logTabsetList$log <- tabPanel(title=tags$div(class="log-tab-color", lang$nav$gams$boxGamsOutput$gamsOutputTabset$logFile),
                                  value = paste0("log_", jID),
                                  tags$pre(style = "max-height:400px;max-height:50vh;overflow:auto;",
                                           id = "asyncLogContainer",
                                           genSpinner()
                                  ))
  }
  if(config$activateModules$lstFile){
    logTabsetList$lst <- tabPanel(title = tags$div(class="log-tab-color", lang$nav$gams$boxGamsOutput$gamsOutputTabset$lstFile),
                                  value = paste0("listfile_", jID),
                                  tags$pre(style = "max-height:400px;max-height:50vh;overflow:auto;",
                                           id = "asyncLstContainer", 
                                           genSpinner()
                                  ))
  }
  if(config$activateModules$miroLogFile){
    logTabsetList$miroLog <- tabPanel(title = tags$div(class="log-tab-color", lang$nav$gams$boxGamsOutput$gamsOutputTabset$miroLogFile),
                                      value = paste0("mirolog_", jID),
                                      tags$pre(style = "max-height:400px;max-height:50vh;overflow:auto;",
                                               id = "asyncMiroLogContainer",
                                               genSpinner()
                                      ))
  }
  logTabsetList <- unname(logTabsetList)
  logTabsetList$id <- "asyncLogFileTabsset"
  showModal(modalDialog(
    title = lang$nav$hcubeMode$showLogFileDialog$title,
    do.call(tabsetPanel, logTabsetList),
    footer = modalButton(lang$nav$hcubeMode$showLogFileDialog$cancelButton),
    fade = TRUE, easyClose = TRUE
  ))
}
showJobProgressDialog <- function(jID, progressStatus){
  percentCompleted <- round(progressStatus$noCompleted/progressStatus$noTotal * 100)
  showModal(modalDialog(
    title = lang$nav$hcubeMode$showJobProgressDialog$title,
    tags$div(class = "progress",
             tags$div(class = "progress-bar progress-bar-striped active", 
                      id = paste0("hcubeProgress", jID), 
                      role = "progressbar", `aria-valuenow` = percentCompleted, 
                      `aria-valuemin` = 0, `aria-valuemax` = 100, 
                      style = paste0("width:", percentCompleted, "%;"),
                      paste0(progressStatus$noCompleted, "/", progressStatus$noTotal))
    ),
    footer = modalButton(lang$nav$hcubeMode$showJobProgressDialog$cancelButton),
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
                    actionButton("btRemoveLine" %+% i %+% "_" %+% j, label = "-")
           )
  )
}
addHcubeLoadBlock <- function(id, choices){
  insertUI(
    selector = "#selectorsWrapper",
    where = "beforeEnd",
    ui = tags$div(id = "block" %+% id, style="position:relative;", if(id > 1L){
      tags$hr()
    },
    tags$div(class="and-wrapper", 
             tags$div(class="and-sign", "&"),
             tags$div(id = "blockContent" %+% id, 
                      class = "grid-container"
             ),
             tags$div(class = "item-add-block",
                      tags$div(class = "item-and", 
                               tags$span(class="and-button", 
                                         lang$nav$hcubeLoad$andButton)),
                      tags$div(class = "item-dropdown", 
                               selectInput("newLine_" %+% id, "", 
                                           choices = fields)
                      ),
                      if(id > 1L){
                        tags$div(class = "item-delete",
                                 actionButton("btRemoveBlock" %+% id, label = "-"))
                      }
             )
    ))
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

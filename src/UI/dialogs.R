showReadonlyDialog <- function() {
  showModal(modalDialog(
    title = lang$nav$dialogReadonly$title,
    lang$nav$dialogReadonly$desc,
    footer = tagList(
      modalButton(lang$nav$dialogReadonly$cancelButton),
      actionButton("btSaveReadonly",
        label = lang$nav$dialogReadonly$okButton,
        class = "bt-highlight-1 bt-gms-confirm"
      )
    ),
    fade = TRUE, easyClose = TRUE
  ))
}

showInconsistentOutputDialog <- function() {
  showModal(modalDialog(
    title = lang$nav$dialogInconsistentOutput$title,
    lang$nav$dialogInconsistentOutput$desc,
    footer = tagList(
      modalButton(lang$nav$dialogInconsistentOutput$cancelButton),
      actionButton("btLoadInconsistentOutput",
        label = lang$nav$dialogInconsistentOutput$okButton,
        class = "bt-highlight-1 bt-gms-confirm"
      )
    ),
    fade = TRUE, easyClose = TRUE
  ))
}

showLoginDialog <- function(cred, forwardOnSuccess = NULL) {
  return(showModal(modalDialog(
    title = lang$errMsg$sessionExpired$title,
    lang$errMsg$sessionExpired$desc
  )))
}

showNewScenDialog <- function(tmpScenName = NULL, forwardTo = "btSaveConfirm",
                              scenTags = character(0L), discardPermDefault = FALSE,
                              allScenTags = character()) {
  showModal(modalDialog(
    title = lang$nav$dialogNewScen$title,
    tags$div(
      id = "scenNameWrapper",
      tags$div(
        class = "input-form-mobile",
        textInput("scenName", lang$nav$dialogNewScen$desc,
          value = tmpScenName,
          width = "100%"
        )
      ),
      tags$div(
        class = "input-form-mobile",
        selectizeInput("newScenTags", lang$nav$dialogNewScen$tags,
          unique(c(scenTags, allScenTags)),
          selected = scenTags,
          multiple = TRUE, options = list(
            "create" = TRUE,
            "persist" = FALSE
          ),
          width = "100%"
        )
      ),
      fluidRow(
        class = "keep-meta-wrapper",
        tags$div(
          class = "col-sm-4 col-xs-6 keep-meta-item",
          checkboxInput_MIRO(
            "newScenDiscardAttach",
            lang$nav$dialogNewScen$discardAttach
          )
        ),
        tags$div(
          class = "col-sm-4 col-xs-6 keep-meta-item",
          checkboxInput_MIRO(
            "newScenDiscardViews",
            lang$nav$dialogNewScen$discardViews
          )
        ),
        tags$div(
          class = "col-sm-4 col-xs-6 keep-meta-item",
          checkboxInput_MIRO(
            "newScenDiscardPerm",
            lang$nav$dialogNewScen$discardPerm,
            isTRUE(discardPermDefault)
          )
        )
      )
    ),
    tags$div(
      id = "badScenarioName", class = "gmsalert gmsalert-error",
      lang$nav$dialogNewScen$badName
    ),
    tags$div(
      id = "scenarioExists", class = "err-msg", style = "display:none;",
      lang$nav$dialogNewScen$scenExists
    ),
    footer = tagList(
      tags$div(
        class = "modal-footer-mobile",
        id = "dialogSaveInit",
        modalButton(lang$nav$dialogNewScen$cancelButton),
        tags$button(
          class = "btn btn-default bt-highlight-1 bt-gms-confirm",
          type = "button", onclick = paste0(
            "Shiny.setInputValue('btCheckName', '",
            forwardTo, "', {priority:'event'})"
          ),
          lang$nav$dialogNewScen$okButton
        )
      ),
      tags$div(
        class = "modal-footer-mobile",
        id = "dialogSaveConfirm", style = "display:none;",
        actionButton("btNewName", lang$nav$dialogNewScen$btNewName),
        actionButton(forwardTo, lang$nav$dialogNewScen$btOverwrite,
          class = "bt-highlight-1 bt-gms-confirm"
        )
      )
    ),
    fade = TRUE, easyClose = FALSE
  ))
}

showRemoveScenDialog <- function(forwardTo) {
  showModal(modalDialog(
    title = lang$nav$dialogRemoveScen$title,
    lang$nav$dialogRemoveScen$desc,
    footer = tagList(
      tags$div(
        class = "modal-footer-mobile",
        modalButton(lang$nav$dialogRemoveScen$cancelButton),
        tags$button(
          class = "btn btn-default bt-highlight-1 bt-gms-confirm",
          type = "button", onclick = paste0(
            "Shiny.setInputValue('", forwardTo, "',1,",
            "{priority:'event'})"
          ),
          lang$nav$dialogRemoveScen$okButton
        )
      )
    ), fade = TRUE, easyClose = TRUE
  ))
}

showDeleteScenDialog <- function() {
  showModal(modalDialog(
    title = lang$nav$dialogDeleteScen$title,
    tags$div(
      id = "deleteScen_db",
      lang$nav$dialogDeleteScen$desc
    ),
    tags$div(
      id = "deleteScen_ui", style = "display:none;",
      lang$nav$dialogDeleteScen$removeFromUI$desc
    ),
    footer = tagList(
      tags$div(
        class = "modal-footer-mobile",
        modalButton(lang$nav$dialogDeleteScen$cancelButton),
        actionButton("btDeleteConfirm", lang$nav$dialogDeleteScen$okButton,
          class = "bt-highlight-1 bt-gms-confirm"
        ),
        actionButton("btRemoveDeletedConfirm",
          label = lang$nav$dialogDeleteScen$removeFromUI$okButton,
          class = "bt-highlight-1 bt-gms-confirm", style = "display:none;"
        )
      )
    ),
    fade = TRUE, easyClose = TRUE
  ))
}
showJobRunningDialog <- function() {
  showModal(modalDialog(
    title = lang$errMsg$jobRunning$title,
    lang$errMsg$jobRunning$descAsync,
    footer = tagList(
      modalButton(lang$errMsg$jobRunning$cancelButton),
      actionButton("btSolveDetachCurrent",
        label = lang$errMsg$jobRunning$detachButton,
        class = "bt-highlight-1 bt-gms-confirm"
      )
    ),
    fade = TRUE, easyClose = TRUE
  ))
}

showRemoveExistingOutputDataDialog <- function() {
  showModal(modalDialog(
    title = lang$nav$dialogExistingOutput$title,
    lang$nav$dialogExistingOutput$desc,
    footer = tagList(
      modalButton(lang$nav$dialogExistingOutput$cancelButton),
      actionButton("btSaveOutput", label = lang$nav$dialogExistingOutput$saveOutputButton),
      actionButton("btRemoveOutput",
        label = lang$nav$dialogExistingOutput$discardOutputButton,
        class = "bt-highlight-1 bt-gms-confirm"
      )
    ),
    fade = TRUE, easyClose = TRUE
  ))
}
getLoadDbPanel <- function(title, scenList, tagList, iconName) {
  content <- tagList(
    tags$div(class = "space"),
    selectInput("selLoadScen", lang$nav$dialogLoadScen$selLoadScen,
      scenList,
      multiple = FALSE, width = "100%"
    ),
    if (length(tagList)) {
      tags$div(
        id = "selLoadScenTagsDiv",
        selectInput("selLoadScenTags", lang$nav$dialogLoadScen$selTags,
          tagList,
          multiple = TRUE, width = "100%"
        )
      )
    },
    tags$div(
      lang$nav$dialogLoadScen$sortBy,
      actionButton("btSortName",
        label = lang$nav$dialogLoadScen$btSortNameASC,
        icon = icon("arrow-down-a-z"),
        class = "scen-sort-by"
      ),
      actionButton("btSortTime",
        label = lang$nav$dialogLoadScen$btSortTimeASC,
        icon = icon("arrow-down-1-9"),
        class = "scen-sort-by scen-sort-by-selected"
      )
    ),
    fluidRow(
      div(
        class = "choose-input",
        column(
          6,
          tags$label(
            class = "checkbox-material flex-design",
            "for" = "cbSelectManuallyDb",
            checkboxInput("cbSelectManuallyDb", "", FALSE),
            lang$nav$dialogImport$cbSelectManually
          )
        ),
        column(
          6,
          conditionalPanel(
            condition = "input.cbSelectManuallyDb === true",
            selectInput("selInputDataDb", lang$nav$dialogImport$selInputData,
              setNames(
                names(modelInToImport),
                modelInToImportAlias
              ),
              multiple = TRUE, width = "100%"
            )
          )
        )
      )
    ),
    tags$div(class = "small-space"),
    tags$div(
      style = "text-align: center;",
      actionButton("btLoadScenConfirm",
        lang$nav$dialogLoadScen$okButton,
        class = "bt-highlight-1 bt-gms-confirm"
      )
    )
  )
  tabPanel(title,
    value = "tb_importData_remote",
    tags$div(
      id = "loadData_content",
      fluidRow(
        column(
          12,
          if (!length(scenList)) {
            lang$nav$dialogLoadScen$descNoScen
          } else {
            content
          }
        )
      )
    ),
    icon = icon(iconName)
  )
}
showLoadDataDialog <- function(scenListDb, dbTagList = NULL, selectLocalTab = FALSE) {
  tabLoadFromDb <- NULL
  tabLoadFromLocalFile <- NULL
  tabLoadFromExternalSource <- NULL
  tabLoadFromBase <- NULL

  if (length(externalInputConfig)) {
    tabLoadFromExternalSource <- tabPanel(lang$nav$dialogImport$tabExternal,
      value = "tb_importData_external",
      tags$div(class = "space"),
      fluidRow(
        column(
          12,
          selectInput("selExternalSource", lang$nav$dialogImport$selExternalSource,
            names(externalInputConfig),
            multiple = FALSE, width = "100%"
          ),
          lapply(seq_along(externalInputConfig), function(configId) {
            if (!length(externalInputConfig[[configId]]$localFileInput)) {
              return()
            }
            return(
              conditionalPanel(
                condition = paste0(
                  "input.selExternalSource===",
                  toJSON(names(externalInputConfig)[[configId]], auto_unbox = TRUE)
                ),
                fileInput(paste0("externalSourceFile_", configId),
                  if (length(externalInputConfig[[configId]]$localFileInput$label) &&
                    !identical(externalInputConfig[[configId]]$localFileInput$label, "")) {
                    externalInputConfig[[configId]]$localFileInput$label
                  } else {
                    NULL
                  },
                  multiple = identical(externalInputConfig[[configId]]$localFileInput$multiple, TRUE),
                  accept = externalInputConfig[[configId]]$localFileInput$accept,
                  width = "100%"
                )
              )
            )
          })
        )
      ),
      fluidRow(
        div(
          class = "choose-input",
          column(
            6,
            tags$label(
              class = "checkbox-material flex-design",
              "for" = "cbSelectManuallyExt",
              checkboxInput("cbSelectManuallyExt", "", FALSE),
              lang$nav$dialogImport$cbSelectManually
            )
          ),
          column(
            6,
            conditionalPanel(
              condition = "input.cbSelectManuallyExt === true",
              selectInput("selInputDataExt", lang$nav$dialogImport$selInputData,
                setNames(
                  names(modelInToImport),
                  modelInToImportAlias
                ),
                multiple = TRUE, width = "100%"
              )
            )
          )
        )
      ),
      fluidRow(
        tags$div(
          style = "text-align: center;",
          actionButton("btImportExternal",
            class = "bt-highlight-1 bt-gms-confirm",
            lang$nav$dialogImport$okButton
          )
        )
      ), icon = icon("up-right-from-square")
    )
  }
  if (config$activateModules$loadLocal) {
    tabLoadFromLocalFile <- tabPanel(lang$nav$dialogImport$tabLocal,
      value = "tb_importData_local",
      tags$div(class = "space"),
      tags$div(
        id = "loadLocal_content",
        fluidRow(
          column(
            12,
            fileInput("localInput", lang$nav$dialogImport$descLocal,
              width = "100%",
              multiple = FALSE,
              accept = c(
                "application/vnd.ms-excel",
                "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                "application/zip", "text/csv", ".xlsx", ".xlsm", ".csv", ".xls", ".zip",
                ".gdx", ".miroscen"
              )
            ),
            tags$div(
              class = "gmsalert gmsalert-error", id = "localDataImportError",
              style = "position:relative;white-space:pre-line;", lang$errMsg$unknownError
            )
          )
        ),
        fluidRow(
          div(
            id = "localInputExcelOptions", style = "display: none",
            column(
              6,
              selectInput("selExcelIndexSheet", lang$nav$dialogImport$selExcelIndexSheet,
                "-",
                multiple = FALSE, width = "100%"
              )
            ),
            column(
              6,
              conditionalPanel(
                condition = "input.selExcelIndexSheet !== '-'",
                textInput(
                  "excelIndexSheetRng", lang$nav$dialogImport$excelIndexSheetRng,
                  "A1"
                )
              )
            ),
          )
        ),
        fluidRow(
          div(
            id = "localInputSelectManually", class = "choose-input",
            column(
              6,
              tags$label(
                class = "checkbox-material flex-design",
                "for" = "cbSelectManuallyLoc",
                checkboxInput("cbSelectManuallyLoc", "", FALSE),
                lang$nav$dialogImport$cbSelectManually
              )
            ),
            column(
              6,
              conditionalPanel(
                condition = "input.cbSelectManuallyLoc === true",
                selectInput("selInputDataLoc", lang$nav$dialogImport$selInputData,
                  if (length(modelInToImport)) {
                    setNames(
                      names(modelInToImport),
                      modelInToImportAlias
                    )
                  } else {
                    NULL
                  },
                  multiple = TRUE, width = "100%"
                )
              )
            )
          )
        ),
        fluidRow(
          div(
            id = "localInputCsvOptions", style = "display: none",
            tags$div(
              class = "col-sm-6", id = "csvDelimWrapper", style = "display:none;",
              selectInput("csvDelim", lang$nav$dialogImport$selDelim,
                NULL,
                multiple = FALSE, width = "100%"
              )
            ),
            column(
              6,
              selectInput("csvDecimalSep", lang$nav$dialogImport$selDecimalSep,
                c(".", ","),
                multiple = FALSE, width = "100%"
              )
            ),
            column(
              6,
              selectInput("selInputDataLocCSV", lang$nav$dialogImport$selInputData,
                if (length(ioConfig$modelInRaw)) {
                  c(setNames(
                    names(ioConfig$modelInRaw),
                    vapply(ioConfig$modelInRaw, function(inSym) {
                      return(inSym$alias)
                    }, character(1L), USE.NAMES = FALSE)
                  ), if (length(ioConfig$hcubeScalars)) {
                    setNames(ioConfig$hcubeScalars, vapply(ioConfig$hcubeScalars, function(hcubeScalar) {
                      return(ioConfig$modelIn[[hcubeScalar]]$alias)
                    }, character(1L), USE.NAMES = FALSE))
                  })
                } else {
                  NULL
                },
                multiple = FALSE, width = "100%"
              )
            ),
            uiOutput("csvHeaderMapping")
          )
        ),
        fluidRow(
          tags$div(
            style = "text-align: center;",
            actionButton("btImportLocal",
              class = "bt-highlight-1 bt-gms-confirm",
              disabled = TRUE, lang$nav$dialogImport$okButton
            )
          )
        )
      ),
      icon = icon("file")
    )
  }

  tabLoadFromDb <- getLoadDbPanel(
    title = lang$nav$dialogImport$tabDatabase,
    scenList = scenListDb, tagList = dbTagList,
    iconName = "database"
  )

  loadDataTabs <- list(
    tabLoadFromDb, tabLoadFromLocalFile, tabLoadFromExternalSource
  )
  loadDataTabs <- loadDataTabs[!vapply(loadDataTabs, is.null, logical(1L), USE.NAMES = FALSE)]

  showModal(modalDialog(
    title = lang$nav$dialogImport$title,
    fluidRow(
      tags$div(
        id = "importScenMaxNoScen", class = "gmsalert gmsalert-error",
        lang$nav$dialogLoadScen$maxNoScenExceeded
      ),
      tags$div(
        id = "importScenNoHcubeScen", class = "gmsalert gmsalert-error",
        lang$nav$dialogImport$hcubeHashNoMatch
      ),
      tags$div(
        id = "importScenSnameExistsErr", class = "gmsalert gmsalert-error",
        lang$nav$dialogImport$scenNameExistsErr
      ),
      tags$div(
        id = "importScenNoDsSelected", class = "gmsalert gmsalert-error",
        lang$nav$dialogLoadScen$noDsSelected
      ),
      tags$div(
        id = "symNotInDataSrc", class = "gmsalert gmsalert-error",
        lang$nav$dialogLoadScen$symNotInDataSrc
      ),
      tags$div(
        id = "importScenInvalidFile", class = "gmsalert gmsalert-error",
        lang$nav$dialogLoadScen$invalidFile
      ),
      tags$div(
        id = "importScenError", class = "gmsalert gmsalert-error",
        lang$errMsg$unknownError
      ),
      tags$div(
        id = "importDataTabset",
        do.call(tabBox, c(
          list(
            width = 12, id = "tb_importData",
            selected = if (selectLocalTab) "tb_importData_local"
          ),
          loadDataTabs
        ))
      ),
      tags$div(
        id = "importDataClearSandbox", class = "col-sm-12", style = "display:none;",
        lang$nav$dialogImport$descClearSandbox
      ),
      tags$div(
        id = "importDataOverwrite", class = "col-sm-12", style = "display:none;",
        lang$nav$dialogImport$descOverwriteInput
      )
    ), footer = {
      tagList(
        modalButton(lang$nav$dialogImport$cancelButton),
        actionButton("btReplaceInputData",
          label = lang$nav$dialogImport$replaceButton,
          style = "display:none;"
        ),
        actionButton("btMergeInputData",
          label = lang$nav$dialogImport$mergeButton,
          style = "display:none;"
        ),
        actionButton("btOverwriteScen",
          label = lang$nav$dialogImport$okButton,
          class = "bt-highlight-1 bt-gms-confirm", style = "display:none;"
        ),
        actionButton("btOverwriteScenLocal",
          label = lang$nav$dialogImport$okButton,
          class = "bt-highlight-1 bt-gms-confirm", style = "display:none;"
        )
      )
    }
  ))
}
showLoadScenDialog <- function(dbScenList, uiScenList, isInSplitView, noDBPanel = FALSE,
                               dbTagList = NULL, baseScenName = NULL) {
  tabPanelUI <- NULL
  tabPanelDB <- NULL
  if (isInSplitView && length(uiScenList)) {
    tabPanelUI <- tabPanel(lang$nav$dialogLoadScen$tabUI,
      icon = icon("file"),
      value = "loadScenUI",
      tags$div(class = "space"),
      selectInput("selLoadScenUI", lang$nav$dialogLoadScen$selLoadScen,
        uiScenList, uiScenList[1L],
        multiple = FALSE, width = "100%"
      )
    )
  }
  if (!noDBPanel && length(dbScenList)) {
    tabPanelDB <- tabPanel(lang$nav$dialogLoadScen$tabDB,
      icon = icon("database"),
      value = "loadScenDb",
      tags$div(class = "space"),
      if (length(baseScenName)) {
        tagList(
          tags$div(
            tags$label(
              class = "label-base-scen",
              lang$nav$dialogLoadScen$baseScenLabel
            ),
            tags$span(class = "label label-default base-scen", baseScenName)
          ),
          tags$div(class = "small-space")
        )
      },
      selectInput("selLoadScen", lang$nav$dialogLoadScen$selLoadScen,
        dbScenList,
        selected = if (isInSplitView) NULL else unname(uiScenList),
        multiple = if (isInSplitView) FALSE else TRUE, width = "100%"
      ),
      if (length(dbTagList)) {
        selectInput("selLoadScenTags", lang$nav$dialogLoadScen$selTags,
          dbTagList,
          multiple = TRUE, width = "100%"
        )
      },
      tags$div(class = "space"),
      tags$div(
        lang$nav$dialogLoadScen$sortBy,
        actionButton("btSortName",
          label = lang$nav$dialogLoadScen$btSortNameASC,
          icon = icon("arrow-down-a-z"),
          class = "scen-sort-by"
        ),
        actionButton("btSortTime",
          label = lang$nav$dialogLoadScen$btSortTimeASC,
          icon = icon("arrow-down-1-9"),
          class = "scen-sort-by scen-sort-by-selected"
        )
      )
    )
  }
  showModal(modalDialog(
    title = lang$nav$dialogLoadScen$title,
    tags$div(
      id = "importScenMaxNoScen", class = "gmsalert gmsalert-error",
      lang$nav$dialogLoadScen$maxNoScenExceeded
    ),
    if (is.null(tabPanelUI)) {
      tabsetPanel(
        id = "tabsetLoadScen",
        tabPanelDB
      )
    } else if (is.null(tabPanelDB)) {
      tabsetPanel(
        id = "tabsetLoadScen",
        tabPanelUI
      )
    } else {
      tabsetPanel(
        id = "tabsetLoadScen",
        tabPanelDB, tabPanelUI
      )
    },
    footer = tagList(
      tags$div(
        class = "modal-footer-mobile",
        modalButton(lang$nav$dialogLoadScen$cancelButton),
        actionButton("btLoadScenConfirm", lang$nav$dialogLoadScen$okButton,
          class = "bt-highlight-1 bt-gms-confirm"
        )
      )
    ),
    fade = TRUE, easyClose = TRUE
  ))
}
showEditMetaDialog <- function(metadata,
                               ugroups = character(0L),
                               allowAttachments = FALSE,
                               attachmentMetadata = character(0L),
                               viewsMetadata = character(0L),
                               attachAllowExec = FALSE,
                               isLocked = FALSE,
                               selectedTab = NULL,
                               allScenTags = character(0L)) {
  supportedTabs <- "general"
  scenTags <- csv2Vector(metadata[["_stag"]][[1]])

  contentAccessPerm <- NULL

  content <- tagList(
    tags$div(
      class = "gmsalert gmsalert-error", id = "editMetaBadName",
      lang$nav$dialogEditMeta$badName
    ),
    tags$div(
      class = "gmsalert gmsalert-error", id = "editMetaNameExists",
      lang$nav$dialogEditMeta$scenExists
    ),
    tags$div(class = "space"),
    tags$div(
      class = "input-form-mobile",
      textInput("editMetaName", lang$nav$dialogEditMeta$newName,
        value = metadata[["_sname"]][[1]],
        width = "100%"
      )
    ),
    tags$div(
      class = "input-form-mobile",
      selectizeInput("editMetaTags", lang$nav$dialogEditMeta$newTags,
        unique(c(scenTags, allScenTags)),
        selected = scenTags,
        multiple = TRUE, options = list(
          "create" = TRUE,
          "persist" = FALSE
        ),
        width = "100%"
      )
    )
  )
  writePerm <- csv2Vector(metadata[["_accessw"]][[1]])
  if (!isLocked && length(ugroups) && any(ugroups %in% writePerm)) {
    supportedTabs <- c("general", "accessPerm")
    contentAccessPerm <- tabPanel(lang$nav$dialogEditMeta$categoryAccessPerm,
      value = "accessPerm",
      tags$div(
        class = "gmsalert gmsalert-error", id = "editMetaEmptyPerm",
        lang$nav$dialogEditMeta$emptyPerm
      ),
      tags$div(
        class = "gmsalert gmsalert-error", id = "editMetaIncapOwner",
        lang$nav$dialogEditMeta$incapOwner
      ),
      tags$div(
        class = "gmsalert gmsalert-error", id = "editAccessRightsError",
        lang$nav$dialogEditMeta$errMsg
      ),
      tags$div(class = "space"),
      tags$div(
        id = "contentAccessPerm",
        genSpinner(
          id = "contentAccessPermSpinner",
          absolute = FALSE,
          extraClasses = "gen-spinner-black"
        )
      )
    )
  }
  if (allowAttachments) {
    supportedTabs <- c(supportedTabs, "attachments", "views")
    contentAttachments <- tabPanel(
      lang$nav$dialogEditMeta$categoryAttachments,
      value = "attachments",
      tags$div(
        class = "gmsalert gmsalert-success", id = "attachSuccess",
        lang$nav$dialogEditMeta$attachSuccess
      ),
      tags$div(
        class = "gmsalert gmsalert-error", id = "attachMaxNoError",
        lang$nav$dialogEditMeta$attachMaxNoError
      ),
      tags$div(
        class = "gmsalert gmsalert-error", id = "attachMaxSizeError",
        lang$nav$dialogEditMeta$attachMaxSizeError
      ),
      tags$div(
        class = "gmsalert gmsalert-error", id = "attachDuplicateError",
        lang$nav$dialogEditMeta$attachDuplicateError
      ),
      tags$div(
        class = "gmsalert gmsalert-error", id = "attachForbiddenFnameError",
        lang$nav$dialogEditMeta$attachForbiddenFnameError
      ),
      tags$div(
        class = "gmsalert gmsalert-error", id = "attachRO",
        lang$errMsg$permErr
      ),
      tags$div(
        class = "gmsalert gmsalert-error", id = "attachUnknownError",
        lang$errMsg$unknownError
      ),
      tags$div(class = "space"),
      downloadLink("downloadAttachmentData", "", style = "visibility:hidden;"),
      tags$div(
        class = "input-form-mobile",
        fileInput("file_addAttachments", lang$nav$dialogEditMeta$attachmentsAdd,
          multiple = TRUE, width = "100%"
        )
      ),
      if (length(attachmentMetadata[["name"]])) {
        lapply(seq_along(attachmentMetadata[["name"]]), function(i) {
          tags$div(
            class = "row attachment-line",
            column(
              width = 6,
              HTML(paste0(
                '<button class="btn btn-default bt-icon" id="btRemoveAttachment_', i,
                '" type="button" onclick="Miro.removeAttachment(', i, ')"><i class="fa fa-circle-xmark" role="presentation" aria-label="Remove attachment"></i></button>
                                                                   <a href="#" onclick="Miro.downloadAttachment(', i, ')">',
                htmltools::htmlEscape(attachmentMetadata[["name"]][[i]]), "</a>"
              ))
            ),
            if (attachAllowExec) {
              column(
                width = 6,
                HTML(paste0(
                  '<div class="form-group shiny-input-container"><div class="checkbox"><label><input type="checkbox" onchange="Shiny.setInputValue(\'execPermAttachment_',
                  i, '\',$(this).is(\':checked\'),{priority:\'event\'});"', if (attachmentMetadata[["execPerm"]][[i]]) 'checked="checked"', "><span>",
                  lang$nav$dialogEditMeta$attachmentsExecPerm, "</span></label></div></div>"
                ))
              )
            }
          )
        })
      },
      tags$div(id = "endAttachList", class = "small-space"),
      genSpinner(
        id = "addAttachLoading", hidden = TRUE, absolute = FALSE,
        extraClasses = "gen-spinner-black"
      )
    )
    contentViews <- tabPanel(
      lang$nav$dialogEditMeta$categoryViews,
      value = "views",
      tags$div(
        class = "gmsalert gmsalert-success", id = "viewsSuccess",
        lang$nav$dialogEditMeta$viewsSuccess
      ),
      tags$div(
        class = "gmsalert gmsalert-error", id = "viewsInvalidDataError",
        lang$nav$dialogEditMeta$viewsInvalidData
      ),
      tags$div(class = "gmsalert gmsalert-error", id = "viewsCustomError"),
      tags$div(
        class = "gmsalert gmsalert-error", id = "viewsNoneSelected",
        lang$nav$dialogEditMeta$viewsNoneSelected
      ),
      tags$div(
        class = "gmsalert gmsalert-error", id = "viewsUnknownError",
        lang$errMsg$unknownError
      ),
      tags$div(class = "space"),
      tags$div(
        class = "input-form-mobile",
        fileInput("file_addViews", lang$nav$dialogEditMeta$viewsAdd,
          multiple = TRUE, width = "100%"
        )
      ),
      tags$div(
        id = "currentViewsTable-noData",
        style = paste0(
          "padding:20px;text-align:center;",
          if (length(viewsMetadata[["id"]])) "display:none;"
        ),
        lang$nav$dialogEditMeta$noViewData
      ),
      tags$div(
        id = "currentViewsTable-wrapper",
        style = if (!length(viewsMetadata[["id"]])) "display:none;",
        tags$a(
          onClick = "Miro.selectAllRows('currentViewsTable')",
          href = "#", lang$nav$dialogEditMeta$selectAllViews
        ),
        tags$a(
          onClick = "Miro.selectNoRow('currentViewsTable')",
          href = "#", lang$nav$dialogEditMeta$selectNoViews,
          style = "margin-left:30px;"
        ),
        tags$div(
          style = "max-height:300px;overflow-y:auto;",
          tags$table(
            id = "currentViewsTable",
            class = "miro-table",
            tags$thead(tags$tr(
              tags$th(lang$nav$dialogEditMeta$viewsTableHeaderSymbol),
              tags$th(lang$nav$dialogEditMeta$viewsTableHeaderId)
            )),
            tags$tbody(do.call("tagList", lapply(seq_along(viewsMetadata[["id"]]), function(i) {
              symbolName <- viewsMetadata[["symName"]][i]
              tags$tr(
                onClick = "$(this).toggleClass('selected')",
                tags$td(
                  if (identical(symbolName, viewsMetadata[["symName"]][i - 1L])) {
                    ""
                  } else {
                    viewsMetadata[["symAlias"]][i]
                  },
                  `data-val` = base64_enc(symbolName)
                ),
                tags$td(viewsMetadata[["id"]][i])
              )
            })))
          )
        )
      ),
      tags$div(class = "small-space"),
      downloadLink("downloadViews", "", style = "visibility:hidden;"),
      tags$div(
        class = "input-form-mobile buttons-mobile-wrapper",
        tags$button(
          class = "btn btn-default buttons-mobile-left",
          type = "button",
          onClick = "Miro.sendSelectedRowsRequest('currentViewsTable','downloadViews','viewsNoneSelected',true)",
          icon("download"),
          lang$nav$dialogEditMeta$viewsDownload
        ),
        tags$button(
          class = "btn btn-default bt-remove buttons-mobile-right",
          type = "button",
          onClick = "Miro.sendSelectedRowsRequest('currentViewsTable','removeViews','viewsNoneSelected')",
          icon("trash"),
          lang$nav$dialogEditMeta$viewsRemove
        )
      ),
      genSpinner(
        id = "addViewsLoading", hidden = TRUE, absolute = FALSE,
        extraClasses = "gen-spinner-black"
      )
    )
    contentList <- list(
      id = "tpEditMeta",
      tabPanel(
        lang$nav$dialogEditMeta$categoryGeneral,
        value = "general",
        content
      ), contentAttachments, contentViews
    )
    if (length(contentAccessPerm)) {
      contentList[[5L]] <- contentAccessPerm
    }
    if (length(selectedTab) && selectedTab %in% supportedTabs) {
      contentList$selected <- selectedTab
    }
    content <- do.call(tabsetPanel, contentList)
  } else if (length(contentAccessPerm)) {
    content <- tabsetPanel(
      id = "tpEditMeta",
      tabPanel(
        lang$nav$dialogEditMeta$categoryGeneral,
        value = "general",
        content
      ),
      contentAccessPerm,
      selected = if (length(selectedTab) && selectedTab %in% supportedTabs) selectedTab
    )
  }

  showModal(modalDialog(
    title = lang$nav$dialogEditMeta$title,
    tags$div(
      tags$div(
        class = "gmsalert gmsalert-error", id = "editMetaError",
        lang$nav$dialogEditMeta$errMsg
      ),
      tags$div(
        id = "editMetaUI",
        content
      )
    ),
    footer = tagList(
      tags$div(
        class = "modal-footer-mobile",
        modalButton(lang$nav$dialogEditMeta$cancelButton),
        actionButton("btUpdateMeta", lang$nav$dialogEditMeta$okButton,
          class = "bt-highlight-1 bt-gms-confirm"
        )
      )
    ),
    fade = TRUE, easyClose = TRUE
  ))
}
showScenExportDialog <- function(id, exportTypes) {
  showModal(modalDialog(
    title = lang$nav$dialogExportScen$title,
    tags$div(
      class = "input-form-mobile",
      selectInput("exportFileType", lang$nav$dialogExportScen$desc, exportTypes,
        width = "100%"
      )
    ),
    tags$div(
      `data-display-if` = "input.exportFileType !== 'miroscen' && !input.exportFileType?.startsWith('custom_')",
      class = "gmsalert gmsalert-error", style = "position:relative;",
      lang$nav$fileExport$infoDataOnly
    ),
    tags$div(
      class = "gmsalert gmsalert-error", id = "scenExportError",
      style = "position:relative;white-space:pre-line;", lang$errMsg$unknownError
    ),
    tags$div(
      style = "display:none;",
      numericInput("scenExportId", NULL, id)
    ),
    div(
      class = "choose-input", `data-display-if` = "input.exportFileType !== 'miroscen'",
      column(
        6,
        tags$label(
          class = "checkbox-material flex-design",
          "for" = "cbSelectManuallyExp",
          checkboxInput("cbSelectManuallyExp", "", FALSE),
          lang$nav$dialogExportScen$cbSelectManually
        )
      ),
      column(
        6,
        conditionalPanel(
          condition = "input.cbSelectManuallyExp === true",
          selectInput("selDataToExport", lang$nav$dialogExportScen$selDatasets,
            setNames(
              c(inputDsNames, names(modelOut)),
              c(inputDsAliases, modelOutAlias)
            ),
            multiple = TRUE, width = "100%"
          )
        )
      )
    ),
    footer = tagList(
      tags$div(
        class = "modal-footer-mobile",
        modalButton(lang$nav$dialogExportScen$cancelButton),
        downloadButton("scenExportHandler", lang$nav$dialogExportScen$okButton,
          class = "bt-highlight-1 bt-gms-confirm file-export"
        ),
        actionButton("scenRemoteExportHandler", lang$nav$dialogExportScen$okButton,
          class = "bt-highlight-1 bt-gms-confirm remote-export",
          style = "display: none;"
        )
      )
    ), fade = TRUE, easyClose = TRUE
  ))
}
showHashExistsDialog <- function(scenData, uid) {
  if (length(scenData[["_stag"]]) && !is.na(scenData[["_stag"]]) && nchar(scenData[["_stag"]])) {
    tagsString <- sprintf(
      lang$nav$dialogHashExists$descTags,
      trimws(scenData[["_stag"]],
        which = "both", ","
      )
    )
  } else {
    tagsString <- ""
  }
  showModal(modalDialog(
    title = lang$nav$dialogHashExists$title,
    sprintf(
      lang$nav$dialogHashExists$desc, scenData[["_sname"]],
      if (nchar(tagsString)) {
        tagsString
      } else {
        ""
      },
      scenData[["_uid"]], scenData[["_stime"]]
    ),
    footer = tagList(
      modalButton(lang$nav$dialogHashExists$cancelButton),
      actionButton("btRunNoCheckHash", lang$nav$dialogHashExists$okButton,
        class = "bt-highlight-1 bt-gms-confirm"
      )
    ),
    fade = TRUE, easyClose = TRUE
  ))
}
showJobSubmissionDialog <- function(jobName = "", hashExistsData = NULL, instanceInfo = NULL) {
  if (length(hashExistsData) && nrow(hashExistsData)) {
    if (length(hashExistsData[["_stag"]]) && !is.na(hashExistsData[["_stag"]]) &&
      nchar(hashExistsData[["_stag"]])) {
      tagsString <- sprintf(
        lang$nav$dialogHashExists$descTags,
        trimws(hashExistsData[["_stag"]],
          which = "both", ","
        )
      )
    } else {
      tagsString <- ""
    }
    sameHashInfoBox <- tagList(
      tags$div(class = "space"),
      tags$div(
        class = "gmsalert gmsalert-success", style = "display:block;position:relative;",
        sprintf(
          lang$nav$dialogHashExists$desc, hashExistsData[["_sname"]],
          if (nchar(tagsString)) {
            sprintf(lang$nav$dialogHashExists$descTags, tagsString)
          } else {
            ""
          },
          hashExistsData[["_uid"]], hashExistsData[["_stime"]]
        )
      )
    )
  } else {
    sameHashInfoBox <- NULL
  }
  showModal(modalDialog(
    tags$div(
      class = "gmsalert gmsalert-success", style = "position:relative;",
      id = "jobSubmitSuccess",
      lang$nav$dialogJobSubmission$successMsg
    ),
    tags$div(
      class = "gmsalert gmsalert-error", id = "jobSubmitBadName",
      lang$nav$dialogJobSubmission$badName
    ),
    tags$div(
      class = "gmsalert gmsalert-error", id = "jobSubmitUnknownHost",
      style = "position:relative;",
      lang$nav$dialogRemoteLogin$hostNotFound
    ),
    tags$div(
      class = "gmsalert gmsalert-error", id = "jobSubmitUnauthorized",
      style = "position:relative;",
      lang$nav$dialogRemoteLogin$invalidCred
    ),
    tags$div(
      class = "gmsalert gmsalert-error", id = "jobSubmitUnknownError",
      style = "position:relative;",
      lang$errMsg$unknownError
    ),
    tags$div(
      id = "jobSubmissionLoad", style = "display:none;text-align:center;",
      lang$nav$dialogJobSubmission$descWait,
      tags$div(class = "space"),
      genSpinner(hidden = FALSE, absolute = FALSE, extraClasses = "gen-spinner-black")
    ),
    tags$div(
      id = "jobSubmissionWrapper",
      lang$nav$dialogJobSubmission$desc,
      sameHashInfoBox,
      tags$div(class = "space"),
      tags$div(
        class = "input-form-mobile",
        textInput("jobSubmissionName", lang$nav$dialogJobSubmission$jobName, jobName,
          width = "100%"
        )
      ),
      if (!length(instanceInfo)) {
        tags$div(
          id = "selWorkerInstanceSpinner",
          style = "text-align:center;",
          tags$div(class = "space"),
          genSpinner(hidden = FALSE, absolute = FALSE, extraClasses = "gen-spinner-black")
        )
      },
      tags$div(
        id = "selWorkerInstanceWrapper",
        class = "input-form-mobile",
        style = if (!length(instanceInfo) || !identical(instanceInfo[["instancesSupported"]], TRUE)) "display:none;",
        selectInput("selWorkerInstance",
          lang$nav$dialogJobSubmission$workerInstance,
          choices = instanceInfo$choices, selected = instanceInfo$selected,
          width = "100%"
        )
      ),
      tags$div(class = "small-space")
    ),
    title = lang$nav$dialogJobSubmission$title,
    footer = tagList(
      tags$div(
        class = "modal-footer-mobile",
        modalButton(lang$nav$dialogJobSubmission$cancelButton),
        actionButton("btSubmitAsyncJob", lang$nav$dialogJobSubmission$okButton,
          class = "bt-highlight-1 bt-gms-confirm"
        )
      )
    ),
    fade = TRUE, easyClose = FALSE
  ))
}
# Batch Load module
showBatchLoadDialog <- function(noScenSelected, customScripts = NULL, colNamesForNaming = NULL) {
  if (length(customScripts)) {
    analysisTabset <- selectInput("selHcubeAnalysisScript", lang$nav$dialogBatchLoad$selAnalysisScript,
      setNames(
        seq_along(customScripts),
        vapply(customScripts, "[[", character(1L), "title", USE.NAMES = FALSE)
      ),
      multiple = FALSE, width = "100%"
    )
  } else {
    analysisTabset <- NULL
  }
  createCompareModesDropdown <- function(noScenariosToCompare) {
    if (noScenariosToCompare > 50L) {
      return(NULL)
    }
    compareModesList <- list(
      pivot = lang$nav$dialogBatchLoad$interactiveButtonPivot,
      tab = lang$nav$dialogBatchLoad$interactiveButtonTab,
      split = lang$nav$dialogBatchLoad$interactiveButtonSplit
    )
    compareModesList <- c(compareModesList, setNames(
      lapply(config[["customCompareModules"]], "[[", "label"),
      lapply(config[["customCompareModules"]], "[[", "id")
    ))
    if (noScenariosToCompare > 2L) {
      compareModesList[["split"]] <- NULL
      if (noScenariosToCompare > 10L) {
        compareModesList[["tab"]] <- NULL
      }
    }
    if (length(config$defCompMode) && config$defCompMode %in% names(compareModesList)) {
      defCompModeButton <- list(id = config$defCompMode, label = compareModesList[[config$defCompMode]])
      compareModesList[[config$defCompMode]] <- NULL
    } else {
      defCompModeButton <- list(id = "pivot", label = compareModesList[["pivot"]])
      compareModesList[["pivot"]] <- NULL
    }
    defCompModeButton <- tags$button(
      class = "btn btn-default", type = "button", id = "btBatchCompare",
      style = if (length(compareModesList) > 0L) {
        "margin:6px 0px 6px 5px;border-right:0px;"
      } else {
        "margin:6px 0px 6px 5px;"
      },
      onclick = paste0("Shiny.setInputValue('btBatchCompare','", defCompModeButton$id, "',{priority:'event'});"),
      defCompModeButton$label
    )
    return(tags$div(
      class = "btn-group", class = "batch-load-content",
      defCompModeButton,
      if (length(compareModeList) > 0L) {
        tagList(
          tags$button(
            class = "btn btn-default dropdown-toggle", `data-toggle` = "dropdown",
            style = "margin:6px 0px 6px 0;display:block;",
            tags$span(class = "caret"),
            tags$span(class = "sr-only", "toggle dropdown")
          ),
          tags$ul(
            class = "dropdown-menu", role = "menu", style = "margin-left:5px;max-width:280px;overflow:hidden;",
            lapply(names(compareModesList), function(compareModeId) {
              tags$li(
                tags$a(
                  href = "#",
                  onclick = paste0("Shiny.setInputValue('btBatchCompare','", compareModeId, "',{priority:'event'});"),
                  compareModesList[[compareModeId]]
                )
              )
            })
          )
        )
      }
    ))
  }
  showModal(
    tags$div(
      id = "shiny-modal", class = "modal fade", tabindex = "-1",
      `data-backdrop` = "static", `data-keyboard` = "false",
      tags$div(
        id = "batchLoadModal", class = "modal-dialog",
        tags$div(
          class = "modal-content",
          tags$div(
            class = "modal-header",
            tags$h4(class = "modal-title", lang$nav$dialogBatchLoad$title)
          ),
          tags$div(
            class = "modal-body",
            tags$div(
              class = "gmsalert gmsalert-error", id = "analysisRunScriptRunning",
              lang$nav$dialogBatchLoad$analysisRunScriptRunning
            ),
            tags$div(
              class = "gmsalert gmsalert-error", id = "analysisRunUnknownError",
              lang$errMsg$unknownError
            ),
            tagList(
              tags$div(
                class = "gmsalert gmsalert-success", id = "batchRemoveSuccess",
                lang$nav$dialogBatchLoad$removeSuccess
              ),
              tags$div(
                class = "gmsalert gmsalert-error", id = "batchLoadCustomError",
                lang$errMsg$unknownError
              ),
              tags$div(
                class = "gmsalert gmsalert-error", id = "batchRemoveError",
                lang$errMsg$unknownError
              ),
              tags$div(
                class = "batch-load-remove-content", style = "display:none;",
                sprintf(lang$nav$dialogBatchLoad$removeConfirm, noScenSelected)
              )
            ),
            tags$div(
              class = "batch-load-content",
              if (length(sidsToLoad) <= 50L) {
                lang$nav$dialogBatchLoad$selectMethod
              } else {
                sprintf(lang$nav$dialogBatchLoad$maxScenWarning1, 50L) %+%
                  lang$nav$dialogBatchLoad$maxScenWarning2
              },
              if (length(sidsToLoad) <= 50L) {
                tagList(
                  tags$label(lang$nav$dialogBatchLoad$scenNameCols,
                    `for` = "batchCompareNameCols",
                    style = "font-weight:unset;margin-top:10px;"
                  ),
                  selectInput("batchCompareNameCols", NULL,
                    choices = colNamesForNaming,
                    multiple = TRUE
                  ),
                  conditionalPanel(
                    "input.batchCompareNameCols?.length > 0",
                    textInput("prefixBatchCompareNameCols", label = lang$nav$dialogBatchLoad$scenNamePrefix)
                  )
                )
              }
            ),
            tags$div(
              class = "batch-load-sb-content", style = "display:none;",
              lang$nav$dialogImport$descClearSandbox
            ),
            if (length(analysisTabset)) {
              tagList(
                tags$div(
                  style = "display:none;",
                  class = "batch-load-content batch-load-analysis-content",
                  analysisTabset
                ),
                tags$div(
                  id = "batchLoadAnalysisWrapper",
                  style = "display:none;", class = "batch-load-script-content",
                  genSpinner(
                    id = "batchLoadAnalysisSpinner", absolute = FALSE,
                    extraClasses = "gen-spinner-black"
                  )
                )
              )
            },
            tags$div(
              tags$div(
                class = "btn-group", class = "batch-load-content",
                tags$a(
                  class = "btn btn-default shiny-download-link", type = "button",
                  id = "btBatchDownloadGDX", target = "_blank", href = "", download = NA,
                  style = "margin:6px 0px 6px 0;border-right:0px;",
                  paste0(lang$nav$dialogBatchLoad$downloadButton, " (GDX)")
                ),
                tags$button(
                  class = "btn btn-default dropdown-toggle", `data-toggle` = "dropdown",
                  style = "margin:6px 0px 6px 0;display:block;",
                  tags$span(class = "caret"),
                  tags$span(class = "sr-only", "toggle dropdown")
                ),
                tags$ul(
                  class = "dropdown-menu", role = "menu", style = "margin-left: 5px;",
                  tags$li(
                    tags$a(
                      class = "shiny-download-link", type = "button",
                      id = "btBatchDownloadCSV", target = "_blank", href = "", download = NA,
                      paste0(lang$nav$dialogBatchLoad$downloadButton, " (CSV)")
                    )
                  )
                )
              ),
              if (length(analysisTabset)) {
                tagList(
                  actionButton("btAnalysisConfig",
                    class = "batch-load-content",
                    lang$nav$dialogBatchLoad$analysisButton
                  ),
                  tags$div(
                    style = "display:none;",
                    class = "batch-load-content batch-load-analysis-footer",
                    actionButton("btRunHcubeScript", lang$nav$dialogBatchLoad$runScriptButton,
                      class = "bt-highlight-1 bt-gms-confirm"
                    )
                  ),
                  downloadButton("btDownloadBatchLoadScript",
                    lang$nav$dialogBatchLoad$downloadAnalysisButton,
                    style = "display:none"
                  )
                )
              },
              if (identical(length(sidsToLoad), 1L)) {
                tagList(
                  actionButton("btBatchLoadSb",
                    class = "batch-load-content",
                    style = "margin-left:5px",
                    lang$nav$dialogBatchLoad$interactiveButtonSb
                  ),
                  actionButton("btBatchLoadSbOverwrite",
                    class = "bt-highlight-1 bt-gms-confirm batch-load-sb-content",
                    style = "display:none",
                    lang$nav$dialogBatchLoad$interactiveButtonSb
                  )
                )
              },
              createCompareModesDropdown(length(sidsToLoad))
            )
          ),
          tags$div(
            class = "modal-footer",
            actionButton("btBatchLoadCancel", lang$nav$dialogImport$cancelButton),
            actionButton("btBatchRemove",
              lang$nav$dialogBatchLoad$removeButton,
              class = "bt-remove batch-load-content batch-load-remove-content"
            ),
          )
        )
      ),
      tags$script("$('#shiny-modal').modal().focus();")
    )
  )
}
showNewCompletedJobsDialog <- function() {
  showModal(modalDialog(
    title = lang$nav$newCompletedJobsDialog$title,
    lang$nav$newCompletedJobsDialog$desc,
    footer = tagList(
      modalButton(lang$nav$newCompletedJobsDialog$cancelButton),
      tags$button(
        class = "btn btn-default bt-highlight-1 bt-gms-confirm",
        type = "button", onclick = "Miro.showJobsDialog()",
        lang$nav$newCompletedJobsDialog$okButton
      )
    ),
    fade = TRUE, easyClose = TRUE
  ))
}
getJobsTable <- function(hcubeMeta, jobHist = FALSE, showLogFileDialog = TRUE) {
  if (!inherits(hcubeMeta, "data.frame")) {
    content <- tags$div(
      class = "err-msg",
      lang$errMsg$unknownError
    )
  } else if (length(hcubeMeta) && nrow(hcubeMeta)) {
    jobIds <- hcubeMeta[[1]]
    jobOwners <- hcubeMeta[[2]]
    jobTimes <- hcubeMeta[[4]]
    jStatuses <- hcubeMeta[[3]]
    content <- tags$table(
      class = "miro-table cJob-wrapper",
      tags$tr(
        tags$th(lang$nav$importJobsDialog$header$owner),
        tags$th(lang$nav$importJobsDialog$header$date),
        tags$th(lang$nav$importJobsDialog$header$name),
        tags$th(lang$nav$importJobsDialog$header$status),
        if (!jobHist) {
          tags$th(lang$nav$importJobsDialog$header$action)
        }
      ),
      do.call("tagList", lapply(seq_len(nrow(hcubeMeta)), function(i) {
        jID <- jobIds[i]
        jStatus <- jStatuses[i]
        isHcJob <- identical(
          hcubeMeta[["_scode"]][i],
          SCODEMAP[["hcube_jobconfig"]]
        )
        tags$tr(
          tags$td(jobOwners[i]),
          tags$td(jobTimes[i]),
          tags$td(
            if (isHcJob) {
              tagList(
                trimws(hcubeMeta[["_stag"]][i], whitespace = ","),
                tags$span(class = "badge badge-info", "HC")
              )
            } else {
              hcubeMeta[["_sname"]][i]
            }
          ),
          if (identical(jStatus, JOBSTATUSMAP[["queued"]])) {
            tags$td(lang$nav$importJobsDialog$status$queued)
          } else if (identical(jStatus, JOBSTATUSMAP[["running"]])) {
            tags$td(lang$nav$importJobsDialog$status$running)
          } else if (identical(jStatus, JOBSTATUSMAP[["completed"]])) {
            tags$td(
              tags$div(lang$nav$importJobsDialog$status$completed),
              if (isTRUE(config$activateModules$remoteExecution)) {
                tags$div(
                  class = "progress",
                  style = "display:none;margin-bottom:0;border:1px solid black;",
                  id = paste0("jobImportDlProgressWrapper_", jID),
                  tags$div(
                    class = "progress-bar progress-bar-striped active",
                    id = paste0("jobImportDlProgress_", jID),
                    role = "progressbar", `aria-valuenow` = 5L,
                    `aria-valuemin` = 0, `aria-valuemax` = 100,
                    style = paste0("width:", 5L, "%;"),
                    lang$nav$importJobsDialog$status$downloading
                  )
                )
              }
            )
          } else if (identical(jStatus, JOBSTATUSMAP[["downloaded"]])) {
            tags$td(lang$nav$importJobsDialog$status$downloaded)
          } else if (identical(jStatus, JOBSTATUSMAP[["corrupted"]])) {
            tags$td(lang$nav$importJobsDialog$status$corrupted)
          } else if (identical(jStatus, JOBSTATUSMAP[["corrupted(noDir)"]])) {
            tags$td(
              class = "ttip", lang$nav$importJobsDialog$status$corrupted,
              tags$span(
                lang$nav$importJobsDialog$ttips$corruptedNoDir
              )
            )
          } else if (identical(jStatus, JOBSTATUSMAP[["corrupted(noProcess)"]])) {
            tags$td(
              class = "ttip", lang$nav$importJobsDialog$status$corrupted,
              tags$span(
                lang$nav$importJobsDialog$ttips$corruptedNoProcess
              )
            )
          } else if (identical(jStatus, JOBSTATUSMAP[["corrupted(man)"]])) {
            tags$td(
              class = "ttip", lang$nav$importJobsDialog$status$corrupted,
              tags$span(
                lang$nav$importJobsDialog$ttips$corruptedManual
              )
            )
          } else if (identical(jStatus, JOBSTATUSMAP[["discarded"]])) {
            tags$td(lang$nav$importJobsDialog$status$discarded)
          } else if (identical(jStatus, JOBSTATUSMAP[["discarded(corrupted)"]])) {
            tags$td(
              class = "ttip", lang$nav$importJobsDialog$status$discarded,
              tags$span(
                lang$nav$importJobsDialog$ttips$discardedCorrupted
              )
            )
          } else if (identical(jStatus, JOBSTATUSMAP[["discarded(scheduled)"]])) {
            tags$td(
              class = "ttip", lang$nav$importJobsDialog$status$discarded,
              tags$span(
                lang$nav$importJobsDialog$ttips$discardedScheduled
              )
            )
          } else if (identical(jStatus, JOBSTATUSMAP[["discarded(running)"]])) {
            tags$td(
              class = "ttip", lang$nav$importJobsDialog$status$discarded,
              tags$span(
                lang$nav$importJobsDialog$ttips$discardedActive
              )
            )
          } else if (identical(jStatus, JOBSTATUSMAP[["discarded(completed)"]])) {
            tags$td(
              class = "ttip", lang$nav$importJobsDialog$status$discarded,
              tags$span(
                lang$nav$importJobsDialog$ttips$discardedCompleted
              )
            )
          } else if (identical(jStatus, JOBSTATUSMAP[["imported"]])) {
            tags$td(lang$nav$importJobsDialog$status$imported)
          } else if (identical(jStatus, JOBSTATUSMAP[["imported(man)"]])) {
            tags$td(
              class = "ttip", lang$nav$importJobsDialog$status$imported,
              tags$span(
                lang$nav$importJobsDialog$ttips$importedManual
              )
            )
          } else {
            tags$td(jStatus)
          },
          if (!jobHist) {
            tags$td(
              if (jStatus %in% c(
                JOBSTATUSMAP[["completed"]],
                JOBSTATUSMAP[["downloaded"]]
              )) {
                if (isTRUE(config$activateModules$remoteExecution)) {
                  jobStatus <- jStatus
                } else {
                  jobStatus <- JOBSTATUSMAP[["downloaded"]]
                }
                tagList(
                  tags$button(
                    class = "btn btn-default",
                    style = if (identical(jobStatus, JOBSTATUSMAP[["completed"]])) {
                      ""
                    } else {
                      "display:none;"
                    },
                    id = paste0("btDownloadJob_", jID),
                    onclick = paste0(
                      "Shiny.setInputValue('downloadJobData',",
                      jID, ",{priority:\'event\'});"
                    ),
                    lang$nav$importJobsDialog$buttons$download
                  ),
                  tags$button(
                    class = "btn btn-default",
                    style = if (identical(jobStatus, JOBSTATUSMAP[["completed"]])) {
                      "display:none"
                    } else {
                      ""
                    },
                    id = paste0("btImportJob_", jID),
                    onclick = paste0(
                      "Shiny.setInputValue('importJob',",
                      jID, ",{priority:\'event\'});"
                    ),
                    lang$nav$importJobsDialog$buttons$import
                  ),
                  if (!isHcJob && showLogFileDialog) {
                    tags$button(
                      class = "btn btn-default",
                      onclick = paste0(
                        "Shiny.setInputValue('showJobLog', '",
                        jID, "',{priority:\'event\'});"
                      ),
                      lang$nav$importJobsDialog$buttons$log
                    )
                  }
                )
              },
              if (isHcJob && identical(jStatus, JOBSTATUSMAP[["running"]])) {
                tags$button(
                  class = "btn btn-default",
                  onclick = paste0(
                    "Shiny.setInputValue('showJobProgress', '",
                    jID, "',{priority:\'event\'});"
                  ),
                  lang$nav$importJobsDialog$buttons$progress
                )
              },
              tags$button(
                class = "btn btn-default",
                onclick = paste0(
                  "Miro.confirmModalShow('",
                  lang$nav$importJobsDialog$discardConfirm$title, "', '",
                  lang$nav$importJobsDialog$discardConfirm$desc, "', '",
                  lang$nav$importJobsDialog$discardConfirm$cancelButton, "', '",
                  lang$nav$importJobsDialog$discardConfirm$confirmButton,
                  "', 'Shiny.setInputValue(\\'discardJob\\',", jID,
                  ",{priority:\\'event\\'})')"
                ),
                lang$nav$importJobsDialog$buttons$discard
              )
            )
          }
        )
      }))
    )
  } else {
    content <- tags$div(
      style = "padding:20px;text-align:center;",
      if (jobHist) {
        lang$nav$importJobsDialog$noJobsHist
      } else {
        lang$nav$importJobsDialog$noJobs
      }
    )
  }
  if (jobHist) {
    return(getJobsTableSkeleton(content = content))
  } else {
    return(content)
  }
}
showJobsCompletedDialog <- function() {
  showModal(modalDialog(
    title = lang$nav$hcubeMode$jobsCompletedDialog$title,
    lang$nav$hcubeMode$jobsCompletedDialog$desc,
    fade = TRUE, easyClose = TRUE
  ))
}
showJobHistoryDialog <- function(jobMeta) {
  showModal(modalDialog(
    title = lang$nav$importJobsDialog$histTitle,
    getJobsTable(jobMeta, jobHist = TRUE),
    fade = TRUE, easyClose = TRUE, size = "l"
  ))
}
showJobLogFileDialog <- function(jID) {
  logTabsetList <- list()
  if (config$activateModules$logFile) {
    logTabsetList$log <- tabPanel(
      title = tags$div(class = "log-tab-color", lang$nav$gams$boxGamsOutput$gamsOutputTabset$logFile),
      value = paste0("log_", jID),
      tags$pre(
        style = "max-height:400px;max-height:50vh;overflow:auto;",
        id = "asyncLogContainer",
        genSpinner(extraClasses = "gen-spinner-black")
      )
    )
  }
  if (config$activateModules$lstFile) {
    logTabsetList$lst <- tabPanel(
      title = tags$div(class = "log-tab-color", lang$nav$gams$boxGamsOutput$gamsOutputTabset$lstFile),
      value = paste0("listfile_", jID),
      tags$pre(
        style = "max-height:400px;max-height:50vh;overflow:auto;",
        id = "asyncLstContainer",
        genSpinner(extraClasses = "gen-spinner-black")
      )
    )
  }
  if (config$activateModules$miroLogFile) {
    logTabsetList$miroLog <- tabPanel(
      title = tags$div(class = "log-tab-color", lang$nav$gams$boxGamsOutput$gamsOutputTabset$miroLogFile),
      value = paste0("mirolog_", jID),
      tags$pre(
        style = "max-height:400px;max-height:50vh;overflow:auto;",
        id = "asyncMiroLogContainer",
        genSpinner(extraClasses = "gen-spinner-black")
      )
    )
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
showJobProgressDialog <- function(jID, progressStatus) {
  percentCompleted <- round(progressStatus$noCompleted / progressStatus$noTotal * 100)
  showModal(modalDialog(
    title = lang$nav$hcubeMode$showJobProgressDialog$title,
    tags$div(
      class = "progress",
      tags$div(
        class = "progress-bar progress-bar-striped active",
        id = paste0("hcubeProgress", jID),
        role = "progressbar", `aria-valuenow` = percentCompleted,
        `aria-valuemin` = 0, `aria-valuemax` = 100,
        style = paste0("width:", percentCompleted, "%;"),
        paste0(
          progressStatus$noCompleted, "/", progressStatus$noTotal,
          if (length(progressStatus$noFail)) paste0(" (", progressStatus$noFail, ")")
        )
      )
    ),
    footer = modalButton(lang$nav$hcubeMode$showJobProgressDialog$cancelButton),
    fade = TRUE, easyClose = TRUE
  ))
}
# Hypercube load module
generateLine <- function(i, j, type, label, choices = NULL) {
  fluidRow(
    class = "vertical-align",
    id = paste0("line", i, "_", j),
    tags$div(
      class = "col-xs-10 col-sm-10 col-lg-9",
      fluidRow(
        tags$div(class = "item-name col-sm-12 col-lg-5", helpText(label)),
        tags$div(
          class = "item-scen-drop col-sm-6 col-lg-3",
          switch(type,
            number = {
              selectInput(paste0("op_", i, "_", j),
                label = NULL,
                choices = c("=", "<", ">", "<=", ">=", "!=")
              )
            },
            text = {
              selectInput(paste0("op_", i, "_", j),
                label = NULL,
                choices = setNames(
                  c(
                    "=", "!=", "%LIKE%",
                    "%NOTLIKE%", "LIKE%", "%LIKE",
                    "%EXIST", "%NOTEXIST"
                  ),
                  c(
                    lang$nav$queryBuilder$operators$is,
                    lang$nav$queryBuilder$operators$notis,
                    lang$nav$queryBuilder$operators$contains,
                    lang$nav$queryBuilder$operators$notcontains,
                    lang$nav$queryBuilder$operators$startswith,
                    lang$nav$queryBuilder$operators$endswith,
                    lang$nav$queryBuilder$operators$exists,
                    lang$nav$queryBuilder$operators$notexists
                  )
                ),
                selected = "="
              )
            },
            csv = {
              selectInput(paste0("op_", i, "_", j),
                label = NULL,
                choices = setNames(
                  c(
                    "%,LIKE,%", "%,NOTLIKE,%", "%LIKE%",
                    "%NOTLIKE%", ",LIKE%", "%LIKE,"
                  ),
                  c(
                    lang$nav$queryBuilder$operators$is,
                    lang$nav$queryBuilder$operators$notis,
                    lang$nav$queryBuilder$operators$contains,
                    lang$nav$queryBuilder$operators$notcontains,
                    lang$nav$queryBuilder$operators$startswith,
                    lang$nav$queryBuilder$operators$endswith
                  )
                ),
                selected = "%,LIKE,%"
              )
            },
            date = {
              selectInput(paste0("op_", i, "_", j),
                label = NULL,
                choices = setNames("BETWEEN", lang$nav$queryBuilder$operators$between)
              )
            }
          )
        ),
        tags$div(
          class = "item-search-crit col-sm-6 col-lg-4",
          switch(type,
            number = {
              numericInput(paste0("val_", i, "_", j),
                label = NULL,
                value = 0L
              )
            },
            date = {
              dateRangeInput(paste0("val_", i, "_", j), label = NULL)
            },
            {
              conditionalPanel(
                paste0("!['%EXIST','%NOTEXIST'].includes(input.op_", i, "_", j, ")"),
                if (identical(type, "csv") && length(choices)) {
                  selectizeInput(paste0("val_", i, "_", j), NULL, choices,
                    multiple = FALSE, options = list(
                      "create" = TRUE,
                      "persist" = FALSE
                    ),
                  )
                } else {
                  textInput(paste0("val_", i, "_", j), label = NULL)
                }
              )
            }
          )
        )
      )
    ),
    tags$div(
      class = "col-xs-2 col-sm-2 custom-left-padding",
      fluidRow(
        tags$div(
          class = "col-lg-2 no-side-margin item-delete",
          actionButton(paste0("btRemoveLine", i, "_", j), label = NULL, icon = icon("circle-minus"), class = "btn-custom btn-item-delete")
        )
      )
    )
  )
}
addQueryBuilderBlock <- function(id, choices) {
  insertUI(
    selector = "#selectorsWrapper",
    where = "beforeEnd",
    ui = tags$div(
      id = paste0("block", id), style = "position:relative;", if (id > 1L) {
        tags$div(
          tags$hr(),
          tags$div(class = "or-sign", lang$nav$queryBuilder$orButton)
        )
      },
      tags$div(class = "and-sign", lang$nav$queryBuilder$conditionBlock),
      tags$div(
        class = "and-wrapper",
        fluidRow(
          id = paste0("blockContent", id),
          class = "grid-container"
        ),
        fluidRow(
          class = "and-row",
          tags$div(
            class = "col-xs-10 col-sm-10 col-lg-2",
            selectInput(paste0("newLine_", id), "",
              choices = c(setNames("", lang$nav$queryBuilder$andButton), choices)
            )
          ),
          if (id > 1L) {
            tags$div(
              class = "col-lg-2 col-lg-offset-8",
              actionButton(paste0("btRemoveBlock", id), label = lang$nav$queryBuilder$rmBlock, class = "btn-custom btn-remove-block")
            )
          }
        )
      )
    )
  )
}

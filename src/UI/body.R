## UI body
genSplitCompButtons <- function(id) {
  tags$div(
    id = paste0("scenSplit", id, "_open"), class = "split-open",
    tags$div(
      title = lang$nav$scen$tooltips$load, class = "split-open-cell",
      actionButton(paste0("btScenSplit", id, "_open"), lang$nav$scen$split$load,
        class = "scenSplit-button-load"
      )
    ),
    tags$div(
      title = lang$nav$scen$tooltips$loadActive, class = "split-open-cell",
      HTML(paste0(
        '<button class="btn btn-default action-button scenSplit-button-load"
type="button" onclick="Shiny.setInputValue(\'loadActiveScenSplitComp\', ', id + 1,
        ', {priority: \'event\'})">',
        lang$nav$scen$split$loadActive, "</button>"
      ))
    )
  )
}
getJobsTableSkeleton <- function(id = NULL, content = NULL) {
  tags$div(
    style = "max-height: 70vh;overflow:auto;margin-bottom:20px",
    tags$div(
      class = "gmsalert gmsalert-success", id = "fetchJobsDiscarded",
      lang$nav$importJobsDialog$discardSuccess
    ),
    tags$div(
      class = "gmsalert gmsalert-success", id = "fetchJobsImported",
      lang$nav$importJobsDialog$importSuccess
    ),
    tags$div(
      class = "gmsalert gmsalert-error", id = "fetchJobsAccessDenied",
      lang$nav$importJobsDialog$accessDenied
    ),
    tags$div(
      class = "gmsalert gmsalert-error", id = "fetchJobsUnknownHost",
      lang$nav$dialogRemoteLogin$hostNotFound
    ),
    tags$div(
      class = "gmsalert gmsalert-error", id = "fetchJobsJobNotFound",
      lang$nav$importJobsDialog$jobNotFound
    ),
    tags$div(
      class = "gmsalert gmsalert-error", id = "fetchJobsMaxDownloads",
      lang$nav$importJobsDialog$maxDownloads
    ),
    tags$div(
      class = "gmsalert gmsalert-error", id = "fetchJobsError",
      lang$errMsg$unknownError
    ),
    if (is.null(id)) {
      content
    } else {
      uiOutput(id)
    }
  )
}
buildUI <- TRUE
if (!debugMode) {
  miroCacheFile <- paste0(
    modelNameRaw, "_",
    MIROVersion, "_0_",
    miroLanguage,
    if (config$activateModules$remoteExecution) "_1" else "_0"
  )
  if (isShinyProxy) {
    miroCacheFile <- file.path(Sys.getenv("MIRO_DATA_DIR"), "cache", miroCacheFile)
  } else {
    miroCacheFile <- file.path(miroWorkspace, "cache", miroCacheFile)
  }
  if (file.exists(miroCacheFile)) {
    load(miroCacheFile)
    if (dir.exists(cacheTestDir)) {
      buildUI <- FALSE
    }
  }
}

if (buildUI) {
  inputTabContent <- lapply(seq_along(inputTabs), function(tabId) {
    content <- lapply(inputTabs[[tabId]], function(i) {
      hasDependency <- !is.null(modelInWithDep[[names(modelIn)[[i]]]])
      tabContent <- switch(modelIn[[i]]$type,
        slider = {
          if (hasDependency) {
            sliderStepSize <- 1L
            slider <- sliderInput(paste0("slider_", i),
              label = modelIn[[i]]$slider$label, min = NULL, max = NULL,
              value = if (length(modelIn[[i]]$slider$default) > 1) {
                numeric(2L)
              } else {
                numeric(1L)
              }, step = sliderStepSize,
              width = modelIn[[i]]$slider$width,
              ticks = if (isFALSE(modelIn[[i]]$slider$ticks)) FALSE else TRUE
            )
            slider <- tagList(
              tags$ul(class = "err-msg input-validation-error", id = "valErr_" %+% names(modelIn)[i]),
              tagAppendAttributes(slider, style = "display:none;"),
              tags$div(
                id = paste0("no_data_dep_", i), class = "in-no-data-dep",
                lang$nav$inputScreen$noDataDep
              )
            )
          } else {
            sliderName <- tolower(names(modelIn)[[i]])
            sliderStepSize <- sliderValues[[sliderName]]$step
            slider <- tagList(
              tags$ul(class = "err-msg input-validation-error", id = "valErr_" %+% names(modelIn)[i]),
              sliderInput(paste0("slider_", i),
                label = modelIn[[i]]$slider$label,
                min = sliderValues[[sliderName]]$min,
                max = sliderValues[[sliderName]]$max,
                value = sliderValues[[sliderName]]$def,
                step = sliderValues[[sliderName]]$step,
                width = modelIn[[i]]$slider$width,
                ticks = if (isFALSE(modelIn[[i]]$slider$ticks)) FALSE else TRUE
              )
            )
          }
          slider
        },
        dropdown = {
          if (hasDependency) {
            tagList(
              tags$ul(class = "err-msg input-validation-error", id = "valErr_" %+% names(modelIn)[i]),
              tagAppendAttributes(selectInput(paste0("dropdown_", i),
                label = modelIn[[i]]$dropdown$label,
                choices = character(0), selected = character(0),
                multiple = isTRUE(modelIn[[i]]$dropdown$multiple)
              ),
              style = "display:none;"
              ),
              tags$div(
                id = paste0("no_data_dep_", i), class = "in-no-data-dep",
                lang$nav$inputScreen$noDataDep
              )
            )
          } else {
            choices <- modelIn[[i]]$dropdown$choices

            if (!is.null(modelIn[[i]]$dropdown$aliases)) {
              names(choices) <- modelIn[[i]]$dropdown$aliases
            }
            tagList(
              tags$ul(class = "err-msg input-validation-error", id = "valErr_" %+% names(modelIn)[i]),
              selectInput(paste0("dropdown_", i),
                label = modelIn[[i]]$dropdown$label,
                choices = choices,
                selected = modelIn[[i]]$dropdown$selected,
                multiple = if (identical(
                  modelIn[[i]]$dropdown$multiple,
                  TRUE
                )) {
                  TRUE
                } else {
                  FALSE
                }
              )
            )
          }
        },
        dropdowne = {
          tagList(
            tags$ul(class = "err-msg input-validation-error", id = "valErr_" %+% names(modelIn)[i]),
            selectInput(paste0("dropdowne_", i),
              label = modelIn[[i]]$dropdowne$label,
              choices = character(0), selected = character(0),
              multiple = if (identical(
                modelIn[[i]]$dropdowne$multiple,
                TRUE
              )) {
                TRUE
              } else {
                FALSE
              }
            )
          )
        },
        daterange = {
          tagList(
            tags$ul(class = "err-msg input-validation-error", id = "valErr_" %+% names(modelIn)[i]),
            dateRangeInput(paste0("daterange_", i),
              label = modelIn[[i]]$daterange$label,
              start = modelIn[[i]]$daterange$start,
              end = modelIn[[i]]$daterange$end,
              min = modelIn[[i]]$daterange$min,
              max = modelIn[[i]]$daterange$max,
              format = modelIn[[i]]$daterange$format,
              startview = modelIn[[i]]$daterange$startview,
              weekstart = modelIn[[i]]$daterange$weekstart,
              language = miroLanguage,
              separator = if (identical(
                modelIn[[i]]$daterange$separator,
                NULL
              )) {
                " to "
              } else {
                modelIn[[i]]$daterange$separator
              },
              width = modelIn[[i]]$daterange$width,
              autoclose = if (identical(
                modelIn[[i]]$daterange$autoclose,
                FALSE
              )) {
                FALSE
              } else {
                TRUE
              }
            )
          )
        },
        date = {
          tagList(
            tags$ul(class = "err-msg input-validation-error", id = "valErr_" %+% names(modelIn)[i]),
            dateInput(paste0("date_", i),
              label = modelIn[[i]]$date$label,
              value = modelIn[[i]]$date$value, min = modelIn[[i]]$date$min,
              max = modelIn[[i]]$date$max, format = modelIn[[i]]$date$format,
              startview = modelIn[[i]]$date$startview,
              weekstart = modelIn[[i]]$date$weekstart,
              language = miroLanguage,
              width = modelIn[[i]]$date$width
            )
          )
        },
        checkbox = {
          if (hasDependency) {
            tagList(
              tags$ul(class = "err-msg input-validation-error", id = "valErr_" %+% names(modelIn)[i]),
              tags$div(
                id = paste0("cbDiv_", i), style = "display:none;",
                tags$label(
                  class = "cb-label", "for" = paste0("cb_", i),
                  modelIn[[i]]$checkbox$label
                ),
                tags$div(
                  tags$label(
                    class = modelIn[[i]]$checkbox$class,
                    "for" = paste0("cb_", i),
                    checkboxInput(paste0("cb_", i),
                      label = NULL, value = NULL,
                      width = modelIn[[i]]$checkbox$width
                    )
                  )
                )
              ),
              tags$div(
                id = paste0("no_data_dep_", i), class = "in-no-data-dep",
                lang$nav$inputScreen$noDataDep
              )
            )
          } else {
            tagList(
              tags$ul(class = "err-msg input-validation-error", id = "valErr_" %+% names(modelIn)[i]),
              tags$label(class = "cb-label", "for" = paste0("cb_", i), modelIn[[i]]$checkbox$label),
              tags$div(
                tags$label(
                  class = modelIn[[i]]$checkbox$class, "for" = paste0("cb_", i),
                  checkboxInput(paste0("cb_", i),
                    label = NULL,
                    value = modelIn[[i]]$checkbox$value,
                    width = modelIn[[i]]$checkbox$width
                  )
                )
              )
            )
          }
        },
        textinput = {
          tagList(
            tags$ul(class = "err-msg input-validation-error", id = "valErr_" %+% names(modelIn)[i]),
            textInput(paste0("text_", i),
              label = modelIn[[i]]$textinput$label,
              value = modelIn[[i]]$textinput$value,
              width = modelIn[[i]]$textinput$width,
              placeholder = modelIn[[i]]$textinput$placeholder
            )
          )
        },
        numericinput = {
          tagList(
            tags$ul(class = "err-msg input-validation-error", id = "valErr_" %+% names(modelIn)[i]),
            autoNumericInput(paste0("numeric_", i),
              label = modelIn[[i]]$numericinput$label,
              value = modelIn[[i]]$numericinput$value,
              min = modelIn[[i]]$numericinput$min,
              max = modelIn[[i]]$numericinput$max,
              decimal = modelIn[[i]]$numericinput[["decimal"]],
              sign = modelIn[[i]]$numericinput$sign,
              decimalCharacter = modelIn[[i]]$numericinput[["decimalCharacter"]],
              digitGroupSeparator = modelIn[[i]]$numericinput$digitGroupSeparator
            )
          )
        },
        {
          tagList(
            if (length(modelIn[[i]]$label) && !identical(trimws(modelIn[[i]]$label), "")) {
              tags$div(
                id = paste0("tableLabel_", i),
                class = "readme-wrapper label-wrapper",
                markdown(modelIn[[i]]$label)
              )
            },
            tags$ul(class = "err-msg input-validation-error", id = "valErr_" %+% names(modelIn)[i]),
            tags$div(id = paste0("data-in_", i), {
              if (modelIn[[i]]$type == "hot") {
                tagList(
                  tags$div(
                    class = "hot-search-wrapper",
                    tags$label(
                      lang$nav$hot$search,
                      tags$input(
                        class = "hot-search-box",
                        type = "search", id = paste0("in_", i, "-search")
                      )
                    )
                  ),
                  rHandsontableOutput(paste0("in_", i))
                )
              } else if (modelIn[[i]]$type == "dt") {
                tagList(
                  tags$div(
                    style = "margin-bottom:10px;",
                    actionButton(
                      paste0("in_", i, "_add_row"),
                      lang$renderers$miroPivot$btAddRow
                    ),
                    actionButton(paste0("in_", i, "_remove_row"),
                      lang$renderers$miroPivot$btRemoveRows,
                      class = "bt-remove"
                    )
                  ),
                  dataTableOutput(paste0("in_", i))
                )
              } else {
                tryCatch(
                  {
                    generateDataUI(paste0("data-in_", i),
                      type = modelIn[[i]]$rendererName,
                      customOptions = modelIn[[i]]$options,
                      height = modelIn[[i]]$height
                    )
                  },
                  error = function(e) {
                    flog.error(paste0(
                      sprintf(lang$errMsg$renderGraph$desc, modelInAlias[i]),
                      conditionMessage(e)
                    ))
                    errMsg <- sprintf(lang$errMsg$renderGraph$desc, modelInAlias[i])
                    showErrorMsg(lang$errMsg$renderGraph$title, errMsg)
                  }
                )
              }
            }),
            tags$div(
              id = paste0("graph-in_", i), class = "render-output",
              style = paste0("padding:1px;display:none;", if (!is.null(configGraphsIn[[i]]$height)) {
                sprintf("min-height: %s;", addCssDim(configGraphsIn[[i]]$height, 5))
              })
            )
          )
        }
      )
      if (length(inputTabTitles[[tabId]]) > 1L) {
        titleId <- match(i, inputTabs[[tabId]]) + 1L
        return(tabPanel(
          title = inputTabTitles[[tabId]][titleId],
          value = paste0("inputTabset", tabId, "_", titleId - 1L),
          tags$div(class = "small-space"),
          tabContent,
          tags$div(class = "small-space")
        ))
      }
      if (length(inputTabs[[tabId]]) > 1L) {
        return(column(width = 6L, tabContent))
      }
      return(tabContent)
    })

    return(tabPanel(
      title = inputTabTitles[[tabId]][1],
      value = paste0("inputTabset_", tabId),
      if (length(inputTabTitles[[tabId]]) > 1L) {
        MIROtabsetPanel(
          id = paste0("inputTabset", tabId),
          btCollapsedTabs = lang$nav$inputScreen$btCollapsedTabs,
          content
        )
      } else {
        tagList(
          tags$div(class = "small-space"),
          if (length(inputTabs[[tabId]]) > 1L) {
            fluidRow(content)
          } else {
            content
          },
          tags$div(class = "small-space")
        )
      }
    ))
  })
  if (length(config$readmeFile)) {
    inputTabContent <- c(list(tabPanel(
      title = if (length(config$readme$tabTitle)) {
        config$readme$tabTitle
      } else {
        "README"
      },
      value = "inputTabset_0",
      tagList(
        tags$div(class = "small-space"),
        tags$div(
          class = "readme-wrapper",
          HTML(config$readmeFile)
        ),
        tags$div(class = "small-space")
      )
    )), inputTabContent)
  }
  tabItemList <- list(
    tabItem(
      tabName = "inputData",
      fluidRow(
        box(
          title = list(
            tags$div(
              id = "dirtyFlagIcon", title = lang$nav$inputScreen$dirtyFlag, class = "inline-el",
              style = "display:none;", icon("exclamation-triangle")
            ),
            uiOutput("inputDataTitle", inline = TRUE),
            tags$div(
              style = "float: right;",
              HTML(paste0(
                '<button type="button" class="btn btn-default bt-icon btRemove" id="btRemove1"
                                   onclick="Miro.confirmModalShow(\'',
                lang$nav$dialogRemoveScen$title, "', '",
                lang$nav$dialogRemoveScen$desc, "', '",
                lang$nav$dialogRemoveScen$cancelButton, "', '",
                lang$nav$dialogRemoveScen$okButton,
                '\', \'Shiny.setInputValue(\\\'btRemoveConfirm\\\', 1, {priority: \\\'event\\\'})\')">
                            <i class="fa fa-times" role="presentation" aria-label="', lang$nav$dialogRemoveScen$title, '"></i></button>'
              ))
            )
          ), status = "primary", solidHeader = TRUE, width = 12L,
          tags$div(
            class = "scen-header",
            tags$div(
              class = "out-buttons-wrapper",
              tags$div(
                title = lang$nav$scen$tooltips$btTableView, class = "scen-button-tt",
                tagAppendAttributes(
                  actionButton(
                    inputId = "btGraphIn",
                    icon = icon("chart-bar"), label = NULL,
                    class = "scen-button"
                  ),
                  disabled = ""
                )
              )
            )
          ),
          tags$div(class = "small-space"),
          MIROtabBox(
            id = "inputTabset",
            btCollapsedTabs = lang$nav$inputScreen$btCollapsedTabs,
            inputTabContent, hideTabs = identical(length(inputTabContent), 1L)
          )
        )
      )
    ),
    tabItem(
      tabName = "loadResults",
      fluidRow(
        box(
          title = lang$nav$queryBuilder$title, status = "primary",
          solidHeader = TRUE, width = 12, style = "overflow-x: auto",
          tags$div(
            id = "loadContent",
            tags$div(id = "selectorsWrapper"),
            tags$div(
              id = "buttonsWrapper", class = "item-or-query",
              actionButton("btNewBlock",
                label = lang$nav$queryBuilder$orButton,
                class = "btn-custom"
              )
            ),
            tags$div(
              class = "item-or-query",
              actionButton("btSendQuery",
                label = lang$nav$queryBuilder$queryButton,
                class = "bt-highlight-1"
              )
            )
          ),
          genSpinner(id = "hyperQueryLoad", hidden = TRUE, absolute = FALSE),
          tags$div(id = "queryBuilderError", class = "gmsalert gmsalert-error"),
          tags$div(style = "min-height: 80px;", dataTableOutput("batchLoadResults")),
          tags$div(
            id = "batchLoadNoData",
            style = "text-align:center;font-size:16px;font-weight:bold;margin:20px;display:none;",
            lang$nav$queryBuilder$noData
          ),
          tags$div(
            id = "batchLoadButtons", style = "display:none;padding:30px 0 50px 0;",
            tags$div(
              class = "col-sm-6",
              tags$div(
                actionButton("batchLoadSelected", lang$nav$queryBuilder$chooseSelectedButton,
                  class = "bt-highlight-1"
                ),
                actionButton("batchLoadCurrent", lang$nav$queryBuilder$chooseCurrentButton,
                  class = "bt-highlight-1"
                ),
                actionButton("batchLoadAll", lang$nav$queryBuilder$chooseAllButton,
                  class = "bt-highlight-1"
                )
              )
            )
          )
        )
      )
    ),
    tabItem(
      tabName = "scenarios",
      generateScenarioTabsetPivot(),
      tags$div(
        id = "scen-tab-view", style = if (identical(config$defCompMode, "tab")) "" else "display:none;",
        tags$div(
          style = "float: right;margin: 12px 5px;",
          tags$a(
            id = "btCmpTabCloseAll", style = "padding: 3px;",
            style = "display:none",
            href = "#",
            onclick = paste0(
              "Miro.confirmModalShow('",
              lang$nav[["dialogCloseAllScen"]]$title, "', '",
              lang$nav[["dialogCloseAllScen"]]$desc, "', '",
              lang$nav[["dialogCloseAllScen"]]$cancelButton, "', '",
              lang$nav[["dialogCloseAllScen"]]$okButton,
              "','Shiny.setInputValue(\\'btCmpTabCloseAll\\',1,{priority:\\'event\\'})')"
            ),
            lang$nav$scen$btCloseAll
          )
        ),
        tabsetPanel(id = "scenTabset"),
        tags$div(
          id = "cmpTabNoScenWrapper", lang$nav$scen$noScen, class = "no-scen",
          tags$div(
            style = "margin: 10px;",
            tags$button(
              class = "btn btn-default action-button",
              type = "button",
              onclick = "Shiny.setInputValue('btLoadScen',1,{priority: 'event'})",
              lang$nav$scen$btLoad
            )
          )
        )
      ),
      fluidRow(
        tags$div(
          id = "scen-split-view", style = if (identical(config$defCompMode, "split")) "" else "display:none;",
          box(
            width = 6, solidHeader = TRUE, status = "primary", title =
              tagList(
                tags$span(id = "cmpScenTitle_2"),
                tags$div(
                  style = "float: right;",
                  actionButton(
                    inputId = "btScenSplit1_close",
                    class = "bt-icon",
                    icon = icon("times"),
                    label = NULL
                  )
                )
              ),
            tags$div(id = "scenSplit1_content", style = "display:none;"),
            genSplitCompButtons(1)
          ),
          box(
            width = 6, solidHeader = TRUE, status = "primary",
            title = tagList(
              tags$span(id = "cmpScenTitle_3"),
              tags$div(
                style = "float: right;",
                actionButton(
                  inputId = "btScenSplit2_close",
                  class = "bt-icon", icon = icon("times"), label = NULL
                )
              )
            ),
            tags$div(id = "scenSplit2_content", style = "display:none;"),
            genSplitCompButtons(2)
          )
        )
      )
    )
  )
  outputTabset <- tagList(
    fluidRow(
      box(
        title = lang$nav$gams$boxModelStatus$title, status = "primary", solidHeader = TRUE, width = 12,
        uiOutput("modelStatus")
      )
    ),
    if (any(
      config$activateModules$logFile, config$activateModules$lstFile,
      config$activateModules$miroLogFile
    )) {
      logTabsetList <- list()
      if (config$activateModules$logFile) {
        logTabsetList$log <- tabPanel(
          title = tags$div(class = "log-tab-color", lang$nav$gams$boxGamsOutput$gamsOutputTabset$logFile),
          value = "log",
          tags$pre(
            id = "logStatusContainer",
            class = "shiny-text-output noplaceholder"
          ),
          checkboxInput("logUpdate",
            label = lang$nav$gams$boxGamsOutput$gamsOutputTabset$logUpdate,
            value = TRUE
          )
        )
        if (config$activateModules$miroLogFile) {
          logTabsetList$miroLog <- tabPanel(
            title = tags$div(class = "log-tab-color", lang$nav$gams$boxGamsOutput$gamsOutputTabset$miroLogFile),
            value = "mirolog",
            tagAppendAttributes(
              class = "shiny-text-output noplaceholder pre-style-div",
              uiOutput("miroLogContainer")
            )
          )
        }
      } else if (config$activateModules$miroLogFile) {
        logTabsetList$log <- tabPanel(
          title = tags$div(class = "log-tab-color", lang$nav$gams$boxGamsOutput$gamsOutputTabset$logFile),
          value = "mirolog",
          tags$div(
            id = "logStatusContainer",
            class = "shiny-text-output noplaceholder pre-style-div"
          ),
          checkboxInput("logUpdate",
            label = lang$nav$gams$boxGamsOutput$gamsOutputTabset$logUpdate,
            value = TRUE
          )
        )
      }
      if (config$activateModules$lstFile) {
        logTabsetList$lst <- tabPanel(
          title = tags$div(class = "log-tab-color", lang$nav$gams$boxGamsOutput$gamsOutputTabset$lstFile),
          value = "listfile",
          verbatimTextOutput("listFileContainer")
        )
      }
      logTabsetList <- unname(logTabsetList)
      logTabsetList$id <- "logFileTabsset"
      fluidRow(
        box(
          title = lang$nav$gams$boxGamsOutput$title, status = "primary", solidHeader = TRUE,
          width = 12, collapsible = TRUE,
          do.call(tabsetPanel, logTabsetList)
        )
      )
    }
  )
  outputTabContent <- lapply(seq_along(outputTabs), function(tabId) {
    content <- lapply(outputTabs[[tabId]], function(i) {
      tabContent <- tagList(
        tags$div(
          id = paste0("scenGraph_1_", i), class = "render-output",
          style = if (!is.null(configGraphsOut[[i]]$height)) {
            sprintf("min-height: %s;", addCssDim(configGraphsOut[[i]]$height, 5))
          }
        ),
        tags$div(id = paste0("scenTable_1_", i), class = "render-output", style = "display:none;")
      )
      if (length(outputTabTitles[[tabId]]) > 1L) {
        titleId <- match(i, outputTabs[[tabId]]) + 1L
        return(tabPanel(
          title = outputTabTitles[[tabId]][titleId],
          value = paste0("outputTabset_", tabId, "_", titleId - 1L),
          tags$div(class = "small-space"),
          tabContent,
          tags$div(class = "small-space")
        ))
      }
      if (length(outputTabs[[tabId]]) > 1L) {
        return(column(width = 6, tabContent))
      }
      return(tabContent)
    })
    return(tabPanel(
      title = outputTabTitles[[tabId]][1],
      value = paste0("outputTabset_", tabId),
      if (length(outputTabTitles[[tabId]]) > 1L) {
        MIROtabsetPanel(
          id = paste0("outputTabset_", tabId),
          btCollapsedTabs = lang$nav$inputScreen$btCollapsedTabs,
          content
        )
      } else {
        tagList(
          tags$div(class = "small-space"),
          if (length(outputTabs[[tabId]]) > 1L) {
            fluidRow(content)
          } else {
            content
          },
          tags$div(class = "small-space")
        )
      }
    ))
  })
  if (length(config$scripts$base)) {
    outputTabContent <- c(
      outputTabContent,
      lapply(seq_along(config$scripts$base), function(scriptId) {
        tabPanel(
          title = config$scripts$base[[scriptId]]$tabTitle,
          value = paste0("outputTabset_", length(outputTabContent) + scriptId),
          tagList(
            tags$div(class = "small-space"),
            tags$button(
              type = "button", class = "btn btn-default btn-run-script", lang$nav$scriptOutput$runButton,
              onclick = paste0("Shiny.setInputValue('runScript', ", scriptId, ", {priority: 'event'})")
            ),
            tags$div(class = "small-space"),
            tags$div(
              id = paste0("scriptOutput_", scriptId), class = "script-wrapper",
              tags$div(class = "out-no-data", lang$nav$outputScreen$boxResults$noData),
              tags$div(
                class = "script-spinner", style = "display:none;text-align:center;",
                genSpinner(absolute = FALSE, externalStyle = "margin-top: 50px")
              ),
              tags$iframe(class = "script-output")
            ),
            tags$div(class = "small-space")
          )
        )
      })
    )
  }
  tabItemList <- c(tabItemList, list(
    tabItem(
      tabName = "gamsinter",
      if (config$activateModules$remoteExecution) {
        fluidRow(
          tabBox(
            width = 12, id = "jobListPanel",
            tabPanel(lang$nav$gams$boxGamsOutput$tabCurrent,
              value = "current",
              outputTabset
            ),
            tabPanel(lang$nav$gams$boxGamsOutput$tabJobList,
              value = "joblist",
              fluidRow(
                box(
                  title = tagList(
                    lang$nav$hcubeImport$title,
                    tags$div(
                      style = "float: right;",
                      actionButton(
                        inputId = "refreshActiveJobs",
                        class = "bt-icon",
                        icon = icon("refresh"), label = NULL
                      )
                    )
                  ),
                  status = "primary", solidHeader = TRUE, width = 12,
                  genSpinner("jImport_load", absolute = FALSE),
                  getJobsTableSkeleton(id = "jImport_output"),
                  tags$div(
                    class = "col-sm-6",
                    actionButton(
                      "btShowHistory",
                      lang$nav$hcubeImport$btShowHistory
                    )
                  )
                )
              )
            )
          )
        )
      } else {
        outputTabset
      }
    ),
    tabItem(
      tabName = "outputData",
      fluidRow(
        box(
          title = list(
            tags$div(
              id = "dirtyFlagIconO", title = lang$nav$inputScreen$dirtyFlag, class = "inline-el",
              style = "display:none;", icon("exclamation-triangle")
            ),
            uiOutput("outputDataTitle", inline = TRUE),
            tags$div(
              style = "float: right;",
              HTML(paste0(
                '<button type="button" class="btn btn-default bt-icon btRemove"
                                   onclick="Miro.confirmModalShow(\'',
                lang$nav$dialogRemoveScen$title, "', '",
                lang$nav$dialogRemoveScen$desc, "', '",
                lang$nav$dialogRemoveScen$cancelButton, "', '",
                lang$nav$dialogRemoveScen$okButton,
                '\', \'Shiny.setInputValue(\\\'btRemoveConfirm\\\', 1, {priority: \\\'event\\\'})\')">
                            <i class="fa fa-times" role="presentation" aria-label="', lang$nav$dialogRemoveScen$title, '"></i></button>'
              ))
            )
          ), status = "primary", solidHeader = TRUE, width = 12,
          tags$div(
            class = "scen-header",
            tags$div(
              class = "out-buttons-wrapper",
              if (isTRUE(config$hasSymbolLinks)) {
                tags$div(
                  title = lang$nav$scen$tooltips$btSymbolLink, class = "scen-button-tt",
                  tags$button(
                    class = "btn btn-default scen-button", id = "btSymbolLink",
                    tags$i(
                      class = "fa fa-share", role = "presentation",
                      `aria-label` = "Load dataset as input data"
                    ),
                    onclick = paste0(
                      "Miro.confirmModalShow('",
                      lang$nav$dialogImport$title, "', '",
                      lang$nav$dialogImport$descOverwriteInput, "', '",
                      lang$nav$dialogImport$cancelButton, "', '",
                      lang$nav$dialogImport$okButton,
                      "', 'Shiny.setInputValue(\\'btSymbolLink\\',1",
                      ",{priority:\\'event\\'})')"
                    )
                  )
                )
              },
              if (isTRUE(config$activateModules$downloadTempFiles)) {
                tags$div(
                  title = lang$nav$scen$tooltips$btDownloadTmpFiles, class = "scen-button-tt",
                  actionButton("btDownloadTmpFiles", icon("folder-open"),
                    class = "scen-button"
                  )
                )
              },
              tags$div(
                title = lang$nav$scen$tooltips$btTableView, class = "scen-button-tt",
                actionButton("outputTableView", icon("chart-bar"),
                  class = "scen-button"
                )
              )
            )
          ),
          tags$div(class = "small-space"),
          MIROtabBox(
            id = "outputTabset", btCollapsedTabs = lang$nav$inputScreen$btCollapsedTabs,
            outputTabContent, hideTabs = identical(length(outputTabContent), 1L)
          )
        )
      )
    )
  ))
  miroBody <- dashboardBody({
    tagList(
      tags$head(
        if (isTRUE(config$readme$enableMath)) {
          tagList(
            tags$link(type = "text/css", rel = "stylesheet", href = "katex.min.css"),
            tags$script(type = "application/javascript", `defer src` = "katex.min.js"),
            tags$script(
              type = "application/javascript", `defer src` = "auto-render.min.js",
              onload = if (isTRUE(config$readme$enableMath)) {
                "renderMathInElement(document.getElementsByClassName('readme-wrapper')[0],
{throwOnError:false,delimiters:[{left:'$$',right:'$$',display:true},{left: '$',right:'$',display:false}]});"
              }
            )
          )
        },
        tags$meta(
          name = "color-scheme",
          content = if (identical(config$theme, "browser")) "dark light" else "normal"
        ),
        tags$link(type = "text/css", rel = "stylesheet", href = paste0("skin_", config$theme, ".css")),
        tags$script(src = "miro.js", type = "application/javascript"),
        # styles that depend on data from config JSON file
        # Logo ratio should be 4,6 (width/height)
        tags$style(HTML(
          paste0(
            '
.main-header .logo {
  background-image: url("',
            if (!identical(config$UILogo, "gams_logo.png") &&
              dir.exists(paste0(currentModelDir, .Platform$file.sep, "static_", modelName))) {
              "static_"
            }, modelName, "/", config$UILogo, '") ',
            if (!identical(config$UILogo, "gams_logo.png") &&
              dir.exists(paste0(currentModelDir, .Platform$file.sep, "static_", modelName))) {
              "!important;
  background-size: contain;
}"
            }
          )
        ))
      ),
      HTML(paste0(
        '<!-- Creates modal dialog for confirm messages -->
<div class="modal fade" id="confirmModal" tabindex="-1" role="dialog" aria-labelledby="myModalLabel" aria-hidden="true">
  <div class="modal-dialog">
    <div class="modal-content" style="width:685px;">
      <div class="modal-header">
        <h4 class="modal-title"></h4>
      </div>
      <div class="modal-body">
      </div>
      <div class="modal-footer">
      </div>
    </div>
</div>
</div>
<div id="loading-screen"><noscript><div class="miro-noscript">Please enable Javascript to use GAMS MIRO</div></noscript>
<div class="lds-ellipsis" style="position:relative;top:50%;left:50%"><div></div><div></div><div></div><div></div>
       </div></div>'
      )),
      do.call(tabItems, tabItemList)
    )
  })
  if (!debugMode) {
    cacheTestDir <- attr(rHandsontableOutput("test"), "html_dependencies")[[1]]$src$file
    if (!dir.exists(dirname(miroCacheFile))) {
      dir.create(dirname(miroCacheFile), recursive = TRUE, mode = "0700")
    }
    save(
      list = c("cacheTestDir", "miroBody"),
      file = miroCacheFile
    )
  }
}
if (dir.exists(paste0(currentModelDir, .Platform$file.sep, "static_", modelName))) {
  addResourcePath(paste0("static_", modelName), paste0(
    currentModelDir, .Platform$file.sep,
    "static_", modelName
  ))
}

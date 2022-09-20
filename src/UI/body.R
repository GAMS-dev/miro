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
    miroLanguage, "_",
    miroColorTheme,
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

if (dir.exists(paste0(currentModelDir, .Platform$file.sep, "static_", modelName))) {
  staticDirExists <- TRUE
  addResourcePath(paste0("static_", modelName), paste0(
    currentModelDir, .Platform$file.sep,
    "static_", modelName
  ))
} else {
  staticDirExists <- FALSE
}

if (buildUI) {
  inputTabContent <- lapply(seq_along(inputTabs), function(tabId) {
    content <- lapply(inputTabs[[tabId]], function(i) {
      hasDependency <- !is.null(modelInWithDep[[names(modelIn)[[i]]]])
      tabContent <- switch(modelIn[[i]]$type,
        slider = {
          widgetlabel <- modelIn[[i]]$slider$label
          if (!is.null(modelIn[[i]]$slider$tooltip)) {
            widgetlabel <- widgetTooltip(widgetlabel, modelIn[[i]]$slider$tooltip)
          }
          if (hasDependency) {
            sliderStepSize <- 1L
            slider <- sliderInput(paste0("slider_", i),
              label = widgetlabel, min = NULL, max = NULL,
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
                label = widgetlabel,
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
          widgetlabel <- modelIn[[i]]$dropdown$label
          if (!is.null(modelIn[[i]]$dropdown$tooltip)) {
            widgetlabel <- widgetTooltip(widgetlabel, modelIn[[i]]$dropdown$tooltip)
          }
          if (hasDependency) {
            tagList(
              tags$ul(class = "err-msg input-validation-error", id = "valErr_" %+% names(modelIn)[i]),
              tagAppendAttributes(selectInput(paste0("dropdown_", i),
                label = widgetlabel,
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
                label = widgetlabel,
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
          widgetlabel <- modelIn[[i]]$dropdowne$label
          if (!is.null(modelIn[[i]]$dropdowne$tooltip)) {
            widgetlabel <- widgetTooltip(widgetlabel, modelIn[[i]]$dropdowne$tooltip)
          }
          tagList(
            tags$ul(class = "err-msg input-validation-error", id = "valErr_" %+% names(modelIn)[i]),
            selectInput(paste0("dropdowne_", i),
              label = widgetlabel,
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
          widgetlabel <- modelIn[[i]]$daterange$label
          if (!is.null(modelIn[[i]]$daterange$tooltip)) {
            widgetlabel <- widgetTooltip(widgetlabel, modelIn[[i]]$daterange$tooltip)
          }
          tagList(
            tags$ul(class = "err-msg input-validation-error", id = "valErr_" %+% names(modelIn)[i]),
            dateRangeInput(paste0("daterange_", i),
              label = widgetlabel,
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
          widgetlabel <- modelIn[[i]]$date$label
          if (!is.null(modelIn[[i]]$date$tooltip)) {
            widgetlabel <- widgetTooltip(widgetlabel, modelIn[[i]]$date$tooltip)
          }
          tagList(
            tags$ul(class = "err-msg input-validation-error", id = "valErr_" %+% names(modelIn)[i]),
            dateInput(paste0("date_", i),
              label = widgetlabel,
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
          widgetlabel <- modelIn[[i]]$checkbox$label
          if (!is.null(modelIn[[i]]$checkbox$tooltip)) {
            widgetlabel <- widgetTooltip(widgetlabel, modelIn[[i]]$checkbox$tooltip)
          }
          if (hasDependency) {
            tagList(
              tags$ul(class = "err-msg input-validation-error", id = "valErr_" %+% names(modelIn)[i]),
              tags$div(
                id = paste0("cbDiv_", i), style = "display:none;",
                tags$label(
                  class = "cb-label", "for" = paste0("cb_", i),
                  widgetlabel
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
              tags$label(class = "cb-label", "for" = paste0("cb_", i), widgetlabel),
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
          widgetlabel <- modelIn[[i]]$textinput$label
          if (!is.null(modelIn[[i]]$textinput$tooltip)) {
            widgetlabel <- widgetTooltip(widgetlabel, modelIn[[i]]$textinput$tooltip)
          }
          tagList(
            tags$ul(class = "err-msg input-validation-error", id = "valErr_" %+% names(modelIn)[i]),
            textInput(paste0("text_", i),
              label = widgetlabel,
              value = modelIn[[i]]$textinput$value,
              width = modelIn[[i]]$textinput$width,
              placeholder = modelIn[[i]]$textinput$placeholder
            )
          )
        },
        numericinput = {
          widgetlabel <- modelIn[[i]]$numericinput$label
          if (!is.null(modelIn[[i]]$numericinput$tooltip)) {
            widgetlabel <- widgetTooltip(widgetlabel, modelIn[[i]]$numericinput$tooltip)
          }
          tagList(
            tags$ul(class = "err-msg input-validation-error", id = "valErr_" %+% names(modelIn)[i]),
            autoNumericInput(paste0("numeric_", i),
              label = widgetlabel,
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
                class = "label-toggle-wrapper",
                tags$div(
                  id = paste0("tableLabel_", i),
                  class = "readme-wrapper label-wrapper label-collapsed",
                  markdown(modelIn[[i]]$label)
                ),
                tags$div(
                  class = "label-toggle",
                  tags$a(
                    id = paste0("tableLabel_", i, "_toggle"),
                    class = "btn toggle-label-height",
                    href = "#",
                    tags$i(class = "fa fa-circle-chevron-down")
                  )
                )
              )
            },
            tags$ul(class = "err-msg input-validation-error", id = "valErr_" %+% names(modelIn)[i]),
            tags$div(id = paste0("data-in_", i), {
              if (modelIn[[i]]$type == "hot") {
                tagList(
                  tags$div(
                    class = "miro-show-on-desktop-devices",
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
                  ),
                  tags$div(
                    class = "miro-show-on-mobile-devices", style = "display:none",
                    dataTableOutput(paste0("in_m_", i)),
                    tags$div(
                      class = "buttons-mobile-wrapper",
                      style = "margin-top:10px;",
                      actionButton(
                        paste0("in_m_", i, "_add_row"),
                        lang$renderers$miroPivot$btAddRow
                      ),
                      actionButton(paste0("in_m_", i, "_remove_row"),
                        lang$renderers$miroPivot$btRemoveRows,
                        class = "bt-remove"
                      )
                    )
                  )
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
              style = paste0("padding:1px;display:none;")
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
  customComparisonTabContent <- lapply(config[["customCompareModules"]], function(compareModuleConfig) {
    fluidRow(
      class = "box-title-mobile",
      tags$div(
        id = paste0("scen-", compareModuleConfig[["id"]], "-view"),
        class = "scen-compare-tab-wrapper",
        style = "display:none;",
        box(
          class = "box-mobile",
          width = 12L, solidHeader = TRUE, status = "primary", title =
            tagList(
              tags$button(
                id = paste0("btRefreshCustomCmp_", compareModuleConfig[["idx"]]),
                title = lang$nav$scen$tooltips$btRefresh,
                disabled = "true",
                class = "btn btn-default bt-icon action-button",
                onclick = paste0("Shiny.setInputValue('btRefreshComp',-", compareModuleConfig[["idx"]], ",{priority: 'event'})"),
                tags$i(
                  class = "fas fa-rotate",
                  `aria-label` = lang$nav$scen$tooltips$btRefresh
                )
              ),
              tags$div(
                style = "float:right;", title = lang$nav$scen$tooltips$btCloseAll,
                tags$button(
                  class = "btn btn-default bt-icon action-button",
                  onclick = paste0("Shiny.setInputValue('btCloseScenCmp','", compareModuleConfig[["id"]], "',{priority: 'event'})"),
                  tags$i(
                    class = "fas fa-xmark",
                    `aria-label` = lang$nav$scen$tooltips$btCloseAll
                  )
                )
              )
            ),
          tags$div(
            id = paste0("cmpCustomNoScenWrapper_", compareModuleConfig[["idx"]]), class = "no-scen", lang$nav$scen$noScen,
            tags$div(
              style = "margin: 10px;",
              tags$button(
                class = "btn btn-default action-button btn-switch-sidebar",
                type = "button",
                "data-target" = "loadResults",
                lang$nav$scen$btLoad
              )
            )
          ),
          tags$div(
            id = paste0("customCompScenWrapper_", compareModuleConfig[["idx"]]),
            style = "margin-top: 10px;display:none;"
          )
        )
      )
    )
  })
  tabItemList <- list(
    tabItem(
      tabName = "inputData",
      fluidRow(
        class = "box-title-mobile",
        box(
          class = "box-mobile",
          title = list(
            tags$div(
              id = "dirtyFlagIcon", title = lang$nav$inputScreen$dirtyFlag, class = "inline-el",
              style = "display:none;", icon("triangle-exclamation")
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
                            <i class="fa fa-xmark" role="presentation" aria-label="', lang$nav$dialogRemoveScen$title, '"></i></button>'
              ))
            )
          ), status = "primary", solidHeader = TRUE, width = 12L,
          tags$div(
            class = "scen-header",
            tags$div(
              class = "out-buttons-wrapper",
              tags$div(
                title = lang$nav$scen$tooltips$btRefreshGraph, class = "scen-button-tt",
                tags$button(
                  class = "btn btn-default action-button scen-button",
                  type = "button",
                  style = "display:none",
                  id = "btRefreshGraphIn",
                  icon("rotate")
                )
              ),
              tags$div(
                title = lang$nav$scen$tooltips$btTableView, class = "scen-button-tt",
                tags$button(
                  class = "btn btn-default action-button scen-button",
                  type = "button",
                  disabled = "",
                  id = "btGraphIn",
                  icon("chart-bar")
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
        class = "box-title-mobile",
        box(
          class = "box-mobile",
          title = lang$nav$queryBuilder$title, status = "primary",
          solidHeader = TRUE, width = 12,
          tags$div(
            id = "loadContent",
            tags$div(id = "selectorsWrapper"),
            tags$div(
              id = "buttonsWrapper", class = "item-or-query",
              actionButton("btNewBlock",
                label = lang$nav$queryBuilder$addBlock,
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
          tags$div(style = "min-height: 80px;margin-top: 15px;", dataTableOutput("batchLoadResults")),
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
        id = "scen-tab-view",
        style = if (identical(config$defCompMode, "tab")) "" else "display:none;",
        class = "scen-compare-tab-wrapper",
        tags$div(
          class = "close-all-tabs",
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
        tags$div(
          class = "tabs-ul-mobile-helper",
          tabsetPanel(id = "scenTabset"),
        ),
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
        class = "box-title-mobile",
        tags$div(
          id = "scen-split-view", style = if (identical(config$defCompMode, "split")) "" else "display:none;",
          class = "scen-compare-tab-wrapper",
          box(
            class = "box-mobile",
            width = 6, solidHeader = TRUE, status = "primary", title =
              tagList(
                tags$span(id = "cmpScenTitle_2"),
                tags$div(
                  style = "float: right;",
                  actionButton(
                    inputId = "btScenSplit1_close",
                    class = "bt-icon",
                    icon = icon("xmark"),
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
                  class = "bt-icon", icon = icon("xmark"), label = NULL
                )
              )
            ),
            tags$div(id = "scenSplit2_content", style = "display:none;"),
            genSplitCompButtons(2)
          )
        )
      ),
      customComparisonTabContent
    )
  )
  outputTabset <- tagList(
    fluidRow(
      class = "box-title-mobile",
      box(
        class = "box-mobile",
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
          tags$div(
            class = "update-mobile",
            checkboxInput("logUpdate",
              label = lang$nav$gams$boxGamsOutput$gamsOutputTabset$logUpdate,
              value = TRUE
            )
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
        class = "box-title-mobile",
        tags$div(
          class = "col-sm-12",
          tags$div(
            class = "box box-solid box-primary",
            tags$div(
              class = "box-header",
              tags$h3(class = "box-title", lang$nav$gams$boxGamsOutput$title)
            ),
            tags$div(
              class = "box-body box-mobile",
              do.call(tabsetPanel, logTabsetList),
              tags$div(
                class = "pull-right",
                tags$div(
                  class = "btn-group batch-load-content dropup",
                  tags$button(
                    class = "btn btn-default dropdown-toggle", `data-toggle` = "dropdown",
                    tags$i(class = "fas fa-download"),
                    tags$i(class = "fas fa-caret-up"),
                    tags$span(class = "sr-only", "toggle download dropdown")
                  ),
                  tags$ul(
                    class = "dropdown-menu dropdown-menu-right",
                    role = "menu",
                    if (config$activateModules$logFile) {
                      tags$li(
                        tags$a(
                          class = "shiny-download-link", type = "button",
                          id = "btDownloadLogFilesLog", target = "_blank", href = "", download = NA,
                          lang$nav$gams$boxGamsOutput$gamsOutputTabset$logFile
                        )
                      )
                    },
                    if (config$activateModules$miroLogFile) {
                      tags$li(
                        tags$a(
                          class = "shiny-download-link", type = "button",
                          id = "btDownloadLogFilesMiroLog", target = "_blank", href = "", download = NA,
                          lang$nav$gams$boxGamsOutput$gamsOutputTabset$miroLogFile
                        )
                      )
                    },
                    if (config$activateModules$lstFile) {
                      tags$li(
                        tags$a(
                          class = "shiny-download-link", type = "button",
                          id = "btDownloadLogFilesLst", target = "_blank", href = "", download = NA,
                          lang$nav$gams$boxGamsOutput$gamsOutputTabset$lstFile
                        )
                      )
                    }
                  )
                ),
                if (config$activateModules$remoteExecution) {
                  tags$button(
                    id = "btDetachCurrentJob", type = "button", title = lang$nav$gams$boxGamsOutput$gamsOutputTabset$detachButton,
                    class = "btn btn-default action-button",
                    tags$i(class = "fas fa-link-slash")
                  )
                }
              )
            )
          )
        )
      )
    }
  )
  outputTabContent <- lapply(seq_along(outputTabs), function(tabId) {
    content <- lapply(outputTabs[[tabId]], function(i) {
      tabContent <- tagList(
        if (length(configGraphsOut[[i]]$label) && !identical(trimws(configGraphsOut[[i]]$label), "")) {
          tags$div(
            class = "label-toggle-wrapper",
            tags$div(
              id = paste0("tableOutLabel_", i),
              class = "readme-wrapper label-wrapper label-collapsed",
              markdown(configGraphsOut[[i]]$label)
            ),
            tags$div(
              class = "label-toggle",
              tags$a(
                id = paste0("tableOutLabel_", i, "_toggle"),
                class = "btn toggle-label-height",
                href = "#",
                tags$i(class = "fa fa-circle-chevron-down")
              )
            )
          )
        },
        tags$div(
          id = paste0("scenGraph_1_", i), class = "render-output"
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
                        icon = icon("rotate"), label = NULL
                      )
                    )
                  ),
                  status = "primary", solidHeader = TRUE, width = 12,
                  genSpinner("jImport_load", absolute = FALSE),
                  getJobsTableSkeleton(id = "jImport_output"),
                  tags$div(
                    class = "col-sm-6 no-padding-left no-padding-right",
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
        class = "box-title-mobile",
        box(
          class = "box-mobile",
          title = list(
            tags$div(
              id = "dirtyFlagIconO", title = lang$nav$inputScreen$dirtyFlag, class = "inline-el",
              style = "display:none;", icon("triangle-exclamation")
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
                            <i class="fa fa-xmark" role="presentation" aria-label="', lang$nav$dialogRemoveScen$title, '"></i></button>'
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
  if (isShinyProxy) {
    staticDir <- "../../assets/static/"
  } else {
    staticDir <- ""
  }
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
        tags$meta(name = "apple-mobile-web-app-capable", content = "yes"),
        tags$link(type = "text/css", rel = "stylesheet", href = if (identical(config$customColorTheme, TRUE)) {
          paste0("static_", modelName, "/custom_theme.css")
        } else {
          paste0(miroColorTheme, "_", config$theme, ".css")
        }),
        tags$script(src = "miro.js", type = "application/javascript"),
        if (staticDirExists && file.exists(file.path(currentModelDir, paste0("static_", modelName), "manifest.json"))) {
          tags$link(rel = "manifest", href = paste0("static_", modelName, "/manifest.json"), crossorigin = "use-credentials")
        } else {
          tags$link(rel = "manifest", href = "manifest.json", crossorigin = "use-credentials")
        },
        if (!isShinyProxy && staticDirExists && file.exists(file.path(currentModelDir, paste0("static_", modelName), "apple-touch-icon.png"))) {
          tags$link(rel = "apple-touch-icon", sizes = "180x180", href = paste0("static_", modelName, "/apple-touch-icon.png"))
        } else {
          tags$link(rel = "apple-touch-icon", sizes = "180x180", href = paste0(staticDir, "apple-touch-icon.png"))
        },
        tags$link(href = paste0(staticDir, "iphone5_splash.png"), media = "(device-width: 320px) and (device-height: 568px) and (-webkit-device-pixel-ratio: 2)", rel = "apple-touch-startup-image"),
        tags$link(href = paste0(staticDir, "iphone6_splash.png"), media = "(device-width: 375px) and (device-height: 667px) and (-webkit-device-pixel-ratio: 2)", rel = "apple-touch-startup-image"),
        tags$link(href = paste0(staticDir, "iphoneplus_splash.png"), media = "(device-width: 621px) and (device-height: 1104px) and (-webkit-device-pixel-ratio: 3)", rel = "apple-touch-startup-image"),
        tags$link(href = paste0(staticDir, "iphonex_splash.png"), media = "(device-width: 375px) and (device-height: 812px) and (-webkit-device-pixel-ratio: 3)", rel = "apple-touch-startup-image"),
        tags$link(href = paste0(staticDir, "iphonexr_splash.png"), media = "(device-width: 414px) and (device-height: 896px) and (-webkit-device-pixel-ratio: 2)", rel = "apple-touch-startup-image"),
        tags$link(href = paste0(staticDir, "iphonexsmax_splash.png"), media = "(device-width: 414px) and (device-height: 896px) and (-webkit-device-pixel-ratio: 3)", rel = "apple-touch-startup-image"),
        tags$link(href = paste0(staticDir, "ipad_splash.png"), media = "(device-width: 768px) and (device-height: 1024px) and (-webkit-device-pixel-ratio: 2)", rel = "apple-touch-startup-image"),
        tags$link(href = paste0(staticDir, "ipadpro1_splash.png"), media = "(device-width: 834px) and (device-height: 1112px) and (-webkit-device-pixel-ratio: 2)", rel = "apple-touch-startup-image"),
        tags$link(href = paste0(staticDir, "ipadpro3_splash.png"), media = "(device-width: 834px) and (device-height: 1194px) and (-webkit-device-pixel-ratio: 2)", rel = "apple-touch-startup-image"),
        tags$link(href = paste0(staticDir, "ipadpro2_splash.png"), media = "(device-width: 1024px) and (device-height: 1366px) and (-webkit-device-pixel-ratio: 2)", rel = "apple-touch-startup-image"),
        tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "favicon-32x32.png"),
        tags$link(rel = "icon", type = "image/png", sizes = "16x16", href = "favicon-16x16.png"),
        tags$meta(name = "msapplication-TileColor", content = "#ff9900"),
        # styles that depend on data from config JSON file
        # Logo ratio should be 4,6 (width/height)
        tags$style(
          HTML(
            paste0(
              '
.main-header .logo {
  background-image: url("',
              if (!identical(config$UILogo, "gams_logo.png") &&
                staticDirExists) {
                "static_"
              }, modelName, "/", config$UILogo, '") ',
              if (!identical(config$UILogo, "gams_logo.png") &&
                staticDirExists) {
                "!important;
  background-size: contain;
}"
              }
            )
          )
        ),
        if (staticDirExists && identical(config$customCss, TRUE)) {
          tags$link(type = "text/css", rel = "stylesheet", href = paste0("static_", modelName, "/custom.css"))
        }
      ),
      HTML(paste0(
        '<!-- Creates modal dialog for confirm messages -->
<div class="modal fade" id="confirmModal" tabindex="-1" role="dialog" aria-hidden="true">
  <div class="modal-dialog">
    <div class="modal-content">
      <div class="modal-header">
        <h4 class="modal-title"></h4>
      </div>
      <div class="modal-body">
      </div>
      <div class="modal-footer modal-footer-mobile">
      </div>
    </div>
</div>
</div>
<div class="modal fade" id="commandPalette" tabindex="-1" role="dialog" aria-hidden="true">
  <div class="modal-dialog modal-dialog-centered">
    <div class="modal-content">
      <div class="modal-body">
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

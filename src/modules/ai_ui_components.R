renderMarkdownPreservingKatexDelimiters <- function(text) {
  wrapped <- stringi::stri_replace_all_fixed(
    text, c("\\[", "\\("), c("`\\[", "`\\("),
    vectorize_all = FALSE
  )
  wrapped <- stringi::stri_replace_all_fixed(
    wrapped, c("\\]", "\\)"), c("\\]`", "\\)`"),
    vectorize_all = FALSE
  )
  html <- commonmark::markdown_html(wrapped, hardbreaks = FALSE, smart = TRUE, extensions = TRUE)
  html <- stringi::stri_replace_all_fixed(
    html, c("<code>\\[", "<code>\\("), c("\\[", "\\("),
    vectorize_all = FALSE
  )
  stringi::stri_replace_all_fixed(
    html, c("\\]</code>", "\\)</code>"), c("\\]", "\\)"),
    vectorize_all = FALSE
  )
}
aiChartIdsIn <- function(text) {
  chartIds <- stringi::stri_match_all_regex(text, "```mirachart[ \\t]*\\n\\s*(aichart_[a-z0-9]+)\\s*\\n[ \\t]*```")[[1]][, 2]
  return(unique(chartIds[!is.na(chartIds)]))
}
renderAiChatBubble <- function(msg, lang, charLimit = 300) {
  isUser <- msg$role == "user"
  wrapperClass <- if (isUser) "miro-ai-chat-bubble-wrapper miro-ai-chat-user" else "miro-ai-chat-bubble-wrapper miro-ai-chat-assistant"
  isLongUserMsg <- isUser && nchar(msg$text) > charLimit
  return(tags$div(
    class = wrapperClass,
    tags$div(
      class = "miro-ai-chat-actions",
      tags$button(
        class = "miro-ai-chat-btn",
        onclick = "Miro.copyChatContent(this)",
        title = lang$btCopy,
        tags$i(class = "fas fa-copy")
      )
    ),
    tags$div(
      class = "miro-ai-chat-bubble",
      tags$b(class = "miro-ai-chat-author", if (isUser) lang$chatBubbleUserHeader else lang$chatBubbleAssistantHeader),
      tags$div(
        class = paste("miro-ai-chat-content-html miro-markdown", if (isLongUserMsg) "miro-ai-text-collapsed" else ""),
        # Assistant output is untrusted (LLM-generated, can echo back
        # user-controlled data such as scenario names) and commonmark has no
        # safe-HTML mode, so the rendered markup is kept out of the DOM as an
        # inert `data-raw-html` attribute (htmltools entity-encodes attribute
        # values) instead of live children. The client sanitizes it with
        # DOMPurify and only then injects it as HTML -- see ai_assistant.js.
        `data-raw-html` = if (isUser) {
          NULL
        } else {
          renderMarkdownPreservingKatexDelimiters(msg$text)
        },
        `data-charts` = if (length(msg$charts)) {
          jsonlite::toJSON(unname(lapply(msg$charts, function(chart) {
            list(
              id = chart$id, title = chart$title, type = chart$type, labels = I(chart$labels),
              canOpenInPivot = isTRUE(chart$canOpenInPivot),
              datasets = lapply(chart$datasets, function(dataset) {
                c(
                  list(label = dataset$label, data = I(dataset$data)),
                  if (length(dataset$color)) list(color = dataset$color),
                  if (length(dataset$colors)) list(colors = I(dataset$colors))
                )
              })
            )
          })), auto_unbox = TRUE, na = "null", digits = NA)
        },
        if (isUser) {
          msg$text
        }
      ),
      if (isLongUserMsg) {
        tags$div(
          class = "miro-ai-expand-wrapper",
          tags$button(
            class = "miro-ai-expand-btn",
            onclick = "Miro.toggleChatCollapse(this)",
            tags$i(class = "fas fa-chevron-down")
          )
        )
      },
      tags$div(class = "miro-ai-chat-content-raw", style = "display: none;", msg$text)
    )
  ))
}
renderAiChatDivider <- function(contextText, backToSandboxLabel = NULL) {
  if (is.null(backToSandboxLabel)) {
    return(tags$div(
      class = "miro-ai-chat-context-divider",
      contextText
    ))
  }
  tags$div(
    class = "miro-ai-chat-context-divider miro-ai-chat-context-divider-action",
    tags$span(class = "miro-ai-chat-context-label", contextText),
    tags$button(
      type = "button",
      class = "miro-ai-context-back-btn",
      title = backToSandboxLabel,
      onclick = "Shiny.setInputValue('btAiBackToSandbox', 1, {priority: 'event'})",
      tags$i(class = "fas fa-arrow-left", `aria-hidden` = "true"),
      backToSandboxLabel
    )
  )
}
aiInputChangeOperationLabel <- function(change, lang) {
  labelKey <- switch(change$operation,
    MULTIPLY = "changeOpMultiply",
    DIVIDE = "changeOpDivide",
    ADD = "changeOpAdd",
    SUBTRACT = "changeOpSubtract",
    SET = "changeOpSet"
  )
  return(sprintf(lang[[labelKey]], formatAiInputValue(change$value)))
}
aiInputChangeFilterLabel <- function(change) {
  if (!length(change$filters)) {
    return(NULL)
  }
  return(paste(vapply(change$filters, function(filterSpec) {
    return(sprintf("%s %s %s", filterSpec$columnAlias, filterSpec$operator, filterSpec$value))
  }, character(1L), USE.NAMES = FALSE), collapse = " & "))
}
renderAiInputChangePreviewTable <- function(change, lang) {
  preview <- change$preview
  if (change$isScalar) {
    beforeHeader <- lang$changeProposalBefore
    afterHeader <- lang$changeProposalAfter
  } else {
    beforeHeader <- sprintf("%s (%s)", change$targetColumnAlias, lang$changeProposalBefore)
    afterHeader <- sprintf("%s (%s)", change$targetColumnAlias, lang$changeProposalAfter)
  }
  return(tags$div(
    class = "miro-ai-change-preview",
    tags$table(
      class = "miro-ai-change-table",
      tags$thead(tags$tr(
        lapply(names(preview$labels), function(colName) tags$th(colName)),
        tags$th(class = "miro-ai-change-col-before", beforeHeader),
        tags$th(class = "miro-ai-change-col-after", afterHeader)
      )),
      tags$tbody(lapply(seq_along(preview$before), function(rowId) {
        tags$tr(
          lapply(preview$labels, function(labelCol) tags$td(labelCol[[rowId]])),
          tags$td(class = "miro-ai-change-cell-before", preview$before[[rowId]]),
          tags$td(class = "miro-ai-change-cell-after", preview$after[[rowId]])
        )
      }))
    )
  ))
}
renderAiInputChangeStatus <- function(change, lang) {
  if (identical(change$status, "applied")) {
    return(tags$div(
      class = "miro-ai-change-status miro-ai-change-status-applied",
      icon("circle-check"), lang$changeStatusApplied
    ))
  }
  if (identical(change$status, "discarded")) {
    return(tags$div(
      class = "miro-ai-change-status miro-ai-change-status-discarded",
      icon("circle-xmark"), lang$changeStatusDiscarded
    ))
  }
  if (identical(change$status, "outdated")) {
    return(tags$div(
      class = "miro-ai-change-status miro-ai-change-status-outdated",
      icon("triangle-exclamation"), lang$changeStatusOutdated
    ))
  }
  if (identical(change$status, "failed")) {
    return(tags$div(
      class = "miro-ai-change-status miro-ai-change-status-failed",
      icon("triangle-exclamation"), sprintf(lang$changeStatusFailed, change$statusDetail %||% "")
    ))
  }
  return(tags$div(
    class = "miro-ai-change-actions",
    tags$button(
      type = "button", class = "btn btn-primary btn-sm miro-ai-change-apply",
      onclick = "Miro.resolveAiInputChange(this, true)",
      icon("check"), lang$btApplyChange
    ),
    tags$button(
      type = "button", class = "btn btn-default btn-sm miro-ai-change-discard",
      onclick = "Miro.resolveAiInputChange(this, false)",
      icon("xmark"), lang$btDiscardChange
    )
  ))
}
renderAiInputChangeProposal <- function(change, lang) {
  filterLabel <- aiInputChangeFilterLabel(change)
  return(tags$div(
    class = "miro-ai-chat-bubble-wrapper miro-ai-chat-assistant miro-ai-change-proposal",
    `data-change-id` = change$changeId,
    tags$div(
      class = "miro-ai-chat-bubble miro-ai-change-card",
      tags$b(
        class = "miro-ai-chat-author",
        icon("pen-to-square"), lang$changeProposalHeader
      ),
      if (length(change$reason)) {
        tags$div(class = "miro-ai-change-reason", change$reason)
      },
      if (isTRUE(change$rebased) && identical(change$status, "pending")) {
        tags$div(class = "miro-ai-change-rebased", icon("rotate"), lang$changeRebased)
      },
      tags$dl(
        class = "miro-ai-change-facts",
        tags$dt(lang$changeProposalSymbol), tags$dd(change$symbolAlias),
        if (!change$isScalar) tags$dt(lang$changeProposalColumn),
        if (!change$isScalar) tags$dd(change$targetColumnAlias),
        tags$dt(lang$changeProposalOperation),
        tags$dd(aiInputChangeOperationLabel(change, lang)),
        if (!is.null(filterLabel)) tags$dt(lang$changeProposalFilter),
        if (!is.null(filterLabel)) tags$dd(filterLabel),
        if (!change$isScalar && change$preview$noRows > 0L) tags$dt(lang$changeProposalAffected),
        if (!change$isScalar && change$preview$noRows > 0L) {
          tags$dd(format(change$preview$noRows, big.mark = ","))
        }
      ),
      if (change$preview$noRowsShown > 0L) {
        renderAiInputChangePreviewTable(change, lang)
      },
      if (change$preview$noRowsShown < change$preview$noRows) {
        tags$div(class = "miro-ai-change-truncated", sprintf(
          lang$changeProposalTruncated,
          format(change$preview$noRowsShown, big.mark = ","),
          format(change$preview$noRows, big.mark = ",")
        ))
      },
      renderAiInputChangeStatus(change, lang)
    )
  ))
}
renderAiInputChangeGroupStatus <- function(group, lang) {
  if (identical(group$status, "applied")) {
    return(tags$div(
      class = "miro-ai-change-status miro-ai-change-status-applied",
      icon("circle-check"), lang$changeGroupStatusApplied
    ))
  }
  if (identical(group$status, "retired")) {
    return(tags$div(
      class = "miro-ai-change-status miro-ai-change-status-discarded",
      icon("circle-info"), lang$changeGroupStatusRetired
    ))
  }
  if (identical(group$status, "outdated")) {
    return(tags$div(
      class = "miro-ai-change-status miro-ai-change-status-outdated",
      icon("triangle-exclamation"), lang$changeStatusOutdated
    ))
  }
  if (identical(group$status, "failed")) {
    return(tags$div(
      class = "miro-ai-change-status miro-ai-change-status-failed",
      icon("triangle-exclamation"), sprintf(lang$changeStatusFailed, group$statusDetail %||% "")
    ))
  }
  return(tags$div(
    class = "miro-ai-change-actions",
    tags$button(
      type = "button", class = "btn btn-primary btn-sm miro-ai-change-apply-all",
      onclick = "Miro.resolveAiInputChangeGroup(this)",
      icon("check-double"), sprintf(lang$btApplyAllChanges, length(group$changeIds))
    )
  ))
}
renderAiInputChangeGroup <- function(group, lang) {
  return(tags$div(
    class = "miro-ai-chat-bubble-wrapper miro-ai-chat-assistant miro-ai-change-proposal miro-ai-change-group",
    `data-change-id` = group$groupId,
    tags$div(
      class = "miro-ai-chat-bubble miro-ai-change-card",
      tags$b(
        class = "miro-ai-chat-author",
        icon("list-check"), sprintf(lang$changeGroupHeader, length(group$changeIds))
      ),
      if (isTRUE(group$rebased) && identical(group$status, "pending")) {
        tags$div(class = "miro-ai-change-rebased", icon("rotate"), lang$changeGroupRebased)
      },
      if (isTRUE(group$independent)) {
        tags$div(class = "miro-ai-change-reason", lang$changeGroupIndependent)
      } else {
        tagList(
          tags$div(class = "miro-ai-change-reason", lang$changeGroupCombined),
          lapply(seq_along(group$steps), function(stepIdx) {
            step <- group$steps[[stepIdx]]
            filterLabel <- aiInputChangeFilterLabel(step)
            tags$div(
              class = "miro-ai-change-group-step",
              tags$div(
                class = "miro-ai-change-group-step-title",
                sprintf(
                  "%d. %s%s: %s%s", stepIdx, step$symbolAlias,
                  if (isTRUE(step$isScalar)) "" else sprintf(" (%s)", step$targetColumnAlias),
                  aiInputChangeOperationLabel(step, lang),
                  if (is.null(filterLabel)) "" else sprintf(" [%s]", filterLabel)
                )
              ),
              if (step$preview$noRowsShown > 0L) renderAiInputChangePreviewTable(step, lang),
              if (step$preview$noRowsShown < step$preview$noRows) {
                tags$div(class = "miro-ai-change-truncated", sprintf(
                  lang$changeProposalTruncated,
                  format(step$preview$noRowsShown, big.mark = ","),
                  format(step$preview$noRows, big.mark = ",")
                ))
              }
            )
          })
        )
      },
      renderAiInputChangeGroupStatus(group, lang)
    )
  ))
}
aiToolActivityFilterLabel <- function(activity) {
  if (!length(activity$filters)) {
    return(NULL)
  }
  return(paste(vapply(activity$filters, function(filterSpec) {
    return(sprintf("%s %s %s", filterSpec$column, filterSpec$operator, filterSpec$value))
  }, character(1L), USE.NAMES = FALSE), collapse = " & "))
}
renderAiToolActivity <- function(activity, lang) {
  hasError <- length(activity$error) > 0L
  filterLabel <- aiToolActivityFilterLabel(activity)
  symbolLabel <- activity$symbolAlias %||% activity$symbolName
  statusText <- NULL
  if (hasError) {
    statusText <- lang$toolActivityStatusError
  } else if (length(activity$rowCount)) {
    statusText <- sprintf(lang$toolActivityStatusRows, format(activity$rowCount, big.mark = ","))
  }
  return(tags$div(
    class = "miro-ai-chat-bubble-wrapper miro-ai-chat-assistant miro-ai-tool-activity",
    tags$details(
      class = paste(c("miro-ai-activity-card", if (hasError) "miro-ai-activity-card-error"), collapse = " "),
      tags$summary(
        icon("magnifying-glass"),
        tags$span(class = "miro-ai-activity-symbol", symbolLabel),
        if (length(statusText)) tags$span(class = "miro-ai-activity-status", statusText)
      ),
      tags$dl(
        class = "miro-ai-activity-facts",
        tags$dt(lang$toolActivitySymbol), tags$dd(symbolLabel),
        if (length(activity$scenarioNames)) tags$dt(lang$toolActivityScenarios),
        if (length(activity$scenarioNames)) tags$dd(paste(activity$scenarioNames, collapse = ", ")),
        if (length(activity$aggregation)) tags$dt(lang$toolActivityAggregation),
        if (length(activity$aggregation)) tags$dd(activity$aggregation),
        if (length(activity$analysisMode)) tags$dt(lang$toolActivityAnalysisMode),
        if (length(activity$analysisMode)) tags$dd(activity$analysisMode),
        if (length(activity$groupByColumn)) tags$dt(lang$toolActivityGroupBy),
        if (length(activity$groupByColumn)) tags$dd(paste(activity$groupByColumn, collapse = ", ")),
        if (!is.null(filterLabel)) tags$dt(lang$toolActivityFilter),
        if (!is.null(filterLabel)) tags$dd(filterLabel),
        if (length(activity$orderBy)) tags$dt(lang$toolActivityOrderBy),
        if (length(activity$orderBy)) {
          tags$dd(if (length(activity$sortDirection)) sprintf("%s (%s)", activity$orderBy, activity$sortDirection) else activity$orderBy)
        },
        if (length(activity$limit)) tags$dt(lang$toolActivityLimit),
        if (length(activity$limit)) tags$dd(format(activity$limit, big.mark = ",")),
        if (!hasError && length(activity$rowCount)) tags$dt(lang$toolActivityRows),
        if (!hasError && length(activity$rowCount)) tags$dd(format(activity$rowCount, big.mark = ","))
      ),
      if (isTRUE(activity$truncated) && length(activity$warning)) {
        tags$div(class = "miro-ai-activity-warning", icon("triangle-exclamation"), activity$warning)
      },
      if (hasError) {
        tags$div(class = "miro-ai-activity-error", icon("triangle-exclamation"), activity$error)
      }
    )
  ))
}
renderAiChatItem <- function(item, lang, isActiveCompareContext = FALSE) {
  if (identical(item$role, "inputChange")) {
    return(renderAiInputChangeProposal(item$change, lang))
  }
  if (identical(item$role, "inputChangeGroup")) {
    return(renderAiInputChangeGroup(item$group, lang))
  }
  if (identical(item$role, "toolActivity")) {
    return(renderAiToolActivity(item$activity, lang))
  }
  if (is.null(item$role)) {
    return(renderAiChatDivider(item, if (isActiveCompareContext) lang$backToSandbox))
  }
  return(renderAiChatBubble(item, lang))
}
renderAiTokenInfoContainer <- function(lang, inputTokens = 0L, outputTokens = 0L) {
  tags$div(
    class = "miro-ai-token-info-wrapper",
    tags$span(
      title = lang$inputTokensTitle,
      id = "miroAiInputTokens",
      sprintf(lang$inputTokensLabel, format(inputTokens, big.mark = ","))
    ),
    tags$span(
      title = lang$outputTokensTitle,
      id = "miroAiOutputTokens",
      sprintf(lang$outputTokensLabel, format(outputTokens, big.mark = ","))
    )
  )
}
# Mira's mark: a four-point sparkle with a lens in its center ("mira" = look), a small
# companion star and a dot. Inline SVG, so it takes the text color of its surroundings;
# the parts are animated in CSS (see .mira-icon in miro.less).
renderMiraIcon <- function() {
  HTML(paste0(
    '<svg class="mira-icon" viewBox="0 0 24 24" width="1em" height="1em" aria-hidden="true" focusable="false">',
    '<path class="mira-icon-star" fill-rule="evenodd" d="M10 3.2Q11.9 10.1 18.8 12Q11.9 13.9 10 20.8',
    'Q8.1 13.9 1.2 12Q8.1 10.1 10 3.2ZM10 10.3A1.7 1.7 0 1 0 10 13.7A1.7 1.7 0 1 0 10 10.3Z"/>',
    '<path class="mira-icon-spark" d="M18.5 1.8Q19 5 22.2 5.5Q19 6 18.5 9.2Q18 6 14.8 5.5Q18 5 18.5 1.8Z"/>',
    '<circle class="mira-icon-dot" cx="19.4" cy="18.8" r="1.35"/>',
    "</svg>"
  ))
}
renderAiChatLogContent <- function(chatHistory, lang, contextIsSandbox = TRUE) {
  # when comparing scenarios, the latest context divider offers to go back to the sandbox
  activeContextIdx <- if (!isTRUE(contextIsSandbox)) {
    max(c(0L, which(vapply(chatHistory, function(item) is.null(item$role), logical(1L)))))
  } else {
    0L
  }
  tagList(
    lapply(seq_along(chatHistory), function(itemIdx) {
      renderAiChatItem(chatHistory[[itemIdx]], lang,
        isActiveCompareContext = identical(itemIdx, activeContextIdx)
      )
    }),
    genSpinner("miroAiChatSpinner", hidden = TRUE, absolute = FALSE, externalStyle = "z-index: 1")
  )
}
# Base mode: chat drawer that slides in from the right and overlays the content.
# The chat log is filled by the server whenever the drawer is opened.
renderAiChatDrawer <- function(lang, supportedModels, chartPalette = defaultChartColors) {
  htmltools::attachDependencies(tags$aside(
    id = "miroAiChatDrawer",
    class = "miro-ai-assistant-drawer miro-ai-drawer-overlay",
    role = "dialog",
    `aria-labelledby` = "miroAiChatDrawerTitle",
    `aria-hidden` = "true",
    inert = NA,
    # drag (or focus + arrow keys) to change the width, double-click to reset
    tags$div(
      class = "miro-ai-drawer-resizer",
      role = "separator",
      tabindex = "0",
      `aria-orientation` = "vertical",
      `aria-controls` = "miroAiChatDrawer",
      `aria-label` = lang$resizeHandle,
      title = lang$resizeHandle
    ),
    tags$div(
      class = "miro-ai-assistant-drawer-header",
      tags$div(
        class = "miro-ai-drawer-heading",
        tags$h4(id = "miroAiChatDrawerTitle", class = "miro-ai-drawer-title", lang$title),
        renderAiTokenInfoContainer(lang)
      ),
      tags$div(
        class = "miro-ai-drawer-actions",
        tags$button(
          id = "btMiroAiClearChat", type = "button",
          class = "btn btn-default action-button",
          title = lang$btClear, `aria-label` = lang$btClear,
          icon("trash")
        ),
        tags$button(
          type = "button", class = "btn btn-default miro-ai-close-drawer-btn",
          title = lang$btClose, `aria-label` = lang$btClose,
          onclick = "Miro.hideAiDrawer()",
          icon("xmark")
        )
      )
    ),
    tags$div(
      class = "miro-ai-assistant-drawer-body",
      renderAiChatWindow(lang, supportedModels, chartPalette = chartPalette)
    )
  ), htmlwidgets::getDependency("chartjs", "chartjs"))
}
renderAiChatWindow <- function(lang, supportedModels, chatHistory = NULL, renderApplyCodeBtn = FALSE, selectedModel = NULL,
                               userInputPlaceholderKey = "userTextPlaceholder", chartPalette = defaultChartColors) {
  tagList(
    tags$div(
      id = "miroAiChatLog",
      class = "miro-ai-chat-log",
      `data-activity-group-label` = lang$toolActivityGroupCount,
      `data-copy-label` = lang$btCopy,
      `data-chart-download-label` = lang$btDownloadChart,
      `data-chart-unavailable-label` = lang$chartUnavailable,
      `data-chart-open-label` = lang$btOpenChartInPivot,
      `data-chart-palette` = jsonlite::toJSON(chartPalette),
      lapply(chatHistory, function(item) renderAiChatItem(item, lang)),
      genSpinner("miroAiChatSpinner",
        hidden = TRUE, absolute = FALSE,
        externalStyle = "z-index: 1"
      )
    ),
    tags$div(
      class = "miro-ai-chat-input-wrapper",
      tags$div(
        class = "miro-ai-textarea-row",
        tags$textarea(
          id = "miroAiUserText",
          placeholder = lang[[userInputPlaceholderKey]],
          `data-shiny-no-bind-input` = TRUE,
          class = "form-control miro-ai-textarea-input",
          rows = "1"
        ),
        tags$button(
          id = "btMiroAiSend",
          type = "button",
          class = "miro-ai-send-btn",
          style = "display: none;",
          icon("paper-plane")
        ),
        tags$button(
          id = "btMiroAiStop",
          type = "button",
          class = "miro-ai-stop-btn action-button",
          style = "display: none;",
          icon("stop")
        )
      ),
      tags$div(
        class = "miro-ai-chat-controls",
        tags$div(
          class = "miro-ai-lean-select",
          style = if (renderApplyCodeBtn) "max-width: 160px;",
          tagAppendAttributes(
            selectInput(
              inputId = "miroAiModel",
              label = NULL,
              choices = supportedModels,
              selected = selectedModel,
              width = "100%",
              selectize = TRUE
            ),
            style = "margin-bottom: 0;"
          )
        ),
        if (renderApplyCodeBtn) {
          tags$button(
            id = "btMiroAiApplyCode",
            type = "button",
            class = "btn btn-default action-button miro-ai-apply-btn",
            icon("file-import"),
            lang$btApplyCode
          )
        }
      )
    ),
    tags$div(
      class = "miro-ai-disclaimer",
      lang$disclaimer
    )
  )
}

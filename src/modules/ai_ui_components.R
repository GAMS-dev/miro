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
renderAiChatDivider <- function(contextText) {
  tags$div(
    class = "miro-ai-chat-context-divider",
    contextText
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
renderAiChatItem <- function(item, lang) {
  if (identical(item$role, "inputChange")) {
    return(renderAiInputChangeProposal(item$change, lang))
  }
  if (identical(item$role, "toolActivity")) {
    return(renderAiToolActivity(item$activity, lang))
  }
  if (is.null(item$role)) {
    return(renderAiChatDivider(item))
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
renderAiChatWindow <- function(lang, supportedModels, chatHistory = NULL, renderApplyCodeBtn = FALSE, selectedModel = NULL, userInputPlaceholderKey = "userTextPlaceholder") {
  tagList(
    tags$div(
      id = "miroAiChatLog",
      class = "miro-ai-chat-log",
      `data-activity-group-label` = lang$toolActivityGroupCount,
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
      ),
      if (renderApplyCodeBtn) {
        renderAiTokenInfoContainer(lang)
      }
    ),
    tags$div(
      class = "miro-ai-disclaimer",
      lang$disclaimer
    )
  )
}

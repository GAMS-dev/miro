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
      lapply(chatHistory, function(item) {
        if (is.null(item$role)) {
          return(renderAiChatDivider(item))
        }
        return(renderAiChatBubble(item, lang))
      }),
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

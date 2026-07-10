renderAiChatBubble <- function(msg, lang) {
  isUser <- msg$role == "user"
  wrapperClass <- if (isUser) "chat-bubble-wrapper chat-user" else "chat-bubble-wrapper chat-assistant"
  return(tags$div(
    class = wrapperClass,
    tags$div(
      class = "chat-actions",
      tags$button(
        class = "chat-btn",
        onclick = "Miro.copyChatContent(this)",
        title = lang$btCopy,
        tags$i(class = "fas fa-copy")
      )
    ),
    tags$div(
      class = "chat-bubble",
      tags$b(class = "chat-author", if (isUser) lang$chatBubbleUserHeader else lang$chatBubbleAssistantHeader),
      tags$div(class = "chat-content-html", markdown(msg$text)),
      tags$div(class = "chat-content-raw", style = "display: none;", msg$text)
    )
  ))
}
renderAiChatDivider <- function(contextText) {
  tags$div(
    class = "chat-context-divider",
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
renderAiChatWindow <- function(lang, supportedModels, chatHistory = NULL, renderApplyCodeBtn = FALSE, selectedModel = NULL) {
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
        externalStyle = "z-index: 1", extraClasses = "gen-spinner-black"
      )
    ),
    tags$div(
      class = "miro-ai-chat-input-wrapper",
      tags$textarea(
        id = "miroAiUserText",
        placeholder = lang$userTextPlaceholder,
        `data-shiny-no-bind-input` = TRUE,
        class = "form-control miro-ai-textarea-input",
        rows = "1"
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

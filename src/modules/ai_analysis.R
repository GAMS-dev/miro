aiAssistantState <- list(
  history = list(),
  sessionId = NULL,
  activeSids = NULL,
  inputTokens = 0L,
  outputTokens = 0L,
  chatHistory = list(list(role = "model", text = lang$nav$aiBotDialog$welcomeMessage))
)

observeEvent(input$btAIOutput, {
  flog.debug("AI Output Analysis button clicked.")
  if (!identical(activeScen$getSid(), aiAssistantState$activeSids)) {
    aiAssistantState$activeSids <<- activeScen$getSid()
    aiAssistantState$chatHistory[[length(aiAssistantState$chatHistory) + 1]] <<- list(sprintf(lang$nav$aiBotDialog$currentContextSandbox, activeScen$getScenName()))
  }
  showModal(tagAppendAttributes(modalDialog(
    size = "l",
    easyClose = TRUE,
    footer = tagList(
      modalButton(lang$general$modal$cancelButton),
      actionButton("btMiroAiClearChat", lang$nav$aiBotDialog$btClear,
        class = "btn btn-default bt-remove", style = "float: left;"
      ),
      renderAiTokenInfoContainer(lang$nav$aiBotDialog, aiAssistantState$inputTokens, aiAssistantState$outputTokens)
    ),
    tags$div(
      class = "miro-ai-modal-body",
      renderAiChatWindow(lang$nav$aiBotDialog, config$aiAssistant$supportedModels, aiAssistantState$chatHistory)
    )
  ), class = "ai-assistant-modal"))
})

mAiTask <- NULL
aiTask <- ExtendedTask$new(
  function(userInput, dbConnProvider, dbSchema, ioConfig, assistantConfig, model, allowedSids, history) {
    mAiTask <<- mirai(
      {
        library(R6)
        library(httr)
        library(jsonlite)
        source("global.R")
        source("components/ai_provider.R")
        source("components/ai_adapter.R")

        conn <- dbConnProvider$getConnection()
        on.exit(DBI::dbDisconnect(conn))

        providerAndModel <- strsplit(model, "/", fixed = TRUE)[[1]]

        assistant <- MiroAiAssistant$new(apiKeys = assistantConfig$apiKeys)
        aiAdapter <- AIToolAdapter$new(dbSchema, conn, ioConfig)
        aiProvider <- assistant$getProvider(providerAndModel[1], providerAndModel[2])
        aiAdapter$setAllowedSids(allowedSids)

        toolSchema <- aiAdapter$getToolSchema()
        toolDispatcher <- list(
          "queryScenarioDatabase" = function(args) aiAdapter$queryScenarioDatabase(args)
        )
        devContext <- ""
        if (!is.null(assistantConfig$domainContext)) {
          devContext <- sprintf("### DOMAIN CONTEXT\n%s\n\n", assistantConfig$domainContext)
        }
        devInstructions <- ""
        if (!is.null(assistantConfig$customInstructions)) {
          bullets <- paste(paste("-", assistantConfig$customInstructions), collapse = "\n")
          devInstructions <- sprintf("### CUSTOM INSTRUCTIONS\n%s\n\n", bullets)
        }
        schemaContext <- sprintf(
          "### DATABASE SCHEMA\nYou have access to a backend database containing the following structural layout configurations:\n%s\n\n",
          aiAdapter$getSystemMetadata()
        )
        guardrails <- paste(
          "### SYSTEM RULES",
          "You are an expert data analyst assistant for a UI of an optimization model.",
          "When a user requests a general report or summary:",
          "1. Prioritize querying the optimization results first, focusing on high-level 'output' symbols.",
          "2. Only query detailed 'input parameters' if needed to explain underlying bottlenecks.",
          "3. Do not sequentially scan or crawl every table.",
          "You MUST use the provided `queryScenarioDatabase` tool to look up calculations. Do not guess numbers.",
          "Do not expose the scenario ID (`_sid` column). It is only intended for internal use.",
          sep = "\n"
        )
        if (length(history$sessionId)) {
          aiProvider$sessionId <- history$sessionId
        }
        if (length(history$history)) {
          aiProvider$history <- history$history
        }

        result <- aiProvider$sendMessage(
          userInput = userInput,
          systemInstructions = paste0(devContext, devInstructions, schemaContext, guardrails),
          tools = toolSchema,
          toolDispatcher = toolDispatcher
        )
        return(list(
          text = result$text,
          inputTokens = result$inputTokens,
          outputTokens = result$outputTokens,
          newSessionId = aiProvider$sessionId,
          newHistory = aiProvider$history
        ))
      },
      userInput = userInput,
      dbConnProvider = dbConnProvider,
      dbSchema = dbSchema,
      ioConfig = ioConfig,
      assistantConfig = assistantConfig,
      model = model,
      allowedSids = allowedSids,
      history = history
    )
  }
)

observeEvent(input$miroAiUserText, {
  flog.debug("AI Chat message sent.")
  userText <- input$miroAiUserText
  req(userText)
  aiAssistantState$chatHistory[[length(aiAssistantState$chatHistory) + 1]] <<- list(role = "user", text = userText)
  session$sendCustomMessage(
    "gms-addAiChatBubble",
    list(html = as.character(renderAiChatBubble(
      list(role = "user", text = userText),
      lang$nav$aiBotDialog
    )), role = "user")
  )
  aiTask$invoke(
    userInput = userText,
    dbConnProvider = dbConnProvider,
    dbSchema = dbSchema,
    ioConfig = ioConfig,
    assistantConfig = config$aiAssistant,
    model = input$miroAiModel,
    allowedSids = aiAssistantState$activeSids,
    history = list(sessionId = aiAssistantState$sessionId, history = aiAssistantState$history)
  )
})
observe({
  aiOutput <- ""
  result <- NULL
  if (tryCatch(
    {
      result <- aiTask$result()
      aiOutput <- result$text
      FALSE
    },
    shiny.silent.error = function(err) {
      return(TRUE)
    },
    error = function(err) {
      flog.error("Problems generating AI response. Error message: %s", conditionMessage(err))
      aiOutput <<- sprintf("### Error\nFailed to calculate response: %s", err$message)
      return(FALSE)
    }
  )) {
    return()
  }
  aiAssistantState$chatHistory[[length(aiAssistantState$chatHistory) + 1]] <<- list(role = "model", text = aiOutput)
  session$sendCustomMessage(
    "gms-addAiChatBubble",
    list(html = as.character(renderAiChatBubble(
      list(role = "model", text = aiOutput),
      lang$nav$aiBotDialog
    )), role = "model")
  )
  if (!is.null(result)) {
    aiAssistantState$history <<- result$newHistory
    aiAssistantState$sessionId <<- result$newSessionId
    aiAssistantState$inputTokens <<- aiAssistantState$inputTokens + result$inputTokens
    aiAssistantState$outputTokens <<- aiAssistantState$outputTokens + result$outputTokens
    setTextContent(session, "#miroAiInputTokens", sprintf(
      lang$nav$aiBotDialog$inputTokensLabel,
      format(aiAssistantState$outputTokens, big.mark = ",")
    ))
    setTextContent(session, "#miroAiOutputTokens", sprintf(
      lang$nav$aiBotDialog$outputTokensLabel,
      format(aiAssistantState$inputTokens, big.mark = ",")
    ))
  }
})
observeEvent(input$btMiroAiClearChat, {
  flog.debug("Clear AI chat button clicked.")
  aiAssistantState$chatHistory <<- aiAssistantState$chatHistory[1:2]
  setContent(session, "#miroAiChatLog", as.character(tagList(
    lapply(aiAssistantState$chatHistory, function(item) {
      if (is.null(item$role)) {
        return(renderAiChatDivider(item))
      }
      return(renderAiChatBubble(item, lang$nav$aiBotDialog))
    }),
    genSpinner("miroAiChatSpinner", hidden = TRUE, absolute = FALSE, extraClasses = "gen-spinner-black")
  )))
  aiAssistantState$history <<- list()
  aiAssistantState$sessionId <<- NULL
})
observeEvent(input$abortAiCall, {
  flog.debug("AI Assistant dialog closed")
  stop_mirai(mAiTask)
})

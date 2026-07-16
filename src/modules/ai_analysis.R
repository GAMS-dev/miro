observe({
  dialogState <- aiAssistantDialogState()
  isolate({
    if (is.null(dialogState$sidsToLoad)) {
      return()
    }
    newSids <- unique(sort(dialogState$sidsToLoad))
    if (!identical(newSids, aiAssistantState$activeSids)) {
      aiAssistantState$activeSids <<- newSids
      # append if last history item is not a context item, else overwrite
      historyIdx <- length(aiAssistantState$chatHistory)
      if (!is.null(aiAssistantState$chatHistory[[historyIdx]]$role)) {
        historyIdx <- historyIdx + 1L
      }
      aiAssistantState$chatHistory[[historyIdx]] <<- list(dialogState$chatContext)
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
        renderAiChatWindow(lang$nav$aiBotDialog, config$aiAssistant$supportedModels, aiAssistantState$chatHistory, selectedModel = aiAssistantState$model)
      )
    ), class = "ai-assistant-modal"))
  })
})

observeEvent(input$btAIOutput, {
  flog.debug("AI Output Analysis button clicked.")
  tempScenName <- lang$nav$dialogNewScen$newScenName
  tempScenTags <- NULL
  sidToLoad <- NULL
  if (!is.null(activeScen)) {
    if (isFALSE(rv$sandboxUnsaved)) {
      sidToLoad <- activeScen$getSid()
    }
    tempScenName <- activeScen$getScenName()
    tempScenTags <- activeScen$getStags()
  }
  if (is.null(sidToLoad)) {
    aiAssistantState$tempScen <<- tryCatch(
      {
        saveScenDumpTempToDb(tempScenName, tempScenTags)
      },
      error = function(err) {
        flog.error("Unable to open AI Assistant because sandbox scenario could not be dumped to database. Error message: %s", conditionMessage(err))
        return(NULL)
      }
    )
    if (is.null(aiAssistantState$tempScen)) {
      return()
    }
    sidToLoad <- aiAssistantState$tempScen$getSid()
  }
  aiAssistantDialogState(list(
    sidsToLoad = sidToLoad, scenIdNameMap = NULL,
    chatContext = sprintf(lang$nav$aiBotDialog$currentContextSandbox, tempScenName), timestamp = Sys.time()
  ))
})

mAiTask <- NULL
aiTask <- ExtendedTask$new(
  function(userInput, dbConnProvider, dbSchema, ioConfig, assistantConfig, model, allowedSids, scenIdNameMap, history) {
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
        if (length(scenIdNameMap)) {
          aiAdapter$setScenIdNameMap(scenIdNameMap)
        }
        aiProvider <- assistant$getProvider(providerAndModel[1], providerAndModel[2])
        aiAdapter$setAllowedSids(allowedSids)

        toolSchema <- aiAdapter$getToolSchema()
        toolDispatcher <- list(
          "queryScenarioDatabase" = function(args) aiAdapter$queryScenarioDatabase(args)
        )
        roleContext <- "### ROLE
Expert optimization analyst for a GAMS MIRO app. Answer scenario questions using the scenario database: precise, concise, quantitative, directly on-point. No unneeded explanation.
"

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
          "### DATABASE SCHEMA\nBackend database structure:\n%s\n\n",
          aiAdapter$getSystemMetadata()
        )

        guardrails <- "### TOOL USAGE
The database is the sole source of truth. Before calling `queryScenarioDatabase`, plan all needed info and fetch it in as few, batched queries as possible—no exploratory querying, full scans, or extra rows/columns. Never infer, estimate, or fabricate values obtainable from the database.

### ANALYSIS
1. **Outputs first, inputs to verify:** Lead reports with outputs (KPIs, objectives, violations, aggregates). To explain *why* an output changed, you MUST query the relevant `direction: \"input\"` data for the exact cause—never invent a narrative.
2. **Scalars matter:** Output scalars (`isScalar: true`) give quick KPIs; input scalars often hold the key config/assumption changes.
3. **Data quality:** Watch for missing values, bad units, outliers, duplicates, infeasibilities, rule violations. Summarize patterns, don't list every issue unless asked.
4. **Ambiguity:** Make the smallest reasonable assumption rather than pulling extra data.
5. **User levers:** Use `widget` metadata (type, min, max, readonly) to distinguish active decision variables from fixed parameters; respect min/max bounds for feasibility/sensitivity questions.

### REPORTING
Answer directly—concise, quantitative, prioritized. Synthesize into conclusions, don't just restate query output. Separate observation from interpretation. If data is insufficient, say what's missing; don't speculate.

### SCENARIO RULES
1. Use exact scenario names (`_scenario_name`, `_target_scenario`, `_baseline_scenario`)—never generic labels like \"Scenario 1\" or \"Base case\".
2. Single scenario available: report its results directly, without noting the lack of comparisons."
        if (length(history$sessionId)) {
          aiProvider$sessionId <- history$sessionId
        }
        if (length(history$history)) {
          aiProvider$history <- history$history
        }
        if (length(history$chatHistory)) {
          aiProvider$chatHistory <- history$chatHistory
        }

        result <- aiProvider$sendMessage(
          userInput = userInput,
          systemInstructions = paste0(roleContext, devContext, devInstructions, schemaContext, guardrails),
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
      scenIdNameMap = scenIdNameMap,
      history = history
    )
  }
)

observeEvent(input$miroAiUserText, {
  flog.debug("AI Chat message sent.")
  userText <- input$miroAiUserText
  req(userText)
  if (unresolved(mAiTask)) {
    flog.error("User sent new chat message while old response was not complete. Should never happen!")
    return()
  }
  aiAssistantState$chatHistory[[length(aiAssistantState$chatHistory) + 1]] <<- list(role = "user", text = userText)
  chatHistory <- NULL
  if (!identical(aiAssistantState$model, input$miroAiModel)) {
    if (!is.null(aiAssistantState$model) && !identical(dirname(aiAssistantState$model), dirname(input$miroAiModel))) {
      # switch from one provider to another -> need to pass state as chat history
      chatHistory <- aiAssistantState$chatHistory[lapply(aiAssistantState$chatHistory, length) > 1L][-1]
      aiAssistantState$history <<- list()
      aiAssistantState$sessionId <<- NULL
    }
    aiAssistantState$model <<- input$miroAiModel
  }
  session$sendCustomMessage(
    "gms-addAiChatBubble",
    list(html = as.character(renderAiChatBubble(
      list(role = "user", text = userText),
      lang$nav$aiBotDialog
    )), role = "user")
  )
  dialogState <- aiAssistantDialogState()
  aiTask$invoke(
    userInput = userText,
    dbConnProvider = dbConnProvider,
    dbSchema = dbSchema,
    ioConfig = ioConfig,
    assistantConfig = config$aiAssistant,
    model = aiAssistantState$model,
    allowedSids = aiAssistantState$activeSids,
    scenIdNameMap = dialogState$scenIdNameMap,
    history = list(sessionId = aiAssistantState$sessionId, history = aiAssistantState$history, chatHistory = chatHistory)
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
      format(aiAssistantState$inputTokens, big.mark = ",")
    ))
    setTextContent(session, "#miroAiOutputTokens", sprintf(
      lang$nav$aiBotDialog$outputTokensLabel,
      format(aiAssistantState$outputTokens, big.mark = ",")
    ))
  }
})
observeEvent(input$btMiroAiClearChat, {
  flog.debug("Clear AI chat button clicked.")
  stop_mirai(mAiTask)
  aiAssistantState$chatHistory <<- list(aiAssistantState$chatHistory[[1]], list(aiAssistantDialogState()$chatContext))
  setContent(session, "#miroAiChatLog", as.character(tagList(
    lapply(aiAssistantState$chatHistory, function(item) {
      if (is.null(item$role)) {
        return(renderAiChatDivider(item))
      }
      return(renderAiChatBubble(item, lang$nav$aiBotDialog))
    }),
    genSpinner("miroAiChatSpinner", hidden = TRUE, absolute = FALSE, externalStyle = "z-index: 1")
  )), sanitizeHTML = TRUE)
  aiAssistantState$history <<- list()
  aiAssistantState$sessionId <<- NULL
  aiAssistantState$inputTokens <<- 0L
  aiAssistantState$outputTokens <<- 0L
})
observeEvent(input$btMiroAiStop, {
  flog.debug("AI Assistant stop button clicked.")
  stop_mirai(mAiTask)
})
observeEvent(input$abortAiCall, {
  flog.debug("AI Assistant dialog closed")
  stop_mirai(mAiTask)
  if (!is.null(aiAssistantState$tempScen)) {
    tryCatch(
      {
        aiAssistantState$tempScen$delete()$destroy()
      },
      error = function(err) {
        flog.warn("Problems removing temporary scenario. Error message: %s", conditionMessage(err))
      }
    )
    aiAssistantState$tempScen <<- NULL
  }
})

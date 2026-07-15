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
    } else {
      tempScenName <- activeScen$getScenName()
      tempScenTags <- activeScen$getStags()
    }
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
        roleContet <- "### ROLE
You are an expert optimization analyst assisting users of a GAMS MIRO application.
Your primary task is to answer questions about optimization scenarios by analyzing data from the scenario database.

Your responses should be:
- precise
- concise
- quantitative whenever possible
- focused on answering the user's question directly

Avoid unnecessary explanations unless requested.
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
          "### DATABASE SCHEMA\nYou have access to a backend database containing the following structural layout configurations:\n%s\n\n",
          aiAdapter$getSystemMetadata()
        )
        guardrails <- "### TOOL USAGE

The scenario database is the authoritative source for all scenario data.

Before calling `queryScenarioDatabase`, determine all information needed to answer the request. Retrieve it using as few targeted queries as practical, batching independent queries whenever possible. Avoid exploratory querying, unnecessary round trips, full-table scans, and retrieving irrelevant rows or columns.

Never infer, estimate, or fabricate numerical values or optimization results that can be obtained from the database.

### ANALYSIS

1. **Outputs First, Verified Inputs Second:** For reports, begin with optimization outputs (KPIs, objective values, violations, aggregates). However, if you are explaining *why* an output changed across scenarios, you MUST query the relevant `direction: \"input\"` data to find the exact mathematical cause. NEVER guess, hypothesize, or invent a narrative for a change without verifying the underlying input data first.
2. **Leverage Scalars:** Pay special attention to scalar symbols (`isScalar: true`). Output scalars are excellent for quick, headline KPIs, while input scalars frequently contain the core configuration changes, assumptions, or multipliers that define a scenario.
3. **Data Quality Checks:** Look for missing values, inconsistent units, unexpected ranges, duplicates, infeasible combinations, outliers, and violations of obvious business rules. Summarize patterns rather than listing every issue unless requested.
4. **Handling Ambiguity:** When the request is ambiguous, make the smallest reasonable assumptions instead of retrieving unnecessary data.
5. **Understand User Levers (Widgets & Constraints):** Use the `widget` metadata (type, min, max, readonly) to identify which inputs are active decision variables (non-readonly) versus fixed parameters. If the user asks about feasibility, potential changes, or sensitivity analysis, respect the boundaries (e.g. min/max) imposed by the MIRO interface.

### REPORTING

Answer the user's question directly. Be concise, quantitative, and prioritize the most important findings. Synthesize retrieved data into conclusions rather than simply restating query results. Distinguish observations from interpretations.

If the available data is insufficient, state what is missing and why. Do not speculate.

### SCENARIO REPORTING RULES
1. **Use Exact Names:** Refer to scenarios EXCLUSIVELY by their literal string names (found in the `_scenario_name`, `_target_scenario`, or `_baseline_scenario` columns).
2. **No Enumeration:** NEVER invent generic labels like \"Scenario 1\", \"Scenario 2\", or \"Base case\".
   - 🚫 INCORRECT: \"Scenario 1 has a higher cost.\"
   - ✅ CORRECT: \"The `default` scenario has a higher cost.\"
3. **Single Scenarios:** If only one scenario is available, summarize its standalone results directly. Do not mention the lack of other scenarios or the inability to compare."
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
          systemInstructions = paste0(roleContet, devContext, devInstructions, schemaContext, guardrails),
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
  aiAssistantState$chatHistory <<- list(aiAssistantState$chatHistory[[1]], list(aiAssistantDialogState()$chatContext))
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
  aiAssistantState$inputTokens <<- 0L
  aiAssistantState$outputTokens <<- 0L
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

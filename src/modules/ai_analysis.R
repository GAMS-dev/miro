observe({
  dialogState <- aiAssistantDialogState()
  isolate({
    if (is.null(dialogState$sidsToLoad)) {
      return()
    }
    newSids <- unique(sort(dialogState$sidsToLoad))
    newIsSandbox <- isTRUE(dialogState$isSandbox)
    if (!identical(newSids, aiAssistantState$activeSids) ||
      !identical(newIsSandbox, aiAssistantState$contextIsSandbox)) {
      if (!is.null(aiAssistantState$activeSids)) {
        aiAssistantState$pendingContextNote <<- dialogState$chatContext
      }
      aiAssistantState$activeSids <<- newSids
      aiAssistantState$contextIsSandbox <<- newIsSandbox
      if (!newIsSandbox) {
        for (changeIdx in aiAssistantState$changeIndex) {
          if (identical(aiAssistantState$chatHistory[[changeIdx]]$change$status, "pending")) {
            updateAiInputChangeCard(changeIdx, "outdated")
          }
        }
      }
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
    sidsToLoad = sidToLoad, scenIdNameMap = NULL, isSandbox = TRUE,
    chatContext = sprintf(lang$nav$aiBotDialog$currentContextSandbox, tempScenName), timestamp = Sys.time()
  ))
})

# Input data can only be changed where the user could change it by hand anyway.
aiInputModificationEnabled <- !isTRUE(config$activateModules$readonlyMode)

aiCanModifyInput <- function() {
  return(aiInputModificationEnabled && isTRUE(aiAssistantState$contextIsSandbox))
}

mAiTask <- NULL
# set whenever we cancel the running mirai ourselves, so that the observer below can
# tell a deliberate abort apart from a genuine failure
aiTaskCancelled <- FALSE
aiTaskRunning <- reactiveVal(FALSE)
mAiActivityFile <- NULL
aiActivityCursor <- 0L
cancelAiTask <- function() {
  aiTaskCancelled <<- TRUE
  aiTaskRunning(FALSE)
  if (length(mAiActivityFile)) {
    unlink(mAiActivityFile)
  }
  stop_mirai(mAiTask)
}
flushAiActivity <- function() {
  if (!length(mAiActivityFile) || !file.exists(mAiActivityFile)) {
    return(invisible())
  }
  entries <- tryCatch(jsonlite::fromJSON(mAiActivityFile, simplifyVector = FALSE), error = function(err) NULL)
  if (is.null(entries) || length(entries) <= aiActivityCursor) {
    return(invisible())
  }
  newEntries <- entries[(aiActivityCursor + 1L):length(entries)]
  aiActivityCursor <<- length(entries)
  for (activity in newEntries) {
    aiAssistantState$chatHistory[[length(aiAssistantState$chatHistory) + 1L]] <<- list(role = "toolActivity", activity = activity)
    session$sendCustomMessage("gms-addAiChatCard", list(
      html = as.character(renderAiToolActivity(activity, lang$nav$aiBotDialog))
    ))
  }
  return(invisible())
}
observe({
  if (!aiTaskRunning()) {
    return()
  }
  invalidateLater(750, session)
  isolate(flushAiActivity())
})
aiTask <- ExtendedTask$new(
  function(userInput, dbConnProvider, dbSchema, ioConfig, assistantConfig, model, allowedSids, scenIdNameMap, history, allowInputModification, activityFile) {
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
        aiAdapter <- AIToolAdapter$new(dbSchema, conn, ioConfig,
          inputModificationEnabled = allowInputModification
        )
        if (length(scenIdNameMap)) {
          aiAdapter$setScenIdNameMap(scenIdNameMap)
        }
        aiProvider <- assistant$getProvider(providerAndModel[1], providerAndModel[2])

        activityLog <- list()
        writeActivitySnapshot <- function() {
          tmpFile <- paste0(activityFile, ".tmp")
          tryCatch(
            {
              writeLines(jsonlite::toJSON(activityLog, auto_unbox = TRUE), tmpFile)
              file.rename(tmpFile, activityFile)
            },
            error = function(err) NULL
          )
        }
        aiAdapter$setAllowedSids(allowedSids)

        toolSchema <- aiAdapter$getToolSchema()
        toolDispatcher <- list(
          "queryScenarioDatabase" = function(args) {
            dbRes <- aiAdapter$queryScenarioDatabase(args)
            activity <- tryCatch(aiAdapter$describeToolCall("queryScenarioDatabase", args, dbRes), error = function(err) NULL)
            if (!is.null(activity)) {
              activityLog[[length(activityLog) + 1L]] <<- activity
              writeActivitySnapshot()
            }
            dbRes
          },
          "modifySandboxInputData" = function(args) aiAdapter$modifySandboxInputData(args)
        )
        roleContext <- "### ROLE
Expert optimization analyst for a GAMS MIRO app. Answer scenario questions using the scenario database: precise, concise, quantitative, directly on-point. No unneeded explanation.
"

        devContext <- ""
        domainContext <- trimws(assistantConfig$domainContext %||% "")
        if (nchar(domainContext)) {
          devContext <- sprintf("### DOMAIN CONTEXT\n%s\n\n", domainContext)
        }

        devInstructions <- ""
        # the schema allows a single string or an array of instructions
        customInstructions <- trimws(as.character(unlist(
          assistantConfig$customInstructions %||% character(0L),
          use.names = FALSE
        )))
        customInstructions <- customInstructions[nchar(customInstructions) > 0L]
        if (length(customInstructions)) {
          bullets <- paste(paste("-", customInstructions), collapse = "\n")
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
Answer directly—concise, quantitative, prioritized. Synthesize into conclusions, don't just restate query output. Separate observation from interpretation. If data is insufficient, say what's missing; don't speculate. For math, use `\\(...\\)` (inline) or `\\[...\\]` (display)—never `$`/`$$`.

### SCENARIO RULES
1. Use exact scenario names (`_scenario_name`, `_target_scenario`, `_baseline_scenario`)—never generic labels like \"Scenario 1\" or \"Base case\".
2. Single scenario available: report its results directly, without noting the lack of comparisons."
        if (allowInputModification) {
          guardrails <- paste0(guardrails, "

### INPUT DATA CHANGES
`modifySandboxInputData` only *proposes* a change; the user confirms each one on a card showing the exact before/after values.
1. **On request only** -- never change data to \"fix\" something you spotted; report it instead.
2. **Stay within `modifiable`** and any `widget` min/max. `kind: \"numeric\"`: `value` plus one column from `modifiable.columns`, one call per column. `kind: \"choice\"`: SET with `textValue` from `widget.choices` -- no `value`, no `filters`.
3. **Scope it** -- without `filters` the whole column changes; say which rows you are affecting.
4. **Prefer MULTIPLY/DIVIDE/ADD/SUBTRACT** over SET, which throws the user's own numbers away.
5. **Never claim it happened** -- say what you proposed and that it needs confirming. Do not re-query to verify: the database holds the old values until the user confirms.")
        }
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
          newHistory = aiProvider$history,
          pendingInputChanges = aiAdapter$getPendingInputChanges()
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
      history = history,
      allowInputModification = allowInputModification,
      activityFile = activityFile
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
      # only the user/model bubbles carry over; context dividers and input
      # change proposals have no counterpart in a provider's chat history
      chatHistory <- Filter(function(item) length(item$text) > 0L, aiAssistantState$chatHistory)[-1]
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
  modelInput <- userText
  notes <- character(0L)
  if (!is.null(aiAssistantState$pendingContextNote)) {
    notes <- c(notes, sprintf(
      "Context switch: %s. This is now the active data context - treat it as authoritative and don't conflate it with figures or conclusions from earlier in this conversation.",
      aiAssistantState$pendingContextNote
    ))
    aiAssistantState$pendingContextNote <<- NULL
  }
  if (length(aiAssistantState$changeNotes)) {
    notes <- c(notes, sprintf(
      "The user reviewed your proposed input data changes: %s. The sandbox scenario in the database now reflects the applied ones, so re-query it instead of relying on earlier figures.",
      paste(aiAssistantState$changeNotes, collapse = "; ")
    ))
    aiAssistantState$changeNotes <<- character(0L)
  }
  if (length(notes)) {
    modelInput <- sprintf("[%s]\n\n%s", paste(notes, collapse = " "), userText)
  }
  aiTaskCancelled <<- FALSE
  if (length(mAiActivityFile)) {
    unlink(mAiActivityFile)
  }
  mAiActivityFile <<- tempfile(pattern = "miro_ai_activity_", fileext = ".json")
  aiActivityCursor <<- 0L
  aiTaskRunning(TRUE)
  aiTask$invoke(
    userInput = modelInput,
    dbConnProvider = dbConnProvider,
    dbSchema = dbSchema,
    ioConfig = ioConfig,
    assistantConfig = config$aiAssistant,
    model = aiAssistantState$model,
    allowedSids = aiAssistantState$activeSids,
    scenIdNameMap = dialogState$scenIdNameMap,
    history = list(sessionId = aiAssistantState$sessionId, history = aiAssistantState$history, chatHistory = chatHistory),
    allowInputModification = aiCanModifyInput(),
    activityFile = mAiActivityFile
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
      if (aiTaskCancelled) {
        # the request was aborted on purpose (stop button, chat cleared, dialog closed);
        # the UI was already updated by the handler that cancelled it
        return(TRUE)
      }
      flog.error("Problems generating AI response. Error message: %s", conditionMessage(err))
      aiOutput <<- sprintf("### Error\nFailed to calculate response: %s", err$message)
      return(FALSE)
    }
  )) {
    return()
  }
  aiTaskRunning(FALSE)
  # catch any activity written between the last poll and completion, so it
  # is never missing from the chat even though the request already finished
  flushAiActivity()
  aiAssistantState$chatHistory[[length(aiAssistantState$chatHistory) + 1]] <<- list(role = "model", text = aiOutput)
  session$sendCustomMessage(
    "gms-addAiChatBubble",
    list(html = as.character(renderAiChatBubble(
      list(role = "model", text = aiOutput),
      lang$nav$aiBotDialog
    )), role = "model")
  )
  if (!is.null(result)) {
    if (length(result$pendingInputChanges)) {
      registerAiInputChanges(result$pendingInputChanges)
    }
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
updateAiInputChangeCard <- function(historyIdx, status, statusDetail = NULL) {
  aiAssistantState$chatHistory[[historyIdx]]$change$status <<- status
  aiAssistantState$chatHistory[[historyIdx]]$change$statusDetail <<- statusDetail
  change <- aiAssistantState$chatHistory[[historyIdx]]$change
  session$sendCustomMessage("gms-updateAiChatCard", list(
    id = change$changeId,
    html = as.character(renderAiInputChangeProposal(change, lang$nav$aiBotDialog))
  ))
  return(invisible())
}
aiInputChangeSummary <- function(change) {
  return(sprintf(
    "%s (%s %s)", change$symbolName,
    tolower(change$operation), formatAiInputValue(change$value)
  ))
}
#' Values a single-choice widget currently accepts, as the widget itself sees
#' them (a dependent dropdown's choices follow another symbol's data).
aiInputChangeChoices <- function(symName) {
  return(tryCatch(unname(as.character(sandboxInputData$getWidget(symName)$getChoices())),
    error = function(err) {
      flog.warn(
        "Could not determine choices of symbol: '%s'. Error message: %s",
        symName, conditionMessage(err)
      )
      return(character(0L))
    }
  ))
}
#' Take the changes the assistant queued during the last request and turn each
#' one into a confirmation card. The preview is recomputed against the *live*
#' sandbox data (the assistant works on a database dump that can be a few
#' seconds old), so what the card shows is what applying it would produce.
registerAiInputChanges <- function(pendingChanges) {
  for (change in pendingChanges) {
    while (change$changeId %in% names(aiAssistantState$changeIndex)) {
      change$changeId <- paste0(change$changeId, "_")
    }
    change$status <- "pending"
    currentData <- tryCatch(isolate(sandboxInputData$getData(change$symbolName)()),
      error = function(err) err
    )
    changeResult <- NULL
    if (!inherits(currentData, "error")) {
      changeResult <- tryCatch(applyAiInputChange(currentData, change), error = function(err) err)
    }
    if (inherits(currentData, "error") || inherits(changeResult, "error")) {
      errMsg <- conditionMessage(if (inherits(currentData, "error")) currentData else changeResult)
      flog.warn(
        "AI assistant proposed a change to symbol: '%s' that could not be previewed. Error message: %s",
        change$symbolName, errMsg
      )
      change$status <- "failed"
      change$statusDetail <- errMsg
      change$preview <- list(labels = NULL, before = character(0L), after = character(0L), noRows = 0L, noRowsShown = 0L)
    } else if (!change$isScalar && !any(changeResult$rowMask)) {
      change$status <- "failed"
      change$statusDetail <- lang$nav$aiBotDialog$changeErrNoRows
      change$preview <- list(labels = NULL, before = character(0L), after = character(0L), noRows = 0L, noRowsShown = 0L)
    } else if (isTRUE(change$isChoice) && !change$value %in% aiInputChangeChoices(change$symbolName)) {
      change$status <- "failed"
      change$statusDetail <- sprintf(lang$nav$aiBotDialog$changeErrBadChoice, change$value)
      change$preview <- list(labels = NULL, before = character(0L), after = character(0L), noRows = 0L, noRowsShown = 0L)
    } else {
      change$preview <- buildAiInputChangePreview(currentData, change, changeResult)
      change$dataHash <- digest::digest(currentData, algo = "sha1")
    }
    if (identical(change$status, "failed")) {
      aiAssistantState$changeNotes <<- c(
        aiAssistantState$changeNotes,
        sprintf("REJECTED %s: %s", aiInputChangeSummary(change), change$statusDetail)
      )
    }
    historyIdx <- length(aiAssistantState$chatHistory) + 1L
    aiAssistantState$chatHistory[[historyIdx]] <<- list(role = "inputChange", change = change)
    aiAssistantState$changeIndex[[change$changeId]] <<- historyIdx
    session$sendCustomMessage("gms-addAiChatCard", list(
      html = as.character(renderAiInputChangeProposal(change, lang$nav$aiBotDialog))
    ))
  }
  return(invisible())
}
refreshAiSandboxSnapshot <- function() {
  tempScenName <- lang$nav$dialogNewScen$newScenName
  tempScenTags <- NULL
  if (!is.null(activeScen)) {
    tempScenName <- activeScen$getScenName()
    tempScenTags <- activeScen$getStags()
  }
  tempScen <- tryCatch(saveScenDumpTempToDb(tempScenName, tempScenTags),
    error = function(err) {
      flog.error(
        "Could not refresh the AI assistant sandbox snapshot after applying an input change. Error message: %s",
        conditionMessage(err)
      )
      return(NULL)
    }
  )
  if (is.null(tempScen)) {
    return(invisible())
  }
  aiAssistantState$tempScen <<- tempScen
  aiAssistantState$activeSids <<- tempScen$getSid()
  return(invisible())
}

observeEvent(input$miroAiInputChangeAction, {
  changeAction <- input$miroAiInputChangeAction
  if (length(changeAction$id) != 1L ||
    !changeAction$id %in% names(aiAssistantState$changeIndex)) {
    flog.warn("Received AI input change action for unknown change. Ignoring.")
    return()
  }
  historyIdx <- aiAssistantState$changeIndex[[changeAction$id]]
  change <- aiAssistantState$chatHistory[[historyIdx]]$change
  if (!identical(change$status, "pending")) {
    return()
  }
  if (!isTRUE(changeAction$apply)) {
    flog.debug("AI input data change discarded by user (symbol: '%s').", change$symbolName)
    updateAiInputChangeCard(historyIdx, "discarded")
    aiAssistantState$changeNotes <<- c(
      aiAssistantState$changeNotes,
      sprintf("DISCARDED %s", aiInputChangeSummary(change))
    )
    return()
  }
  if (!aiCanModifyInput()) {
    # belt and braces: the card is retired on a context switch, so getting here
    # means one survived it (a stale client, a second browser tab)
    flog.info("Refusing to apply AI proposed input data change outside the sandbox context.")
    updateAiInputChangeCard(historyIdx, "outdated")
    return()
  }
  currentData <- tryCatch(isolate(sandboxInputData$getData(change$symbolName)()), error = function(err) err)
  if (inherits(currentData, "error") ||
    !identical(digest::digest(currentData, algo = "sha1"), change$dataHash)) {
    flog.info("Input data of symbol: '%s' changed since the AI assistant proposed a change to it.", change$symbolName)
    updateAiInputChangeCard(historyIdx, "outdated")
    return()
  }
  newData <- tryCatch(applyAiInputChange(currentData, change)$data, error = function(err) err)
  if (inherits(newData, "error")) {
    flog.error(
      "Could not apply AI proposed change to symbol: '%s'. Error message: %s",
      change$symbolName, conditionMessage(newData)
    )
    updateAiInputChangeCard(historyIdx, "failed", conditionMessage(newData))
    return()
  }
  flog.info("Applying AI proposed input data change to symbol: '%s'.", change$symbolName)
  sandboxInputData$setData(change$symbolName, newData)
  # setData deliberately does not flag the scenario as modified (it is also used
  # to load scenarios), so the sandbox has to be marked unsaved explicitly
  markUnsaved(markDirty = TRUE)
  updateAiInputChangeCard(historyIdx, "applied")
  aiAssistantState$changeNotes <<- c(
    aiAssistantState$changeNotes,
    sprintf("APPLIED %s", aiInputChangeSummary(change))
  )
  # any other proposal for the same symbol was previewed against data that no
  # longer exists, so it can no longer be confirmed as shown
  for (otherIdx in aiAssistantState$changeIndex) {
    if (identical(otherIdx, historyIdx)) {
      next
    }
    otherChange <- aiAssistantState$chatHistory[[otherIdx]]$change
    if (identical(otherChange$status, "pending") &&
      identical(otherChange$symbolName, change$symbolName)) {
      updateAiInputChangeCard(otherIdx, "outdated")
    }
  }
  refreshAiSandboxSnapshot()
  showNotification(sprintf(
    lang$nav$aiBotDialog$nfChangeApplied,
    change$symbolAlias
  ), type = "message")
})

observeEvent(input$btMiroAiClearChat, {
  flog.debug("Clear AI chat button clicked.")
  cancelAiTask()
  aiAssistantState$chatHistory <<- list(aiAssistantState$chatHistory[[1]], list(aiAssistantDialogState()$chatContext))
  aiAssistantState$changeIndex <<- list()
  aiAssistantState$changeNotes <<- character(0L)
  setContent(session, "#miroAiChatLog", as.character(tagList(
    lapply(aiAssistantState$chatHistory, function(item) renderAiChatItem(item, lang$nav$aiBotDialog)),
    genSpinner("miroAiChatSpinner", hidden = TRUE, absolute = FALSE, externalStyle = "z-index: 1")
  )), sanitizeHTML = TRUE)
  aiAssistantState$history <<- list()
  aiAssistantState$sessionId <<- NULL
  aiAssistantState$inputTokens <<- 0L
  aiAssistantState$outputTokens <<- 0L
  aiAssistantState$pendingContextNote <<- NULL
  session$sendCustomMessage(
    "gms-resetAiChatRequest",
    list()
  )
})
observeEvent(input$btMiroAiStop, {
  flog.debug("AI Assistant stop button clicked.")
  cancelAiTask()
  session$sendCustomMessage(
    "gms-resetAiChatRequest",
    list(dividerHtml = as.character(renderAiChatDivider(lang$nav$aiBotDialog$requestAbortedMessage)))
  )
})
observeEvent(input$abortAiCall, {
  flog.debug("AI Assistant dialog closed")
  cancelAiTask()
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

observe({
  dialogState <- aiAssistantDialogState()
  isolate({
    if (is.null(dialogState$sidsToLoad)) {
      return()
    }
    newSids <- suppressWarnings(as.integer(unlist(dialogState$sidsToLoad, use.names = FALSE)))
    newSids <- unique(sort(newSids[!is.na(newSids)]))
    readableSids <- tryCatch(
      db$importDataset("_scenMeta", colNames = "_sid", subsetSids = newSids)[["_sid"]],
      error = function(e) {
        flog.error("Problems fetching scenario metadata for AI assistant. Error message: '%s'.", conditionMessage(e))
        integer()
      }
    )
    if (!length(newSids) || !all(newSids %in% readableSids)) {
      flog.error("AI assistant: Invalid scenario id(s) requested. This is likely an attempt to tamper with the app!")
      return()
    }
    newIsSandbox <- isTRUE(dialogState$isSandbox)
    # every sandbox snapshot gets a new sid, so for the sandbox only its label tells
    # whether the user sees a different context
    contextChanged <- !identical(newIsSandbox, aiAssistantState$contextIsSandbox) ||
      !identical(dialogState$chatContext, aiAssistantState$contextLabel) ||
      (!newIsSandbox && !identical(newSids, aiAssistantState$activeSids))
    if (!identical(newSids, aiAssistantState$activeSids) || contextChanged) {
      if (!is.null(aiAssistantState$activeSids)) {
        aiAssistantState$pendingContextNote <<- dialogState$chatContext
      }
      aiAssistantState$activeSids <<- newSids
      aiAssistantState$contextIsSandbox <<- newIsSandbox
      aiAssistantState$contextHasSandbox <<- newIsSandbox || isTRUE(dialogState$includesSandbox)
    }
    if (contextChanged) {
      aiAssistantState$contextLabel <<- dialogState$chatContext
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
    setContent(session, "#miroAiChatLog",
      as.character(renderAiChatLogContent(aiAssistantState$chatHistory, lang$nav$aiBotDialog,
        contextIsSandbox = aiAssistantState$contextIsSandbox
      )),
      sanitizeHTML = TRUE
    )
    if (!is.null(aiAssistantState$model)) {
      updateSelectInput(session, "miroAiModel", selected = aiAssistantState$model)
    }
    setAiTokenInfo()
    session$sendCustomMessage("gms-showAiDrawer", list())
  })
})

setAiTokenInfo <- function() {
  setTextContent(session, "#miroAiInputTokens", sprintf(
    lang$nav$aiBotDialog$inputTokensLabel,
    format(aiAssistantState$inputTokens, big.mark = ",")
  ))
  setTextContent(session, "#miroAiOutputTokens", sprintf(
    lang$nav$aiBotDialog$outputTokensLabel,
    format(aiAssistantState$outputTokens, big.mark = ",")
  ))
}

observeEvent(input$btAIOutput, {
  flog.debug("AI Output Analysis button clicked.")
  openAiSandboxContext()
})
observeEvent(input$btAiBackToSandbox, {
  flog.debug("AI Assistant: switch context back to sandbox scenario requested.")
  openAiSandboxContext()
})

getAiSandboxScenName <- function(inCompare = FALSE) {
  scenName <- if (is.null(activeScen)) lang$nav$dialogNewScen$newScenName else activeScen$getScenName()
  if (inCompare) {
    scenName <- paste(scenName, lang$nav$scen$scenNameSandboxSuffix)
  }
  return(scenName)
}
# Returns the snapshot scenario or NULL if the sandbox could not be dumped.
snapshotAiSandbox <- function(scenName) {
  tempScen <- tryCatch(saveScenDumpTempToDb(scenName, if (!is.null(activeScen)) activeScen$getStags()),
    error = function(err) {
      flog.error(
        "Could not dump sandbox scenario to database for AI assistant. Error message: %s",
        conditionMessage(err)
      )
      return(NULL)
    }
  )
  if (!is.null(tempScen)) {
    setAiTempScen(tempScen, scenName)
    markAiSnapshotCurrent()
  }
  return(tempScen)
}

openAiSandboxContext <- function() {
  tempScenName <- getAiSandboxScenName()
  if (!is.null(activeScen) && isFALSE(rv$sandboxUnsaved) && length(activeScen$getSid())) {
    sidToLoad <- activeScen$getSid()
    markAiSnapshotCurrent()
  } else {
    tempScen <- snapshotAiSandbox(tempScenName)
    if (is.null(tempScen)) {
      return()
    }
    sidToLoad <- tempScen$getSid()
  }
  aiAssistantDialogState(list(
    sidsToLoad = sidToLoad, scenIdNameMap = NULL, isSandbox = TRUE,
    chatContext = sprintf(lang$nav$aiBotDialog$currentContextSandbox, tempScenName), timestamp = Sys.time()
  ))
}
openAiCompareContext <- function(sids, scenIdNameMap, includeSandbox = FALSE) {
  if (includeSandbox) {
    # always snapshot: the sandbox must not share its sid with a selected scenario
    tempScen <- snapshotAiSandbox(getAiSandboxScenName(inCompare = TRUE))
    if (is.null(tempScen)) {
      return(showErrorMsg(lang$errMsg$loadScen$title, lang$errMsg$unknownError))
    }
    sids <- c(sids, tempScen$getSid())
  }
  aiAssistantDialogState(list(
    sidsToLoad = sids, isSandbox = FALSE, includesSandbox = includeSandbox,
    chatContext = sprintf(lang$nav$aiBotDialog$currentContextCompare, length(sids)),
    scenIdNameMap = scenIdNameMap, timestamp = Sys.time()
  ))
}

setAiTempScen <- function(tempScen, scenName = NULL) {
  oldTempScen <- aiAssistantState$tempScen
  aiAssistantState$tempScen <<- tempScen
  aiAssistantState$sandboxScenName <<- scenName
  if (is.null(oldTempScen) || (!is.null(tempScen) &&
    identical(oldTempScen$getSid(), tempScen$getSid()))) {
    return(invisible())
  }
  tryCatch(
    {
      oldTempScen$delete()$destroy()
    },
    error = function(err) {
      flog.warn("Problems removing temporary scenario. Error message: %s", conditionMessage(err))
    }
  )
  return(invisible())
}

# The drawer does not block the app, so the sandbox can change after the snapshot the
# assistant queries was taken: input edits set rv$inputDataDirty (which is reset here),
# output changes (solve, scenario load) bump the revision of the sandbox in scenData.
markAiSnapshotCurrent <- function() {
  aiAssistantState$snapshotRevision <<- scenData$getSandboxRevision()
  rv$inputDataDirty <- FALSE
  return(invisible())
}
# Returns TRUE if the assistant now looks at different data, NA if the snapshot is
# outdated but could not be refreshed.
syncAiSandboxSnapshot <- function() {
  if (!isTRUE(aiAssistantState$contextHasSandbox) ||
    (!isTRUE(rv$inputDataDirty) &&
      identical(scenData$getSandboxRevision(), aiAssistantState$snapshotRevision))) {
    return(FALSE)
  }
  if (is.null(aiAssistantState$tempScen) && !isTRUE(rv$sandboxUnsaved) &&
    !is.null(activeScen) && length(activeScen$getSid())) {
    # unchanged saved database scenario in sandbox -> no need for copy
    markAiSnapshotCurrent()
    if (identical(activeScen$getSid(), aiAssistantState$activeSids)) {
      return(FALSE)
    }
    aiAssistantState$activeSids <<- activeScen$getSid()
    return(TRUE)
  }
  if (!refreshAiSandboxSnapshot()) {
    return(NA)
  }
  return(TRUE)
}

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
        library(dplyr)
        library(tidyr)
        library(futile.logger)
        source("global.R")
        source("components/ai_provider.R")
        source("components/ai_adapter.R")
        source("components/render_util.R")

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
          "createChart" = function(args) aiAdapter$createChart(args),
          "modifySandboxInputData" = function(args) aiAdapter$modifySandboxInputData(args)
        )
        roleContext <- "### ROLE
You are Mira, the AI assistant of GAMS MIRO, acting as an expert optimization analyst for this app. Answer scenario questions using the scenario database: precise, concise, quantitative, directly on-point. No unneeded explanation.
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
Use `createChart` where a chart clarifies a comparison or trend.

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
          pendingInputChanges = aiAdapter$getPendingInputChanges(),
          charts = aiAdapter$getCharts()
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
  sandboxChanged <- syncAiSandboxSnapshot()
  if (is.na(sandboxChanged)) {
    session$sendCustomMessage("gms-resetAiChatRequest", list(
      userText = userText,
      dividerHtml = as.character(renderAiChatDivider(lang$nav$aiBotDialog$sandboxSyncFailedMessage))
    ))
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
  if (sandboxChanged && !length(notes)) {
    notes <- c(notes, "The sandbox scenario may have changed since your last answer. The database now holds the current data, so re-query it instead of relying on earlier figures.")
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
  aiAssistantState$requestIsSandbox <<- isTRUE(aiAssistantState$contextIsSandbox)
  aiTask$invoke(
    userInput = modelInput,
    dbConnProvider = dbConnProvider,
    dbSchema = dbSchema,
    ioConfig = ioConfig,
    assistantConfig = config$aiAssistant,
    model = aiAssistantState$model,
    allowedSids = aiAssistantState$activeSids,
    scenIdNameMap = if (isTRUE(aiAssistantState$contextHasSandbox) && !is.null(aiAssistantState$tempScen)) {
      c(dialogState$scenIdNameMap, setNames(aiAssistantState$sandboxScenName, aiAssistantState$tempScen$getSid()))
    } else {
      dialogState$scenIdNameMap
    },
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
  modelMessage <- list(role = "model", text = aiOutput)
  if (length(result$charts)) {
    aiAssistantState$charts[names(result$charts)] <<- lapply(result$charts, function(chart) {
      chart$isSandbox <- aiAssistantState$requestIsSandbox
      chart$canOpenInPivot <- !is.null(getAiChartPivotTarget(chart))
      return(chart)
    })
  }
  chartIds <- intersect(aiChartIdsIn(aiOutput), names(aiAssistantState$charts))
  if (length(chartIds)) {
    modelMessage$charts <- aiAssistantState$charts[chartIds]
  }
  aiAssistantState$chatHistory[[length(aiAssistantState$chatHistory) + 1]] <<- modelMessage
  session$sendCustomMessage(
    "gms-addAiChatBubble",
    list(html = as.character(renderAiChatBubble(modelMessage, lang$nav$aiBotDialog)), role = "model")
  )
  if (!is.null(result)) {
    if (length(result$pendingInputChanges)) {
      registerAiInputChanges(result$pendingInputChanges)
    }
    aiAssistantState$history <<- result$newHistory
    aiAssistantState$sessionId <<- result$newSessionId
    aiAssistantState$inputTokens <<- aiAssistantState$inputTokens + result$inputTokens
    aiAssistantState$outputTokens <<- aiAssistantState$outputTokens + result$outputTokens
    setAiTokenInfo()
  }
})
getAiChartPivotTarget <- function(chart) {
  sheetName <- chart$symbolName
  view <- chart$view
  isScalar <- sheetName %in% c(ioConfig$modelOut[[scalarsOutName]]$symnames, ioConfig$modelIn[[scalarsFileName]]$symnames) ||
    (sheetName %in% names(modelIn) && !length(modelIn[[sheetName]]$headers))
  if (isScalar && is.null(getCompareTabValues(sheetName))) {
    sheetName <- if (sheetName %in% ioConfig$modelOut[[scalarsOutName]]$symnames) scalarsOutName else scalarsFileName
    view$filter$scalar <- chart$symbolName
    view$aggregations <- c(view$aggregations, list(description = NULL))
  }
  if (is.null(getCompareTabValues(sheetName))) {
    return(NULL)
  }
  return(list(sheetName = sheetName, view = view))
}
openAiChartInPivot <- function(chart) {
  pivotTarget <- getAiChartPivotTarget(chart)
  if (is.null(pivotTarget)) {
    return(showErrorMsg(lang$nav$aiBotDialog$btOpenChartInPivot, lang$nav$aiBotDialog$chartNotInPivot))
  }
  sheetName <- pivotTarget$sheetName
  view <- pivotTarget$view
  sidsToLoad <<- if (isTRUE(chart$isSandbox)) list("sb") else as.list(as.character(chart$sids))
  if (length(chart$baselineSid) && !isTRUE(chart$isSandbox)) {
    sidsToLoad <<- orderByBaseline(sidsToLoad, as.character(chart$baselineSid))
    view$baselineComparison$record <- baselineScenPlaceholder
  }
  pivotCompViewOverrides[[sheetName]] <<- view
  pivotCompSheetToOpen <<- sheetName
  currentCompMode <<- "pivot"
  scenData$setScenIdNameMap(character(0L))
  session$sendCustomMessage("gms-hideAiDrawer", list())
  loadScenariosIntoCompareMode("pivotView")
}
observeEvent(input$miroAiOpenChart, {
  chartId <- input$miroAiOpenChart
  if (!is.character(chartId) || length(chartId) != 1L || is.null(aiAssistantState$charts[[chartId]])) {
    flog.error("AI assistant: Invalid chart id received. This is likely an attempt to tamper with the app!")
    return()
  }
  flog.debug("AI assistant: open chart in pivot view requested.")
  if (!length(scenData$getRefScenMap("cmpPivot"))) {
    return(openAiChartInPivot(aiAssistantState$charts[[chartId]]))
  }
  aiAssistantState$pendingChartId <<- chartId
  showModal(modalDialog(
    title = lang$nav$aiBotDialog$btOpenChartInPivot,
    lang$nav$aiBotDialog$openChartReplaceDesc,
    footer = tagList(
      modalButton(lang$nav$dialogLoadScen$cancelButton),
      actionButton("btAiOpenChartConfirm", lang$nav$aiBotDialog$btOpenChartReplace,
        class = "bt-highlight-1 bt-gms-confirm"
      )
    ),
    fade = TRUE, easyClose = TRUE
  ))
})
observeEvent(input$btAiOpenChartConfirm, {
  chartId <- aiAssistantState$pendingChartId
  aiAssistantState$pendingChartId <<- NULL
  if (length(chartId) && !is.null(aiAssistantState$charts[[chartId]])) {
    openAiChartInPivot(aiAssistantState$charts[[chartId]])
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
  # once one of them is handled on its own, "apply all" would no longer apply
  # what its card shows
  groupId <- aiAssistantState$changeGroupOf[[change$changeId]]
  if (!identical(status, "pending") && length(groupId)) {
    groupIdx <- aiAssistantState$changeGroupIndex[[groupId]]
    if (identical(aiAssistantState$chatHistory[[groupIdx]]$group$status, "pending")) {
      updateAiInputChangeGroupCard(groupIdx, if (identical(status, "outdated")) "outdated" else "retired")
    }
  }
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
  pendingIds <- character(0L)
  for (change in pendingChanges) {
    while (change$changeId %in% names(aiAssistantState$changeIndex)) {
      change$changeId <- paste0(change$changeId, "_")
    }
    change <- previewAiInputChange(change)
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
    if (identical(change$status, "pending")) {
      pendingIds <- c(pendingIds, change$changeId)
    }
  }
  if (length(pendingIds) > 1L) {
    registerAiInputChangeGroup(pendingIds)
  }
  return(invisible())
}
getAiSandboxInputData <- function(symName) {
  return(isolate(sandboxInputData$getData(symName)()))
}
AI_EMPTY_CHANGE_PREVIEW <- list(
  labels = NULL, before = character(0L), after = character(0L), noRows = 0L, noRowsShown = 0L
)
previewAiInputChange <- function(change, currentData = NULL) {
  change$status <- "pending"
  change$statusDetail <- NULL
  if (is.null(currentData)) {
    currentData <- tryCatch(getAiSandboxInputData(change$symbolName), error = function(err) err)
  }
  changeResult <- NULL
  if (!inherits(currentData, "error")) {
    changeResult <- tryCatch(applyAiInputChange(currentData, change), error = function(err) err)
  }
  errMsg <- NULL
  if (inherits(currentData, "error") || inherits(changeResult, "error")) {
    errMsg <- conditionMessage(if (inherits(currentData, "error")) currentData else changeResult)
    flog.warn(
      "AI assistant proposed a change to symbol: '%s' that could not be previewed. Error message: %s",
      change$symbolName, errMsg
    )
  } else if (!change$isScalar && !any(changeResult$rowMask)) {
    errMsg <- lang$nav$aiBotDialog$changeErrNoRows
  } else if (isTRUE(change$isChoice) && !change$value %in% aiInputChangeChoices(change$symbolName)) {
    errMsg <- sprintf(lang$nav$aiBotDialog$changeErrBadChoice, change$value)
  } else {
    # the assistant checked the range against its database dump; a rebased
    # proposal starts from different values, so it has to be checked again
    errMsg <- aiInputChangeBoundsError(ioConfig$modelIn[[change$symbolName]], change$symbolName, changeResult$newValues)
  }
  if (!is.null(errMsg)) {
    change$status <- "failed"
    change$statusDetail <- errMsg
    change$preview <- AI_EMPTY_CHANGE_PREVIEW
    change$dataHash <- NULL
    return(change)
  }
  change$preview <- buildAiInputChangePreview(currentData, change, changeResult)
  change$dataHash <- digest::digest(currentData, algo = "sha1")
  return(change)
}
aiInputChangePreviewUnchanged <- function(oldPreview, newPreview) {
  return(identical(oldPreview, newPreview) && newPreview$noRowsShown == newPreview$noRows)
}
rebaseAiInputChange <- function(historyIdx, currentData = NULL, refreshGroup = TRUE) {
  change <- previewAiInputChange(aiAssistantState$chatHistory[[historyIdx]]$change, currentData)
  change$rebased <- TRUE
  aiAssistantState$chatHistory[[historyIdx]]$change <<- change
  if (identical(change$status, "failed")) {
    aiAssistantState$changeNotes <<- c(
      aiAssistantState$changeNotes,
      sprintf("REJECTED %s: %s", aiInputChangeSummary(change), change$statusDetail)
    )
  }
  session$sendCustomMessage("gms-updateAiChatCard", list(
    id = change$changeId,
    html = as.character(renderAiInputChangeProposal(change, lang$nav$aiBotDialog))
  ))
  groupId <- aiAssistantState$changeGroupOf[[change$changeId]]
  if (refreshGroup && length(groupId)) {
    refreshAiInputChangeGroup(groupId)
  }
  return(invisible(groupId))
}
#' Rebases every pending proposal for these symbols (except `exceptIdxs`)
#' onto their new data, then refreshes each affected group once.
rebaseAiInputChangesOf <- function(newData, exceptIdxs = integer(0L)) {
  groupIds <- character(0L)
  for (otherIdx in aiAssistantState$changeIndex) {
    if (otherIdx %in% exceptIdxs) {
      next
    }
    otherChange <- aiAssistantState$chatHistory[[otherIdx]]$change
    if (identical(otherChange$status, "pending") && otherChange$symbolName %in% names(newData)) {
      flog.debug("Rebasing AI proposed change to symbol: '%s' onto its updated data.", otherChange$symbolName)
      groupIds <- c(groupIds, rebaseAiInputChange(otherIdx, newData[[otherChange$symbolName]], refreshGroup = FALSE))
    }
  }
  for (groupId in unique(groupIds)) {
    refreshAiInputChangeGroup(groupId)
  }
  return(invisible())
}
registerAiInputChangeGroup <- function(changeIds) {
  group <- buildAiInputChangeGroup(changeIds)
  if (is.null(group)) {
    return(invisible())
  }
  historyIdx <- length(aiAssistantState$chatHistory) + 1L
  aiAssistantState$chatHistory[[historyIdx]] <<- list(role = "inputChangeGroup", group = group)
  aiAssistantState$changeGroupIndex[[group$groupId]] <<- historyIdx
  for (changeId in changeIds) {
    aiAssistantState$changeGroupOf[[changeId]] <<- group$groupId
  }
  session$sendCustomMessage("gms-addAiChatCard", list(
    html = as.character(renderAiInputChangeGroup(group, lang$nav$aiBotDialog))
  ))
  return(invisible())
}
#' The combined preview of a group, computed from its members' current
#' previews and the live data. NULL if they cannot be applied together.
buildAiInputChangeGroup <- function(changeIds) {
  members <- lapply(changeIds, function(changeId) {
    aiAssistantState$chatHistory[[aiAssistantState$changeIndex[[changeId]]]]$change
  })
  sequence <- tryCatch(applyAiInputChangeSequence(members, getAiSandboxInputData),
    error = function(err) err
  )
  if (inherits(sequence, "error")) {
    flog.warn(
      "AI assistant proposed changes that cannot be applied together. Error message: %s",
      conditionMessage(sequence)
    )
    return(NULL)
  }
  independent <- aiInputChangesAreIndependent(members, sequence)
  group <- list(
    groupId = paste0("group_", changeIds[[1L]]),
    changeIds = changeIds,
    status = "pending",
    independent = independent,
    steps = if (!independent) {
      lapply(seq_along(members), function(memberIdx) {
        step <- members[[memberIdx]]
        step$preview <- buildAiInputChangePreview(
          sequence$steps[[memberIdx]]$before, step, sequence$steps[[memberIdx]]$result
        )
        return(step)
      })
    }
  )
  boundsError <- aiInputChangeSequenceBoundsError(members, sequence)
  if (!is.null(boundsError)) {
    group$status <- "failed"
    group$statusDetail <- boundsError
  }
  return(group)
}
#' After a member was rebased, the group's combined preview is stale too: it
#' is recomputed against the live data, or the group is retired if not all of
#' its members are pending any more (or they can no longer be combined).
refreshAiInputChangeGroup <- function(groupId) {
  groupIdx <- aiAssistantState$changeGroupIndex[[groupId]]
  group <- aiAssistantState$chatHistory[[groupIdx]]$group
  if (!identical(group$status, "pending") && !identical(group$status, "failed")) {
    return(invisible())
  }
  allPending <- all(vapply(group$changeIds, function(changeId) {
    identical(aiAssistantState$chatHistory[[aiAssistantState$changeIndex[[changeId]]]]$change$status, "pending")
  }, logical(1L)))
  newGroup <- if (allPending) buildAiInputChangeGroup(group$changeIds)
  if (is.null(newGroup)) {
    updateAiInputChangeGroupCard(groupIdx, "retired")
    return(invisible())
  }
  newGroup$rebased <- TRUE
  aiAssistantState$chatHistory[[groupIdx]]$group <<- newGroup
  session$sendCustomMessage("gms-updateAiChatCard", list(
    id = newGroup$groupId,
    html = as.character(renderAiInputChangeGroup(newGroup, lang$nav$aiBotDialog))
  ))
  return(invisible())
}
#' Each proposal alone was checked against the widget's range by the
#' assistant, but applied together they can still leave it.
aiInputChangeSequenceBoundsError <- function(changes, sequence) {
  for (changeIdx in seq_along(changes)) {
    symName <- changes[[changeIdx]]$symbolName
    boundsError <- aiInputChangeBoundsError(
      ioConfig$modelIn[[symName]], symName, sequence$steps[[changeIdx]]$result$newValues
    )
    if (!is.null(boundsError)) {
      return(boundsError)
    }
  }
  return(NULL)
}
updateAiInputChangeGroupCard <- function(historyIdx, status, statusDetail = NULL) {
  aiAssistantState$chatHistory[[historyIdx]]$group$status <<- status
  aiAssistantState$chatHistory[[historyIdx]]$group$statusDetail <<- statusDetail
  group <- aiAssistantState$chatHistory[[historyIdx]]$group
  session$sendCustomMessage("gms-updateAiChatCard", list(
    id = group$groupId,
    html = as.character(renderAiInputChangeGroup(group, lang$nav$aiBotDialog))
  ))
  return(invisible())
}
refreshAiSandboxSnapshot <- function() {
  inCompare <- !isTRUE(aiAssistantState$contextIsSandbox)
  oldSid <- if (!is.null(aiAssistantState$tempScen)) aiAssistantState$tempScen$getSid()
  tempScen <- snapshotAiSandbox(getAiSandboxScenName(inCompare))
  if (is.null(tempScen)) {
    # forces another attempt before the next request is sent
    aiAssistantState$snapshotRevision <<- NULL
    return(FALSE)
  }
  aiAssistantState$activeSids <<- if (inCompare) {
    c(setdiff(aiAssistantState$activeSids, oldSid), tempScen$getSid())
  } else {
    tempScen$getSid()
  }
  return(TRUE)
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
  currentData <- tryCatch(getAiSandboxInputData(change$symbolName), error = function(err) err)
  if (inherits(currentData, "error") ||
    !identical(digest::digest(currentData, algo = "sha1"), change$dataHash)) {
    flog.info("Input data of symbol: '%s' changed since the AI assistant proposed a change to it.", change$symbolName)
    rebased <- previewAiInputChange(change, if (!inherits(currentData, "error")) currentData)
    if (!identical(rebased$status, "pending") ||
      !aiInputChangePreviewUnchanged(change$preview, rebased$preview)) {
      # what would be applied now is not what the card showed: show the
      # updated preview and let the user confirm again
      rebaseAiInputChange(historyIdx, if (!inherits(currentData, "error")) currentData)
      return()
    }
    # the data changed, but not in any way this proposal is affected by
    aiAssistantState$chatHistory[[historyIdx]]$change <<- rebased
    change <- rebased
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
  # longer exists, so it is rebased onto the new data
  rebaseAiInputChangesOf(setNames(list(newData), change$symbolName), historyIdx)
  refreshAiSandboxSnapshot()
  showNotification(sprintf(
    lang$nav$aiBotDialog$nfChangeApplied,
    change$symbolAlias
  ), type = "message")
})

observeEvent(input$miroAiInputChangeGroupAction, {
  groupId <- input$miroAiInputChangeGroupAction$id
  if (length(groupId) != 1L || !groupId %in% names(aiAssistantState$changeGroupIndex)) {
    flog.warn("Received AI input change group action for unknown group. Ignoring.")
    return()
  }
  groupIdx <- aiAssistantState$changeGroupIndex[[groupId]]
  group <- aiAssistantState$chatHistory[[groupIdx]]$group
  if (!identical(group$status, "pending")) {
    return()
  }
  memberIdxs <- vapply(group$changeIds, function(changeId) {
    as.integer(aiAssistantState$changeIndex[[changeId]])
  }, integer(1L), USE.NAMES = FALSE)
  members <- lapply(memberIdxs, function(memberIdx) aiAssistantState$chatHistory[[memberIdx]]$change)
  if (!all(vapply(members, function(member) identical(member$status, "pending"), logical(1L)))) {
    # belt and braces: resolving a member on its own already retires the group
    updateAiInputChangeGroupCard(groupIdx, "retired")
    return()
  }
  retireAll <- function(status) {
    updateAiInputChangeGroupCard(groupIdx, status)
    for (memberIdx in memberIdxs) {
      updateAiInputChangeCard(memberIdx, status)
    }
  }
  if (!aiCanModifyInput()) {
    flog.info("Refusing to apply AI proposed input data changes outside the sandbox context.")
    retireAll("outdated")
    return()
  }
  # the previews were computed against this very data; if it changed, what
  # would be applied is no longer what the cards show
  dataChanged <- vapply(members, function(member) {
    currentData <- tryCatch(getAiSandboxInputData(member$symbolName), error = function(err) err)
    return(inherits(currentData, "error") ||
      !identical(digest::digest(currentData, algo = "sha1"), member$dataHash))
  }, logical(1L))
  if (any(dataChanged)) {
    flog.info(
      "Input data of symbol(s): '%s' changed since the AI assistant proposed changes to it.",
      paste(unique(vapply(members[dataChanged], `[[`, character(1L), "symbolName")), collapse = "', '")
    )
    # rebase the affected members and the group, then let the user confirm
    # the updated previews
    for (memberIdx in memberIdxs[dataChanged]) {
      rebaseAiInputChange(memberIdx, refreshGroup = FALSE)
    }
    refreshAiInputChangeGroup(groupId)
    return()
  }
  # computed in full before anything is written, so either all proposals are
  # applied or none
  sequence <- tryCatch(applyAiInputChangeSequence(members, getAiSandboxInputData),
    error = function(err) err
  )
  errMsg <- if (inherits(sequence, "error")) {
    conditionMessage(sequence)
  } else {
    aiInputChangeSequenceBoundsError(members, sequence)
  }
  if (!is.null(errMsg)) {
    flog.error("Could not apply AI proposed changes together. Error message: %s", errMsg)
    updateAiInputChangeGroupCard(groupIdx, "failed", errMsg)
    return()
  }
  flog.info(
    "Applying %d AI proposed input data changes to symbol(s): '%s'.",
    length(members), paste(names(sequence$data), collapse = "', '")
  )
  # first, so marking the members applied below does not retire the group
  updateAiInputChangeGroupCard(groupIdx, "applied")
  for (symName in names(sequence$data)) {
    sandboxInputData$setData(symName, sequence$data[[symName]])
  }
  markUnsaved(markDirty = TRUE)
  for (memberIdx in memberIdxs) {
    updateAiInputChangeCard(memberIdx, "applied")
  }
  aiAssistantState$changeNotes <<- c(
    aiAssistantState$changeNotes,
    vapply(members, function(member) sprintf("APPLIED %s", aiInputChangeSummary(member)), character(1L))
  )
  # other proposals for these symbols were previewed against data that no
  # longer exists, so they are rebased onto the new data
  rebaseAiInputChangesOf(sequence$data, memberIdxs)
  refreshAiSandboxSnapshot()
  showNotification(sprintf(
    lang$nav$aiBotDialog$nfChangesApplied, length(members)
  ), type = "message")
})

observeEvent(input$btMiroAiClearChat, {
  flog.debug("Clear AI chat button clicked.")
  cancelAiTask()
  aiAssistantState$chatHistory <<- list(aiAssistantState$chatHistory[[1]], list(aiAssistantDialogState()$chatContext))
  aiAssistantState$changeIndex <<- list()
  aiAssistantState$changeGroupIndex <<- list()
  aiAssistantState$changeGroupOf <<- list()
  aiAssistantState$changeNotes <<- character(0L)
  aiAssistantState$charts <<- list()
  setContent(session, "#miroAiChatLog",
    as.character(renderAiChatLogContent(aiAssistantState$chatHistory, lang$nav$aiBotDialog,
      contextIsSandbox = aiAssistantState$contextIsSandbox
    )),
    sanitizeHTML = TRUE
  )
  aiAssistantState$history <<- list()
  aiAssistantState$sessionId <<- NULL
  aiAssistantState$inputTokens <<- 0L
  aiAssistantState$outputTokens <<- 0L
  setAiTokenInfo()
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
  flog.debug("AI Assistant drawer closed")
  cancelAiTask()
  session$sendCustomMessage("gms-resetAiChatRequest", list())
  setAiTempScen(NULL)
})

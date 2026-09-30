AI_PROVIDER_MAX_RETRY_WAIT <- 10

# Azure documents `retry-after-ms` for standard deployments and adds the
# seconds-based `retry-after` only for provisioned ones; Bedrock may send
# `retry-after`. Without either, back off exponentially with jitter.
retryWaitSeconds <- function(resp, attempt) {
  respHeaders <- headers(resp)
  waitMs <- suppressWarnings(as.numeric(respHeaders[["retry-after-ms"]]))
  if (length(waitMs) == 1L && !is.na(waitMs)) {
    return(waitMs / 1000)
  }
  # an HTTP-date value parses to NA and falls through to the backoff
  waitS <- suppressWarnings(as.numeric(respHeaders[["retry-after"]]))
  if (length(waitS) == 1L && !is.na(waitS)) {
    return(waitS)
  }
  return(2^(attempt - 1L) + runif(1L))
}

httpPostWithRetry <- function(url, authHeaders, payload, errorPrefix, extractErrorMessage) {
  attempt <- 0L
  repeat {
    attempt <- attempt + 1L
    resp <- POST(
      url,
      body = toJSON(payload, auto_unbox = TRUE),
      encode = "raw",
      authHeaders,
      content_type_json(),
      timeout(600L)
    )
    code <- status_code(resp)
    if (code == 200) {
      return(content(resp, type = "application/json", encoding = "utf-8"))
    }
    if (code %in% c(429L, 503L) && attempt <= 2L) {
      wait <- retryWaitSeconds(resp, attempt)
      if (wait <= AI_PROVIDER_MAX_RETRY_WAIT) {
        Sys.sleep(max(wait, 0))
        next
      }
    }
    errBody <- tryCatch(content(resp, type = "application/json", encoding = "utf-8"),
      error = function(e) NULL
    )
    stop(errorPrefix, extractErrorMessage(errBody, code), call. = FALSE)
  }
}

#' ==========================================================================
#' Base Provider Class (Abstract)
#' ==========================================================================
MiroAiBaseProvider <- R6Class("MiroAiBaseProvider",
  public = list(
    sessionId = NULL,
    history = NULL,
    debugFile = NULL,
    initialize = function(modelName, apiKey, debugFile = NULL) {
      private$modelName <- modelName
      private$apiKey <- apiKey
      self$history <- list()
      self$debugFile <- debugFile
      return(self)
    },
    sendMessage = function(userInput, systemInstructions, tools = NULL, toolDispatcher = NULL, maxIterations = 10) {
      payload <- private$prepareInitialPayload(userInput, systemInstructions, tools)
      iterationCounter <- 0
      loopActive <- TRUE
      finalText <- "An unexpected error occurred."
      totalTokenUsage <- list(input = 0, output = 0)

      while (loopActive) {
        iterationCounter <- iterationCounter + 1
        if (iterationCounter > maxIterations) {
          stop("Maximum AI iteration limit reached without resolving an answer.", call. = FALSE)
        }

        if (!is.null(self$debugFile)) {
          private$logToDebugFile("REQUEST PAYLOAD", payload)
        }

        rawResponse <- private$executeHttpCall(payload)

        if (!is.null(self$debugFile)) {
          private$logToDebugFile("RESPONSE CONTENT", rawResponse)
        }

        parsedTurn <- private$parseApiResponse(rawResponse)
        totalTokenUsage$input <- totalTokenUsage$input + (parsedTurn$tokens$input %||% 0L)
        totalTokenUsage$output <- totalTokenUsage$output + (parsedTurn$tokens$output %||% 0L)

        if (parsedTurn$hasToolCalls) {
          if (is.null(toolDispatcher)) {
            stop("Model requested a tool call, but no toolDispatcher was provided.", call. = FALSE)
          }
          toolResults <- lapply(parsedTurn$toolCalls, function(toolCall) {
            toolFunc <- toolDispatcher[[toolCall$name]]
            dbRes <- if (!is.null(toolFunc) && is.function(toolFunc)) {
              tryCatch(
                {
                  toolFunc(toolCall$arguments)
                },
                error = function(err) {
                  toJSON(list(error = sprintf("Tool Execution Failure: %s", err$message)), auto_unbox = TRUE)
                }
              )
            } else {
              toJSON(list(error = sprintf("Tool '%s' not found in dispatcher.", toolCall$name)), auto_unbox = TRUE)
            }
            list(id = toolCall$id, name = toolCall$name, output = dbRes)
          })
          payload <- private$prepareNextIterationPayload(toolResults)
        } else {
          # AI has drafted its final text response
          finalText <- parsedTurn$text
          loopActive <- FALSE
        }
      }
      return(list(
        text = finalText,
        inputTokens = totalTokenUsage$input,
        outputTokens = totalTokenUsage$output
      ))
    },
    clearSession = function() {
      self$sessionId <- NULL
      self$history <- list()
      return(self)
    }
  ), active = list(
    # restores a conversation held by another provider (after switching the model)
    chatHistory = function(newChatHistory) {
      if (missing(newChatHistory)) {
        return(if (private$replaysHistory) self$history else private$.chatHistory)
      }
      # the last item is the current user turn, which is sent as userInput instead
      transferred <- lapply(newChatHistory[-length(newChatHistory)], function(msg) {
        private$historyMessage(identical(msg$role, "user"), msg$text)
      })
      if (private$replaysHistory) {
        self$history <- transferred
      } else {
        private$.chatHistory <- transferred
      }
    }
  ), private = list(
    modelName = NULL,
    apiKey = NULL,
    .chatHistory = NULL,
    # TRUE: the full history is resent with every request (self$history);
    # FALSE: the provider keeps the conversation server-side, so a transferred
    # history is only sent once, with the first request
    replaysHistory = FALSE,
    errorPrefix = "AI provider API Error: ",
    prepareInitialPayload = function(userInput) stop("Abstract"),
    parseApiResponse = function(rawResponse) stop("Abstract"),
    prepareNextIterationPayload = function(toolResults) stop("Abstract"),
    buildUrl = function() stop("Abstract"),
    # a named list of headers; NEVER log it: it carries the credential
    buildAuthHeaders = function() stop("Abstract"),
    historyMessage = function(isUser, text) {
      return(list(role = if (isUser) "user" else "assistant", content = text))
    },
    extractErrorMessage = function(errBody, code) {
      # the string case has to be tested first: `$` on an atomic vector is an
      # error, not NULL, so it would mask the real failure
      if (is.character(errBody) && length(errBody) == 1L) {
        return(errBody)
      }
      msg <- NULL
      if (is.list(errBody)) {
        msg <- errBody$error$message %||% errBody$message %||% errBody$Message
      }
      return(msg %||% sprintf("HTTP %d", code))
    },
    executeHttpCall = function(payload) {
      return(httpPostWithRetry(
        private$buildUrl(), do.call(add_headers, private$buildAuthHeaders()), payload,
        private$errorPrefix, private$extractErrorMessage
      ))
    },
    finalizeText = function(text, hasToolCalls, truncated = FALSE, filteredBy = NULL) {
      if (truncated) {
        if (hasToolCalls) {
          stop("The model's response was truncated (maximum output length reached) while requesting a tool call; cannot safely continue.", call. = FALSE)
        }
        return(paste0(text, "\n\n[Note: This response was truncated because it reached the maximum output length.]"))
      }
      if (length(filteredBy)) {
        return(paste0(text, sprintf("\n\n[Note: This response was filtered by a content safety policy of %s.]", filteredBy)))
      }
      return(text)
    },
    # Tool arguments must reach the dispatcher in one shape, no matter which provider
    # produced them. OpenAI sends them as a JSON string that fromJSON() simplifies,
    # but the Anthropic and Google bodies come out of httr::content(), which parses
    # with simplifyVector = FALSE (and has to: the raw body is replayed back to the
    # API as chat history, so its array structure must survive). Their tool arguments
    # would therefore arrive as deeply nested lists. Round-tripping just the arguments
    # through jsonlite yields the simplified shape the tools expect: atomic vectors,
    # and arrays of objects as data frames.
    simplifyToolArguments = function(arguments) {
      if (is.null(arguments)) {
        return(NULL)
      }
      return(fromJSON(toJSON(arguments, auto_unbox = TRUE, null = "null")))
    },
    logToDebugFile = function(title, contentData) {
      timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      separator <- paste0(rep("=", 80), collapse = "")
      logHeader <- sprintf("[%s] %s (Model: %s)", timestamp, title, private$modelName)
      jsonBody <- tryCatch(
        {
          jsonlite::toJSON(contentData, pretty = TRUE, auto_unbox = TRUE)
        },
        error = function(e) {
          paste("Could not serialize to JSON:", e$message)
        }
      )
      logLines <- c(separator, logHeader, separator, jsonBody, "")
      readr::write_lines(logLines, file = self$debugFile, append = TRUE)
    }
  )
)
#' ==========================================================================
#' Concrete Provider: Google Gemini (Interactions API)
#' ==========================================================================
MiroAiGoogleProvider <- R6Class("MiroAiGoogleProvider",
  inherit = MiroAiBaseProvider,
  private = list(
    errorPrefix = "Gemini API Error: ",
    systemInstructionsPayload = NULL,
    toolsPayload = NULL,
    prepareInitialPayload = function(userInput, systemInstructions, tools) {
      private$systemInstructionsPayload <- systemInstructions
      payload <- list(
        model = private$modelName,
        input = userInput,
        system_instruction = private$systemInstructionsPayload,
        store = TRUE
      )
      if (!is.null(tools) && length(tools) > 0) {
        private$toolsPayload <- tools
        payload$tools <- private$toolsPayload
      }
      if (!is.null(self$sessionId)) payload$previous_interaction_id <- self$sessionId
      if (length(self$chatHistory)) {
        payload$input <- c(self$chatHistory, list(private$historyMessage(TRUE, userInput)))
      }
      return(payload)
    },
    historyMessage = function(isUser, text) {
      return(list(
        type = if (isUser) "user_input" else "model_output",
        content = list(list(type = "text", text = text))
      ))
    },
    buildUrl = function() "https://generativelanguage.googleapis.com/v1beta/interactions",
    buildAuthHeaders = function() list(`x-goog-api-key` = private$apiKey),
    parseApiResponse = function(resContent) {
      if (!is.null(resContent$id)) self$sessionId <- resContent$id
      toolCalls <- list()
      hasToolCalls <- FALSE
      finalText <- ""
      for (step in resContent$steps) {
        if (!is.null(step$type) && step$type == "function_call") {
          hasToolCalls <- TRUE
          toolCalls[[length(toolCalls) + 1]] <- list(
            id = step$id, name = step$name,
            arguments = private$simplifyToolArguments(step$arguments)
          )
        }
        if (!is.null(step$type) && step$type == "model_output") {
          finalText <- step$content[[1]]$text
        }
      }
      return(list(
        hasToolCalls = hasToolCalls,
        toolCalls = toolCalls,
        text = finalText,
        tokens = list(
          input = resContent$usage$total_input_tokens,
          output = resContent$usage$total_output_tokens
        )
      ))
    },
    prepareNextIterationPayload = function(toolResults) {
      toolSteps <- lapply(toolResults, function(res) {
        list(type = "function_result", call_id = res$id, name = res$name, result = list(output = res$output))
      })

      payload <- list(
        model = private$modelName,
        input = toolSteps,
        previous_interaction_id = self$sessionId,
        store = TRUE,
        system_instruction = private$systemInstructionsPayload
      )
      if (!is.null(private$toolsPayload)) {
        payload$tools <- private$toolsPayload
      }
      return(payload)
    }
  )
)

#' ==========================================================================
#' Concrete Provider: OpenAI ChatGPT (Responses API)
#' ==========================================================================
MiroAiOpenAiProvider <- R6Class("MiroAiOpenAiProvider",
  inherit = MiroAiBaseProvider,
  private = list(
    errorPrefix = "OpenAI API Error: ",
    systemInstructionsPayload = NULL,
    toolsPayload = NULL,
    prepareInitialPayload = function(userInput, systemInstructions, tools) {
      private$systemInstructionsPayload <- systemInstructions
      payload <- list(
        model = private$modelName,
        input = userInput,
        store = TRUE,
        instructions = private$systemInstructionsPayload
      )
      if (!is.null(tools) && length(tools) > 0) {
        private$toolsPayload <- tools
        payload$tools <- private$toolsPayload
      }
      if (!is.null(self$sessionId)) {
        payload$previous_response_id <- self$sessionId
      } else if (length(self$chatHistory)) {
        payload$input <- c(self$chatHistory, list(private$historyMessage(TRUE, userInput)))
      }
      return(payload)
    },
    buildUrl = function() "https://api.openai.com/v1/responses",
    buildAuthHeaders = function() list(Authorization = paste("Bearer", private$apiKey)),
    parseApiResponse = function(resContent) {
      if (!is.null(resContent$id)) self$sessionId <- resContent$id

      toolCalls <- list()
      hasToolCalls <- FALSE
      finalText <- ""

      for (item in resContent$output) {
        if (item$type == "function_call") {
          hasToolCalls <- TRUE
          toolCalls[[length(toolCalls) + 1]] <- list(
            id = item$call_id,
            name = item$name,
            arguments = fromJSON(item$arguments)
          )
        } else if (item$type == "message" && !identical(item$phase, "commentary")) {
          messageText <- paste(vapply(item$content, function(contentItem) {
            if (contentItem$type == "output_text") {
              return(contentItem$text)
            } else if (contentItem$type == "refusal") {
              return(contentItem$refusal)
            } else {
              ""
            }
          }, character(1L), USE.NAMES = FALSE), collapse = "\n")
          finalText <- paste(finalText, messageText, sep = "\n")
        }
      }
      return(list(
        hasToolCalls = hasToolCalls,
        toolCalls = toolCalls,
        text = trimws(finalText, which = "left"),
        tokens = list(
          input = resContent$usage$input_tokens,
          output = resContent$usage$output_tokens
        )
      ))
    },
    prepareNextIterationPayload = function(toolResults) {
      toolOutputs <- lapply(toolResults, function(res) {
        list(
          type = "function_call_output",
          call_id = res$id,
          output = res$output
        )
      })
      payload <- list(
        model = private$modelName,
        input = toolOutputs,
        previous_response_id = self$sessionId,
        store = TRUE,
        instructions = private$systemInstructionsPayload
      )
      if (!is.null(private$toolsPayload)) {
        payload$tools <- private$toolsPayload
      }
      return(payload)
    }
  )
)

#' ==========================================================================
#' Concrete Provider: Anthropic Claude (Messages API)
#' ==========================================================================
MiroAiAnthropicProvider <- R6Class("MiroAiAnthropicProvider",
  inherit = MiroAiBaseProvider,
  private = list(
    replaysHistory = TRUE,
    errorPrefix = "Claude API Error: ",
    systemInstructionsPayload = NULL,
    toolsPayload = NULL,
    # thinking is always on for current Claude models and counts towards max_tokens
    maxTokens = 32000L,
    # models with safety classifiers: declined requests are retried server-side on a fallback model
    fallbackModels = c("claude-opus-5-5", "claude-fable-5-1"),
    buildUrl = function() "https://api.anthropic.com/v1/messages",
    buildAuthHeaders = function() {
      headers <- list(`x-api-key` = private$apiKey, `anthropic-version` = "2023-06-01")
      if (private$modelName %in% private$fallbackModels) {
        headers$`anthropic-beta` <- "server-side-fallback-2026-07-01,thinking-binding-controls-2026-08-01"
      }
      return(headers)
    },
    buildPayload = function() {
      payload <- list(
        model = private$modelName,
        max_tokens = private$maxTokens,
        messages = self$history,
        system = private$systemInstructionsPayload,
        # Automatic caching: Anthropic places this breakpoint on the last
        # cacheable message block and moves it forward as the conversation
        # grows, so the growing history is served from cache.
        cache_control = list(type = "ephemeral")
      )
      if (!is.null(private$toolsPayload)) {
        payload$tools <- private$toolsPayload
      }
      if (private$modelName %in% private$fallbackModels) {
        payload$fallbacks <- "default"
        # the system prompt is rebuilt for every request (e.g. when the scenarios in context
        # change), which invalidates the thinking blocks of earlier turns: drop them instead
        # of rejecting the request
        payload$thinking <- list(type = "adaptive", block_binding = list(prefix_mismatch_behavior = "drop_block"))
      }
      return(payload)
    },
    prepareInitialPayload = function(userInput, systemInstructions, tools) {
      self$history[[length(self$history) + 1]] <- private$historyMessage(TRUE, userInput)
      private$systemInstructionsPayload <- list(
        list(type = "text", text = systemInstructions, cache_control = list(type = "ephemeral"))
      )
      if (!is.null(tools) && length(tools) > 0) {
        private$toolsPayload <- lapply(tools, function(f) {
          list(name = f$name, description = f$description, input_schema = f$parameters)
        })
      }
      return(private$buildPayload())
    },
    parseApiResponse = function(resContent) {
      if (identical(resContent$stop_reason, "refusal")) {
        stop("Claude declined to answer this request.", call. = FALSE)
      }
      self$history[[length(self$history) + 1]] <- list(role = "assistant", content = resContent$content)

      toolCalls <- list()
      finalText <- ""
      for (block in resContent$content) {
        if (block$type == "tool_use") {
          toolCalls[[length(toolCalls) + 1]] <- list(
            id = block$id,
            name = block$name,
            arguments = private$simplifyToolArguments(block$input)
          )
        } else if (block$type == "text") {
          finalText <- paste0(finalText, block$text)
        }
      }
      hasToolCalls <- length(toolCalls) > 0L

      return(list(
        hasToolCalls = hasToolCalls,
        toolCalls = toolCalls,
        text = private$finalizeText(finalText, hasToolCalls,
          truncated = identical(resContent$stop_reason, "max_tokens")
        ),
        tokens = list(
          input = resContent$usage$input_tokens,
          output = resContent$usage$output_tokens
        )
      ))
    },
    prepareNextIterationPayload = function(toolResults) {
      toolResultBlocks <- lapply(toolResults, function(res) {
        list(
          type = "tool_result",
          tool_use_id = res$id,
          content = res$output
        )
      })
      self$history[[length(self$history) + 1]] <- list(role = "user", content = toolResultBlocks)
      return(private$buildPayload())
    }
  )
)

azureFederatedTokenFile <- function() {
  return(Sys.getenv(
    "AZURE_FEDERATED_TOKEN_FILE",
    "/var/run/secrets/azure/tokens/azure-identity-token"
  ))
}

MiroAzureEntraTokenProvider <- R6Class("MiroAzureEntraTokenProvider",
  public = list(
    initialize = function(tenantId, clientId,
                          federatedTokenFile = azureFederatedTokenFile(),
                          scope = "https://ai.azure.com/.default") {
      private$tenantId <- tenantId
      private$clientId <- clientId
      private$federatedTokenFile <- federatedTokenFile
      private$scope <- scope
      return(self)
    },
    getToken = function() {
      if (!is.null(private$cachedToken) && private$expiresAt > Sys.time()) {
        return(private$cachedToken)
      }
      tokenInfo <- private$fetchEntraToken()
      # Refresh five minutes early so a long tool loop can never hand out a
      # token that expires halfway through the conversation.
      private$cachedToken <- tokenInfo$access_token
      private$expiresAt <- Sys.time() + max(as.numeric(tokenInfo$expires_in %||% 3600) - 300, 60)
      return(private$cachedToken)
    }
  ), private = list(
    tenantId = NULL,
    clientId = NULL,
    federatedTokenFile = NULL,
    scope = NULL,
    cachedToken = NULL,
    expiresAt = NULL,
    buildCredentialFields = function() {
      # read on every request: the kubelet rotates the projected token
      if (!file.exists(private$federatedTokenFile)) {
        stop(sprintf(
          "Azure workload identity is not configured on this server (no token at %s).",
          private$federatedTokenFile
        ), call. = FALSE)
      }
      return(list(
        client_assertion_type = "urn:ietf:params:oauth:client-assertion-type:jwt-bearer",
        client_assertion = trimws(paste(readLines(private$federatedTokenFile, warn = FALSE), collapse = ""))
      ))
    },
    requestToken = function() {
      # evaluated up front, so a missing token fails before any request is made
      credentialFields <- private$buildCredentialFields()
      return(POST(
        sprintf(
          "https://login.microsoftonline.com/%s/oauth2/v2.0/token",
          utils::URLencode(private$tenantId, reserved = TRUE)
        ),
        body = c(
          list(grant_type = "client_credentials", client_id = private$clientId, scope = private$scope),
          credentialFields
        ),
        encode = "form",
        timeout(30L)
      ))
    },
    fetchEntraToken = function() {
      # Microsoft recommends retrying federated token requests: a freshly
      # added federated credential takes a while to propagate (AADSTS70021)
      attempts <- 3L
      for (attempt in seq_len(attempts)) {
        resp <- private$requestToken()
        resBody <- tryCatch(content(resp, type = "application/json", encoding = "utf-8"),
          error = function(e) NULL
        )
        if (status_code(resp) == 200 && length(resBody$access_token)) {
          return(resBody)
        }
        if (attempt < attempts) {
          Sys.sleep(attempt)
        }
      }
      errMsg <- resBody$error_description %||% resBody$error %||%
        sprintf("HTTP %d", status_code(resp))
      stop("Azure Entra ID authentication failed: ", errMsg, call. = FALSE)
    }
  )
)

#' Desktop only: the MIRO launcher signs the user in with their own Microsoft
#' account and keeps their current access token in the file named by
#' MIRO_AZURE_ACCESS_TOKEN_FILE (see components/azure-user-auth.cjs). R never
#' sees a refresh token or any other credential, and re-reads the file on
#' every call since the launcher replaces it before the token expires.
MiroAzureAccessTokenFileProvider <- R6Class("MiroAzureAccessTokenFileProvider",
  public = list(
    initialize = function(tokenFile = Sys.getenv("MIRO_AZURE_ACCESS_TOKEN_FILE")) {
      private$tokenFile <- tokenFile
      return(self)
    },
    getToken = function() {
      signInAgain <- "Open the MIRO settings and sign in with Microsoft again."
      tokenInfo <- if (nzchar(private$tokenFile) && file.exists(private$tokenFile)) {
        tryCatch(fromJSON(private$tokenFile, simplifyVector = FALSE), error = function(e) NULL)
      }
      if (!length(tokenInfo$accessToken)) {
        stop("You are not signed in to Microsoft Foundry. ", signInAgain, call. = FALSE)
      }
      expiresAt <- suppressWarnings(as.POSIXct(tokenInfo$expiresOn %||% NA_character_,
        format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"
      ))
      if (is.na(expiresAt) || expiresAt <= Sys.time() + 60) {
        stop("Your Microsoft sign-in for Microsoft Foundry has expired. ", signInAgain, call. = FALSE)
      }
      return(tokenInfo$accessToken)
    }
  ), private = list(
    tokenFile = NULL
  )
)

#' ==========================================================================
#' Concrete Provider: Microsoft Foundry (Chat Completions API)
#' ==========================================================================
# where MIRO sends the API key or Entra token: TLS only, and no userinfo,
# query or fragment that would change where or how it is sent
isValidAzureEndpoint <- function(endpoint) {
  return(grepl("^https://[^/?#@[:space:]]+(/[^?#@[:space:]]*)?$", endpoint))
}

# Accepts what admins paste: the resource root, a Foundry project endpoint,
# the documented `.../openai/v1/` base URL or a full `/chat/completions` (or
# `/responses`) route.
normalizeAzureEndpoint <- function(endpoint) {
  endpoint <- sub("/+$", "", trimws(endpoint %||% ""))
  return(sub("/openai(/v1(/chat/completions|/responses)?)?$", "", endpoint))
}
newAzureTokenProvider <- function(azureConfig) {
  if (identical(azureConfig$authMode, "usersignin")) {
    return(MiroAzureAccessTokenFileProvider$new())
  }
  if (identical(azureConfig$authMode, "workloadidentity")) {
    return(MiroAzureEntraTokenProvider$new(tenantId = azureConfig$tenantId, clientId = azureConfig$clientId))
  }
  return(NULL)
}

azureAuthHeaders <- function(tokenProvider, apiKey) {
  if (!is.null(tokenProvider)) {
    return(list(Authorization = paste("Bearer", tokenProvider$getToken())))
  }
  return(list(`api-key` = apiKey))
}

# Claude models on Foundry are served via the Anthropic Messages API only, not chat/completions
isAzureClaudeDeployment <- function(deploymentName) {
  return(grepl("claude", deploymentName, ignore.case = TRUE))
}

MiroAiAzureProvider <- R6Class("MiroAiAzureProvider",
  inherit = MiroAiBaseProvider,
  public = list(
    initialize = function(modelName, azureConfig, debugFile = NULL) {
      super$initialize(modelName, azureConfig$key, debugFile = debugFile)
      private$endpoint <- azureConfig$endpoint
      private$tokenProvider <- newAzureTokenProvider(azureConfig)
      return(self)
    }
  ),
  private = list(
    replaysHistory = TRUE,
    errorPrefix = "Microsoft Foundry API Error: ",
    endpoint = NULL,
    tokenProvider = NULL,
    systemInstructions = NULL,
    toolsPayload = NULL,
    buildUrl = function() {
      return(paste0(normalizeAzureEndpoint(private$endpoint), "/openai/v1/chat/completions"))
    },
    buildAuthHeaders = function() {
      return(azureAuthHeaders(private$tokenProvider, private$apiKey))
    },
    buildPayload = function() {
      # the system prompt is rebuilt for every request, so it is not part of the stored history;
      # no max_completion_tokens on purpose: left out, each model defaults to its own maximum
      payload <- list(
        model = private$modelName,
        messages = c(list(list(role = "system", content = private$systemInstructions)), self$history)
      )
      if (!is.null(private$toolsPayload)) {
        payload$tools <- private$toolsPayload
      }
      return(payload)
    },
    prepareInitialPayload = function(userInput, systemInstructions, tools) {
      private$systemInstructions <- systemInstructions
      self$history[[length(self$history) + 1L]] <- private$historyMessage(TRUE, userInput)
      if (!is.null(tools) && length(tools) > 0) {
        private$toolsPayload <- lapply(tools, function(f) {
          list(type = "function", `function` = list(name = f$name, description = f$description, parameters = f$parameters))
        })
      }
      return(private$buildPayload())
    },
    parseApiResponse = function(resContent) {
      choice <- resContent$choices[[1L]]
      message <- choice$message
      assistantMessage <- list(role = "assistant")
      if (!is.null(message$content)) {
        assistantMessage$content <- message$content
      }
      # replayed verbatim: every tool result must refer to a tool call id the model emitted
      if (length(message$tool_calls)) {
        assistantMessage$tool_calls <- message$tool_calls
      }
      self$history[[length(self$history) + 1L]] <- assistantMessage

      toolCalls <- lapply(message$tool_calls, function(toolCall) {
        list(
          id = toolCall$id,
          name = toolCall$`function`$name,
          arguments = fromJSON(toolCall$`function`$arguments %||% "{}")
        )
      })
      hasToolCalls <- length(toolCalls) > 0L
      finalText <- message$content %||% message$refusal %||% ""

      return(list(
        hasToolCalls = hasToolCalls,
        toolCalls = toolCalls,
        text = private$finalizeText(finalText, hasToolCalls,
          truncated = identical(choice$finish_reason, "length"),
          filteredBy = if (identical(choice$finish_reason, "content_filter")) "Microsoft Foundry"
        ),
        tokens = list(
          input = resContent$usage$prompt_tokens,
          output = resContent$usage$completion_tokens
        )
      ))
    },
    prepareNextIterationPayload = function(toolResults) {
      for (res in toolResults) {
        self$history[[length(self$history) + 1L]] <- list(role = "tool", tool_call_id = res$id, content = res$output)
      }
      return(private$buildPayload())
    }
  )
)

MiroAiAzureAnthropicProvider <- R6Class("MiroAiAzureAnthropicProvider",
  inherit = MiroAiAnthropicProvider,
  public = list(
    initialize = function(modelName, azureConfig, debugFile = NULL) {
      super$initialize(modelName, azureConfig$key, debugFile = debugFile)
      private$endpoint <- azureConfig$endpoint
      private$tokenProvider <- newAzureTokenProvider(azureConfig)
      return(self)
    }
  ),
  private = list(
    errorPrefix = "Microsoft Foundry API Error: ",
    endpoint = NULL,
    tokenProvider = NULL,
    # server-side fallbacks are not available on Foundry
    fallbackModels = character(0L),
    buildUrl = function() {
      return(paste0(normalizeAzureEndpoint(private$endpoint), "/anthropic/v1/messages"))
    },
    buildAuthHeaders = function() {
      return(c(azureAuthHeaders(private$tokenProvider, private$apiKey), list(`anthropic-version` = "2023-06-01")))
    }
  )
)

AI_PROVIDER_REGISTRY <- list(
  openai = list(
    label = "OpenAI", custom = FALSE,
    construct = function(modelId, entry, debugFile) MiroAiOpenAiProvider$new(modelId, entry$key, debugFile = debugFile)
  ),
  anthropic = list(
    label = "Anthropic", custom = FALSE,
    construct = function(modelId, entry, debugFile) MiroAiAnthropicProvider$new(modelId, entry$key, debugFile = debugFile)
  ),
  google = list(
    label = "Google", custom = FALSE,
    construct = function(modelId, entry, debugFile) MiroAiGoogleProvider$new(modelId, entry$key, debugFile = debugFile)
  ),
  azure = list(
    label = "Microsoft Foundry", custom = TRUE,
    construct = function(modelId, entry, debugFile) {
      if (isAzureClaudeDeployment(modelId)) {
        return(MiroAiAzureAnthropicProvider$new(modelId, entry, debugFile = debugFile))
      }
      return(MiroAiAzureProvider$new(modelId, entry, debugFile = debugFile))
    }
  )
)

#' ==========================================================================
#' MiroAiAssistant
#' ==========================================================================
MiroAiAssistant <- R6Class("MiroAiAssistant",
  public = list(
    apiKeys = NULL,
    allModels = list(
      OpenAI = c(
        "GPT-6 Astra" = "openai/gpt-6-astra",
        "GPT-6.1 Sol" = "openai/gpt-6.1-sol",
        "GPT-6 Luna" = "openai/gpt-6-luna"
      ),
      Anthropic = c(
        "Claude Opus 5.5" = "anthropic/claude-opus-5-5",
        "Claude Sonnet 5.5" = "anthropic/claude-sonnet-5-5",
        "Claude Fable 5.1" = "anthropic/claude-fable-5-1",
        "Claude Haiku 4.5" = "anthropic/claude-haiku-4-5"
      ),
      Google = c(
        "Gemini 3.1 Pro"   = "google/gemini-3.1-pro-preview",
        "Gemini 3.8 Flash" = "google/gemini-3.8-flash",
        "Gemini 3.7 Flash" = "google/gemini-3.7-flash",
        "Gemini 3.6 Flash" = "google/gemini-3.6-flash"
      ),
      `Microsoft Foundry` = character(0L)
    ),
    debugFile = NULL,
    initialize = function(config = NULL, apiKeys = NULL) {
      debugFileTmp <- trimws(Sys.getenv("MIRO_AI_ASSISTANT_DEBUG_FILE_PATH", ""))
      if (!identical(debugFileTmp, "")) {
        self$debugFile <- debugFileTmp
      }
      if (length(apiKeys)) {
        # short circuit if API keys are already parsed from config
        self$apiKeys <- setNames(apiKeys, tolower(names(apiKeys)))
        private$registerAllCustomProviderModels()
        return(self)
      }
      self$apiKeys <- list()
      if (!is.list(config)) {
        config <- fromJSON(config, simplifyDataFrame = FALSE, simplifyVector = FALSE)
      }
      private$validateConfig(config)
      return(self)
    },
    getProvider = function(provider, modelId) {
      provider <- tolower(provider)
      if (!provider %in% names(self$apiKeys)) {
        stop(sprintf("Unknown provider: %s specified", provider), call. = FALSE)
      }
      if (!paste0(provider, "/", modelId) %in% unlist(self$supportedModels, use.names = FALSE)) {
        stop(sprintf("Unknown model: %s specified", modelId), call. = FALSE)
      }
      registryEntry <- AI_PROVIDER_REGISTRY[[provider]]
      if (is.null(registryEntry)) {
        stop(sprintf("Provider: %s not found (getProvider). Please report to support@gams.com.", provider), call. = FALSE)
      }
      return(registryEntry$construct(modelId, self$apiKeys[[provider]], self$debugFile))
    }
  ), active = list(
    supportedModels = function(value) {
      if (missing(value)) {
        if (is.null(private$`.supportedModels`)) {
          models <- self$allModels[private$providerIds[names(self$allModels)] %in% names(self$apiKeys)]
          # a provider with no models would render as an empty <optgroup>
          private$`.supportedModels` <- models[lengths(models) > 0L]
        }
        return(private$`.supportedModels`)
      }
      stop("Cant set supportedModels", call. = FALSE)
    }
  ), private = list(
    .supportedModels = NULL,
    providerIds = setNames(
      names(AI_PROVIDER_REGISTRY),
      vapply(AI_PROVIDER_REGISTRY, `[[`, character(1L), "label")
    ),
    validateConfig = function(config) {
      if (!length(config$apiKeys)) {
        stop("Invalid or no api key", call. = FALSE)
      }
      for (apiKey in config$apiKeys) {
        providerId <- tolower(apiKey$provider %||% "")
        if (!providerId %in% private$providerIds) {
          stop(sprintf("AI Provider: %s currently not supported", apiKey$provider), call. = FALSE)
        }
        isSecretless <- identical(providerId, "azure") &&
          tolower(apiKey$authMode %||% "") %in% c("workloadidentity", "usersignin")
        if (!isSecretless && (!length(apiKey$key) || nchar(apiKey$key) < 1L)) {
          stop(sprintf("Invalid API key for provider: %s", apiKey$provider), call. = FALSE)
        }
        self$apiKeys[[providerId]] <- switch(providerId,
          azure = private$validateAzureConfig(apiKey),
          list(key = apiKey$key)
        )
      }
      private$registerAllCustomProviderModels()
      if (!length(self$supportedModels)) {
        stop("No usable AI models configured.", call. = FALSE)
      }
      return(self)
    },
    validateModelsList = function(entry, providerLabel) {
      models <- Filter(function(m) length(m$id) == 1L && nzchar(m$id), entry$models %||% list())
      if (!length(models)) {
        warning(sprintf(
          "%s is configured but no model deployments are cached in the config.", providerLabel
        ), call. = FALSE)
      }
      return(models)
    },
    validateAzureConfig = function(entry) {
      endpoint <- normalizeAzureEndpoint(entry$endpoint)
      if (!isValidAzureEndpoint(endpoint)) {
        stop("Microsoft Foundry endpoint must be an https:// URL without credentials, query or fragment.", call. = FALSE)
      }
      authMode <- tolower(entry$authMode %||% "apikey")
      if (!authMode %in% c("apikey", "workloadidentity", "usersignin")) {
        stop("Azure authMode must be one of 'apiKey', 'workloadIdentity' or 'userSignIn'.", call. = FALSE)
      }
      if (authMode %in% c("workloadidentity", "usersignin")) {
        if (!nzchar(entry$tenantId %||% "") || !nzchar(entry$clientId %||% "")) {
          stop("Azure Entra ID authentication requires both tenantId and clientId.", call. = FALSE)
        }
      }
      return(list(
        key = if (authMode %in% c("workloadidentity", "usersignin")) NULL else entry$key,
        endpoint = endpoint,
        authMode = authMode, tenantId = entry$tenantId, clientId = entry$clientId,
        models = private$validateModelsList(entry, "Microsoft Foundry")
      ))
    },
    registerCustomProviderModels = function(providerId) {
      providerLabel <- AI_PROVIDER_REGISTRY[[providerId]]$label
      entry <- self$apiKeys[[providerId]]
      # the memo would otherwise hide the models we are about to register
      private$`.supportedModels` <- NULL
      if (!length(entry) || !length(entry$models)) {
        self$allModels[[providerLabel]] <- character(0L)
        return(invisible(self))
      }
      ids <- vapply(entry$models, function(m) as.character(m$id), character(1L))
      labels <- vapply(entry$models, function(m) {
        label <- as.character(m$label %||% "")
        if (nzchar(label)) label else as.character(m$id)
      }, character(1L))
      # selectInput silently collapses duplicate choice names, so disambiguate
      # with the deployment id, which is unique per resource
      dupes <- labels %in% labels[duplicated(labels)]
      labels[dupes] <- sprintf("%s (%s)", labels[dupes], ids[dupes])
      self$allModels[[providerLabel]] <- setNames(paste0(providerId, "/", ids), labels)
      return(invisible(self))
    },
    registerAllCustomProviderModels = function() {
      customProviderIds <- names(Filter(function(p) isTRUE(p$custom), AI_PROVIDER_REGISTRY))
      for (providerId in customProviderIds) {
        private$registerCustomProviderModels(providerId)
      }
      return(invisible(self))
    }
  )
)

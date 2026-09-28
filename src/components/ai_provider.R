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

#' ==========================================================================
#' MiroAiAssistant
#' ==========================================================================
MiroAiAssistant <- R6Class("MiroAiAssistant",
  public = list(
    apiKeys = NULL,
    allModels = list(
      OpenAI = c(
        "GPT-6 Astra" = "openai/gpt-6-astra",
        "GPT-6 Sol" = "openai/gpt-6-sol",
        "GPT-6 Luna" = "openai/gpt-6-luna"
      ),
      Anthropic = c(
        "Claude Opus 5.5" = "anthropic/claude-opus-5-5",
        "Claude Sonnet 5" = "anthropic/claude-sonnet-5",
        "Claude Fable 5.1" = "anthropic/claude-fable-5-1",
        "Claude Haiku 4.5" = "anthropic/claude-haiku-4-5"
      ),
      Google = c(
        "Gemini 3.1 Pro"   = "google/gemini-3.1-pro-preview",
        "Gemini 3.8 Flash" = "google/gemini-3.8-flash",
        "Gemini 3.7 Flash" = "google/gemini-3.7-flash",
        "Gemini 3.6 Flash" = "google/gemini-3.6-flash"
      )
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
      providerId <- match(provider, tolower(names(self$supportedModels)))
      if (length(providerId) != 1L || is.na(providerId)) {
        stop(sprintf("Unknown provider: %s specified", provider), call. = FALSE)
      }
      if (!paste0(provider, "/", modelId) %in% self$supportedModels[[providerId]]) {
        stop(sprintf("Unknown model: %s specified", modelId), call. = FALSE)
      }
      if (provider == "google") {
        return(MiroAiGoogleProvider$new(modelId, self$apiKeys[[provider]], debugFile = self$debugFile))
      }
      if (provider == "openai") {
        return(MiroAiOpenAiProvider$new(modelId, self$apiKeys[[provider]], debugFile = self$debugFile))
      }
      if (provider == "anthropic") {
        return(MiroAiAnthropicProvider$new(modelId, self$apiKeys[[provider]], debugFile = self$debugFile))
      }
      stop(sprintf("Provider: %s not found (getProvider). Please report to support@gams.com.", provider), call. = FALSE)
    }
  ), active = list(
    supportedModels = function(value) {
      if (missing(value)) {
        if (is.null(private$`.supportedModels`)) {
          private$`.supportedModels` <- self$allModels[tolower(names(self$allModels)) %in% names(self$apiKeys)]
        }
        return(private$`.supportedModels`)
      }
      stop("Cant set supportedModels", call. = FALSE)
    }
  ), private = list(
    .supportedModels = NULL,
    validateConfig = function(config) {
      if (!length(config$apiKeys)) {
        stop("Invalid or no api key", call. = FALSE)
      }
      for (apiKey in config$apiKeys) {
        providerId <- match(tolower(apiKey$provider), tolower(names(self$allModels)))
        if (length(providerId) != 1L || is.na(providerId)) {
          stop(sprintf("AI Provider: %s currently not supported", apiKey$provider), call. = FALSE)
        }
        if (!length(apiKey$key) || nchar(apiKey$key) < 1L) {
          stop(sprintf("Invalid API key for provider: %s", apiKey$provider), call. = FALSE)
        }
        self$apiKeys[[tolower(apiKey$provider)]] <- apiKey$key
      }
      return(self)
    }
  )
)

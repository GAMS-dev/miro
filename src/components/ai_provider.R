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
    chatHistory = function(newChatHistory) stop("Abstract")
  ), private = list(
    modelName = NULL,
    apiKey = NULL,
    .chatHistory = NULL,
    prepareInitialPayload = function(userInput) stop("Abstract"),
    executeHttpCall = function(payload) stop("Abstract"),
    parseApiResponse = function(rawResponse) stop("Abstract"),
    prepareNextIterationPayload = function(toolResults) stop("Abstract"),
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
  active = list(
    chatHistory = function(newChatHistory) {
      if (missing(newChatHistory)) {
        return(private$.chatHistory)
      }
      # the last item is the current user turn, which is sent as userInput instead
      private$.chatHistory <- lapply(newChatHistory[-length(newChatHistory)], function(msg) {
        list(
          type = if (msg$role == "user") "user_input" else "model_output",
          content = list(list(type = "text", text = msg$text))
        )
      })
    }
  ),
  private = list(
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
        payload$input <- c(self$chatHistory, list(list(
          type = "user_input",
          content = list(list(type = "text", text = userInput))
        )))
      }
      return(payload)
    },
    executeHttpCall = function(payload) {
      url <- "https://generativelanguage.googleapis.com/v1beta/interactions"
      resp <- POST(url,
        body = toJSON(payload, auto_unbox = TRUE), encode = "json",
        content_type_json(), add_headers(`x-goog-api-key` = private$apiKey),
        timeout(120L)
      )
      if (status_code(resp) != 200) {
        errBody <- tryCatch(content(resp, type = "application/json", encoding = "utf-8"), error = function(e) NULL)
        errMsg <- errBody$error$message
        if (is.null(errMsg)) {
          errMsg <- sprintf("HTTP %d", status_code(resp))
        }
        stop("Gemini API Error: ", errMsg, call. = FALSE)
      }
      return(content(resp, type = "application/json", encoding = "utf-8"))
    },
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
  active = list(
    chatHistory = function(newChatHistory) {
      if (missing(newChatHistory)) {
        return(private$.chatHistory)
      }
      # the last item is the current user turn, which is sent as userInput instead
      private$.chatHistory <- lapply(newChatHistory[-length(newChatHistory)], function(msg) {
        list(
          role = if (msg$role == "user") "user" else "assistant",
          content = msg$text
        )
      })
    }
  ),
  private = list(
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
        payload$input <- c(self$chatHistory, list(list(role = "user", content = userInput)))
      }
      return(payload)
    },
    executeHttpCall = function(payload) {
      url <- "https://api.openai.com/v1/responses"
      resp <- POST(
        url,
        body = toJSON(payload, auto_unbox = TRUE),
        encode = "raw",
        add_headers(Authorization = paste("Bearer", private$apiKey)),
        content_type_json(),
        timeout(120L)
      )
      if (status_code(resp) != 200) {
        errBody <- tryCatch(content(resp, type = "application/json", encoding = "utf-8"), error = function(e) NULL)
        errMsg <- errBody$error$message
        if (is.null(errMsg)) {
          errMsg <- sprintf("HTTP %d", status_code(resp))
        }
        stop("OpenAI API Error: ", errMsg, call. = FALSE)
      }
      return(content(resp, type = "application/json", encoding = "utf-8"))
    },
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
  active = list(
    chatHistory = function(newChatHistory) {
      if (missing(newChatHistory)) {
        return(self$history)
      }
      self$history <- lapply(newChatHistory[-length(newChatHistory)], function(msg) {
        list(
          role = if (msg$role == "user") "user" else "assistant",
          content = msg$text
        )
      })
    }
  ),
  private = list(
    systemInstructionsPayload = NULL,
    toolsPayload = NULL,
    prepareInitialPayload = function(userInput, systemInstructions, tools) {
      self$history[[length(self$history) + 1]] <- list(role = "user", content = userInput)
      private$systemInstructionsPayload <- list(
        list(type = "text", text = systemInstructions, cache_control = list(type = "ephemeral"))
      )
      payload <- list(
        model = private$modelName,
        max_tokens = 8192,
        messages = self$history,
        system = private$systemInstructionsPayload,
        # Automatic caching: Anthropic places this breakpoint on the last
        # cacheable message block and moves it forward as the conversation
        # grows, so the growing history is served from cache.
        cache_control = list(type = "ephemeral")
      )
      if (!is.null(tools) && length(tools) > 0) {
        private$toolsPayload <- lapply(tools, function(f) {
          list(name = f$name, description = f$description, input_schema = f$parameters)
        })
        payload$tools <- private$toolsPayload
      }
      return(payload)
    },
    executeHttpCall = function(payload) {
      url <- "https://api.anthropic.com/v1/messages"
      resp <- POST(
        url,
        body = toJSON(payload, auto_unbox = TRUE),
        encode = "raw",
        add_headers(
          `x-api-key` = private$apiKey,
          `anthropic-version` = "2023-06-01"
        ),
        content_type_json(),
        timeout(120L)
      )
      if (status_code(resp) != 200) {
        errBody <- tryCatch(content(resp, type = "application/json", encoding = "utf-8"), error = function(e) NULL)
        errMsg <- errBody$error$message
        if (is.null(errMsg)) {
          errMsg <- sprintf("HTTP %d", status_code(resp))
        }
        stop("Claude API Error: ", errMsg, call. = FALSE)
      }
      return(content(resp, type = "application/json", encoding = "utf-8"))
    },
    parseApiResponse = function(resContent) {
      self$history[[length(self$history) + 1]] <- list(role = "assistant", content = resContent$content)

      toolCalls <- list()
      hasToolCalls <- FALSE
      finalText <- ""

      for (block in resContent$content) {
        if (block$type == "tool_use") {
          hasToolCalls <- TRUE
          toolCalls[[length(toolCalls) + 1]] <- list(
            id = block$id,
            name = block$name,
            arguments = private$simplifyToolArguments(block$input)
          )
        } else if (block$type == "text") {
          finalText <- paste0(finalText, block$text)
        }
      }

      if (identical(resContent$stop_reason, "max_tokens")) {
        if (hasToolCalls) {
          stop("Claude's response was truncated (max_tokens reached) while requesting a tool call; cannot safely continue.", call. = FALSE)
        }
        finalText <- paste0(finalText, "\n\n[Note: This response was truncated because it reached the maximum output length.]")
      }

      return(list(
        hasToolCalls = hasToolCalls,
        toolCalls = toolCalls,
        text = finalText,
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
      payload <- list(
        model = private$modelName,
        max_tokens = 8192,
        messages = self$history,
        system = private$systemInstructionsPayload,
        cache_control = list(type = "ephemeral")
      )
      if (!is.null(private$toolsPayload)) {
        payload$tools <- private$toolsPayload
      }
      return(payload)
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

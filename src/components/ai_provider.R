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
    sendMessage = function(userInput, systemInstructions, tools = NULL, toolDispatcher = NULL, maxIterations = 5) {
      payload <- private$prepareInitialPayload(userInput, systemInstructions, tools)
      iterationCounter <- 0
      loopActive <- TRUE
      finalText <- "An unexpected error occurred."
      lastTokenUsage <- list(input = 0, output = 0)

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
        lastTokenUsage <- parsedTurn$tokens

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
            list(id = toolCall$id, name = toolCall$name, output = fromJSON(dbRes))
          })
          payload <- private$prepareNextIterationPayload(toolResults, parsedTurn$rawContent)
        } else {
          # AI has drafted its final text response
          finalText <- parsedTurn$text
          loopActive <- FALSE
        }
      }
      return(list(
        text = finalText,
        inputTokens = lastTokenUsage$input,
        outputTokens = lastTokenUsage$output
      ))
    },
    clearSession = function() {
      self$sessionId <- NULL
      self$history <- list()
      return(self)
    }
  ), private = list(
    modelName = NULL,
    apiKey = NULL,
    prepareInitialPayload = function(userInput) stop("Abstract"),
    executeHttpCall = function(payload) stop("Abstract"),
    parseApiResponse = function(rawResponse) stop("Abstract"),
    prepareNextIterationPayload = function(toolResults, rawContent) stop("Abstract"),
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
    prepareInitialPayload = function(userInput, systemInstructions, tools) {
      payload <- list(
        model = private$modelName,
        input = userInput,
        system_instruction = systemInstructions,
        store = TRUE
      )
      if (!is.null(tools) && length(tools) > 0) payload$tools <- tools
      if (!is.null(self$sessionId)) payload$previous_interaction_id <- self$sessionId
      return(payload)
    },
    executeHttpCall = function(payload) {
      url <- "https://generativelanguage.googleapis.com/v1beta/interactions"
      resp <- POST(url, body = toJSON(payload, auto_unbox = TRUE), encode = "json", content_type_json(), add_headers(`x-goog-api-key` = private$apiKey))
      if (status_code(resp) != 200) stop("Gemini API Error: ", content(resp)$error$message, call. = FALSE)
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
          toolCalls[[length(toolCalls) + 1]] <- list(id = step$id, name = step$name, arguments = step$arguments)
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
        ),
        rawContent = resContent
      ))
    },
    prepareNextIterationPayload = function(toolResults, rawContent) {
      toolSteps <- lapply(toolResults, function(res) {
        list(type = "function_result", call_id = res$id, name = res$name, result = list(output = res$output))
      })

      return(list(
        model = private$modelName,
        input = toolSteps,
        previous_interaction_id = self$sessionId,
        store = TRUE
      ))
    }
  )
)

#' ==========================================================================
#' Concrete Provider: OpenAI ChatGPT (Responses API)
#' ==========================================================================
MiroAiOpenAiProvider <- R6Class("MiroAiOpenAiProvider",
  inherit = MiroAiBaseProvider,
  private = list(
    prepareInitialPayload = function(userInput, systemInstructions, tools) {
      payload <- list(
        model = private$modelName,
        input = userInput,
        store = TRUE
      )
      if (!is.null(tools) && length(tools) > 0) {
        payload$tools <- tools
      }
      if (!is.null(self$sessionId)) {
        payload$previous_response_id <- self$sessionId
      } else {
        # Only inject the system instruction if it's a brand new thread to save tokens
        payload$instructions <- systemInstructions
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
        content_type_json()
      )
      if (status_code(resp) != 200) stop("OpenAI API Error: ", content(resp)$error$message, call. = FALSE)
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
            id = item$id,
            name = item$name,
            arguments = fromJSON(item$arguments)
          )
        } else if (item$type == "message") {
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
        ),
        rawContent = resContent
      ))
    },
    prepareNextIterationPayload = function(toolResults, rawContent) {
      toolOutputs <- lapply(toolResults, function(res) {
        list(
          type = "function_call_output",
          call_id = res$id,
          output = toJSON(res$output, auto_unbox = TRUE)
        )
      })
      return(list(
        model = private$modelName,
        input = toolOutputs,
        previous_response_id = self$sessionId,
        store = TRUE
      ))
    }
  )
)

#' ==========================================================================
#' Concrete Provider: Anthropic Claude (Messages API)
#' ==========================================================================
MiroAiAnthropicProvider <- R6Class("MiroAiAnthropicProvider",
  inherit = MiroAiBaseProvider,
  private = list(
    prepareInitialPayload = function(userInput, systemInstructions, tools) {
      self$history[[length(self$history) + 1]] <- list(role = "user", content = userInput)
      payload <- list(
        model = private$modelName,
        max_tokens = 8192,
        messages = self$history,
        system = list(
          list(type = "text", text = systemInstructions, cache_control = list(type = "ephemeral"))
        )
      )
      if (!is.null(tools) && length(tools) > 0) {
        payload$tools <- lapply(tools, function(f) {
          list(name = f$name, description = f$description, input_schema = f$parameters)
        })
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
        content_type_json()
      )
      if (status_code(resp) != 200) stop("Claude API Error: ", content(resp)$error$message, call. = FALSE)
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
            arguments = block$input
          )
        } else if (block$type == "text") {
          finalText <- paste0(finalText, block$text)
        }
      }
      return(list(
        hasToolCalls = hasToolCalls,
        toolCalls = toolCalls,
        text = finalText,
        tokens = list(
          input = resContent$usage$input_tokens,
          output = resContent$usage$output_tokens
        ),
        rawContent = resContent
      ))
    },
    prepareNextIterationPayload = function(toolResults, rawContent) {
      toolResultBlocks <- lapply(toolResults, function(res) {
        list(
          type = "tool_result",
          tool_use_id = res$id,
          content = toJSON(res$output, auto_unbox = TRUE)
        )
      })
      self$history[[length(self$history) + 1]] <- list(role = "user", content = toolResultBlocks)
      payload <- list(
        model = private$modelName,
        max_tokens = 4096,
        messages = self$history
      )
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
        "GPT-5.5 (Flagship)" = "openai/gpt-5.5",
        "GPT-5.5 Pro (Advanced Reasoning)" = "openai/gpt-5.5-pro",
        "GPT-5.4 (Standard/Balanced)" = "openai/gpt-5.4",
        "GPT-5.4 Mini (Fast/Cheap)" = "openai/gpt-5.4-mini",
        "GPT-4.1 (Proven/Stable)" = "openai/gpt-4.1",
        "GPT-4o (Legacy Standard)" = "openai/gpt-4o",
        "GPT-4o Mini (Ultra-Cheap)" = "openai/gpt-4o-mini"
      ),
      Anthropic = c(
        "Claude Fable 5 (Long-horizon agents)" = "anthropic/claude-fable-5",
        "Claude Opus 4.8 (Enterprise Coding)" = "anthropic/claude-opus-4-8",
        "Claude Sonnet 5 (Balanced Speed/Intelligence)" = "anthropic/claude-sonnet-5",
        "Claude Haiku 4.5 (Fast/Cheap)" = "anthropic/claude-haiku-4-5-20251001",
        "Claude Sonnet 4.6 (Proven/Stable)" = "anthropic/claude-sonnet-4-6",
        "Claude 3.7 Sonnet (Legacy)" = "anthropic/claude-3-7-sonnet",
        "Claude 3.5 Sonnet (Legacy)" = "anthropic/claude-3-5-sonnet-20241022"
      ),
      Google = c(
        "Gemini 3.5 Flash (Frontier Intelligence)" = "google/gemini-3.5-flash",
        "Gemini 3.1 Pro (Preview)" = "google/gemini-3.1-pro-preview",
        "Gemini 3.1 Flash-Lite (Fast/Volume)" = "google/gemini-3.1-flash-lite",
        "Gemini 2.5 Pro (Proven/Stable)" = "google/gemini-2.5-pro",
        "Gemini 2.5 Flash (High Volume/Cheap)" = "google/gemini-2.5-flash",
        "Gemini 2.0 Flash (Legacy)" = "google/gemini-2.0-flash"
      )
    ),
    debugFile = NULL,
    initialize = function(config = NULL, apiKeys = NULL) {
      debugFileTmp <- trimws(Sys.getenv("AI_ASSISTANT_DEBUG_FILE_PATH", ""))
      if (!identical(debugFileTmp, "")) {
        self$debugFile <- debugFileTmp
      }
      if (length(apiKeys)) {
        # short circuit if API keys are already parsed from config
        self$apiKeys <- apiKeys
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
        self$apiKeys[[apiKey$provider]] <- apiKey$key
      }
      return(self)
    }
  )
)

library(R6)
library(jsonlite)
library(httr)

source("../../components/ai_provider.R")

# ==============================================================================
# 1. MiroAiAssistant Tests (Configuration & Factory Logic)
# ==============================================================================

test_that("MiroAiAssistant initializes correctly and validates API keys", {
  # Test with valid config list
  valid_config <- list(
    apiKeys = list(
      list(provider = "openai", key = "sk-fake-openai"),
      list(provider = "anthropic", key = "sk-fake-anthropic")
    )
  )

  assistant <- MiroAiAssistant$new(config = valid_config)

  expect_equal(assistant$apiKeys$openai$key, "sk-fake-openai")
  expect_equal(assistant$apiKeys$anthropic$key, "sk-fake-anthropic")
  expect_null(assistant$apiKeys$google)
})

test_that("MiroAiAssistant filters supportedModels based on loaded keys", {
  # Only Google key provided
  assistant <- MiroAiAssistant$new(apiKeys = list(google = list(key = "fake-key")))

  supported <- assistant$supportedModels
  expect_true("google" %in% tolower(names(supported)))
  expect_false("openai" %in% tolower(names(supported)))
  expect_false("anthropic" %in% tolower(names(supported)))
})

test_that("MiroAiAssistant getProvider returns correct R6 instances", {
  assistant <- MiroAiAssistant$new(apiKeys = list(
    openai = list(key = "sk-openai"),
    google = list(key = "sk-google")
  ))

  # Valid providers
  openai_provider <- assistant$getProvider("openai", "gpt-6.1-sol")
  google_provider <- assistant$getProvider("google", "gemini-3.6-flash")

  expect_true(inherits(openai_provider, "MiroAiOpenAiProvider"))
  expect_true(inherits(google_provider, "MiroAiGoogleProvider"))

  # Invalid provider/model combos
  expect_error(assistant$getProvider("anthropic", "claude-fable-5-1"), "Unknown provider")
  expect_error(assistant$getProvider("openai", "invalid-model"), "Unknown model")
})

# ==============================================================================
# 2. Mock Classes for Testing the Provider Base Loop
# ==============================================================================

# We create a mock OpenAI provider that intercepts the HTTP call and returns
# hardcoded R lists that mimic the OpenAI Responses API JSON structure.
MockOpenAiProvider <- R6Class("MockOpenAiProvider",
  inherit = MiroAiOpenAiProvider,
  public = list(
    mockResponses = list(),
    callCount = 0,
    setMockResponses = function(responses) {
      self$mockResponses <- responses
    }
  ),
  private = list(
    executeHttpCall = function(payload) {
      self$callCount <- self$callCount + 1
      if (self$callCount > length(self$mockResponses)) {
        stop("Test Error: Ran out of mock responses.")
      }
      return(self$mockResponses[[self$callCount]])
    }
  )
)

# ==============================================================================
# 3. Provider Logic & Tool Dispatch Tests
# ==============================================================================

test_that("MiroAiBaseProvider sendMessage handles plain text responses", {
  provider <- MockOpenAiProvider$new("gpt-4o", "fake-key")

  # Mock a standard text response from OpenAI
  provider$setMockResponses(list(
    list(
      id = "resp_123",
      output = list(
        list(
          type = "message",
          content = list(
            list(type = "output_text", text = "Hello! I am an AI.")
          )
        )
      ),
      usage = list(input_tokens = 10, output_tokens = 8)
    )
  ))

  res <- provider$sendMessage(
    userInput = "Who are you?",
    systemInstructions = "You are helpful."
  )

  expect_equal(res$text, "Hello! I am an AI.")
  expect_equal(res$inputTokens, 10)
  expect_equal(res$outputTokens, 8)
  expect_equal(provider$sessionId, "resp_123")
})

test_that("MiroAiBaseProvider executes tool calls and iterates", {
  provider <- MockOpenAiProvider$new("gpt-4o", "fake-key")

  # Iteration 1: The model decides to call a tool
  iter_1_response <- list(
    id = "resp_tool_call",
    output = list(
      list(
        type = "function_call",
        id = "call_999",
        name = "getWeather",
        arguments = '{"location": "Munich"}'
      )
    ),
    usage = list(input_tokens = 15, output_tokens = 10)
  )

  # Iteration 2: The model reads the tool output and drafts a final text answer
  iter_2_response <- list(
    id = "resp_final",
    output = list(
      list(
        type = "message",
        content = list(
          list(type = "output_text", text = "The weather in Munich is 22 degrees.")
        )
      )
    ),
    usage = list(input_tokens = 30, output_tokens = 12)
  )

  provider$setMockResponses(list(iter_1_response, iter_2_response))

  # Mock Dispatcher
  mockDispatcher <- list(
    getWeather = function(args) {
      # In reality, args is already parsed to a list by the provider's parser
      return(jsonlite::toJSON(list(temp = 22, condition = "Sunny"), auto_unbox = TRUE))
    }
  )

  res <- provider$sendMessage(
    userInput = "What is the weather in Munich?",
    systemInstructions = "You are a weather bot.",
    tools = list(list(name = "getWeather", type = "function")),
    toolDispatcher = mockDispatcher
  )

  # Assertions
  expect_equal(provider$callCount, 2)
  expect_equal(res$text, "The weather in Munich is 22 degrees.")
  expect_equal(res$outputTokens, 22) # Should reflect the total usage
})

test_that("MiroAiBaseProvider enforces maxIterations guardrail", {
  provider <- MockOpenAiProvider$new("gpt-4o", "fake-key")

  # Create a looping scenario where the AI keeps calling the same tool
  infinite_tool_call <- list(
    id = "resp_loop",
    output = list(
      list(
        type = "function_call",
        id = "call_loop",
        name = "dummyTool",
        arguments = "{}"
      )
    ),
    usage = list(input_tokens = 1, output_tokens = 1)
  )

  # Feed it enough responses to hit the limit
  provider$setMockResponses(rep(list(infinite_tool_call), 5))

  mockDispatcher <- list(
    dummyTool = function(args) '{"status": "ok"}'
  )

  # Ensure it aborts when hitting maxIterations = 3
  expect_error(
    provider$sendMessage(
      userInput = "Trigger loop",
      systemInstructions = "System",
      toolDispatcher = mockDispatcher,
      maxIterations = 3
    ),
    "Maximum AI iteration limit reached"
  )
})

test_that("MiroAiAnthropicProvider formats history correctly across iterations", {
  # Mocking Anthropic to specifically test the history array parsing
  MockAnthropicProvider <- R6Class("MockAnthropicProvider",
    inherit = MiroAiAnthropicProvider,
    private = list(
      executeHttpCall = function(payload) {
        # Return a simple text response
        list(
          content = list(list(type = "text", text = "Anthropic Response")),
          usage = list(input_tokens = 5, output_tokens = 5)
        )
      }
    )
  )

  anthropic <- MockAnthropicProvider$new("claude-sonnet-5-5", "key")

  anthropic$sendMessage("Hello Claude", "System text")

  # Check that history was built correctly
  expect_equal(length(anthropic$history), 2)
  expect_equal(anthropic$history[[1]]$role, "user")
  expect_equal(anthropic$history[[2]]$role, "assistant")

  # Clear session test
  anthropic$clearSession()
  expect_equal(length(anthropic$history), 0)
})

# A mock that captures every outgoing payload (instead of just returning a
# canned response), so tests can assert on exactly what would be sent to the
# real Claude API -- payload shape, cache_control placement, tool_result
# content, etc.
MockAnthropicCapture <- R6Class("MockAnthropicCapture",
  inherit = MiroAiAnthropicProvider,
  public = list(
    capturedPayloads = list(),
    mockResponses = list()
  ),
  private = list(
    callCount = 0,
    executeHttpCall = function(payload) {
      private$callCount <- private$callCount + 1
      self$capturedPayloads[[private$callCount]] <- payload
      self$mockResponses[[private$callCount]]
    }
  )
)

test_that("MiroAiAnthropicProvider passes tool output through as the original raw JSON string", {
  # Regression test: the provider used to fromJSON() the adapter's result and
  # then re-toJSON() it before sending to Claude. That round-trip silently
  # drops fields whose value is the literal string "NA" (jsonlite's NA
  # handling), so a scenario using "NA" as a region code would vanish from
  # what the model actually sees.
  provider <- MockAnthropicCapture$new("claude-sonnet-5-5", "key")
  provider$mockResponses <- list(
    list(
      content = list(list(type = "tool_use", id = "call_1", name = "getData", input = list())),
      stop_reason = "tool_use",
      usage = list(input_tokens = 1, output_tokens = 1)
    ),
    list(
      content = list(list(type = "text", text = "done")),
      stop_reason = "end_turn",
      usage = list(input_tokens = 1, output_tokens = 1)
    )
  )

  rawAdapterOutput <- jsonlite::toJSON(
    data.frame(region = "NA", value = 42, stringsAsFactors = FALSE),
    dataframe = "rows", auto_unbox = TRUE
  )
  dispatcher <- list(getData = function(args) rawAdapterOutput)

  provider$sendMessage(
    userInput = "query the data",
    systemInstructions = "system",
    tools = list(list(name = "getData", description = "d", parameters = list())),
    toolDispatcher = dispatcher
  )

  toolResultMessage <- provider$capturedPayloads[[2]]$messages[[3]]
  toolResultContent <- toolResultMessage$content[[1]]$content

  expect_equal(as.character(toolResultContent), as.character(rawAdapterOutput))
  expect_match(as.character(toolResultContent), '"region":"NA"', fixed = TRUE)
})

test_that("MiroAiAnthropicProvider surfaces max_tokens truncation instead of hiding it", {
  provider <- MockAnthropicCapture$new("claude-sonnet-5-5", "key")
  provider$mockResponses <- list(
    list(
      content = list(list(type = "text", text = "partial answer")),
      stop_reason = "max_tokens",
      usage = list(input_tokens = 1, output_tokens = 1)
    )
  )

  res <- provider$sendMessage("hi", "system")

  expect_match(res$text, "truncated", ignore.case = TRUE)
})

test_that("MiroAiAnthropicProvider reports refusals instead of returning an empty answer", {
  provider <- MockAnthropicCapture$new("claude-opus-5-5", "key")
  provider$mockResponses <- list(
    list(
      content = list(),
      stop_reason = "refusal",
      stop_details = list(type = "refusal", category = "bio"),
      usage = list(input_tokens = 1, output_tokens = 0)
    )
  )

  expect_error(provider$sendMessage("hi", "system"), "declined")
})

test_that("MiroAiAnthropicProvider errors instead of continuing when a tool call is truncated", {
  provider <- MockAnthropicCapture$new("claude-sonnet-5-5", "key")
  provider$mockResponses <- list(
    list(
      content = list(list(type = "tool_use", id = "call_1", name = "getData", input = list())),
      stop_reason = "max_tokens",
      usage = list(input_tokens = 1, output_tokens = 1)
    )
  )

  expect_error(
    provider$sendMessage(
      "hi", "system",
      tools = list(list(name = "getData", description = "d", parameters = list())),
      toolDispatcher = list(getData = function(a) "{}")
    ),
    "truncated"
  )
})

test_that("MiroAiAnthropicProvider keeps prompt-cache breakpoints bounded across many iterations", {
  # Anthropic allows at most 4 cache_control breakpoints per request. The
  # provider relies on a static breakpoint on `system` plus a single
  # top-level `cache_control` field -- Anthropic's automatic caching moves
  # that breakpoint to the newest message server-side, so our payload never
  # accumulates one marker per turn regardless of how long the tool loop runs.
  countCacheMarkers <- function(payload) {
    n <- 0
    for (block in payload$system) if (!is.null(block$cache_control)) n <- n + 1
    if (!is.null(payload$cache_control)) n <- n + 1
    n
  }

  provider <- MockAnthropicCapture$new("claude-sonnet-5-5", "key")
  toolTurn <- function(i) {
    list(
      content = list(list(type = "tool_use", id = paste0("t", i), name = "getData", input = list())),
      stop_reason = "tool_use",
      usage = list(input_tokens = 1, output_tokens = 1)
    )
  }
  finalTurn <- list(
    content = list(list(type = "text", text = "done")),
    stop_reason = "end_turn",
    usage = list(input_tokens = 1, output_tokens = 1)
  )
  provider$mockResponses <- c(lapply(1:5, toolTurn), list(finalTurn))

  provider$sendMessage(
    "hi", "system",
    tools = list(list(name = "getData", description = "d", parameters = list())),
    toolDispatcher = list(getData = function(a) jsonlite::toJSON(list(ok = TRUE), auto_unbox = TRUE)),
    maxIterations = 10
  )

  markerCounts <- vapply(provider$capturedPayloads, countCacheMarkers, numeric(1))

  expect_true(all(markerCounts <= 4))
  expect_true(all(markerCounts == 2))

  # The top-level breakpoint should be present on every payload -- Anthropic
  # moves it to the newest message server-side, so our code never has to.
  for (payload in provider$capturedPayloads) {
    expect_false(is.null(payload$cache_control))
  }
})

MockOpenAiCapture <- R6Class("MockOpenAiCapture",
  inherit = MiroAiOpenAiProvider,
  public = list(capturedPayloads = list(), mockResponses = list()),
  private = list(
    callCount = 0,
    executeHttpCall = function(payload) {
      private$callCount <- private$callCount + 1
      self$capturedPayloads[[private$callCount]] <- payload
      self$mockResponses[[private$callCount]]
    }
  )
)

MockGoogleCapture <- R6Class("MockGoogleCapture",
  inherit = MiroAiGoogleProvider,
  public = list(capturedPayloads = list(), mockResponses = list()),
  private = list(
    callCount = 0,
    executeHttpCall = function(payload) {
      private$callCount <- private$callCount + 1
      self$capturedPayloads[[private$callCount]] <- payload
      self$mockResponses[[private$callCount]]
    }
  )
)

test_that("MiroAiOpenAiProvider keeps tools and instructions on follow-up requests", {
  provider <- MockOpenAiCapture$new("gpt-5.6", "key")
  provider$mockResponses <- list(
    list(
      id = "r1",
      output = list(list(type = "function_call", call_id = "c1", name = "getData", arguments = "{}")),
      usage = list(input_tokens = 1, output_tokens = 1)
    ),
    list(
      id = "r2",
      output = list(list(type = "message", content = list(list(type = "output_text", text = "done")))),
      usage = list(input_tokens = 1, output_tokens = 1)
    )
  )

  provider$sendMessage(
    userInput = "hi",
    systemInstructions = "sys prompt",
    tools = list(list(name = "getData", type = "function", description = "d", parameters = list())),
    toolDispatcher = list(getData = function(a) jsonlite::toJSON(list(ok = TRUE), auto_unbox = TRUE))
  )

  expect_true(!is.null(provider$capturedPayloads[[2]]$tools))
  expect_equal(provider$capturedPayloads[[2]]$instructions, "sys prompt")
})

test_that("MiroAiOpenAiProvider passes tool output through as the original raw JSON string", {
  provider <- MockOpenAiCapture$new("gpt-5.6", "key")
  provider$mockResponses <- list(
    list(
      id = "r1",
      output = list(list(type = "function_call", call_id = "c1", name = "getData", arguments = "{}")),
      usage = list(input_tokens = 1, output_tokens = 1)
    ),
    list(
      id = "r2",
      output = list(list(type = "message", content = list(list(type = "output_text", text = "done")))),
      usage = list(input_tokens = 1, output_tokens = 1)
    )
  )

  rawAdapterOutput <- jsonlite::toJSON(
    data.frame(region = "NA", value = 42, stringsAsFactors = FALSE),
    dataframe = "rows", auto_unbox = TRUE
  )

  provider$sendMessage(
    userInput = "hi",
    systemInstructions = "sys prompt",
    tools = list(list(name = "getData", type = "function", description = "d", parameters = list())),
    toolDispatcher = list(getData = function(a) rawAdapterOutput)
  )

  sentOutput <- provider$capturedPayloads[[2]]$input[[1]]$output

  expect_equal(as.character(sentOutput), as.character(rawAdapterOutput))
  expect_match(as.character(sentOutput), '"region":"NA"', fixed = TRUE)
})

test_that("MiroAiGoogleProvider keeps tools and system_instruction on follow-up requests", {
  provider <- MockGoogleCapture$new("gemini-3.5-flash", "key")
  provider$mockResponses <- list(
    list(
      id = "r1",
      steps = list(list(type = "function_call", id = "c1", name = "getData", arguments = list())),
      usage = list(total_input_tokens = 1, total_output_tokens = 1)
    ),
    list(
      id = "r2",
      steps = list(list(type = "model_output", content = list(list(text = "done")))),
      usage = list(total_input_tokens = 1, total_output_tokens = 1)
    )
  )

  provider$sendMessage(
    userInput = "hi",
    systemInstructions = "sys prompt",
    tools = list(list(name = "getData", type = "function", description = "d", parameters = list())),
    toolDispatcher = list(getData = function(a) jsonlite::toJSON(list(ok = TRUE), auto_unbox = TRUE))
  )

  expect_true(!is.null(provider$capturedPayloads[[2]]$tools))
  expect_equal(provider$capturedPayloads[[2]]$system_instruction, "sys prompt")
})

test_that("MiroAiGoogleProvider passes tool output through as the original raw JSON string", {
  provider <- MockGoogleCapture$new("gemini-3.5-flash", "key")
  provider$mockResponses <- list(
    list(
      id = "r1",
      steps = list(list(type = "function_call", id = "c1", name = "getData", arguments = list())),
      usage = list(total_input_tokens = 1, total_output_tokens = 1)
    ),
    list(
      id = "r2",
      steps = list(list(type = "model_output", content = list(list(text = "done")))),
      usage = list(total_input_tokens = 1, total_output_tokens = 1)
    )
  )

  rawAdapterOutput <- jsonlite::toJSON(
    data.frame(region = "NA", value = 42, stringsAsFactors = FALSE),
    dataframe = "rows", auto_unbox = TRUE
  )

  provider$sendMessage(
    userInput = "hi",
    systemInstructions = "sys prompt",
    tools = list(list(name = "getData", type = "function", description = "d", parameters = list())),
    toolDispatcher = list(getData = function(a) rawAdapterOutput)
  )

  sentOutput <- provider$capturedPayloads[[2]]$input[[1]]$result$output

  expect_equal(as.character(sentOutput), as.character(rawAdapterOutput))
  expect_match(as.character(sentOutput), '"region":"NA"', fixed = TRUE)
})

test_that("MiroAiAnthropicProvider hands tool arguments to the dispatcher simplified", {
  # Regression test: httr::content() parses the response body with
  # simplifyVector = FALSE, so `input` arrives with every JSON array as a nested list.
  # Passing that straight through meant the tools received scenarioNames as
  # list("Base") and filters as a list of lists, which made queryScenarioDatabase fail
  # with "invalid subscript type 'list'" and reject every filter -- the tool was
  # unusable on Anthropic while working fine on OpenAI.
  provider <- MockAnthropicCapture$new("claude-sonnet-5-5", "key")
  provider$mockResponses <- list(
    list(
      content = list(list(
        type = "tool_use", id = "call_1", name = "getData",
        input = list(
          symbolName = "demand",
          scenarioNames = list("Base", "High Demand"),
          filters = list(list(column = "region", operator = "=", value = "EU"))
        )
      )),
      stop_reason = "tool_use",
      usage = list(input_tokens = 1, output_tokens = 1)
    ),
    list(
      content = list(list(type = "text", text = "done")),
      stop_reason = "end_turn",
      usage = list(input_tokens = 1, output_tokens = 1)
    )
  )

  receivedArgs <- NULL
  dispatcher <- list(getData = function(args) {
    receivedArgs <<- args
    jsonlite::toJSON(list(ok = TRUE), auto_unbox = TRUE)
  })

  provider$sendMessage(
    userInput = "query the data",
    systemInstructions = "system",
    tools = list(list(name = "getData", description = "d", parameters = list())),
    toolDispatcher = dispatcher
  )

  expect_identical(receivedArgs$scenarioNames, c("Base", "High Demand"))
  expect_true(is.data.frame(receivedArgs$filters))
  expect_identical(receivedArgs$filters$column, "region")
  expect_identical(receivedArgs$filters$value, "EU")
  expect_identical(receivedArgs$symbolName, "demand")
})

test_that("MiroAiGoogleProvider hands tool arguments to the dispatcher simplified", {
  provider <- MockGoogleCapture$new("gemini-3.6-flash", "key")
  provider$mockResponses <- list(
    list(
      steps = list(list(
        type = "function_call", id = "call_1", name = "getData",
        arguments = list(scenarioNames = list("Base"), groupByColumn = list("region", "year"))
      )),
      usage = list(total_input_tokens = 1, total_output_tokens = 1)
    ),
    list(
      steps = list(list(type = "model_output", content = list(list(text = "done")))),
      usage = list(total_input_tokens = 1, total_output_tokens = 1)
    )
  )

  receivedArgs <- NULL
  dispatcher <- list(getData = function(args) {
    receivedArgs <<- args
    jsonlite::toJSON(list(ok = TRUE), auto_unbox = TRUE)
  })

  provider$sendMessage(
    userInput = "query the data",
    systemInstructions = "system",
    tools = list(list(name = "getData", description = "d", parameters = list())),
    toolDispatcher = dispatcher
  )

  expect_identical(receivedArgs$scenarioNames, "Base")
  expect_identical(receivedArgs$groupByColumn, c("region", "year"))
})

test_that("MiroAiBaseProvider keeps counting tokens after a response without usage", {
  # Regression test: `0L + NULL` is a zero-length vector, and it stays zero-length
  # however much is added to it afterwards. One turn whose response carried no usage
  # block therefore blanked the session's token counters permanently.
  provider <- MockAnthropicCapture$new("claude-sonnet-5-5", "key")
  provider$mockResponses <- list(
    list(
      content = list(list(type = "tool_use", id = "call_1", name = "getData", input = list())),
      stop_reason = "tool_use"
      # no usage block at all
    ),
    list(
      content = list(list(type = "text", text = "done")),
      stop_reason = "end_turn",
      usage = list(input_tokens = 7, output_tokens = 3)
    )
  )
  dispatcher <- list(getData = function(args) jsonlite::toJSON(list(ok = TRUE), auto_unbox = TRUE))

  res <- provider$sendMessage(
    userInput = "query the data",
    systemInstructions = "system",
    tools = list(list(name = "getData", description = "d", parameters = list())),
    toolDispatcher = dispatcher
  )

  expect_equal(res$inputTokens, 7)
  expect_equal(res$outputTokens, 3)
})

test_that("MiroAiAssistant accepts a provider name whose case differs from the model list", {
  assistant <- MiroAiAssistant$new(config = list(
    apiKeys = list(list(provider = "Anthropic", key = "test-key"))
  ))

  expect_true("anthropic" %in% tolower(names(assistant$supportedModels)))
  expect_length(assistant$supportedModels, 1L)

  modelId <- sub("^anthropic/", "", assistant$supportedModels[[1L]][[1L]])
  provider <- assistant$getProvider("anthropic", modelId)
  expect_s3_class(provider, "MiroAiAnthropicProvider")
})

transferredChatHistory <- list(
  list(role = "user", text = "earlier question"),
  list(role = "model", text = "earlier answer"),
  list(role = "user", text = "current question")
)

test_that("MiroAiOpenAiProvider sends userInput, not the raw last turn, after a provider switch", {
  provider <- MockOpenAiCapture$new("gpt-5.6", "key")
  provider$mockResponses <- list(list(
    id = "r1",
    output = list(list(type = "message", content = list(list(type = "output_text", text = "done")))),
    usage = list(input_tokens = 1, output_tokens = 1)
  ))
  provider$chatHistory <- transferredChatHistory

  provider$sendMessage(userInput = "[context note]\n\ncurrent question", systemInstructions = "sys")

  sentInput <- provider$capturedPayloads[[1]]$input
  expect_length(sentInput, 3L)
  expect_equal(sentInput[[1]], list(role = "user", content = "earlier question"))
  expect_equal(sentInput[[2]], list(role = "assistant", content = "earlier answer"))
  expect_equal(sentInput[[3]], list(role = "user", content = "[context note]\n\ncurrent question"))
})

test_that("MiroAiGoogleProvider sends userInput, not the raw last turn, after a provider switch", {
  provider <- MockGoogleCapture$new("gemini-3.5-flash", "key")
  provider$mockResponses <- list(list(
    id = "r1",
    steps = list(list(type = "model_output", content = list(list(text = "done")))),
    usage = list(total_input_tokens = 1, total_output_tokens = 1)
  ))
  provider$chatHistory <- transferredChatHistory

  provider$sendMessage(userInput = "[context note]\n\ncurrent question", systemInstructions = "sys")

  sentInput <- provider$capturedPayloads[[1]]$input
  expect_length(sentInput, 3L)
  expect_equal(sentInput[[2]]$type, "model_output")
  expect_equal(sentInput[[3]]$type, "user_input")
  expect_equal(sentInput[[3]]$content[[1]]$text, "[context note]\n\ncurrent question")
})

# ==============================================================================
# Microsoft Foundry
# ==============================================================================

azureEntry <- function(authMode = "apiKey",
                       models = list(
                         list(label = "GPT-4o", id = "gpt4o-prod"),
                         list(label = "Llama 3.3 70B", id = "llama-33-70b")
                       ), ...) {
  modifyList(list(
    provider = "azure", key = "secret-value",
    endpoint = "https://res.services.ai.azure.com",
    authMode = authMode,
    tenantId = "11111111-2222-3333-4444-555555555555",
    clientId = "66666666-7777-8888-9999-000000000000", models = models
  ), list(...))
}

# Captures the outgoing payloads as well as returning canned responses, so the
# replayed history can be asserted on. Only executeHttpCall is overridden here.
MockAzureCapture <- R6Class("MockAzureCapture",
  inherit = MiroAiAzureProvider,
  public = list(
    capturedPayloads = list(),
    mockResponses = list(),
    authHeaders = function() private$buildAuthHeaders(),
    errorMessage = function(body, code) private$extractErrorMessage(body, code),
    url = function() private$buildUrl()
  ),
  private = list(
    callCount = 0,
    executeHttpCall = function(payload) {
      private$callCount <- private$callCount + 1
      self$capturedPayloads[[private$callCount]] <- payload
      if (private$callCount > length(self$mockResponses)) {
        stop("Test Error: Ran out of mock responses.")
      }
      return(self$mockResponses[[private$callCount]])
    }
  )
)

# Canned Chat Completions API bodies
azureTextResponse <- function(text, finishReason = "stop", usage = list(prompt_tokens = 1L, completion_tokens = 1L)) {
  list(
    id = "chatcmpl-1",
    choices = list(list(
      index = 0L, finish_reason = finishReason,
      message = list(role = "assistant", content = text)
    )),
    usage = usage
  )
}

azureToolResponse <- function(name, arguments, callId = "call_1",
                              usage = list(prompt_tokens = 1L, completion_tokens = 1L)) {
  list(
    id = "chatcmpl-1",
    choices = list(list(
      index = 0L, finish_reason = "tool_calls",
      message = list(role = "assistant", content = NULL, tool_calls = list(list(
        id = callId, type = "function", `function` = list(name = name, arguments = arguments)
      )))
    )),
    usage = usage
  )
}

newAzureProvider <- function(entry = azureEntry(), model = "gpt4o-prod") {
  config <- MiroAiAssistant$new(config = list(apiKeys = list(entry)))$apiKeys$azure
  MockAzureCapture$new(model, config)
}

test_that("MiroAiAssistant parses an Azure entry alongside a plain-key provider", {
  assistant <- MiroAiAssistant$new(config = list(apiKeys = list(
    list(provider = "openai", key = "sk-fake-openai"),
    azureEntry()
  )))

  expect_equal(assistant$apiKeys$openai$key, "sk-fake-openai")
  expect_equal(assistant$apiKeys$azure$endpoint, "https://res.services.ai.azure.com")
  expect_equal(assistant$apiKeys$azure$authMode, "apikey")
  expect_length(assistant$apiKeys$azure$models, 2L)
})

test_that("Azure endpoint is normalised and its models reach supportedModels", {
  assistant <- MiroAiAssistant$new(config = list(apiKeys = list(
    azureEntry(endpoint = "https://res.services.ai.azure.com/")
  )))

  # a trailing slash would otherwise produce a double slash in the request URL
  expect_equal(assistant$apiKeys$azure$endpoint, "https://res.services.ai.azure.com")
  expect_equal(
    assistant$supportedModels[["Microsoft Foundry"]],
    c("GPT-4o" = "azure/gpt4o-prod", "Llama 3.3 70B" = "azure/llama-33-70b")
  )
  # the display name must not leak into the provider id
  expect_s3_class(assistant$getProvider("azure", "gpt4o-prod"), "MiroAiAzureProvider")
})

test_that("Azure models survive the rebuild the mirai worker performs", {
  # The worker only ever receives $apiKeys and rebuilds the assistant from it.
  # If the Azure group were registered on the config branch alone, the dropdown
  # would look right while every request failed as an unknown model.
  fromConfig <- MiroAiAssistant$new(config = list(apiKeys = list(azureEntry())))
  inWorker <- MiroAiAssistant$new(apiKeys = fromConfig$apiKeys)

  expect_identical(inWorker$supportedModels, fromConfig$supportedModels)
  expect_s3_class(inWorker$getProvider("azure", "llama-33-70b"), "MiroAiAzureProvider")
})

test_that("Azure config validation rejects malformed entries", {
  expectAzureError <- function(regexp, ...) {
    expect_error(
      suppressWarnings(MiroAiAssistant$new(config = list(apiKeys = list(azureEntry(...))))),
      regexp
    )
  }
  expectAzureError("must be an https", endpoint = "http://insecure.example.com")
  expectAzureError("must be an https", endpoint = "")
  expectAzureError("authMode", authMode = "oauth")
  expectAzureError("tenantId and clientId", authMode = "workloadIdentity", key = NULL, tenantId = NULL)
  # the endpoint receives the credential: no userinfo, query or fragment
  expectAzureError("without credentials", endpoint = "https://user@res.openai.azure.com")
  expectAzureError("without credentials", endpoint = "https://res.openai.azure.com/?x=1")
  expectAzureError("without credentials", endpoint = "https://res.openai.azure.com/#x")
  expectAzureError("tenantId and clientId", authMode = "userSignIn", key = NULL, clientId = "")
  expectAzureError("No usable AI models", models = list())

  # a model entry without an id is dropped rather than trusted
  expect_warning(
    expect_error(
      MiroAiAssistant$new(config = list(apiKeys = list(azureEntry(models = list(list(label = "x")))))),
      "No usable AI models"
    ),
    "no model deployments"
  )
})

test_that("Azure accepts every documented endpoint form", {
  urlFor <- function(endpoint) {
    newAzureProvider(azureEntry(endpoint = endpoint))$url()
  }
  # whatever the admin entered is tried as long as it is https: ports, IPs and
  # internal names are all fine (e.g. a gateway or a Private Link endpoint)
  expect_equal(urlFor("https://10.0.0.5:8443"), "https://10.0.0.5:8443/openai/v1/chat/completions")
  expect_equal(urlFor("https://foundry.internal"), "https://foundry.internal/openai/v1/chat/completions")
  expected <- "https://res.openai.azure.com/openai/v1/chat/completions"
  expect_equal(urlFor("https://res.openai.azure.com"), expected)
  expect_equal(urlFor("https://res.openai.azure.com/openai/v1/"), expected)
  expect_equal(urlFor("https://res.openai.azure.com/openai/v1/chat/completions"), expected)
  expect_equal(urlFor("https://res.openai.azure.com/openai/v1/responses"), expected)
  # a custom domain, e.g. an API Management gateway, keeps its path too
  expect_equal(
    urlFor("https://ai-gateway.contoso.com/foundry/openai/v1"),
    "https://ai-gateway.contoso.com/foundry/openai/v1/chat/completions"
  )
  # a Foundry project endpoint keeps its project path
  expect_equal(
    urlFor("https://res.services.ai.azure.com/api/projects/my-project"),
    "https://res.services.ai.azure.com/api/projects/my-project/openai/v1/chat/completions"
  )
})

test_that("Entra tenant ids are percent-encoded into the token URL", {
  capturedUrl <- NULL
  testthat::local_mocked_bindings(
    POST = function(url, ...) {
      capturedUrl <<- url
      structure(list(), class = "response")
    },
    status_code = function(resp) 200L,
    content = function(...) list(access_token = "tok", expires_in = 3600),
    .package = "httr"
  )
  tokenFile <- tempfile()
  writeLines("jwt", tokenFile)
  MiroAzureEntraTokenProvider$new("contoso.onmicrosoft.com/x", "c", tokenFile)$getToken()
  expect_equal(
    capturedUrl,
    "https://login.microsoftonline.com/contoso.onmicrosoft.com%2Fx/oauth2/v2.0/token"
  )
})

test_that("retry waits prefer retry-after-ms, then retry-after, then backoff", {
  withHeaders <- function(h) structure(list(headers = h), class = "response")
  testthat::local_mocked_bindings(headers = function(resp) resp$headers, .package = "httr")
  expect_equal(retryWaitSeconds(withHeaders(list(`retry-after-ms` = "1500", `retry-after` = "9")), 1L), 1.5)
  expect_equal(retryWaitSeconds(withHeaders(list(`retry-after` = "3")), 1L), 3)
  # an HTTP-date retry-after falls back to exponential backoff with jitter
  wait <- retryWaitSeconds(withHeaders(list(`retry-after` = "Wed, 21 Oct 2026 07:28:00 GMT")), 2L)
  expect_gte(wait, 2)
  expect_lt(wait, 3)
})

test_that("a retry-after beyond the cap fails fast instead of blocking", {
  calls <- 0L
  testthat::local_mocked_bindings(
    POST = function(...) {
      calls <<- calls + 1L
      structure(list(), class = "response")
    },
    status_code = function(resp) 429L,
    headers = function(resp) list(`retry-after` = "60"),
    content = function(...) list(message = "slow down"),
    .package = "httr"
  )
  expect_error(
    httpPostWithRetry("https://x", NULL, list(), "Err: ", function(body, code) body$message),
    "Err: slow down"
  )
  expect_equal(calls, 1L)
})

test_that("Azure duplicate model labels are disambiguated", {
  assistant <- MiroAiAssistant$new(config = list(apiKeys = list(azureEntry(models = list(
    list(label = "GPT-4o", id = "gpt4o-prod"),
    list(label = "GPT-4o", id = "gpt4o-test"),
    list(id = "no-label")
  )))))

  # selectInput collapses duplicate choice names, which would silently hide a deployment
  expect_equal(
    assistant$supportedModels[["Microsoft Foundry"]],
    c(
      "GPT-4o (gpt4o-prod)" = "azure/gpt4o-prod",
      "GPT-4o (gpt4o-test)" = "azure/gpt4o-test",
      "no-label" = "azure/no-label"
    )
  )
})

test_that("Azure builds a Chat Completions payload", {
  provider <- newAzureProvider()
  provider$mockResponses <- list(azureTextResponse("Hello."))

  result <- provider$sendMessage(
    "What is the demand?", "SYSTEM INSTRUCTIONS",
    tools = list(list(name = "getData", description = "d", parameters = list(type = "object")))
  )

  payload <- provider$capturedPayloads[[1]]
  expect_equal(result$text, "Hello.")
  expect_equal(payload$model, "gpt4o-prod")
  expect_equal(payload$messages[[1]], list(role = "system", content = "SYSTEM INSTRUCTIONS"))
  expect_equal(payload$messages[[2]], list(role = "user", content = "What is the demand?"))
  expect_equal(payload$tools[[1]]$type, "function")
  expect_equal(payload$tools[[1]]$`function`$name, "getData")
  expect_null(payload$max_completion_tokens)
  # no api-version query param: the v1 path is self-versioning
  expect_equal(provider$url(), "https://res.services.ai.azure.com/openai/v1/chat/completions")
})

test_that("Claude deployments on Azure use the Anthropic Messages API", {
  entry <- azureEntry(models = list(
    list(label = "Claude Sonnet", id = "claude-sonnet-4-5"), list(label = "GPT-4o", id = "gpt4o-prod")
  ))
  assistant <- MiroAiAssistant$new(config = list(apiKeys = list(entry)))
  provider <- assistant$getProvider("azure", "claude-sonnet-4-5")
  expect_s3_class(provider, "MiroAiAzureAnthropicProvider")
  expect_s3_class(assistant$getProvider("azure", "gpt4o-prod"), "MiroAiAzureProvider")

  private <- provider$.__enclos_env__$private
  expect_equal(private$buildUrl(), "https://res.services.ai.azure.com/anthropic/v1/messages")
  expect_equal(
    private$buildAuthHeaders(),
    list(`api-key` = "secret-value", `anthropic-version` = "2023-06-01")
  )
  private$modelName <- "claude-opus-5-5"
  payload <- private$prepareInitialPayload("q", "sys", NULL)
  expect_equal(payload$model, "claude-opus-5-5")
  expect_equal(payload$messages[[1]]$content, "q")
  # server-side fallbacks do not exist on Foundry
  expect_null(payload$fallbacks)
  expect_null(payload$thinking)
  expect_null(private$buildAuthHeaders()$`anthropic-beta`)
})

test_that("Claude models drop thinking blocks invalidated by a changed system prompt", {
  private <- MiroAiAnthropicProvider$new("claude-haiku-5-5", "sk-ant")$.__enclos_env__$private
  payload <- private$prepareInitialPayload("q", "sys", NULL)
  expect_equal(payload$thinking$block_binding$prefix_mismatch_behavior, "drop_block")
  expect_null(payload$fallbacks)
  expect_identical(private$buildAuthHeaders()$`anthropic-beta`, "thinking-binding-controls-2026-08-01")
  private$modelName <- "claude-opus-5-5"
  expect_identical(
    private$buildAuthHeaders()$`anthropic-beta`,
    "server-side-fallback-2026-07-01,thinking-binding-controls-2026-08-01"
  )
})

test_that("Azure surfaces truncation and content filtering", {
  provider <- newAzureProvider()
  provider$mockResponses <- list(azureTextResponse("partial", finishReason = "length"))
  result <- provider$sendMessage("q", "sys")
  expect_match(result$text, "truncated")

  provider <- newAzureProvider()
  provider$mockResponses <- list(azureTextResponse("", finishReason = "content_filter"))
  result <- provider$sendMessage("q", "sys")
  expect_match(result$text, "content safety policy")
})

test_that("Azure tool arguments reach the dispatcher simplified", {
  provider <- newAzureProvider()
  rawArguments <- '{"symbolName":"demand","scenarioNames":["Base","High Demand"],"filters":[{"column":"region","operator":"=","value":"EU"}]}'
  provider$mockResponses <- list(
    azureToolResponse("queryScenarioDatabase", rawArguments),
    azureTextResponse("Done.")
  )

  received <- NULL
  provider$sendMessage("q", "sys",
    tools = list(list(name = "queryScenarioDatabase", description = "d", parameters = list())),
    toolDispatcher = list(queryScenarioDatabase = function(args) {
      received <<- args
      toJSON(list(ok = TRUE), auto_unbox = TRUE)
    })
  )

  # arguments arrive as a JSON string, so fromJSON() simplifies them in one step;
  # simplifyToolArguments() would hand the dispatcher a bare string instead
  expect_equal(received$symbolName, "demand")
  expect_equal(received$scenarioNames, c("Base", "High Demand"))
  expect_true(is.data.frame(received$filters))
  expect_equal(received$filters$column, "region")
})

test_that("Azure passes tool output through as the original raw JSON string", {
  # Same regression this guards against for OpenAI: fromJSON()-then-toJSON()
  # would silently drop a literal "NA" cell (jsonlite's NA handling).
  provider <- newAzureProvider()
  rawArguments <- '{"symbolName":"demand"}'
  provider$mockResponses <- list(
    azureToolResponse("getData", rawArguments, callId = "call_abc"),
    azureTextResponse("Done.")
  )

  toolOutput <- toJSON(data.frame(region = "NA", value = 42), dataframe = "rows", auto_unbox = TRUE)
  provider$sendMessage("q", "sys",
    tools = list(list(name = "getData", description = "d", parameters = list())),
    toolDispatcher = list(getData = function(args) toolOutput)
  )

  sentMessages <- provider$capturedPayloads[[2]]$messages
  # the assistant turn is replayed with its tool call, followed by the result
  expect_equal(sentMessages[[3]]$tool_calls[[1]]$id, "call_abc")
  toolMessage <- sentMessages[[4]]
  expect_equal(toolMessage$role, "tool")
  expect_equal(toolMessage$tool_call_id, "call_abc")
  expect_equal(as.character(toolMessage$content), as.character(toolOutput))
  expect_match(as.character(toolMessage$content), '"region":"NA"', fixed = TRUE)
})

test_that("Azure accumulates token usage across tool iterations", {
  provider <- newAzureProvider()
  provider$mockResponses <- list(
    azureToolResponse("getData", "{}"),
    azureTextResponse("Done.", usage = list(prompt_tokens = 7L, completion_tokens = 3L))
  )

  result <- provider$sendMessage("q", "sys",
    tools = list(list(name = "getData", description = "d", parameters = list())),
    toolDispatcher = list(getData = function(args) "{}")
  )
  expect_equal(result$inputTokens, 8L)
  expect_equal(result$outputTokens, 4L)
})

test_that("Azure restores a prior chat history, then replays the full history", {
  provider <- newAzureProvider()
  provider$mockResponses <- list(azureTextResponse("Hi."), azureTextResponse("Again."))

  provider$chatHistory <- list(
    list(role = "user", text = "first"),
    list(role = "model", text = "answer"),
    list(role = "user", text = "second")
  )
  provider$sendMessage("second", "SYS ONE")

  # the trailing entry is the message being sent, so it must not appear twice
  firstMessages <- provider$capturedPayloads[[1]]$messages
  expect_equal(vapply(firstMessages, function(m) m$role, character(1L)), c("system", "user", "assistant", "user"))
  expect_equal(firstMessages[[2]]$content, "first")
  expect_equal(firstMessages[[4]]$content, "second")

  # Chat Completions keeps no server-side state: the next turn resends
  # everything, with the system prompt of that request
  provider$sendMessage("third", "SYS TWO")
  secondMessages <- provider$capturedPayloads[[2]]$messages
  expect_equal(secondMessages[[1]], list(role = "system", content = "SYS TWO"))
  expect_equal(
    vapply(secondMessages[-1], function(m) m$content, character(1L)),
    c("first", "answer", "second", "Hi.", "third")
  )
  expect_null(provider$sessionId)
})

test_that("Azure selects the right auth header per mode", {
  expect_equal(newAzureProvider()$authHeaders(), list(`api-key` = "secret-value"))

  entraProvider <- newAzureProvider(azureEntry(authMode = "workloadIdentity", key = NULL))
  fakeToken <- R6Class("FakeToken", public = list(getToken = function() "tok-123"))$new()
  entraProvider$.__enclos_env__$private$tokenProvider <- fakeToken
  expect_equal(entraProvider$authHeaders(), list(Authorization = "Bearer tok-123"))
})

test_that("Azure surfaces the varying error body shapes", {
  provider <- newAzureProvider()
  expect_equal(provider$errorMessage(list(error = list(message = "quota")), 429L), "quota")
  expect_equal(provider$errorMessage(list(message = "bad deployment"), 404L), "bad deployment")
  expect_equal(provider$errorMessage("plain text", 400L), "plain text")
  expect_equal(provider$errorMessage(NULL, 500L), "HTTP 500")
})

test_that("Entra tokens are cached per instance and refreshed before they expire", {
  MockToken <- R6Class("MockToken",
    inherit = MiroAzureEntraTokenProvider,
    public = list(fetchCount = 0, expiresIn = 3600),
    private = list(
      fetchEntraToken = function() {
        self$fetchCount <- self$fetchCount + 1
        list(access_token = paste0("tok-", self$fetchCount), expires_in = self$expiresIn)
      }
    )
  )

  provider <- MockToken$new("tenant", "client")
  # one sendMessage can make up to 10 HTTP calls; each must not re-authenticate
  tokens <- replicate(5, provider$getToken())
  expect_equal(provider$fetchCount, 1L)
  expect_equal(unique(tokens), "tok-1")

  # an expired entry is refetched rather than handed out. Seed it directly:
  # the five minute early refresh is floored at 60s, so no expires_in the
  # token endpoint could return would make an entry stale straight away.
  expiring <- MockToken$new("tenant2", "client2")
  expect_equal(expiring$getToken(), "tok-1")
  expiring$.__enclos_env__$private$expiresAt <- Sys.time() - 1
  expect_equal(expiring$getToken(), "tok-2")
  expect_equal(expiring$fetchCount, 2L)
})

test_that("workload identity needs no key but a tenant and client id", {
  wiEntry <- azureEntry(authMode = "workloadIdentity", key = NULL)
  assistant <- MiroAiAssistant$new(config = list(apiKeys = list(wiEntry)))
  expect_equal(assistant$apiKeys$azure$authMode, "workloadidentity")
  expect_null(assistant$apiKeys$azure$key)
  # a stale key left in the entry is dropped rather than kept around
  withKey <- MiroAiAssistant$new(config = list(apiKeys = list(
    azureEntry(authMode = "workloadIdentity", key = "leftover")
  )))
  expect_null(withKey$apiKeys$azure$key)

  expect_error(
    MiroAiAssistant$new(config = list(apiKeys = list(
      azureEntry(authMode = "workloadIdentity", key = NULL, clientId = NULL)
    ))),
    "tenantId and clientId"
  )
  # only Azure workload identity is secretless
  expect_error(
    MiroAiAssistant$new(config = list(apiKeys = list(azureEntry(key = NULL)))),
    "Invalid API key"
  )
})

test_that("workload identity sends the projected token as client assertion", {
  tokenFile <- tempfile()
  writeLines("header.payload.signature", tokenFile)
  captured <- NULL
  testthat::local_mocked_bindings(
    POST = function(url, body, ...) {
      captured <<- list(url = url, body = body)
      structure(list(), class = "response")
    },
    status_code = function(resp) 200L,
    content = function(...) list(access_token = "tok", expires_in = 3600),
    .package = "httr"
  )
  provider <- MiroAzureEntraTokenProvider$new(
    "11111111-2222-3333-4444-555555555555", "66666666-7777-8888-9999-000000000000",
    federatedTokenFile = tokenFile
  )
  expect_equal(provider$getToken(), "tok")
  expect_equal(captured$body$client_assertion, "header.payload.signature")
  expect_equal(
    captured$body$client_assertion_type,
    "urn:ietf:params:oauth:client-assertion-type:jwt-bearer"
  )
  expect_null(captured$body$client_secret)
  expect_equal(captured$body$scope, "https://ai.azure.com/.default")
})

test_that("workload identity retries a failed federated token request", {
  tokenFile <- tempfile()
  writeLines("jwt", tokenFile)
  calls <- 0L
  testthat::local_mocked_bindings(
    POST = function(...) {
      calls <<- calls + 1L
      structure(list(ok = calls > 1L), class = "response")
    },
    status_code = function(resp) if (resp$ok) 200L else 400L,
    content = function(resp, ...) {
      if (resp$ok) list(access_token = "tok") else list(error = "invalid_client")
    },
    .package = "httr"
  )
  provider <- MiroAzureEntraTokenProvider$new("t.onmicrosoft.com", "c", federatedTokenFile = tokenFile)
  expect_equal(provider$getToken(), "tok")
  expect_equal(calls, 2L)
})

test_that("workload identity without a mounted token fails with a clear message", {
  provider <- MiroAzureEntraTokenProvider$new("t", "c", federatedTokenFile = tempfile())
  expect_error(provider$getToken(), "not configured on this server")
})

test_that("the workload identity provider authenticates with a bearer token", {
  provider <- newAzureProvider(azureEntry(authMode = "workloadIdentity", key = NULL))
  fakeToken <- R6Class("FakeToken", public = list(getToken = function() "wi-tok"))$new()
  provider$.__enclos_env__$private$tokenProvider <- fakeToken
  expect_equal(provider$authHeaders(), list(Authorization = "Bearer wi-tok"))
})

test_that("desktop Microsoft sign-in reads the launcher's access token file", {
  tokenFile <- tempfile()
  writeToken <- function(expiresIn) {
    writeLines(jsonlite::toJSON(list(
      accessToken = "user-tok",
      expiresOn = format(Sys.time() + expiresIn, "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC")
    ), auto_unbox = TRUE), tokenFile)
  }
  writeToken(3600)
  provider <- MiroAzureAccessTokenFileProvider$new(tokenFile)
  expect_equal(provider$getToken(), "user-tok")
  # the launcher replaces the file, so every call must see the current one
  writeToken(30)
  expect_error(provider$getToken(), "has expired")
  expect_error(MiroAzureAccessTokenFileProvider$new(tempfile())$getToken(), "not signed in")
  expect_error(MiroAzureAccessTokenFileProvider$new("")$getToken(), "not signed in")
})

test_that("desktop Microsoft sign-in is secretless and authenticates as the user", {
  assistant <- MiroAiAssistant$new(config = list(apiKeys = list(
    azureEntry(authMode = "userSignIn", key = NULL)
  )))
  expect_equal(assistant$apiKeys$azure$authMode, "usersignin")
  expect_null(assistant$apiKeys$azure$key)

  tokenFile <- tempfile()
  writeLines(jsonlite::toJSON(list(
    accessToken = "user-tok",
    expiresOn = format(Sys.time() + 3600, "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC")
  ), auto_unbox = TRUE), tokenFile)
  withr::local_envvar(MIRO_AZURE_ACCESS_TOKEN_FILE = tokenFile)
  provider <- newAzureProvider(azureEntry(authMode = "userSignIn", key = NULL))
  expect_equal(provider$authHeaders(), list(Authorization = "Bearer user-tok"))
})

test_that("Entra authentication failures surface the AADSTS code, not the assertion", {
  tokenFile <- tempfile()
  writeLines("assertion-jwt", tokenFile)
  testthat::local_mocked_bindings(
    POST = function(...) structure(list(), class = "response"),
    status_code = function(resp) 400L,
    content = function(...) list(error_description = "AADSTS70021: No matching federated identity record found."),
    .package = "httr"
  )
  err <- tryCatch(MiroAzureEntraTokenProvider$new("t.onmicrosoft.com", "c", tokenFile)$getToken(),
    error = function(e) conditionMessage(e)
  )
  expect_match(err, "AADSTS70021")
  expect_false(grepl("assertion-jwt", err, fixed = TRUE))
})

test_that("every provider sends its request through the shared HTTP call", {
  calls <- list()
  testthat::local_mocked_bindings(
    POST = function(url, config, ...) {
      calls[[length(calls) + 1L]] <<- list(url = url, headers = config$headers)
      structure(list(code = if (length(calls) == 1L) 429L else 200L), class = "response")
    },
    status_code = function(resp) resp$code,
    headers = function(resp) list(`retry-after-ms` = "1"),
    content = function(resp, ...) list(ok = TRUE),
    .package = "httr"
  )
  anthropic <- MiroAiAnthropicProvider$new("claude-opus-5-5", "sk-ant")
  # a 429 is retried once, then the 200 reply is returned
  expect_equal(anthropic$.__enclos_env__$private$executeHttpCall(list()), list(ok = TRUE))
  expect_length(calls, 2L)
  expect_equal(calls[[2]]$url, "https://api.anthropic.com/v1/messages")
  expect_equal(calls[[2]]$headers[["x-api-key"]], "sk-ant")
  expect_match(calls[[2]]$headers[["anthropic-beta"]], "server-side-fallback", fixed = TRUE)

  calls <- list(list())
  MiroAiOpenAiProvider$new("gpt-6.1-sol", "sk-oai")$.__enclos_env__$private$executeHttpCall(list())
  expect_equal(calls[[2]]$url, "https://api.openai.com/v1/responses")
  expect_equal(calls[[2]]$headers[["Authorization"]], "Bearer sk-oai")

  calls <- list(list())
  MiroAiGoogleProvider$new("gemini-3.8-flash", "g-key")$.__enclos_env__$private$executeHttpCall(list())
  expect_equal(calls[[2]]$url, "https://generativelanguage.googleapis.com/v1beta/interactions")
  expect_equal(calls[[2]]$headers[["x-goog-api-key"]], "g-key")
})

test_that("provider errors carry the provider's prefix and message", {
  testthat::local_mocked_bindings(
    POST = function(...) structure(list(code = 401L), class = "response"),
    status_code = function(resp) resp$code,
    content = function(resp, ...) list(error = list(message = "invalid x-api-key")),
    .package = "httr"
  )
  provider <- MiroAiAnthropicProvider$new("claude-sonnet-5-5", "bad")
  expect_error(provider$.__enclos_env__$private$executeHttpCall(list()), "Claude API Error: invalid x-api-key", fixed = TRUE)
})

# ==============================================================================
# Amazon Bedrock
# ==============================================================================

bedrockEntry <- function(authMode = "apiKey",
                         models = list(list(label = "Claude Haiku", id = "us.anthropic.claude-haiku-4-5-20251001-v1:0")),
                         ...) {
  modifyList(list(provider = "bedrock", key = "bedrock-api-key", region = "eu-central-1", authMode = authMode, models = models), list(...))
}

MockBedrockCapture <- R6Class("MockBedrockCapture",
  inherit = MiroAiBedrockProvider,
  public = list(
    capturedPayloads = list(),
    mockResponses = list(),
    url = function() private$buildUrl()
  ),
  private = list(
    callCount = 0,
    executeHttpCall = function(payload) {
      private$callCount <- private$callCount + 1
      self$capturedPayloads[[private$callCount]] <- payload
      return(self$mockResponses[[private$callCount]])
    }
  )
)

bedrockResponse <- function(content, stopReason = "end_turn", usage = list(inputTokens = 1L, outputTokens = 1L)) {
  list(output = list(message = list(role = "assistant", content = content)), stopReason = stopReason, usage = usage)
}

newBedrockProvider <- function(entry = bedrockEntry(), model = entry$models[[1]]$id) {
  config <- MiroAiAssistant$new(config = list(apiKeys = list(entry)))$apiKeys$bedrock
  MockBedrockCapture$new(model, config)
}

test_that("Bedrock models reach supportedModels and the provider", {
  assistant <- MiroAiAssistant$new(config = list(apiKeys = list(bedrockEntry())))
  expect_equal(
    assistant$supportedModels$`Amazon Bedrock`,
    c(`Claude Haiku` = "bedrock/us.anthropic.claude-haiku-4-5-20251001-v1:0")
  )
  expect_s3_class(assistant$getProvider("bedrock", "us.anthropic.claude-haiku-4-5-20251001-v1:0"), "MiroAiBedrockProvider")
  # not listed unless configured
  expect_null(MiroAiAssistant$new(apiKeys = list(openai = list(key = "k")))$supportedModels$`Amazon Bedrock`)
})

test_that("Bedrock config validation rejects malformed entries", {
  expectBedrockError <- function(pattern, ...) {
    expect_error(MiroAiAssistant$new(config = list(apiKeys = list(bedrockEntry(...)))), pattern)
  }
  expectBedrockError("region", region = "evil.example.com/x")
  expectBedrockError("authMode", authMode = "sigv4")
  expectBedrockError("promptCaching", models = list(list(id = "m", promptCaching = "no")))
  expectBedrockError("Invalid API key", key = NULL)
  # workload identity needs no key
  config <- MiroAiAssistant$new(config = list(apiKeys = list(bedrockEntry(authMode = "workloadIdentity", key = NULL))))$apiKeys$bedrock
  expect_equal(config$authMode, "workloadidentity")
  expect_null(config$key)
})

test_that("Bedrock builds a Converse payload", {
  provider <- newBedrockProvider()
  provider$mockResponses <- list(bedrockResponse(list(list(text = "Hello."))))
  result <- provider$sendMessage("q", "SYSTEM", tools = list(list(name = "getData", description = "d", parameters = list(type = "object"))))

  payload <- provider$capturedPayloads[[1]]
  expect_equal(result$text, "Hello.")
  cachePoint <- list(cachePoint = list(type = "default"))
  expect_equal(payload$messages[[1]], list(role = "user", content = list(list(text = "q"), cachePoint)))
  expect_equal(payload$system, list(list(text = "SYSTEM"), cachePoint))
  expect_equal(payload$toolConfig$tools[[1]]$toolSpec, list(name = "getData", description = "d", inputSchema = list(json = list(type = "object"))))
  expect_null(payload$inferenceConfig)
  # the model id is one percent-encoded path segment
  expect_equal(provider$url(), "https://bedrock-runtime.eu-central-1.amazonaws.com/model/us.anthropic.claude-haiku-4-5-20251001-v1%3A0/converse")

  # prompt caching can be switched off per model
  provider <- newBedrockProvider(bedrockEntry(models = list(list(id = "meta.llama3-3-70b-instruct-v1:0", promptCaching = FALSE))))
  provider$mockResponses <- list(bedrockResponse(list(list(text = "Hi."))))
  provider$sendMessage("q", "SYSTEM")
  expect_equal(provider$capturedPayloads[[1]]$system, list(list(text = "SYSTEM")))
  expect_equal(provider$capturedPayloads[[1]]$messages[[1]]$content, list(list(text = "q")))
})

test_that("Bedrock drops prompt caching once a model rejects it", {
  bodies <- list()
  testthat::local_mocked_bindings(
    POST = function(url, config, body, ...) {
      bodies[[length(bodies) + 1L]] <<- fromJSON(body, simplifyVector = FALSE)
      structure(list(code = if (length(bodies) == 1L) 400L else 200L), class = "response")
    },
    status_code = function(resp) resp$code,
    content = function(resp, ...) {
      if (resp$code == 400L) list(Message = "You invoked an unsupported model or your request did not allow prompt caching.") else list(ok = TRUE)
    },
    .package = "httr"
  )
  provider <- MiroAiBedrockProvider$new("anthropic.claude-3-haiku-20240307-v1:0", list(key = "k", region = "us-east-1", authMode = "apikey"))
  private <- provider$.__enclos_env__$private
  expect_equal(private$executeHttpCall(private$prepareInitialPayload("q", "sys", NULL)), list(ok = TRUE))
  expect_length(bodies, 2L)
  expect_match(toJSON(bodies[[1]]), "cachePoint", fixed = TRUE)
  expect_false(grepl("cachePoint", toJSON(bodies[[2]]), fixed = TRUE))
  # later requests go without it right away
  private$executeHttpCall(private$buildPayload())
  expect_length(bodies, 3L)
  expect_false(grepl("cachePoint", toJSON(bodies[[3]]), fixed = TRUE))
})

test_that("Bedrock merges consecutive turns and drops blank text of a carried-over history", {
  expect_equal(
    bedrockConverseMessages(list(
      list(role = "user", content = list(list(text = "first"))),
      list(role = "assistant", content = list(list(text = " \n"))),
      list(role = "user", content = list(list(text = "second")))
    )),
    list(list(role = "user", content = list(list(text = "first"), list(text = "second"))))
  )
})

test_that("model choices keep a model id that contains slashes", {
  choice <- "bedrock/arn:aws:bedrock:eu-central-1:123:application-inference-profile/abc"
  expect_equal(aiModelProvider(choice), "bedrock")
  expect_equal(aiModelId(choice), "arn:aws:bedrock:eu-central-1:123:application-inference-profile/abc")
  expect_equal(aiModelId("openai/gpt-6-luna"), "gpt-6-luna")
})

test_that("Bedrock runs tool calls and replays them with their results", {
  provider <- newBedrockProvider()
  toolUse <- list(toolUse = list(toolUseId = "tooluse_1", name = "getData", input = list(
    symbolName = "demand", filters = list(list(column = "region", operator = "=", value = "EU"))
  )))
  provider$mockResponses <- list(
    bedrockResponse(list(toolUse), stopReason = "tool_use"),
    bedrockResponse(list(list(text = "Done.")), usage = list(inputTokens = 7L, outputTokens = 3L))
  )
  toolOutput <- toJSON(data.frame(region = "NA", value = 42), dataframe = "rows", auto_unbox = TRUE)
  received <- NULL
  result <- provider$sendMessage("q", "sys",
    tools = list(list(name = "getData", description = "d", parameters = list())),
    toolDispatcher = list(getData = function(args) {
      received <<- args
      toolOutput
    })
  )

  expect_equal(result$text, "Done.")
  expect_equal(result$inputTokens, 8L)
  expect_equal(received$symbolName, "demand")
  expect_true(is.data.frame(received$filters))
  sent <- provider$capturedPayloads[[2]]$messages
  expect_equal(sent[[2]], list(role = "assistant", content = list(toolUse)))
  expect_equal(sent[[3]]$content[[1]]$toolResult$toolUseId, "tooluse_1")
  # the replayed history itself carries no cache point
  expect_length(provider$.__enclos_env__$self$history[[3]]$content, 1L)
  expect_equal(sent[[3]]$content[[1]]$toolResult$content[[1]]$text, as.character(toolOutput))
})

test_that("Bedrock keeps reasoning within a turn and drops it from earlier turns", {
  provider <- newBedrockProvider()
  reasoning <- list(reasoningContent = list(reasoningText = list(text = "", signature = "sig")))
  toolUse <- list(toolUse = list(toolUseId = "tooluse_1", name = "getData", input = list()))
  provider$mockResponses <- list(
    bedrockResponse(list(reasoning, toolUse), stopReason = "tool_use"),
    bedrockResponse(list(reasoning, list(text = "Done."))),
    bedrockResponse(list(list(text = "Again.")))
  )
  tools <- list(list(name = "getData", description = "d", parameters = list()))
  provider$sendMessage("q", "sys", tools = tools, toolDispatcher = list(getData = function(args) "[]"))
  expect_equal(provider$capturedPayloads[[2]]$messages[[2]]$content, list(reasoning, toolUse))

  provider$sendMessage("q2", "sys2", tools = tools)
  sent <- provider$capturedPayloads[[3]]$messages
  expect_equal(sent[[2]]$content, list(toolUse))
  expect_equal(sent[[4]]$content, list(list(text = "Done.")))
})

test_that("Bedrock surfaces truncation and guardrails, and restores a prior chat", {
  provider <- newBedrockProvider()
  provider$mockResponses <- list(bedrockResponse(list(list(text = "partial")), stopReason = "max_tokens"))
  result <- provider$sendMessage("q", "sys")
  expect_match(result$text, "truncated")

  provider <- newBedrockProvider()
  provider$mockResponses <- list(bedrockResponse(list(list(text = "Sorry.")), stopReason = "guardrail_intervened"))
  result <- provider$sendMessage("q", "sys")
  expect_match(result$text, "content safety policy of Amazon Bedrock")

  provider <- newBedrockProvider()
  provider$mockResponses <- list(bedrockResponse(list(list(text = "Again."))))
  provider$chatHistory <- list(list(role = "user", text = "first"), list(role = "assistant", text = "answer"), list(role = "user", text = "q"))
  provider$sendMessage("q", "sys")
  expect_equal(provider$capturedPayloads[[1]]$messages[[2]], list(role = "assistant", content = list(list(text = "answer"))))
})

test_that("awsSigV4Headers matches the AWS reference signer", {
  # signature computed with botocore's SigV4Auth for the same request
  headers <- awsSigV4Headers(
    list(accessKeyId = "AKIDEXAMPLE", secretAccessKey = "wJalrXUtnFEMI/K7MDENG+bPxRfiCYEXAMPLEKEY", sessionToken = "SESSIONTOKEN"), # pragma: allowlist secret
    "eu-central-1", "bedrock", "bedrock-runtime.eu-central-1.amazonaws.com",
    "/model/arn%3Aaws%3Abedrock%3Aeu-central-1%3A123%3Ainference-profile%2Feu.anthropic.claude-haiku-4-5-v1%3A0/converse",
    "{\"messages\":[{\"role\":\"user\",\"content\":[{\"text\":\"hé\"}]}]}",
    time = as.POSIXct("2026-11-04 16:15:38", tz = "UTC")
  )
  expect_equal(headers$`x-amz-date`, "20261104T161538Z")
  expect_equal(headers$`x-amz-security-token`, "SESSIONTOKEN")
  expect_equal(
    headers$Authorization,
    paste0(
      "AWS4-HMAC-SHA256 Credential=AKIDEXAMPLE/20261104/eu-central-1/bedrock/aws4_request, ",
      "SignedHeaders=content-type;host;x-amz-date;x-amz-security-token, ",
      "Signature=a1309cbbede393ccb148e63be47a0da6ea89f1e567984468240f23a4f9bb67cd"
    )
  )
})

test_that("Bedrock authenticates with the API key or signs with IRSA credentials", {
  sent <- list()
  testthat::local_mocked_bindings(
    POST = function(url, config, body, ...) {
      sent[[length(sent) + 1L]] <<- list(url = url, headers = config$headers, body = body)
      structure(list(code = 200L), class = "response")
    },
    status_code = function(resp) resp$code,
    content = function(resp, ...) list(ok = TRUE),
    .package = "httr"
  )
  apiKeyProvider <- MiroAiBedrockProvider$new("m", list(key = "bedrock-api-key", region = "us-east-1", authMode = "apikey"))
  apiKeyProvider$.__enclos_env__$private$executeHttpCall(list(a = 1))
  expect_equal(sent[[1]]$headers[["Authorization"]], "Bearer bedrock-api-key")
  expect_equal(sent[[1]]$url, "https://bedrock-runtime.us-east-1.amazonaws.com/model/m/converse")

  irsaProvider <- MiroAiBedrockProvider$new("m", list(region = "us-east-1", authMode = "workloadidentity"))
  irsaProvider$.__enclos_env__$private$credentials <- list(getCredentials = function() {
    list(accessKeyId = "AKID", secretAccessKey = "secret", sessionToken = "token") # pragma: allowlist secret
  })
  irsaProvider$.__enclos_env__$private$executeHttpCall(list(a = 1))
  expect_match(sent[[2]]$headers[["Authorization"]], "^AWS4-HMAC-SHA256 Credential=AKID/[0-9]{8}/us-east-1/bedrock/aws4_request")
  expect_equal(sent[[2]]$headers[["x-amz-security-token"]], "token")
  # the signed body is the one that is sent
  expect_equal(as.character(sent[[2]]$body), '{"a":1}')
})

test_that("IRSA exchanges the projected token for cached temporary credentials", {
  withr::local_envvar(AWS_REGION = NA)
  tokenFile <- tempfile()
  writeLines("projected-jwt", tokenFile)
  requests <- list()
  testthat::local_mocked_bindings(
    POST = function(url, body, ...) {
      requests[[length(requests) + 1L]] <<- list(url = url, body = body)
      structure(list(code = 200L), class = "response")
    },
    status_code = function(resp) resp$code,
    content = function(resp, ...) {
      list(AssumeRoleWithWebIdentityResponse = list(AssumeRoleWithWebIdentityResult = list(Credentials = list(
        AccessKeyId = "ASIA", SecretAccessKey = "secret", SessionToken = "token", # pragma: allowlist secret
        Expiration = as.numeric(Sys.time()) + 3600
      ))))
    },
    .package = "httr"
  )
  credentials <- MiroAwsWebIdentityCredentials$new("eu-central-1", "arn:aws:iam::123:role/miro", tokenFile)
  expect_equal(credentials$getCredentials(), list(accessKeyId = "ASIA", secretAccessKey = "secret", sessionToken = "token")) # pragma: allowlist secret
  credentials$getCredentials()
  expect_length(requests, 1L)
  expect_equal(requests[[1]]$url, "https://sts.eu-central-1.amazonaws.com/")
  expect_equal(requests[[1]]$body$Action, "AssumeRoleWithWebIdentity")
  expect_equal(requests[[1]]$body$RoleArn, "arn:aws:iam::123:role/miro")
  expect_equal(requests[[1]]$body$WebIdentityToken, "projected-jwt")
})

test_that("IRSA failures surface STS' message, and a missing setup is explained", {
  tokenFile <- tempfile()
  writeLines("projected-jwt", tokenFile)
  testthat::local_mocked_bindings(
    POST = function(...) structure(list(code = 400L), class = "response"),
    status_code = function(resp) resp$code,
    content = function(resp, ...) list(Error = list(Code = "InvalidIdentityToken", Message = "Missing a required claim: iss")),
    .package = "httr"
  )
  credentials <- MiroAwsWebIdentityCredentials$new("eu-central-1", "arn:aws:iam::123:role/miro", tokenFile)
  expect_error(credentials$getCredentials(), "AWS authentication failed: Missing a required claim: iss", fixed = TRUE)
  expect_error(
    MiroAwsWebIdentityCredentials$new("eu-central-1", "", "")$getCredentials(),
    "AWS_ROLE_ARN or AWS_WEB_IDENTITY_TOKEN_FILE",
    fixed = TRUE
  )
})

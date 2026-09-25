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

  expect_equal(assistant$apiKeys$openai, "sk-fake-openai")
  expect_equal(assistant$apiKeys$anthropic, "sk-fake-anthropic")
  expect_null(assistant$apiKeys$google)
})

test_that("MiroAiAssistant filters supportedModels based on loaded keys", {
  # Only Google key provided
  assistant <- MiroAiAssistant$new(apiKeys = list(google = "fake-key"))

  supported <- assistant$supportedModels
  expect_true("google" %in% tolower(names(supported)))
  expect_false("openai" %in% tolower(names(supported)))
  expect_false("anthropic" %in% tolower(names(supported)))
})

test_that("MiroAiAssistant getProvider returns correct R6 instances", {
  assistant <- MiroAiAssistant$new(apiKeys = list(
    openai = "sk-openai",
    google = "sk-google"
  ))

  # Valid providers
  openai_provider <- assistant$getProvider("openai", "gpt-6-sol")
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

  anthropic <- MockAnthropicProvider$new("claude-sonnet-5", "key")

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
  provider <- MockAnthropicCapture$new("claude-sonnet-5", "key")
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
  provider <- MockAnthropicCapture$new("claude-sonnet-5", "key")
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

test_that("MiroAiAnthropicProvider errors instead of continuing when a tool call is truncated", {
  provider <- MockAnthropicCapture$new("claude-sonnet-5", "key")
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

  provider <- MockAnthropicCapture$new("claude-sonnet-5", "key")
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
  provider <- MockAnthropicCapture$new("claude-sonnet-5", "key")
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
  provider <- MockAnthropicCapture$new("claude-sonnet-5", "key")
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

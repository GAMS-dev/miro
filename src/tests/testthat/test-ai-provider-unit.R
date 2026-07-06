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
  openai_provider <- assistant$getProvider("openai", "gpt-5.5-pro")
  google_provider <- assistant$getProvider("google", "gemini-3.5-flash")

  expect_true(inherits(openai_provider, "MiroAiOpenAiProvider"))
  expect_true(inherits(google_provider, "MiroAiGoogleProvider"))

  # Invalid provider/model combos
  expect_error(assistant$getProvider("anthropic", "claude-fable-5"), "Unknown provider")
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

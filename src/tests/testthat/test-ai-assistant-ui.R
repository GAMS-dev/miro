test_that(
  "AI Output Analysis chat works",
  retryUiTest(as.integer(Sys.getenv("MIRO_MAX_RETRIES", "3")), {
    createTestDb()

    testModel <- "transport"
    testModelPath <- file.path(testDir, "model", testModel)
    Sys.setenv(MIRO_MODEL_PATH = file.path(testModelPath, paste0(testModel, ".gms")))
    Sys.setenv(MIRO_MODE = "base")
    # a syntactically valid but non-functional key is enough: it activates the
    # AI assistant module so the UI can be exercised, and lets us assert on
    # the (deterministic) error path the app takes when the provider rejects
    # the request, without depending on real credentials or a mocked backend
    Sys.setenv(MIRO_AI_ASSISTANT_CONFIG = jsonlite::toJSON(list(
      apiKeys = list(list(provider = "anthropic", key = "sk-ant-test-invalid-key-0000000000"))
    ), auto_unbox = TRUE))
    modelDataPath <- file.path(testModelPath, paste0("data_", testModel))

    file.copy2(
      file.path(testDir, "data", "transport.gdx"),
      file.path(modelDataPath, "default.gdx")
    )

    source(file.path(testDir, "shinytest", "ai_chat_output_analysis_test.R"), local = TRUE)

    unlink(modelDataPath, recursive = TRUE, force = TRUE)

    Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE", "MIRO_AI_ASSISTANT_CONFIG"))
  })
)

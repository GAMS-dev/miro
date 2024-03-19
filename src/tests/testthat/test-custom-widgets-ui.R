test_that(
  "Custom widgets work",
  try_again(as.integer(Sys.getenv("MIRO_MAX_RETRIES", "3")), {
    createTestDb()

    Sys.setenv("MIRO_MODE" = "base")

    Sys.setenv(MIRO_MODEL_PATH = file.path(
      getwd(), "..", "model", "sudoku_custom_widget",
      "sudoku.gms"
    ))

    source(file.path(testDir, "shinytest", "custom_widgets_multiple_symbols_test.R"), local = TRUE)

    skip_if(
      identical(Sys.getenv("ENGINE_URL"), ""),
      "Skipping custom widget scalar test as no ENGINE_URL was not set."
    )

    Sys.setenv(MIRO_MODEL_PATH = file.path(
      getwd(), "..", "model", "sudoku_custom_widget_scalar",
      "sudoku.gms"
    ))

    Sys.setenv(MIRO_REMOTE_EXEC = "true")
    Sys.setenv("MIRO_REMOTE_EXEC_URL" = Sys.getenv("ENGINE_URL"))
    Sys.setenv("MIRO_REMOTE_EXEC_USERNAME" = Sys.getenv("ENGINE_USER"))
    Sys.setenv("MIRO_REMOTE_EXEC_TOKEN" = getEngineToken(Sys.getenv("ENGINE_URL"), Sys.getenv("ENGINE_USER"), Sys.getenv("ENGINE_PASSWORD")))
    Sys.setenv("MIRO_REMOTE_EXEC_NS" = Sys.getenv("ENGINE_NS"))

    if (file.exists(file.path("~", ".miro", ".cred_sudoku"))) {
      unlink(file.path("~", ".miro", ".cred_sudoku"), force = TRUE)
    }

    createTestDb()

    source(file.path(testDir, "shinytest", "custom_widgets_scalars_test.R"), local = TRUE)

    Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE", "MIRO_REMOTE_EXEC"))
  })
)

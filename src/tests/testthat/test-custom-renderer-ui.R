test_that(
  "Custom renderers work",
  try_again(as.integer(Sys.getenv("MIRO_MAX_RETRIES", "3")), {
    skip_if(
      identical(Sys.getenv("GAMS_SYS_DIR"), ""),
      "GAMS_SYS_DIR environment variable not set. Skipping tests."
    )

    createTestDb()

    testModelPath <- file.path(testDir, "model", "transport_custom_map")
    Sys.setenv(MIRO_MODEL_PATH = file.path(
      testModelPath,
      "transport_custom_map.gms"
    ))
    Sys.setenv(MIRO_MODE = "base")
    if (!identical(Sys.getenv("MIRO_TEST_GAMS_LICE"), "")) {
      saveAdditionalGamsClArgs(
        testModelPath, "transport_custom_map",
        paste0('license="', Sys.getenv("MIRO_TEST_GAMS_LICE"), '"')
      )
    }
    source(file.path(testDir, "shinytest", "multiple_symbol_renderer.R"), local = TRUE)
    if (!identical(Sys.getenv("MIRO_TEST_GAMS_LICE"), "")) {
      file.rename(
        file.path(testModelPath, "conf_transport_custom_map", "transport_custom_map_tmp.json"),
        file.path(testModelPath, "conf_transport_custom_map", "transport_custom_map.json")
      )
    }

    Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE"))
  })
)

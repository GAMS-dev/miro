test_that(
  "Custom renderers for input symbols work",
  try_again(as.integer(Sys.getenv("MIRO_MAX_RETRIES", "3")), {
    skip_if(
      identical(Sys.getenv("GAMS_SYS_DIR"), ""),
      "GAMS_SYS_DIR environment variable not set. Skipping tests."
    )

    createTestDb()

    testModelPath <- file.path(testDir, "model", "transport")
    Sys.setenv(MIRO_MODEL_PATH = file.path(
      testModelPath,
      "transport.gms"
    ))
    file.copy(
      file.path(testModelPath, "conf_transport", "transport.json"),
      file.path(testModelPath, "conf_transport", "transport_tmp.json"),
      overwrite = TRUE
    )
    configJSON <- suppressWarnings(jsonlite::fromJSON(file.path(testModelPath, "conf_transport", "transport_tmp.json"),
      simplifyDataFrame = FALSE,
      simplifyMatrix = FALSE
    ))
    configJSON$dataRendering$a <- list(
      outType = "mirorenderer_a",
      additionalData = c("d", "type")
    )
    jsonlite::write_json(configJSON, file.path(testModelPath, "conf_transport", "transport.json"),
      pretty = TRUE, auto_unbox = TRUE, null = "null"
    )
    source(file.path(testDir, "shinytest", "multiple_symbol_renderer_input.R"), local = TRUE)
    file.rename(
      file.path(testModelPath, "conf_transport", "transport_tmp.json"),
      file.path(testModelPath, "conf_transport", "transport.json")
    )

    Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE"))
  })
)

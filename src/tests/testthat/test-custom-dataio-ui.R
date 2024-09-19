test_that(
  "Custom dataIO works",
  try_again(as.integer(Sys.getenv("MIRO_MAX_RETRIES", "3")), {
    skip_if(
      identical(Sys.getenv("GAMS_SYS_DIR"), ""),
      "GAMS_SYS_DIR environment variable not set. Skipping tests."
    )

    createTestDb()

    modelToTest <- "transport"
    testModelDir <- file.path(testDir, "model", modelToTest)
    modelDataPath <- file.path(testModelDir, paste0("data_", modelToTest))
    configJSONFileName <- file.path(
      testModelDir, paste0("conf_", modelToTest),
      paste0(modelToTest, ".json")
    )

    Sys.setenv(MIRO_MODEL_PATH = file.path(testModelDir, paste0(modelToTest, ".gms")))
    Sys.setenv(MIRO_MODE = "base")

    file.copy(file.path(dirname(configJSONFileName), paste0(tolower(modelToTest), ".json")),
      file.path(dirname(configJSONFileName), paste0(tolower(modelToTest), "_tmp.json")),
      overwrite = TRUE
    )

    configJSON <- suppressWarnings(jsonlite::fromJSON(configJSONFileName,
      simplifyDataFrame = FALSE,
      simplifyMatrix = FALSE
    ))
    configJSON$customDataImport <- list(
      list(
        label = "Importer without file",
        symNames = c("a", "b", "f", "beta"),
        functionName = "miroimport_noFile"
      ),
      list(
        label = "Importer with file",
        localFileInput = list(
          label = "Please upload your files here",
          multiple = TRUE,
          accept = c(".csv", "text/csv")
        ),
        symNames = "b",
        functionName = "miroimport_withFile"
      ),
      list(
        label = "JSON import",
        localFileInput = list(
          label = "Please upload your JSON file here",
          multiple = FALSE,
          accept = c(".json", "application/json")
        ),
        functionName = "miroimport_JSON"
      ),
      list(
        label = "Importer without file (scalars)",
        symNames = c("_scalars"),
        functionName = "miroimport_noFile2"
      )
    )
    configJSON$customDataExport <- list(
      list(
        label = "My custom exporter",
        symNames = c("a", "schedule"),
        functionName = "miroexport_test"
      ),
      list(
        label = "JSON export",
        functionName = "miroexport_JSON",
        localFileOutput = list(
          filename = "output.json",
          contentType = "application/json"
        )
      )
    )
    jsonlite::write_json(configJSON, configJSONFileName, pretty = TRUE, auto_unbox = TRUE, null = "null")

    source(file.path(testDir, "shinytest", "custom_importer_test.R"), local = TRUE)

    source(file.path(testDir, "shinytest", "custom_exporter_test.R"), local = TRUE)

    file.move(
      file.path(dirname(configJSONFileName), paste0(tolower(modelToTest), "_tmp.json")),
      file.path(dirname(configJSONFileName), paste0(tolower(modelToTest), ".json"))
    )

    Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE"))
  })
)

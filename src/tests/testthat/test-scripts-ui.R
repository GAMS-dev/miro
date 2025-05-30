test_that(
  "Analysis scripts in Base Mod work",
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
    Sys.setenv(PATH = paste0(Sys.getenv("GAMS_SYS_DIR"), .Platform$path.sep, Sys.getenv("PATH")))
    Sys.setenv(MIRO_MODE = "base")

    file.copy(file.path(dirname(configJSONFileName), paste0(tolower(modelToTest), ".json")),
      file.path(dirname(configJSONFileName), paste0(tolower(modelToTest), "_tmp.json")),
      overwrite = TRUE
    )

    configJSON <- suppressWarnings(jsonlite::fromJSON(configJSONFileName,
      simplifyDataFrame = FALSE,
      simplifyMatrix = FALSE
    ))
    testFile <- file.path(testDir, "bla.txt")
    unlink(testFile)
    licenseFileArg <- character()
    if (!identical(Sys.getenv("MIRO_TEST_GAMS_LICE"), "")) {
      licenseFileArg <- paste0("license=", gsub("\\", "/", Sys.getenv("MIRO_TEST_GAMS_LICE"), fixed = TRUE))
    }
    configJSON$scripts <- list(base = list(
      list(
        tabTitle = "asd", id = "script1", command = "gams",
        args = c(
          "script1.gms", paste0("--testfile=", testFile),
          licenseFileArg
        ),
        outputFile = "test.md",
        markdown = FALSE
      ),
      list(
        tabTitle = "def", id = "script2", command = "gams",
        args = c("script2.gms", licenseFileArg),
        outputFile = "test.md",
        markdown = TRUE
      )
    ), hcube = list(
      list(
        title = "bla123", id = "batch1", command = "gams",
        args = c("script_batch.gms", licenseFileArg),
        outputFile = "test.md",
        markdown = TRUE
      )
    ))
    jsonlite::write_json(configJSON, configJSONFileName, pretty = TRUE, auto_unbox = TRUE, null = "null")

    source(file.path(testDir, "shinytest", "base_mode_scripts_test.R"), local = TRUE)

    source(file.path(testDir, "shinytest", "batch_scripts_test.R"), local = TRUE)

    file.copy(file.path(dirname(configJSONFileName), paste0(tolower(modelToTest), "_tmp.json")),
      file.path(dirname(configJSONFileName), paste0(tolower(modelToTest), ".json")),
      overwrite = TRUE
    )

    Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE"))
  })
)

context("UI tests - Custom importer")
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
configJSON$remoteImport <- list(
  list(
    name = "Importer without file",
    templates = list(
      list(
        symNames = c("a", "b"),
        source = "customFunction",
        functionName = "customImporter"
      )
    )
  ),
  list(
    name = "Importer with file",
    localFileInput = list(
      label = "Please upload your files here",
      multiple = TRUE,
      accept = c(".csv", "text/csv")
    ),
    templates = list(
      list(
        symNames = "b",
        source = "customFunction",
        functionName = "customImporterWithFile"
      )
    )
  )
)
jsonlite::write_json(configJSON, configJSONFileName, pretty = TRUE, auto_unbox = TRUE, null = "null")

test_that(
  "Custom importer functions work",
  expect_pass(testApp(file.path(testDir, ".."), "custom_importer_test",
    compareImages = FALSE
  ))
)

file.move(
  file.path(dirname(configJSONFileName), paste0(tolower(modelToTest), "_tmp.json")),
  file.path(dirname(configJSONFileName), paste0(tolower(modelToTest), ".json"))
)

Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE"))

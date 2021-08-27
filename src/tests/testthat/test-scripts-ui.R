context("UI tests - Analysis scripts in Base Mode")
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
))
jsonlite::write_json(configJSON, configJSONFileName, pretty = TRUE, auto_unbox = TRUE, null = "null")

test_that(
  "Analysis scripts in Base Mode work",
  expect_pass(testApp(file.path(testDir, ".."), "base_mode_scripts_test",
    compareImages = FALSE
  ))
)

file.copy(file.path(dirname(configJSONFileName), paste0(tolower(modelToTest), "_tmp.json")),
  file.path(dirname(configJSONFileName), paste0(tolower(modelToTest), ".json")),
  overwrite = TRUE
)

Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE"))

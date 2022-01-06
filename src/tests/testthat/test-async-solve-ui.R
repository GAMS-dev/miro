context("UI tests - asynchronous solve")
skip_if(
  identical(Sys.getenv("ENGINE_URL"), ""),
  "Skipping asynchronous solve tests as ENGINE_URL was not set."
)
skip_if(
  identical(Sys.getenv("ENGINE_USER"), ""),
  "Skipping asynchronous solve tests as ENGINE_USER was not set."
)
skip_if(
  identical(Sys.getenv("ENGINE_PASSWORD"), ""),
  "Skipping asynchronous solve tests as ENGINE_PASSWORD was not set."
)
skip_if(
  identical(Sys.getenv("ENGINE_NS"), ""),
  "Skipping asynchronous solve tests as ENGINE_NS was not set."
)

createTestDb()

apiURL <- Sys.getenv("ENGINE_URL")
inviterUser <- Sys.getenv("ENGINE_USER")
inviterPass <- Sys.getenv("ENGINE_PASSWORD")
namespace <- Sys.getenv("ENGINE_NS")

inviteeName <- paste0(Sys.getenv("ENGINE_USER"), "_", round(runif(1, 1, 100000)))
Sys.setenv(ENGINE_USER_INVITEE = inviteeName)

createUser(apiURL, inviterUser, inviterPass, namespace,
  inviteeName, inviterPass,
  volumeQuota = 165L
)

modelToTest <- "transport"
modelToTestUpper <- paste0(toupper(substr(modelToTest, 1, 1)), substring(modelToTest, 2))
testModelDir <- file.path(testDir, "model", modelToTest)
modelDataPath <- file.path(testModelDir, paste0("data_", modelToTest))
configJSONFileName <- file.path(
  testModelDir, paste0("conf_", modelToTest),
  paste0(modelToTest, ".json")
)

file.move(
  file.path(testModelDir, paste0(modelToTest, ".gms")),
  file.path(testModelDir, paste0("bk_", modelToTest, ".gms"))
)
file.move(
  file.path(testModelDir, paste0(modelToTest, "_files.txt")),
  file.path(testModelDir, paste0("bk_", modelToTest, "_files.txt"))
)
file.copy(
  file.path(testModelDir, paste0("bk_", modelToTest, ".gms")),
  file.path(testModelDir, paste0(modelToTestUpper, ".gms"))
)
modelToTest <- modelToTestUpper
Sys.setenv(MIRO_MODEL_PATH = file.path(testModelDir, paste0(modelToTest, ".gms")))
Sys.setenv(MIRO_MODE = "base")
Sys.setenv(MIRO_REMOTE_EXEC = "true")

writeLines(
  paste0(modelToTest, ".gms"),
  file.path(testModelDir, paste0(tolower(modelToTest), "_files.txt"))
)

# add --sleep=1 to extraClArgs and hide log and lst file
file.copy(configJSONFileName, file.path(
  dirname(configJSONFileName),
  paste0(tolower(modelToTest), "_tmp.json")
), overwrite = TRUE)
configJSON <- suppressWarnings(jsonlite::fromJSON(configJSONFileName,
  simplifyDataFrame = FALSE,
  simplifyMatrix = FALSE
))
configJSON$extraClArgs <- c(configJSON$extraClArgs, "--sleep=1", "--largeLog=1")
configJSON$activateModules$logFile <- FALSE
configJSON$activateModules$lstFile <- FALSE
jsonlite::write_json(configJSON, configJSONFileName, pretty = TRUE, auto_unbox = TRUE, null = "null")
file.copy2(file.path(testDir, "data", "transport.gdx"), file.path(modelDataPath, "default.gdx"))

if (file.exists(file.path("~", ".miro", paste0(".cred_", tolower(modelToTest))))) {
  unlink(file.path("~", ".miro", paste0(".cred_", tolower(modelToTest))), force = TRUE)
}

test_that(
  "Solve asynchronously with GAMS MIRO Engine works",
  expect_pass(testApp(file.path(testDir, ".."), "async_solve_test",
    compareImages = FALSE
  ))
)

removeUser(apiURL, inviterUser, inviterPass, inviteeName)

file.rename(
  file.path(dirname(configJSONFileName), paste0(tolower(modelToTest), "_tmp.json")),
  file.path(dirname(configJSONFileName), paste0(tolower(modelToTest), ".json"))
)

unlink(file.path(testModelDir, paste0(modelToTestUpper, ".gms")),
  recursive = TRUE, force = FALSE
)
file.move(
  file.path(testModelDir, paste0("bk_", tolower(modelToTest), ".gms")),
  file.path(testModelDir, paste0(tolower(modelToTest), ".gms"))
)
file.move(
  file.path(testModelDir, paste0("bk_", tolower(modelToTest), "_files.txt")),
  file.path(testModelDir, paste0(tolower(modelToTest), "_files.txt"))
)

unlink(modelDataPath, recursive = TRUE, force = TRUE)

Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE", "MIRO_REMOTE_EXEC"))

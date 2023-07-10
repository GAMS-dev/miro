context("UI tests - Hypercube Module")
skip_if(
  identical(Sys.getenv("ENGINE_URL"), ""),
  "Skipping asynchronous solve tests as no ENGINE_URL was not set."
)
skip_if(
  identical(Sys.getenv("ENGINE_USER"), ""),
  "Skipping asynchronous solve tests as no ENGINE_USER was not set."
)
skip_if(
  identical(Sys.getenv("ENGINE_PASSWORD"), ""),
  "Skipping asynchronous solve tests as no ENGINE_PASSWORD was not set."
)
skip_if(
  identical(Sys.getenv("ENGINE_NS"), ""),
  "Skipping asynchronous solve tests as no ENGINE_NS was not set."
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
  volumeQuota = 72L
)

modelToTest <- "pickstock_configuration"
testModelDir <- file.path(testDir, "model", modelToTest)
modelDataPath <- file.path(testModelDir, paste0("data_", modelToTest))
configJSONFileName <- file.path(
  testModelDir, paste0("conf_", modelToTest),
  paste0(modelToTest, ".json")
)
# END setup

Sys.setenv(MIRO_MODEL_PATH = file.path(testModelDir, paste0(modelToTest, ".gms")))
Sys.setenv(MIRO_MODEL_NAME = modelToTest)
Sys.setenv(MIRO_REMOTE_EXEC = "true")

# activate local upload module, deactivate
file.copy(file.path(dirname(configJSONFileName), paste0(tolower(modelToTest), "_expected.json")),
  file.path(dirname(configJSONFileName), paste0(tolower(modelToTest), ".json")),
  overwrite = TRUE
)
configJSON <- suppressWarnings(jsonlite::fromJSON(configJSONFileName,
  simplifyDataFrame = FALSE,
  simplifyMatrix = FALSE
))
configJSON$activateModules$loadLocal <- TRUE
configJSON$activateModules$hcube <- TRUE
configJSON$activateModules$attachments <- TRUE
configJSON$extraClArgs <- c(configJSON$extraClArgs, "--sleep=1")

configJSON$inputWidgets[["_gmspar_sliderrange"]]$noHcube <- FALSE
configJSON$inputWidgets[["trainingdays"]]$noHcube <- TRUE
configJSON$inputWidgets[["_gmsopt_LstTitleLeftAligned"]] <- configJSON$inputWidgets[["_gmsopt_checkbox"]]
configJSON$inputWidgets[["_gmsopt_LstTitleLeftAligned"]]$noHcube <- FALSE
configJSON$inputWidgets[["_gmsopt_checkbox"]] <- NULL
configJSON$outputAttachments <- list(list(
  filename = "dowjones2016.csv",
  execPerm = TRUE, throwError = FALSE
))

configJSON$scripts$hcube <- list(list(
  title = "Test analysis",
  id = "script1",
  command = "gams",
  args = c("test_script.gms", "--testVar", "test"),
  outputFile = "out.txt"
))

jsonlite::write_json(configJSON, configJSONFileName, pretty = TRUE, auto_unbox = TRUE, null = "null")

if (file.exists(file.path("~", ".miro", paste0(".cred_", tolower(modelToTest))))) {
  unlink(file.path("~", ".miro", paste0(".cred_", tolower(modelToTest))), force = TRUE)
}

test_that(
  "Solving Hypercube jobs works",
  {
    source(file.path(testDir, "shinytest", "hcube_module_solve_test.R"), local = TRUE)
  }
)

test_that(
  "Loading Hypercube jobs works",
  {
    source(file.path(testDir, "shinytest", "hcube_module_load_test.R"), local = TRUE)
  }
)

test_that(
  "Compare Mode works with Hypercube jobs",
  {
    source(file.path(testDir, "shinytest", "hcube_module_compare_test.R"), local = TRUE)
  }
)

removeUser(apiURL, inviterUser, inviterPass, inviteeName)

Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_REMOTE_EXEC"))

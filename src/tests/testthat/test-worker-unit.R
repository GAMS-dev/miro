context("Unit tests - worker class")

source("../../components/worker.R")

worker <- Worker$new(
  metadata = list(
    uid = "test", modelName = "pickstock", noNeedCred = FALSE,
    maxSizeToRead = 5000,
    modelDataFiles = c(MIROGdxInName, MIROGdxOutName),
    MIROGdxInName = MIROGdxInName,
    clArgs = c(
      paste0("execMode=", gamsExecMode),
      paste0('IDCGDXOutput="', MIROGdxOutName, '"')
    ),
    textEntries = c("pickstock.lst"),
    gamsExecMode = gamsExecMode,
    extraClArgs = "MIP=CBC",
    saveTraceFile = TRUE,
    modelGmsName = "pickstock.gms", gamsSysDir = Sys.getenv("GAMS_SYS_DIR"), csvDelim = ",",
    timeout = 10L, serverOS = getOS(),
    modelData = file.path(getwd(), "..", "model", "pickstock", "pickstock.zip"),
    rememberMeFileName = "",
    hiddenLogFile = FALSE
  ),
  remote = TRUE
)
privateWorkerEnv <- environment(worker$getCredentials)$private

test_that("Resolving Engine url works", {
  expect_error(privateWorkerEnv$resolveRemoteURL("http://localhost-asd"))
  expect_identical(privateWorkerEnv$resolveRemoteURL("http://localhost"), "http://localhost")
  expect_identical(privateWorkerEnv$resolveRemoteURL("http://localhost:123"), "http://localhost:123")
  expect_identical(privateWorkerEnv$resolveRemoteURL("localhost"), "http://localhost")
  expect_identical(privateWorkerEnv$resolveRemoteURL("localhost/"), "http://localhost")
  expect_identical(privateWorkerEnv$resolveRemoteURL("localhost:123"), "http://localhost:123")
  expect_identical(privateWorkerEnv$resolveRemoteURL("http://localhost/asd"), "http://localhost/asd")
  expect_identical(privateWorkerEnv$resolveRemoteURL("localhost/asd"), "http://localhost/asd")
  expect_identical(privateWorkerEnv$resolveRemoteURL("miro.gams.com/engine/"), "https://miro.gams.com/engine")
  expect_identical(privateWorkerEnv$resolveRemoteURL("localhost-asd:123/asd/def"), "https://localhost-asd:123/asd/def")
  expect_identical(privateWorkerEnv$resolveRemoteURL("localhost-asd"), "https://localhost-asd")
  expect_error(privateWorkerEnv$resolveRemoteURL("ftp://miro.gams.com/engine/"))
})

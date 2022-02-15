context("Integration test - App deployment")
library(processx)
library(zip)

createTestDb()

modelToTest <- "pickstock"
testModelDir <- file.path(testDir, "model", modelToTest)

# use non-ASCII characters in model path
testModelPath <- file.path(testDir, "model", paste0(modelToTest, "_dä plöy"))
Sys.setenv(MIRO_MODEL_PATH = file.path(testModelPath, paste0(modelToTest, ".gms")))

# can be set directly in process$new call when fixed in processx package: https://github.com/r-lib/processx/pull/268
Sys.setenv(MIRO_BUILD = "true")
Sys.setenv(RE_SHINY_PATH = ".")
Sys.setenv(RE_SHINY_PORT = "9876")
Sys.setenv(R_LIB_PATHS = .libPaths()[[1]])
Sys.unsetenv("MIRO_REMOTE_EXEC")

expect_deploy_works <- function(useTemp = TRUE, buildArchive = TRUE, miroMode = "base", manipulate = NULL) {
  if (file.exists(testModelPath)) {
    if (unlink(testModelPath, recursive = TRUE, force = TRUE)) {
      stop(paste0("Could not remove old ", testModelPath, " directory for tests"))
    }
  }
  dir.create(testModelPath)
  file.copy(file.path(testDir, "model", modelToTest, "."),
    testModelPath,
    overwrite = TRUE, recursive = TRUE
  )
  file.copy(file.path(testDir, "data", "täst"),
    file.path(testModelPath, "."),
    overwrite = TRUE, recursive = TRUE
  )
  file.copy(file.path(testDir, "data", "pickstock_files_täst.txt"),
    file.path(testModelPath, "pickstock_files.txt"),
    overwrite = TRUE
  )
  if (identical(manipulate, "noAssembly") || identical(manipulate, "emptyAssembly") ||
    identical(manipulate, "wrongAssembly")) {
    file.remove(file.path(testModelPath, paste0(modelToTest, "_files.txt")))
  }
  if (identical(manipulate, "emptyAssembly") || identical(manipulate, "wrongAssembly")) {
    file.create(file.path(testModelPath, paste0(modelToTest, "_files.txt")))
  }
  if (identical(manipulate, "wrongAssembly")) {
    writeLines(c("Hello", "World"), file.path(testModelPath, paste0(modelToTest, "_files.txt")))
  }

  Sys.setenv(MIRO_USE_TMP = if (useTemp) "true" else "false")
  Sys.setenv(MIRO_BUILD_ARCHIVE = if (buildArchive) "true" else "false")
  Sys.setenv(MIRO_MODE = miroMode)

  deployProc <- process$new(file.path(R.home("bin"), "R"),
    c("-f", "start-shiny.R"),
    wd = file.path(testDir, ".."),
    stderr = "|",
    stdout = "|"
    # env = c(MIRO_MODEL_PATH = Sys.getenv("MIRO_MODEL_PATH"),
    #         MIRO_BUILD = "true",
    #         RE_SHINY_PATH = ".",
    #         RE_SHINY_PORT = "9876",
    #         R_LIB_PATHS = .libPaths()[[1]])
  )
  deployProc$wait()
  if (identical(manipulate, "noAssembly") || identical(manipulate, "emptyAssembly") ||
    identical(manipulate, "wrongAssembly")) {
    expect_true(!identical(deployProc$get_exit_status(), 0L))
    unlink(testModelPath, recursive = TRUE, force = TRUE)
    return()
  } else {
    expect_identical(deployProc$get_exit_status(), 0L)
    if (!identical(deployProc$get_exit_status(), 0L)) {
      print(deployProc$read_all_output())
      print(deployProc$read_all_error())
    }
  }

  # unzip .miroapp file and check contents
  testModelDir <- dirname(Sys.getenv("MIRO_MODEL_PATH"))
  unzipDir <- file.path(testModelDir, "miroappContents")
  modelFiles <- readLines(file.path(testModelDir, paste0(modelToTest, "_files.txt")), warn = FALSE, encoding = "UTF-8")
  modelFilesPaths <- c()
  for (modelFile in modelFiles) {
    modelFilesPaths <- c(modelFilesPaths, file.path(unzipDir, modelFile))
  }
  miroappPath <- file.path(testModelDir, paste0(modelToTest, ".miroapp"))
  expect_true(file.exists(miroappPath))
  unzip(miroappPath, exdir = unzipDir)

  miroAppFiles <- c(
    "scripts_pickstock/hcube_analysis.ipynb", "static_pickstock/example.png",
    "static_pickstock/model.png"
  )
  expect_true(all(miroAppFiles %in% zip_list(miroappPath)[[1L]]))
  if (buildArchive) {
    expect_true(file.exists(file.path(unzipDir, paste0(modelToTest, ".zip"))))
    unzip(file.path(unzipDir, paste0(modelToTest, ".zip")), exdir = unzipDir)
  }
  expect_true(all(file.exists(modelFilesPaths)))

  MIROVersion <- NULL
  APIVersion <- NULL
  local({
    eval(parse(text = readLines(file.path(testDir, "..", "app.R"),
      n = 5L, warn = FALSE
    )))
    MIROVersion <<- MIROVersion
    APIVersion <<- APIVersion
  })
  expect_true(all(file.exists(file.path(unzipDir, c(".miroconf", "miroapp.json")))))
  appConf <- read_json(file.path(unzipDir, "miroapp.json"))
  expect_identical(appConf$miro_version, MIROVersion)
  expect_identical(appConf$api_version, APIVersion)
  expect_identical(appConf$main_gms_name, paste0(modelToTest, ".gms"))
  expect_identical(appConf$modes_included, "base")
  expect_identical(appConf$use_temp_dir, if (useTemp) TRUE else FALSE)

  unlink(testModelPath, recursive = TRUE, force = TRUE)
}

test_that(sprintf("Example app: '%s' can be deployed: ", modelToTest), {
  # multi-user
  expect_deploy_works(useTemp = TRUE, buildArchive = TRUE)
  # single-user
  expect_deploy_works(useTemp = FALSE, buildArchive = FALSE)
  # local multi-user
  expect_deploy_works(useTemp = TRUE, buildArchive = FALSE)
})

test_that(sprintf("Example app: '%s' can not be deployed with faulty model assembly file : ", modelToTest), {
  expect_deploy_works(useTemp = TRUE, buildArchive = TRUE, miroMode = "base", manipulate = "noAssembly")
  expect_deploy_works(useTemp = TRUE, buildArchive = TRUE, miroMode = "base", manipulate = "emptyAssembly")
  expect_deploy_works(useTemp = TRUE, buildArchive = TRUE, miroMode = "base", manipulate = "wrongAssembly")
})

Sys.unsetenv(c(
  "MIRO_MODEL_PATH", "MIRO_USE_TMP", "MIRO_BUILD_ARCHIVE",
  "MIRO_BUILD", "MIRO_MODE", "RE_SHINY_PATH", "RE_SHINY_PORT", "R_LIB_PATHS"
))

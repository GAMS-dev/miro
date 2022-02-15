context("Integration test - App deployment with signature")
library(processx)
library(zip)

source("../../components/sign_app.R")

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

expect_deploy_works <- function(useTemp = TRUE, buildArchive = TRUE, manipulate = NULL,
                                pubkeys = NULL) {
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

  Sys.setenv(MIRO_USE_TMP = if (useTemp) "true" else "false")
  Sys.setenv(MIRO_BUILD_ARCHIVE = if (buildArchive) "true" else "false")

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
  expect_identical(deployProc$get_exit_status(), 0L)
  if (!identical(deployProc$get_exit_status(), 0L)) {
    print(deployProc$read_all_output())
    print(deployProc$read_all_error())
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
  expect_true(appIsSigned(miroappPath))

  if (length(manipulate)) {
    unzipDirTmp <- file.path(testModelDir, "miroappContentsTmp")
    unzip(miroappPath, exdir = unzipDirTmp)
    fileNamesTmp <- zip::zip_list(miroappPath)$filename
    if (identical(manipulate, "hashes")) {
      currentHashes <- readLines(file.path(unzipDirTmp, ".miro_hashes"), encoding = "UTF-8")
      writeLines(c(currentHashes, "newfile:::asd"), file.path(unzipDirTmp, ".miro_hashes"))
    } else if (identical(manipulate, "file")) {
      writeLines("asd", file.path(unzipDirTmp, "scripts_pickstock", "hcube_analysis.ipynb"))
    } else if (identical(manipulate, "newfile")) {
      writeLines("asd", file.path(unzipDirTmp, ".bla"))
      fileNamesTmp <- c(fileNamesTmp, ".bla")
    } else if (identical(manipulate, "removefile")) {
      if (!identical(unlink(file.path(unzipDirTmp, "static_pickstock"), recursive = TRUE), 0L)) {
        stop("Problem removing dir", call. = FALSE)
      }
      fileNamesTmp <- fileNamesTmp[!startsWith(fileNamesTmp, "static_pickstock")]
    } else if (identical(manipulate, "removesig")) {
      if (!identical(unlink(file.path(unzipDirTmp, ".miro_sig"), recursive = TRUE), 0L)) {
        stop("Problem removing file", call. = FALSE)
      }
      fileNamesTmp <- fileNamesTmp[fileNamesTmp != ".miro_sig"]
    }
    zipMiro(miroappPath, fileNamesTmp, unzipDirTmp)
    if (identical(manipulate, "removesig")) {
      expect_false(appIsSigned(miroappPath))
    }
  }
  unzip(miroappPath, exdir = unzipDir)

  if (length(pubkeys)) {
    if (length(manipulate)) {
      expect_false(verifyAppSignature(unzipDir, pubKeyPaths = pubkeys, printFingerprint = FALSE))
    } else {
      expect_true(verifyAppSignature(unzipDir, pubKeyPaths = pubkeys, printFingerprint = FALSE))
    }
  }

  miroAppFiles <- c(
    "scripts_pickstock/hcube_analysis.ipynb", "static_pickstock/example.png",
    "static_pickstock/model.png"
  )
  if (!identical(manipulate, "removefile")) {
    expect_true(all(miroAppFiles %in% zip_list(miroappPath)[[1L]]))
  }
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

key <- openssl::rsa_keygen()
pubkey <- as.list(key)$pubkey

privKeyFile <- tempfile()
pubKeyFile <- tempfile()
openssl::write_pem(key, privKeyFile)
openssl::write_pem(pubkey, pubKeyFile)

Sys.setenv("MIRO_PRIVATE_KEY_PATH" = privKeyFile)

test_that(sprintf("Example app: '%s' can be deployed with signature: ", modelToTest), {
  # multi-user
  expect_deploy_works(useTemp = TRUE, buildArchive = TRUE, pubkeys = pubKeyFile)
  # single-user
  expect_deploy_works(useTemp = FALSE, buildArchive = FALSE, pubkeys = pubKeyFile)
})

privKeyPassFile <- tempfile()
writeLines(c("asd", "def", "supersecret", "bla"), privKeyPassFile)
Sys.setenv("MIRO_PRIVATE_KEY_PASSWORD_PATH" = privKeyPassFile)

test_that(sprintf("Example app: '%s' can be deployed with signature: ", modelToTest), {
  # local multi-user
  expect_deploy_works(useTemp = TRUE, buildArchive = FALSE, pubkeys = pubKeyFile)
})

key <- openssl::rsa_keygen()
pubkey <- as.list(key)$pubkey

privKeyFile <- tempfile()
privKeyPassFile <- tempfile()
pubKeyFile2 <- tempfile()
openssl::write_pem(key, privKeyFile, password = "supersecret")
openssl::write_pem(pubkey, pubKeyFile2)

writeLines(c("asd", "def", "supersecret", "bla"), privKeyPassFile)

Sys.setenv("MIRO_PRIVATE_KEY_PATH" = privKeyFile)
Sys.setenv("MIRO_PRIVATE_KEY_PASSWORD_PATH" = privKeyPassFile)

test_that(sprintf("Example app: '%s' can be deployed with signature (password protected key and multiple pubkeys): ", modelToTest), {
  # multi-user
  expect_deploy_works(useTemp = TRUE, buildArchive = TRUE, pubkeys = c(pubKeyFile, pubKeyFile2))
  # single-user
  expect_deploy_works(useTemp = FALSE, buildArchive = FALSE, pubkeys = c(pubKeyFile, pubKeyFile2))
  # local multi-user
  expect_deploy_works(useTemp = TRUE, buildArchive = FALSE, pubkeys = c(pubKeyFile, pubKeyFile2))
})


test_that("Manipulating signed app breaks signature", {
  expect_deploy_works(
    useTemp = TRUE, buildArchive = TRUE, pubkeys = c(pubKeyFile, pubKeyFile2),
    manipulate = "hashes"
  )
  expect_deploy_works(
    useTemp = TRUE, buildArchive = TRUE, pubkeys = c(pubKeyFile, pubKeyFile2),
    manipulate = "file"
  )
  expect_deploy_works(
    useTemp = TRUE, buildArchive = TRUE, pubkeys = c(pubKeyFile, pubKeyFile2),
    manipulate = "newfile"
  )
  expect_deploy_works(
    useTemp = TRUE, buildArchive = TRUE, pubkeys = c(pubKeyFile, pubKeyFile2),
    manipulate = "removefile"
  )
  expect_deploy_works(
    useTemp = TRUE, buildArchive = TRUE, pubkeys = c(pubKeyFile, pubKeyFile2),
    manipulate = "removesig"
  )
})

Sys.unsetenv(c(
  "MIRO_MODEL_PATH", "MIRO_USE_TMP", "MIRO_BUILD_ARCHIVE",
  "MIRO_BUILD", "MIRO_MODE", "RE_SHINY_PATH", "RE_SHINY_PORT", "R_LIB_PATHS",
  "MIRO_PRIVATE_KEY_PATH", "MIRO_PRIVATE_KEY_PASSWORD_PATH"
))

test_that(
  "Scenario permissions work",
  try_again(as.integer(Sys.getenv("MIRO_MAX_RETRIES", "3")), {
    skip_if(
      identical(Sys.getenv("ENGINE_URL"), ""),
      "Skipping permissions tests as ENGINE_URL was not set."
    )
    skip_if(
      identical(Sys.getenv("ENGINE_USER"), ""),
      "Skipping permissions tests as ENGINE_USER was not set."
    )
    skip_if(
      identical(Sys.getenv("ENGINE_PASSWORD"), ""),
      "Skipping permissions tests as ENGINE_PASSWORD was not set."
    )
    skip_if(
      identical(Sys.getenv("ENGINE_NS"), ""),
      "Skippingpermissions tests as ENGINE_NS was not set."
    )
    skip_if(
      !identical(Sys.getenv("MIRO_DB_TYPE"), "postgres"),
      "Skipping permissions tests as MIRO_DB_TYPE was not set to 'postgres'."
    )
    apiURL <- Sys.getenv("ENGINE_URL")
    inviterUser <- Sys.getenv("ENGINE_USER")
    inviterPass <- Sys.getenv("ENGINE_PASSWORD")
    namespace <- Sys.getenv("ENGINE_NS")

    inviteeName <- paste0(Sys.getenv("ENGINE_USER"), "_", round(runif(1, 1, 100000)))

    createUser(
      apiURL, inviterUser, inviterPass, namespace,
      inviteeName, inviterPass
    )

    createTestDb()

    modelToTest <- "pickstock_with_data"
    miroModelDir <- file.path(testDir, "model", modelToTest)
    Sys.setenv(MIRO_MODEL_PATH = file.path(miroModelDir, "pickstock_with_data.gms"))
    Sys.setenv(MIRO_MODE = "base")
    Sys.setenv("MIRO_ENGINE_HOST" = apiURL)
    Sys.setenv("SHINYPROXY_WEBSERVICE_ACCESS_TOKEN" = getEngineToken(apiURL, inviterUser, inviterPass))
    Sys.setenv("SHINYPROXY_WEBSERVICE_ACCESS_TOKEN2" = getEngineToken(apiURL, inviterUser, inviterPass))
    Sys.setenv("MIRO_ENGINE_NAMESPACE" = namespace)
    Sys.setenv("SHINYPROXY_USERNAME" = inviterUser)
    Sys.setenv("SHINYPROXY_USERNAME2" = inviteeName)
    Sys.setenv("SHINYPROXY_USERGROUPS" = "USERS")
    Sys.setenv("MIRO_VERSION_STRING" = "2.11.9999")
    dataDirUser1 <- file.path(miroModelDir, paste0("data_", modelToTest, "1"))
    dataDirUser2 <- file.path(miroModelDir, paste0("data_", modelToTest, "2"))
    dataDirUser3 <- file.path(miroModelDir, paste0("data_", modelToTest, "3"))
    miroCacheDir <- file.path(getwd(), "..", "testcache")
    Sys.setenv("MIRO_DATA_DIR" = dataDirUser1)
    Sys.setenv("MIRO_DATA_DIR2" = dataDirUser2)
    Sys.setenv("MIRO_DATA_DIR3" = dataDirUser3)
    Sys.setenv("MIRO_CACHE_DIR" = miroCacheDir)
    Sys.setenv(MIRO_FORCE_SCEN_IMPORT = "true")

    copyDirRecursive(
      file.path(miroModelDir, paste0("data_", modelToTest)),
      dataDirUser1
    )
    copyDirRecursive(
      file.path(miroModelDir, paste0("data_", modelToTest)),
      dataDirUser2
    )
    copyDirRecursive(
      file.path(miroModelDir, paste0("data_", modelToTest)),
      dataDirUser3
    )
    registerModel(
      apiURL, namespace, inviterUser, inviterPass, modelToTest,
      file.path(miroModelDir, paste0(modelToTest, ".zip"))
    )

    source(file.path(testDir, "shinytest", "permissions_test.R"), local = TRUE)
    source(file.path(testDir, "shinytest", "permissions_save_as_test.R"), local = TRUE)

    removeUser(apiURL, inviterUser, inviterPass, inviteeName)
    removeModel(apiURL, namespace, inviterUser, inviterPass, modelToTest)
    unlink(dataDirUser1, recursive = TRUE)
    unlink(dataDirUser2, recursive = TRUE)
    unlink(dataDirUser3, recursive = TRUE)
    unlink(miroCacheDir, recursive = TRUE)

    Sys.unsetenv(c(
      "MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE", "MIRO_ENGINE_HOST",
      "SHINYPROXY_USERNAME",
      "SHINYPROXY_USERNAME2",
      "MIRO_FORCE_SCEN_IMPORT",
      "SHINYPROXY_USERGROUPS",
      "SHINYPROXY_WEBSERVICE_ACCESS_TOKEN",
      "SHINYPROXY_WEBSERVICE_ACCESS_TOKEN2",
      "MIRO_ENGINE_NAMESPACE",
      "MIRO_VERSION_STRING",
      "MIRO_DATA_DIR",
      "MIRO_DATA_DIR2",
      "MIRO_DATA_DIR3"
    ))
  })
)

test_that(
  "Layout settings work",
  try_again(as.integer(Sys.getenv("MIRO_MAX_RETRIES", "3")), {
    createTestDb()

    Sys.setenv(MIRO_MODEL_PATH = file.path(
      testDir, "model", "pickstock_configuration",
      "pickstock_configuration.gms"
    ))
    configJSONPath <- file.path(testDir, "model", "pickstock_configuration", "conf_pickstock_configuration")
    file.copy(file.path(configJSONPath, "pickstock_configuration.json"),
      file.path(configJSONPath, "pickstock_configuration_tmp.json"),
      overwrite = TRUE
    )
    configJSON <- suppressWarnings(jsonlite::fromJSON(file.path(configJSONPath, "pickstock_configuration.json"),
      simplifyDataFrame = FALSE,
      simplifyMatrix = FALSE
    ))
    configJSON$layoutSettings <- list(
      maxTabsExpandedInput = 2L,
      maxTabsExpandedOutput = 3L,
      maxTabsExpandedPivotComp = 4L,
      maxTabsExpandedSplitComp = 1L,
      maxTabsExpandedTabComp = 6L
    )
    configJSON$defCompMode <- "tab"
    jsonlite::write_json(configJSON, file.path(configJSONPath, "pickstock_configuration.json"),
      pretty = TRUE, auto_unbox = TRUE, null = "null"
    )

    source(file.path(testDir, "shinytest", "layout_expanded_tabs_test.R"), local = TRUE)

    file.rename(
      file.path(configJSONPath, "pickstock_configuration_tmp.json"),
      file.path(configJSONPath, "pickstock_configuration.json")
    )

    Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH"))
  })
)

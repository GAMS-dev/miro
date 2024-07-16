source("../../components/json_sorter.R")
library(V8)

test_that(
  "Config mode works",
  try_again(as.integer(Sys.getenv("MIRO_MAX_RETRIES", "3")), {
    createTestDb()

    sorter <- JSONSorter$new("../../JS")
    expect_identical(
      sorter$sort('{"c":1,"a":1.23,"b": ["b", "a", "c"]}'),
      "{\n    \"a\": 1.23,\n    \"b\": [\n        \"b\",\n        \"a\",\n        \"c\"\n    ],\n    \"c\": 1\n}"
    )


    Sys.setenv(MIRO_MODEL_PATH = file.path(
      testDir, "model", "pickstock_config_mode",
      "pickstock_configuration.gms"
    ))
    Sys.setenv(MIRO_MODE = "config")
    configJSONPath <- file.path(testDir, "model", "pickstock_config_mode", "conf_pickstock_configuration")
    file.copy(file.path(configJSONPath, "pickstock_configuration_expected.json"),
      file.path(configJSONPath, "pickstock_configuration.json"),
      overwrite = TRUE
    )

    source(file.path(testDir, "shinytest", "config_mode_general_test.R"), local = TRUE)
    source(file.path(testDir, "shinytest", "config_mode_table_test.R"), local = TRUE)
    source(file.path(testDir, "shinytest", "config_mode_widget_test.R"), local = TRUE)
    source(file.path(testDir, "shinytest", "config_mode_graph_test.R"), local = TRUE)

    Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE"))
  })
)

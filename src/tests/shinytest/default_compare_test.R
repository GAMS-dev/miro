jsonPath <- file.path("..", "model", "transport", "conf_transport", "transport.json")
configJSON <- suppressWarnings(jsonlite::fromJSON(jsonPath,
  simplifyDataFrame = FALSE,
  simplifyMatrix = FALSE
))

writeConfig <- function() {
  jsonlite::write_json(configJSON, jsonPath,
    pretty = TRUE,
    auto_unbox = TRUE, null = "null"
  )
}

# default = split view
app <- AppDriver$new("../../", name = "default_compare_test", variant = NULL, load_timeout = 20000)

app$click(selector = 'a[data-value="scenarios"]')
expect_error(app$click(selector = "#scenSplit1_open > div:nth-child(2) > button"), NA)
Sys.sleep(2L)
expect_identical(app$get_text("#cmpScenTitle_2"), "New Scenario (Sandbox)")
Sys.sleep(2L)
app$stop()

# default = tab view
configJSON[["defCompMode"]] <- "tab"
writeConfig()
app <- AppDriver$new("../../", name = "default_compare_test", variant = NULL, load_timeout = 20000)

app$click(selector = 'a[data-value="scenarios"]')
expect_error(app$click(selector = "#scenTabset #btLoadScen"), NA)
Sys.sleep(2L)
app$stop()

# default = pivot view
configJSON[["defCompMode"]] <- "pivot"
writeConfig()
app <- AppDriver$new("../../", name = "default_compare_test", variant = NULL, load_timeout = 20000)

app$click(selector = 'a[data-value="scenarios"]')
expect_error(app$click(selector = "#pivotCompBtWrapper button"), NA)
Sys.sleep(2L)
app$stop()

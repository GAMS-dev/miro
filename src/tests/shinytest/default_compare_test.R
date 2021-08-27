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
app <- ShinyDriver$new("../../", loadTimeout = 20000)
app$snapshotInit("default_compare_test")

app$findElement('a[data-value="scenarios"]')$click()
expect_error(app$findElements(".scenSplit-button-load")[[2]]$click(), NA)
Sys.sleep(2L)
expect_true(app$waitFor("$('#cmpScenTitle_2').text()==='New Scenario (Sandbox)';", timeout = 50))
app$snapshot(items = list(output = c("inputDataTitle")), screenshot = TRUE)
Sys.sleep(2L)
app$stop()

# default = tab view
configJSON[["defCompMode"]] <- "tab"
writeConfig()
app <- ShinyDriver$new("../../", loadTimeout = 20000)

app$findElement('a[data-value="scenarios"]')$click()
expect_error(app$findElement("#scenTabset #btLoadScen")$click(), NA)
Sys.sleep(2L)
app$stop()

# default = pivot view
configJSON[["defCompMode"]] <- "pivot"
writeConfig()
app <- ShinyDriver$new("../../", loadTimeout = 20000)

app$findElement('a[data-value="scenarios"]')$click()
expect_error(app$findElement("#pivotCompBtWrapper button")$click(), NA)
Sys.sleep(2L)
app$stop()

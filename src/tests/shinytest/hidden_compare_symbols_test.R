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

# "schedule" (alias: "shipment quantities") is hidden in scenario comparison
# mode only; "_scalars_out" stays visible everywhere, and is
# used to confirm comparison mode still renders normally otherwise.
configJSON[["hiddenCompareSymbols"]] <- "schedule"
configJSON[["defCompMode"]] <- "split"
writeConfig()

app <- AppDriver$new("../../",
  name = "hidden_compare_symbols_test", variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)
Sys.sleep(2L)

# the hidden symbol still shows up as a tab on the regular Output screen
app$click(selector = 'a[data-value="outputData"]')
Sys.sleep(1L)
expect_true(app$get_js("$('#outputTabset a:contains(\"shipment quantities\")').length > 0", timeout = 2000L))
expect_true(app$get_js("$('#outputTabset a:contains(\"Output Scalars\")').length > 0", timeout = 2000L))

# but not in split scenario comparison mode (box 1 = sandbox scenario)
app$click(selector = 'a[data-value="scenarios"]')
expect_error(app$click(selector = "#scenSplit1_open > div:nth-child(2) > button"), NA)
Sys.sleep(2L)
expect_identical(app$get_text("#cmpScenTitle_2"), "New Scenario (Sandbox)")
expect_equal(app$get_js("$('#contentScen_2 a:contains(\"Shipment quantities\")').length"), 0L)
expect_true(app$get_js("$('#contentScen_2 a:contains(\"Output Scalars\")').length > 0", timeout = 2000L))

app$stop()

# an entire output/input group's tab should disappear from comparison mode
# too, if every symbol it contains is hidden there. "Location information"
# (members: ilocdata, jlocdata) already exists as an input group in this
# model's config, so we only need to add the output group ourselves.
configJSON[["outputGroups"]] <- list(list(
  name = "Both Outputs",
  members = list("schedule", "_scalars_out")
))
configJSON[["hiddenCompareSymbols"]] <- list("schedule", "_scalars_out", "ilocdata", "jlocdata", "d")
writeConfig()

app <- AppDriver$new("../../",
  name = "hidden_compare_symbols_test", variant = NULL,
  load_timeout = as.integer(Sys.getenv("MIRO_TEST_LOAD_TIMEOUT", "20000")),
  timeout = as.integer(Sys.getenv("MIRO_TEST_TIMEOUT", "4000"))
)
Sys.sleep(2L)

# both groups still show up as a single tab outside of comparison mode
app$click(selector = 'a[data-value="outputData"]')
Sys.sleep(1L)
expect_true(app$get_js("$('#outputTabset_1 a:contains(\"shipment\")').length > 0"), label = "shipment tab exists in output section")
expect_true(app$get_js("$('#outputTabset_1 a:contains(\"Output Scalars\")').length > 0"), label = "_scalars_out tab exists in output section")
app$click(selector = 'a[data-value="inputData"]')
Sys.sleep(1L)
expect_true(app$get_js("$('#inputTabset a:contains(\"location information\")').length > 0"), label = "Input group tab exists")

# but both whole group tabs are gone from split scenario comparison mode,
# since every symbol they contain is hidden there
app$click(selector = 'a[data-value="scenarios"]')
expect_error(app$click(selector = "#scenSplit1_open > div:nth-child(2) > button"), NA)
Sys.sleep(2L)
expect_identical(app$get_text("#cmpScenTitle_2"), "New Scenario (Sandbox)")
expect_equal(app$get_js("$('#contentScen_2 a:contains(\"Both Outputs\")').length"), 0L, label = "Output group tab hidden in split compare")
expect_equal(app$get_js("$('#contentScen_2 a:contains(\"location information\")').length"), 0L, label = "Input group tab hidden in split compare")
expect_equal(app$get_js("$('#contentScen_2 a:contains(\"distance\")').length"), 0L, label = "distance tab hidden in split compare")

app$stop()

# clean up: restore defaults so later tests in this run see the default config
# configJSON[["hiddenCompareSymbols"]] <- NULL
# configJSON[["outputGroups"]] <- NULL
# writeConfig()

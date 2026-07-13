library(R6)
library(DBI)
library(RSQLite)
library(jsonlite)

source("../../components/ai_adapter.R")

# Mock DbSchema class to simulate the schema behavior
MockDbSchema <- R6Class("MockDbSchema",
  public = list(
    getDbTableName = function(sym) {
      if (sym == "_scenMeta") {
        return("scen_meta")
      }
      return(paste0("data_", sym))
    },
    getDbTableNames = function() {
      c("demand", "capacity")
    },
    getDbSchema = function(sym) {
      if (sym == "_scenMeta") {
        return(list(colNames = c(sid = "_sid", sname = "_sname", stag = "_stag")))
      }
      if (sym == "demand") {
        return(list(colNames = c("_sid", "region", "year", "value")))
      }
      return(list(colNames = c("_sid", "dummy")))
    }
  )
)

# Mock IO Config
mockIoConfig <- list(
  modelIn = list(
    demand = list(
      alias = "Customer Demand",
      type = "parameter",
      headers = list(
        region = list(alias = "Region", type = "set"),
        year = list(alias = "Year", type = "set"),
        value = list(alias = "Value", type = "numeric")
      )
    )
  ),
  modelOut = list()
)

# --- Test Environment Setup ---

setup_test_env <- function() {
  conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  # Populate mock scenario metadata
  DBI::dbWriteTable(conn, "scen_meta", tibble::tibble(
    `_sid` = c(1, 2, 3),
    `_sname` = c("Base", "High Demand", "Low Demand"),
    `_stag` = c("production", "test", "test")
  ))

  # Populate mock data table
  DBI::dbWriteTable(conn, "data_demand", tibble::tibble(
    `_sid` = c(1, 1, 2, 3),
    region = c("NA", "EU", "NA", "EU"),
    year = c("2024", "2024", "2024", "2024"),
    value = c(100, 150, 200, 50)
  ))

  dbSchema <- MockDbSchema$new()
  adapter <- AIToolAdapter$new(dbSchema = dbSchema, conn = conn, ioConfig = mockIoConfig)

  list(conn = conn, adapter = adapter)
}

# --- Unit Tests ---

test_that("AIToolAdapter initializes and getSystemMetadata builds scenario maps correctly", {
  env <- setup_test_env()
  adapter <- env$adapter

  adapter$setAllowedSids(c(1, 2, 3))

  # getSystemMetadata is required to build private$scenarioMap
  meta_json <- adapter$getSystemMetadata()
  meta <- jsonlite::fromJSON(meta_json)

  expect_true(inherits(adapter, "AIToolAdapter"))
  expect_equal(nrow(meta$activeScenarios), 3)
  expect_true("Base" %in% meta$activeScenarios$name)

  DBI::dbDisconnect(env$conn)
})

test_that("getToolSchema returns correct JSON schema structure with analysis modes", {
  env <- setup_test_env()
  adapter <- env$adapter
  adapter$setAllowedSids(c(1, 2))

  schema <- adapter$getToolSchema()

  expect_type(schema, "list")
  expect_equal(schema[[1]]$name, "queryScenarioDatabase")
  expect_equal(schema[[1]]$type, "function")
  expect_true("symbolName" %in% schema[[1]]$parameters$required)
  expect_true("scenarioNames" %in% schema[[1]]$parameters$required)

  # Check conditional addition of analysis properties
  expect_true("analysisMode" %in% names(schema[[1]]$parameters$properties))
  expect_true("baselineScenarioName" %in% names(schema[[1]]$parameters$properties))

  DBI::dbDisconnect(env$conn)
})

test_that("queryScenarioDatabase blocks unauthorized scenarios (Security Guardrail)", {
  env <- setup_test_env()
  adapter <- env$adapter
  adapter$setAllowedSids(c(1, 2)) # Only allow Base and High Demand
  adapter$getSystemMetadata()

  query_args <- list(
    symbolName = "demand",
    scenarioNames = c("Low Demand") # Attempting to access unauthorized scenario
  )

  res <- adapter$queryScenarioDatabase(query_args)
  res_list <- jsonlite::fromJSON(res)

  expect_true(!is.null(res_list$error))
  expect_match(res_list$error, "Access Denied")

  DBI::dbDisconnect(env$conn)
})

test_that("queryScenarioDatabase executes basic SELECT correctly and maps names", {
  env <- setup_test_env()
  adapter <- env$adapter
  adapter$setAllowedSids(c(1, 2, 3))
  adapter$getSystemMetadata()

  query_args <- list(
    symbolName = "demand",
    scenarioNames = c("Base")
  )

  res <- adapter$queryScenarioDatabase(query_args)
  res_df <- jsonlite::fromJSON(res)
  print(res_df)

  # Check basic filtering
  expect_equal(nrow(res_df), 2)
  expect_equal(res_df$region, c("NA", "EU"))

  # Check scenario name mapping was applied instead of raw _sid
  expect_true("_scenario_name" %in% names(res_df))
  expect_true(all(res_df$`_scenario_name` == "Base"))

  DBI::dbDisconnect(env$conn)
})

test_that("queryScenarioDatabase handles aggregations and grouping", {
  env <- setup_test_env()
  adapter <- env$adapter
  adapter$setAllowedSids(c(1, 2, 3))
  adapter$getSystemMetadata()

  query_args <- list(
    symbolName = "demand",
    scenarioNames = c("Base", "High Demand"),
    selectColumn = "value",
    aggregation = "SUM",
    groupByColumn = "region"
  )

  res <- adapter$queryScenarioDatabase(query_args)
  res_df <- jsonlite::fromJSON(res)

  # The adapter groups by _sid AND region in STANDARD mode.
  # Base NA: 100, Base EU: 150, High Demand NA: 200
  expect_equal(nrow(res_df), 3)

  # Assert the new _sum column alias is applied
  expect_true("_sum" %in% names(res_df))

  base_na_sum <- res_df$`_sum`[res_df$`_scenario_name` == "Base" & res_df$region == "NA"]
  high_demand_na_sum <- res_df$`_sum`[res_df$`_scenario_name` == "High Demand" & res_df$region == "NA"]

  expect_equal(base_na_sum, 100)
  expect_equal(high_demand_na_sum, 200)

  DBI::dbDisconnect(env$conn)
})

test_that("queryScenarioDatabase calculates DIFFERENCE correctly", {
  env <- setup_test_env()
  adapter <- env$adapter
  adapter$setAllowedSids(c(1, 2, 3))
  adapter$getSystemMetadata()

  query_args <- list(
    symbolName = "demand",
    scenarioNames = c("Base", "High Demand"),
    selectColumn = "value",
    aggregation = "SUM",
    groupByColumn = "region",
    analysisMode = "DIFFERENCE",
    baselineScenarioName = "Base"
  )

  res <- adapter$queryScenarioDatabase(query_args)
  res_df <- jsonlite::fromJSON(res)

  # Should output `_delta` column
  expect_true("_delta" %in% names(res_df))
  expect_true("_target_scenario" %in% names(res_df))
  expect_true("_baseline_scenario" %in% names(res_df))

  # NA region: Target(High Demand=200) - Base(Base=100) = 100
  # EU region: Target(High Demand=0) - Base(Base=150) = -150
  delta_na <- res_df$`_delta`[res_df$region == "NA"]
  delta_eu <- res_df$`_delta`[res_df$region == "EU"]

  expect_equal(delta_na, 100)
  expect_equal(delta_eu, -150)

  DBI::dbDisconnect(env$conn)
})

test_that("queryScenarioDatabase calculates PERCENT_CHANGE correctly", {
  env <- setup_test_env()
  adapter <- env$adapter
  adapter$setAllowedSids(c(1, 2, 3))
  adapter$getSystemMetadata()

  query_args <- list(
    symbolName = "demand",
    scenarioNames = c("Base", "Low Demand"),
    selectColumn = "value",
    aggregation = "SUM",
    groupByColumn = "region",
    analysisMode = "PERCENT_CHANGE",
    baselineScenarioName = "Base"
  )

  res <- adapter$queryScenarioDatabase(query_args)
  res_df <- jsonlite::fromJSON(res)

  # Should output `_pct_change` column
  expect_true("_pct_change" %in% names(res_df))

  # EU region: Target(Low Demand=50) - Base(Base=150) / 150 * 100 = -66.666...
  pct_eu <- res_df$`_pct_change`[res_df$region == "EU"]

  expect_equal(round(pct_eu, 2), -66.67)

  DBI::dbDisconnect(env$conn)
})

test_that("queryScenarioDatabase sanitizes and applies IN filters", {
  env <- setup_test_env()
  adapter <- env$adapter
  adapter$setAllowedSids(c(1, 2, 3))
  adapter$getSystemMetadata()

  query_args <- list(
    symbolName = "demand",
    scenarioNames = c("Base", "High Demand", "Low Demand"),
    filters = data.frame(
      column = "region",
      operator = "IN",
      value = "NA, EU",
      stringsAsFactors = FALSE
    )
  )

  res <- adapter$queryScenarioDatabase(query_args)
  res_df <- jsonlite::fromJSON(res)

  expect_equal(nrow(res_df), 4)

  DBI::dbDisconnect(env$conn)
})

test_that("queryScenarioDatabase prevents SQL injection via invalid column names", {
  env <- setup_test_env()
  adapter <- env$adapter
  adapter$setAllowedSids(c(1))
  adapter$getSystemMetadata()

  query_args <- list(
    symbolName = "demand",
    scenarioNames = c("Base"),
    selectColumn = "value; DROP TABLE data_demand;--"
  )

  res <- adapter$queryScenarioDatabase(query_args)
  res_list <- jsonlite::fromJSON(res)

  expect_true(!is.null(res_list$error))
  expect_match(res_list$error, "Invalid column")

  DBI::dbDisconnect(env$conn)
})

test_that("queryScenarioDatabase handles pagination/truncation warnings", {
  env <- setup_test_env()
  adapter <- env$adapter
  adapter$setAllowedSids(c(1, 2, 3))
  adapter$getSystemMetadata()

  query_args <- list(
    symbolName = "demand",
    scenarioNames = c("Base", "High Demand", "Low Demand"),
    limit = 2
  )

  res <- adapter$queryScenarioDatabase(query_args)
  res_list <- jsonlite::fromJSON(res)

  expect_true(!is.null(res_list$`_warning`))
  expect_match(res_list$`_warning`, "truncated to 2 rows")
  expect_equal(nrow(res_list$data), 2)

  DBI::dbDisconnect(env$conn)
})

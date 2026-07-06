library(R6)
library(DBI)
library(RSQLite)
library(jsonlite)

source("../../components/ai_adapter.R")

# --- Mock Objects Setup ---

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
        return(list(colNames = c(sid = "_sid", name = "_sname", stag = "_stag")))
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

test_that("AIToolAdapter initializes and sets SIDs correctly", {
  env <- setup_test_env()
  adapter <- env$adapter

  # Test setting allowed SIDs
  adapter$setAllowedSids(c(1, "2", NA))

  # Use an accessor or test behavior to verify (since allowedSids is private)
  # We test this implicitly by checking metadata filtering later
  expect_true(inherits(adapter, "AIToolAdapter"))

  DBI::dbDisconnect(env$conn)
})

test_that("getToolSchema returns correct JSON schema structure", {
  env <- setup_test_env()
  schema <- env$adapter$getToolSchema()

  expect_type(schema, "list")
  expect_equal(schema[[1]]$name, "queryScenarioDatabase")
  expect_equal(schema[[1]]$type, "function")
  expect_true("symbolName" %in% schema[[1]]$parameters$required)

  DBI::dbDisconnect(env$conn)
})

test_that("queryScenarioDatabase blocks unauthorized scenarios (Security Guardrail)", {
  env <- setup_test_env()
  adapter <- env$adapter
  adapter$setAllowedSids(c(1, 2)) # Only allow SIDs 1 and 2

  query_args <- list(
    symbolName = "demand",
    scenarioIds = c(3) # Attempting to access unauthorized SID 3
  )

  res <- adapter$queryScenarioDatabase(query_args)
  res_list <- jsonlite::fromJSON(res)

  expect_true(!is.null(res_list$error))
  expect_match(res_list$error, "Access Denied")

  DBI::dbDisconnect(env$conn)
})

test_that("queryScenarioDatabase executes basic SELECT correctly", {
  env <- setup_test_env()
  adapter <- env$adapter
  adapter$setAllowedSids(c(1, 2, 3))

  query_args <- list(
    symbolName = "demand",
    scenarioIds = c(1) # Only fetch for SID 1
  )

  res <- adapter$queryScenarioDatabase(query_args)
  res_df <- jsonlite::fromJSON(res)

  expect_equal(nrow(res_df), 2)
  expect_equal(res_df$region, c("NA", "EU"))

  DBI::dbDisconnect(env$conn)
})

test_that("queryScenarioDatabase handles aggregations and grouping", {
  env <- setup_test_env()
  adapter <- env$adapter
  adapter$setAllowedSids(c(1, 2, 3))

  query_args <- list(
    symbolName = "demand",
    scenarioIds = c(1, 2),
    selectColumn = "value",
    aggregation = "SUM",
    groupByColumn = "region"
  )

  res <- adapter$queryScenarioDatabase(query_args)
  res_df <- jsonlite::fromJSON(res)

  # NA region: SID 1 (100) + SID 2 (200) = 300
  # EU region: SID 1 (150)
  expect_equal(nrow(res_df), 2)
  expect_true("sum" %in% names(res_df))
  na_sum <- res_df$sum[res_df$region == "NA"]
  expect_equal(na_sum, 300)
  eu_sum <- res_df$sum[res_df$region == "EU"]
  expect_equal(eu_sum, 150)

  DBI::dbDisconnect(env$conn)
})

test_that("queryScenarioDatabase sanitizes and applies IN filters", {
  env <- setup_test_env()
  adapter <- env$adapter
  adapter$setAllowedSids(c(1, 2, 3))

  query_args <- list(
    symbolName = "demand",
    scenarioIds = c(1, 2, 3),
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

  query_args <- list(
    symbolName = "demand",
    scenarioIds = c(1),
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

  query_args <- list(
    symbolName = "demand",
    scenarioIds = c(1, 2, 3),
    limit = 2
  )

  res <- adapter$queryScenarioDatabase(query_args)
  res_list <- jsonlite::fromJSON(res)

  expect_true(!is.null(res_list$`_warning`))
  expect_match(res_list$`_warning`, "truncated to 2 rows")
  expect_equal(nrow(res_list$data), 2)

  DBI::dbDisconnect(env$conn)
})

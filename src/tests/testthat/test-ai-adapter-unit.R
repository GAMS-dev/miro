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
        return(list(colNames = c(sid = "_sid", uid = "_uid", sname = "_sname", stag = "_stag")))
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
    ),
    capacity = list(
      alias = "Plant Capacity",
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
    `_uid` = "user",
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

test_that("getSystemMetadata disambiguates scenarios sharing a name by owner", {
  env <- setup_test_env()
  DBI::dbExecute(env$conn, "UPDATE scen_meta SET _sname = 'Base', _uid = 'other' WHERE _sid = 2")
  env$adapter$setAllowedSids(c(1, 2, 3))

  meta <- jsonlite::fromJSON(env$adapter$getSystemMetadata())

  expect_setequal(meta$activeScenarios$name, c("Base (user)", "Base (other)", "Low Demand"))

  DBI::dbDisconnect(env$conn)
})

test_that("getSystemMetadata describes each symbol's columns, aliases and types", {
  # the shape of this block is the contract the model is prompted with, so it
  # should only ever change deliberately
  env <- setup_test_env()
  adapter <- env$adapter
  adapter$setAllowedSids(c(1, 2, 3))

  symbols <- jsonlite::fromJSON(adapter$getSystemMetadata(), simplifyVector = FALSE)$databaseSymbols

  expect_equal(names(symbols), c("demand", "capacity"))
  expect_equal(symbols$demand$alias, "Customer Demand")
  expect_equal(symbols$demand$direction, "input")
  expect_false(symbols$demand$isScalar)
  expect_equal(names(symbols$demand$columns), c("region", "year", "value"))
  expect_equal(symbols$demand$columns$region$alias, "Region")
  expect_equal(symbols$demand$columns$value$type, "numeric")
  # a column type is reported as one of the two kinds MIRO stores
  expect_true(all(vapply(symbols$demand$columns, "[[", character(1L), "type") %in%
    c("string", "numeric")))

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

  # Check basic filtering
  expect_equal(nrow(res_df), 2)
  expect_equal(res_df$region, c("NA", "EU"))

  # Check scenario name mapping was applied instead of raw _sid
  expect_true("_scenario_name" %in% names(res_df))
  expect_true(all(res_df$`_scenario_name` == "Base"))

  DBI::dbDisconnect(env$conn)
})

test_that("queryScenarioDatabase renders a NULL cell as an explicit `null`, not a dropped key", {
  # A dropped key is indistinguishable from a value that just wasn't
  # selected -- an LLM reading the raw JSON can (and, in production, did)
  # mistake a neighboring column's value for the missing one. The fix is
  # `na = "null"` on the toJSON calls in queryScenarioDatabase.
  env <- setup_test_env()
  adapter <- env$adapter
  adapter$setAllowedSids(c(1, 2, 3))
  adapter$getSystemMetadata()

  DBI::dbAppendTable(env$conn, "data_demand", tibble::tibble(
    `_sid` = 1, region = "APAC", year = "2024", value = NA_real_
  ))

  res <- adapter$queryScenarioDatabase(list(symbolName = "demand", scenarioNames = c("Base")))

  expect_match(res, '"region":"APAC","year":"2024","value":null', fixed = TRUE)

  res_df <- jsonlite::fromJSON(res)
  expect_true("value" %in% names(res_df))
  expect_true(is.na(res_df$value[res_df$region == "APAC"]))

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

  # High Demand (sid 2) has no EU row by default; add one so the EU delta
  # is a real comparison rather than a NULL-propagated aggregate.
  DBI::dbAppendTable(env$conn, "data_demand", tibble::tibble(
    `_sid` = 2, region = "EU", year = "2024", value = 50
  ))

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
  expect_true(all(res_df$`_target_scenario` == "High Demand"))
  expect_true(all(res_df$`_baseline_scenario` == "Base"))

  # NA region: Target(High Demand=200) - Base(Base=100) = 100
  # EU region: Target(High Demand=50) - Base(Base=150) = -100
  delta_na <- res_df$`_delta`[res_df$region == "NA"]
  delta_eu <- res_df$`_delta`[res_df$region == "EU"]

  expect_equal(delta_na, 100)
  expect_equal(delta_eu, -100)

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

test_that("queryScenarioDatabase calculates PERCENT_CHANGE correctly for COUNT aggregation", {
  env <- setup_test_env()
  adapter <- env$adapter
  adapter$setAllowedSids(c(1, 2, 3))
  adapter$getSystemMetadata()

  # COUNT() always returns a SQL integer, unlike SUM() on this table's floating-point
  # `value` column - this regression-tests that PERCENT_CHANGE doesn't truncate via
  # integer division when both operands are integers (e.g. 11 vs 10 evaluating to 0%
  # instead of 10%). Base (sid 1) ends up with 10 total NA rows, target (sid 2) with 11.
  DBI::dbAppendTable(env$conn, "data_demand", tibble::tibble(
    `_sid` = 1, region = "NA", year = "2024", value = seq_len(9)
  ))
  DBI::dbAppendTable(env$conn, "data_demand", tibble::tibble(
    `_sid` = 2, region = "NA", year = "2024", value = seq_len(10)
  ))

  query_args <- list(
    symbolName = "demand",
    scenarioNames = c("Base", "High Demand"),
    selectColumn = "value",
    aggregation = "COUNT",
    groupByColumn = "region",
    analysisMode = "PERCENT_CHANGE",
    baselineScenarioName = "Base"
  )

  res <- adapter$queryScenarioDatabase(query_args)
  res_df <- jsonlite::fromJSON(res)

  # Base (sid 1) NA count: 1 (setup) + 9 appended = 10
  # Target (sid 2) NA count: 1 (setup) + 10 appended = 11
  # (11 - 10) / 10 * 100 = 10%
  pct_na <- res_df$`_pct_change`[res_df$region == "NA"]

  expect_equal(round(pct_na, 2), 10)

  DBI::dbDisconnect(env$conn)
})

test_that("queryScenarioDatabase calculates DIFFERENCE against multiple target scenarios", {
  env <- setup_test_env()
  adapter <- env$adapter
  adapter$setAllowedSids(c(1, 2, 3))
  adapter$getSystemMetadata()

  query_args <- list(
    symbolName = "demand",
    scenarioNames = c("Base", "High Demand", "Low Demand"),
    selectColumn = "value",
    aggregation = "SUM",
    analysisMode = "DIFFERENCE",
    baselineScenarioName = "Base"
  )

  res <- adapter$queryScenarioDatabase(query_args)
  res_df <- jsonlite::fromJSON(res)

  # One row per non-baseline scenario, no rows silently dropped
  expect_equal(nrow(res_df), 2)
  expect_setequal(res_df$`_target_scenario`, c("High Demand", "Low Demand"))
  expect_true(all(res_df$`_baseline_scenario` == "Base"))

  # Base total = 100 + 150 = 250; High Demand total = 200; Low Demand total = 50
  delta_high <- res_df$`_delta`[res_df$`_target_scenario` == "High Demand"]
  delta_low <- res_df$`_delta`[res_df$`_target_scenario` == "Low Demand"]

  expect_equal(delta_high, -50)
  expect_equal(delta_low, -200)

  DBI::dbDisconnect(env$conn)
})

test_that("queryScenarioDatabase calculates PERCENT_CHANGE against multiple target scenarios", {
  env <- setup_test_env()
  adapter <- env$adapter
  adapter$setAllowedSids(c(1, 2, 3))
  adapter$getSystemMetadata()

  query_args <- list(
    symbolName = "demand",
    scenarioNames = c("Base", "High Demand", "Low Demand"),
    selectColumn = "value",
    aggregation = "SUM",
    analysisMode = "PERCENT_CHANGE",
    baselineScenarioName = "Base"
  )

  res <- adapter$queryScenarioDatabase(query_args)
  res_df <- jsonlite::fromJSON(res)

  expect_equal(nrow(res_df), 2)
  expect_setequal(res_df$`_target_scenario`, c("High Demand", "Low Demand"))

  # Base total = 250; High Demand = 200 -> -20%; Low Demand = 50 -> -80%
  pct_high <- res_df$`_pct_change`[res_df$`_target_scenario` == "High Demand"]
  pct_low <- res_df$`_pct_change`[res_df$`_target_scenario` == "Low Demand"]

  expect_equal(round(pct_high, 2), -20)
  expect_equal(round(pct_low, 2), -80)

  DBI::dbDisconnect(env$conn)
})

test_that("queryScenarioDatabase applies groupByColumn per target scenario for multi-target DIFFERENCE", {
  env <- setup_test_env()
  adapter <- env$adapter
  adapter$setAllowedSids(c(1, 2, 3))
  adapter$getSystemMetadata()

  # Fill in the missing region rows so every scenario has both a NA and EU
  # value, keeping the per-group deltas free of NULL propagation.
  DBI::dbAppendTable(env$conn, "data_demand", tibble::tibble(
    `_sid` = c(2, 3),
    region = c("EU", "NA"),
    year = c("2024", "2024"),
    value = c(30, 40)
  ))

  query_args <- list(
    symbolName = "demand",
    scenarioNames = c("Base", "High Demand", "Low Demand"),
    selectColumn = "value",
    aggregation = "SUM",
    groupByColumn = "region",
    analysisMode = "DIFFERENCE",
    baselineScenarioName = "Base"
  )

  res <- adapter$queryScenarioDatabase(query_args)
  res_df <- jsonlite::fromJSON(res)

  # 2 target scenarios x 2 regions = 4 rows
  expect_equal(nrow(res_df), 4)
  expect_true("region" %in% names(res_df))

  # Base: NA=100, EU=150. High Demand: NA=200, EU=30. Low Demand: NA=40, EU=50.
  delta_high_na <- res_df$`_delta`[res_df$`_target_scenario` == "High Demand" & res_df$region == "NA"]
  delta_high_eu <- res_df$`_delta`[res_df$`_target_scenario` == "High Demand" & res_df$region == "EU"]
  delta_low_na <- res_df$`_delta`[res_df$`_target_scenario` == "Low Demand" & res_df$region == "NA"]
  delta_low_eu <- res_df$`_delta`[res_df$`_target_scenario` == "Low Demand" & res_df$region == "EU"]

  expect_equal(delta_high_na, 100)
  expect_equal(delta_high_eu, -120)
  expect_equal(delta_low_na, -60)
  expect_equal(delta_low_eu, -100)

  DBI::dbDisconnect(env$conn)
})

test_that("queryScenarioDatabase orders results across multiple target scenarios", {
  env <- setup_test_env()
  adapter <- env$adapter
  adapter$setAllowedSids(c(1, 2, 3))
  adapter$getSystemMetadata()

  query_args <- list(
    symbolName = "demand",
    scenarioNames = c("Base", "High Demand", "Low Demand"),
    selectColumn = "value",
    aggregation = "SUM",
    analysisMode = "DIFFERENCE",
    baselineScenarioName = "Base",
    orderBy = "_delta",
    sortDirection = "DESC"
  )

  res <- adapter$queryScenarioDatabase(query_args)
  res_df <- jsonlite::fromJSON(res)

  # Base total = 250; High Demand delta = -50; Low Demand delta = -200.
  # DESC ordering must hold across the unioned per-target branches.
  expect_equal(res_df$`_delta`, c(-50, -200))
  expect_equal(res_df$`_target_scenario`, c("High Demand", "Low Demand"))

  DBI::dbDisconnect(env$conn)
})

test_that("queryScenarioDatabase rejects DIFFERENCE/PERCENT_CHANGE without selectColumn", {
  env <- setup_test_env()
  adapter <- env$adapter
  adapter$setAllowedSids(c(1, 2, 3))
  adapter$getSystemMetadata()

  query_args <- list(
    symbolName = "demand",
    scenarioNames = c("Base", "High Demand"),
    analysisMode = "DIFFERENCE",
    baselineScenarioName = "Base"
  )

  res <- adapter$queryScenarioDatabase(query_args)
  res_list <- jsonlite::fromJSON(res)

  expect_true(!is.null(res_list$error))
  expect_match(res_list$error, "selectColumn")

  DBI::dbDisconnect(env$conn)
})

test_that("queryScenarioDatabase rejects DIFFERENCE with only one valid scenario", {
  env <- setup_test_env()
  adapter <- env$adapter
  adapter$setAllowedSids(c(1, 2, 3))
  adapter$getSystemMetadata()

  query_args <- list(
    symbolName = "demand",
    scenarioNames = c("Base"),
    selectColumn = "value",
    aggregation = "SUM",
    analysisMode = "DIFFERENCE",
    baselineScenarioName = "Base"
  )

  res <- adapter$queryScenarioDatabase(query_args)
  res_list <- jsonlite::fromJSON(res)

  expect_true(!is.null(res_list$error))
  expect_match(res_list$error, "at least two valid scenarioNames")

  DBI::dbDisconnect(env$conn)
})

test_that("queryScenarioDatabase rejects DIFFERENCE without a baselineScenarioName", {
  env <- setup_test_env()
  adapter <- env$adapter
  adapter$setAllowedSids(c(1, 2, 3))
  adapter$getSystemMetadata()

  query_args <- list(
    symbolName = "demand",
    scenarioNames = c("Base", "High Demand", "Low Demand"),
    selectColumn = "value",
    aggregation = "SUM",
    analysisMode = "DIFFERENCE"
  )

  res <- adapter$queryScenarioDatabase(query_args)
  res_list <- jsonlite::fromJSON(res)

  expect_true(!is.null(res_list$error))
  expect_match(res_list$error, "baselineScenarioName is required")

  DBI::dbDisconnect(env$conn)
})

test_that("queryScenarioDatabase rejects an invalid analysisMode", {
  env <- setup_test_env()
  adapter <- env$adapter
  adapter$setAllowedSids(c(1, 2, 3))
  adapter$getSystemMetadata()

  query_args <- list(
    symbolName = "demand",
    scenarioNames = c("Base", "High Demand"),
    analysisMode = "BOGUS_MODE"
  )

  res <- adapter$queryScenarioDatabase(query_args)
  res_list <- jsonlite::fromJSON(res)

  expect_true(!is.null(res_list$error))
  expect_match(res_list$error, "Invalid analysisMode")

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

setup_multi_target_truncation_env <- function() {
  conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  DBI::dbWriteTable(conn, "scen_meta", tibble::tibble(
    `_sid` = 1:5,
    `_uid` = "user",
    `_sname` = c("Base", "T1", "T2", "T3", "T4"),
    `_stag` = "test"
  ))

  # Every scenario has 30 region rows; with the default limit of 50, a target
  # scenario more than ~2 branches deep should be pushed out of the result.
  regionNames <- sprintf("R%02d", 1:30)
  rows <- do.call(rbind, lapply(1:5, function(sid) {
    data.frame(`_sid` = sid, region = regionNames, year = "2024", value = sid * 10 + seq_along(regionNames), check.names = FALSE)
  }))
  DBI::dbWriteTable(conn, "data_demand", rows)

  dbSchema <- MockDbSchema$new()
  adapter <- AIToolAdapter$new(dbSchema = dbSchema, conn = conn, ioConfig = mockIoConfig)

  list(conn = conn, adapter = adapter)
}

test_that("queryScenarioDatabase reports target scenarios dropped by truncation instead of hiding them", {
  env <- setup_multi_target_truncation_env()
  adapter <- env$adapter
  adapter$setAllowedSids(1:5)
  adapter$getSystemMetadata()

  query_args <- list(
    symbolName = "demand",
    scenarioNames = c("Base", "T1", "T2", "T3", "T4"),
    selectColumn = "value",
    aggregation = "SUM",
    groupByColumn = "region",
    analysisMode = "DIFFERENCE",
    baselineScenarioName = "Base"
  )

  res <- adapter$queryScenarioDatabase(query_args)
  res_list <- jsonlite::fromJSON(res)

  expect_true(!is.null(res_list$`_warning`))
  expect_match(res_list$`_warning`, "truncated to 50 rows")
  expect_true("_missing_target_scenarios" %in% names(res_list))
  expect_setequal(res_list$`_missing_target_scenarios`, c("T3", "T4"))
  expect_match(res_list$`_warning`, "T3")
  expect_match(res_list$`_warning`, "T4")

  # Scenarios that DO still have rows in the truncated output must not be
  # flagged as missing.
  expect_false("T1" %in% res_list$`_missing_target_scenarios`)
  expect_false("T2" %in% res_list$`_missing_target_scenarios`)

  DBI::dbDisconnect(env$conn)
})

test_that("queryScenarioDatabase returns a clean error for malformed JSON arguments instead of crashing", {
  env <- setup_test_env()
  adapter <- env$adapter
  adapter$setAllowedSids(c(1, 2, 3))
  adapter$getSystemMetadata()

  res <- adapter$queryScenarioDatabase("{not valid json")
  res_list <- jsonlite::fromJSON(res)

  expect_true(!is.null(res_list$error))
  expect_match(res_list$error, "Invalid JSON argument format")

  DBI::dbDisconnect(env$conn)
})

test_that("queryScenarioDatabase rejects a missing symbolName instead of crashing", {
  env <- setup_test_env()
  adapter <- env$adapter
  adapter$setAllowedSids(c(1, 2, 3))
  adapter$getSystemMetadata()

  res <- adapter$queryScenarioDatabase(list(scenarioNames = c("Base")))
  res_list <- jsonlite::fromJSON(res)

  expect_true(!is.null(res_list$error))
  expect_match(res_list$error, "symbolName")

  DBI::dbDisconnect(env$conn)
})

test_that("queryScenarioDatabase rejects a missing scenarioNames", {
  env <- setup_test_env()
  adapter <- env$adapter
  adapter$setAllowedSids(c(1, 2, 3))
  adapter$getSystemMetadata()

  res <- adapter$queryScenarioDatabase(list(symbolName = "demand"))
  res_list <- jsonlite::fromJSON(res)

  expect_true(!is.null(res_list$error))
  expect_match(res_list$error, "scenarioNames")

  DBI::dbDisconnect(env$conn)
})

test_that("queryScenarioDatabase normalizes a single filter object sent instead of an array", {
  env <- setup_test_env()
  adapter <- env$adapter
  adapter$setAllowedSids(c(1, 2, 3))
  adapter$getSystemMetadata()

  argsJson <- jsonlite::toJSON(list(
    symbolName = "demand",
    scenarioNames = list("Base"),
    filters = list(column = "region", operator = "=", value = "EU")
  ), auto_unbox = TRUE)

  res <- adapter$queryScenarioDatabase(as.character(argsJson))
  res_df <- jsonlite::fromJSON(res)

  expect_equal(nrow(res_df), 1)
  expect_equal(res_df$region, "EU")
  expect_equal(res_df$value, 150)

  DBI::dbDisconnect(env$conn)
})

test_that("queryScenarioDatabase rejects a malformed filters shape instead of crashing", {
  env <- setup_test_env()
  adapter <- env$adapter
  adapter$setAllowedSids(c(1, 2, 3))
  adapter$getSystemMetadata()

  query_args <- list(
    symbolName = "demand",
    scenarioNames = c("Base"),
    filters = list("region")
  )

  res <- adapter$queryScenarioDatabase(query_args)
  res_list <- jsonlite::fromJSON(res)

  expect_true(!is.null(res_list$error))
  expect_match(res_list$error, "filters")

  DBI::dbDisconnect(env$conn)
})

test_that("queryScenarioDatabase requires an explicit aggregation for DIFFERENCE/PERCENT_CHANGE", {
  env <- setup_test_env()
  adapter <- env$adapter
  adapter$setAllowedSids(c(1, 2, 3))
  adapter$getSystemMetadata()

  query_args <- list(
    symbolName = "demand",
    scenarioNames = c("Base", "High Demand"),
    selectColumn = "value",
    analysisMode = "DIFFERENCE",
    baselineScenarioName = "Base"
    # aggregation omitted -> defaults to "NONE", which must now be rejected
  )

  res <- adapter$queryScenarioDatabase(query_args)
  res_list <- jsonlite::fromJSON(res)

  expect_true(!is.null(res_list$error))
  expect_match(res_list$error, "valid aggregation")

  DBI::dbDisconnect(env$conn)
})

test_that("queryScenarioDatabase returns error messages as scalar strings, not arrays", {
  env <- setup_test_env()
  adapter <- env$adapter
  adapter$setAllowedSids(c(1, 2, 3))
  adapter$getSystemMetadata()

  # This validation path previously omitted `auto_unbox = TRUE`, producing
  # {"error":["msg"]} instead of {"error":"msg"}.
  query_args <- list(
    symbolName = "demand",
    scenarioNames = c("Base"),
    filters = data.frame(column = "not_a_real_column", operator = "=", value = "x", stringsAsFactors = FALSE)
  )
  res <- adapter$queryScenarioDatabase(query_args)

  expect_match(res, '"error":"Forbidden target filter column configuration\\."')

  DBI::dbDisconnect(env$conn)
})

test_that("queryScenarioDatabase rejects array values for scalar-only arguments instead of crashing", {
  env <- setup_test_env()
  adapter <- env$adapter
  adapter$setAllowedSids(c(1, 2, 3))
  adapter$getSystemMetadata()

  cases <- list(
    list(field = "selectColumn", args = list(symbolName = "demand", scenarioNames = c("Base"), selectColumn = c("region", "value"))),
    list(field = "orderBy", args = list(symbolName = "demand", scenarioNames = c("Base"), orderBy = c("region", "value"))),
    list(field = "baselineScenarioName", args = list(symbolName = "demand", scenarioNames = c("Base", "High Demand"), selectColumn = "value", aggregation = "SUM", analysisMode = "DIFFERENCE", baselineScenarioName = c("Base", "High Demand"))),
    list(field = "analysisMode", args = list(symbolName = "demand", scenarioNames = c("Base"), analysisMode = c("STANDARD", "DIFFERENCE"))),
    list(field = "sortDirection", args = list(symbolName = "demand", scenarioNames = c("Base"), orderBy = "value", sortDirection = c("ASC", "DESC"))),
    list(field = "aggregation", args = list(symbolName = "demand", scenarioNames = c("Base"), selectColumn = "value", aggregation = c("SUM", "AVG")))
  )

  for (testCase in cases) {
    res <- tryCatch(adapter$queryScenarioDatabase(testCase$args), error = function(e) e)

    expect_false(inherits(res, "error"), info = sprintf("field '%s' crashed instead of returning an error JSON", testCase$field))

    res_list <- jsonlite::fromJSON(res)
    expect_true(!is.null(res_list$error), info = sprintf("field '%s' did not return an error", testCase$field))
    expect_match(res_list$error, sprintf("`%s`", testCase$field), fixed = TRUE)
    expect_match(res_list$error, "single string value")
  }

  DBI::dbDisconnect(env$conn)
})

test_that("queryScenarioDatabase still accepts these arguments as normal scalars", {
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
    baselineScenarioName = "Base",
    orderBy = "_delta",
    sortDirection = "DESC"
  )

  res <- adapter$queryScenarioDatabase(query_args)
  res_df <- jsonlite::fromJSON(res)

  expect_true(is.null(res_df$error))
  expect_true("_delta" %in% names(res_df))
  expect_equal(res_df$`_delta`, c(100, NA))

  DBI::dbDisconnect(env$conn)
})

setup_multi_dim_test_env <- function() {
  conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  DBI::dbWriteTable(conn, "scen_meta", tibble::tibble(
    `_sid` = c(1, 2),
    `_uid` = "user",
    `_sname` = c("Base", "High Demand"),
    `_stag` = c("production", "test")
  ))

  # region and year both vary independently so grouping by both is
  # distinguishable from grouping by either one alone.
  DBI::dbWriteTable(conn, "data_demand", tibble::tibble(
    `_sid` = c(1, 1, 1, 1, 2, 2, 2, 2),
    region = c("NA", "NA", "EU", "EU", "NA", "NA", "EU", "EU"),
    year = c("2024", "2025", "2024", "2025", "2024", "2025", "2024", "2025"),
    value = c(10, 20, 30, 40, 100, 200, 300, 400)
  ))

  dbSchema <- MockDbSchema$new()
  adapter <- AIToolAdapter$new(dbSchema = dbSchema, conn = conn, ioConfig = mockIoConfig)

  list(conn = conn, adapter = adapter)
}

test_that("queryScenarioDatabase groups by multiple columns in STANDARD mode", {
  env <- setup_multi_dim_test_env()
  adapter <- env$adapter
  adapter$setAllowedSids(c(1, 2))
  adapter$getSystemMetadata()

  query_args <- list(
    symbolName = "demand",
    scenarioNames = c("Base"),
    selectColumn = "value",
    aggregation = "SUM",
    groupByColumn = c("region", "year")
  )

  res <- adapter$queryScenarioDatabase(query_args)
  res_df <- jsonlite::fromJSON(res)

  # 2 regions x 2 years = 4 distinct groups
  expect_equal(nrow(res_df), 4)
  expect_true(all(c("region", "year") %in% names(res_df)))

  na_2024 <- res_df$`_sum`[res_df$region == "NA" & res_df$year == "2024"]
  eu_2025 <- res_df$`_sum`[res_df$region == "EU" & res_df$year == "2025"]

  expect_equal(na_2024, 10)
  expect_equal(eu_2025, 40)

  DBI::dbDisconnect(env$conn)
})

test_that("queryScenarioDatabase groups DIFFERENCE by multiple columns", {
  env <- setup_multi_dim_test_env()
  adapter <- env$adapter
  adapter$setAllowedSids(c(1, 2))
  adapter$getSystemMetadata()

  query_args <- list(
    symbolName = "demand",
    scenarioNames = c("Base", "High Demand"),
    selectColumn = "value",
    aggregation = "SUM",
    groupByColumn = c("region", "year"),
    analysisMode = "DIFFERENCE",
    baselineScenarioName = "Base"
  )

  res <- adapter$queryScenarioDatabase(query_args)
  res_df <- jsonlite::fromJSON(res)

  expect_equal(nrow(res_df), 4)
  expect_true(all(c("region", "year") %in% names(res_df)))

  delta_na_2024 <- res_df$`_delta`[res_df$region == "NA" & res_df$year == "2024"]
  delta_eu_2025 <- res_df$`_delta`[res_df$region == "EU" & res_df$year == "2025"]

  expect_equal(delta_na_2024, 100 - 10)
  expect_equal(delta_eu_2025, 400 - 40)

  DBI::dbDisconnect(env$conn)
})

test_that("queryScenarioDatabase rejects more than 3 groupByColumn entries", {
  env <- setup_test_env()
  adapter <- env$adapter
  adapter$setAllowedSids(c(1, 2, 3))
  adapter$getSystemMetadata()

  query_args <- list(
    symbolName = "demand",
    scenarioNames = c("Base"),
    selectColumn = "value",
    aggregation = "SUM",
    groupByColumn = c("region", "year", "value", "_sid")
  )

  res <- adapter$queryScenarioDatabase(query_args)
  res_list <- jsonlite::fromJSON(res)

  expect_true(!is.null(res_list$error))
  expect_match(res_list$error, "at most 3 columns")

  DBI::dbDisconnect(env$conn)
})

test_that("queryScenarioDatabase rejects an invalid column inside groupByColumn", {
  env <- setup_test_env()
  adapter <- env$adapter
  adapter$setAllowedSids(c(1, 2, 3))
  adapter$getSystemMetadata()

  query_args <- list(
    symbolName = "demand",
    scenarioNames = c("Base"),
    selectColumn = "value",
    aggregation = "SUM",
    groupByColumn = c("region", "not_a_real_column")
  )

  res <- adapter$queryScenarioDatabase(query_args)
  res_list <- jsonlite::fromJSON(res)

  expect_true(!is.null(res_list$error))
  expect_match(res_list$error, "not_a_real_column")

  DBI::dbDisconnect(env$conn)
})

test_that("queryScenarioDatabase treats an empty groupByColumn array as no grouping", {
  env <- setup_multi_dim_test_env()
  adapter <- env$adapter
  adapter$setAllowedSids(c(1, 2))
  adapter$getSystemMetadata()

  query_args <- list(
    symbolName = "demand",
    scenarioNames = c("Base"),
    selectColumn = "value",
    aggregation = "SUM",
    groupByColumn = list()
  )

  res <- adapter$queryScenarioDatabase(query_args)
  res_df <- jsonlite::fromJSON(res)

  # No group-by columns -> a single grand-total row for the Base scenario:
  # 10 + 20 + 30 + 40 = 100
  expect_equal(nrow(res_df), 1)
  expect_equal(res_df$`_sum`, 100)

  DBI::dbDisconnect(env$conn)
})

test_that("queryScenarioDatabase treats a single-element groupByColumn array like the old scalar form", {
  env <- setup_multi_dim_test_env()
  adapter <- env$adapter
  adapter$setAllowedSids(c(1, 2))
  adapter$getSystemMetadata()

  query_args <- list(
    symbolName = "demand",
    scenarioNames = c("Base"),
    selectColumn = "value",
    aggregation = "SUM",
    groupByColumn = list("region")
  )

  res <- adapter$queryScenarioDatabase(query_args)
  res_df <- jsonlite::fromJSON(res)

  # Grouped by region alone: NA = 10 + 20 = 30, EU = 30 + 40 = 70
  expect_equal(nrow(res_df), 2)
  expect_false("year" %in% names(res_df))
  expect_equal(res_df$`_sum`[res_df$region == "NA"], 30)
  expect_equal(res_df$`_sum`[res_df$region == "EU"], 70)

  DBI::dbDisconnect(env$conn)
})

test_that("queryScenarioDatabase surfaces a database error instead of a mapping failure", {
  # In DIFFERENCE/PERCENT_CHANGE mode the scenario-name mapping selects `_target_scenario`
  # by name. The database-error fallback frame has only an `error` column, so letting it
  # reach that mapping replaced the real message with "undefined columns selected".
  env <- setup_test_env()
  adapter <- env$adapter
  adapter$setAllowedSids(c(1, 2, 3))
  adapter$getSystemMetadata()

  DBI::dbExecute(env$conn, "DROP TABLE data_demand")

  res <- adapter$queryScenarioDatabase(list(
    symbolName = "demand",
    scenarioNames = c("Base", "High Demand"),
    selectColumn = "value",
    aggregation = "SUM",
    analysisMode = "DIFFERENCE",
    baselineScenarioName = "Base"
  ))

  expect_match(jsonlite::fromJSON(res)$error, "Database Engine Interface Error")
  expect_no_match(res, "undefined columns selected", fixed = TRUE)

  DBI::dbDisconnect(env$conn)
})

test_that("queryScenarioDatabase returns an empty result rather than erroring in DIFFERENCE mode", {
  # A length-1 column assignment onto a zero-row frame fails with
  # "replacement has 1 row, data has 0", so an empty diff result used to be reported
  # to the model as a tool execution failure.
  env <- setup_test_env()
  adapter <- env$adapter
  adapter$setAllowedSids(c(1, 2, 3))
  adapter$getSystemMetadata()

  res <- adapter$queryScenarioDatabase(list(
    symbolName = "demand",
    scenarioNames = c("Base", "High Demand"),
    selectColumn = "value",
    aggregation = "SUM",
    groupByColumn = "region",
    analysisMode = "DIFFERENCE",
    baselineScenarioName = "Base",
    filters = data.frame(
      column = "region", operator = "=", value = "does-not-exist",
      stringsAsFactors = FALSE
    )
  ))

  # a zero-row frame serializes to `[]`, which parses back to an empty list
  expect_false("error" %in% names(jsonlite::fromJSON(res)))
  expect_length(jsonlite::fromJSON(res), 0L)

  DBI::dbDisconnect(env$conn)
})

test_that("describeToolCall returns NULL for tools other than queryScenarioDatabase", {
  env <- setup_test_env()
  adapter <- env$adapter
  adapter$setAllowedSids(c(1, 2, 3))
  adapter$getSystemMetadata()

  expect_null(adapter$describeToolCall("modifySandboxInputData", list(symbolName = "demand"), "{}"))

  DBI::dbDisconnect(env$conn)
})

test_that("describeToolCall summarizes a successful query with its symbol alias and row count", {
  env <- setup_test_env()
  adapter <- env$adapter
  adapter$setAllowedSids(c(1, 2, 3))
  adapter$getSystemMetadata()

  query_args <- list(
    symbolName = "demand", scenarioNames = c("Base"),
    filters = data.frame(column = "region", operator = "=", value = "NA", stringsAsFactors = FALSE)
  )
  res <- adapter$queryScenarioDatabase(query_args)
  activity <- adapter$describeToolCall("queryScenarioDatabase", query_args, res)

  expect_equal(activity$symbolName, "demand")
  # the alias is looked up from the database schema, not just echoed back
  expect_equal(activity$symbolAlias, "Customer Demand")
  expect_equal(activity$scenarioNames, "Base")
  expect_equal(activity$rowCount, 1L)
  expect_false(activity$truncated)
  expect_null(activity$error)
  expect_equal(activity$filters[[1]]$column, "region")
  expect_equal(activity$filters[[1]]$value, "NA")

  DBI::dbDisconnect(env$conn)
})

test_that("describeToolCall reports the error message when the query is rejected", {
  env <- setup_test_env()
  adapter <- env$adapter
  adapter$setAllowedSids(c(1, 2, 3))
  adapter$getSystemMetadata()

  query_args <- list(symbolName = "nonexistent", scenarioNames = c("Base"))
  res <- adapter$queryScenarioDatabase(query_args)
  activity <- adapter$describeToolCall("queryScenarioDatabase", query_args, res)

  expect_match(activity$error, "does not exist")
  expect_null(activity$rowCount)
  # no symbol of that name exists, so the alias falls back to the raw name
  expect_equal(activity$symbolAlias, "nonexistent")

  DBI::dbDisconnect(env$conn)
})

test_that("describeToolCall flags a truncated result and surfaces the warning", {
  env <- setup_test_env()
  adapter <- env$adapter
  adapter$setAllowedSids(c(1, 2, 3))
  adapter$getSystemMetadata()

  query_args <- list(symbolName = "demand", scenarioNames = c("Base", "High Demand", "Low Demand"), limit = 1)
  res <- adapter$queryScenarioDatabase(query_args)
  activity <- adapter$describeToolCall("queryScenarioDatabase", query_args, res)

  expect_true(activity$truncated)
  expect_match(activity$warning, "truncated")
  expect_equal(activity$rowCount, 1L)
  expect_equal(activity$limit, 1L)

  DBI::dbDisconnect(env$conn)
})

test_that("describeToolCall reports zero rows for an empty result set", {
  env <- setup_test_env()
  adapter <- env$adapter
  adapter$setAllowedSids(c(1, 2, 3))
  adapter$getSystemMetadata()

  query_args <- list(
    symbolName = "demand", scenarioNames = c("Base"),
    filters = data.frame(column = "region", operator = "=", value = "does-not-exist", stringsAsFactors = FALSE)
  )
  res <- adapter$queryScenarioDatabase(query_args)
  activity <- adapter$describeToolCall("queryScenarioDatabase", query_args, res)

  expect_equal(activity$rowCount, 0L)
  expect_false(activity$truncated)
  expect_null(activity$error)

  DBI::dbDisconnect(env$conn)
})

test_that("describeToolCall omits NONE/STANDARD defaults and echoes explicit aggregation/analysisMode", {
  env <- setup_test_env()
  adapter <- env$adapter
  adapter$setAllowedSids(c(1, 2, 3))
  adapter$getSystemMetadata()

  plainArgs <- list(symbolName = "demand", scenarioNames = c("Base"), aggregation = "NONE", analysisMode = "STANDARD")
  res <- adapter$queryScenarioDatabase(plainArgs)
  plainActivity <- adapter$describeToolCall("queryScenarioDatabase", plainArgs, res)
  expect_null(plainActivity$aggregation)
  expect_null(plainActivity$analysisMode)

  aggArgs <- list(
    symbolName = "demand", scenarioNames = c("Base", "High Demand"),
    selectColumn = "value", aggregation = "SUM", groupByColumn = "region"
  )
  res2 <- adapter$queryScenarioDatabase(aggArgs)
  aggActivity <- adapter$describeToolCall("queryScenarioDatabase", aggArgs, res2)
  expect_equal(aggActivity$aggregation, "SUM")
  expect_equal(aggActivity$groupByColumn, "region")

  DBI::dbDisconnect(env$conn)
})

test_that("describeToolCall returns NULL when the tool arguments cannot be parsed", {
  env <- setup_test_env()
  adapter <- env$adapter
  adapter$setAllowedSids(c(1, 2, 3))
  adapter$getSystemMetadata()

  expect_null(adapter$describeToolCall("queryScenarioDatabase", "not-json{{", "{}"))

  DBI::dbDisconnect(env$conn)
})

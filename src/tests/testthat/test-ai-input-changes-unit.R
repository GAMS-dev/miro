library(R6)
library(DBI)
library(RSQLite)
library(jsonlite)

source("../../components/ai_adapter.R")

# Mock DbSchema exposing one editable table symbol, one readonly table symbol
# and one scalar widget (each scalar lives in its own single-column table).
MockModifyDbSchema <- R6Class("MockModifyDbSchema",
  public = list(
    getDbTableName = function(sym) {
      if (sym == "_scenMeta") {
        return("scen_meta")
      }
      return(paste0("data_", sym))
    },
    getDbTableNames = function() {
      c(
        "demand", "capacity", "maxstock", "label", "modeltype", "basregion",
        "markets", "schedule", "fixedcost"
      )
    },
    getDbSchema = function(sym) {
      if (sym == "_scenMeta") {
        return(list(colNames = c(sid = "_sid", uid = "_uid", sname = "_sname", stag = "_stag")))
      }
      if (sym %in% c("demand", "capacity")) {
        return(list(colNames = c("region", "year", "value")))
      }
      if (sym == "schedule") {
        return(list(colNames = c("day", "baseload", "extraload")))
      }
      if (sym == "fixedcost") {
        return(list(colNames = c("region", "cost")))
      }
      if (sym == "label") {
        return(list(colNames = c("region", "note")))
      }
      return(list(colNames = sym))
    }
  )
)

mockModifyIoConfig <- list(
  modelIn = list(
    demand = list(
      alias = "Customer Demand", type = "hot",
      headers = list(
        region = list(alias = "Region", type = "set"),
        year = list(alias = "Year", type = "set"),
        value = list(alias = "Value", type = "numeric")
      )
    ),
    capacity = list(
      alias = "Plant Capacity", type = "hot", readonly = TRUE,
      headers = list(
        region = list(alias = "Region", type = "set"),
        year = list(alias = "Year", type = "set"),
        value = list(alias = "Value", type = "numeric")
      )
    ),
    label = list(
      alias = "Region Labels", type = "hot",
      headers = list(
        region = list(alias = "Region", type = "set"),
        note = list(alias = "Note", type = "string")
      )
    ),
    maxstock = list(
      alias = "Maximum stock", type = "slider",
      slider = list(min = 1, max = 10, default = 5)
    ),
    modeltype = list(
      alias = "Model type", type = "dropdown",
      dropdown = list(
        choices = c("lp", "mip", "minlp"), aliases = c("LP", "MIP", "MINLP"),
        multiple = FALSE, selected = "lp"
      )
    ),
    basregion = list(
      alias = "Base region", type = "dropdown",
      dropdown = list(
        choices = character(0L), multiple = FALSE,
        dependencyConfig = list(
          fw = list(demand = list("region")),
          staticChoices = "all", staticAliases = "All regions"
        )
      )
    ),
    markets = list(
      alias = "Markets", type = "dropdown",
      dropdown = list(choices = c("NA", "EU"), multiple = TRUE)
    ),
    # `readonlyCols` is resolved into the headers during init
    schedule = list(
      alias = "Schedule", type = "hot",
      headers = list(
        day = list(alias = "Day", type = "string"),
        baseload = list(alias = "Base load", type = "numeric", readonly = TRUE),
        extraload = list(alias = "Extra load", type = "numeric")
      )
    ),
    fixedcost = list(
      alias = "Fixed cost", type = "hot",
      headers = list(
        region = list(alias = "Region", type = "string"),
        cost = list(alias = "Cost", type = "numeric", readonly = TRUE)
      )
    )
  ),
  modelOut = list()
)

setup_modify_test_env <- function(inputModificationEnabled = TRUE, ioConfig = mockModifyIoConfig) {
  conn <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")

  DBI::dbWriteTable(conn, "scen_meta", tibble::tibble(
    `_sid` = c(1, 2),
    `_uid` = "user",
    `_sname` = c("Sandbox", "Other"),
    `_stag` = c("", "")
  ))
  DBI::dbWriteTable(conn, "data_demand", tibble::tibble(
    `_sid` = c(1, 1, 1, 1, 2),
    region = c("NA", "EU", "NA", "EU", "EU"),
    year = c("2024", "2024", "2025", "2025", "2024"),
    value = c(100, 150, 200, NA, 999)
  ))
  DBI::dbWriteTable(conn, "data_capacity", tibble::tibble(
    `_sid` = 1, region = "EU", year = "2024", value = 50
  ))
  DBI::dbWriteTable(conn, "data_label", tibble::tibble(
    `_sid` = 1, region = "EU", note = "hello"
  ))
  DBI::dbWriteTable(conn, "data_maxstock", tibble::tibble(
    `_sid` = 1, maxstock = 4
  ))
  DBI::dbWriteTable(conn, "data_modeltype", tibble::tibble(
    `_sid` = 1, modeltype = "lp"
  ))
  DBI::dbWriteTable(conn, "data_basregion", tibble::tibble(
    `_sid` = 1, basregion = "all"
  ))
  DBI::dbWriteTable(conn, "data_markets", tibble::tibble(
    `_sid` = c(1, 1), markets = c("NA", "EU")
  ))
  DBI::dbWriteTable(conn, "data_schedule", tibble::tibble(
    `_sid` = c(1, 1), day = c("mon", "tue"), baseload = c(10, 20), extraload = c(1, 2)
  ))
  DBI::dbWriteTable(conn, "data_fixedcost", tibble::tibble(
    `_sid` = 1, region = "EU", cost = 42
  ))

  adapter <- AIToolAdapter$new(
    dbSchema = MockModifyDbSchema$new(), conn = conn, ioConfig = ioConfig,
    inputModificationEnabled = inputModificationEnabled
  )
  adapter$setAllowedSids(1)
  adapter$getSystemMetadata()

  list(conn = conn, adapter = adapter)
}

# --- shared helpers (run in both the AI worker and the Shiny session) ---------

test_that("matchAiInputChangeRows supports the documented operators", {
  data <- tibble::tibble(
    region = c("NA", "EU", "AP"),
    value = c(10, 20, NA)
  )
  eq <- list(list(columnIdx = 1L, operator = "=", value = "EU"))
  expect_equal(matchAiInputChangeRows(data, eq), c(FALSE, TRUE, FALSE))

  neq <- list(list(columnIdx = 1L, operator = "!=", value = "EU"))
  expect_equal(matchAiInputChangeRows(data, neq), c(TRUE, FALSE, TRUE))

  inOp <- list(list(columnIdx = 1L, operator = "IN", value = "EU, AP"))
  expect_equal(matchAiInputChangeRows(data, inOp), c(FALSE, TRUE, TRUE))

  # a missing value can not be compared, so it never matches and is left alone
  gt <- list(list(columnIdx = 2L, operator = ">", value = "15"))
  expect_equal(matchAiInputChangeRows(data, gt), c(FALSE, TRUE, FALSE))

  # no filters at all means every row
  expect_equal(matchAiInputChangeRows(data, list()), rep(TRUE, 3L))
})

test_that("matchAiInputChangeRows supports NOT IN like SQL", {
  data <- tibble::tibble(
    prc = c("EECOA-ST", "eecoa_x", "EEGAS-CC", NA, "100%"),
    value = c(1, 2, 3, 4, 5)
  )
  match <- function(operator, value, columnIdx = 1L, isNumericColumn = FALSE) {
    matchAiInputChangeRows(data, list(list(
      columnIdx = columnIdx, operator = operator, value = value, isNumericColumn = isNumericColumn
    )))
  }
  expect_equal(match("NOT IN", "EECOA-ST, 100%"), c(FALSE, TRUE, TRUE, FALSE, FALSE))
  expect_equal(match("NOT IN", "1.0,3", columnIdx = 2L, isNumericColumn = TRUE), c(FALSE, TRUE, FALSE, TRUE, TRUE))
})

test_that("modifySandboxInputData selects the same rows in the database as in R", {
  env <- setup_modify_test_env()
  DBI::dbAppendTable(env$conn, "data_demand", tibble::tibble(`_sid` = 1, region = NA_character_, year = "2026", value = 5))
  sandboxData <- DBI::dbGetQuery(env$conn, "SELECT region, year, value FROM data_demand WHERE _sid = 1")
  for (filter in list(
    list(column = "year", operator = "NOT IN", value = "2025"),
    list(column = "region", operator = "NOT IN", value = "NA"),
    list(column = "value", operator = "NOT IN", value = "150, 5"),
    list(column = "value", operator = ">=", value = "150"),
    list(column = "value", operator = "<=", value = "150")
  )) {
    res <- jsonlite::fromJSON(env$adapter$modifySandboxInputData(list(
      symbolName = "demand", targetColumn = "value", operation = "ADD", value = 1,
      filters = as.data.frame(filter, stringsAsFactors = FALSE)
    )))
    change <- env$adapter$getPendingInputChanges()[[length(env$adapter$getPendingInputChanges())]]
    expect_equal(res$valuesAffected, sum(matchAiInputChangeRows(sandboxData, change$filters)), info = filter$operator)
  }
  expect_match(jsonlite::fromJSON(env$adapter$modifySandboxInputData(list(
    symbolName = "demand", targetColumn = "value", operation = "ADD", value = 1,
    filters = data.frame(column = "region", operator = "LIKE", value = "E%")
  )))$error, "Invalid filter operator 'LIKE'")
  expect_match(jsonlite::fromJSON(env$adapter$modifySandboxInputData(list(
    symbolName = "demand", targetColumn = "value", operation = "ADD", value = 1,
    filters = data.frame(column = "region", operator = "IN", value = "")
  )))$error, "at least one value")
  expect_match(jsonlite::fromJSON(env$adapter$modifySandboxInputData(
    '{"symbolName":"demand","targetColumn":"value","operation":"ADD","value":1,"filters":[{"column":"region","operator":"NOT IN","value":null}]}'
  ))$error, "single string")
  expect_match(jsonlite::fromJSON(env$adapter$modifySandboxInputData(list(
    symbolName = "demand", targetColumn = "value", operation = "ADD", value = 1,
    filters = data.frame(column = "value", operator = ">", value = "Inf")
  )))$error, "not a number")
  DBI::dbDisconnect(env$conn)
})

test_that("matchAiInputChangeRows combines several filters with AND", {
  data <- tibble::tibble(
    region = c("EU", "EU", "NA"),
    year = c("2024", "2025", "2024")
  )
  filters <- list(
    list(columnIdx = 1L, operator = "=", value = "EU"),
    list(columnIdx = 2L, operator = "=", value = "2024")
  )
  expect_equal(matchAiInputChangeRows(data, filters), c(TRUE, FALSE, FALSE))
})

test_that("applyAiInputChange only touches the target column of matching rows", {
  data <- tibble::tibble(
    region = c("NA", "EU", "EU"),
    value = c(100, 150, NA),
    cost = c(1, 2, 3)
  )
  change <- list(
    symbolName = "demand", isScalar = FALSE, operation = "MULTIPLY", value = 1.5,
    targetColumnIdx = 2L,
    filters = list(list(columnIdx = 1L, operator = "=", value = "EU"))
  )
  res <- applyAiInputChange(data, change)

  expect_equal(res$data$value, c(100, 225, NA))
  expect_equal(res$data$region, data$region)
  expect_equal(res$data$cost, data$cost)
  expect_equal(res$rowMask, c(FALSE, TRUE, TRUE))
  expect_true(tibble::is_tibble(res$data))
})

test_that("applyAiInputChange implements every supported operation", {
  data <- tibble::tibble(value = c(10, 20))
  change <- list(symbolName = "x", isScalar = FALSE, targetColumnIdx = 1L, filters = list())

  expect_equal(applyAiInputChange(data, modifyList(change, list(operation = "MULTIPLY", value = 2)))$data$value, c(20, 40))
  expect_equal(applyAiInputChange(data, modifyList(change, list(operation = "DIVIDE", value = 2)))$data$value, c(5, 10))
  expect_equal(applyAiInputChange(data, modifyList(change, list(operation = "ADD", value = 5)))$data$value, c(15, 25))
  expect_equal(applyAiInputChange(data, modifyList(change, list(operation = "SUBTRACT", value = 5)))$data$value, c(5, 15))
  expect_equal(applyAiInputChange(data, modifyList(change, list(operation = "SET", value = 7)))$data$value, c(7, 7))
})

test_that("applyAiInputChange handles scalar widgets", {
  change <- list(symbolName = "maxstock", isScalar = TRUE, operation = "ADD", value = 2)
  res <- applyAiInputChange(4, change)

  expect_equal(res$data, 6)
  expect_equal(res$oldValues, 4)
  expect_equal(res$newValues, 6)
})

test_that("buildAiInputChangePreview reports the affected rows and truncates", {
  data <- tibble::tibble(
    region = rep("EU", 5L), year = as.character(2020:2024), value = c(1, 2, 3, 4, 5)
  )
  change <- list(
    symbolName = "demand", isScalar = FALSE, operation = "MULTIPLY", value = 2,
    targetColumnIdx = 3L, columnAliases = c("Region", "Year", "Value"), filters = list()
  )
  res <- applyAiInputChange(data, change)
  preview <- buildAiInputChangePreview(data, change, res, maxRows = 2L)

  expect_equal(preview$noRows, 5L)
  expect_equal(preview$noRowsShown, 2L)
  expect_equal(names(preview$labels), c("Region", "Year"))
  expect_equal(preview$before, c("1", "2"))
  expect_equal(preview$after, c("2", "4"))
})

# --- tool schema / metadata ---------------------------------------------------

test_that("the modification tool is only offered when input changes are enabled", {
  env <- setup_modify_test_env(inputModificationEnabled = FALSE)
  toolNames <- vapply(env$adapter$getToolSchema(), "[[", character(1L), "name")
  expect_false("modifySandboxInputData" %in% toolNames)
  DBI::dbDisconnect(env$conn)

  env <- setup_modify_test_env()
  schema <- env$adapter$getToolSchema()
  toolNames <- vapply(schema, "[[", character(1L), "name")
  expect_true("modifySandboxInputData" %in% toolNames)

  modifyTool <- schema[[match("modifySandboxInputData", toolNames)]]
  expect_equal(modifyTool$parameters$required, c("symbolName", "operation"))
  expect_equal(modifyTool$parameters$properties$textValue$type, "string")
  expect_equal(modifyTool$parameters$properties$operation$enum, AI_INPUT_CHANGE_OPERATIONS)
  DBI::dbDisconnect(env$conn)
})

test_that("getSystemMetadata advertises modifiable columns of writable symbols only", {
  env <- setup_modify_test_env()
  meta <- jsonlite::fromJSON(env$adapter$getSystemMetadata(), simplifyVector = FALSE)

  expect_equal(meta$databaseSymbols$demand$modifiable, list(kind = "numeric", columns = list("value")))
  expect_equal(meta$databaseSymbols$maxstock$modifiable, list(kind = "numeric", columns = list("maxstock")))
  expect_equal(meta$databaseSymbols$modeltype$modifiable, list(kind = "choice"))
  expect_equal(meta$databaseSymbols$basregion$modifiable, list(kind = "choice"))
  # readonly table, table without any numeric column, multi-select dropdown
  expect_null(meta$databaseSymbols$capacity$modifiable)
  expect_null(meta$databaseSymbols$label$modifiable)
  expect_null(meta$databaseSymbols$markets$modifiable)

  DBI::dbDisconnect(env$conn)
})

test_that("a widget drawn by a custom widget is modifiable only if that renderer is reactive", {
  ioConfig <- mockModifyIoConfig
  ioConfig$modelIn$maxstock$definedByExternalSymbol <- "stockwidget"
  ioConfig$modelIn$stockwidget <- list(alias = "Stock", type = "custom", apiVersion = 2L)
  env <- setup_modify_test_env(ioConfig = ioConfig)
  meta <- jsonlite::fromJSON(env$adapter$getSystemMetadata(), simplifyVector = FALSE)
  expect_equal(meta$databaseSymbols$maxstock$modifiable, list(kind = "numeric", columns = list("maxstock")))
  DBI::dbDisconnect(env$conn)

  ioConfig$modelIn$stockwidget$apiVersion <- NULL
  env <- setup_modify_test_env(ioConfig = ioConfig)
  meta <- jsonlite::fromJSON(env$adapter$getSystemMetadata(), simplifyVector = FALSE)
  expect_null(meta$databaseSymbols$maxstock$modifiable)
  DBI::dbDisconnect(env$conn)
})

test_that("getSystemMetadata hides modifiable columns when changes are disabled", {
  env <- setup_modify_test_env(inputModificationEnabled = FALSE)
  meta <- jsonlite::fromJSON(env$adapter$getSystemMetadata(), simplifyVector = FALSE)

  expect_null(meta$databaseSymbols$demand$modifiable)

  DBI::dbDisconnect(env$conn)
})

# --- proposing a change -------------------------------------------------------

test_that("modifySandboxInputData queues a proposal instead of writing to the database", {
  env <- setup_modify_test_env()
  adapter <- env$adapter

  res <- jsonlite::fromJSON(adapter$modifySandboxInputData(list(
    symbolName = "demand", targetColumn = "value", operation = "MULTIPLY", value = 1.1,
    filters = data.frame(column = "region", operator = "=", value = "EU", stringsAsFactors = FALSE),
    reason = "User asked for 10% more European demand."
  )))

  expect_equal(res$status, "AWAITING_USER_CONFIRMATION")
  expect_equal(res$valuesAffected, 2L)
  expect_equal(res$before$sum, 150)
  expect_equal(res$after$sum, 165)

  # the sandbox data in the database is untouched
  expect_equal(
    DBI::dbGetQuery(env$conn, "SELECT value FROM data_demand WHERE _sid = 1 AND region = 'EU' AND year = '2024'")$value,
    150
  )

  # the missing value stays missing, so only one of the two EU rows has a number
  expect_equal(res$preview$`_before`, c("150", "NA"))
  expect_equal(res$preview$`_after`, c("165", "NA"))
  expect_equal(res$preview$Region, c("EU", "EU"))

  pending <- adapter$getPendingInputChanges()
  expect_length(pending, 1L)
  expect_equal(pending[[1L]]$symbolName, "demand")
  expect_equal(pending[[1L]]$operation, "MULTIPLY")
  expect_equal(pending[[1L]]$value, 1.1)
  expect_equal(pending[[1L]]$targetColumnIdx, 3L)
  expect_equal(pending[[1L]]$filters[[1L]]$columnIdx, 1L)
  expect_equal(pending[[1L]]$reason, "User asked for 10% more European demand.")

  DBI::dbDisconnect(env$conn)
})

test_that("modifySandboxInputData resolves a scalar widget without a target column", {
  env <- setup_modify_test_env()
  res <- jsonlite::fromJSON(env$adapter$modifySandboxInputData(list(
    symbolName = "maxstock", operation = "SET", value = 8
  )))

  expect_equal(res$status, "AWAITING_USER_CONFIRMATION")
  expect_equal(res$preview$`_before`, "4")
  expect_equal(res$preview$`_after`, "8")
  expect_true(env$adapter$getPendingInputChanges()[[1L]]$isScalar)

  DBI::dbDisconnect(env$conn)
})

test_that("modifySandboxInputData proposes one change per call", {
  env <- setup_modify_test_env()
  env$adapter$modifySandboxInputData(list(symbolName = "maxstock", operation = "SET", value = 8))
  env$adapter$modifySandboxInputData(list(
    symbolName = "demand", targetColumn = "value", operation = "ADD", value = 1
  ))

  pending <- env$adapter$getPendingInputChanges()
  expect_length(pending, 2L)
  expect_false(identical(pending[[1L]]$changeId, pending[[2L]]$changeId))

  DBI::dbDisconnect(env$conn)
})

# --- refusals -----------------------------------------------------------------

expect_modify_error <- function(adapter, args, pattern) {
  res <- jsonlite::fromJSON(adapter$modifySandboxInputData(args))
  expect_false(is.null(res$error))
  expect_match(res$error, pattern)
  expect_length(adapter$getPendingInputChanges(), 0L)
}

test_that("modifySandboxInputData refuses symbols the user cannot edit either", {
  env <- setup_modify_test_env()

  # readonly table
  expect_modify_error(env$adapter, list(
    symbolName = "capacity", targetColumn = "value", operation = "MULTIPLY", value = 2
  ), "cannot be modified")
  # output-only/unknown symbol
  expect_modify_error(env$adapter, list(
    symbolName = "notasymbol", operation = "MULTIPLY", value = 2
  ), "not an input symbol")
  # table without a numeric column
  expect_modify_error(env$adapter, list(
    symbolName = "label", targetColumn = "note", operation = "MULTIPLY", value = 2
  ), "cannot be modified")

  DBI::dbDisconnect(env$conn)
})

test_that("modifySandboxInputData validates the operation and its operand", {
  env <- setup_modify_test_env()
  base <- list(symbolName = "demand", targetColumn = "value")

  expect_modify_error(env$adapter, c(base, list(operation = "SQUARE", value = 2)), "`operation` is required")
  expect_modify_error(env$adapter, c(base, list(operation = "MULTIPLY")), "`value` is required")
  expect_modify_error(env$adapter, list(
    symbolName = "markets", operation = "SET", textValue = "EU"
  ), "cannot be modified")
  expect_modify_error(env$adapter, c(base, list(operation = "MULTIPLY", value = "abc")), "`value` is required")
  expect_modify_error(env$adapter, c(base, list(operation = "DIVIDE", value = 0)), "divide by zero")

  DBI::dbDisconnect(env$conn)
})

test_that("modifySandboxInputData validates the target column", {
  env <- setup_modify_test_env()

  expect_modify_error(env$adapter, list(
    symbolName = "demand", operation = "MULTIPLY", value = 2
  ), "`targetColumn` is required")
  expect_modify_error(env$adapter, list(
    symbolName = "demand", targetColumn = "nope", operation = "MULTIPLY", value = 2
  ), "Invalid `targetColumn`")
  expect_modify_error(env$adapter, list(
    symbolName = "demand", targetColumn = "region", operation = "MULTIPLY", value = 2
  ), "is not numeric")

  DBI::dbDisconnect(env$conn)
})

test_that("modifySandboxInputData validates filters", {
  env <- setup_modify_test_env()
  base <- list(symbolName = "demand", targetColumn = "value", operation = "MULTIPLY", value = 2)

  expect_modify_error(env$adapter, c(base, list(
    filters = data.frame(column = "nope", operator = "=", value = "EU", stringsAsFactors = FALSE)
  )), "Invalid filter column")
  expect_modify_error(env$adapter, c(base, list(
    filters = data.frame(column = "region", operator = "~", value = "EU", stringsAsFactors = FALSE)
  )), "Invalid filter operator")
  expect_modify_error(env$adapter, c(base, list(
    filters = data.frame(column = "region", operator = ">", value = "EU", stringsAsFactors = FALSE)
  )), "requires a numeric filter column")
  # a numeric column is compared numerically, in R and in SQL alike
  expect_modify_error(env$adapter, c(base, list(
    filters = data.frame(column = "value", operator = "=", value = "lots", stringsAsFactors = FALSE)
  )), "is not a number")
  # filters that match nothing would silently produce a no-op proposal
  expect_modify_error(env$adapter, c(base, list(
    filters = data.frame(column = "region", operator = "=", value = "MARS", stringsAsFactors = FALSE)
  )), "No row of symbol")
  # a scalar widget has no rows to filter
  expect_modify_error(env$adapter, list(
    symbolName = "maxstock", operation = "SET", value = 8,
    filters = data.frame(column = "maxstock", operator = "=", value = "4", stringsAsFactors = FALSE)
  ), "single value")

  DBI::dbDisconnect(env$conn)
})

test_that("modifySandboxInputData keeps scalar widgets inside their slider bounds", {
  env <- setup_modify_test_env()

  expect_modify_error(env$adapter, list(
    symbolName = "maxstock", operation = "SET", value = 99
  ), "exceeds the maximum")
  expect_modify_error(env$adapter, list(
    symbolName = "maxstock", operation = "SUBTRACT", value = 10
  ), "below the minimum")

  DBI::dbDisconnect(env$conn)
})

test_that("modifySandboxInputData refuses to run outside a single sandbox context", {
  env <- setup_modify_test_env(inputModificationEnabled = FALSE)
  expect_modify_error(env$adapter, list(
    symbolName = "maxstock", operation = "SET", value = 8
  ), "cannot be modified in the current context")
  DBI::dbDisconnect(env$conn)

  env <- setup_modify_test_env()
  env$adapter$setAllowedSids(c(1, 2))
  expect_modify_error(env$adapter, list(
    symbolName = "maxstock", operation = "SET", value = 8
  ), "single sandbox scenario")
  DBI::dbDisconnect(env$conn)
})

# --- worker proposal -> session applies it to the live sandbox data ------------

test_that("a queued proposal applies cleanly to the live sandbox data", {
  # what the AI worker queues is re-applied by the Shiny session to the tibble
  # the input widget holds, so the resolved column/row indices have to line up
  # with the widget's data, not just with the database dump the worker saw
  env <- setup_modify_test_env()
  env$adapter$modifySandboxInputData(list(
    symbolName = "demand", targetColumn = "value", operation = "MULTIPLY", value = 2,
    filters = data.frame(column = "year", operator = "=", value = "2024", stringsAsFactors = FALSE)
  ))
  change <- env$adapter$getPendingInputChanges()[[1L]]

  liveData <- tibble::tibble(
    region = c("NA", "EU", "NA", "EU"),
    year = c("2024", "2024", "2025", "2025"),
    value = c(100, 150, 200, NA)
  )
  res <- applyAiInputChange(liveData, change)

  expect_equal(res$data$value, c(200, 300, 200, NA))
  expect_equal(names(res$data), names(liveData))

  preview <- buildAiInputChangePreview(liveData, change, res)
  expect_equal(preview$noRows, 2L)
  expect_equal(names(preview$labels), c("Region", "Year"))
  expect_equal(preview$before, c("100", "150"))
  expect_equal(preview$after, c("200", "300"))

  DBI::dbDisconnect(env$conn)
})

test_that("buildAiInputChangePreview drops the label table for a single column symbol", {
  data <- tibble::tibble(value = c(1, 2))
  change <- list(
    symbolName = "x", isScalar = FALSE, operation = "ADD", value = 1,
    targetColumnIdx = 1L, columnAliases = "Value", filters = list()
  )
  preview <- buildAiInputChangePreview(data, change, applyAiInputChange(data, change))

  expect_null(preview$labels)
  expect_equal(preview$after, c("2", "3"))
})

test_that("a change over a large table is summarized without reading it all", {
  # the worker only ever sees the head of the affected rows, no matter how many
  # of them there are
  env <- setup_modify_test_env()
  DBI::dbExecute(env$conn, "DELETE FROM data_demand")
  bigTable <- tibble::tibble(
    `_sid` = 1L,
    region = rep(c("EU", "NA"), each = 5000L),
    year = "2024",
    value = c(rep(2, 5000L), rep(100, 5000L))
  )
  DBI::dbWriteTable(env$conn, "data_demand", bigTable, append = TRUE)

  res <- jsonlite::fromJSON(env$adapter$modifySandboxInputData(list(
    symbolName = "demand", targetColumn = "value", operation = "MULTIPLY", value = 3,
    filters = data.frame(column = "region", operator = "=", value = "EU", stringsAsFactors = FALSE)
  )))

  expect_equal(res$valuesAffected, 5000L)
  expect_equal(res$before, list(count = 5000L, sum = 10000, min = 2, max = 2))
  expect_equal(res$after, list(count = 5000L, sum = 30000, min = 6, max = 6))
  expect_equal(nrow(res$preview), AI_INPUT_CHANGE_MODEL_PREVIEW_ROWS)
  expect_match(res$previewNote, "first 10 of 5000 affected rows")

  DBI::dbDisconnect(env$conn)
})

test_that("deriveAiInputChangeSummary matches a full recomputation", {
  values <- c(-4, -1, 0, 2.5, 10, NA)
  before <- list(
    count = sum(!is.na(values)), sum = sum(values, na.rm = TRUE),
    min = min(values, na.rm = TRUE), max = max(values, na.rm = TRUE)
  )
  for (operation in AI_INPUT_CHANGE_OPERATIONS) {
    for (operand in c(3, -2, 0.5)) {
      after <- deriveAiInputChangeSummary(before, length(values), operation, operand)
      if (identical(operation, "SET")) {
        # SET also fills the missing value, so every row ends up counting
        expect_equal(after$count, length(values), info = operation)
        expect_equal(after$sum, length(values) * operand, info = operation)
        expect_equal(after$min, operand, info = operation)
        next
      }
      expected <- computeAiInputChangeValues(values, operation, operand)
      expect_equal(after$sum, sum(expected, na.rm = TRUE), info = paste(operation, operand))
      expect_equal(after$min, min(expected, na.rm = TRUE), info = paste(operation, operand))
      expect_equal(after$max, max(expected, na.rm = TRUE), info = paste(operation, operand))
    }
  }
})

test_that("IN and ordering filters translate to the same rows in SQL as in R", {
  # the worker filters in SQL, the session filters in R -- they have to agree
  env <- setup_modify_test_env()
  liveData <- tibble::tibble(
    region = c("NA", "EU", "NA", "EU"),
    year = c("2024", "2024", "2025", "2025"),
    value = c(100, 150, 200, NA)
  )
  for (filters in list(
    data.frame(column = "region", operator = "IN", value = "EU, AP", stringsAsFactors = FALSE),
    data.frame(column = "value", operator = ">", value = "120", stringsAsFactors = FALSE),
    data.frame(column = "region", operator = "!=", value = "EU", stringsAsFactors = FALSE)
  )) {
    res <- jsonlite::fromJSON(env$adapter$modifySandboxInputData(list(
      symbolName = "demand", targetColumn = "value", operation = "ADD", value = 1,
      filters = filters
    )))
    change <- utils::tail(env$adapter$getPendingInputChanges(), 1L)[[1L]]
    expect_equal(
      res$valuesAffected, sum(matchAiInputChangeRows(liveData, change$filters)),
      info = paste(filters$column, filters$operator, filters$value)
    )
  }

  DBI::dbDisconnect(env$conn)
})

# --- choice symbols (single-select dropdowns) ---------------------------------

test_that("modifySandboxInputData proposes a new selection for a dropdown", {
  env <- setup_modify_test_env()
  res <- jsonlite::fromJSON(env$adapter$modifySandboxInputData(list(
    symbolName = "modeltype", operation = "SET", textValue = "mip",
    reason = "User asked to solve it as a MIP."
  )))

  expect_equal(res$status, "AWAITING_USER_CONFIRMATION")
  expect_equal(res$currentValue, "lp")
  expect_equal(res$newValue, "mip")

  change <- env$adapter$getPendingInputChanges()[[1L]]
  expect_true(change$isChoice)
  expect_equal(change$operation, "SET")
  expect_equal(change$value, "mip")

  # the value in the database is untouched
  expect_equal(
    DBI::dbGetQuery(env$conn, "SELECT modeltype FROM data_modeltype WHERE _sid = 1")$modeltype,
    "lp"
  )

  DBI::dbDisconnect(env$conn)
})

test_that("modifySandboxInputData accepts a dropdown label in place of its value", {
  env <- setup_modify_test_env()
  res <- jsonlite::fromJSON(env$adapter$modifySandboxInputData(list(
    symbolName = "modeltype", operation = "SET", textValue = "MINLP"
  )))

  expect_equal(res$newValue, "minlp")
  expect_equal(env$adapter$getPendingInputChanges()[[1L]]$value, "minlp")

  DBI::dbDisconnect(env$conn)
})

test_that("modifySandboxInputData reads dependent dropdown choices from the scenario", {
  # `basregion` offers "all" plus whatever regions the demand table holds
  env <- setup_modify_test_env()
  res <- jsonlite::fromJSON(env$adapter$modifySandboxInputData(list(
    symbolName = "basregion", operation = "SET", textValue = "EU"
  )))

  expect_equal(res$currentValue, "all")
  expect_equal(res$newValue, "EU")
  DBI::dbDisconnect(env$conn)

  env <- setup_modify_test_env()
  expect_modify_error(env$adapter, list(
    symbolName = "basregion", operation = "SET", textValue = "MARS"
  ), "Valid values: all, NA, EU")

  DBI::dbDisconnect(env$conn)
})

test_that("modifySandboxInputData rejects nonsensical choice changes", {
  env <- setup_modify_test_env()

  expect_modify_error(env$adapter, list(
    symbolName = "modeltype", operation = "SET", textValue = "quadratic"
  ), "is not one of the values")
  expect_modify_error(env$adapter, list(
    symbolName = "modeltype", operation = "MULTIPLY", value = 2
  ), "only the SET operation")
  expect_modify_error(env$adapter, list(
    symbolName = "modeltype", operation = "SET", value = 2
  ), "`textValue` is required")
  # a no-op proposal would only cost the user a confirmation click
  expect_modify_error(env$adapter, list(
    symbolName = "modeltype", operation = "SET", textValue = "lp"
  ), "already set to")

  DBI::dbDisconnect(env$conn)
})

test_that("a queued choice change applies to the live dropdown value", {
  env <- setup_modify_test_env()
  env$adapter$modifySandboxInputData(list(
    symbolName = "modeltype", operation = "SET", textValue = "mip"
  ))
  change <- env$adapter$getPendingInputChanges()[[1L]]

  # a dropdown widget holds its selection as a plain string
  res <- applyAiInputChange("lp", change)
  expect_equal(res$data, "mip")
  expect_equal(res$oldValues, "lp")

  preview <- buildAiInputChangePreview("lp", change, res)
  expect_null(preview$labels)
  expect_equal(preview$before, "lp")
  expect_equal(preview$after, "mip")

  DBI::dbDisconnect(env$conn)
})

# --- readonly columns ---------------------------------------------------------

test_that("a readonly column is left out of the columns that may be changed", {
  env <- setup_modify_test_env()
  meta <- jsonlite::fromJSON(env$adapter$getSystemMetadata(), simplifyVector = FALSE)

  # `baseload` is readonly, so only `extraload` is offered
  expect_equal(meta$databaseSymbols$schedule$modifiable, list(kind = "numeric", columns = list("extraload")))
  expect_true(meta$databaseSymbols$schedule$columns$baseload$readonly)
  expect_null(meta$databaseSymbols$schedule$columns$extraload$readonly)

  # a table whose only numeric column is readonly cannot be changed at all
  expect_null(meta$databaseSymbols$fixedcost$modifiable)

  DBI::dbDisconnect(env$conn)
})

test_that("modifySandboxInputData refuses to change a readonly column", {
  env <- setup_modify_test_env()

  expect_modify_error(env$adapter, list(
    symbolName = "schedule", targetColumn = "baseload", operation = "MULTIPLY", value = 2
  ), "is readonly and cannot be changed")
  # the hint points at the columns that can be changed
  expect_modify_error(env$adapter, list(
    symbolName = "schedule", targetColumn = "baseload", operation = "MULTIPLY", value = 2
  ), "Modifiable columns: extraload")
  expect_modify_error(env$adapter, list(
    symbolName = "fixedcost", targetColumn = "cost", operation = "MULTIPLY", value = 2
  ), "cannot be modified")

  DBI::dbDisconnect(env$conn)
})

test_that("a writable column of a partly readonly table can still be changed", {
  env <- setup_modify_test_env()
  res <- jsonlite::fromJSON(env$adapter$modifySandboxInputData(list(
    symbolName = "schedule", targetColumn = "extraload", operation = "ADD", value = 5
  )))

  expect_equal(res$status, "AWAITING_USER_CONFIRMATION")
  expect_equal(res$valuesAffected, 2L)

  # the readonly column is not touched by the queued change
  change <- env$adapter$getPendingInputChanges()[[1L]]
  liveData <- tibble::tibble(
    day = c("mon", "tue"), baseload = c(10, 20), extraload = c(1, 2)
  )
  applied <- applyAiInputChange(liveData, change)$data
  expect_equal(applied$baseload, c(10, 20))
  expect_equal(applied$extraload, c(6, 7))

  DBI::dbDisconnect(env$conn)
})

# --- where a dependent dropdown's choices come from ---------------------------

test_that("a dependent dropdown names the symbol and column its choices come from", {
  env <- setup_modify_test_env()
  meta <- jsonlite::fromJSON(env$adapter$getSystemMetadata(), simplifyVector = FALSE)

  # a statically configured dropdown simply lists its choices
  expect_equal(meta$databaseSymbols$modeltype$widget[["choices"]], list("lp", "mip", "minlp"))
  expect_null(meta$databaseSymbols$modeltype$widget[["choicesFrom"]])

  # a dependent one says where the values live, so the model can query them ...
  expect_equal(
    meta$databaseSymbols$basregion$widget[["choicesFrom"]],
    list(list(symbol = "demand", column = "region"))
  )
  # ... and gets them spelled out as well while there are few enough of them
  expect_equal(meta$databaseSymbols$basregion$widget[["choices"]], list("all", "NA", "EU"))

  DBI::dbDisconnect(env$conn)
})

test_that("a dependent dropdown with too many choices only points at its source", {
  env <- setup_modify_test_env()
  DBI::dbExecute(env$conn, "DELETE FROM data_demand")
  DBI::dbWriteTable(env$conn, "data_demand", tibble::tibble(
    `_sid` = 1L,
    region = sprintf("r%04d", seq_len(AI_MAX_LISTED_CHOICES + 1L)),
    year = "2024", value = 1
  ), append = TRUE)

  meta <- jsonlite::fromJSON(env$adapter$getSystemMetadata(), simplifyVector = FALSE)

  # `$` would partial-match `choicesFrom` here, so index exactly
  expect_null(meta$databaseSymbols$basregion$widget[["choices"]])
  expect_equal(
    meta$databaseSymbols$basregion$widget[["choicesFrom"]],
    list(list(symbol = "demand", column = "region"))
  )
  # the choices are still validated against the real values
  expect_equal(
    jsonlite::fromJSON(env$adapter$modifySandboxInputData(list(
      symbolName = "basregion", operation = "SET", textValue = "r0007"
    )))$newValue,
    "r0007"
  )

  DBI::dbDisconnect(env$conn)
})

# ==============================================================================
# Applying several proposals together ("Apply all")
# ==============================================================================

demandChange <- function(operation, value, region = NULL, filterColumnIdx = 1L, targetColumnIdx = 2L) {
  list(
    symbolName = "demand", isScalar = FALSE, operation = operation, value = value,
    targetColumnIdx = targetColumnIdx,
    filters = if (length(region)) list(list(columnIdx = filterColumnIdx, operator = "=", value = region)) else list()
  )
}
demandData <- function() {
  tibble::tibble(region = c("NA", "EU", "AS"), value = c(100, 150, 200), cost = c(1, 2, 3))
}

test_that("proposals are applied one after another, each on the previous result", {
  symbolData <- list(demand = demandData(), maxstock = 4)
  changes <- list(
    demandChange("MULTIPLY", 2, "EU"),
    list(symbolName = "maxstock", isScalar = TRUE, operation = "ADD", value = 1),
    demandChange("ADD", 5)
  )
  sequence <- applyAiInputChangeSequence(changes, function(symName) symbolData[[symName]])

  expect_equal(sequence$data$demand$value, c(105, 305, 205))
  expect_equal(sequence$data$maxstock, 5)
  # the third step started from the first step's result, not the original
  expect_equal(sequence$steps[[3L]]$before$value, c(100, 300, 200))
  expect_equal(sequence$steps[[3L]]$result$oldValues, c(100, 300, 200))
})

test_that("each symbol's data is read only once, even when it is NULL", {
  reads <- character(0L)
  getData <- function(symName) {
    reads <<- c(reads, symName)
    if (identical(symName, "maxstock")) NULL else demandData()
  }
  applyAiInputChangeSequence(list(
    demandChange("ADD", 1, "EU"), demandChange("ADD", 1, "NA"),
    list(symbolName = "maxstock", isScalar = TRUE, operation = "SET", value = 1),
    list(symbolName = "maxstock", isScalar = TRUE, operation = "ADD", value = 1)
  ), getData)
  expect_equal(reads, c("demand", "maxstock"))
})

test_that("edits of different rows of the same symbol are independent", {
  changes <- list(demandChange("MULTIPLY", 2, "EU"), demandChange("ADD", 5, "NA"))
  sequence <- applyAiInputChangeSequence(changes, function(symName) demandData())
  expect_true(aiInputChangesAreIndependent(changes, sequence))
})

test_that("edits of the same cells are not independent", {
  changes <- list(demandChange("MULTIPLY", 2, "EU"), demandChange("ADD", 5))
  sequence <- applyAiInputChangeSequence(changes, function(symName) demandData())
  expect_false(aiInputChangesAreIndependent(changes, sequence))
})

test_that("a filter on a column another edit changes makes them dependent", {
  # the second edit only touches rows whose cost is 2, which the first edit
  # changes: applied together it matches different rows than on its own
  changes <- list(
    demandChange("SET", 9, "EU", targetColumnIdx = 3L),
    list(
      symbolName = "demand", isScalar = FALSE, operation = "ADD", value = 1, targetColumnIdx = 2L,
      filters = list(list(columnIdx = 3L, operator = "=", value = "2"))
    )
  )
  sequence <- applyAiInputChangeSequence(changes, function(symName) demandData())
  expect_false(aiInputChangesAreIndependent(changes, sequence))
})

test_that("edits of different symbols are independent", {
  changes <- list(
    demandChange("ADD", 5),
    list(symbolName = "maxstock", isScalar = TRUE, operation = "ADD", value = 1)
  )
  symbolData <- list(demand = demandData(), maxstock = 4)
  sequence <- applyAiInputChangeSequence(changes, function(symName) symbolData[[symName]])
  expect_true(aiInputChangesAreIndependent(changes, sequence))
})

test_that("the widget bounds apply to the combined result", {
  sliderConfig <- list(type = "slider", slider = list(min = 0, max = 10))
  expect_null(aiInputChangeBoundsError(sliderConfig, "maxstock", c(3, 10)))
  expect_match(aiInputChangeBoundsError(sliderConfig, "maxstock", 12), "exceeds the maximum")
  expect_match(aiInputChangeBoundsError(sliderConfig, "maxstock", -1), "below the minimum")
  # no range configured, or nothing numeric to check
  expect_null(aiInputChangeBoundsError(list(type = "numericinput", numericinput = list()), "x", 1e9))
  expect_null(aiInputChangeBoundsError(sliderConfig, "maxstock", NA))
})

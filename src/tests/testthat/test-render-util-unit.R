library(dplyr)
library(tidyr)
library(tibble)
library(futile.logger)

source("../../components/render_util.R")
baselineScenPlaceholder <- "_baseline"

# ==============================================================================
# dashboardPrepareData: baseline comparison numerical tolerance
# ==============================================================================
# Regression coverage for the `numericalTolerance` global setting (threaded in
# as `zeroTolerance` here): a baseline value that is only nonzero due to
# floating-point noise should be snapped to exactly 0 before it is used as a
# divisor for "percentage difference"/"normalization" metrics, rather than
# producing a wildly inflated (but finite, and therefore misleading) result.

test_that("dashboardPrepareData snaps a near-zero baseline to 0 under the default tolerance", {
  viewData <- tibble(
    scenario = c("Base", "Target"),
    value = c(1e-10, 50)
  )
  config <- list(
    baselineComparison = list(
      domain = "scenario",
      record = "Base",
      metrics = list("percentage difference")
    )
  )

  result <- dashboardPrepareData(config, viewData)

  # Baseline (1e-10) is snapped to exactly 0 under the default 1e-6 tolerance,
  # so dividing by it produces Inf rather than a large-but-finite percentage.
  expect_true(all(is.infinite(result$data$value)))
})

test_that("dashboardPrepareData respects a custom, tighter zeroTolerance", {
  viewData <- tibble(
    scenario = c("Base", "Target"),
    value = c(1e-10, 50)
  )
  config <- list(
    baselineComparison = list(
      domain = "scenario",
      record = "Base",
      metrics = list("percentage difference")
    )
  )

  # With a tolerance tighter than 1e-10, the baseline is no longer snapped to
  # 0, so the division produces a finite (if inflated) result instead of Inf.
  result <- dashboardPrepareData(config, viewData, zeroTolerance = 1e-12)

  expect_true(all(is.finite(result$data$value)))
  expect_equal(result$data$value[1], 0) # Base vs. itself
  expect_gt(result$data$value[2], 1e10) # Target vs. a near-zero baseline: huge but finite
})

test_that("dashboardPrepareData leaves an ordinary (non-near-zero) baseline untouched", {
  viewData <- tibble(
    scenario = c("Base", "Target"),
    value = c(20, 25)
  )
  config <- list(
    baselineComparison = list(
      domain = "scenario",
      record = "Base",
      metrics = list("percentage difference")
    )
  )

  result <- dashboardPrepareData(config, viewData)

  # (25 - 20) / 20 * 100 = 25; (20 - 20) / 20 * 100 = 0 -- unaffected by the
  # zero-tolerance snap since the baseline (20) is nowhere near zero.
  expect_equal(sort(result$data$value), c(0, 25))
})

test_that("dashboardPrepareData resolves the baseline scenario placeholder", {
  viewData <- tibble(
    `_scenName` = c("Base", "Target"),
    value = c(10, 50)
  )
  configWithRecord <- function(record) {
    list(baselineComparison = list(
      domain = "_scenName",
      record = record,
      metrics = list("percentage difference")
    ))
  }

  expect_identical(
    dashboardPrepareData(configWithRecord("_baseline"), viewData, baselineScenName = "Base")$data,
    dashboardPrepareData(configWithRecord("Base"), viewData)$data
  )
  expect_identical(
    dashboardPrepareData(configWithRecord("_baseline"), viewData)$data,
    dashboardPrepareData(list(), viewData)$data
  )
})

test_that("dashboardPrepareData counts records missing in a scenario as 0 when comparing sums", {
  # Target lacks the seattle/chicago record of Base
  viewData <- tibble(
    `_scenName` = c("Base", "Base", "Base", "Target", "Target"),
    i = c("seattle", "san-diego", "seattle", "san-diego", "seattle"),
    j = c("chicago", "chicago", "topeka", "chicago", "topeka"),
    value = c(1.7, 1.8, 1.8, 1.8, 1.8)
  )
  config <- list(
    rows = "i", cols = list(`_scenName` = NULL), filter = list(j = "chicago"), aggregationFunction = "sum",
    baselineComparison = list(domain = "_scenName", record = "Base", metrics = list("absolute difference"))
  )
  expect_equal(dashboardPrepareData(config, viewData)$data$Target, c(0, -1.7))
  config$aggregationFunction <- "mean"
  expect_equal(dashboardPrepareData(config, viewData)$data$Target, c(0, NA))
})

test_that("dashboardPrepareData counts a scenario without any record as 0 when comparing sums", {
  viewData <- tibble(`_scenName` = c("Base", "Base"), i = c("seattle", "san-diego"), value = c(1.7, 1.8))
  config <- list(
    rows = "i", cols = list(`_scenName` = NULL), aggregationFunction = "sum",
    baselineComparison = list(domain = "_scenName", record = "Base", metrics = list("absolute difference"))
  )
  expect_equal(dashboardPrepareData(config, viewData, scenNames = c("Base", "Target"))$data$Target, c(-1.8, -1.7))
})

test_that("completeBaselineCompData only fills the scenario dimension and keeps factors", {
  data <- tibble(`_scenName` = factor(c("b", "B")), i = c("x", "y"), value = c(1, 2))
  completed <- completeBaselineCompData(data, "_scenName", c("b", "B", "c"), "sum")
  expect_identical(levels(completed$`_scenName`), c("b", "B", "c"))
  expect_equal(nrow(completed), 6L)
  expect_identical(completeBaselineCompData(data, "_scenName", NULL, "mean"), data)
  names(data)[1] <- "category"
  expect_identical(completeBaselineCompData(data, "category", NULL, "sum"), data)
})

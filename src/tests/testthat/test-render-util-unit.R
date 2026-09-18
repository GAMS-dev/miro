library(dplyr)
library(tidyr)
library(tibble)
library(futile.logger)

source("../../components/render_util.R")

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

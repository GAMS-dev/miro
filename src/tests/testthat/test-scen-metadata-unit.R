context("Unit tests - scenario metadata class")

source("../../components/util.R")
source("../../components/scen_metadata.R")

test_that("Initializing scenario metadata works", {
  expect_error(
    ScenarioMetadata$new(name = "asd", tags = "bla"),
    NA
  )
  expect_error(
    ScenarioMetadata$new(name = " "),
    class = "error_bad_name"
  )
  expect_error(
    ScenarioMetadata$new(name = "a", tags = rep.int("A", 1001)),
    NA
  )
  expect_error(
    ScenarioMetadata$new(name = "a", tags = as.character(rnorm(1001))),
    class = "error_bad_tags"
  )
})

test_that("Active bindings work", {
  metadata <- ScenarioMetadata$new(name = "asd", tags = c("bla", "blub"))
  expect_equal(metadata$name, "asd")
  expect_equal(metadata$tags, c("bla", "blub"))
  expect_error(metadata$name <- "test", NA)
  expect_equal(metadata$name, "test")
  expect_error(metadata$tags <- c("A", "B"), NA)
  expect_equal(metadata$tags, c("A", "B"))
  expect_error(metadata$name <- " ", class = "error_bad_name")
  expect_equal(metadata$name, "test")
  expect_error(metadata$owner <- "test", class = "error_locked")
})

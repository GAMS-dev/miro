context("util")

test_that("Compare MIRO versions works", {
  miroVersion <- MiroVersion$new("1.0.3")
  expect_identical(miroVersion$laterThan("1.0.4"), FALSE)
  expect_identical(miroVersion$laterThan("0.1.4"), TRUE)
  expect_identical(miroVersion$laterThan("2.0.4"), FALSE)
})

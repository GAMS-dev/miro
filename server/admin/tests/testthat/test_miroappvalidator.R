context("MiroAppValidator")

test_that("MiroAppValidator class works", {
  miroAppValidator <- MiroAppValidator$new()$validate(file.path("transport.miroapp"))
  expect_identical(miroAppValidator$getMIROVersion(), "1.1.1")
  expect_identical(miroAppValidator$getModelName(), "transport")
  expect_identical(miroAppValidator$getModesSupported(), c("base"))
  expect_error(MiroAppValidator$new()$validate(file.path("transport_local.miroapp")))
})

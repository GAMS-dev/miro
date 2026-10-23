source("../../modules/load_dynamic_tab_content.R", local = TRUE)

modelOut <- list(a = NULL, b = NULL, c = NULL)
modelIn <- list(x = NULL, y = NULL)
outputTabs <- list(1L, c(2L, 3L))
scenInputTabs <- list(c(0L, 2L))
isGroupOfSheets <- c(FALSE, TRUE, TRUE)
config <- list(hiddenCompareSymbols = "x")

test_that("getCompareTabValues finds the comparison mode tab of a sheet", {
  expect_identical(getCompareTabValues("a"), "contentScen_0_1")
  expect_identical(getCompareTabValues("c"), c("contentScen_0_2", "contentScen_0_2_2"))
  expect_identical(getCompareTabValues(scalarsFileName), c("contentScen_0_3", "contentScen_0_3_1"))
  expect_identical(getCompareTabValues("y", tabsetId = 4L), c("contentScen_4_3", "contentScen_4_3_2"))
  expect_null(getCompareTabValues("x"))
  expect_null(getCompareTabValues("unknown"))
})

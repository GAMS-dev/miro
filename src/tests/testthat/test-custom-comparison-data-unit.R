library(R6)
library(testthat)

source("../../components/custom_comparison_data.R")

MockAttachments <- R6Class("MockAttachments", public = list(
  includeSandboxScen = NULL,
  getData = function(scenIds, fileNames, includeContent, includeSandboxScen) {
    self$includeSandboxScen <- includeSandboxScen
    return(scenIds)
  }
))
mockScenData <- function(refScenMap) {
  return(list(
    getRefScenMap = function(refId) refScenMap,
    getById = function(type, refId) list(list(`_sid` = 1L), list(`_sid` = 2L))
  ))
}

test_that("getAttachmentData includes sandbox attachments only if the sandbox is compared", {
  attachments <- MockAttachments$new()
  data <- CustomComparisonData$new(mockScenData(c("1", "2")), "cmp", attachments)
  expect_identical(data$getAttachmentData(), c(1L, 2L))
  expect_false(attachments$includeSandboxScen)
  data$getAttachmentData(includeSandboxScen = TRUE)
  expect_true(attachments$includeSandboxScen)

  data <- CustomComparisonData$new(mockScenData(c("sb_cmp", "1")), "cmp", attachments)
  data$getAttachmentData()
  expect_true(attachments$includeSandboxScen)
})

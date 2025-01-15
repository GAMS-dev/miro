context("Integration tests - MIRO pivot renderer")
library(dplyr)
library(shiny)
library(DT)
library(tidyr)
library(futile.logger)
library(chartjs)
library(jsonlite)

source("../../components/scenario_extensions.R")
source("../../components/views.R")
source("../../modules/renderers/miro-pivot.R")

lang <<- list()

testData <- tibble(
  a = rep.int(paste0("a", seq_len(5)), 2L), b = paste0("b", seq_len(10)),
  c = paste0("c", seq_len(10)), d = paste0("d", seq_len(10)),
  e = rep.int(c("e2", "e10"), 5L), f = "f10",
  value = 1:10
)
testDataFactor <- testData %>% mutate(across(where(is.character), as.factor))

convert_to_df <- function(df) {
  data.frame(df %>% ungroup() %>% mutate(across(where(is.factor), as.character)))
}

test_that("MIRO pivot renderer handles filtering", {
  testServer(renderMiroPivot,
    {
      session$setInputs(
        rowIndexList = letters[1:6], colIndexList = character(),
        filterIndexList = character(), aggregationFunction = "sum"
      )
      expect_null(filteredData())
      session$setInputs(aggregationIndexList = character())
      # fully initialized
      expect_identical(filteredData(), list(
        data = testDataFactor,
        filterElements = list()
      ))
      session$setInputs(
        rowIndexList = letters[2:6],
        filterIndexList = "a"
      )
      expect_identical(
        filteredData(),
        list(
          data = testDataFactor[c(1, 6), ],
          filterElements = list(a = factor(c("a1", "a2", "a3", "a4", "a5"))),
          multiFilterIndices = NULL,
          singleFilterIndices = NULL
        )
      )
      session$setInputs(rowIndexList = letters[3:6], filterIndexList = c("a", "b"))
      expect_identical(
        filteredData(),
        list(
          data = testDataFactor[1, ],
          filterElements = list(
            a = factor(c("a1", "a2", "a3", "a4", "a5")),
            b = factor(c("b1", "b6"),
              levels = c(
                "b1", "b10", "b2", "b3", "b4", "b5",
                "b6", "b7", "b8", "b9"
              )
            )
          ),
          multiFilterIndices = NULL,
          singleFilterIndices = NULL
        )
      )
      session$setInputs(
        filterIndexList = c("a"),
        aggregationIndexList = c("b")
      )
      expect_identical(
        filteredData(),
        list(
          data = testDataFactor[c(1, 6), ],
          filterElements = list(
            a = factor(c("a1", "a2", "a3", "a4", "a5")),
            b = factor(c("b1", "b6"),
              levels = c(
                "b1", "b10", "b2", "b3", "b4", "b5",
                "b6", "b7", "b8", "b9"
              )
            )
          ),
          multiFilterIndices = NULL,
          singleFilterIndices = NULL
        )
      )
      expect_equal(
        convert_to_df(dataToRender()),
        data.frame(
          c = paste0("c", c("1", "6")),
          d = paste0("d", c("1", "6")),
          e = paste0("e", c("2", "10")),
          f = "f10",
          value = c(1L, 6L)
        )
      )
      session$setInputs(
        filter_b = paste0("b", 1:10),
        filterIndexList = character(), rowIndexList = letters[1:6][-2]
      )
      expect_identical(convert_to_df(dataToRender()), data.frame(
        a = c("a1", "a1", "a2", "a2", "a3", "a3", "a4", "a4", "a5", "a5"),
        c = paste0("c", c("1", "6", "2", "7", "3", "8", "4", "9", "10", "5")),
        d = paste0("d", c("1", "6", "2", "7", "3", "8", "4", "9", "10", "5")),
        e = paste0("e", c("2", "10", "10", "2", "2", "10", "10", "2", "10", "2")),
        f = "f10",
        "value" = c(1L, 6L, 2L, 7L, 3L, 8L, 4L, 9L, 10L, 5L)
      ))
    },
    args = list(
      data = testData,
      options = list(
        enablePersistentViews = FALSE,
        "_metadata_" = list(symtype = "parameter")
      )
    )
  )
})

test_that("MIRO pivot renderer handles pivoting", {
  testServer(renderMiroPivot,
    {
      session$setInputs(
        rowIndexList = letters[1:5], colIndexList = "f",
        filterIndexList = character(), aggregationIndexList = character()
      )
      expect_identical(convert_to_df(dataToRender()), data.frame(
        a = c("a1", "a1", "a2", "a2", "a3", "a3", "a4", "a4", "a5", "a5"),
        b = paste0("b", c("1", "6", "2", "7", "3", "8", "4", "9", "10", "5")),
        c = paste0("c", c("1", "6", "2", "7", "3", "8", "4", "9", "10", "5")),
        d = paste0("d", c("1", "6", "2", "7", "3", "8", "4", "9", "10", "5")),
        e = paste0("e", c("2", "10", "10", "2", "2", "10", "10", "2", "10", "2")),
        "f10" = c(1L, 6L, 2L, 7L, 3L, 8L, 4L, 9L, 10L, 5L)
      ))
      session$setInputs(rowIndexList = letters[2:6], colIndexList = "a")
      expect_equal(convert_to_df(dataToRender()), data.frame(
        b = paste0("b", c("1", "10", "2", "3", "4", "5", "6", "7", "8", "9")),
        c = paste0("c", c("1", "10", "2", "3", "4", "5", "6", "7", "8", "9")),
        d = paste0("d", c("1", "10", "2", "3", "4", "5", "6", "7", "8", "9")),
        e = paste0("e", c("2", "10", "10", "2", "10", "2", "10", "2", "10", "2")),
        f = "f10",
        "a1" = c(1L, NA, NA, NA, NA, NA, 6L, NA, NA, NA),
        "a2" = c(NA, NA, 2L, NA, NA, NA, NA, 7L, NA, NA),
        "a3" = c(NA, NA, NA, 3L, NA, NA, NA, NA, 8L, NA),
        "a4" = c(NA, NA, NA, NA, 4L, NA, NA, NA, NA, 9L),
        "a5" = c(NA, 10L, NA, NA, NA, 5L, NA, NA, NA, NA)
      ))
      session$setInputs(rowIndexList = letters[2:5], colIndexList = c("a", "f"))
      expect_equal(convert_to_df(dataToRender()), data.frame(
        b = paste0("b", c("1", "10", "2", "3", "4", "5", "6", "7", "8", "9")),
        c = paste0("c", c("1", "10", "2", "3", "4", "5", "6", "7", "8", "9")),
        d = paste0("d", c("1", "10", "2", "3", "4", "5", "6", "7", "8", "9")),
        e = paste0("e", c("2", "10", "10", "2", "10", "2", "10", "2", "10", "2")),
        "a1․f10" = c(1L, NA, NA, NA, NA, NA, 6L, NA, NA, NA),
        "a2․f10" = c(NA, NA, 2L, NA, NA, NA, NA, 7L, NA, NA),
        "a3․f10" = c(NA, NA, NA, 3L, NA, NA, NA, NA, 8L, NA),
        "a4․f10" = c(NA, NA, NA, NA, 4L, NA, NA, NA, NA, 9L),
        "a5․f10" = c(NA, 10L, NA, NA, NA, 5L, NA, NA, NA, NA)
      ))
      session$setInputs(rowIndexList = letters[2:5], colIndexList = c("f", "a"))
      expect_equal(convert_to_df(dataToRender()), data.frame(
        b = paste0("b", c("1", "10", "2", "3", "4", "5", "6", "7", "8", "9")),
        c = paste0("c", c("1", "10", "2", "3", "4", "5", "6", "7", "8", "9")),
        d = paste0("d", c("1", "10", "2", "3", "4", "5", "6", "7", "8", "9")),
        e = paste0("e", c("2", "10", "10", "2", "10", "2", "10", "2", "10", "2")),
        "f10․a1" = c(1L, NA, NA, NA, NA, NA, 6L, NA, NA, NA),
        "f10․a2" = c(NA, NA, 2L, NA, NA, NA, NA, 7L, NA, NA),
        "f10․a3" = c(NA, NA, NA, 3L, NA, NA, NA, NA, 8L, NA),
        "f10․a4" = c(NA, NA, NA, NA, 4L, NA, NA, NA, NA, 9L),
        "f10․a5" = c(NA, 10L, NA, NA, NA, 5L, NA, NA, NA, NA)
      ))
      session$setInputs(rowIndexList = letters[c(2, 3, 4, 6)], colIndexList = c("e", "a"))
      expect_equal(convert_to_df(dataToRender()), data.frame(
        b = paste0("b", c("1", "10", "2", "3", "4", "5", "6", "7", "8", "9")),
        c = paste0("c", c("1", "10", "2", "3", "4", "5", "6", "7", "8", "9")),
        d = paste0("d", c("1", "10", "2", "3", "4", "5", "6", "7", "8", "9")),
        f = "f10",
        "e10․a1" = c(NA, NA, NA, NA, NA, NA, 6L, NA, NA, NA),
        "e10․a2" = c(NA, NA, 2L, NA, NA, NA, NA, NA, NA, NA),
        "e10․a3" = c(NA, NA, NA, NA, NA, NA, NA, NA, 8L, NA),
        "e10․a4" = c(NA, NA, NA, NA, 4L, NA, NA, NA, NA, NA),
        "e10․a5" = c(NA, 10L, NA, NA, NA, NA, NA, NA, NA, NA),
        "e2․a1" = c(1L, NA, NA, NA, NA, NA, NA, NA, NA, NA),
        "e2․a2" = c(NA, NA, NA, NA, NA, NA, NA, 7L, NA, NA),
        "e2․a3" = c(NA, NA, NA, 3L, NA, NA, NA, NA, NA, NA),
        "e2․a4" = c(NA, NA, NA, NA, NA, NA, NA, NA, NA, 9L),
        "e2․a5" = c(NA, NA, NA, NA, NA, 5L, NA, NA, NA, NA)
      ))
    },
    args = list(
      data = testData,
      options = list(enablePersistentViews = FALSE)
    )
  )
})
test_that("MIRO pivot renderer handles aggregation", {
  testServer(renderMiroPivot,
    {
      session$setInputs(
        rowIndexList = character(), colIndexList = "a",
        filterIndexList = character(), aggregationIndexList = letters[2:6],
        aggregationFunction = "sum"
      )
      expect_identical(convert_to_df(dataToRender()), data.frame(
        a1 = 7L,
        a2 = 9L,
        a3 = 11L,
        a4 = 13L,
        a5 = 15L
      ))
      session$setInputs(aggregationFunction = "count")
      expect_identical(convert_to_df(dataToRender()), data.frame(
        a1 = 2L,
        a2 = 2L,
        a3 = 2L,
        a4 = 2L,
        a5 = 2L
      ))
      session$setInputs(aggregationFunction = "min")
      expect_identical(convert_to_df(dataToRender()), data.frame(
        a1 = 1L,
        a2 = 2L,
        a3 = 3L,
        a4 = 4L,
        a5 = 5L
      ))
      session$setInputs(aggregationFunction = "max")
      expect_identical(convert_to_df(dataToRender()), data.frame(
        a1 = 6L,
        a2 = 7L,
        a3 = 8L,
        a4 = 9L,
        a5 = 10L
      ))
      session$setInputs(aggregationFunction = "mean")
      expect_identical(convert_to_df(dataToRender()), data.frame(
        a1 = 3.5,
        a2 = 4.5,
        a3 = 5.5,
        a4 = 6.5,
        a5 = 7.5
      ))
      session$setInputs(
        colIndexList = "e",
        filter_a = paste0("a", c("1", "2", "3")),
        aggregationIndexList = letters[1:5],
        aggregationFunction = "median"
      )
      expect_identical(convert_to_df(dataToRender()), data.frame(
        e10 = 6L,
        e2 = 3L
      ))
    },
    args = list(
      data = testData,
      options = list(
        enablePersistentViews = FALSE,
        "_metadata_" = list(symtype = "parameter")
      )
    )
  )
})
test_that("MIRO pivot renderer handles sorting (part1)", {
  testServer(renderMiroPivot,
    {
      session$setInputs(
        rowIndexList = letters[c(6, 5, 3, 2, 4)], colIndexList = "a",
        filterIndexList = character(), aggregationIndexList = character(),
        aggregationFunction = "sum"
      )
      expect_identical(
        convert_to_df(dataToRender()),
        data.frame(
          f = "f10",
          e = paste0("e", c(10, 10, 10, 10, 10, 2, 2, 2, 2, 2)),
          c = paste0("c", c(10, 2, 4, 6, 8, 1, 3, 5, 7, 9)),
          b = paste0("b", c(10, 2, 4, 6, 8, 1, 3, 5, 7, 9)),
          d = paste0("d", c(10, 2, 4, 6, 8, 1, 3, 5, 7, 9)),
          a1 = c(NA_integer_, NA_integer_, NA_integer_, 6L, NA_integer_, 1L, NA_integer_, NA_integer_, NA_integer_, NA_integer_),
          a2 = c(NA_integer_, 2L, NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_, 7L, NA_integer_),
          a3 = c(NA_integer_, NA_integer_, NA_integer_, NA_integer_, 8L, NA_integer_, 3L, NA_integer_, NA_integer_, NA_integer_),
          a4 = c(NA_integer_, NA_integer_, 4L, NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_, 9L),
          a5 = c(10L, NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_, 5L, NA_integer_, NA_integer_)
        )
      )
    },
    args = list(
      data = testData,
      options = list(
        enablePersistentViews = FALSE,
        "_metadata_" = list(symtype = "parameter")
      )
    )
  )
})
test_that("MIRO pivot renderer handles sorting (part2)", {
  testServer(renderMiroPivot,
    {
      session$setInputs(
        rowIndexList = c("c", "d"), colIndexList = c("a", "e"),
        filterIndexList = "b", filter_b = c("b11", "b5"),
        aggregationIndexList = "f", aggregationFunction = "sum"
      )
      expect_identical(
        convert_to_df(dataToRender()),
        data.frame(
          c = c("c1", "c1", "c10", "c10"),
          d = paste0("d", c(10, 4, 5, 9)),
          b = c("b5", "b11", "b5", "b11"),
          `a4.e10` = c(NA_integer_, 4L, NA_integer_, NA_integer_),
          `a4.e2` = c(NA_integer_, NA_integer_, NA_integer_, 9L),
          `a5.e10` = c(10L, NA_integer_, NA_integer_, NA_integer_),
          `a5.e2` = c(NA_integer_, NA_integer_, 5L, NA_integer_)
        )
      )
    },
    args = list(
      data = tibble(
        a = rep.int(paste0("a", seq_len(5)), 2L), b = rep.int(c("b2", "b10", "b4", "b11", "b5"), 2L),
        c = rep.int(c("c10", "c1"), 5L), d = paste0("d", seq_len(10)),
        e = rep.int(c("e2", "e10"), 5L), f = "f10",
        value = 1:10
      ),
      options = list(
        enablePersistentViews = FALSE,
        "_metadata_" = list(symtype = "parameter")
      )
    )
  )
})
MockView <- R6Class("MockView", inherit = Views, private = list(
  getSymbolName = function(session) {
    return("test1")
  }
))
views <- MockView$new(
  "test1",
  character(),
  "test1"
)
views$addConf(list(test1 = list(view1 = list(
  rows = letters[c(6, 5, 3, 2, 4)], cols = "a",
  filters = character(), aggregations = character(),
  aggregationFunction = "sum"
))))
test_that("MIRO pivot renderer views work", {
  testServer(renderMiroPivot,
    {
      session$setInputs(
        rowIndexList = letters[1:6], colIndexList = character(),
        filterIndexList = character(), aggregationIndexList = character(),
        aggregationFunction = "sum"
      )
      expect_identical(
        convert_to_df(dataToRender()),
        data.frame(
          a = c("a1", "a1", "a2", "a2", "a3", "a3", "a4", "a4", "a5", "a5"),
          b = c("b2", "b2", "b10", "b10", "b4", "b4", "b11", "b11", "b5", "b5"),
          c = c("c1", "c10", "c1", "c10", "c1", "c10", "c1", "c10", "c1", "c10"),
          d = c("d6", "d1", "d2", "d7", "d8", "d3", "d4", "d9", "d10", "d5"),
          e = c("e10", "e2", "e10", "e2", "e10", "e2", "e10", "e2", "e10", "e2"),
          f = "f10",
          value = c(6L, 1L, 2L, 7L, 8L, 3L, 4L, 9L, 10L, 5L)
        )
      )
      session$setInputs(saveViewConfirm = 1L, newViewName = "view1")
      expect_identical(views$get(session, "view1"), list(
        rows = letters[c(6, 5, 3, 2, 4)], cols = "a",
        filters = character(), aggregations = character(),
        aggregationFunction = "sum"
      ))
      session$setInputs(
        rowIndexList = character(), colIndexList = "a", filter_a = c("a1", "a5"),
        filter_c = "c2",
        filterIndexList = character(), aggregationIndexList = letters[2:6],
        aggregationFunction = "count", pivotRenderer = "bar"
      )
      session$setInputs(saveViewConfirm = 1L, newViewName = "view2")
      expect_identical(views$get(session, "view2"), list(
        aggregationFunction = "count",
        pivotRenderer = "bar",
        domainFilter = list(default = NULL),
        hideEmptyCols = FALSE,
        fixedColumns = TRUE,
        tableSummarySettings = list(
          rowEnabled = FALSE, rowSummaryFunction = "sum",
          colEnabled = FALSE, colSummaryFunction = "sum"
        ),
        aggregations = list(
          b = NULL, c = "c2", d = NULL,
          e = NULL, f = NULL
        ),
        cols = list(a = c("a1", "a5"))
      ))
      session$setInputs(deleteView = htmlIdEnc("view1"))
      expect_error(views$get(session, "view1"))
      expect_output(session$setInputs(savedViews = htmlIdEnc("view1")))
    },
    args = list(
      data = tibble(
        a = rep.int(paste0("a", seq_len(5)), 2L), b = rep.int(c("b2", "b10", "b4", "b11", "b5"), 2L),
        c = rep.int(c("c10", "c1"), 5L), d = paste0("d", seq_len(10)),
        e = rep.int(c("e2", "e10"), 5L), f = "f10",
        value = 1:10
      ),
      views = views,
      options = list(
        enablePersistentViews = TRUE,
        "_metadata_" = list(symtype = "parameter")
      )
    )
  )
})

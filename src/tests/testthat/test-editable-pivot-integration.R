context("Integration tests - Editable MIRO pivot renderer")
library(dplyr)
library(shiny)
library(DT)
library(tidyr)
library(futile.logger)
library(chartjs)
library(jsonlite)

source("../../components/views.R")
source("../../modules/renderers/miro-pivot.R")


testData <- tibble(
  a = rep.int(paste0("a", seq_len(5)), 2L), b = paste0("b", seq_len(10)),
  c = paste0("c", seq_len(10)), d = paste0("d", seq_len(10)),
  e = rep.int(c("e2", "e10"), 5L), f = "f10",
  value = 1:10
)

convert_to_df <- function(df) {
  data.frame(df %>% ungroup() %>% mutate(across(where(is.factor), as.character)))
}
lang <<- list()

test_that("Editing in list view works", {
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
          b = c("b1", "b6", "b2", "b7", "b3", "b8", "b4", "b9", "b10", "b5"),
          c = c("c1", "c6", "c2", "c7", "c3", "c8", "c4", "c9", "c10", "c5"),
          d = c("d1", "d6", "d2", "d7", "d3", "d8", "d4", "d9", "d10", "d5"),
          e = c("e2", "e10", "e10", "e2", "e2", "e10", "e10", "e2", "e10", "e2"),
          f = "f10",
          value = c(1L, 6L, 2L, 7L, 3L, 8L, 4L, 9L, 10L, 5L)
        )
      )
      session$setInputs(pivotTable_cell_edit = list(col = 6, row = 1L, value = 2L))
      session$setInputs(pivotTable_cell_edit = list(col = 5, row = 1L, value = "f3"))
      expect_identical(
        convert_to_df(dataToRender()),
        data.frame(
          a = c("a1", "a1", "a2", "a2", "a3", "a3", "a4", "a4", "a5", "a5"),
          b = c("b1", "b6", "b2", "b7", "b3", "b8", "b4", "b9", "b10", "b5"),
          c = c("c1", "c6", "c2", "c7", "c3", "c8", "c4", "c9", "c10", "c5"),
          d = c("d1", "d6", "d2", "d7", "d3", "d8", "d4", "d9", "d10", "d5"),
          e = c("e2", "e10", "e10", "e2", "e2", "e10", "e10", "e2", "e10", "e2"),
          f = c("f3", "f10", "f10", "f10", "f10", "f10", "f10", "f10", "f10", "f10"),
          value = c(2L, 6L, 2L, 7L, 3L, 8L, 4L, 9L, 10L, 5L)
        )
      )
      session$setInputs(pivotTable_cell_edit = list(col = 0, row = 1L, value = "a2"))
      expect_identical(
        convert_to_df(dataToRender()),
        data.frame(
          a = c("a1", "a2", "a2", "a2", "a3", "a3", "a4", "a4", "a5", "a5"),
          b = c("b6", "b1", "b2", "b7", "b3", "b8", "b4", "b9", "b10", "b5"),
          c = c("c6", "c1", "c2", "c7", "c3", "c8", "c4", "c9", "c10", "c5"),
          d = c("d6", "d1", "d2", "d7", "d3", "d8", "d4", "d9", "d10", "d5"),
          e = c("e10", "e2", "e10", "e2", "e2", "e10", "e10", "e2", "e10", "e2"),
          f = c("f10", "f3", "f10", "f10", "f10", "f10", "f10", "f10", "f10", "f10"),
          value = c(6L, 2L, 2L, 7L, 3L, 8L, 4L, 9L, 10L, 5L)
        )
      )
      # invalid value should result in identical data
      session$setInputs(pivotTable_cell_edit = list(col = 6L, row = 1L, value = "a2"))
      expect_identical(
        convert_to_df(dataToRender()),
        data.frame(
          a = c("a1", "a2", "a2", "a2", "a3", "a3", "a4", "a4", "a5", "a5"),
          b = c("b6", "b1", "b2", "b7", "b3", "b8", "b4", "b9", "b10", "b5"),
          c = c("c6", "c1", "c2", "c7", "c3", "c8", "c4", "c9", "c10", "c5"),
          d = c("d6", "d1", "d2", "d7", "d3", "d8", "d4", "d9", "d10", "d5"),
          e = c("e10", "e2", "e10", "e2", "e2", "e10", "e10", "e2", "e10", "e2"),
          f = c("f10", "f3", "f10", "f10", "f10", "f10", "f10", "f10", "f10", "f10"),
          value = c(6L, 2L, 2L, 7L, 3L, 8L, 4L, 9L, 10L, 5L)
        )
      )
      # clicking the add row button should not throw an error
      session$setInputs(btAddRow = 1L)
      session$setInputs(
        newRow_1 = "a4", newRow_2 = "b9", newRow_3 = "c9", newRow_4 = "d9",
        newRow_5 = "e2", newRow_6 = "f11", newRow_7 = 1.5, btAddRowConfirm = 1L
      )
      expect_identical(
        convert_to_df(dataToRender()),
        data.frame(
          a = c("a1", "a2", "a2", "a2", "a3", "a3", "a4", "a4", "a4", "a5", "a5"),
          b = c("b6", "b1", "b2", "b7", "b3", "b8", "b4", "b9", "b9", "b10", "b5"),
          c = c("c6", "c1", "c2", "c7", "c3", "c8", "c4", "c9", "c9", "c10", "c5"),
          d = c("d6", "d1", "d2", "d7", "d3", "d8", "d4", "d9", "d9", "d10", "d5"),
          e = c("e10", "e2", "e10", "e2", "e2", "e10", "e10", "e2", "e2", "e10", "e2"),
          f = c("f10", "f3", "f10", "f10", "f10", "f10", "f10", "f10", "f11", "f10", "f10"),
          value = c(6L, 2L, 2L, 7L, 3L, 8L, 4L, 9L, 1.5, 10L, 5L)
        )
      )
      # no rows selected
      session$setInputs(btRemoveRows = 1L)
      expect_identical(
        convert_to_df(dataToRender()),
        data.frame(
          a = c("a1", "a2", "a2", "a2", "a3", "a3", "a4", "a4", "a4", "a5", "a5"),
          b = c("b6", "b1", "b2", "b7", "b3", "b8", "b4", "b9", "b9", "b10", "b5"),
          c = c("c6", "c1", "c2", "c7", "c3", "c8", "c4", "c9", "c9", "c10", "c5"),
          d = c("d6", "d1", "d2", "d7", "d3", "d8", "d4", "d9", "d9", "d10", "d5"),
          e = c("e10", "e2", "e10", "e2", "e2", "e10", "e10", "e2", "e2", "e10", "e2"),
          f = c("f10", "f3", "f10", "f10", "f10", "f10", "f10", "f10", "f11", "f10", "f10"),
          value = c(6L, 2L, 2L, 7L, 3L, 8L, 4L, 9L, 1.5, 10L, 5L)
        )
      )
      session$setInputs(pivotTable_rows_selected = c(2, 11), btRemoveRows = 2L)
      expect_identical(
        convert_to_df(dataToRender()),
        data.frame(
          a = c("a1", "a2", "a2", "a3", "a3", "a4", "a4", "a4", "a5"),
          b = c("b6", "b2", "b7", "b3", "b8", "b4", "b9", "b9", "b10"),
          c = c("c6", "c2", "c7", "c3", "c8", "c4", "c9", "c9", "c10"),
          d = c("d6", "d2", "d7", "d3", "d8", "d4", "d9", "d9", "d10"),
          e = c("e10", "e10", "e2", "e2", "e10", "e10", "e2", "e2", "e10"),
          f = c("f10", "f10", "f10", "f10", "f10", "f10", "f10", "f11", "f10"),
          value = c(6L, 2L, 7L, 3L, 8L, 4L, 9L, 1.5, 10L)
        )
      )
      expect_identical(
        convert_to_df(session$returned()),
        data.frame(
          a = c("a2", "a3", "a4", "a1", "a2", "a3", "a4", "a5", "a4"),
          b = c("b2", "b3", "b4", "b6", "b7", "b8", "b9", "b10", "b9"),
          c = c("c2", "c3", "c4", "c6", "c7", "c8", "c9", "c10", "c9"),
          d = c("d2", "d3", "d4", "d6", "d7", "d8", "d9", "d10", "d9"),
          e = c("e10", "e2", "e10", "e10", "e2", "e10", "e2", "e10", "e2"),
          f = c("f10", "f10", "f10", "f10", "f10", "f10", "f10", "f10", "f11"),
          value = c(2, 3, 4, 6, 7, 8, 9, 10, 1.5)
        )
      )
    },
    args = list(
      data = testData,
      options = list(
        enablePersistentViews = FALSE, `_input_` = TRUE,
        "_metadata_" = list(symtype = "parameter")
      )
    )
  )
})

test_that("Editing with reordered columns works", {
  testServer(renderMiroPivot,
    {
      session$setInputs(
        rowIndexList = c(letters[5:6], letters[1:4]), colIndexList = character(),
        filterIndexList = character(), aggregationIndexList = character(),
        aggregationFunction = "sum"
      )
      expect_identical(
        convert_to_df(dataToRender()),
        data.frame(
          e = c("e10", "e10", "e10", "e10", "e10", "e2", "e2", "e2", "e2", "e2"),
          f = c("f10", "f10", "f10", "f10", "f10", "f10", "f10", "f10", "f10", "f10"),
          a = c("a1", "a2", "a3", "a4", "a5", "a1", "a2", "a3", "a4", "a5"),
          b = c("b6", "b2", "b8", "b4", "b10", "b1", "b7", "b3", "b9", "b5"),
          c = c("c6", "c2", "c8", "c4", "c10", "c1", "c7", "c3", "c9", "c5"),
          d = c("d6", "d2", "d8", "d4", "d10", "d1", "d7", "d3", "d9", "d5"),
          value = c(6L, 2L, 8L, 4L, 10L, 1L, 7L, 3L, 9L, 5L)
        )
      )
      session$setInputs(pivotTable_cell_edit = list(col = 6, row = 1L, value = 2L))
      session$setInputs(pivotTable_cell_edit = list(col = 5, row = 1L, value = "d3"))
      expect_identical(
        convert_to_df(dataToRender()),
        data.frame(
          e = c("e10", "e10", "e10", "e10", "e10", "e2", "e2", "e2", "e2", "e2"),
          f = c("f10", "f10", "f10", "f10", "f10", "f10", "f10", "f10", "f10", "f10"),
          a = c("a1", "a2", "a3", "a4", "a5", "a1", "a2", "a3", "a4", "a5"),
          b = c("b6", "b2", "b8", "b4", "b10", "b1", "b7", "b3", "b9", "b5"),
          c = c("c6", "c2", "c8", "c4", "c10", "c1", "c7", "c3", "c9", "c5"),
          d = c("d3", "d2", "d8", "d4", "d10", "d1", "d7", "d3", "d9", "d5"),
          value = c(2L, 2L, 8L, 4L, 10L, 1L, 7L, 3L, 9L, 5L)
        )
      )
      session$setInputs(pivotTable_cell_edit = list(col = 0, row = 1L, value = "e2"))
      expect_identical(
        convert_to_df(dataToRender()),
        data.frame(
          e = c("e10", "e10", "e10", "e10", "e2", "e2", "e2", "e2", "e2", "e2"),
          f = c("f10", "f10", "f10", "f10", "f10", "f10", "f10", "f10", "f10", "f10"),
          a = c("a2", "a3", "a4", "a5", "a1", "a1", "a2", "a3", "a4", "a5"),
          b = c("b2", "b8", "b4", "b10", "b1", "b6", "b7", "b3", "b9", "b5"),
          c = c("c2", "c8", "c4", "c10", "c1", "c6", "c7", "c3", "c9", "c5"),
          d = c("d2", "d8", "d4", "d10", "d1", "d3", "d7", "d3", "d9", "d5"),
          value = c(2L, 8L, 4L, 10L, 1L, 2L, 7L, 3L, 9L, 5L)
        )
      )
      # invalid value should result in identical data
      session$setInputs(pivotTable_cell_edit = list(col = 6L, row = 1L, value = "a2"))
      expect_identical(
        convert_to_df(dataToRender()),
        data.frame(
          e = c("e10", "e10", "e10", "e10", "e2", "e2", "e2", "e2", "e2", "e2"),
          f = c("f10", "f10", "f10", "f10", "f10", "f10", "f10", "f10", "f10", "f10"),
          a = c("a2", "a3", "a4", "a5", "a1", "a1", "a2", "a3", "a4", "a5"),
          b = c("b2", "b8", "b4", "b10", "b1", "b6", "b7", "b3", "b9", "b5"),
          c = c("c2", "c8", "c4", "c10", "c1", "c6", "c7", "c3", "c9", "c5"),
          d = c("d2", "d8", "d4", "d10", "d1", "d3", "d7", "d3", "d9", "d5"),
          value = c(2L, 8L, 4L, 10L, 1L, 2L, 7L, 3L, 9L, 5L)
        )
      )
      # clicking the add row button should not throw an error
      session$setInputs(btAddRow = 1L)
      session$setInputs(
        newRow_1 = "e3", newRow_2 = "f9", newRow_3 = "a1", newRow_4 = "b2",
        newRow_5 = "c3", newRow_6 = "d4", newRow_7 = 1.5, btAddRowConfirm = 1L
      )
      expect_identical(
        convert_to_df(dataToRender()),
        data.frame(
          e = c("e10", "e10", "e10", "e10", "e2", "e2", "e2", "e2", "e2", "e2", "e3"),
          f = c("f10", "f10", "f10", "f10", "f10", "f10", "f10", "f10", "f10", "f10", "f9"),
          a = c("a2", "a3", "a4", "a5", "a1", "a1", "a2", "a3", "a4", "a5", "a1"),
          b = c("b2", "b8", "b4", "b10", "b1", "b6", "b7", "b3", "b9", "b5", "b2"),
          c = c("c2", "c8", "c4", "c10", "c1", "c6", "c7", "c3", "c9", "c5", "c3"),
          d = c("d2", "d8", "d4", "d10", "d1", "d3", "d7", "d3", "d9", "d5", "d4"),
          value = c(2L, 8L, 4L, 10L, 1L, 2L, 7L, 3L, 9L, 5L, 1.5)
        )
      )
      # no rows selected
      session$setInputs(btRemoveRows = 1L)
      expect_identical(
        convert_to_df(dataToRender()),
        data.frame(
          e = c("e10", "e10", "e10", "e10", "e2", "e2", "e2", "e2", "e2", "e2", "e3"),
          f = c("f10", "f10", "f10", "f10", "f10", "f10", "f10", "f10", "f10", "f10", "f9"),
          a = c("a2", "a3", "a4", "a5", "a1", "a1", "a2", "a3", "a4", "a5", "a1"),
          b = c("b2", "b8", "b4", "b10", "b1", "b6", "b7", "b3", "b9", "b5", "b2"),
          c = c("c2", "c8", "c4", "c10", "c1", "c6", "c7", "c3", "c9", "c5", "c3"),
          d = c("d2", "d8", "d4", "d10", "d1", "d3", "d7", "d3", "d9", "d5", "d4"),
          value = c(2L, 8L, 4L, 10L, 1L, 2L, 7L, 3L, 9L, 5L, 1.5)
        )
      )
      session$setInputs(pivotTable_rows_selected = c(2, 11), btRemoveRows = 2L)
      expect_identical(
        convert_to_df(dataToRender()),
        data.frame(
          e = c("e10", "e10", "e10", "e2", "e2", "e2", "e2", "e2", "e2"),
          f = c("f10", "f10", "f10", "f10", "f10", "f10", "f10", "f10", "f10"),
          a = c("a2", "a4", "a5", "a1", "a1", "a2", "a3", "a4", "a5"),
          b = c("b2", "b4", "b10", "b1", "b6", "b7", "b3", "b9", "b5"),
          c = c("c2", "c4", "c10", "c1", "c6", "c7", "c3", "c9", "c5"),
          d = c("d2", "d4", "d10", "d1", "d3", "d7", "d3", "d9", "d5"),
          value = c(2, 4, 10, 1, 2, 7, 3, 9, 5)
        )
      )
      # remove row by setting value to "" (empty string)
      session$setInputs(pivotTable_cell_edit = list(col = 6L, row = 1L, value = ""))
      expect_identical(
        convert_to_df(dataToRender()),
        data.frame(
          e = c("e10", "e10", "e2", "e2", "e2", "e2", "e2", "e2"),
          f = c("f10", "f10", "f10", "f10", "f10", "f10", "f10", "f10"),
          a = c("a4", "a5", "a1", "a1", "a2", "a3", "a4", "a5"),
          b = c("b4", "b10", "b1", "b6", "b7", "b3", "b9", "b5"),
          c = c("c4", "c10", "c1", "c6", "c7", "c3", "c9", "c5"),
          d = c("d4", "d10", "d1", "d3", "d7", "d3", "d9", "d5"),
          value = c(4, 10, 1, 2, 7, 3, 9, 5)
        )
      )
      expect_identical(
        convert_to_df(session$returned()),
        data.frame(
          a = c("a1", "a3", "a4", "a5", "a1", "a2", "a4", "a5"),
          b = c("b1", "b3", "b4", "b5", "b6", "b7", "b9", "b10"),
          c = c("c1", "c3", "c4", "c5", "c6", "c7", "c9", "c10"),
          d = c("d1", "d3", "d4", "d5", "d3", "d7", "d9", "d10"),
          e = c("e2", "e2", "e10", "e2", "e2", "e2", "e2", "e10"),
          f = c("f10", "f10", "f10", "f10", "f10", "f10", "f10", "f10"),
          value = c(1, 3, 4, 5, 2, 7, 9, 10)
        )
      )
    },
    args = list(
      data = testData,
      options = list(
        enablePersistentViews = FALSE, `_input_` = TRUE,
        "_metadata_" = list(symtype = "parameter")
      )
    )
  )
})

test_that("Editing with active filters works", {
  testServer(renderMiroPivot,
    {
      session$setInputs(
        rowIndexList = letters[2:6], colIndexList = character(),
        filterIndexList = "a", filter_a = "a2", aggregationIndexList = character(),
        aggregationFunction = "sum"
      )
      expect_identical(
        convert_to_df(dataToRender()),
        data.frame(
          b = c("b2", "b7"),
          c = c("c2", "c7"),
          d = c("d2", "d7"),
          e = c("e10", "e2"),
          f = c("f10", "f10"),
          value = c(2L, 7L)
        )
      )
      session$setInputs(pivotTable_cell_edit = list(col = 5, row = 2L, value = 2L))
      session$setInputs(pivotTable_cell_edit = list(col = 4, row = 2L, value = "f9"))
      expect_identical(
        convert_to_df(dataToRender()),
        data.frame(
          b = c("b2", "b7"),
          c = c("c2", "c7"),
          d = c("d2", "d7"),
          e = c("e10", "e2"),
          f = c("f10", "f9"),
          value = c(2L, 2L)
        )
      )
      session$setInputs(
        rowIndexList = c("b", "c", "d", "f"),
        filterIndexList = c("a", "e"),
        filter_e = c("e10", "e2")
      )
      expect_identical(
        convert_to_df(dataToRender()),
        data.frame(
          b = c("b2", "b7"),
          c = c("c2", "c7"),
          d = c("d2", "d7"),
          f = c("f10", "f9"),
          e = c("e10", "e2"),
          value = c(2L, 2L)
        )
      )
      session$setInputs(pivotTable_cell_edit = list(col = 0, row = 1L, value = "b3"))
      expect_identical(
        convert_to_df(dataToRender()),
        data.frame(
          b = c("b3", "b7"),
          c = c("c2", "c7"),
          d = c("d2", "d7"),
          f = c("f10", "f9"),
          e = c("e10", "e2"),
          value = c(2L, 2L)
        )
      )
      # invalid value should result in identical data
      session$setInputs(pivotTable_cell_edit = list(col = 5L, row = 1L, value = "a2"))
      expect_identical(
        convert_to_df(dataToRender()),
        data.frame(
          b = c("b3", "b7"),
          c = c("c2", "c7"),
          d = c("d2", "d7"),
          f = c("f10", "f9"),
          e = c("e10", "e2"),
          value = c(2L, 2L)
        )
      )
      # clicking the add row button should not throw an error
      session$setInputs(btAddRow = 1L)
      session$setInputs(
        newRow_1 = "b2", newRow_2 = "c2", newRow_3 = "d2", newRow_4 = "f10",
        newRow_5 = "e1", newRow_6 = 1.5, btAddRowConfirm = 1L
      )
      # new row is filtered out
      expect_identical(
        convert_to_df(dataToRender()),
        data.frame(
          b = c("b3", "b7"),
          c = c("c2", "c7"),
          d = c("d2", "d7"),
          f = c("f10", "f9"),
          e = c("e10", "e2"),
          value = c(2, 2)
        )
      )
      session$setInputs(filter_e = c("e10", "e1"))
      session$elapse(550)
      expect_identical(
        convert_to_df(dataToRender()),
        data.frame(
          b = c("b2", "b3"),
          c = c("c2", "c2"),
          d = c("d2", "d2"),
          f = c("f10", "f10"),
          e = c("e1", "e10"),
          value = c(1.5, 2L)
        )
      )
      session$setInputs(pivotTable_rows_selected = c(2), btRemoveRows = 2L)
      expect_identical(
        convert_to_df(dataToRender()),
        data.frame(
          b = c("b2"),
          c = c("c2"),
          d = c("d2"),
          f = c("f10"),
          e = c("e1"),
          value = c(1.5)
        )
      )
      expect_identical(
        convert_to_df(session$returned()),
        data.frame(
          a = c("a1", "a3", "a4", "a5", "a1", "a2", "a3", "a4", "a5", "a2"),
          b = c("b1", "b3", "b4", "b5", "b6", "b7", "b8", "b9", "b10", "b2"),
          c = c("c1", "c3", "c4", "c5", "c6", "c7", "c8", "c9", "c10", "c2"),
          d = c("d1", "d3", "d4", "d5", "d6", "d7", "d8", "d9", "d10", "d2"),
          e = c("e2", "e2", "e10", "e2", "e10", "e2", "e10", "e2", "e10", "e1"),
          f = c("f10", "f10", "f10", "f10", "f10", "f9", "f10", "f10", "f10", "f10"),
          value = c(1, 3, 4, 5, 6, 2, 8, 9, 10, 1.5)
        )
      )
    },
    args = list(
      data = testData,
      options = list(
        enablePersistentViews = FALSE, `_input_` = TRUE,
        "_metadata_" = list(symtype = "parameter")
      )
    )
  )
})

test_that("Editing with pivoted columns works", {
  testServer(renderMiroPivot,
    {
      session$setInputs(
        rowIndexList = c("b", "e", "f"), colIndexList = c("c", "d"),
        filterIndexList = "a", filter_a = "a2", aggregationIndexList = character(),
        aggregationFunction = "sum"
      )
      expect_identical(
        convert_to_df(dataToRender()),
        data.frame(
          b = c("b2", "b7"),
          e = c("e10", "e2"),
          f = c("f10", "f10"),
          c2.d2 = c(2L, NA),
          c7.d7 = c(NA, 7L)
        )
      )
      session$setInputs(pivotTable_cell_edit = list(col = 4, row = 2L, value = 2L))
      session$setInputs(pivotTable_cell_edit = list(col = 2, row = 2L, value = "f9"))
      expect_identical(
        convert_to_df(dataToRender()),
        data.frame(
          b = c("b2", "b7"),
          e = c("e10", "e2"),
          f = c("f10", "f9"),
          c2.d2 = c(2L, NA),
          c7.d7 = c(NA, 2L)
        )
      )
      session$setInputs(pivotTable_cell_edit = list(col = 4, row = 1L, value = 1.5))
      session$setInputs(pivotTable_cell_edit = list(col = 0, row = 1L, value = "b3"))
      expect_identical(
        convert_to_df(dataToRender()),
        data.frame(
          b = c("b3", "b7"),
          e = c("e10", "e2"),
          f = c("f10", "f9"),
          c2.d2 = c(2, NA),
          c7.d7 = c(1.5, 2)
        )
      )
      session$setInputs(pivotTable_cell_edit = list(col = 4, row = 1L, value = ""))
      session$setInputs(pivotTable_cell_edit = list(col = 0, row = 1L, value = "b2"))
      expect_identical(
        convert_to_df(dataToRender()),
        data.frame(
          b = c("b2", "b7"),
          e = c("e10", "e2"),
          f = c("f10", "f9"),
          c2.d2 = c(2, NA),
          c7.d7 = c(NA, 2)
        )
      )
      session$setInputs(pivotTable_cell_edit = list(col = 4, row = 1L, value = 1.5))
      session$setInputs(pivotTable_cell_edit = list(col = 0, row = 1L, value = "b3"))
      expect_identical(
        convert_to_df(dataToRender()),
        data.frame(
          b = c("b3", "b7"),
          e = c("e10", "e2"),
          f = c("f10", "f9"),
          c2.d2 = c(2, NA),
          c7.d7 = c(1.5, 2)
        )
      )
      session$setInputs(pivotTable_rows_selected = 1:2, btRemoveRows = 2L)
      # all rows removed -> filter switches to a1
      expect_identical(
        convert_to_df(dataToRender()),
        data.frame(
          b = c("b1", "b6"),
          e = c("e2", "e10"),
          f = c("f10", "f10"),
          c1.d1 = c(1, NA),
          c6.d6 = c(NA, 6)
        )
      )
      # clicking the add row button should not throw an error
      session$setInputs(filter_a = "a1", btAddRow = 1L)
      session$setInputs(
        newRow_1 = "b1", newRow_2 = "e2", newRow_3 = "f10",
        newRow_5 = 1.2, btAddRowConfirm = 1L
      )
      expect_identical(
        convert_to_df(dataToRender()),
        data.frame(
          b = c("b1", "b6"),
          e = c("e2", "e10"),
          f = c("f10", "f10"),
          c1.d1 = c(1, NA),
          c6.d6 = c(1.2, 6)
        )
      )
      expect_identical(
        convert_to_df(session$returned()),
        data.frame(
          a = c("a1", "a3", "a4", "a5", "a1", "a3", "a4", "a5", "a1"),
          b = c("b1", "b3", "b4", "b5", "b6", "b8", "b9", "b10", "b1"),
          c = c("c1", "c3", "c4", "c5", "c6", "c8", "c9", "c10", "c6"),
          d = c("d1", "d3", "d4", "d5", "d6", "d8", "d9", "d10", "d6"),
          e = c("e2", "e2", "e10", "e2", "e10", "e10", "e2", "e10", "e2"),
          f = c("f10", "f10", "f10", "f10", "f10", "f10", "f10", "f10", "f10"),
          value = c(1, 3, 4, 5, 6, 8, 9, 10, 1.2)
        )
      )
      session$setInputs(
        newRow_1 = "b1", newRow_2 = "e2", newRow_3 = "f11",
        newRow_4 = 123.001,
        newRow_5 = 156.123456789, btAddRowConfirm = 1L
      )
      expect_identical(
        convert_to_df(dataToRender()),
        data.frame(
          b = c("b1", "b1", "b6"),
          e = c("e2", "e2", "e10"),
          f = c("f10", "f11", "f10"),
          c1.d1 = c(1, 123.001, NA),
          c6.d6 = c(1.2, 156.123456789, 6)
        )
      )
      expect_identical(
        convert_to_df(session$returned()),
        data.frame(
          a = c("a1", "a3", "a4", "a5", "a1", "a3", "a4", "a5", "a1", "a1", "a1"),
          b = c("b1", "b3", "b4", "b5", "b6", "b8", "b9", "b10", "b1", "b1", "b1"),
          c = c("c1", "c3", "c4", "c5", "c6", "c8", "c9", "c10", "c6", "c1", "c6"),
          d = c("d1", "d3", "d4", "d5", "d6", "d8", "d9", "d10", "d6", "d1", "d6"),
          e = c("e2", "e2", "e10", "e2", "e10", "e10", "e2", "e10", "e2", "e2", "e2"),
          f = c("f10", "f10", "f10", "f10", "f10", "f10", "f10", "f10", "f10", "f11", "f11"),
          value = c(1, 3, 4, 5, 6, 8, 9, 10, 1.2, 123.001, 156.123456789)
        )
      )
    },
    args = list(
      data = testData,
      options = list(
        enablePersistentViews = FALSE, `_input_` = TRUE,
        "_metadata_" = list(symtype = "parameter")
      )
    )
  )
})

test_that("Adding rows to empty table works", {
  testServer(renderMiroPivot,
    {
      session$setInputs(
        rowIndexList = c("b", "e", "f"), colIndexList = c("c", "d"),
        filterIndexList = "a", filter_a = "a2", aggregationIndexList = character(),
        aggregationFunction = "sum"
      )
      expect_identical(
        convert_to_df(dataToRender()),
        data.frame(
          b = character(),
          e = character(),
          f = character(),
          value = integer()
        )
      )
      session$setInputs(filter_a = "a1", btAddRow = 1L)
      session$setInputs(
        newRow_1 = "a1", newRow_2 = "b1", newRow_3 = "c1",
        newRow_4 = "d6", newRow_5 = "e2", newRow_6 = "f10", newRow_7 = 1.2,
        btAddRowConfirm = 1L
      )
      expect_identical(
        convert_to_df(dataToRender()),
        data.frame(
          b = "b1",
          e = "e2",
          f = "f10",
          c1.d6 = 1.2
        )
      )
      expect_identical(
        convert_to_df(session$returned()),
        data.frame(
          a = "a1",
          b = "b1",
          c = "c1",
          d = "d6",
          e = "e2",
          f = "f10",
          value = 1.2
        )
      )
    },
    args = list(
      data = testData[0, ],
      options = list(
        enablePersistentViews = FALSE, `_input_` = TRUE,
        "_metadata_" = list(symtype = "parameter")
      )
    )
  )
})

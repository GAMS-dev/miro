context(paste0("Integration tests - Editable MIRO pivot - big data - list view -", Sys.info()[["sysname"]]))
skip_if(
  identical(Sys.getenv("SKIP_PERFORMANCE_TESTS"), "true"),
  "Skipping performance tests since SKIP_PERFORMANCE_TESTS is set."
)
library(dplyr)
library(shiny)
library(DT)
library(tidyr)
library(futile.logger)
library(chartjs)
library(jsonlite)

source("../../components/views.R")
source("../../modules/renderers/miro-pivot.R")

load("data/emp-bigdata.testData")
dataHeaders <- names(data)

convert_to_df <- function(df) {
  data.frame(df %>% ungroup() %>% mutate(across(where(is.factor), as.character)))
}
pr <- PerformanceReporter$new()
lang <<- list()

test_that("Editing large data in list view works", {
  testServer(renderMiroPivot,
    {
      session$setInputs(
        rowIndexList = dataHeaders[-length(dataHeaders)], colIndexList = character(),
        filterIndexList = character(), aggregationIndexList = character(),
        aggregationFunction = "sum"
      )
      pr$measure(
        "listEditenableEdit",
        session$setInputs(enableEdit = 1L)
      )
      pr$measure(
        "listEditRow1",
        session$setInputs(pivotTable_cell_edit = list(col = 10, row = 1L, value = 2L))
      )
      expect_identical(session$returned()[["cubeOutput"]][4065846], 2)
      pr$measure(
        "listEditRow2",
        session$setInputs(pivotTable_cell_edit = list(col = 0, row = 1L, value = "A_No_Nuclear"))
      )
      pr$measure(
        "listEditRowNewUEL",
        session$setInputs(pivotTable_cell_edit = list(col = 0, row = 1L, value = "blabla"))
      )
      expect_true("blabla" %in% session$returned()[[1]])
      pr$measure(
        "listEditAddRow",
        session$setInputs(
          newRow_1 = "BAU_upd", newRow_2 = "VAR_Act", newRow_3 = "-", newRow_4 = "-",
          newRow_5 = "RHSA-WHLTHRD", newRow_6 = "2010", newRow_7 = "CH",
          newRow_8 = "2010", newRow_9 = "SUM-WK-D01", newRow_10 = "bla", newRow_11 = 1.2345678,
          btAddRowConfirm = 1L
        )
      )
      res <- session$returned()[["cubeOutput"]]
      expect_identical(res[length(res)], 1.2345678)

      expect_identical(session$returned()[["cubeOutput"]][4065846], 2)
      expect_identical(nrow(session$returned()), 4163490L)
      pr$measure(
        "listEditRemoveRows",
        session$setInputs(pivotTable_rows_selected = c(2, 11), btRemoveRows = 2L)
      )
      expect_identical(nrow(session$returned()), 4163488L)
    },
    args = list(
      data = data,
      options = list(
        enablePersistentViews = FALSE, `_input_` = TRUE,
        "_metadata_" = list(symtype = "parameter")
      )
    )
  )
})
pr$publish()
context(paste0("Integration tests - Editable MIRO pivot - big data - table view - ", Sys.info()[["sysname"]]))
gc()
test_that("Editing large data in table view works", {
  testServer(renderMiroPivot,
    {
      session$setInputs(
        rowIndexList = dataHeaders[-c(2, 6, 9, length(dataHeaders))],
        colIndexList = c("ALLYEAR", "soName"),
        filterIndexList = "ALL_TS", filter_ALL_TS = "SUM-WK-D01",
        aggregationIndexList = character(),
        aggregationFunction = "sum"
      )
      pr$measure(
        "tableEditEnableEdit",
        session$setInputs(enableEdit = 1L)
      )
      expect_identical(length(dataToRender()), 105L)
      pr$measure(
        "tableEditRow1",
        session$setInputs(pivotTable_cell_edit = list(col = 8, row = 1L, value = 2L))
      )
      pr$measure(
        "tableEditRow2",
        session$setInputs(pivotTable_cell_edit = list(col = 6, row = 1L, value = "AYAYAA"))
      )
      expect_true("AYAYAA" %in% session$returned()[["UC_N"]])
      expect_identical(dataToRender()[[9]][1], 2)

      nrow <- nrow(session$returned())
      pr$measure(
        "tableEditAddRowDialog",
        session$setInputs(btAddRow = 1L)
      )
      pr$measure(
        "tableEditAddRow",
        session$setInputs(
          newRow_1 = "BAU_upd", newRow_2 = "-", newRow_3 = "-", newRow_4 = "-",
          newRow_5 = "CH", newRow_6 = "-", newRow_7 = "AYAYAA",
          newRow_8 = 1.23456, newRow_11 = 7.890123,
          btAddRowConfirm = 1L
        )
      )
      res <- session$returned()[["cubeOutput"]]
      expect_identical(res[length(res) - 1], 1.23456)
      expect_identical(res[length(res)], 7.890123)
      expect_identical(nrow(session$returned()), nrow + 2L)

      pr$measure(
        "tableEditRemoveRows",
        session$setInputs(pivotTable_rows_selected = c(1, 11), btRemoveRows = 2L)
      )
      expect_identical(nrow(session$returned()), 4163475L)
    },
    args = list(
      data = data,
      options = list(
        enablePersistentViews = FALSE, `_input_` = TRUE,
        "_metadata_" = list(symtype = "parameter")
      )
    )
  )
})
pr$publish()

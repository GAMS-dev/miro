context("MIRO pivot renderer")
library(dplyr)

source("../../global.R")
source("../../components/util.R")
source("../../modules/renderers/miro-pivot.R")


testData <- tibble(a = rep.int(paste0("a", seq_len(5)), 2L), b = paste0("b", seq_len(10)),
                   c = paste0("c", seq_len(10)), d = paste0("d", seq_len(10)),
                   e = paste0("e", seq_len(10)), f = "f10",
                   value = 1:10)
testDataFactor <- testData %>% mutate_if(is.character, as.factor)

convert_to_df <- function(df){
  data.frame(df %>% ungroup() %>% mutate_if(is.factor, as.character))
}

getFactorCol <- function(colName){
  return(structure(1:10,
                   .Label = paste0(colName, c("1", "10", "2", "3", "4", "5", "6", "7", "8", "9")),
                   class = "factor"))
}

test_that("MIRO pivot renderer handles filtering", {
  testServer(renderMiroPivot, {
    session$setInputs(rowIndexList = letters[1:6], colIndexList = character(),
                      filterIndexList = character())
    expect_null(filteredData())
    session$setInputs(aggregationIndexList = character())
    # fully initialized
    expect_identical(filteredData(), list(data = testDataFactor,
                                          filterElements = list()))
    session$setInputs(rowIndexList = letters[2:6],
                      filterIndexList = "a")
    expect_identical(filteredData(), 
                     list(data = testDataFactor[c(1, 6), ],
                          filterElements = list(a = factor(c("a1", "a2", "a3", "a4", "a5"))),
                          multiFilterIndices = NULL))
    session$setInputs(rowIndexList = letters[3:6], filterIndexList = c("a", "b"))
    expect_identical(filteredData(), 
                     list(data = testDataFactor[1, ],
                          filterElements = list(a = factor(c("a1", "a2", "a3", "a4", "a5")),
                                                b = factor(c("b1", "b6"),
                                                           levels = c("b1", "b10", "b2", "b3", "b4", "b5",
                                                                      "b6", "b7", "b8", "b9"))),
                          multiFilterIndices = NULL))
    session$setInputs(filterIndexList = c("a"),
                      aggregationIndexList = c("b"))
    expect_identical(filteredData(), 
                     list(data = testDataFactor[c(1, 6), ],
                          filterElements = list(a = factor(c("a1", "a2", "a3", "a4", "a5")),
                                                b = factor(c("b1", "b6"),
                                                           levels = c("b1", "b10", "b2", "b3", "b4", "b5",
                                                                      "b6", "b7", "b8", "b9"))),
                          multiFilterIndices = NULL))
    expect_equal(convert_to_df(dataToRender()),
                 data.frame(c = paste0("c", c("1", "6")),
                            d = paste0("d", c("1", "6")),
                            e = paste0("e", c("1", "6")),
                            f = "f10",
                            value = c(1L, 1L)))
    session$setInputs(filter_b = paste0("b", 1:10),
                      filterIndexList = character(), rowIndexList = letters[1:6][-2])
    expect_identical(convert_to_df(dataToRender()), data.frame(a = c("a1", "a1", "a2", "a2", "a3", "a3", "a4", "a4", "a5", "a5"),
                                                               c = paste0("c", c("1", "6", "2", "7", "3", "8", "4", "9", "10", "5")),
                                                               d = paste0("d", c("1", "6", "2", "7", "3", "8", "4", "9", "10", "5")),
                                                               e = paste0("e", c("1", "6", "2", "7", "3", "8", "4", "9", "10", "5")),
                                                               f = "f10",
                                                               "value" = 1L))
  }, args = list(data = testData,
                 options = list(enablePersistentViews = FALSE)))
})

test_that("MIRO pivot renderer handles pivoting", {
  testServer(renderMiroPivot, {
    session$setInputs(rowIndexList = letters[1:5], colIndexList = "f",
                      filterIndexList = character(), aggregationIndexList = character())
    expect_identical(convert_to_df(dataToRender()), data.frame(a = c("a1", "a1", "a2", "a2", "a3", "a3", "a4", "a4", "a5", "a5"),
                                                               b = paste0("b", c("1", "6", "2", "7", "3", "8", "4", "9", "10", "5")),
                                                               c = paste0("c", c("1", "6", "2", "7", "3", "8", "4", "9", "10", "5")),
                                                               d = paste0("d", c("1", "6", "2", "7", "3", "8", "4", "9", "10", "5")),
                                                               e = paste0("e", c("1", "6", "2", "7", "3", "8", "4", "9", "10", "5")),
                                                               "f10" = c(1L, 6L, 2L, 7L, 3L, 8L, 4L, 9L, 10L, 5L)))
    session$setInputs(rowIndexList = letters[2:6], colIndexList = "a")
    expect_equal(convert_to_df(dataToRender()), data.frame(b = paste0("b", c("1", "10", "2", "3", "4", "5", "6", "7", "8", "9")),
                                                           c = paste0("c", c("1", "10", "2", "3", "4", "5", "6", "7", "8", "9")),
                                                           d = paste0("d", c("1", "10", "2", "3", "4", "5", "6", "7", "8", "9")),
                                                           e = paste0("e", c("1", "10", "2", "3", "4", "5", "6", "7", "8", "9")),
                                                           f = "f10",
                                                           "a1" = c(1L, NA, NA, NA, NA, NA, 6L, NA, NA, NA),
                                                           "a2" = c(NA, NA, 2L, NA, NA, NA, NA, 7L, NA, NA),
                                                           "a3" = c(NA, NA, NA, 3L, NA, NA, NA, NA, 8L, NA),
                                                           "a4" = c(NA, NA, NA, NA, 4L, NA, NA, NA, NA, 9L),
                                                           "a5" = c(NA, 10L, NA, NA, NA, 5L, NA, NA, NA, NA)))
  }, args = list(data = testData,
                 options = list(enablePersistentViews = FALSE)))
})
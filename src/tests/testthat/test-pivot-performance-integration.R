context(paste0("Integration tests - MIRO pivot - big data - ", Sys.info()[['sysname']]))
skip_if(identical(Sys.getenv("SKIP_PERFORMANCE_TESTS"), "true"),
        "Skipping performance tests since SKIP_PERFORMANCE_TESTS is set.")
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

convert_to_df <- function(df){
  data.frame(df %>% ungroup() %>% mutate_if(is.factor, as.character))
}
pr <- PerformanceReporter$new()
lang <<- list()

test_that("Filtering, pivoting, aggregating large data works", {
  testServer(renderMiroPivot, {
    session$setInputs(rowIndexList = dataHeaders[-length(dataHeaders)], colIndexList = character(),
                      filterIndexList = character(),
                      aggregationIndexList = character(),
                      aggregationFunction = "sum")
    pr$measure("filter",
               {
                 session$setInputs(rowIndexList = dataHeaders[-c(2, 6, length(dataHeaders))],
                                   filterIndexList = c("soName", "ALLYEAR"),
                                   filter_soName = c("Cost_Flox", "VAR_Comprd", "Time_NPV",
                                                     "EQ_Combal", "EQ_CombalM"),
                                   filter_ALLYEAR = c("2010", "2012", "2015", "2020", "2030"))
                 expect_identical(nrow(dataToRender()), 484565L)
               })

    pr$measure("pivot",
               {
                 session$setInputs(rowIndexList = dataHeaders[-c(2, 4, 6, 9, length(dataHeaders))],
                                   colIndexList = c("COM_GRP", "ALL_TS"))
                 expect_identical(length(dataToRender()), 99563L)
               })

    pr$measure("aggregate",
               {
                 session$setInputs(rowIndexList = dataHeaders[-c(2, 4, 5, 6, 9, length(dataHeaders))],
                                   aggregationIndexList = c("PRC"))
                 expect_identical(nrow(dataToRender()), 25L)
               })
  }, args = list(data = data,
                 options = list(enablePersistentViews = FALSE, input = TRUE,
                                "_metadata_" = list(symtype = "parameter"))))
})
pr$publish()

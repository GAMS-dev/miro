miroanalysis_test1Output <- function(id, height = NULL, options = NULL, path = NULL) {
  ns <- NS(id)
  tags$div(
    plotOutput(ns("maxstockVsErrorTrain")), tableOutput(ns("maxstockVsErrorTrainTable")),
    plotOutput(ns("maxstockVsErrorTest")), tableOutput(ns("maxstockVsErrorTestTable"))
  )
}

renderMiroanalysis_test1 <- function(input, output, session, data, options = NULL, path = NULL, ...) {
  scalarsPivoted <- dplyr::bind_rows(lapply(data$get("_scalars"), tidyr::pivot_wider, names_from = "scalar", values_from = "value", id_cols = character()))
  scalarsOutPivoted <- dplyr::bind_rows(lapply(data$get("_scalars_out"), tidyr::pivot_wider, names_from = "scalar", values_from = "value", id_cols = character()))
  scalars <- suppressWarnings(dplyr::mutate(dplyr::bind_cols(scalarsPivoted, scalarsOutPivoted), across(everything(), as.numeric)))
  scalars[["error_train_rel"]] <- scalars[["error_train"]] / scalars[["trainingdays"]]
  scalars[["error_test_rel"]] <- scalars[["error_test"]] / scalars[["trainingdays"]]
  output$maxstockVsErrorTrain <- renderPlot(boxplot(error_train_rel ~ maxstock, scalars, main = "Training error (rel.) vs maximum number of stocks"))
  output$maxstockVsErrorTrainTable <- renderTable(scalars[c("maxstock", "error_train_rel")])
  output$maxstockVsErrorTest <- renderPlot(boxplot(error_test_rel ~ maxstock, scalars, main = "Testing error (rel.) vs maximum number of stocks"))
  output$maxstockVsErrorTestTable <- renderTable(scalars[c("maxstock", "error_test_rel")])
}

miroanalysis_test2Output <- function(id, height = NULL, options = NULL, path = NULL) {
  ns <- NS(id)
  tags$div(plotOutput(ns("tdVsErrorRatio")), tableOutput(ns("tdVsErrorRatioTable")))
}

renderMiroanalysis_test2 <- function(input, output, session, data, options = NULL, path = NULL, ...) {
  scalarsPivoted <- dplyr::bind_rows(lapply(data$get("_scalars"), tidyr::pivot_wider, names_from = "scalar", values_from = "value", id_cols = character()))
  scalarsOutPivoted <- dplyr::bind_rows(lapply(data$get("_scalars_out"), tidyr::pivot_wider, names_from = "scalar", values_from = "value", id_cols = character()))
  scalars <- suppressWarnings(dplyr::mutate(dplyr::bind_cols(scalarsPivoted, scalarsOutPivoted), across(everything(), as.numeric)))
  scalars[["error_train_rel"]] <- scalars[["error_train"]] / scalars[["trainingdays"]]
  scalars[["error_test_rel"]] <- scalars[["error_test"]] / scalars[["trainingdays"]]
  scalars[["error_ratio_rel"]] <- scalars[["error_train_rel"]] / scalars[["error_test_rel"]]
  output$tdVsErrorRatio <- renderPlot(boxplot(error_ratio_rel ~ trainingdays, scalars, main = "Error ratio (rel.) vs number training days"))
  output$tdVsErrorRatioTable <- renderTable(scalars[c("trainingdays", "error_ratio_rel")])
}

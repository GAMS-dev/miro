mirocompare_test1Output <- function(id, height = NULL, options = NULL, path = NULL) {
  ns <- NS(id)
  tags$div(
    textOutput(ns("title")),
    plotOutput(ns("maxstockVsErrorTrain")), tableOutput(ns("maxstockVsErrorTrainTable")),
    plotOutput(ns("maxstockVsErrorTest")), tableOutput(ns("maxstockVsErrorTestTable"))
  )
}

renderMirocompare_test1 <- function(input, output, session, data, options = NULL, path = NULL, rendererEnv = NULL, views = NULL, ...) {
  views$add(session, "test1", list(a = "bla"))
  if (!identical(options$advanced, list(test = "huhu"))) {
    stop("AYAYAYAYAYA!!!", call. = FALSE)
  }
  if (length(options$title)) {
    if (tryCatch(
      {
        views$get(session, "test123", "global")
        TRUE
      },
      error_not_found = function(e) {
        return(FALSE)
      }
    )) {
      stop("NANANANANANA!!!", call. = FALSE)
    }
  } else if (!identical(views$get(session, "test123", "global"), list(b = "def"))) {
    stop("NANANANANANA!!!", call. = FALSE)
  }
  if (!identical(views$get(session, "test1", "local"), list(a = "bla"))) {
    stop("NONONONONO!!!", call. = FALSE)
  }
  output$title <- renderText(if (length(options$title)) options$title else "default")
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

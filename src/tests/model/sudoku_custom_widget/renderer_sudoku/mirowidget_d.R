mirowidget_dOutput <- function(id, height = NULL, options = NULL, path = NULL) {
  ns <- NS(id)
  return(tagList(textOutput(ns("i")), textOutput(ns("j")), rHandsontableOutput(ns("sudoku"))))
}

renderMirowidget_d <- function(input, output, session, data, options = NULL, path = NULL, rendererEnv = NULL, views = NULL, ...) {
  output$sudoku <- renderRHandsontable(rhandsontable(data[["d"]]()))
  output$i <- renderText({
    if (length(data[["i"]]())) {
      stopifnot(length(data[["i"]]()) == 2L)
      return(paste(data[["i"]]()[[1]], collapse = ","))
    }
    return("")
  })
  output$j <- renderText({
    if (length(data[["j"]]())) {
      stopifnot(length(data[["j"]]()) == 2L)
      return(paste(data[["j"]]()[[1]], collapse = ","))
    }
    return("")
  })
  dataToReturn <- reactive({
    if (is.null(input$sudoku)) {
      return(data[["d"]]())
    }
    dataTmp <- hot_to_r(input$sudoku)
  })
  return(list(d = dataToReturn, i = reactive({
    dataTmp <- unique(dataToReturn()[[1]])
    tibble(i = dataTmp, text = rep.int("", length(dataTmp)))
  }), j = reactive({
    dataTmp <- unique(dataToReturn()[[2]])
    tibble(j = dataTmp, text = rep.int("", length(dataTmp)))
  })))
}

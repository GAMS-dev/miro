mirowidget_dOutput <- function(id, height = NULL, options = NULL, path = NULL) {
  ns <- NS(id)
  return(tagList(
    textOutput(ns("i")),
    textOutput(ns("j")),
    textOutput(ns("initial_state2")),
    selectInput(ns("bla"), "Bla", choices = c("bla1", "bla2")),
    rHandsontableOutput(ns("sudoku"))
  ))
}

renderMirowidget_d <- function(input, output, session, data, options = NULL, path = NULL, rendererEnv = NULL, views = NULL, ...) {
  output$sudoku <- renderRHandsontable(rhandsontable(data[["d"]]()))
  outputOptions(output, "sudoku", suspendWhenHidden = FALSE)
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
  output$initial_state2 <- renderText({
    return(as.character(sum(rowSums(data[["initial_state2"]]()[, -1], na.rm = TRUE))))
  })
  dataToReturn <- reactive({
    if (is.null(input$sudoku)) {
      return(NULL)
    }
    dataTmp <- hot_to_r(input$sudoku)
  })
  observe({
    if (length(data[["_gmspar_bla"]]()) && data[["_gmspar_bla"]]() %in% c("bla1", "bla2")) {
      updateSelectInput(session, "bla", selected = data[["_gmspar_bla"]]())
    }
  })
  return(list(d = dataToReturn, i = reactive({
    if (is.null(input$sudoku)) {
      return(NULL)
    }
    dataTmp <- unique(dataToReturn()[[1]])
    tibble(i = dataTmp, text = rep.int("", length(dataTmp)))
  }), j = data[["j"]], ii = data[["ii"]], test124 = data[["test124"]], `_gmspar_bla` = reactive({
    input$bla
  })))
}

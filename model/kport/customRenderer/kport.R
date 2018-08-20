testOutput <- function(id, height = NULL, options = NULL, path = NULL){
  ns <- NS(id)
  
  # set default height
  if(is.null(height)){
    height <- 800
  }
  tagList(
    sliderInput(ns("test"), "test slider", 10, 50, 20, width = "100%"),
    textOutput(ns("out"))
  )
}

renderTest <- function(input, output, server, data, options = NULL, path = NULL){
  output$out <- renderText(input$test)
}
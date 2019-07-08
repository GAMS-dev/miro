test123Output <- function(id, height, options, path){
   ns <- NS(id)
   sliderInput(ns('asd'), 'test123', 0, 10, 2)
}

renderTest123 <- function(input, output, session, data, options = NULL, path = NULL){
    print(data)
   test <- reactiveValues()
   observe(test$a <- input$asd)
   return(test)
}


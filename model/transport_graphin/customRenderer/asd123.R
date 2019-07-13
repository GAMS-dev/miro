test123Output <- function(id, height, options, path){
   ns <- NS(id)
   timevisOutput(ns('mytime'))
}

renderTest123 <- function(input, output, session, data, options = NULL, path = NULL){
  if(length(data) && nrow(data))
   output$mytime <- renderTimevis(timevis(options = list(editable = TRUE)))
  else
    output$mytime <- renderTimevis(timevis())
   return(reactive({input$mytime_data}))
}


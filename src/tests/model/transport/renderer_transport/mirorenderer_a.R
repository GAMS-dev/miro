mirorenderer_aOutput <- function(id, height = NULL, options = NULL, path = NULL) {
  ns <- NS(id)
  uiOutput(ns("test"))
}

renderMirorenderer_a <- function(input, output, session, data, options = NULL, path = NULL, rendererEnv = NULL, views = NULL, outputScalarsFull = NULL, ...) {
  output$test <- renderUI({
    if (length(data) == 3L && all(names(data) %in% c("a", "type", "d"))) {
      return(paste(data[["type"]], sum(data[["a"]][[2]]), sum(data[["d"]][[3]]), sep = "  "))
    } else {
      return("BAD")
    }
  })
}

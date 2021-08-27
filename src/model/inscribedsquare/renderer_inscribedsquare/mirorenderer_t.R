mirorenderer_tOutput <- function(id, height = NULL, options = NULL, path = NULL) {
  ns <- NS(id)
  if (is.null(height)) {
    height <- 700
  }
  tagList(textOutput(ns("title")), plotOutput(ns("curve"),
    width = height, height = height
  ))
}

renderMirorenderer_t <- function(input, output, session, data, options = NULL, path = NULL, views = NULL, ...) {
  fxfunc <- data[[1]][[5]]
  fyfunc <- data[[1]][[6]]
  output$title <- renderText(sprintf(
    "%s for curve = (%s, %s)",
    options$title, fxfunc, fyfunc
  ))
  t <- seq(-pi, pi, 0.01)
  fx <- eval(parse(text = fxfunc))
  fy <- eval(parse(text = fyfunc))
  t <- data[[2]][1:4]
  fsolx <- eval(parse(text = fxfunc))
  fsoly <- eval(parse(text = fyfunc))
  output$curve <- renderPlot({
    plot(fx, fy,
      type = "l", ann = FALSE, bty = "n", xaxt = "n",
      yaxt = "n", asp = 1L
    )
    points(fsolx, fsoly, col = "red")
  })
}

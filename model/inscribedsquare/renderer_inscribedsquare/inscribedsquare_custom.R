curveOutput <- function(id, height = NULL, options = NULL, path = NULL){
    ns <- NS(id)
 
    # set default height
    if(is.null(height)){
      height <- 700
    } 
    tagList( 
       textOutput(ns("title")),
       plotOutput(ns("curve"), width = height, height = height)
    )
}

renderCurve <- function(input, output, session, data, options = NULL, path = NULL){ 
    #renderer 
    #output$title <- renderText(paste0(options$title))

    #fxfunc <- "sin(t)*cos(t-t*t)"
    #fyfunc <- "t*sin(t)"
    
    # we have been hiding --fx and --fy in the label of the 5th and 6th entries of t
    # and the labels seem to be in data[[1]]
    fxfunc <- data[[1]][[5]]
    fyfunc <- data[[1]][[6]]
    output$title <- renderText(sprintf("%s for curve = (%s, %s)", options$title, fxfunc, fyfunc))
    
    # evaluate curve on sample points
    t <- seq(-pi,pi,0.01)
    fx <- eval(parse(text=fxfunc))
    fy <- eval(parse(text=fyfunc))
    
    # evaluate corner points of rectangle
    # this seems to be t.l
    t <- data[[2]]  #unlist(data[2], use.names = FALSE)
    fsolx <- eval(parse(text=fxfunc))
    fsoly <- eval(parse(text=fyfunc))
    
    # plot curve and rectangle corner points
    output$curve <- renderPlot({
      plot(fx, fy, type="l", ann=FALSE, bty="n", xaxt="n", yaxt="n")
      points(fsolx, fsoly, col="red")
    })
}

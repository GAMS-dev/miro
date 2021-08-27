generateDataUI <- function(id, type, height = NULL, customOptions = NULL) {
  ns <- NS(id)

  typeCustom <- type

  if (tolower(typeCustom) == "miropivot") {
    return(tags$div(
      id = ns("data"),
      miroPivotOutput(ns("inputPivot"),
        height = height, options = customOptions
      )
    ))
  }

  tryCatch(
    {
      customInput <- match.fun(typeCustom %+% "Output")
    },
    error = function(e) {
      stop(sprintf("An input function for the custom generator: '%s' was not found.
                   Please make sure you first define such a function.", typeCustom), call. = FALSE)
    }
  )
  data <- customInput(ns("custom"),
    height = height, options = customOptions,
    path = customRendererDir
  )
  return(tags$div(id = ns("data"), data))
}

generateData <- function(input, output, session, data, type,
                         configData = NULL, customOptions = NULL, rendererEnv = NULL,
                         views = NULL, attachments = NULL) {
  typeCustom <- type
  if (tolower(typeCustom) == "miropivot") {
    return(renderMiroPivot("inputPivot", data,
      options = customOptions,
      path = customRendererDir, rendererEnv = rendererEnv, views = views
    ))
  }
  tryCatch(
    {
      customGenerator <- match.fun(paste0(
        "render", toupper(substr(typeCustom, 1, 1)),
        substr(typeCustom, 2, nchar(typeCustom))
      ))
    },
    error = function(e) {
      stop(sprintf("A custom generator function: '%s' was not found.
                   Please make sure you first define such a function.", typeCustom), call. = FALSE)
    }
  )
  tryCatch(
    {
      return(callModule(customGenerator, "custom", data,
        options = customOptions,
        path = customRendererDir, rendererEnv = rendererEnv, views = views,
        attachments = attachments
      ))
    },
    error = function(e) {
      stop(sprintf(
        "An error occured in the custom generator function: '%s'. Error message: %s.",
        typeCustom, conditionMessage(e)
      ), call. = FALSE)
    }
  )
}

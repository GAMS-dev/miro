JSONSorter <- R6Class("JSONSorter",
  public = list(
    initialize = function(libPath = "JS", filePath = NULL) {
      private$filePath <- filePath
      private$ctx <- v8()
      private$ctx$source(file.path(libPath, "jsonabc.js"))
      private$ctx$assign("jsonabc", JS("require('jsonabc')"))
      return(invisible(self))
    },
    sort = function(JSON) {
      stopifnot(is.character(JSON), length(JSON) == 1)
      return(private$ctx$call("jsonabc.sort", JSON, TRUE))
    },
    write = function(dataToWrite, filePath = NULL) {
      writeLines(
        self$sort(toJSON(dataToWrite, auto_unbox = TRUE, null = "null")),
        if (is.null(filePath)) private$filePath else filePath
      )
    }
  ),
  private = list(
    ctx = NULL,
    filePath = NULL
  )
)

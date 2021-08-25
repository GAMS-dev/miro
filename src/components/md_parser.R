MarkdownParser <- R6Class("MarkdownParser",
                          public = list(
                            initialize = function(enableMath = FALSE, libPath = 'www'){
                              private$ctx <- v8()
                              private$ctx$source(file.path(libPath, 'showdown.min.js'))
                              if(enableMath){
                                private$ctx$source(file.path(libPath, 'mathjax-extension.js'))
                              }
                              private$ctx$assign("converter",
                                                 JS(paste0("new showdown.Converter({tables: true,
                                                    tasklists:true, strikethrough:true,
                                                    noHeaderId: true,
                                                    openLinksInNewWindow: true",
                                                    if(enableMath) ",extensions: ['mathjax']", "})")))
                              return(invisible(self))
                            },
                            parse = function(markdown){
                              stopifnot(is.character(markdown), length(markdown) == 1)
                              return(private$ctx$call("converter.makeHtml", markdown))
                            },
                            parseFile = function(filePath){
                              if(!file.exists(filePath))
                                stop(sprintf("File: %s does not exist.", filePath),
                                     call. = FALSE)
                              return(self$parse(read_file(filePath)))
                            },
                            md2html = function(mdFilePath, htmlFilePath){
                              write_file(self$parseFile(mdFilePath), htmlFilePath)
                              return(invisible(self))
                            }
                          ),
                          private = list(
                            ctx = NULL
                          ))

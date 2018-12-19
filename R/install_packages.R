newPackages <- requiredPackages[!(requiredPackages %in% 
                                    installed.packages(lib.loc = RLibPath)[, "Package"])]
if(length(newPackages)){
  checkSourceDefault <- getOption("install.packages.check.source")
  options(install.packages.check.source = "no")
  packageDepDb <- list("R6" = c(), "stringi" = c(),
                       "shiny" = c("httpuv", "mime", 
                                   "jsonlite", "xtable", "digest", "htmltools", "R6", 
                                   "sourcetools", "later", "promises", "crayon", "rlang"),
                       "shinydashboard" = c("shiny", "htmltools", "promises"),
                       "DT" = c("htmltools", "htmlwidgets", "magrittr", "crosstalk"),
                       "processx" = c("assertthat", "crayon", "ps", "R6"),
                       "V8" = c("Rcpp", "jsonlite", "curl"), 
                       "dplyr" = c("assertthat", "bindrcpp", "glue", "magrittr", "pkgconfig",
                                   "R6", "Rcpp", "rlang", "tibble"), "readr" = c("Rcpp", "tibble", "hms", "R6", "BH"),
                       "readxl" = c("cellranger", "Rcpp", "tibble"), "writexl" = c(), 
                       "rhandsontable" = c("jsonlite", "htmlwidgets", "magrittr"),
                       "plotly" = c("ggplot2", "scales", "httr", "jsonlite", "magrittr", "digest", "viridisLite", "base64enc",
                                    "htmltools", "htmlwidgets", "tidyr", "hexbin", "RColorBrewer", "dplyr", "tibble", "lazyeval", "rlang",
                                    "crosstalk", "purrr", "data.table", "promises"),
                       "jsonlite" = c(), "jsonvalidate" = c("V8"), "rpivotTable" = c("htmlwidgets"),
                       "futile.logger" = c("lambda.r", "futile.options"), "dygraphs" = c("magrittr", "htmlwidgets", 
                                                                                         "htmltools", "zoo", "xts"),
                       "xts" = c("zoo"), "zip" = c(), "tidyr" = c("dplyr", "glue", "magrittr", "purrr", "Rcpp", "rlang", 
                                                                  "stringi", "tibble", "tidyselect"),
                       "DBI" = c(), "RPostgres" = c("bit64", "blob", "DBI", "hms", "Rcpp", 
                                                    "withr", "BH", "plogr"),
                       "RSQLite" = c("bit64", "blob", "DBI", "memoise", "pkgconfig", "Rcpp", "BH", "plogr"))
  newPackages <- unique(unlist(c(lapply(newPackages, function(package){packageDepDb[[package]]}), newPackages)))
  if(identical(tolower(Sys.info()[["sysname"]]), "windows")){
    binFileExt <- "_.*\\.zip$"
  }else{
    binFileExt <- "_.*\\.tgz$"
  }
  for(pkg_name in newPackages){
    if(pkg_name %in% installed.packages(lib.loc = RLibPath)[, "Package"]){
      next
    }
    tryCatch({
      if(!is.null(RLibPath)){
        pkg_path <- NULL
        try(pkg_path <- list.files(RLibPath, paste0("^", pkg_name, binFileExt), 
                                   full.names = TRUE, recursive = TRUE))
        if(length(pkg_path)){
          install.packages(pkg_path[[1]], lib = RLibPath, repos = NULL, 
                           type="binary", dependencies = FALSE)
          next
        }
      }
      
      install.packages(pkg_name, lib = if(length(RLibPath)) RLibPath else .libPaths()[[1]], repos = CRANMirror, dependencies = TRUE)
    }, error = function(e){
      if(exists("flog.fatal")){
        flog.fatal("Problems installing required R packages. Error message: %s.", e)
      }
      errMsg <<- paste(errMsg, paste0("Some packages could not be installed. Error message: ", 
                                      e), sep = "\n")
    })
  }
  options(install.packages.check.source = checkSourceDefault)
  rm(checkSourceDefault) 
  rm(binFileExt)
}

tryCatch({
  suppressMessages(lapply(requiredPackages, library, character.only = TRUE, 
                          quietly = TRUE, verbose = FALSE, warn.conflicts = FALSE, lib.loc = RLibPath))
}, error = function(e){
  if(exists("flog.fatal")){
    flog.fatal("Problems loading required R packages. Error message: %s.", e)
  }
  errMsg <<- paste(errMsg, paste0("Not all the required packages are installed. Error message: ", e), sep = "\n")
})
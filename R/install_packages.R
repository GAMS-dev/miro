newPackages <- requiredPackages[!(requiredPackages %in% 
                                    installed.packages(lib.loc = RLibPath)[, "Package"])]
if(length(newPackages)){
  checkSourceDefault <- getOption("install.packages.check.source")
  options(install.packages.check.source = "no")
  packageDepDb <- list("R6" = c(), "stringi" = c("tools", "utils", "stats"),
                       "shiny" = c("methods", "utils", "grDevices", "httpuv", "mime", 
                                   "jsonlite", "xtable", "digest", "htmltools", "R6", 
                                   "sourcetools", "later", "promises", "tools", "crayon", "rlang"),
                       "shinydashboard" = c("utils", "shiny", "htmltools", "promises"),
                       "DT" = c("htmltools", "htmlwidgets", "magrittr", "crosstalk"),
                       "processx" = c("assertthat", "crayon", "ps", "R6", "utils"),
                       "V8" = c("Rcpp", "jsonlite", "curl", "utils"), 
                       "dplyr" = c("assertthat", "bindrcpp", "glue", "magrittr", "methods", "pkgconfig",
                                   "R6", "Rcpp", "rlang", "tibble"), "readr" = c("Rcpp", "tibble", "hms", "R6", "BH"),
                       "readxl" = c("cellranger", "Rcpp", "tibble"), "writexl" = c(), 
                       "rhandsontable" = c("jsonlite", "htmlwidgets", "magrittr", "methods"),
                       "plotly" = c("ggplot2", "tools", "scales", "httr", "jsonlite", "magrittr", "digest", "viridisLite", "base64enc",
                                    "htmltools", "htmlwidgets", "tidyr", "hexbin", "RColorBrewer", "dplyr", "tibble", "lazyeval", "rlang",
                                    "crosstalk", "purrr", "data.table", "promises"),
                       "jsonlite" = c("methods"), "jsonvalidate" = c("V8"), "rpivotTable" = c("htmlwidgets"),
                       "futile.logger" = c("utils", "lambda.r", "futile.options"), "dygraphs" = c("magrittr", "htmlwidgets", 
                                                                                                  "htmltools", "zoo", "xts"),
                       "xts" = c("zoo", "methods"), "zip" = c(), "tidyr" = c("dplyr", "glue", "magrittr", "purrr", "Rcpp", "rlang", 
                                                                             "stringi", "tibble", "tidyselect"),
                       "DBI" = c("methods"), "RPostgres" = c("bit64", "blob", "DBI", "hms", "methods", "Rcpp", 
                                                             "withr", "BH", "plogr"))
  newPackages <- unique(unlist(c(lapply(newPackages, function(package){packageDepDb[[package]]}), newPackages)))
  for(pkg_name in newPackages){
    tryCatch({
	  if(!is.null(RLibPath)){
	    pkg_path <- NULL
	    try(pkg_path <- list.files(RLibPath, paste0("^", pkg_name, "_.*\\.zip$"), 
	                               full.names = TRUE, recursive = TRUE))
	    if(length(pkg_path)){
	      install.packages(pkg_path[[1]], lib = RLibPath, repos = NULL, 
	                       type="binary", dependencies = FALSE)
	      next
	    }
	  }
      
    install.packages(pkg_name, lib = RLibPath, repos = CRANMirror, dependencies = TRUE)
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
}

tryCatch({
  lapply(requiredPackages, library, character.only = TRUE, 
         quietly = TRUE, verbose = FALSE, lib.loc = RLibPath)
}, error = function(e){
  if(exists("flog.fatal")){
    flog.fatal("Problems loading required R packages. Error message: %s.", e)
  }
  errMsg <<- paste(errMsg, paste0("Not all the required packages are installed. Error message: ", e), sep = "\n")
})
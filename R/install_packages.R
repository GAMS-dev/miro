newPackages <- requiredPackages[!requiredPackages %in% installedPackages]
if(length(newPackages)){
  checkSourceDefault <- getOption("install.packages.check.source")
  options(install.packages.check.source = "no")
  packageDepDb <- list("R6" = c(), "stringi" = c(),
                       "shiny" = c("httpuv", "mime", 
                                   "jsonlite", "xtable", "digest", "htmltools", "R6", 
                                   "sourcetools", "later", "promises", "crayon", "rlang", 
                                   "Rcpp", "BH", "magrittr"),
                       "shinydashboard" = c("shiny", "htmltools", "promises"),
                       "processx" = c("ps", "R6"),
                       "V8" = c("Rcpp", "jsonlite", "curl"), 
                       "dplyr" = c("assertthat", "bindrcpp", "glue", "magrittr", "pkgconfig",
                                   "R6", "Rcpp", "rlang", "tibble", "tidyselect", "BH", "plogr",
                                   "bindr", "cli", "crayon", "pillar", "purrr", "fansi", "utf8"), 
                       "readr" = c("Rcpp", "tibble", "hms", "R6", "clipr", "BH", "pkgconfig",
                                   "rlang", "cli", "crayon", "pillar", "assertthat", "fansi",
                                   "utf8"),
                       "readxl" = c("cellranger", "Rcpp", "tibble", "rematch", "cli", "crayon",
                                    "pillar", "rlang", "assertthat", "fansi", "utf8"), 
                       "writexl" = c(), 
                       "rhandsontable" = c("jsonlite", "htmlwidgets", "magrittr", "htmltools", "yaml",
                                           "digest", "Rcpp"),
                       "jsonlite" = c(), "jsonvalidate" = c("V8", "Rcpp", "jsonlite", "curl"), 
                       "rpivotTable" = c("htmlwidgets", "jsonlite", "yaml", "digest", "Rcpp"),
                       "futile.logger" = c("lambda.r", "futile.options", "formatR"),
                       "zip" = c(), "tidyr" = c("dplyr", "glue", "magrittr", "purrr", "Rcpp", "rlang", 
                                                "stringi", "tibble", "tidyselect", "assertthat", 
                                                "bindrcpp", "pkgconfig", "R6", "BH", "plogr", "cli",
                                                "crayon", "pillar", "bindr", "fansi", "utf8"),
                       "DBI" = c())
  newPackages <- unique(unlist(lapply(newPackages, function(package){c(package, packageDepDb[[package]])})))
  if(identical(tolower(Sys.info()[["sysname"]]), "windows")){
    binFileExt <- "_.*\\.zip$"
  }else{
    binFileExt <- "_.*\\.(tgz|tar)$"
  }
  for(pkg_name in newPackages){
    print(paste0("Installing: ", pkg_name))
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
      
      install.packages(pkg_name, lib = if(length(RLibPath)) RLibPath else .libPaths()[[1]], 
                       repos = CRANMirror, dependencies = c("Depends", "Imports", "LinkingTo"))
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
  suppressWarnings(suppressMessages(lapply(requiredPackages, library, character.only = TRUE, 
                          quietly = TRUE, verbose = FALSE, warn.conflicts = FALSE, lib.loc = RLibPath)))

  }, error = function(e){
  if(exists("flog.fatal")){
    flog.fatal("Problems loading required R packages. Error message: %s.", e)
  }
  errMsg <<- paste(errMsg, paste0("Not all the required packages are installed. Error message: ", e), sep = "\n")
})
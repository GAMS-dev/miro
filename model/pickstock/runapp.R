RLibPath <- '/Applications/GAMS25.1/sysdir/GMSWebUI/library'

  if(RLibPath == ""){
     RLibPath <- NULL
  }else{
     assign(".lib.loc", RLibPath, envir = environment(.libPaths))
  }
  if(!'shiny'%in%installed.packages(lib.loc = RLibPath)[, 'Package']){
    checkSourceDefault <- getOption("install.packages.check.source")
    options(install.packages.check.source = "no")
    newPackages <- c("httpuv", "mime", 
                     "jsonlite", "xtable", "digest", "htmltools", "R6", 
                     "sourcetools", "later", "promises", "crayon", "rlang", 
                     "Rcpp", "BH", "magrittr", "shiny")
    if(identical(tolower(Sys.info()[["sysname"]]), "windows")){
      binFileExt <- "_.*\\.zip$"
    }else{
      binFileExt <- "_.*\\.tgz$"
    }
    for(pkg_name in newPackages){
      print(paste0("Installing: ", pkg_name))
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
      install.packages(pkg_name, lib = if(length(RLibPath)) RLibPath else .libPaths()[[1]], repos = 'https://cloud.r-project.org', dependencies = TRUE)
    }
    options(install.packages.check.source = checkSourceDefault)
    rm(checkSourceDefault)
  }
  library("shiny", character.only = TRUE, quietly = TRUE, verbose = FALSE, warn.conflicts = FALSE, lib.loc = RLibPath)
  shiny::runApp(appDir = file.path("/Applications/GAMS25.1/sysdir/GMSWebUI"), launch.browser=TRUE)
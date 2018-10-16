newPackages <- requiredPackages[!(requiredPackages %in% 
                                    installed.packages(lib.loc = RLibPath)[, "Package"])]
if(length(newPackages)){
  checkSourceDefault <- getOption("install.packages.check.source")
  options(install.packages.check.source = "no")
  newPackages <- unique(unlist(c(newPackages, 
                                 tools::package_dependencies(newPackages,
                                                             recursive = TRUE))))
  for(pkg_name in newPackages){
    tryCatch({
      pkg_path <- list.files(RLibPath, paste0("^", pkg_name, "_.*\\.zip$"), 
                             full.names = TRUE, recursive = TRUE)
      if(is.null(RLibPath) || !length(pkg_path)){
        install.packages(pkg_name, lib = RLibPath, repos = CRANMirror, dependencies = TRUE)
      }else{
        install.packages(pkg_path[[1]], lib = RLibPath, repos = NULL, 
                         type="binary", dependencies = FALSE)
      }
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
  lapply(requiredPackages, library, character.only = TRUE, lib.loc = RLibPath)
}, error = function(e){
  if(exists("flog.fatal")){
    flog.fatal("Problems loading required R packages. Error message: %s.", e)
  }
  errMsg <<- paste(errMsg, paste0("Not all the required packages are installed. Error message: ", e), sep = "\n")
})
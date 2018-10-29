newPackages <- requiredPackages[!(requiredPackages %in% 
                                    installed.packages(lib.loc = RLibPath)[, "Package"])]
if(length(newPackages)){
  checkSourceDefault <- getOption("install.packages.check.source")
  options(install.packages.check.source = "no")

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
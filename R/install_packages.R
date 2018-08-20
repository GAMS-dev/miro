newPackages <- requiredPackages[!(requiredPackages %in% installed.packages()[, "Package"])]
if(length(newPackages)){
  tryCatch({
    install.packages(newPackages, repos = CRANMirror, dependencies=TRUE)
  }, error = function(e){
    flog.fatal("Problems installing required R packages. Error message: %s.", e)
    errMsg <<- paste(errMsg, paste0("Some packages could not be installed. Error message: ", e), sep = "\n")
  }, warning = function(w){
    flog.fatal("Problems installing required R packages. Error message: %s.", w)
    errMsg <<- paste(errMsg, paste0("Some packages could not be installed. Error message: ", w), sep = "\n")
  })
}

tryCatch({
  lapply(requiredPackages, library, character.only = TRUE)
}, error = function(e){
  flog.fatal("Problems loading required R packages. Error message: %s.", e)
  errMsg <<- paste(errMsg, paste0("Not all the required packages are installed. Error message: ", e), sep = "\n")
})
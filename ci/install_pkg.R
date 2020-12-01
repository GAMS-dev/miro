libSrcPath = '/home/miro/r/library_src'
options(warn = 2)

listOfLibs = list.files(libSrcPath)

RLibPath <- .libPaths()[1]

for(package in c('Rcpp', 'plogr', 'BH', 'RSQLite')){
    packageFile = listOfLibs[grepl(paste0(package[1], '_'),
        listOfLibs, fixed = TRUE)][1]

    install.packages(file.path(libSrcPath, packageFile), 
      lib = RLibPath, repos = NULL, 
      type = "source", dependencies = FALSE)
}
# clean up unncecessary files
dontDisplayMe <- lapply(list.dirs(RLibPath, full.names = TRUE, recursive = FALSE), 
    function(x) {
        unlink(file.path(x, c("help", "doc", "tests", "html",
                              "include", "unitTests",
                              file.path("libs", "*dSYM"))), force=TRUE, recursive=TRUE)
})
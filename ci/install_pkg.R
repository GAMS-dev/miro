libSrcPath <- "/home/miro/r/library_src"
options(warn = 2)

listOfLibs <- list.files(libSrcPath)

RLibPath <- .libPaths()[1]

for (package in c("RSQLite")) {
  packageFile <- listOfLibs[grepl(paste0(package[1], "_"),
    listOfLibs,
    fixed = TRUE
  )][1]
  if (is.na(packageFile)) {
    print(sprintf("Source package: %s was not found. Downloading latest version from CRAN.", package))
    install.packages(package, lib = RLibPath, repos = "https://cloud.r-project.org/")
  } else {
    install.packages(file.path(libSrcPath, packageFile),
      lib = RLibPath, repos = NULL,
      type = "source", dependencies = FALSE
    )
  }
}
# need to install reinstall V8 with static libv8 as we also want nodejs
Sys.setenv(DOWNLOAD_STATIC_LIBV8 = 1)
install.packages("V8", lib = RLibPath, repos = "https://cloud.r-project.org/")

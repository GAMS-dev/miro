scriptPath <- Sys.getenv("SCRIPTS_PATH")
libSrcPath <- file.path(scriptPath, "..", "r", "library_src")

source(file.path(scriptPath, "globals.R"))
options(warn = 2)

if (R.version[["major"]] < 3 ||
  R.version[["major"]] == 3 && gsub(
    "\\..$", "",
    R.version[["minor"]]
  ) < 6) {
  stop("The R version you are using is not supported. At least version 3.6 is required to run GAMS MIRO.", call. = FALSE)
}
listOfLibs <- list.files(libSrcPath)

if (!length(RLibPath)) {
  RLibPath <- .libPaths()[1]
}

for (package in packageVersionMap) {
  if (package[1] %in% installedPackages) {
    print(sprintf("Skipping '%s' as it is already installed.", package[1]))
    next
  }

  if (length(package) == 2L) {
    packageFile <- paste0(package[1], "_", package[2], ".tar.gz")
  } else {
    packageFile <- listOfLibs[grepl(paste0(package[1], "_"),
      listOfLibs,
      fixed = TRUE
    )][1]
  }
  install.packages(file.path(libSrcPath, packageFile),
    lib = RLibPath, repos = NULL,
    type = "source", dependencies = FALSE
  )
}
# clean up unncecessary files
unlink(file.path(RLibPath, "INSTALLING"), force = TRUE, recursive = FALSE)
dontDisplayMe <- lapply(
  list.dirs(RLibPath, full.names = TRUE, recursive = FALSE),
  function(x) {
    unlink(file.path(x, c(
      "help", "doc", "tests", "html",
      "include", "unitTests",
      file.path("libs", "*dSYM")
    )), force = TRUE, recursive = TRUE)
  }
)

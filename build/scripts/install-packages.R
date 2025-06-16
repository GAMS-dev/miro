# install required packages for MIRO
local({
  packageVersionMapTmp <- read.csv("./src/miro-pkg-lock.csv", header = FALSE, stringsAsFactors = FALSE)
  packageVersionMapTmp <- trimws(deparse(lapply(seq_len(nrow(packageVersionMapTmp)), function(pkgIdx) {
    pkgInfo <- packageVersionMapTmp[pkgIdx, ]
    pkgInfo <- trimws(c(pkgInfo[[1]], pkgInfo[[2]]))
    if (identical(pkgInfo[2], "")) {
      return(pkgInfo[1])
    }
    return(pkgInfo)
  })), "right")
  packageVersionMapTmp[1] <- paste0("packageVersionMap <- ", packageVersionMapTmp[1])
  globalsSrc <- readLines("./scripts/globals.R", warn = FALSE)
  linesToReplaceLo <- grep("packageVersionMap", globalsSrc)[1] - 1
  linesToReplaceUp <- which("" == trimws(globalsSrc))
  linesToReplaceUp <- linesToReplaceUp[linesToReplaceUp > linesToReplaceLo][1]
  globalsSrc <- c(
    globalsSrc[seq_len(linesToReplaceLo)],
    packageVersionMapTmp,
    globalsSrc[seq(linesToReplaceUp, length(globalsSrc))]
  )
  writeLines(globalsSrc, "./scripts/globals.R")
})

source("./scripts/globals.R")
print("Library paths:")
print(.libPaths())
print("Development library path:")
print(RlibPathDevel)
print(sessionInfo())
options(timeout = 120)
if (CIBuild) {
  installedPackagesNotInTmpLib <- !installedPackages %in% c(installedPackagesTmp, "base", "boot", "class", "cluster", "codetools", "compiler", "datasets", "foreign", "graphics", "grDevices", "grid", "KernSmooth", "lattice", "MASS", "Matrix", "methods", "mgcv", "nlme", "nnet", "parallel", "rpart", "spatial", "splines", "stats", "stats4", "survival", "tcltk", "tools", "translations", "utils")
  if (any(installedPackagesNotInTmpLib)) {
    print(sprintf(
      "Some packages (%s) were found in lib path but not in tmp lib path. These packages will be removed from lib path and reinstalled.",
      paste(installedPackages[installedPackagesNotInTmpLib], collapse = ",")
    ))
    pathsToUnlink <- file.path(RLibPath, installedPackages)[installedPackagesNotInTmpLib]
    pathsFailedUnlink <- unlink(pathsToUnlink, recursive = TRUE, force = TRUE) != 0L
    if (any(pathsFailedUnlink)) {
      stop(sprintf(
        "Failed to remove: %s",
        paste(pathsToUnlink[pathsFailedUnlink], collapse = ",")
      ), call. = FALSE)
    }
  }
  installedPackages <- installedPackagesTmp
  customPackages <- packageVersionMap[vapply(packageVersionMap, function(package) {
    return(length(package) == 1L)
  }, logical(1), USE.NAMES = FALSE)]
  if (isMac) {
    # Required e.g. to find gettext for data.table
    Sys.setenv(
      CPATH = paste0("/opt/homebrew/include:", Sys.getenv("CPATH"))
    )
  }
}
for (libPath in c(RLibPath, RlibPathDevel, RlibPathTmp)) {
  if (!dir.exists(libPath) &&
    !dir.create(libPath, showWarnings = TRUE, recursive = TRUE)) {
    stop(sprintf("Could not create directory: %s", libPath))
  }
}
if (isLinux) {
  writeLines("", file.path(RLibPath, "INSTALLING"))
}
if (isMac &&
  !identical(
    paste0(R.version["major"][[1]], ".", strsplit(R.version["minor"][[1]], ".", fixed = TRUE)[[1]][1]),
    paste(strsplit(Rversion, ".", fixed = TRUE)[[1]][1:2], collapse = ".")
  )) {
  stop(
    sprintf(
      "R version used for building (%s) must be identical to R version in deployment bundle (%s)",
      paste0(R.version["major"][[1]], ".", strsplit(R.version["minor"][[1]], ".", fixed = TRUE)[[1]][1]),
      paste(strsplit(Rversion, ".", fixed = TRUE)[[1]][1:2], collapse = ".")
    )
  )
}
requiredPackages <- c(
  "withr", "pkgbuild", "remotes", "jsonlite", "V8",
  "zip", "tibble", "readr", "R6", "processx",
  "Rcpp", "futile.logger", "stringi", "xml2"
)
if (identical(Sys.getenv("BUILD_DOCKER"), "true")) {
  requiredPackages <- c(requiredPackages, "DBI", "blob")
}
if (isLinux) {
  requiredPackages <- c(requiredPackages, "askpass")
} else {
  requiredPackages <- c(requiredPackages, "testthat", "shinytest2")
}
installedPackagesDevel <- installed.packages(RlibPathDevel)
newPackages <- requiredPackages[!requiredPackages %in%
  installedPackagesDevel[, "Package"]]

# make sure correct version of packages is installed
devPkgVersionMap <- list(list("zip", c(2, 1)))
for (devPkgToInstall in devPkgVersionMap) {
  if (!devPkgToInstall[[1]] %in% newPackages) {
    pkgId <- match(devPkgToInstall[[1]], installedPackagesDevel[, "Package"])
    if (!is.na(pkgId)) {
      versionInstalled <- as.integer(strsplit(installedPackagesDevel[pkgId, "Version"], ".", fixed = TRUE)[[1]][c(1, 2)])
      if (versionInstalled[1] == devPkgToInstall[[2]][1] && versionInstalled[2] < devPkgToInstall[[2]][2]) {
        newPackages <- c(newPackages, devPkgToInstall[[1]])
      }
    }
  }
}

for (newPackage in newPackages) {
  if (identical(newPackage, "V8")) {
    Sys.setenv(DOWNLOAD_STATIC_LIBV8 = 1)
  }
  install.packages(newPackage,
    repos = CRANMirrors[1], lib = RlibPathDevel,
    dependencies = c("Depends", "Imports", "LinkingTo"),
    INSTALL_opts = "--no-multiarch"
  )
}

options(warn = 2)
.libPaths(c(RlibPathDevel, .libPaths()))

listOfLibs <- character(0L)
if (isLinux) {
  listOfLibs <- list.files(file.path("r", "library_src"))
}

packageIsInstalled <- function(package) {
  if (isLinux) {
    if (length(package) == 2L) {
      return(paste0(package[1], "_", package[2], ".tar.gz") %in% listOfLibs)
    }
    return(sum(grepl(paste0(package[1], "_"),
      listOfLibs,
      fixed = TRUE
    )) > 0L)
  }
  return(package[1] %in% installedPackages)
}

installStaticOpenSSL <- function(destDir) {
  openSSLVersion <- jsonlite::fromJSON("build-config.json")[["opensslVersion"]]
  tarDestFile <- file.path(tempdir(), "openssl.tar.gz")
  finalDestDir <- file.path(destDir, paste0("openssl-", openSSLVersion))
  Sys.setenv(OPENSSL_PKG_CONFIG_PATH = file.path(
    finalDestDir,
    "lib", "pkgconfig"
  ))
  if (dir.exists(finalDestDir)) {
    if (identical(Sys.getenv("BUILD_OPENSSL_FORCE_OVERWRITE"), "TRUE")) {
      if (unlink(finalDestDir, recursive = TRUE, force = TRUE) != 0) {
        stop(sprintf(
          "Something went wrong removing directory: %s.",
          finalDestDir
        ))
      }
    } else {
      print(sprintf("Skipping build of openssl as directory: %s already exists", finalDestDir))
      return()
    }
  } else if (!dir.create(finalDestDir, recursive = TRUE)) {
    stop(sprintf(
      "Something went wrong creating static openssl directory: %s.",
      finalDestDir
    ))
  }
  openSSLSourceURL <- paste0("https://www.openssl.org/source/openssl-", openSSLVersion, ".tar.gz")
  if (download.file(
    openSSLSourceURL,
    tarDestFile
  ) != 0) {
    stop(sprintf(
      "Something went downloading: %s.",
      openSSLSourceURL
    ))
  }
  proc <- processx::run("tar", c("-xvzf", basename(tarDestFile)),
    wd = dirname(tarDestFile),
    error_on_status = FALSE
  )
  if (proc$status != 0L) {
    stop(sprintf(
      "Something went wrong extracting: %s.\n\nStdout: %s\n\nStderr: %s",
      tarDestFile, proc$stdout, proc$stderr
    ))
  }
  proc <- processx::run("./config", c("no-shared", paste0("--prefix=", finalDestDir), paste0("--openssldir=", finalDestDir)),
    wd = file.path(dirname(tarDestFile), paste0("openssl-", openSSLVersion)),
    error_on_status = FALSE
  )
  if (proc$status != 0L) {
    stop(sprintf(
      "Something went configuring openssl.\n\nStdout: %s\n\nStderr: %s",
      proc$stdout, proc$stderr
    ))
  }
  proc <- processx::run("make",
    wd = file.path(dirname(tarDestFile), paste0("openssl-", openSSLVersion)),
    error_on_status = FALSE
  )
  if (proc$status != 0L) {
    stop(sprintf(
      "Something went building openssl.\n\nStdout: %s\n\nStderr: %s",
      proc$stdout, proc$stderr
    ))
  }
  proc <- processx::run("make", c("install"),
    wd = file.path(dirname(tarDestFile), paste0("openssl-", openSSLVersion)),
    error_on_status = FALSE
  )
  if (proc$status != 0L) {
    stop(sprintf(
      "Something went moving openssl to: %s. \n\nStdout: %s\n\nStderr: %s",
      finalDestDir, proc$stdout, proc$stderr
    ))
  }
  print(sprintf("Installed static openssl in directory: %s", finalDestDir))
}

dontDisplayMe <- lapply(c("pkgbuild", "remotes"), library, character.only = TRUE)

if (isLinux && !dir.exists(RlibPathSrc) &&
  !dir.create(RlibPathSrc, showWarnings = TRUE, recursive = TRUE)) {
  stop(sprintf("Could not create directory: %s", RlibPathSrc))
}

if (!dir.exists("./dist/dump") &&
  !dir.create("./dist/dump", showWarnings = TRUE, recursive = TRUE)) {
  stop("Could not create output directory: ./dist/dump")
}
if (dir.exists(file.path(".", "r-src", "build")) &&
  unlink(file.path(".", "r-src", "build"), recursive = TRUE, force = TRUE) != 0L) {
  stop("Could not remove old contents of '.', 'r-src', 'build'")
}
if (!dir.create(file.path(".", "r-src", "build"))) {
  stop("Could not create build directory: ./r-src/build")
}

installPackage <- function(package, attempt = 0) {
  if (attempt == 3L) {
    stop(sprintf("Problems installing package: %s", package[0]))
  }
  tryCatch(
    {
      if (isLinux) {
        downloadPackage(package)
      } else if (isMac && identical(package[1], "V8")) {
        # use binary from CRAN to avoid having absolute path to v8 dylib compiled into binary
        options(install.packages.check.source = "no")
        install.packages(package[1], if (CIBuild) RlibPathTmp else RLibPath,
          repos = CRANMirrors[attempt + 1],
          dependencies = FALSE, INSTALL_opts = "--no-multiarch"
        )
      } else if (isMac && identical(Sys.info()[["machine"]], "x86_64") && identical(package[1], "data.table")) {
        # use binary from CRAN to avoid issue with openMP (https://github.com/Rdatatable/data.table/issues/6622)
        options(install.packages.check.source = "no")
        install.packages(package[1], if (CIBuild) RlibPathTmp else RLibPath,
          repos = CRANMirrors[attempt + 1],
          dependencies = FALSE, INSTALL_opts = "--no-multiarch"
        )
      } else {
        # if ( isMac && identical(package[1], 'data.table') ) {
        #    makevarsPath <- '~/.R/Makevars'
        #    if ( file.exists(makevarsPath) ) {
        #        stop("Makevars already exist. Won't overwrite!")
        #    }
        #    on.exit(unlink(makevarsPath))
        #    if (!dir.exists(dirname(makevarsPath)) &&
        #        !dir.create(dirname(makevarsPath), showWarnings = TRUE, recursive = TRUE)){
        #        stop(sprintf('Could not create directory: %s', dirname(makevarsPath)))
        #    }
        #    writeLines(c('LLVM_LOC = /usr/local/opt/llvm',
        #        'CC=$(LLVM_LOC)/bin/clang -fopenmp',
        #       'CXX=$(LLVM_LOC)/bin/clang++ -fopenmp',
        #       '# -O3 should be faster than -O2 (default) level optimisation ..',
        #       'CFLAGS=-g -O3 -Wall -pedantic -std=gnu99 -mtune=native -pipe',
        #       'CXXFLAGS=-g -O3 -Wall -pedantic -std=c++11 -mtune=native -pipe',
        #       'LDFLAGS=-L/usr/local/opt/gettext/lib -L$(LLVM_LOC)/lib -Wl,-rpath,$(LLVM_LOC)/lib',
        #        'CPPFLAGS=-I/usr/local/opt/gettext/include -I$(LLVM_LOC)/include -I/Library/Developer/CommandLineTools/SDKs/MacOSX.sdk/usr/include'),
        #    makevarsPath)
        # }
        withr::with_libpaths(if (CIBuild) RlibPathTmp else RLibPath, install_version(package[1], package[2],
          out = "./dist/dump",
          dependencies = FALSE, repos = CRANMirrors[attempt + 1],
          INSTALL_opts = "--no-multiarch"
        ))
      }
    },
    error = function(e) {
      print(conditionMessage(e))
      installPackage(package, attempt + 1)
    }
  )
}
downloadPackage <- function(package) {
  packageFileNameTmp <- remotes::download_version(package[1], package[2],
    repos = CRANMirrors[1]
  )
  packageFileName <- file.path(
    RlibPathSrc,
    paste0(package[1], "_", package[2], ".tar.gz")
  )
  if (!suppressWarnings(file.rename(packageFileNameTmp, packageFileName))) {
    if (!file.copy(packageFileNameTmp, packageFileName, overwrite = TRUE)) {
      print("Source path info:")
      print(file.info(dirname(packageFileNameTmp)))
      print("Destination path info:")
      print(file.info(dirname(packageFileName)))
      stop(sprintf(
        "Problems renaming package: '%s' from '%s' to '%s'.",
        package[1], packageFileNameTmp, packageFileName
      ))
    }
    if (unlink(packageFileNameTmp) != 0) {
      print(sprintf("WARNING: Could not remove temporary file: %s", packageFileNameTmp))
    }
  }
}

if (CIBuild) {
  dirsInLibPath <- dir(RlibPathTmp)
  lockedLibs <- startsWith(dirsInLibPath, "00LOCK-")
  if (any(lockedLibs)) {
    print(paste0(
      "Locked libraries found. Will remove locks for these libraries: ",
      paste(dirsInLibPath[lockedLibs], collapse = ", ")
    ))
    unlink(file.path(RlibPathTmp, dirsInLibPath[lockedLibs]),
      force = TRUE, recursive = TRUE
    )
  }
}

if (isWindows) {
  pkgbuild::check_build_tools(debug = TRUE)
}

for (package in packageVersionMap) {
  if (packageIsInstalled(package)) {
    print(sprintf("Skipping '%s' as it is already installed.", package[1]))
    next
  }
  if (length(package) == 1L) {
    packagePath <- build(file.path(".", "r-src", package),
      dest_path = file.path(".", "r-src", "build/"),
      binary = FALSE, vignettes = FALSE, manual = FALSE,
      args = NULL, quiet = FALSE
    )
    if (isLinux && (!identical(package, "openssl") || identical(Sys.getenv("BUILD_DOCKER"), "true"))) {
      # we should include binary openssl linked against openssl3 in AppImage
      if (!file.rename(
        packagePath,
        file.path(RlibPathSrc, basename(packagePath))
      )) {
        if (!file.copy(packagePath, file.path(RlibPathSrc, basename(packagePath)), overwrite = TRUE)) {
          stop(sprintf(
            "Problems renaming file: '%s' to '%s'.",
            packagePath, file.path(RlibPathSrc, basename(packagePath))
          ))
        }
        if (unlink(packagePath) != 0) {
          print(sprintf("WARNING: Could not remove temporary file: %s", packagePath))
        }
      }
      next
    }
    CIMacOpenSSLBuild <- CIBuild && isMac && identical(package, "openssl")
    if (CIMacOpenSSLBuild) {
      installStaticOpenSSL(file.path(getwd(), RlibPathDevel, "openssl-static"))
    }
    install.packages(packagePath,
      lib = if (CIBuild && !isLinux) RlibPathTmp else RLibPath, repos = NULL,
      type = "source", dependencies = FALSE, INSTALL_opts = "--no-multiarch"
    )
    if (CIMacOpenSSLBuild) {
      rOpenSSLBinary <- file.path(RlibPathTmp, "openssl", "libs", "openssl.so")
      proc <- processx::run("otool", c(
        "-L",
        rOpenSSLBinary
      ),
      error_on_status = FALSE
      )
      if (proc$status != 0L) {
        stop(sprintf(
          "Something went wrong checking dependencies of: %s.\n\nStdout: %s\n\nStderr: %s",
          rOpenSSLBinary, proc$stdout, proc$stderr
        ))
      }
      if (grepl("libssl", proc$stdout, fixed = TRUE)) {
        stop(sprintf(
          "R OpenSSL binary: %s is still depent on openSSL shared library (something went wrong with static compilation).\n\nStdout: %s\n\nStderr: %s",
          rOpenSSLBinary, proc$stdout, proc$stderr
        ))
      }
    }
  } else {
    installPackage(package)
    if (CIBuild) {
      installedPackagesTmp <- c(installedPackagesTmp, package[1])
    }
  }
}
if (CIBuild && !isLinux) {
  # install packages to lib path devel and copy over
  print("Copying files from lib path devel to final destination")
  for (installedPackageTmp in c(installedPackagesTmp, customPackages)) {
    if (identical(installedPackageTmp, "openssl-static")) {
      next
    }
    if (any(!file.copy(file.path(RlibPathTmp, installedPackageTmp),
      RLibPath,
      overwrite = TRUE, recursive = TRUE
    ))) {
      print(sprintf(
        "Failed to copy: %s to: %s",
        file.path(RlibPathTmp, installedPackageTmp),
        RLibPath
      ))
      stop(sprintf(
        "Failed to copy: %s to: %s",
        file.path(RlibPathTmp, installedPackageTmp),
        RLibPath
      ), call. = FALSE)
    }
  }
}
# clean up unncecessary files
print("Cleaning up unnecessary files")
unlink(file.path(".", "r-src", "build/"), recursive = TRUE, force = TRUE)
dontDisplayMe <- lapply(
  list.dirs(RLibPath, full.names = TRUE, recursive = FALSE),
  function(x) {
    unlink(file.path(x, c(
      "help", "doc", "tests", "html",
      "include", "unitTests", file.path("inst", "examples"),
      file.path("libs", "*dSYM")
    )), force = TRUE, recursive = TRUE)
  }
)
if (isWindows) {
  unlink(file.path("r", c("doc", "tests", file.path("bin", "i386"))), force = TRUE, recursive = TRUE)
}
# replace directories with periods in their names with symlinks
# as directories with periods must be frameworks for codesign to not nag
if (isMac) {
  print("Fixing some directory names")
  currWd <- getwd()
  setwd(file.path(".", "r"))
  dirsWithPeriod <- list.dirs(file.path("."))
  dirsWithPeriod <- dirsWithPeriod[grepl(".*\\..*", basename(dirsWithPeriod), perl = TRUE)]
  dirsWithPeriod <- dirsWithPeriod[dirsWithPeriod != "."]
  dirsWithPeriod <- dirsWithPeriod[vapply(Sys.readlink(dirsWithPeriod), identical, logical(1L), USE.NAMES = FALSE, "")]
  dirsWithoutPeriod <- gsub(".", "", dirsWithPeriod, fixed = TRUE)
  if (length(dirsWithoutPeriod)) {
    dirsWithoutPeriod <- paste0(".", dirsWithoutPeriod)
  }

  if (!all(file.rename(
    file.path(
      dirname(dirsWithoutPeriod),
      basename(dirsWithPeriod)
    ),
    dirsWithoutPeriod
  ))) {
    stop("Some directories could not be renamed!")
  }
  currWorkDir <- getwd()
  dirsWithoutPeriod <- file.path(currWorkDir, dirsWithoutPeriod)
  dirsWithoutPeriodBase <- basename(dirsWithoutPeriod)
  dirsWithPeriod <- basename(dirsWithPeriod)
  tryCatch(
    {
      for (i in seq_along(dirsWithPeriod)) {
        setwd(dirname(dirsWithoutPeriod[i]))
        file.symlink(
          dirsWithoutPeriodBase[i],
          dirsWithPeriod[i]
        )
      }
    },
    error = function(e) {
      stop(sprintf(
        "Problems creating symlinks!! Error message: %s",
        conditionMessage(e)
      ))
    },
    finally = {
      setwd(currWorkDir)
    }
  )
  setwd(file.path(".", "fontconfig", "fonts", "confd"))
  # fix some symlinks that are hardlinked to /Library/Frameworks/R.frameworks
  print("Fixing some symbolic links")
  filesWithBadLink <- list.files(".")
  filesWithBadLink <- filesWithBadLink[filesWithBadLink != "README"]
  for (fileWithBadLink in filesWithBadLink) {
    unlink(fileWithBadLink, force = TRUE)
    file.symlink(
      file.path(
        "..", "..", "fontconfig",
        "conf.avail", fileWithBadLink
      ),
      fileWithBadLink
    )
  }
  setwd(currWd)
}

# update commit hash in about dialog
print(paste0("Setting hash in about dialog to: ", Sys.getenv("CI_COMMIT_SHORT_SHA", "__HASH__")))
aboutDialog <- readLines("./renderer/about.js", warn = FALSE)
aboutDialog <- gsub("__HASH__",
  Sys.getenv("CI_COMMIT_SHORT_SHA", "__HASH__"), aboutDialog,
  fixed = TRUE
)
writeLines(aboutDialog, "./renderer/about.js")

# build MIRO example apps
if (!identical(Sys.getenv("BUILD_DOCKER"), "true")) {
  examplesPath <- file.path(getwd(), "src", "examples")
  if (dir.exists(examplesPath)) {
    unlink(examplesPath, force = TRUE, recursive = TRUE)
  }
  if (length(RlibPathDevel)) {
    Sys.setenv(R_LIBS = file.path(getwd(), RlibPathDevel))
  }
  print(sessionInfo())
  Sys.setenv(MIRO_BUILD = "true")
  for (modelName in c(
    "pickstock", "transport", "sudoku", "tsp", "farming",
    "inscribedsquare", "cpack", "cutstock"
  )) {
    print(sprintf("Building example app: %s", modelName))
    Sys.setenv(MIRO_MODE = "base")
    if (!dir.exists(file.path(examplesPath, modelName)) &&
      !dir.create(file.path(examplesPath, modelName), recursive = TRUE)) {
      stop(sprintf("Could not create path: %s", examplesPath))
    }
    modelPath <- file.path(
      getwd(), "src", "model",
      modelName
    )
    miroAppPath <- file.path(modelPath, paste0(modelName, ".miroapp"))

    Sys.setenv(MIRO_MODEL_PATH = file.path(modelPath, paste0(modelName, ".gms")))

    buildProc <- processx::run(file.path(R.home(), "bin", "Rscript"),
      c("--vanilla", "./app.R"),
      error_on_status = FALSE,
      wd = file.path(getwd(), "src")
    )
    if (buildProc$status != 0L) {
      stop(sprintf(
        "Something went wrong while creating MIRO app for model: %s.\n\nStdout: %s\n\nStderr: %s",
        modelName, buildProc$stdout, buildProc$stderr
      ))
    }
    zip::unzip(miroAppPath, exdir = file.path(examplesPath, modelName))
  }
}

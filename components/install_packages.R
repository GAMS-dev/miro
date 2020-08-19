installAndRequirePackages <- function(requiredPackages, installedPackages, RLibPath, CRANMirror, miroWorkspace, installMIROPackages = FALSE,
                                      attachPackages = TRUE){
  newPackages <- requiredPackages[!requiredPackages %in% installedPackages]
  errMsg <- NULL
  if(length(newPackages)){
    checkSourceDefault <- getOption("install.packages.check.source")
    options(install.packages.check.source = "no")
    
    if(installMIROPackages){
      packageVersionMap <- read.csv("miro-pkg-lock.csv",
                                    header = FALSE, stringsAsFactors = FALSE)
      packageVersionMap <- lapply(seq_len(nrow(packageVersionMap)), function(pkgIdx){
        pkgInfo <- trimws(as.character(packageVersionMap[pkgIdx, ]))
        if(identical(pkgInfo[2], "")){
          return(pkgInfo[1])
        }
        return(pkgInfo)
      })
      installedPackagesTmp <- installed.packages()
      # install packages to lib path devel and copy over
      installedPackagesTmp <- packageVersionMap[vapply(packageVersionMap, function(packageVersion){
        packageId <- match(packageVersion[1], installedPackagesTmp[, "Package"])
        !is.na(packageId) &&
          (length(packageVersion) == 1L ||
             identical(packageVersion[2], installedPackagesTmp[packageId, "Version"]))
      }, logical(1), USE.NAMES = FALSE)]
      installedPackagesTmp <- vapply(installedPackagesTmp, "[[",
                                     character(1), 1, USE.NAMES = FALSE)
      buildDepInstalled <- "remotes" %in% installedPackages
      print("Installing R packages. This might take more than an hour...")
      for(package in packageVersionMap){
        if (package[1] %in% installedPackagesTmp){
          next
        }
        if(!buildDepInstalled){
          print("Installing: remotes")
          install.packages("remotes", lib = if(length(RLibPath)) RLibPath else .libPaths()[[1]], 
                           repos = CRANMirror, dependencies = c("Depends", "Imports", "LinkingTo"))
          buildDepInstalled <- TRUE
        }
        print(paste0("Installing: ", package[1]))
        if(length(package) == 1L){
          if(package %in% c("DBI", "chartjs")){
            # those are not submodules
            remotes::install_github(paste0("GAMS-dev/miro_desktop/r-src/", package),
                                    dependencies = FALSE,
                                    INSTALL_opts = '--no-multiarch')
          }else{
            remotes::install_github(paste0("GAMS-dev/", if(identical(package, "gdxrrwMIRO")) "gdxrrw-miro" else package),
                                    dependencies = FALSE,
                                    INSTALL_opts = '--no-multiarch')
          }
        }else{
          remotes::install_version(package[1], package[2],
                                   dependencies = FALSE,
                                   INSTALL_opts = '--no-multiarch')
        }
      }
      rm(installedPackagesTmp, packageVersionMap, buildDepInstalled)
    }else{
      customLibPath <- NULL
      customLibPath <- file.path(miroWorkspace, "custom_packages")
      if(!dir.exists(customLibPath) &&
         !dir.create(customLibPath, recursive = TRUE)){
        warning(sprintf("Problems creating directory for custom packages: '%s'. Do you miss write permissions?",
                        customLibPath))
      }else{
        for(customPackage in newPackages){
          print(paste0("Installing: ", customPackage))
          install.packages(customPackage, lib = customLibPath, 
                           repos = CRANMirror, dependencies = c("Depends", "Imports", "LinkingTo"))
        }
      }
      rm(customPackages, customLibPath)
    }

    options(install.packages.check.source = checkSourceDefault)
  }
  if(attachPackages){
    tryCatch({
      suppressWarnings(suppressMessages(lapply(requiredPackages, library, character.only = TRUE, 
                                               quietly = TRUE, verbose = FALSE, warn.conflicts = FALSE, lib.loc = RLibPath)))
      
    }, error = function(e){
      if(exists("flog.fatal")){
        flog.fatal("Problems loading required R packages. Error message: %s.", conditionMessage(e))
      }
      errMsg <<- paste0("Not all the required packages are installed. Error message: ", conditionMessage(e))
    })
  }
  return(errMsg)
}

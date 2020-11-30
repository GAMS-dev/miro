Rversion <- "4.0.2"
CRANMirrors <- c('https://cloud.r-project.org/',
    'https://ftp.fau.de/cran/',
    'https://stat.ethz.ch/CRAN/')

RLibPath <- Sys.getenv('LIB_PATH')
packageVersionMap <- list(c("data.table", "1.12.2"), c("backports", "1.1.9"), c("assertthat", 
"0.2.1"), c("crayon", "1.3.4"), c("glue", "1.4.2"), c("fansi", 
"0.4.1"), c("cli", "2.0.2"), c("utf8", "1.1.4"), c("Rcpp", "1.0.5"
), c("R6", "2.4.1"), c("BH", "1.72.0-3"), c("magrittr", "1.5"
), c("rlang", "0.4.7"), c("later", "1.1.0.1"), c("promises", 
"1.1.1"), c("httpuv", "1.5.4"), c("mime", "0.9"), "jsonlite", 
    c("digest", "0.6.25"), c("sourcetools", "0.1.7"), c("xtable", 
    "1.8-4"), c("fastmap", "1.0.1"), c("curl", "4.3"), c("V8", 
    "3.2.0"), c("base64enc", "0.1-3"), c("htmltools", "0.5.0"
    ), c("withr", "2.2.0"), c("leaflet.providers", "1.9.0"), 
    c("commonmark", "1.7"), "shiny", c("colorspace", "1.4-1"), 
    c("purrr", "0.3.4"), c("yaml", "2.2.1"), c("labeling", "0.3"
    ), c("munsell", "0.5.0"), c("lazyeval", "0.2.2"), c("pkgconfig", 
    "2.0.3"), c("ellipsis", "0.3.1"), c("vctrs", "0.3.4"), c("tidyselect", 
    "1.1.0"), c("plogr", "0.2.0"), c("htmlwidgets", "1.5.1"), 
    c("png", "0.1-7"), c("RColorBrewer", "1.1-2"), c("lattice", 
    "0.20-41"), c("sp", "1.4-2"), c("viridisLite", "0.3.0"), 
    c("raster", "3.3-13"), c("farver", "2.0.3"), c("lifecycle", 
    "0.2.0"), c("scales", "1.1.1"), c("zeallot", "0.1.0"), "crosstalk", 
    "DT", "gdxrrwMIRO", "leaflet", c("pillar", "1.4.6"), c("tibble", 
    "3.0.3"), c("dplyr", "0.8.5"), c("sys", "3.4"), c("askpass", 
    "1.1"), c("prettyunits", "1.1.1"), c("stringi", "1.4.6"), 
    "DBI", c("blob", "1.2.1"), c("hms", "0.5.3"), c("cpp11", 
    "0.2.1"), c("tidyr", "1.1.2"), c("memoise", "1.1.0"), "httr", 
    "plotly", "shinydashboard", "timevis", c("rematch", "1.0.1"
    ), c("formatR", "1.7"), c("ps", "1.3.4"), c("clipr", "0.7.0"
    ), c("cellranger", "1.1.0"), c("progress", "1.2.2"), c("lambda.r", 
    "1.2.4"), c("futile.options", "1.0.1"), c("zoo", "1.8-8"), 
    c("globals", "0.12.5"), c("listenv", "0.8.0"), c("processx", 
    "3.4.4"), c("readr", "1.3.1"), c("readxl", "1.3.1"), c("writexl", 
    "1.3.1"), c("rpivotTable", "0.3.0"), c("futile.logger", "1.4.3"
    ), c("zip", "2.1.1"), c("leaflet.minicharts", "0.6.0"), c("xts", 
    "0.12-0"), c("dygraphs", "1.1.1.6"), c("future", "1.18.0"
    ), "miro.util", "rhandsontable", "sortable", "chartjs", "RSQLite")

isMac <- Sys.info()['sysname'] == 'Darwin' || grepl("^darwin", R.version$os)
isWindows <- .Platform$OS.type == 'windows'
isLinux <- grepl("linux-gnu", R.version$os)

if ( identical(Sys.getenv('BUILD_DOCKER'), 'true') ) {
    if ( identical(RLibPath, "") ) {
        RLibPath <- NULL
    }
    isMac     <- FALSE
    isWindows <- FALSE
    isLinux   <- TRUE
    packageVersionMap <- c(packageVersionMap[!vapply(packageVersionMap, function(package){
        identical(package[1], 'RSQLite')}, logical(1L), USE.NAMES = FALSE)], list("RPostgres"))
} else {
    if ( identical(RLibPath, '') ) {
        stop("Library path not specified. Use environment variable LIB_PATH to specify where libraries should be installed.", 
            call. = FALSE)
    }
}

# on Jenkins use default library
RlibPathDevel <- NULL
CIBuild <- TRUE
if(identical(Sys.getenv("BUILD_NUMBER"), "")){
    RlibPathDevel <-  './build/lib_devel'
    CIBuild <- FALSE
} else if(isWindows) {
    # on Windows, we use the R version we ship, so we need to set library path explicitly, or
    # it will install development libraries inside ./r/library
    RlibPathDevel <- paste0('~/R/win-library/', R.version[['major']], ".",
        strsplit(R.version[['minor']], '.', fixed = TRUE)[[1]][1])
}
RlibPathSrc <- file.path('.', 'r', 'library_src')

RlibPathTmp <- NULL
if(CIBuild){
    RlibPathTmp <- file.path(.libPaths()[1], "miro_lib")
}
installedPackagesTmp <- installed.packages(RlibPathTmp)
# install packages to lib path devel and copy over
installedPackagesTmp <- packageVersionMap[vapply(packageVersionMap, function(packageVersion){
    packageId <- match(packageVersion[1], installedPackagesTmp[, "Package"])
    !is.na(packageId) &&
    identical(packageVersion[2], installedPackagesTmp[packageId, "Version"]) &&
    identical(Rversion, installedPackagesTmp[packageId, "Built"])
}, logical(1), USE.NAMES = FALSE)]
installedPackagesTmp <- vapply(installedPackagesTmp, "[[",
    character(1), 1, USE.NAMES = FALSE)

installedPackages <- installed.packages(RLibPath)[, "Package"]

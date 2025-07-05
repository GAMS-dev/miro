Rversion <- "4.0.4"
CRANMirrors <- c(
  "https://cran.datenrettung360.de/",
  "https://cloud.r-project.org/",
  "https://stat.ethz.ch/CRAN/"
)

RLibPath <- Sys.getenv("LIB_PATH")
packageVersionMap <- list(
  c("data.table", "1.14.8"), c("cpp11", "0.4.5"), c(
    "backports",
    "1.4.1"
  ), c("assertthat", "0.2.1"), c("crayon", "1.5.2"), c(
    "glue",
    "1.6.2"
  ), c("fansi", "1.0.4"), c("cli", "3.6.1"), c("utf8", "1.2.3"), c("Rcpp", "1.0.11"), c("R6", "2.5.1"), c("BH", "1.81.0-1"),
  c("magrittr", "2.0.3"), c("rlang", "1.1.1"), c(
    "ellipsis",
    "0.3.2"
  ), c("later", "1.3.1"), c("promises", "1.2.0.1"),
  c("httpuv", "1.6.11"), c("mime", "0.12"), "jsonlite", c(
    "digest",
    "0.6.33"
  ), c("sourcetools", "0.1.7-1"), c("xtable", "1.8-4"), c("fastmap", "1.1.1"), c("curl", "5.0.1"), c("V8", "4.3.3"), c("base64enc", "0.1-3"), c("htmltools", "0.5.5"), c(
    "jquerylib",
    "0.1.4"
  ), c("withr", "2.5.0"), c("leaflet.providers", "1.9.0"), c("commonmark", "1.9.0"), c("cachem", "1.0.8"), c(
    "memoise",
    "2.0.1"
  ), c("fontawesome", "0.5.1"), c("lifecycle", "1.0.3"), c("vctrs", "0.6.3"), c("fs", "1.6.3"), c("rappdirs", "0.3.3"), c("sass", "0.4.7"), c("bslib", "0.5.0"), "shiny", c(
    "shinyAce",
    "0.4.2"
  ), c("colorspace", "2.1-0"), c("purrr", "1.0.1"),
  c("yaml", "2.3.7"), c("labeling", "0.4.2"), c(
    "munsell",
    "0.5.0"
  ), c("lazyeval", "0.2.2"), c("pkgconfig", "2.0.3"),
  c("tidyselect", "1.2.0"), c("plogr", "0.2.0"), c(
    "htmlwidgets",
    "1.5.4"
  ), c("png", "0.1-8"), c("RColorBrewer", "1.1-3"),
  c("lattice", "0.21-8"), c("sp", "2.0-0"), c(
    "viridisLite",
    "0.4.2"
  ), c("raster", "3.4-13"), c("farver", "2.1.0"), c(
    "scales",
    "1.2.1"
  ), c("zeallot", "0.1.0"), c("crosstalk", "1.2.0"),
  "DT", "gdxrrwMIRO", "leaflet", c("pillar", "1.9.0"), c(
    "tibble",
    "3.2.1"
  ), c("generics", "0.1.3"), c("timechange", "0.2.0"), c("lubridate", "1.9.2"), c("dplyr", "1.1.2"), c(
    "sys",
    "3.4.2"
  ), c("askpass", "1.1"), "openssl", c(
    "prettyunits",
    "1.1.1"
  ), c("stringi", "1.7.12"), c("DBI", "1.1.3"), c(
    "blob",
    "1.2.4"
  ), c("hms", "1.1.3"), c("stringr", "1.5.0"), c(
    "tidyr",
    "1.3.0"
  ), c("httr", "1.4.6"), "plotly", "shinydashboard",
  "timevis", c("rematch", "1.0.1"), c("formatR", "1.14"), c(
    "ps",
    "1.7.5"
  ), c("clipr", "0.8.0"), c("cellranger", "1.1.0"),
  c("progress", "1.2.2"), c("lambda.r", "1.2.4"), c(
    "futile.options",
    "1.0.1"
  ), c("zoo", "1.8-12"), c("globals", "0.16.2"), c(
    "listenv",
    "0.9.0"
  ), c("processx", "3.8.2"), c("readxl", "1.4.3"), c(
    "writexl",
    "1.4.2"
  ), c("futile.logger", "1.4.3"), c("zip", "2.3.0"), c("leaflet.minicharts", "0.6.2"), c(
    "xts",
    "0.13.1"
  ), c("dygraphs", "1.1.1.6"), c("parallelly", "1.36.0"), c("future", "1.33.0"), c("bit", "4.0.5"), c("bit64", "4.0.5"), c("tzdb", "0.4.0"), c("vroom", "1.6.3"), c("readr", "2.1.4"), "miroUtil", "rhandsontable", "sortable", "chartjs", c(
    "RSQLite",
    "2.3.1"
  )
)

isMac <- Sys.info()["sysname"] == "Darwin" || grepl("^darwin", R.version$os)
isWindows <- .Platform$OS.type == "windows"
isLinux <- grepl("linux-gnu", R.version$os)

if (identical(Sys.getenv("BUILD_DOCKER"), "true")) {
  if (identical(RLibPath, "")) {
    RLibPath <- NULL
  }
  isMac <- FALSE
  isWindows <- FALSE
  isLinux <- TRUE
  packageVersionMap <- c(packageVersionMap[!vapply(packageVersionMap, function(package) {
    identical(package[1], "RSQLite")
  }, logical(1L), USE.NAMES = FALSE)], list(c("RPostgres", "1.4.8")))
} else {
  if (identical(RLibPath, "")) {
    stop("Library path not specified. Use environment variable LIB_PATH to specify where libraries should be installed.",
      call. = FALSE
    )
  }
}

# on GitLab use default library
RlibPathDevel <- "./build/lib_devel"
CIBuild <- !identical(Sys.getenv("CI"), "")
RlibPathSrc <- file.path(".", "r", "library_src")

RlibPathTmp <- NULL
if (CIBuild) {
  RlibPathTmp <- file.path(RlibPathDevel, "miro_lib")
  buildConfigContent <- strsplit(paste(readLines("build-config.json"), collapse = " "), '"', fixed = TRUE)[[1]]
  Rversion <- buildConfigContent[which(buildConfigContent == "rVersion") + 2]
}
installedPackagesTmp <- installed.packages(RlibPathTmp)
# install packages to lib path devel and copy over
installedPackagesTmp <- packageVersionMap[vapply(packageVersionMap, function(packageVersion) {
  packageId <- match(packageVersion[1], installedPackagesTmp[, "Package"])
  !is.na(packageId) &&
    identical(packageVersion[2], installedPackagesTmp[packageId, "Version"]) &&
    identical(Rversion, installedPackagesTmp[packageId, "Built"])
}, logical(1), USE.NAMES = FALSE)]
installedPackagesTmp <- vapply(installedPackagesTmp, "[[",
  character(1), 1,
  USE.NAMES = FALSE
)

installedPackages <- installed.packages(RLibPath)[, "Package"]

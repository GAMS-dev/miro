Rversion <- "4.0.4"
CRANMirrors <- c(
  "https://ftp.fau.de/cran/",
  "https://cloud.r-project.org/",
  "https://stat.ethz.ch/CRAN/"
)

RLibPath <- Sys.getenv("LIB_PATH")
packageVersionMap <- list(c("data.table", "1.14.2"), c("cpp11", "0.4.2"), c("backports",
"1.4.1"), c("assertthat", "0.2.1"), c("crayon", "1.5.1"), c("glue",
"1.6.2"), c("fansi", "1.0.3"), c("cli", "3.3.0"), c("utf8", "1.2.2"
), c("Rcpp", "1.0.8.3"), c("R6", "2.5.1"), c("BH", "1.78.0-0"
), c("magrittr", "2.0.3"), c("rlang", "1.0.2"), c("later", "1.3.0"
), c("promises", "1.2.0.1"), c("httpuv", "1.6.5"), c("mime",
"0.12"), "jsonlite", c("digest", "0.6.29"), c("sourcetools",
"0.1.7"), c("xtable", "1.8-4"), c("fastmap", "1.1.0"), c("curl",
"4.3.2"), c("V8", "4.2.0"), c("base64enc", "0.1-3"), c("htmltools",
"0.5.2"), c("jquerylib", "0.1.4"), c("withr", "2.5.0"), c("leaflet.providers",
"1.9.0"), c("commonmark", "1.8"), c("cachem", "1.0.6"), c("fontawesome",
"0.2.2"), c("ellipsis", "0.3.2"), c("lifecycle", "1.0.1"), c("jquerylib",
"0.1.4"), c("fs", "1.5.2"), c("rappdirs", "0.3.3"), c("sass",
"0.4.1"), c("bslib", "0.3.1"), "shiny", c("shinyAce", "0.4.2"
), c("colorspace", "2.0-3"), c("purrr", "0.3.4"), c("yaml", "2.3.5"
), c("labeling", "0.4.2"), c("munsell", "0.5.0"), c("lazyeval",
"0.2.2"), c("pkgconfig", "2.0.3"), c("vctrs", "0.4.1"), c("tidyselect",
"1.1.2"), c("plogr", "0.2.0"), c("htmlwidgets", "1.5.4"), c("png",
"0.1-7"), c("RColorBrewer", "1.1-3"), c("lattice", "0.20-45"),
    c("sp", "1.5-0"), c("viridisLite", "0.4.0"), c("raster",
    "3.4-13"), c("farver", "2.1.0"), c("scales", "1.2.0"), c("zeallot",
    "0.1.0"), c("crosstalk", "1.2.0"), "DT", "gdxrrwMIRO", "leaflet",
    c("pillar", "1.7.0"), c("tibble", "3.1.7"), c("generics",
    "0.1.2"), c("lubridate", "1.8.0"), c("dplyr", "1.0.9"), c("sys",
    "3.4"), c("askpass", "1.1"), "openssl", c("prettyunits",
    "1.1.1"), c("stringi", "1.7.6"), c("DBI", "1.1.2"), c("blob",
    "1.2.3"), c("hms", "1.1.1"), c("tidyr", "1.2.0"), c("memoise",
    "2.0.1"), c("httr", "1.4.3"), "plotly", "shinydashboard",
    "timevis", c("rematch", "1.0.1"), c("formatR", "1.12"), c("ps",
    "1.7.0"), c("clipr", "0.8.0"), c("cellranger", "1.1.0"),
    c("progress", "1.2.2"), c("lambda.r", "1.2.4"), c("futile.options",
    "1.0.1"), c("zoo", "1.8-10"), c("globals", "0.15.0"), c("listenv",
    "0.8.0"), c("processx", "3.5.3"), c("readxl", "1.4.0"), c("writexl",
    "1.4.0"), c("rpivotTable", "0.3.0"), c("futile.logger", "1.4.3"
    ), c("zip", "2.2.0"), c("leaflet.minicharts", "0.6.2"), c("xts",
    "0.12.1"), c("dygraphs", "1.1.1.6"), c("parallelly", "1.31.1"
    ), c("future", "1.26.1"), c("bit", "4.0.4"), c("bit64", "4.0.5"
    ), c("tzdb", "0.3.0"), c("vroom", "1.5.7"), c("readr", "2.1.2"
    ), "miroUtil", "rhandsontable", "sortable", "chartjs", c("RSQLite",
    "2.2.14"))

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
  }, logical(1L), USE.NAMES = FALSE)], list(c("RPostgres", "1.4.4")))
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
  buildConfigContent <- strsplit(readLines("build-config.json"), '"', fixed = TRUE)[[1]]
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

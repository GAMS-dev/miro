licenseContent <- readLines("./src/LICENSE", encoding = "UTF-8")
packages <- read.csv("./src/miro-pkg-lock.csv", stringsAsFactors = FALSE)[[1]]
missingLicenses <- vapply(packages, function(package) {
  if (any(grepl(paste0(" ", package, ","), licenseContent, fixed = TRUE) & grepl("http", licenseContent, fixed = TRUE)) ||
    any(grepl(paste0(" ", package, " "), licenseContent, fixed = TRUE) & grepl("http", licenseContent, fixed = TRUE)) ||
    package %in% c("miro.util")) {
    return(FALSE)
  }
  write(sprintf("License for package: %s missing!", package), stderr())
  return(TRUE)
}, logical(1L), USE.NAMES = FALSE)
if (any(missingLicenses)) {
  quit("no", status = 1L)
}
quit("no", status = 0L)

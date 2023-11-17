library(oysteR)
library(jsonlite)

packageVersionMap <- read.csv("./src/miro-pkg-lock.csv", header = FALSE, stringsAsFactors = FALSE)
packages <- trimws(packageVersionMap[[1]])
versions <- trimws(packageVersionMap[[2]])
versions[versions == ""] <- "*"
auditReport <- audit("haven", "0.2.0", "cran")

vulnerabilities <- get_vulnerabilities(auditReport)
print(vulnerabilities)
write_json(list(vuln_data = vulnerabilities),
  "r-audit-report.json",
  pretty = TRUE, auto_unbox = TRUE, null = "null"
)

# replace MIRO API version, MIRO version and MIRO release date in main.js and package.json with the one set in src/app.R
eval(parse(text = readLines("./src/app.R",
  n = 5L, warn = FALSE
)))
globalsJS <- readLines("./components/globals.js", warn = FALSE)
globalsJS <- gsub(
  "const apiVersion = \\d+;",
  paste0("const apiVersion = ", APIVersion, ";"), globalsJS
)
globalsJS <- gsub(
  "const miroVersion = '[^']+';",
  paste0("const miroVersion = '", MIROVersion, "';"), globalsJS
)
globalsJS <- gsub(
  "const miroRelease = '[^']+';",
  paste0("const miroRelease = '", MIRORDate, "';"), globalsJS
)
writeLines(globalsJS, "./components/globals.js")
packageJSON <- readLines("./package.json", warn = FALSE)
packageJSON <- gsub(
  '"version": "[^"]+",',
  paste0('"version": "', MIROVersion, '",'), packageJSON
)
writeLines(packageJSON, "./package.json")
adminConfig <- readLines("./server/admin/global.R", warn = FALSE)
adminConfig <- gsub(
  'MIRO_VERSION[[:space:]]*<-[[:space:]]*"[^"]+"',
  paste0('MIRO_VERSION <- "', MIROVersion, '"'), adminConfig
)
adminConfig <- gsub(
  "REQUIRED_API_VERSION[[:space:]]*<-.*",
  paste0("REQUIRED_API_VERSION <- ", APIVersion), adminConfig
)
writeLines(adminConfig, "./server/admin/global.R")
dockerImageMiro <- readLines("./server/ui/Dockerfile", warn = FALSE)
dockerImageMiro <- gsub(
  'com\\.gamsmiro\\.version="[^"]+"',
  paste0('com.gamsmiro.version="', MIROVersion, '"'), dockerImageMiro
)
writeLines(dockerImageMiro, "./server/ui/Dockerfile")
dockerImageAdmin <- readLines("./server/admin/Dockerfile", warn = FALSE)
dockerImageAdmin <- gsub(
  'com\\.gamsmiroadmin\\.version="[^"]+"',
  paste0('com.gamsmiroadmin.version="', MIROVersion, '"'), dockerImageAdmin
)
writeLines(dockerImageAdmin, "./server/admin/Dockerfile")
dockerImageAuth <- readLines("./server/auth/Dockerfile", warn = FALSE)
dockerImageAuth <- gsub(
  'com\\.gamsmiroauth\\.version="[^"]+"',
  paste0('com.gamsmiroauth.version="', MIROVersion, '"'), dockerImageAuth
)
writeLines(dockerImageAuth, "./server/auth/Dockerfile")
helmChartYaml <- readLines("./server/kubernetes/gams-miro-server/Chart.yaml", warn = FALSE)
helmChartYaml <- gsub(
  'appVersion:[[:space:]]*"[^"]+"',
  paste0('appVersion: "', MIROVersion, '"'), helmChartYaml
)
writeLines(helmChartYaml, "./server/kubernetes/gams-miro-server/Chart.yaml")

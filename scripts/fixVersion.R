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
helmChartFilePath <- "./server/kubernetes/gams-miro-server/Chart.yaml"
helmChartYaml <- readLines(helmChartFilePath, warn = FALSE)
appVerIndex <- grep("^appVersion:", helmChartYaml)

if (length(appVerIndex) > 0) {
  currentLine <- helmChartYaml[appVerIndex]
  currentAppVer <- sub(".*'([^']+)'.*", "\\1", currentLine)

  if (currentAppVer != MIROVersion) {
    message(paste("Updating appVersion from", currentAppVer, "to", MIROVersion))

    helmChartYaml[appVerIndex] <- paste0("appVersion: '", MIROVersion, "'")

    vIndex <- grep("^version:[[:space:]]*", helmChartYaml)
    if (length(vIndex) > 0) {
      verLine <- helmChartYaml[vIndex]
      cleanVer <- sub("^version:[[:space:]]*", "", verLine)
      cleanVer <- gsub("['\"]", "", cleanVer)
      parts <- as.numeric(strsplit(cleanVer, ".", fixed = TRUE)[[1]])
      if (length(parts) >= 2) {
        parts[2] <- parts[2] + 1
        if (length(parts) >= 3) {
          parts[3] <- 0
        }
        newVer <- paste(parts, collapse = ".")
        helmChartYaml[vIndex] <- paste0("version: ", newVer)
        message(paste("Chart version bumped to:", newVer))
      } else {
        warning("Invalid semantic chart version in Chart.yaml")
      }
    } else {
      warning("Could not find 'version' line in Chart.yaml")
    }
    writeLines(helmChartYaml, helmChartFilePath)
  } else {
    message("MIROVersion matches current appVersion. No changes made.")
  }
} else {
  warning("Could not find 'appVersion' line in Chart.yaml")
}

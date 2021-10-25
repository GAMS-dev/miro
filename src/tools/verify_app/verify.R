#!/usr/bin/env Rscript

# return codes:
# 0: app is signed with one of the public keys provided
# 1: unexpected error
# 2: app is signed, but the signature could not be verified
# 3: app is not signed

args <- commandArgs(trailingOnly = TRUE)

printUsage <- function() {
  write("USE: Rscript --vanilla verify.R -m miro_app_path -p public_key_1 -p public_key2", stderr())
  quit("no", 1L, runLast = FALSE)
}

if (length(args) < 2L) {
  printUsage()
}

miroAppArgId <- which(tolower(args) == "-m") + 1L
publicKeyArgIds <- which(tolower(args) == "-p") + 1L

if (!identical(length(miroAppArgId), 1L)) {
  printUsage()
}

miroAppPath <- args[miroAppArgId]
pubKeyPaths <- args[publicKeyArgIds]

tryCatch(
  {
    source("./components/sign_app.R")
    pubKeyPaths <- c(pubKeyPaths, GAMS_PUBLIC_KEY)
    if (!appIsSigned(miroAppPath, isExtracted = TRUE)) {
      write("App is not signed!", stderr())
      quit("no", 3L, runLast = FALSE)
    }
    isValid <- verifyAppSignature(miroAppPath, pubKeyPaths = pubKeyPaths)
    if (!identical(isValid, TRUE)) {
      write("Signature could not be verified.", stderr())
      quit("no", 2L, runLast = FALSE)
    } else {
      quit("no", 0L, runLast = FALSE)
    }
  },
  error = function(e) {
    write(sprintf(
      "Signature could not be verified. Error message: %s",
      conditionMessage(e)
    ), stderr())
    quit("no", 1L, runLast = FALSE)
  }
)

quit("no", 1L, runLast = FALSE)

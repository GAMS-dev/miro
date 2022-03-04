GAMS_PUBLIC_KEY <- "-----BEGIN PUBLIC KEY-----\nMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEAhJ/jcDaq1LD+/h5O49fU\n1UHNyTS/OxDYcdCHKaiG+HXiv9gBBjwCbTBVTWlZcn+zbtL9oKm4bSdzvk0Ao2NZ\n2stMgvzGLw02af/eNOWqu7i4CNh75pHDuB1oOz4JHgbt6630LiEPKPQ51ihTeXoW\nI9FY8TuXP7EuhvPJMs8PXRTSQT9fABf0bzNxSr1OEAXaqrklJkjU9QiUab8HK1oj\n5NriS+dPF2tiwQ1GLXs9CWmRX03j3Bgisuc+FV7HOiboP2+ZqMzXp5t9sV88Am3L\nPzY/UYR1aWYdsjGaT+wNTI/5ide78UZWwgzUPItw/lpXydiQSaFx2dYvkxHtBJI/\nfwIDAQAB\n-----END PUBLIC KEY-----\n"

genKeyFingerprint <- function(pubKey) {
  trimws(openssl::base64_encode(
    openssl::fingerprint(
      openssl::read_pubkey(pubKey),
      openssl::sha256
    )
  ), "right", "=")
}

trySignAppBundle <- function(miroAppFiles, privKeyPath, passFilePath = "") {
  if (!file.exists(privKeyPath)) {
    stop("Could not find private key", call. = FALSE)
  }
  if (!identical(passFilePath, "")) {
    keyPasswords <- tryCatch(suppressWarnings(readLines(passFilePath, encoding = "UTF-8")), error = function(e) {
      return(character(0L))
    })
    privKey <- NULL
    for (keyPassword in keyPasswords) {
      if (tryCatch(
        {
          privKey <- openssl::read_key(privKeyPath, password = keyPassword)
          TRUE
        },
        error = function(e) {
          return(FALSE)
        }
      )) {
        break
      }
    }
    if (is.null(privKey)) {
      stop("Could not read private key.", call. = FALSE)
    }
  } else {
    privKey <- openssl::read_key(privKeyPath)
  }
  if (!identical(privKey$type, "rsa") || privKey$size < 2048L) {
    stop("Only RSA keys with minimum length of 2048 bits supported", call. = FALSE)
  }
  getFileId <- function(rootDir, fileName) {
    paste0(fileName, "\\/\\", digest::digest(file = file.path(rootDir, fileName), algo = "sha256"))
  }
  fileHashes <- unique(unlist(lapply(miroAppFiles, function(fileObject) {
    rootDir <- fileObject$rootDir
    lapply(fileObject$files, function(miroAppFile) {
      if (dir.exists(file.path(rootDir, miroAppFile))) {
        return(vapply(list.files(file.path(rootDir, miroAppFile),
          all.files = TRUE, recursive = TRUE,
          no.. = TRUE
        ), function(fileName) {
          getFileId(rootDir, file.path(miroAppFile, fileName))
        },
        character(1L),
        USE.NAMES = FALSE
        ))
      }
      return(getFileId(rootDir, miroAppFile))
    })
  }), use.names = FALSE))
  sigDir <- file.path(tempdir(), paste(as.character(openssl::rand_bytes(30)), collapse = ""))
  if (!identical(unlink(sigDir, recursive = TRUE), 0L)) {
    stop(sprintf("Could not unlink existing signature directory: %s.", sigDir), call. = FALSE)
  }
  if (!dir.create(sigDir, recursive = TRUE)) {
    stop(sprintf("Could not create signature directory: %s", sigDir), call. = FALSE)
  }
  fileWithHashes <- file.path(sigDir, ".miro_hashes")
  writeLines(sort(fileHashes), fileWithHashes, useBytes = TRUE)
  sig <- openssl::signature_create(fileWithHashes, key = privKey, hash = openssl::sha512)
  fileWithSig <- file.path(sigDir, ".miro_sig")
  writeBin(sig, fileWithSig)
  fileWithPubKey <- file.path(sigDir, ".miro_pubkey")
  openssl::write_pem(privKey$pubkey, fileWithPubKey)
  return(c(fileWithHashes, fileWithSig, fileWithPubKey, genKeyFingerprint(privKey$pubkey)))
}

appIsSigned <- function(pathToApp, isExtracted = FALSE) {
  if (identical(isExtracted, TRUE)) {
    return(file.exists(file.path(pathToApp, ".miro_hashes")) &&
      file.exists(file.path(pathToApp, ".miro_sig")))
  }
  return(all(c(".miro_hashes", ".miro_sig") %in% zip::zip_list(pathToApp)$filename))
}

verifyAppSignature <- function(appDir, pubKeyPaths, printFingerprint = TRUE) {
  fileWithHashes <- file.path(appDir, ".miro_hashes")
  fileWithSig <- file.path(appDir, ".miro_sig")
  if (!file.exists(fileWithHashes) || !file.exists(fileWithSig)) {
    return(FALSE)
  }
  filesInApp <- list.files(appDir, all.files = TRUE, recursive = TRUE, no.. = TRUE)
  filesInApp <- filesInApp[!filesInApp %in% c(".miro_hashes", ".miro_sig", ".miro_pubkey")]
  fileHashesShould <- readLines(fileWithHashes, encoding = "UTF-8")
  if (!identical(length(fileHashesShould), length(filesInApp))) {
    return(FALSE)
  }
  getFileId <- function(rootDir, fileName) {
    paste0(fileName, "\\/\\", digest::digest(file = file.path(rootDir, fileName), algo = "sha256"))
  }
  fileHashesActual <- vapply(filesInApp, function(fileInApp) {
    return(getFileId(appDir, fileInApp))
  }, character(1L), USE.NAMES = FALSE)
  if (!all(fileHashesActual %in% fileHashesShould)) {
    return(FALSE)
  }
  validPubKey <- NULL
  for (pubKeyPath in pubKeyPaths) {
    signatureValid <- tryCatch(
      {
        openssl::signature_verify(fileWithHashes, fileWithSig,
          hash = openssl::sha512, pubkey = pubKeyPath
        )
        TRUE
      },
      error = function(e) {
        return(FALSE)
      }
    )
    if (signatureValid) {
      validPubKey <- pubKeyPath
      break
    }
  }
  if (!length(validPubKey)) {
    return(FALSE)
  }
  if (printFingerprint) {
    write(
      paste0("mfprnt:::", genKeyFingerprint(validPubKey)),
      stderr()
    )
  }
  return(TRUE)
}

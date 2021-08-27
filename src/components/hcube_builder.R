HcubeBuilder <- R6Class("HcubeBuilder", public = list(
  initialize = function(dataHashes, scalarData) {
    hashesToOrder <- startsWith(names(dataHashes), "__")
    scenHashOrder <- order(names(dataHashes)[hashesToOrder])
    dataHashes <- c(
      dataHashes[!hashesToOrder],
      dataHashes[hashesToOrder][scenHashOrder]
    )
    private$colsNeedSplit <- vector("logical", length(dataHashes))
    names(private$colsNeedSplit) <- names(dataHashes)
    private$isDynamicCol <- vector("logical", length(dataHashes))
    names(private$isDynamicCol) <- names(dataHashes)
    private$dataHashes <- dataHashes
    private$dataRaw <- dataHashes
    private$scalarData <- scalarData
    if (scalarsFileName %in% names(ioConfig$modelInRaw)) {
      scalarsConfig <- ioConfig$modelInRaw[[scalarsFileName]]$symtypes
      names(scalarsConfig) <- ioConfig$modelInRaw[[scalarsFileName]]$symnames
      private$scalarsConfig <- scalarsConfig
    }
    return(invisible(self))
  },
  setDataHashes = function(dataHashes, scalarData) {
    private$scalarData <- scalarData
    private$scenToRemove <- NULL
    private$parValCombinations <- NULL
    isAttach <- startsWith(names(private$dataHashes), "__xattach_")
    private$dataHashes[isAttach] <- NULL
    isNewAttach <- startsWith(names(dataHashes), "__xattach_")
    private$dataHashes <- c(
      private$dataHashes,
      dataHashes[isNewAttach][order(names(dataHashes)[isNewAttach])]
    )
    dataHashes <- dataHashes[!isNewAttach]
    for (dsId in names(dataHashes)) {
      if (!private$isDynamicCol[[dsId]]) {
        private$dataHashes[[dsId]] <- dataHashes[[dsId]]
      }
    }
    return(invisible(self))
  },
  pushRange = function(datasetNameLo, datasetNameUp, data, allCombinations = FALSE) {
    if (datasetNameLo %in% ioConfig$DDPar) {
      stopifnot(datasetNameUp %in% ioConfig$DDPar)
    } else {
      stop("HcubeBuilder: Ranges only supported for double-dash parameters", call. = FALSE)
    }
    dsPrefixes <- paste0("--", substring(c(datasetNameLo, datasetNameUp), 9L), "= ")
    dsIds <- paste0("__cl_", c(datasetNameLo, datasetNameUp))
    if (allCombinations) {
      private$isDynamicCol[[dsIds[1]]] <- TRUE
      private$isDynamicCol[[dsIds[2]]] <- FALSE
      private$dataRaw[[dsIds[1]]] <- paste0(data$min, '|"""|', data$max)
      private$dynamicRangeCols[[dsIds[1]]] <- list(
        id = datasetNameLo,
        colNames = c(datasetNameLo, datasetNameUp)
      )
      private$colsNeedSplit[[dsIds[1]]] <- TRUE
      private$dataHashes[[dsIds[1]]] <- paste0(
        dsPrefixes[1], escapeGAMSCL(data$min),
        '|"""|', dsPrefixes[2], escapeGAMSCL(data$max)
      )
      private$dataHashes[[dsIds[2]]] <- NA_character_
    } else {
      stopifnot(identical(length(data), 2L))
      private$isDynamicCol[dsIds] <- TRUE
      private$dynamicRangeCols[dsIds] <- NULL
      private$dataRaw[dsIds] <- data
      private$dataHashes[dsIds] <- paste0(dsPrefixes, escapeGAMSCL(data))
    }
    return(invisible(self))
  },
  push = function(datasetName, data, ddChoices = NULL) {
    if (datasetName %in% ioConfig$DDPar) {
      dsPrefix <- paste0("--", substring(datasetName, 9L), "= ")
      dsId <- paste0("__cl_", datasetName)
      isClArg <- TRUE
    } else if (datasetName %in% ioConfig$GMSOpt) {
      dsPrefix <- paste0(substring(datasetName, 9L), "= ")
      dsId <- paste0("__cl_", datasetName)
      isClArg <- TRUE
    } else {
      dsPrefix <- paste0("--HCUBE_SCALARV_", datasetName, "= ")
      dsId <- datasetName
      isClArg <- FALSE
    }
    private$isDynamicCol[[dsId]] <- TRUE
    private$dataRaw[[dsId]] <- data
    if (length(ddChoices)) {
      if (!isClArg && length(names(ddChoices))) {
        if (identical(ioConfig$modelIn[[datasetName]]$dropdown$clearValue, TRUE)) {
          private$dataHashes[[dsId]] <- paste0(
            "--HCUBE_SCALART_", datasetName, "= ",
            escapeGAMSCL(names(ddChoices)[match(data, ddChoices)])
          )
          return(invisible(self))
        }
        private$colsNeedSplit[[dsId]] <- TRUE
        private$dataHashes[[dsId]] <- paste0(
          dsPrefix, escapeGAMSCL(data),
          '|"""|--HCUBE_SCALART_', datasetName, "= ",
          escapeGAMSCL(names(ddChoices)[match(data, ddChoices)])
        )
        return(invisible(self))
      }
      private$dataHashes[[dsId]] <- paste0(dsPrefix, escapeGAMSCL(data))
      return(invisible(self))
    }
    if (isClArg || identical(private$scalarsConfig[[dsId]], "set")) {
      private$dataHashes[[dsId]] <- paste0(dsPrefix, escapeGAMSCL(data))
    } else {
      private$dataHashes[[dsId]] <- paste0(dsPrefix, data)
    }
    return(invisible(self))
  },
  getScenHashes = function() {
    return(names(private$parValCombinations))
  },
  removeScen = function(hashesToRemove) {
    private$scenToRemove <- names(private$parValCombinations) %in% hashesToRemove
    private$parValCombinations[private$scenToRemove] <- NULL
    return(invisible(self))
  },
  generateScenHashes = function() {
    dataHashesToUse <- !is.na(private$dataHashes)
    dataHashes <- private$dataHashes[dataHashesToUse]
    isDynamicCol <- private$isDynamicCol[dataHashesToUse]
    colsNeedSplit <- private$colsNeedSplit[dataHashesToUse]
    allParValCombinations <- do.call(
      "expand.grid",
      c(unname(dataHashes),
        stringsAsFactors = FALSE
      )
    )
    private$parValCombinations <- do.call(
      function(...) Map(list, ...),
      allParValCombinations[which(isDynamicCol)]
    )
    for (colIdx in which(colsNeedSplit)) {
      allParValCombinations[[colIdx]] <- gsub('|"""|', " ", allParValCombinations[[colIdx]],
        fixed = TRUE
      )
    }
    names(private$parValCombinations) <- vapply(unite(allParValCombinations, "val",
      seq_along(allParValCombinations),
      remove = TRUE, sep = " "
    )[[1]],
    digest::digest, character(1L),
    algo = "sha256",
    serialize = FALSE, USE.NAMES = FALSE
    )
    return(names(private$parValCombinations))
  },
  getNoScen = function() {
    return(length(private$parValCombinations))
  },
  getHcubeScalars = function() {
    allParValCombinationsRaw <- do.call(
      "expand.grid",
      c(unname(private$dataRaw[which(private$isDynamicCol)]),
        stringsAsFactors = FALSE
      )
    )
    if (length(private$scenToRemove)) {
      stopifnot(identical(length(private$scenToRemove), nrow(allParValCombinationsRaw)))
      allParValCombinationsRaw <- allParValCombinationsRaw[which(!private$scenToRemove), ]
    }
    namesTmp <- names(private$dataRaw)[which(private$isDynamicCol)]
    isClArg <- startsWith(namesTmp, "__cl_")
    namesTmp[isClArg] <- substring(namesTmp[isClArg], 6L)
    names(allParValCombinationsRaw) <- namesTmp
    for (dynamicRangeCol in private$dynamicRangeCols) {
      allParValCombinationsRaw <- separate(allParValCombinationsRaw,
        !!dynamicRangeCol$id,
        dynamicRangeCol$colNames,
        sep = '\\|"""\\|'
      )
    }
    staticScalars <- as_tibble(private$scalarData[!names(private$scalarData) %in% names(allParValCombinationsRaw)])
    return(allParValCombinationsRaw %>%
      bind_cols(
        `_hash` = names(private$parValCombinations),
        staticScalars[rep(1, nrow(allParValCombinationsRaw)), ]
      ) %>%
      mutate_if(~ !is.character(.), as.character) %>%
      pivot_longer(!`_hash`,
        names_to = "scalar", values_to = "value"
      ))
  },
  writeHcubeFile = function(workDir) {
    filePath <- file.path(workDir, "hcube.json")
    write_json(list(jobs = lapply(names(private$parValCombinations), function(scenId) {
      return(list(
        id = scenId,
        arguments = unlist(
          strsplit(unlist(private$parValCombinations[[scenId]],
            use.names = FALSE
          ),
          '|"""|',
          fixed = TRUE
          ),
          use.names = FALSE
        )
      ))
    })), filePath, auto_unbox = TRUE)
    return(filePath)
  }
), private = list(
  parValCombinations = NULL,
  dataRaw = NULL,
  dataHashes = NULL,
  isDynamicCol = NULL,
  scalarData = NULL,
  dynamicRangeCols = list(),
  scalarsConfig = list(),
  colsNeedSplit = NULL,
  scenToRemove = NULL
))

ScenData <- R6Class("ScenData", public = list(
  initialize = function(db, scenDataTemplate, hiddenOutputScalars = character(0L)) {
    private$db <- db
    private$dbSymbols <- dbSchema$getAllSymbols()
    private$scenDataTemplate <- scenDataTemplate
    self$clearSandbox()
    private$hiddenOutputScalars <- tolower(hiddenOutputScalars)
    return(invisible(self))
  },
  getRefScenMap = function(refId = NULL) {
    if (is.null(refId)) {
      return(private$refScenMap)
    }
    if (identical(refId, "sb")) {
      return("sb")
    }
    return(private$refScenMap[[refId]])
  },
  loadSandbox = function(data, symNames = NULL, metaData = NULL) {
    noData <- vapply(data, is.null, logical(1L), USE.NAMES = FALSE)
    if (any(noData)) {
      if (is.null(symNames)) {
        data[noData] <- private$scenDataTemplate
      } else {
        data[noData] <- private$scenDataTemplate[match(symNames, private$dbSymbols)]
      }
    }
    if (is.null(symNames)) {
      symNames <- private$dbSymbols
    }
    stopifnot(identical(length(symNames), length(data)))
    scalarsOutIdx <- match(scalarsOutName, symNames)
    if (!is.na(scalarsOutIdx)) {
      rowsToFilter <- !tolower(data[[scalarsOutIdx]][[1]]) %in% private$hiddenOutputScalars
      private$cachedData[["sb"]][["scalar"]] <- data[[scalarsOutIdx]]
      data[[scalarsOutIdx]] <- data[[scalarsOutIdx]][rowsToFilter, ]
    }
    private$cachedData[["sb"]][["data"]][symNames] <- data
    if (!is.null(metaData)) {
      private$cachedData[["sb"]][["meta"]] <- metaData
    }
    return(invisible(self))
  },
  clearSandbox = function() {
    private$cachedData[["sb"]] <- list(
      data = setNames(private$scenDataTemplate, private$dbSymbols),
      scalar = tibble(
        scalar = character(),
        description = character(),
        value = character()
      ),
      meta = tibble(),
      inDataSid = NA_integer_
    )
    return(invisible(self))
  },
  addRefId = function(refId, scenIds) {
    stopifnot(is.character(refId))
    isSbScen <- scenIds == "sb"
    if (any(isSbScen) && !identical(refId, "sb")) {
      scenIds[isSbScen] <- paste0("sb_", refId)
    }
    private$refScenMap[[refId]] <- unique(c(private$refScenMap[[refId]], as.character(scenIds)))
    return(invisible(self))
  },
  getSandboxHasOutputData = function(scriptOutput) {
    if (!is.null(scriptOutput) && scriptOutput$hasResults()) {
      return(TRUE)
    }
    for (dsName in names(ioConfig$modelOut)) {
      if (identical(dsName, scalarsOutName)) {
        if (identical(length(private$cachedData[["sb"]][["data"]][[dsName]]), 3L) &&
          !all(is.na(private$cachedData[["sb"]][["data"]][[dsName]][[3]]))) {
          return(TRUE)
        }
      } else if (nrow(private$cachedData[["sb"]][["data"]][[dsName]]) > 0L) {
        return(TRUE)
      }
    }
    return(FALSE)
  },
  load = function(scenIds, sheetIds = NULL, symNames = NULL, limit = 1e7,
                  showProgress = TRUE, refId = NULL, registerRef = TRUE) {
    if (identical(refId, "sb") && length(scenIds) > 1L) {
      stop_custom("bad_param", "Cannot load multiple scenarios with refId=sb", call. = FALSE)
    }
    if (!length(scenIds)) {
      return(invisible(self))
    }
    if (!is.null(sheetIds)) {
      symNames <- private$dbSymbols[sheetIds]
    } else if (is.null(symNames)) {
      symNames <- private$dbSymbols
    }
    if (showProgress) {
      prog <- Progress$new()
      on.exit(prog$close())
      prog$set(message = lang$progressBar$loadScenDb$title, value = 0)
      incAmount <- 1 / (length(scenIds) * length(symNames))
    }
    i <- 1L
    checkDirty <- TRUE
    if (!identical(refId, "sb")) {
      isSbScen <- startsWith(as.character(scenIds), "sb")
      if (any(isSbScen)) {
        private$duplicateSandbox(refId)
        if (registerRef) {
          self$addRefId(refId, "sb")
        }
        scenIds <- as.integer(scenIds[!isSbScen])
      }
    }
    for (scenId in scenIds) {
      scenIdChar <- if (identical(refId, "sb")) "sb" else as.character(scenId)
      if (!scenId %in% names(private$cachedData)) {
        # fetch timestamp
        metaData <- private$getMetadata(scenId)
        private$cachedData[[scenIdChar]] <- list(
          data = list(),
          meta = metaData,
          timestamp = metaData[["_stime"]][1],
          inDataSid = NA_integer_
        )
        if (length(metaData[["_scode"]]) && metaData[["_scode"]] > (SCODEMAP[["scen"]] + 10000L)) {
          # Hcube scenario, load input data from job config
          private$cachedData[[scenIdChar]][["inDataSid"]] <- metaData[["_scode"]] - 10000L
        }
        checkDirty <- FALSE
      } else if (identical(refId, "sb")) {
        private$cachedData[["sb"]][["inDataSid"]] <- private$cachedData[[as.character(scenId)]][["inDataSid"]]
      }
      for (symName in symNames) {
        if (identical(refId, "sb") ||
          is.null(private$cachedData[[scenIdChar]][["data"]]) ||
          is.null(private$cachedData[[scenIdChar]][["data"]][[symName]])) {
          if (is.na(private$cachedData[[scenIdChar]][["inDataSid"]]) ||
            symName %in% c(scalarsFileName, names(ioConfig$modelOut))) {
            scenIdToFetch <- scenId
          } else {
            scenIdToFetch <- private$cachedData[[scenIdChar]][["inDataSid"]]
          }
          if (identical(symName, scalarsOutName)) {
            if (checkDirty) {
              private$checkDirty(scenId)
            }
            dataTmp <- private$db$importDataset(
              tableName = symName,
              subsetSids = scenIdToFetch,
              limit = limit
            )
            if (length(dataTmp)) {
              private$cachedData[[scenIdChar]][["scalar"]] <- select(dataTmp, -`_sid`)
            } else {
              private$cachedData[[scenIdChar]][["scalar"]] <- tibble(
                scalar = character(),
                description = character(),
                value = character()
              )
            }
            if (length(private$hiddenOutputScalars)) {
              scalarsInData <- private$cachedData[[scenIdChar]][["scalar"]][[1]]
              rowsToFilter <- !tolower(scalarsInData) %in% private$hiddenOutputScalars
            } else {
              rowsToFilter <- TRUE
            }
            private$cachedData[[scenIdChar]][["data"]][[symName]] <- private$cachedData[[scenIdChar]][["scalar"]][rowsToFilter, ]
          } else {
            if (checkDirty) {
              private$checkDirty(scenId)
            }
            dataTmp <- private$db$importDataset(
              tableName = symName,
              subsetSids = scenIdToFetch,
              limit = limit
            )
            if (length(dataTmp)) {
              private$cachedData[[scenIdChar]][["data"]][[symName]] <- select(dataTmp, -`_sid`)
            } else {
              private$cachedData[[scenIdChar]][["data"]][[symName]] <- private$scenDataTemplate[[match(symName, private$dbSymbols)]]
            }
          }
        }
        if (showProgress) {
          prog$inc(
            amount = incAmount,
            detail = paste0(lang$progressBar$loadScenDb$progress, i)
          )
        }
        i <- i + 1L
      }
    }
    if (registerRef && !is.null(refId) && !identical(refId, "sb")) {
      self$addRefId(refId, scenIds)
    }
    return(invisible(self))
  },
  getInputDataSids = function(scenIds) {
    return(vapply(as.character(scenIds), function(scenId) {
      private$cachedData[[scenId]][["inDataSid"]]
    }, integer(1L), USE.NAMES = FALSE))
  },
  getAll = function(refId, symName = NULL, showProgress = TRUE) {
    stopifnot(identical(length(symName), 1L))
    scenIds <- private$refScenMap[[refId]]
    self$load(scenIds,
      symNames = symName, showProgress = showProgress,
      refId = refId, registerRef = FALSE
    )
    return(lapply(as.character(scenIds), function(scenId) {
      private$cachedData[[scenId]][["data"]][[symName]]
    }))
  },
  get = function(refId, symNames = NULL, sheetIds = NULL, showProgress = TRUE,
                 drop = FALSE, includeHiddenScalars = FALSE) {
    if (identical(refId, "sb")) {
      scenId <- "sb"
    } else {
      scenId <- private$refScenMap[[refId]]
      stopifnot(identical(length(scenId), 1L))
    }
    if (!is.null(sheetIds)) {
      symNames <- private$dbSymbols[sheetIds]
    } else if (is.null(symNames)) {
      symNames <- private$dbSymbols
    }
    if (!identical(refId, "sb")) {
      self$load(scenId,
        symNames = symNames, showProgress = showProgress,
        refId = refId, registerRef = FALSE
      )
    }
    if (includeHiddenScalars && length(private$hiddenOutputScalars)) {
      outScalarsFull <- private$cachedData[[as.character(scenId)]][["scalar"]]
    } else {
      outScalarsFull <- NULL
    }
    if (is.null(symNames)) {
      if (length(outScalarsFull)) {
        dataTmp <- private$cachedData[[as.character(scenId)]][["data"]]
        dataTmp[[scalarsOutName]] <- outScalarsFull
        return(dataTmp)
      }
      return(private$cachedData[[as.character(scenId)]][["data"]])
    }
    if (length(symNames) > 1L || !drop) {
      if (length(outScalarsFull) && scalarsOutName %in% symNames) {
        dataTmp <- private$cachedData[[as.character(scenId)]][["data"]][symNames]
        dataTmp[[scalarsOutName]] <- outScalarsFull
        return(dataTmp)
      }
      return(private$cachedData[[as.character(scenId)]][["data"]][symNames])
    }
    if (length(outScalarsFull) && identical(scalarsOutName, symNames)) {
      return(outScalarsFull)
    }
    return(private$cachedData[[as.character(scenId)]][["data"]][[symNames]])
  },
  getScalars = function(refId, outputScalarsOnly = FALSE) {
    if (identical(refId, "sb")) {
      scenId <- "sb"
    } else {
      scenId <- private$refScenMap[[refId]]
      stopifnot(identical(length(scenId), 1L))
    }
    self$get(refId, symNames = scalarsOutName)
    outputScalarsFull <- private$cachedData[[as.character(scenId)]][["scalar"]]
    if (scalarsFileName %in% private$dbSymbols) {
      inputScalars <- self$get(refId, symNames = scalarsFileName, drop = TRUE)
    } else {
      inputScalars <- NULL
    }
    if (length(outputScalarsFull)) {
      if (!outputScalarsOnly && length(inputScalars)) {
        return(bind_rows(
          inputScalars,
          outputScalarsFull
        ))
      }
      return(outputScalarsFull)
    }
    if (!outputScalarsOnly && length(inputScalars)) {
      return(inputScalars)
    }
    return(tibble(scalar = character(), description = character(), value = character()))
  },
  getById = function(id, refId = NULL, scenIds = NULL, drop = FALSE) {
    if (!is.null(refId)) {
      if (identical(refId, "sb")) {
        scenIds <- "sb"
      } else {
        scenIds <- private$refScenMap[[refId]]
      }
    }
    if (identical(length(scenIds), 0L)) {
      return(list())
    }
    if (identical(length(scenIds), 1L) && drop) {
      return(private$cachedData[[as.character(scenIds)]][[id]])
    }
    return(lapply(private$cachedData[as.character(scenIds)], "[[", id))
  },
  setScenIdNameMap = function(scenIdNameMap) {
    private$scenIdNameMap <- scenIdNameMap
    return(invisible(self))
  },
  clear = function(refId, scenIds = NULL, clearRef = TRUE) {
    stopifnot(is.character(refId))

    if (is.null(scenIds)) {
      scenIds <- private$refScenMap[[refId]]
      refScenIdx <- seq_along(scenIds)
    } else {
      refScenIdx <- match(scenIds, private$refScenMap[[refId]])
      if (any(is.na(refScenIdx))) {
        stop_custom("bad_ref", "The scenario ids could not be found for the refId you provided. Did you forget to register them via ScenData$addRefId()?", call. = FALSE)
      }
    }

    scenIdsToClear <- rep.int(TRUE, length(scenIds))
    # don't clear sandbox data
    scenIdsToClear[scenIds == "sb"] <- FALSE
    for (scenIdsInRef in private$refScenMap[names(private$refScenMap) != refId]) {
      # don't clear scenario data of scenarios with reference count > 0
      scenIdsToClear[which(scenIds %in% scenIdsInRef)] <- FALSE
    }
    scenIdsToClear <- scenIds[scenIdsToClear]
    self$invalidateCache(scenIdsToClear)
    if (clearRef) {
      private$refScenMap[[refId]] <- private$refScenMap[[refId]][-refScenIdx]
    }
    return(invisible(self))
  },
  invalidateCache = function(scenIds) {
    scenIds <- scenIds[scenIds != "sb"]
    scenIds <- as.character(scenIds)[scenIds %in% names(private$cachedData)]
    if (length(scenIds)) {
      private$cachedData[scenIds] <- NULL
    }
    return(invisible(self))
  }
), private = list(
  db = NULL,
  scenDataTemplate = NULL,
  hiddenOutputScalars = NULL,
  cachedData = NULL,
  dbSymbols = NULL,
  refScenMap = list(),
  scenIdNameMap = character(),
  getMetadata = function(scenIds) {
    metaTmp <- private$db$importDataset("_scenMeta",
      subsetSids = scenIds
    )
    if (length(private$scenIdNameMap)) {
      sidNameMapIds <- match(as.character(metaTmp[["_sid"]]), names(private$scenIdNameMap))
      namesNeedRemapping <- !is.na(sidNameMapIds)
      if (any(namesNeedRemapping)) {
        metaTmp[["_sname"]][namesNeedRemapping] <- private$scenIdNameMap[sidNameMapIds[namesNeedRemapping]]
      }
    } else {
      isHcScen <- metaTmp[["_scode"]] > 0
      if (any(isHcScen)) {
        metaTmp[["_sname"]][isHcScen] <- paste0("HC (", substr(metaTmp[["_sname"]][isHcScen], 1L, 8L), "...)")
      }
    }
    return(metaTmp)
  },
  checkDirty = function(scenId) {
    currentTimestamp <- private$db$importDataset("_scenMeta",
      subsetSids = scenId,
      colNames = "_stime"
    )[["_stime"]][1]
    cSid <- as.character(scenId)
    private$cachedData[[cSid]][["dirty"]] <- !identical(
      private$cachedData[[cSid]][["timestamp"]],
      currentTimestamp
    )
    return(invisible(self))
  },
  duplicateSandbox = function(refId) {
    # copy all data from sandbox over
    stopifnot(length(refId) == 1L)
    if (!length(private$cachedData[[paste0("sb_", refId)]])) {
      private$cachedData[[paste0("sb_", refId)]] <- private$cachedData[["sb"]]
      private$cachedData[[paste0("sb_", refId)]][["meta"]][["_sname"]] <- paste(
        private$cachedData[[paste0("sb_", refId)]][["meta"]][["_sname"]],
        lang$nav$scen$scenNameSandboxSuffix
      )
    }
    return(invisible(self))
  }
))

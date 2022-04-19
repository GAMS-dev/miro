Views <- R6Class("Views",
  inherit = ScenarioExtensions,
  public = list(
    setGlobalViews = function(globalViews) {
      if (length(globalViews)) {
        private$globalViews <- globalViews
      }
      return(invisible(self))
    },
    getDuplicatedViews = function() {
      return(private$duplicatedViews)
    },
    getInvalidViews = function() {
      return(private$invalidViews)
    },
    getScenViewConf = function(scenId = "1", symName = NULL) {
      if (is.null(symName)) {
        return(private$scenViewConf[[scenId]])
      }
      return(private$scenViewConf[[scenId]][[symName]])
    },
    cleanConf = function(viewConf, sandbox = TRUE, removeDuplicates = FALSE, scenId = "1") {
      if (!length(viewConf)) {
        return(list())
      }
      stopifnot(is.list(viewConf), length(names(viewConf)) != 0)
      cleanViewConf <- viewConf
      scenId <- as.character(scenId)

      invalidViews <- !names(cleanViewConf) %in% private$getValidSymNames()
      if (any(invalidViews)) {
        private$invalidViews <- names(cleanViewConf)[invalidViews]
        private$invalidViews[startsWith(private$invalidViews, "_pivotcomp_")] <-
          substring(
            private$invalidViews[startsWith(private$invalidViews, "_pivotcomp_")],
            12
          )
        private$invalidViews <- unique(private$invalidViews)
        flog.info(
          "Invalid view configs found. Symbol(s): %s do not exist.",
          private$invalidViews
        )
        cleanViewConf <- cleanViewConf[!invalidViews]
      } else {
        private$invalidViews <- NULL
      }
      private$duplicatedViews <- list()
      for (symbolName in names(cleanViewConf)) {
        names(cleanViewConf[[symbolName]]) <- private$makeIds(names(cleanViewConf[[symbolName]]))
        if (length(names(cleanViewConf[[symbolName]])) != length(cleanViewConf[[symbolName]])) {
          flog.info("Not all views for symbol: %s have names.", symbolName)
          cleanViewConf[[symbolName]] <- NULL
          next
        }
        if (removeDuplicates) {
          if (!length(private$scenViewConf[[scenId]][[symbolName]])) {
            next
          }
          duplicatedIds <- names(cleanViewConf[[symbolName]]) %in%
            names(private$scenViewConf[[scenId]][[symbolName]])
          if (any(duplicatedIds)) {
            private$duplicatedViews[[symbolName]] <- names(cleanViewConf[[symbolName]])[duplicatedIds]
            cleanViewConf[[symbolName]][duplicatedIds] <- NULL
          }
        }
      }
      return(cleanViewConf)
    },
    loadConf = function(viewConf, sandbox = TRUE, scenIds = NULL, sidsToLoad = NULL) {
      if (!is_tibble(viewConf) || length(viewConf) < 4L || nrow(viewConf) == 0) {
        if (sandbox) {
          private$scenViewConf[["1"]] <- list()
        } else {
          for (scenId in as.character(scenIds)) {
            private$scenViewConf[[scenId]] <- list()
          }
        }
        return(invisible(self))
      }

      idColName <- names(viewConf)[1]
      sids <- viewConf[[1]]
      if (length(scenIds) > 1L) {
        if (length(sidsToLoad) != length(scenIds)) {
          stop("sidsToLoad arg must be of the same length as scenIds", call. = FALSE)
        }
        uniqueSids <- sidsToLoad
      } else {
        uniqueSids <- unique(sids)
      }
      if (sandbox && length(uniqueSids) > 1) {
        stop("Invalid view config for sandbox. Only single sid allowed.",
          call. = FALSE
        )
      }
      viewConfTmp <- lapply(uniqueSids, function(sid) {
        datasetSid <- viewConf[sids == sid, ]
        if (!length(datasetSid) || nrow(datasetSid) == 0L) {
          return(list())
        }
        symNames <- datasetSid$symName
        invalidSymNames <- !symNames %in% private$getValidSymNames()
        if (any(invalidSymNames)) {
          flog.info(
            "Orphaned view configuration found for symbol(s): %s.",
            paste(symNames[invalidSymNames], collapse = ", ")
          )
        }
        uniqueSymNames <- unique(symNames)
        retTmp <- lapply(uniqueSymNames, function(symName) {
          dataset <- datasetSid[symNames == symName, ]
          viewIds <- private$makeIds(dataset$id)
          viewData <- lapply(dataset$data, function(data) {
            safeFromJSON(data, simplifyDataFrame = FALSE, simplifyVector = FALSE)
          })
          names(viewData) <- viewIds
          return(viewData)
        })
        names(retTmp) <- uniqueSymNames
        return(retTmp)
      })
      if (sandbox) {
        private$scenViewConf[["1"]] <- viewConfTmp[[1]]
        return(invisible(self))
      }
      scenIds <- as.character(scenIds)
      for (i in seq_along(scenIds)) {
        private$scenViewConf[[scenIds[i]]] <- viewConfTmp[[i]]
      }
      return(invisible(self))
    },
    duplicateSandboxConf = function(scenId) {
      private$scenViewConf[[as.character(scenId)]] <- private$scenViewConf[["1"]]
      return(invisible(self))
    },
    addConf = function(viewConf, scenId = "1") {
      scenId <- as.character(scenId)
      private$scenViewConf[[scenId]] <- modifyList(
        private$scenViewConf[[scenId]],
        self$cleanConf(viewConf, TRUE, TRUE)
      )
      if (identical(scenId, "1")) {
        private$markUnsaved()
      }
      lapply(names(viewConf), function(symName) {
        if (symName %in% names(private$updateCallbacks[[scenId]])) {
          private$updateCallbacks[[scenId]][[symName]]()
        }
      })
      return(invisible(self))
    },
    getConf = function(scenId = "1") {
      scenId <- as.character(scenId)
      return(dplyr::bind_rows(lapply(names(private$scenViewConf[[scenId]]), function(symName) {
        symbolConf <- private$scenViewConf[[scenId]][[symName]]
        return(tibble(
          symName = rep.int(symName, length(symbolConf)),
          id = names(symbolConf),
          data = vapply(symbolConf, function(viewConf) {
            toJSON(viewConf, auto_unbox = TRUE, null = "null")
          }, character(1L), USE.NAMES = FALSE)
        ))
      })))
    },
    getJSON = function(views = NULL, scenId = "1") {
      scenId <- as.character(scenId)
      if (is.null(views)) {
        return(toJSON(private$scenViewConf[[scenId]], auto_unbox = TRUE, null = "null"))
      }
      stopifnot(is.list(views))
      selectedViews <- list()
      for (view in views) {
        if (view[1] %in% names(private$scenViewConf[[scenId]]) &&
          view[2] %in% names(private$scenViewConf[[scenId]][[view[1]]])) {
          selectedViews[[view[1]]][[view[2]]] <- private$scenViewConf[[scenId]][[view[1]]][[view[2]]]
        }
      }
      return(toJSON(selectedViews, auto_unbox = TRUE, null = "null"))
    },
    getSummary = function(tabularInputSymConfig, tabularOutputSymConfig, scenId = "1") {
      scenId <- as.character(scenId)
      if (!length(private$symbolAliases)) {
        outputSymAliases <- vapply(tabularOutputSymConfig,
          "[[", character(1L), "alias",
          USE.NAMES = FALSE
        )
        if (length(tabularInputSymConfig)) {
          inputSymAliases <- vapply(tabularInputSymConfig[match(
            private$tabularInputSymbols,
            names(tabularInputSymConfig)
          )],
          "[[", character(1L), "alias",
          USE.NAMES = FALSE
          )
        } else {
          inputSymAliases <- character(0L)
        }
        private$symbolAliases <- c(
          outputSymAliases, paste(lang$nav$scen$pivo$viewPrefix, outputSymAliases),
          inputSymAliases, paste(lang$nav$scen$pivo$viewPrefix, inputSymAliases)
        )
      }
      symData <- lapply(names(private$scenViewConf[[scenId]]), function(symName) {
        ids <- names(private$scenViewConf[[scenId]][[symName]])
        return(list(rep.int(symName, length(ids)), ids))
      })
      viewsMetadata <- list(symName = NULL, symAlias = NULL, id = NULL)
      viewsMetadata[["symName"]] <- unlist(lapply(symData, "[[", 1L), use.names = FALSE)
      viewsMetadata[["id"]] <- unlist(lapply(symData, "[[", 2L), use.names = FALSE)
      if (!length(viewsMetadata[["symName"]])) {
        return(list(symName = NULL, symAlias = NULL, id = NULL))
      }
      symIds <- match(
        viewsMetadata[["symName"]],
        private$getValidSymNames()
      )
      viewsMetadata[["symAlias"]] <- private$symbolAliases[symIds]

      if (any(is.na(symIds))) {
        flog.info(
          "Invalid symbol names found when trying to get view summary: %s. They were ignored.",
          viewsMetadata[["symName"]][is.na(symIds)]
        )
        viewsMetadata[["symAlias"]][is.na(symIds)] <- viewsMetadata[["symName"]][is.na(symIds)]
      }
      symIdsPivotComp <- startsWith(viewsMetadata[["symName"]], "_pivotcomp_")
      sortedSymIds <- order(symIds)
      sortedSymIds <- c(
        sortedSymIds[sortedSymIds %in% which(!symIdsPivotComp)],
        sortedSymIds[sortedSymIds %in% which(symIdsPivotComp)]
      )
      viewsMetadata[["symName"]] <- viewsMetadata[["symName"]][sortedSymIds]
      viewsMetadata[["symAlias"]] <- viewsMetadata[["symAlias"]][sortedSymIds]
      viewsMetadata[["id"]] <- viewsMetadata[["id"]][sortedSymIds]
      return(viewsMetadata)
    },
    removeConf = function(viewsToRemove, scenId = "1") {
      scenId <- as.character(scenId)
      if (!is.list(viewsToRemove) || !length(viewsToRemove)) {
        return(invisible(self))
      }
      lapply(viewsToRemove, function(viewToRemove) {
        private$removeView(viewToRemove[1], viewToRemove[2], scenId)
      })
      if (identical(scenId, "1")) {
        private$markUnsaved()
      }

      lapply(
        unique(vapply(viewsToRemove, "[[", character(1L), 1,
          USE.NAMES = FALSE
        )),
        function(symName) {
          if (symName %in% names(private$updateCallbacks[[scenId]])) {
            private$updateCallbacks[[scenId]][[symName]]()
          }
        }
      )
      return(invisible(self))
    },
    clearConf = function(scenId = "1") {
      scenId <- as.character(scenId)
      viewsToRemove <- names(private$scenViewConf[[scenId]])
      if (!length(viewsToRemove)) {
        return(invisible(self))
      }
      private$scenViewConf[[scenId]] <- list()

      if (identical(scenId, "1")) {
        private$markUnsaved()
      }
      lapply(
        viewsToRemove,
        function(symName) {
          if (symName %in% names(private$updateCallbacks[[scenId]])) {
            private$updateCallbacks[[scenId]][[symName]]()
          }
        }
      )
      return(invisible(self))
    },
    add = function(session, id, viewConf) {
      symName <- private$getSymbolName(session)
      if (length(symName) == 2) {
        if (!identical(symName[[2]], "1")) {
          stop("Can not modify views in comparison mode.", call. = FALSE)
        }
        symName <- symName[[1]]
      }
      private$markUnsaved()
      if (!symName %in% names(private$scenViewConf[["1"]])) {
        private$scenViewConf[["1"]][[symName]] <- list()
      }
      private$scenViewConf[["1"]][[symName]][[private$makeIds(id)]] <- viewConf
      return(invisible(self))
    },
    getIds = function(session, filter = c("all", "global", "local")) {
      match.arg(filter)
      symName <- private$getSymbolName(session)
      if (length(symName) != 2) {
        symName <- c(symName, "1")
      }
      viewIdsTmp <- character()
      if (!identical(filter, "local")) {
        if (symName[[1]] %in% names(private$globalViews)) {
          viewIdsTmp <- names(private$globalViews[[symName[[1]]]])
        }
        if (identical(filter, "global")) {
          return(viewIdsTmp)
        }
      }
      if (!length(private$scenViewConf) ||
        !symName[[2]] %in% names(private$scenViewConf) ||
        !symName[[1]] %in% names(private$scenViewConf[[symName[[2]]]])) {
        return(viewIdsTmp)
      }
      return(unique(c(viewIdsTmp, names(private$scenViewConf[[symName[[2]]]][[symName[[1]]]]))))
    },
    get = function(session, id = NULL, filter = c("all", "global", "local")) {
      match.arg(filter)
      if (length(id)) {
        id <- private$makeIds(id)
      }
      symName <- private$getSymbolName(session)
      if (length(symName) != 2) {
        symName <- c(symName, "1")
      }
      viewConfTmp <- NULL
      if (!identical(filter, "local") && symName[[1]] %in% names(private$globalViews)) {
        viewConfTmp <- private$globalViews[[symName[[1]]]]
      }
      if (!identical(filter, "global") && length(private$scenViewConf) &&
        symName[[2]] %in% names(private$scenViewConf) && symName[[1]] %in% names(private$scenViewConf[[symName[[2]]]])) {
        if (length(id)) {
          if (id %in% names(private$scenViewConf[[symName[[2]]]][[symName[[1]]]])) {
            return(private$scenViewConf[[symName[[2]]]][[symName[[1]]]][[id]])
          }
          if (id %in% names(viewConfTmp)) {
            return(viewConfTmp[[id]])
          }
          stop(sprintf("View with id: %s could not be found.", id),
            call. = FALSE
          )
        }
        # need to merge local and global views. Local views shadow global views
        for (viewName in names(private$scenViewConf[[symName[[2]]]][[symName[[1]]]])) {
          viewConfTmp[[viewName]] <- private$scenViewConf[[symName[[2]]]][[symName[[1]]]][[viewName]]
        }
      }
      if (length(id)) {
        if (id %in% names(viewConfTmp)) {
          return(viewConfTmp[[id]])
        }
        stop(sprintf("View with id: %s could not be found.", id),
          call. = FALSE
        )
      }
      return(viewConfTmp)
    },
    remove = function(session, id) {
      id <- private$makeIds(id)
      symName <- private$getSymbolName(session)
      if (length(symName) == 2) {
        if (!identical(symName[[2]], "1")) {
          stop("Can not modify views in comparison mode.", call. = FALSE)
        }
        symName <- symName[[1]]
      }
      private$markUnsaved()
      if (symName %in% names(private$scenViewConf[["1"]]) &&
        id %in% names(private$scenViewConf[["1"]][[symName]])) {
        private$scenViewConf[["1"]][[symName]][[id]] <- NULL
        return(invisible(self))
      }
      stop(sprintf("View with id: %s does not exist, so it could not be removed.", id),
        call. = FALSE
      )
    }
  ),
  private = list(
    scenViewConf = list("1" = list()),
    globalViews = list(),
    symbolAliases = NULL,
    duplicatedViews = NULL,
    invalidViews = NULL,
    rv = NULL,
    getValidSymNames = function() {
      return(c(
        private$outputSymbols,
        paste0(
          "_pivotcomp_",
          private$outputSymbols
        ),
        private$tabularInputSymbols,
        paste0(
          "_pivotcomp_",
          private$tabularInputSymbols
        )
      ))
    },
    removeView = function(symName, id, scenId = "1") {
      if (!symName %in% names(private$scenViewConf[[scenId]])) {
        stop(sprintf("Could not remove view for symbol: %s as no views exist for this symbol.", symName),
          call. = FALSE
        )
      }
      if (!id %in% names(private$scenViewConf[[scenId]][[symName]])) {
        stop(sprintf("Could not remove view: %s for symbol: %s as no view with this id exists", id, symName),
          call. = FALSE
        )
      }
      private$scenViewConf[[scenId]][[symName]][[id]] <- NULL
      return(invisible(self))
    },
    makeIds = function(idsRaw) {
      idsTmp <- trimws(as.character(idsRaw))
      if (any(nchar(idsTmp) == 0L)) {
        stop_custom("bad_id", "View ids must not be empty", call. = FALSE)
      }
      return(idsTmp)
    }
  )
)

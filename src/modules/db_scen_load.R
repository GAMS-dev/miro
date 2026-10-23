# load scenario from database
checkIfInputDataExists <- function(inputIdsToLoad = seq_along(modelIn), manualImport = FALSE) {
  tabularInputDsToLoad <- modelInTabularData[modelInTabularData %in% names(modelIn)[inputIdsToLoad]]
  inputDatasetsExist <- vapply(tabularInputDsToLoad,
    sandboxInputData$hasData,
    logical(1),
    USE.NAMES = FALSE
  )

  if (any(inputDatasetsExist)) {
    hideEl(session, "#importDataTabset")
    if (manualImport) {
      showEl(session, "#btReplaceInputData")
      showEl(session, "#btMergeInputData")
      showEl(session, "#importDataOverwrite")
    } else {
      showEl(session, "#btOverwriteScen")
      showEl(session, "#importDataClearSandbox")
    }
  } else {
    overwriteInput <<- 1L
    if (manualImport) {
      loadDatasetsIntoSandbox()
    } else {
      rv$btOverwriteScen <- rv$btOverwriteScen + 1L
    }
  }
}
isMultiScenCompMode <- function(compMode) {
  return(compMode %in% c("pivot", names(config[["customCompareModules"]])))
}
getMultiCompRefId <- function(compMode) {
  if (identical(compMode, "pivot")) {
    return("cmpPivot")
  }
  return(paste0("cmpCustom_", config[["customCompareModules"]][[compMode]][["id"]]))
}
normalizeSandboxSids <- function(sids) {
  sids <- as.character(sids)
  sids[startsWith(sids, "sb")] <- "sb"
  return(sids)
}
parseBaselineSid <- function(value) {
  if (length(value) != 1L || !is.character(value) || !nzchar(value)) {
    return(NULL)
  }
  return(sub("_.*$", "", value))
}
loadCompViews <- function(refId, viewsId, compMode) {
  baselineSid <- scenData$getRefScenMap(refId)[1]
  if (!compModeHasBaseline(compMode) || is.na(baselineSid) || startsWith(baselineSid, "sb")) {
    views$duplicateSandboxConf(viewsId)
    views$setCompViewsScenId(viewsId, "1")
    return(invisible())
  }
  baselineSid <- as.integer(baselineSid)
  inputDataSid <- scenData$getInputDataSids(baselineSid)
  inputDataSid[is.na(inputDataSid)] <- baselineSid
  views$loadConf(
    db$importDataset(tableName = "_scenViews", subsetSids = inputDataSid),
    FALSE, viewsId, inputDataSid
  )
  views$setCompViewsScenId(viewsId, viewsId)
  return(invisible())
}
orderByBaseline <- function(sids, baselineSid) {
  if (!length(baselineSid)) {
    return(sids)
  }
  baselineIdx <- match(baselineSid, as.character(unlist(sids, use.names = FALSE)))
  if (is.na(baselineIdx)) {
    flog.warn("Baseline scenario: '%s' is not among the selected scenarios. Ignoring it.", baselineSid)
    return(sids)
  }
  return(c(sids[baselineIdx], sids[-baselineIdx]))
}
observeEvent(input$btLoadScen, {
  flog.debug("Load Scenario button clicked (multiple scenarios view).")
  rv$btLoadScen <<- isolate(rv$btLoadScen + 1L)
})
observeEvent(input$selLoadScenTags,
  {
    oderByIdentifier <- if (btSortTime) "_stime" else "_sname"
    desc <- if (btSortNameDesc || btSortTimeDesc) TRUE else FALSE
    if (!length(input$selLoadScenTags)) {
      if (length(scenMetaDb) && nrow(scenMetaDb) > maxNoScenToShow) {
        scenMetaDbSubset <<- scenMetaDb[order(scenMetaDb[[oderByIdentifier]],
          decreasing = desc
        ), ][seq_len(maxNoScenToShow), ]
        showHideEl(session, "#importScenMaxNoScen", 4000L)
      } else {
        scenMetaDbSubset <<- scenMetaDb
      }
      updateSelectInput(session, "selLoadScen",
        choices = formatScenList(scenMetaDbSubset, uid, oderByIdentifier,
          desc = desc
        ),
        selected = input$selLoadScen
      )
      return()
    }
    scenMetaDbSubset <<- scenMetaDb[vapply(scenMetaDb[["_stag"]], function(tags) {
      any(csv2Vector(tags) %in% input$selLoadScenTags)
    }, logical(1L), USE.NAMES = FALSE), ]
    updateSelectInput(session, "selLoadScen",
      choices = formatScenList(scenMetaDbSubset, uid,
        oderByIdentifier,
        desc = desc
      ),
      selected = input$selLoadScen
    )
  },
  ignoreNULL = FALSE
)

# load scenario button clicked
observeEvent(virtualActionButton(rv$btLoadScen), {
  # fetch list of saved scenarios
  # only load single scenario as not in comparison mode
  loadIntoSandbox <<- FALSE
  errMsg <- NULL
  tryCatch(
    {
      scenMetaDb <<- db$fetchScenList(scode = SCODEMAP[["scen"]])
    },
    error = function(e) {
      flog.error(
        "Problems fetching list of scenarios from database. Error message: %s.",
        conditionMessage(e)
      )
      errMsg <<- lang$errMsg$fetchScenData$desc
    }
  )
  if (is.null(showErrorMsg(lang$errMsg$fetchScenData$title, errMsg))) {
    return()
  }
  if (is.null(scenMetaDb) || !nrow(scenMetaDb)) {
    scenMetaDb <<- tibble(
      `_sid` = integer(),
      `_uid` = character(),
      `_sname` = character(),
      `_stime` = character(),
      `_stag` = character(),
      `_accessr` = character(),
      `_accessw` = character(),
      `_accessx` = character(),
      `_scode` = integer()
    )
  }
  # fetch only those scenarios that are not already loaded into the ui
  uiSidList <- scenData$getRefScenMap()
  uiSidList[["sb"]] <- NULL
  isInMultiCompMode <- isMultiScenCompMode(currentCompMode)
  compRefId <- if (isInMultiCompMode) getMultiCompRefId(currentCompMode)
  sandboxOption <- NULL
  if (!identical(currentCompMode, "split")) {
    sandboxOption <- list(
      name = paste(activeScen$getScenName(), lang$nav$scen$scenNameSandboxSuffix),
      checked = FALSE
    )
  }
  if (length(names(uiSidList))) {
    if (identical(currentCompMode, "split")) {
      splitCompRefIds <- names(uiSidList) %in% c("cmpSplitL", "cmpSplitR")
      scenMetaDb <<- scenMetaDb[!as.character(scenMetaDb[[1]]) %in% unlist(uiSidList[splitCompRefIds],
        use.names = FALSE
      ), ]
      uiSidList <- unique(unlist(uiSidList[!splitCompRefIds],
        use.names = FALSE
      ))
    } else if (isInMultiCompMode) {
      sidsLoadedInRef <- uiSidList[[compRefId]]
      if (length(sidsLoadedInRef)) {
        sandboxOption$checked <- "sb" %in% normalizeSandboxSids(sidsLoadedInRef)
        uiSidList <- sidsLoadedInRef
      } else {
        uiSidList <- unique(unlist(uiSidList[names(uiSidList) != compRefId],
          use.names = FALSE
        ))
      }
    } else {
      tabRefs <- startsWith(names(uiSidList), "cmpTab_")
      sidsAlreadyLoaded <- unlist(uiSidList[tabRefs], use.names = FALSE)
      if ("sb" %in% normalizeSandboxSids(sidsAlreadyLoaded)) {
        sandboxOption <- NULL
      }
      scenMetaDb <<- scenMetaDb[!as.character(scenMetaDb[[1]]) %in% sidsAlreadyLoaded, ]
      uiSidList <- unique(unlist(uiSidList[!tabRefs], use.names = FALSE))
    }
  } else {
    uiSidList <- integer()
  }

  maxNoScenExceeded <- FALSE
  dbTagList <- scenMetaDb[["_stag"]]
  dbTagList <- csv2Vector(dbTagList[dbTagList != ""])
  baselineOption <- NULL
  if (compModeHasBaseline(currentCompMode)) {
    baselineOption <- list(
      choices = if (isTRUE(sandboxOption$checked)) setNames("sb", sandboxOption$name) else character()
    )
  }
  if (!is.null(uiSidList)) {
    uiSidList <- uiSidList[!startsWith(as.character(uiSidList), "sb")]
    if (length(uiSidList)) {
      uiSidList <- bind_rows(scenData$getById("meta", scenIds = uiSidList))
      scenMetaDb <<- bind_rows(
        scenMetaDb,
        uiSidList[uiSidList[["_scode"]] != SCODEMAP["scen"], ]
      )
      if (identical(currentCompMode, "split")) {
        uiSidList <- formatScenList(uiSidList,
          uid, "_stime",
          desc = TRUE
        )
      } else {
        if (length(baselineOption)) {
          baselineOption$choices <- c(baselineOption$choices, formatScenList(uiSidList, uid))
        }
        uiSidList <- paste0(uiSidList[["_sid"]], "_", uiSidList[["_uid"]])
      }
    } else {
      uiSidList <- NULL
    }
  }
  if (length(baselineOption) && length(scenData$getRefScenMap(compRefId))) {
    refSids <- normalizeSandboxSids(scenData$getRefScenMap(compRefId))
    baselineOption$choices <- baselineOption$choices[order(match(sub("_.*$", "", baselineOption$choices), refSids))]
    baselineOption$selected <- baselineOption$choices[1]
  }
  if (nrow(scenMetaDb) > maxNoScenToShow) {
    scenMetaDbSubset <<- scenMetaDb[order(scenMetaDb[["_stime"]],
      decreasing = TRUE
    ), ][seq_len(maxNoScenToShow), ]
    maxNoScenExceeded <- TRUE
  } else {
    scenMetaDbSubset <<- scenMetaDb
    maxNoScenExceeded <- FALSE
  }
  # by default, put most recently saved scenario first
  dbSidList <- formatScenList(scenMetaDbSubset, uid, "_stime", desc = TRUE)
  loadScenSandboxOptionShown <<- FALSE
  loadScenBaselineShown <<- FALSE
  if (length(dbSidList) || length(uiSidList) || length(sandboxOption)) {
    loadScenSandboxOptionShown <<- !is.null(sandboxOption)
    loadScenBaselineShown <<- !is.null(baselineOption)
    showLoadScenDialog(dbSidList, uiSidList, identical(currentCompMode, "split"),
      dbTagList = dbTagList, sandboxOption = sandboxOption, baselineOption = baselineOption
    )
    if (maxNoScenExceeded) {
      showHideEl(session, "#importScenMaxNoScen", 4000L)
    }
  } else {
    return(showErrorMsg(
      lang$nav$dialogLoadScen$titleNoScen,
      lang$nav$dialogLoadScen$descNoScen
    ))
  }
})

# sort by name
observeEvent(input$btSortName, {
  flog.debug(
    "Button to sort scenarios by name clicked (%s).",
    if (btSortNameDesc) "ascending order" else "descending order"
  )
  removeClassEl(session, "#btSortTime", "scen-sort-by-selected")
  addClassEl(session, "#btSortName", "scen-sort-by-selected")
  btSortTime <<- FALSE
  if (btSortNameDesc) {
    updateSelectInput(session, "selLoadScen",
      choices = formatScenList(scenMetaDbSubset,
        uid, "_sname",
        desc = FALSE
      )
    )
    updateActionButton(session, "btSortName",
      label = lang$nav$dialogLoadScen$btSortNameDESC,
      icon = icon("arrow-down-z-a")
    )
    btSortNameDesc <<- FALSE
  } else {
    updateSelectInput(session, "selLoadScen",
      choices = formatScenList(scenMetaDbSubset,
        uid, "_sname",
        desc = TRUE
      )
    )
    updateActionButton(session, "btSortName",
      label = lang$nav$dialogLoadScen$btSortNameASC,
      icon = icon("arrow-down-a-z")
    )
    btSortNameDesc <<- TRUE
  }
})
# sort by time
observeEvent(input$btSortTime, {
  flog.debug(
    "Button to sort scenarios by time clicked (%s).",
    if (btSortTimeDesc) "ascending order" else "descending order"
  )
  removeClassEl(session, "#btSortName", "scen-sort-by-selected")
  addClassEl(session, "#btSortTime", "scen-sort-by-selected")
  btSortTime <<- TRUE
  if (btSortTimeDesc) {
    updateSelectInput(session, "selLoadScen",
      choices = formatScenList(scenMetaDbSubset,
        uid, "_stime",
        desc = FALSE
      )
    )
    updateActionButton(session, "btSortTime",
      label = lang$nav$dialogLoadScen$btSortTimeDESC,
      icon = icon("arrow-down-9-1")
    )
    btSortTimeDesc <<- FALSE
  } else {
    updateSelectInput(session, "selLoadScen", choices = formatScenList(
      scenMetaDbSubset, uid, "_stime",
      desc = TRUE
    ))
    updateActionButton(session, "btSortTime",
      label = lang$nav$dialogLoadScen$btSortTimeASC,
      icon = icon("arrow-down-1-9")
    )
    btSortTimeDesc <<- TRUE
  }
})

observeEvent(input$btRefreshComp, {
  tabsetId <- input$btRefreshComp
  refId <- NULL
  if (identical(tabsetId, 0L)) {
    flog.debug("Refresh scenario in pivot compare mode clicked.")
  } else if (tabsetId %in% c(2L, 3L)) {
    flog.debug("Refresh scenario in split compare mode clicked.")
  } else if (tabsetId < 0L) {
    if (-tabsetId > length(config[["customCompareModules"]])) {
      flog.error("Invalid value for refresh compare mode button received. This is likely an attempt to tamper with the app!")
      return()
    }
    refId <- paste0("cmpCustom_", config[["customCompareModules"]][[-tabsetId]][["id"]])
    if (!is.null(dynamicUILoaded$dynamicTabsets[[refId]])) {
      dynamicUILoaded$dynamicTabsets[[refId]][["content"]][] <<- FALSE
    }
  } else {
    flog.debug("Refresh scenario in tab compare mode clicked.")
  }
  if (is.null(refId)) {
    refId <- tabIdToRef(tabsetId)
    if (!is.null(dynamicUILoaded$dynamicTabsets[[paste0("tab_", tabsetId)]])) {
      dynamicUILoaded$dynamicTabsets[[paste0("tab_", tabsetId)]][["content"]][] <<- FALSE
    }
  }

  scenIds <- scenData$getRefScenMap(refId)
  scenData$invalidateCache(scenIds)
  sbScenId <- startsWith(as.character(scenIds), "sb")
  if (any(sbScenId)) {
    if (tryCatch(
      {
        scenData$loadSandbox(
          getInputDataFromSandbox(),
          if (length(modelInFileNames)) modelInFileNames else character(),
          activeScen$getMetadataDf()
        )
        FALSE
      },
      no_data = function(e) {
        flog.error(conditionMessage(e))
        showErrorMsg(lang$errMsg$GAMSInput$title, conditionMessage(e))
        return(TRUE)
      },
      error = function(e) {
        flog.error("Unexpected error while fetching input data from sandbox. Error message: '%s'", conditionMessage(e))
        showErrorMsg(lang$errMsg$GAMSInput$title, lang$errMsg$unknownError)
        return(TRUE)
      }
    )) {
      return()
    }
    if (tabsetId > 0L) {
      views$duplicateSandboxConf(tabsetId)
    }
  }
  sidsToLoad <- as.integer(scenIds[!sbScenId])
  if (length(config$scripts$base) && tabsetId > 0L) {
    if (any(sbScenId)) {
      scriptOutput$loadResultsBase(scriptOutput$getResults(), tabsetId)
    } else if (length(sidsToLoad)) {
      scriptOutput$loadResultsBase(db$loadScriptResults(sidsToLoad)[[1]], tabsetId)
    }
  }
  # initialize metadata
  if (tryCatch(
    {
      scenData$load(sidsToLoad,
        symNames = character(), showProgress = FALSE,
        refId = refId, registerRef = FALSE
      )
      FALSE
    },
    error = function(e) {
      flog.error("Unexpected error while refreshing data. Error message: '%s'", conditionMessage(e))
      showErrorMsg(lang$errMsg$GAMSInput$title, lang$errMsg$unknownError)
      return(TRUE)
    }
  )) {
    return()
  }
  if (tabsetId <= 0L) {
    compMode <- if (tabsetId == 0L) "pivot" else config[["customCompareModules"]][[-tabsetId]][["id"]]
    loadCompViews(refId, tabsetId, compMode)
  } else if (length(sidsToLoad)) {
    inputDataSids <- scenData$getInputDataSids(sidsToLoad)
    inputDataSids[is.na(inputDataSids)] <- sidsToLoad[is.na(inputDataSids)]
    views$loadConf(
      db$importDataset(
        tableName = "_scenViews",
        subsetSids = inputDataSids
      ), FALSE,
      tabsetId, inputDataSids
    )
  }
  if (tabsetId >= 0L) {
    sheetNames <- getSheetnamesByTabsetId(tabsetId)
    if (length(sheetNames)) {
      loadDynamicTabContent(session, tabsetId,
        sheetNames,
        initEnv = TRUE
      )
    }
  } else {
    loadDynamicTabContentCustom(session, config[["customCompareModules"]][[-tabsetId]], initEnv = TRUE)
  }

  metaTmp <- scenData$getById("meta", refId = refId, drop = TRUE)
  showElReplaceTxt(
    session, paste0("#cmpScenTitle_", tabsetId),
    paste0(
      if (!identical(uid, metaTmp[["_uid"]][1])) paste0(metaTmp[["_uid"]][1], ": "),
      metaTmp[["_sname"]][1]
    )
  )
  showElReplaceTxt(
    session, paste0("#cmpScenDate_", tabsetId),
    format(as.POSIXct(metaTmp[["_stime"]][1]), "%Y-%m-%d %H:%M:%S")
  )
})

# load scenario confirmed
observeEvent(input$btLoadScenConfirm, {
  flog.debug("Confirm load scenario button clicked.")
  includeSandbox <- FALSE
  if (identical(isolate(input$miroSidebar), "scenarios") &&
    identical(isolate(input$tabsetLoadScen), "loadScenUI")) {
    scenSelected <- isolate(input$selLoadScenUI)
  } else {
    scenSelected <- isolate(input$selLoadScen)
    includeSandbox <- !isInSolveMode && loadScenSandboxOptionShown &&
      isTRUE(isolate(input$cbLoadScenSandbox))
  }
  if (length(scenSelected) && !is.character(scenSelected)) {
    flog.error("Invalid scenario id(s) selected. This is likely an attempt to tamper with the app!")
    return()
  }
  # selectize reports an empty string when nothing is selected
  scenSelected <- scenSelected[nzchar(scenSelected)]
  if (!length(scenSelected) && !includeSandbox) {
    return()
  }
  sidsSelected <- lapply(strsplit(scenSelected, split = "_", fixed = TRUE), "[[", 1)
  if (length(sidsSelected) && (!length(scenMetaDb) ||
    !all(unlist(sidsSelected, use.names = FALSE) %in% as.character(scenMetaDb[["_sid"]])))) {
    flog.error("Invalid scenario id(s) selected. This is likely an attempt to tamper with the app!")
    return()
  }
  if (includeSandbox) {
    sidsSelected <- c(list("sb"), sidsSelected)
  }
  if (!isInSolveMode && loadScenBaselineShown) {
    sidsSelected <- orderByBaseline(sidsSelected, parseBaselineSid(isolate(input$selLoadScenBaseline)))
  }
  loadIntoSandbox <<- FALSE
  sidsToLoad <<- sidsSelected

  # if in comparison mode skip input data check
  if (!isInSolveMode) {
    rv$btOverwriteScen <- rv$btOverwriteScen + 1L
    return()
  }
  if (identical(input$cbSelectManuallyDb, TRUE)) {
    inputIdsToLoad <- match(input$selInputDataDb, names(modelIn))
    if (!length(inputIdsToLoad)) {
      return()
    }
    if (any(is.na(inputIdsToLoad))) {
      flog.error(
        "Invalid datasets selected to be imported from database: %s. This seems like an attempt to tamper with the app!",
        input$selInputDataDb[is.na(inputIdsToLoad)]
      )
      return()
    }
    manualImport <- TRUE
  } else {
    inputIdsToLoad <- seq_along(modelIn)
    manualImport <- FALSE
  }
  checkIfInputDataExists(
    inputIdsToLoad = inputIdsToLoad,
    manualImport = manualImport
  )
})

observeEvent(input$btOverwriteScen, {
  flog.debug("Clear sandbox data button clicked.")
  if (!length(sidsToLoad)) {
    return()
  }
  overwriteInput <<- 1L
  rv$btOverwriteScen <- rv$btOverwriteScen + 1L
})

observeEvent(input$btBatchLoadSbOverwrite, {
  flog.debug("Batch load: overwriting current content in sandbox confirmed.")
  overwriteInput <<- 1L
  switchTab(session, "input")
  rv$btOverwriteScen <- rv$btOverwriteScen + 1L
})

observeEvent(input$btBatchLoadSb, {
  flog.debug("Load scenario into sandbox from Batch Load module button clicked.")
  isInSolveMode <<- TRUE
  loadIntoSandbox <<- FALSE
  if (identical(isolate(rv$sandboxUnsaved), TRUE) ||
    any(vapply(modelInTabularData,
      sandboxInputData$hasData, logical(1),
      USE.NAMES = FALSE
    ))) {
    hideEl(session, ".batch-load-content")
    showEl(session, ".batch-load-sb-content")
  } else {
    overwriteInput <<- 1L
    switchTab(session, "input")
    rv$btOverwriteScen <- rv$btOverwriteScen + 1L
  }
})

observeEvent(input$btBatchCompare, {
  batchCompare(input$btBatchCompare)
})
observeEvent(input$btBatchCompareConfirm, {
  batchCompare(input$btBatchCompareConfirm,
    includeSandbox = isTRUE(input$cbBatchCompareSandbox),
    baselineSid = parseBaselineSid(input$selBatchCompareBaseline)
  )
})
batchCompare <- function(compareMode, includeSandbox = FALSE, baselineSid = NULL) {
  if (!is.character(compareMode) || length(compareMode) != 1L) {
    flog.error("Invalid batch compare mode selected. This is likely an attempt to tamper with the app!")
    return()
  }
  compareOptions <- getBatchCompareOptions(compareMode, length(sidsToLoad))
  includeSandbox <- includeSandbox && compareOptions$sandbox && !"sb" %in% sidsToLoad
  noScenToLoad <- length(sidsToLoad) + includeSandbox
  if (identical(compareMode, "pivot")) {
    viewMode <- "pivotView"
    currentCompMode <<- "pivot"
  } else if (identical(compareMode, "tab")) {
    viewMode <- "tabView"
    currentCompMode <<- "tab"
  } else if (identical(compareMode, "split")) {
    viewMode <- "splitView"
    currentCompMode <<- "split"
    sidInLeftSplit <- length(scenData$getRefScenMap("cmpSplitL"))
    sidInRightSplit <- length(scenData$getRefScenMap("cmpSplitR"))
    if (noScenToLoad > (2L - sum(sidInLeftSplit, sidInRightSplit))) {
      flog.info("Closing currently open scenarios in split comparison mode to load new ones.")
      loadInLeftBoxSplit <<- TRUE
      closeScenSplitBox(2L)
      if (identical(noScenToLoad, 2L)) {
        closeScenSplitBox(3L)
      }
    } else {
      loadInLeftBoxSplit <<- identical(sidInLeftSplit, 0L)
    }
  } else if (identical(compareMode, "aiCompare")) {
    viewMode <- "aiCompare"
    removeModal()
  } else {
    viewMode <- compareMode
    if (!viewMode %in% names(config[["customCompareModules"]])) {
      flog.error("Invalid batch compare mode selected. This is likely an attempt to tamper with the app!")
      return()
    }
    currentCompMode <<- viewMode
  }
  disableEl(session, "#btBatchCompare")
  disableEl(session, "#btBatchCompareConfirm")
  scenIdNameMap <- character(0L)
  if (length(input$batchCompareNameCols)) {
    if (tryCatch(
      {
        mapColIds <- match(input$batchCompareNameCols, names(batchLoadData))
        if (any(is.na(mapColIds))) {
          stop(sprintf(
            "Invalid column(s): %s. This is likely an attempt to tamper with the app!",
            paste(input$batchCompareNameCols[is.na(mapColIds)], collapse = ", ")
          ))
        }
        rowIdsToPick <- match(sidsToLoad, batchLoadData[[1]])
        if (any(is.na(rowIdsToPick))) {
          stop(sprintf(
            "Setting scenIdNameMap: Invalid sid(s): %s. This is likely an attempt to tamper with the app!",
            paste(sidsToLoad[is.na(rowIdsToPick)], collapse = ", ")
          ))
        }
        scenNamesPrefixTmp <- input$prefixBatchCompareNameCols
        if (length(scenNamesPrefixTmp) != 1L || !is.character(scenNamesPrefixTmp)) {
          stop("Invalid prefix for scenario names. This is likely an attempt to tamper with the app!")
        }
        scenNamesTmp <- paste0(scenNamesPrefixTmp, unite(batchLoadData[rowIdsToPick, mapColIds],
          "name",
          sep = "_"
        )[[1]])
        scenIdNameMap <- setNames(scenNamesTmp, as.character(sidsToLoad))
        FALSE
      },
      error = function(e) {
        flog.error(
          "Unexpected error while setting scenIdNameMap. Error message: %s",
          conditionMessage(e)
        )
        return(TRUE)
      }
    )) {
      return(showErrorMsg(lang$errMsg$loadScen$title, lang$errMsg$unknownError))
    }
  }
  flog.debug("Load batch of scenarios to compare mode (%s) button clicked.", viewMode)
  if (identical(viewMode, "aiCompare")) {
    return(openAiCompareContext(sidsToLoad, scenIdNameMap, includeSandbox))
  } else {
    scenData$setScenIdNameMap(scenIdNameMap)
  }
  if (includeSandbox) {
    sidsToLoad <<- c("sb", sidsToLoad)
  }
  if (compareOptions$baseline) {
    sidsToLoad <<- orderByBaseline(sidsToLoad, baselineSid)
  }
  pivotCompRefreshAll <<- TRUE
  isInSolveMode <<- FALSE
  loadIntoSandbox <<- FALSE
  switchCompareMode(session, viewMode, length(sidsToLoad), config[["customCompareModules"]])
  rv$btOverwriteScen <- rv$btOverwriteScen + 1L
}

observeEvent(virtualActionButton(rv$btOverwriteScen), {
  flog.debug(
    "Loading and rendering scenarios: '%s'.",
    paste(sidsToLoad, collapse = ", ")
  )
  refreshAllScenTmp <- pivotCompRefreshAll
  pivotCompRefreshAll <<- FALSE
  suppressCloseModalLocal <- suppressCloseModal
  suppressCloseModal <<- FALSE
  if (!length(sidsToLoad)) {
    return()
  }
  isInMultiCompMode <- !isInSolveMode && isMultiScenCompMode(currentCompMode)
  sidsToLoadVector <- unlist(sidsToLoad, use.names = FALSE)

  if (sum(!occupiedSidSlots) < length(sidsToLoad)) {
    flog.info(
      "Maximum number of scenarios in scenario comparison mode reached (%d).",
      length(occupiedSidSlots)
    )
    return(showErrorMsg(lang$errMsg$maxScen$title, lang$errMsg$maxScen$desc))
  }
  if (isInMultiCompMode && isInRefreshMode$get()) {
    on.exit(hideEl(session, "#loading-screen"), add = TRUE)
  }
  if (isInSolveMode) {
    refId <- "sb"
    viewsSids <- 1L
    symToFetch <- NULL
  } else {
    # get names of symbols on first tab
    if (length(outputTabs)) {
      symToFetch <- names(modelOut)[outputTabs[[1]]]
    } else {
      symToFetch <- scenInputTabs[[1]]
      if (identical(sheetName, 0L)) {
        symToFetch <- scalarsFileName
      } else {
        symToFetch <- names(modelIn)[symToFetch]
      }
    }
    if (refreshAllScenTmp) {
      scenData$invalidateCache(sidsToLoadVector)
    }
    if (identical(currentCompMode, "pivot")) {
      refId <- "cmpPivot"
      viewsSids <- 0L
      # check if we are modifying already opened scenarios
      sidsInPivotComp <- scenData$getRefScenMap(refId)
      if (length(sidsInPivotComp)) {
        if (!is.null(dynamicUILoaded$dynamicTabsets[["tab_0"]])) {
          dynamicUILoaded$dynamicTabsets[["tab_0"]][["content"]][] <<- FALSE
        }
        sidsToRemoveFromPivotComp <- !sidsInPivotComp %in% sidsToLoadVector
        if (any(sidsToRemoveFromPivotComp)) {
          scenData$clear(refId, sidsInPivotComp[sidsToRemoveFromPivotComp])
        }
        if (isGroupOfSheets[[1]]) {
          tabId <- 1
        }
        symToFetch <- getSheetnamesByTabsetId("0")
      }
    } else if (identical(currentCompMode, "tab")) {
      sidsInTabComp <- scenData$getRefScenMap()
      sidsInTabComp[["sb"]] <- NULL
      if (length(sidsInTabComp)) {
        tabRefs <- startsWith(names(sidsInTabComp), "cmpTab_")
        sidsInTabComp <- sidsToLoadVector %in% normalizeSandboxSids(unlist(sidsInTabComp[tabRefs], use.names = FALSE))
        sidsToLoad <- sidsToLoad[!sidsInTabComp]
        sidsToLoadVector <- sidsToLoadVector[!sidsInTabComp]
      }
      refId <- NULL
      viewsSids <- which(!occupiedSidSlots)[seq_along(sidsToLoadVector)] + 3
    } else if (identical(currentCompMode, "split")) {
      sidsInSplitComp <- scenData$getRefScenMap()
      sidsInSplitComp[["sb"]] <- NULL
      if (length(sidsInSplitComp)) {
        splitRefs <- names(sidsInSplitComp) %in% c("cmpSplitL", "cmpSplitR")
        sidsInSplitComp <- sidsToLoadVector %in% unlist(sidsInSplitComp[splitRefs], use.names = FALSE)
        sidsToLoad <- sidsToLoad[!sidsInSplitComp]
        sidsToLoadVector <- sidsToLoadVector[!sidsInSplitComp]
      }
      if (identical(length(sidsToLoadVector), 2L)) {
        refId <- NULL
        viewsSids <- c(2L, 3L)
      } else if (loadInLeftBoxSplit) {
        refId <- "cmpSplitL"
        viewsSids <- 2L
      } else {
        refId <- "cmpSplitR"
        viewsSids <- 3L
      }
    } else if (!currentCompMode %in% names(config[["customCompareModules"]])) {
      flog.error("Invalid state (not in solve mode, and currentCompMode is neither: pivot, tab, split nor a custom analysis mode. Please contact GAMS!.")
      return(showErrorMsg(lang$errMsg$loadScen$title, lang$errMsg$unknownError))
    } else {
      refId <- getMultiCompRefId(currentCompMode)
      viewsSids <- -config[["customCompareModules"]][[currentCompMode]][["idx"]]
      scenData$clear(refId)
      if (!is.null(dynamicUILoaded$dynamicTabsets[[refId]])) {
        dynamicUILoaded$dynamicTabsets[[refId]][["content"]][] <<- FALSE
      }
      symToFetch <- character(0L)
    }
  }
  errMsg <- NULL
  tryCatch(
    {
      scriptDataTmp <- NULL
      sandboxId <- 1L
      if ("sb" %in% sidsToLoadVector) {
        if (tryCatch(
          {
            scenData$loadSandbox(
              getInputDataFromSandbox(),
              if (length(modelInFileNames)) modelInFileNames else character(),
              activeScen$getMetadataDf()
            )
            if (length(refId)) {
              # there are multiple ref ids in tab comparison mode.
              scenData$addRefId(refId, "sb")
            }
            FALSE
          },
          no_data = function(e) {
            flog.error(conditionMessage(e))
            showErrorMsg(lang$errMsg$GAMSInput$title, conditionMessage(e))
            return(TRUE)
          },
          error = function(e) {
            flog.error("Unexpected error while fetching input data from sandbox. Error message: '%s'", conditionMessage(e))
            showErrorMsg(lang$errMsg$GAMSInput$title, lang$errMsg$unknownError)
            return(TRUE)
          }
        )) {
          return()
        }
        sandboxId <- which("sb" == sidsToLoadVector)[1]
        sidsToLoadVector <- sidsToLoadVector[-sandboxId]
        if (!isInMultiCompMode) {
          views$duplicateSandboxConf(viewsSids[sandboxId])
        }
        if (length(config$scripts$base)) {
          scriptDataTmp <- scriptOutput$getResults()
        }
        if (length(refId) && !isInMultiCompMode) {
          viewsSids <- viewsSids[-sandboxId]
        }
        snameTmp <- paste(activeScen$getScenName(), lang$nav$scen$scenNameSandboxSuffix)
      }
      if (length(sidsToLoadVector)) {
        if (length(refId)) {
          # there are multiple ref ids in tab comparison mode.
          scenData$load(as.integer(sidsToLoadVector),
            refId = refId,
            symNames = symToFetch
          )
          if (!isInMultiCompMode) {
            if (identical(refId, "sb")) {
              inputDataSids <- sidsToLoadVector
            } else {
              inputDataSids <- scenData$getInputDataSids(sidsToLoadVector)
              inputDataSids[is.na(inputDataSids)] <- sidsToLoadVector[is.na(inputDataSids)]
            }
            views$loadConf(
              db$importDataset(
                tableName = "_scenViews",
                subsetSids = inputDataSids
              ), isInSolveMode,
              viewsSids, inputDataSids
            )
          }
        }
        scriptDataTmp <- append(
          db$loadScriptResults(sidsToLoadVector,
            msgProgress = lang$progressBar$loadScenDb
          ),
          scriptDataTmp, sandboxId - 1
        )
      }
      if (isInMultiCompMode) {
        scenData$orderRef(refId, unlist(sidsToLoad, use.names = FALSE))
        loadCompViews(refId, viewsSids, currentCompMode)
      }
    },
    error = function(e) {
      flog.error(
        "Some error occurred loading scenarios: '%s' from database. Error message: %s.",
        paste(sidsToLoad, collapse = ", "), conditionMessage(e)
      )
      errMsg <<- lang$errMsg$loadScen$desc
    }
  )
  if (is.null(showErrorMsg(lang$errMsg$loadScen$title, errMsg))) {
    return()
  }

  if (isInSolveMode) {
    # close currently opened scenario
    if (!closeScenario(clearMeta = FALSE)) {
      return()
    }
    if (!loadIntoSandbox) {
      tryCatch(
        {
          activeScen <<- Scenario$new(
            db = db, sid = sidsToLoad[[1]],
            views = views, attachments = attachments,
            rv = rv
          )
        },
        error = function(e) {
          flog.error(
            "Error generating new Scenario object. Error message: '%s'.",
            conditionMessage(e)
          )
          errMsg <<- lang$errMsg$loadScen$desc
        }
      )
      if (is.null(showErrorMsg(lang$errMsg$loadScen$title, errMsg))) {
        return()
      }
      if (length(activeScen$getLockUid())) {
        showNotification(
          tagList(
            tags$b(lang$nav$dialogLockScen$title), tags$br(),
            tags$span(sprintf(
              lang$nav$dialogLockScen$desc,
              activeScen$getLockUid()
            ))
          ),
          duration = 10,
          type = "warning"
        )
      }
    }

    # check whether all input datasets were imported
    if (length(inputDsNames)) {
      scenInputData <- scenData$get("sb", symNames = inputDsNames)
    } else {
      scenInputData <- list()
    }
    if (length(inputDsNames) != length(scenInputData)) {
      flog.error(
        "Length of data provided (%d) does not match the number of input sheets (%d).",
        length(scenInputData), length(inputDsNames)
      )
      return(showErrorMsg(
        lang$nav$dialogLoadScen$titleDataError,
        lang$nav$dialogLoadScen$descDataError
      ))
    }
    names(scenInputData) <- inputDsNames

    if (scalarsFileName %in% inputDsNames) {
      scalarDataset <- scenInputData[[length(scenInputData)]]
    } else {
      scalarDataset <- NULL
    }

    errMsg <- NULL
    loadMode <- "scen"
    newInputCount <- 0L
    datasetsToFetch <- names(modelIn)
    dfClArgs <- NULL
    source("./modules/input_load.R", local = TRUE)
    if (is.null(errMsg) && isFALSE(suppressCloseModalLocal)) {
      removeModal()
    }

    # render output data
    noOutputData <<- !scenData$getSandboxHasOutputData(scriptOutput)
    # rendering tables and graphs
    renderOutputData()

    # load script results
    if (length(config$scripts$base)) {
      scriptOutput$loadResultsBase(scriptDataTmp[[1]])
    }

    if (loadIntoSandbox) {
      flog.debug(
        "Scenario data from a scenario with id: '%s' was loaded into sandbox.",
        sidsToLoad[[1]]
      )
      markUnsaved(markClean = TRUE)
    } else {
      flog.debug("Scenario: '%s' was loaded into UI", activeScen$getSid())
    }
    # function ends here in case not in compare mode!
    return()
  }
  # scenario comparison mode

  if (identical(currentCompMode, "pivot")) {
    if (!dynamicUILoaded$compareModeTabsets[3]) {
      dynamicUILoaded$compareModeTabsets[3] <<- TRUE
      insertUI("#pivotCompScenWrapper",
        where = "afterBegin",
        generateScenarioTabset(0L,
          pivotCompare = TRUE,
          maxTabsExpanded = if (is.null(config$layoutSettings$maxTabsExpandedPivotComp)) 5L else config$layoutSettings$maxTabsExpandedPivotComp
        ),
        immediate = TRUE
      )
    }
    return(tryCatch(
      {
        isInRefreshMode$set(TRUE)
        loadDynamicTabContent(session, 0L, symToFetch, initEnv = TRUE)
        hideEl(session, "#pivotCompBtWrapper")
        showEl(session, "#pivotCompScenWrapper")
        switchTab(session, "scenComp")
        enableEl(session, "#btClosePivotComp")
        removeModal()
        flog.debug(
          "Scenarios: '%s' loaded and rendered in scenario comparison mode (pivot view).",
          paste(sidsToLoad, collapse = ", ")
        )
      },
      error = function(e) {
        flog.warn("Problems rendering scenarios in pivot compare mode. Error message: %s", conditionMessage(e))
        showErrorMsg(lang$errMsg$loadScen$title, lang$errMsg$loadScen$desc)
      }
    ))
  } else if (currentCompMode %in% names(config[["customCompareModules"]])) {
    moduleIdx <- config[["customCompareModules"]][[currentCompMode]][["idx"]]
    return(tryCatch(
      {
        isInRefreshMode$set(TRUE)
        noError <- loadDynamicTabContentCustom(session, config[["customCompareModules"]][[currentCompMode]], initEnv = TRUE)
        hideEl(session, paste0("#cmpCustomNoScenWrapper_", moduleIdx))
        showEl(session, paste0("#customCompScenWrapper_", moduleIdx))
        switchTab(session, "scenComp")
        enableEl(session, paste0("#btRefreshCustomCmp_", moduleIdx))
        if (noError) {
          removeModal()
        }
        flog.debug(
          "Scenarios: '%s' loaded and rendered in custom scenario comparison mode (%s).",
          paste(sidsToLoad, collapse = ", "),
          currentCompMode
        )
      },
      error = function(e) {
        flog.warn("Problems rendering scenarios in custom compare mode (%s). Error message: %s", currentCompMode, conditionMessage(e))
        showErrorMsg(lang$errMsg$loadScen$title, lang$errMsg$loadScen$desc)
      }
    ))
  }
  # split/tab comparison mode
  errMsg <- NULL
  lastImportedTabsetId <- NULL

  lapply(seq_along(sidsToLoad), function(i) {
    tryCatch(
      {
        refIdToLoad <- NULL
        if (identical(currentCompMode, "tab")) {
          scenId <- which.min(occupiedSidSlots) + 3L
          refIdToLoad <- paste0("cmpTab_", scenId)
        } else {
          if (identical(length(sidsToLoad), 2L)) {
            scenId <- i + 1L
            scenData$load(sidsToLoad[[i]],
              symNames = symToFetch,
              showProgress = TRUE,
              refId = if (identical(scenId, 2L)) "cmpSplitL" else "cmpSplitR"
            )
          } else if (loadInLeftBoxSplit) {
            scenId <- 2L
          } else {
            scenId <- 3L
          }
          if (!dynamicUILoaded$compareModeTabsets[scenId - 1L]) {
            dynamicUILoaded$compareModeTabsets[scenId - 1L] <<- TRUE
            insertUI(paste0("#scenSplit", scenId - 1L, "_content"),
              where = "afterBegin",
              generateScenarioTabsetSplit(scenId), immediate = TRUE
            )
          }
        }
        if (length(refIdToLoad)) {
          scenData$load(sidsToLoad[[i]],
            symNames = symToFetch,
            showProgress = TRUE, refId = refIdToLoad
          )
          if (!identical(sidsToLoad[[i]], "sb")) {
            inputDataSids <- scenData$getInputDataSids(sidsToLoad[[i]])
            inputDataSids[is.na(inputDataSids)] <- sidsToLoad[[i]][is.na(inputDataSids)]
            views$loadConf(
              db$importDataset(
                tableName = "_scenViews",
                subsetSids = inputDataSids
              ), FALSE,
              viewsSids[[i]], inputDataSids
            )
          }
        }
        renderScenInCompMode(scenId, refreshData = FALSE)
        # load script results
        if (length(config$scripts$base)) {
          scriptOutput$loadResultsBase(scriptDataTmp[[i]], scenId)
        }
      },
      error = function(e) {
        flog.error(e)
        errMsg <<- lang$errMsg$loadScen$desc
      }
    )

    flog.debug("Scenario: '%s' loaded into UI (compare mode).", sidsToLoad[[i]])
    if (identical(currentCompMode, "tab")) {
      lastImportedTabsetId <<- scenId
      occupiedSidSlots[scenId - 3] <<- TRUE
    }
  })
  switchTab(session, "scenComp")
  if (identical(currentCompMode, "tab")) {
    updateTabsetPanel(session, "scenTabset", selected = paste0("scen_", lastImportedTabsetId, "_"))
  }
  if (is.null(showErrorMsg(lang$errMsg$loadScen$title, errMsg))) {
    return()
  }

  removeModal()
  flog.debug(
    "Scenarios: '%s' loaded and rendered in scenario comparison mode.",
    paste(sidsToLoad, collapse = ", ")
  )
  return()
})

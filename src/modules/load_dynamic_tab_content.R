getSheetnamesByTabsetId <- function(tabsetId){
  if(identical(tabsetId, 1L)){
    tabsetName <- "outputTabset"
    isOutputTabset <- TRUE
  }else{
    tabsetName <- paste0("contentScen_", tabsetId)
    isOutputTabset <- FALSE
  }
  tabIdFull <- isolate(input[[tabsetName]])
  if(is.null(tabIdFull)){
    return()
  }
  groupId <- as.integer(strsplit(tabIdFull, "_", fixed = TRUE)[[1]][3L - isOutputTabset])
  tabId <- NULL
  if(groupId <= length(isGroupOfSheets) && isGroupOfSheets[[groupId]]){
    tabId <- as.integer(strsplit(isolate(input[[paste0(tabsetName, "_", groupId)]]),
                                 "_", fixed = TRUE)[[1L]][[4L - isOutputTabset]])
  }
  if(groupId > length(outputTabs)){
    if(isOutputTabset){
      return()
    }
    groupId <- groupId - length(outputTabs)
    if(groupId > length(scenInputTabs)){
      # is script tab
      return()
    }
    sheetName <- scenInputTabs[[groupId]]
    if(identical(sheetName, 0L)){
      sheetNames <- scalarsFileName
    }else{
      sheetNames <- names(modelIn)[sheetName]
    }
  }else{
    sheetNames <- names(modelOut)[outputTabs[[groupId]]]
  }
  if(is.null(tabId)){
    return(sheetNames)
  }
  return(sheetNames[tabId])
}
getPivotCompGraphConfig <- function(sheetName){
  if(sheetName %in% names(ioConfig$modelOut)){
    graphConfig <- list(outType = "miroPivot",
                        options = list(resetOnInit = isFALSE(isInRefreshMode),
                                       "_metadata_" = list(symname = sheetName,
                                                           headers = c(list("_scenName" = list(alias = lang$nav$scen$pivot$scenColName,
                                                                                               type = "string")),
                                                                       ioConfig$modelOut[[sheetName]]$headers),
                                                           symtype = if(identical(sheetName, scalarsOutName)) "parameter" else ioConfig$modelOut[[sheetName]]$symtype)))
  }else if(!sheetName %in% names(ioConfig$modelIn[[sheetName]]) && identical(sheetName, scalarsFileName)){
    graphConfig <- list(outType = "miroPivot",
                        options = list(resetOnInit = isFALSE(isInRefreshMode),
                                       "_metadata_" = list(symname = sheetName,
                                                           headers = c(list("_scenName" = list(alias = lang$nav$scen$pivot$scenColName,
                                                                                               type = "string")),
                                                                       list(scalar = list(alias = lang$nav$scalarAliases$cols$name,
                                                                                          type = "string"),
                                                                            description = list(alias = lang$nav$scalarAliases$cols$desc,
                                                                                               type = "string"),
                                                                            value = list(alias = lang$nav$scalarAliases$cols$value,
                                                                                         type = "string"))),
                                                           symtype = "parameter")))
  }else{
    graphConfig <- list(outType = "miroPivot",
                        options = list(resetOnInit = isFALSE(isInRefreshMode),
                                       "_metadata_" = list(symname = sheetName,
                                                           headers = c(list("_scenName" = list(alias = lang$nav$scen$pivot$scenColName,
                                                                                               type = "string")),
                                                                       ioConfig$modelIn[[sheetName]]$headers),
                                                           symtype = ioConfig$modelIn[[sheetName]]$symtype)))
  }
  if(isTRUE(config$pivotCompSettings$enableHideEmptyCols)){
    graphConfig$options$enableHideEmptyCols <- TRUE
    graphConfig$options$emptyUEL <- config$pivotCompSettings$emptyUEL
  }
  return(graphConfig)
}
loadDynamicTabContent <- function(session, tabsetId, sheetNames, initEnv = FALSE){
  errMsg <- NULL
  tabsetIdChar <- paste0("tab_", tabsetId)
  isInPivotComp <- identical(tabsetId, 0L)
  isOutputTabset <- identical(tabsetId, 1L)
  refId <- tabIdToRef(tabsetId)
  showLoadingScreen(session, 500)
  on.exit(hideLoadingScreen(session))
  
  if(is.null(dynamicUILoaded$dynamicTabsets[[tabsetIdChar]])){
    dynamicUILoaded$dynamicTabsets[[tabsetIdChar]] <<- list(ui = vector("logical", length(configGraphsOut) + length(modelIn)),
                                                            content = vector("logical", length(configGraphsOut) + length(modelIn)))
  }
  if(initEnv){
    flog.trace("(Re)initializing environment for ref ID: %s", refId)
    if(is.null(rendererEnv[[refId]])){
      rendererEnv[[refId]] <<- new.env(parent = emptyenv())
    }else{
      for(el in ls(envir = rendererEnv[[refId]])){
        if("Observer" %in% class(rendererEnv[[refId]][[el]])){
          rendererEnv[[refId]][[el]]$destroy()
        }
      }
    }
  }
  if(!length(sheetNames)){
    return()
  }
  for(sheetName in sheetNames){
    if(isOutputTabset){
      tabId <- match(sheetName, names(modelOut))
    }else{
      tabId <- match(sheetName, scenTableNamesToDisplay)
    }
    if(isInPivotComp){
      graphConfig <- getPivotCompGraphConfig(sheetName)
    }else{
      if(identical(sheetName, scalarsFileName)){
        graphConfig <- getScenTabData(scalarsFileName)
        graphConfig <- graphConfig$graphConfig
      }else{
        sheetId <- match(sheetName, names(modelOut))
        if(is.na(sheetId)){
          sheetId <- match(sheetName, names(modelIn))
          if(is.na(sheetId)){
            flog.error("Sheet name: '%s' could not be found in either names of input or output symbols. Should never happen!",
                       sheetName)
            return(showErrorMsg(lang$errMsg$loadScen$title,
                                lang$errMsg$loadScen$desc))
          }
          graphConfig <- configGraphsIn[[sheetId]]
        }else{
          graphConfig <- configGraphsOut[[sheetId]]
        }
      }
    }
    if(dynamicUILoaded$dynamicTabsets[[tabsetIdChar]][["ui"]][tabId]){
      if(!isOutputTabset){
        # we don't want to switch view when viewing sandbox output tables
        showEl(session, paste0("#scenGraph_", tabsetId, "_", tabId))
        hideEl(session, paste0("#scenTable_", tabsetId, "_", tabId))
      }
    }else{
      flog.trace("Rendering UI elements for tabset: %s on tab: %s (sheetname: %s)",
                 tabsetId, tabId, sheetName)
      tryCatch({
        insertUI(paste0("#scenGraph_", tabsetId, "_", tabId),
                 ui = renderDataUI(paste0(tabsetIdChar,
                                          "_", tabId), 
                                   type = graphConfig$outType, 
                                   graphTool = graphConfig$graph$tool, 
                                   customOptions = graphConfig$options,
                                   filterOptions = graphConfig$graph$filter,
                                   height = graphConfig$height,
                                   modelDir = modelDir,
                                   createdDynamically = FALSE),
                 immediate = TRUE)
        dynamicUILoaded$dynamicTabsets[[tabsetIdChar]][["ui"]][tabId] <<- TRUE
      }, error = function(e) {
        flog.error("Problems rendering UI elements for scenario dataset: '%s'. Error message: %s.", 
                   sheetName, conditionMessage(e))
        errMsg <<- lang$errMsg$loadScen$desc
      })
      if(is.null(showErrorMsg(lang$errMsg$loadScen$title, errMsg))){
        return()
      }
      if(!isInPivotComp){
        tryCatch({
          insertUI(paste0("#scenTable_", tabsetId, "_", tabId),
                   ui = renderDataUI(paste0("table_", tabsetIdChar, "_",
                                            tabId), type = "datatable",
                                     createdDynamically = FALSE),
                   immediate = TRUE)
        }, error = function(e) {
          flog.error("Problems rendering table UI for scenario dataset: '%s'. Error message: %s.", 
                     sheetName, conditionMessage(e))
          errMsg <<- lang$errMsg$loadScen$desc
        })
        if(is.null(showErrorMsg(lang$errMsg$loadScen$title, errMsg))){
          return()
        }
      }
    }
    if(!dynamicUILoaded$dynamicTabsets[[tabsetIdChar]][["content"]][tabId]){
      flog.trace("Rendering content for tabset: %s on tab: %s (sheetname: %s)", tabsetId, tabId, sheetName)
      if(isInPivotComp){
        tryCatch({
          dataToRender <- scenData$getAll(refId, symName = sheetName)
          names(dataToRender) <- vapply(scenData$getById("meta", refId = refId), function(scenMeta){
            if(identical(scenMeta[["_uid"]][1], uid)){
              return(scenMeta[["_sname"]][1])
            }
            return(paste0(scenMeta[["_uid"]][1], ": ", scenMeta[["_sname"]][1]))
          }, character(1L), USE.NAMES = FALSE)
          dataToRender <- bind_rows(dataToRender, .id = "_scenName")
          callModule(renderData, paste0(tabsetIdChar, "_", tabId), 
                     type = "miroPivot", 
                     data = dataToRender,
                     customOptions = graphConfig$options,
                     roundPrecision = roundPrecision,
                     rendererEnv = rendererEnv[[refId]],
                     views = views, attachments = attachments)
          dynamicUILoaded$dynamicTabsets[[tabsetIdChar]][["content"]][tabId] <<- TRUE
          if(any(unlist(scenData$getById("dirty", refId = refId, drop = TRUE), use.names = FALSE))){
            showErrorMsg(lang$errMsg$loadScen$title, lang$errMsg$loadScen$inconsistentDataWarning)
          }
        }, error = function(e) {
          flog.error("Problem rendering graphs for dataset: '%s'. Error message: %s.",
                     sheetName, conditionMessage(e))
          errMsg <<- paste(errMsg, sprintf(lang$errMsg$renderGraph$desc, sheetName), sep = "\n")
        })
      }else{
        tryCatch({
          callModule(renderData, paste0(tabsetIdChar, "_", tabId), 
                     type = graphConfig$outType, 
                     data = scenData$get(refId,
                                         symNames = c(sheetName, graphConfig$additionalData),
                                         drop = TRUE), 
                     configData = scenData$getScalars(refId), 
                     dtOptions = graphConfig$datatable,
                     graphOptions = graphConfig$graph, 
                     pivotOptions = graphConfig$pivottable, 
                     customOptions = graphConfig$options,
                     roundPrecision = roundPrecision,
                     modelDir = modelDir,
                     rendererEnv = rendererEnv[[refId]], views = views, attachments = attachments)
          callModule(renderData, paste0("table_", tabsetIdChar, "_", tabId),
                     type = "datatable",
                     data = scenData$get(refId, symNames = sheetName, drop = TRUE), 
                     dtOptions = graphConfig$datatable, roundPrecision = roundPrecision)
          dynamicUILoaded$dynamicTabsets[[tabsetIdChar]][["content"]][tabId] <<- TRUE
          if(identical(scenData$getById("dirty", refId = refId, drop = TRUE), TRUE)){
            showErrorMsg(lang$errMsg$loadScen$title, lang$errMsg$loadScen$inconsistentDataWarning)
          }
        }, error = function(e) {
          flog.error("Problem rendering graphs for dataset: '%s'. Error message: %s.",
                     sheetName, conditionMessage(e))
          showEl(session, paste0("#", tabsetIdChar, "_", tabId, "-noData"))
          hideEl(session, paste0("#", tabsetIdChar, "_", tabId, "-data"))
          errMsg <<- paste(errMsg, sprintf(lang$errMsg$renderGraph$desc, sheetName), sep = "\n")
        })
      }
      if(is.null(showErrorMsg(lang$errMsg$loadScen$title, errMsg))){
        return()
      }
    }
  }
}

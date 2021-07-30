rowtmp <- list()
isolate({
  indexMap <- IdIdxMap$new(list(inputGroups = seq_along(configJSON$inputGroups),
                                inputGroups_full  = seq_along(configJSON$inputGroups),
                                inputWidgetGroups = seq_along(configJSON$inputWidgetGroups),
                                inputWidgetGroups_full = seq_along(configJSON$inputWidgetGroups),
                                outputGroups = seq_along(configJSON$outputGroups),
                                outputGroups_full = seq_along(configJSON$outputGroups),
                                symlink = seq_along(configJSON$symbolLinks),
                                outputAttachments = seq_along(configJSON$outputAttachments)))
  
  groupTemp <- list(inputGroups = configJSON$inputGroups,
                    inputWidgetGroups = configJSON$inputWidgetGroups,
                    outputGroups = configJSON$outputGroups)
  rv$generalConfig$inputGroups <- configJSON$inputGroups
  rv$generalConfig$inputWidgetGroups <- configJSON$inputWidgetGroups
  rv$generalConfig$outputGroups <- configJSON$outputGroups
  rv$generalConfig$outputAttachments <- configJSON$outputAttachments
  rv$generalConfig$symbolLinks <- configJSON$symbolLinks
  rv$generalConfig$scripts <- configJSON$scripts
  rv$generalConfig$UILogo <- configJSON$UILogo
})
scalarSymbols <- setNames(c(names(modelIn), 
                            if(length(modelIn[[scalarsFileName]])) 
                              modelIn[[scalarsFileName]]$symnames),  
                          c(modelInAlias, 
                            if(length(modelIn[[scalarsFileName]])) 
                              modelIn[[scalarsFileName]]$symtext))
scalarSymbols <- scalarSymbols[scalarSymbols %in% scalarInputSym]

baseScriptValidator  <- Validator$new(c("id", "tabTitle", "command", 
                                        "args", "outputFile", "markdown", "timeout"),
                                      configJSON$scripts$base,
                                      requiredKeys = c("id", "tabTitle", 
                                                       "command", "outputFile"))
hcubeScriptValidator <- Validator$new(c("id", "title", "command", 
                                        "args", "outputFile", "markdown", "timeout"),
                                      configJSON$scripts$hcube,
                                      requiredKeys = c("id", "title", 
                                                       "command", "outputFile"))
outputAttachmentsValidator <- Validator$new(c("filename", "execPerm", "throwError"),
                                      configJSON$outputAttachments,
                                      requiredKeys = c("filename"))

# set default values for array elements
if(length(configJSON$inputGroups))
  addArrayEl(session, "symbol_inputGroups", defaults = configJSON$inputGroups)
if(length(configJSON$inputWidgetGroups))
  addArrayEl(session, "symbol_inputWidgetGroups", defaults = configJSON$inputWidgetGroups)
if(length(configJSON$outputGroups))
  addArrayEl(session, "symbol_outputGroups", defaults = configJSON$outputGroups)
if(length(configJSON$symbolLinks))
  addArrayEl(session, "symbol_links", defaults = configJSON$symbolLinks)
if(length(configJSON$scripts$base))
  addArrayEl(session, "scripts_base", defaults = configJSON$scripts$base)
if(length(configJSON$scripts$hcube))
  addArrayEl(session, "scripts_hcube", defaults = configJSON$scripts$hcube)
if(length(configJSON$outputAttachments))
  addArrayEl(session, "general_output_attach", defaults = configJSON$outputAttachments)

output$general_logo_preview <- renderImage({
  rv$customLogoChanged
  isolate({
    if(identical(rv$generalConfig$UILogo, "gams_logo.png") || 
       !length(rv$generalConfig$UILogo) || !file.exists(paste0(currentModelDir, .Platform$file.sep, 
                                                               "static_", modelName, .Platform$file.sep, 
                                                               rv$generalConfig$UILogo))){
      filename <- normalizePath(file.path(getwd(), "www", "gams_logo.png"))
    }else{
      filename <- normalizePath(paste0(currentModelDir, .Platform$file.sep, 
                                       "static_", modelName, .Platform$file.sep, 
                                       rv$generalConfig$UILogo))
    }
  })
  list(src = filename, height = "50px", alt = "custom logo")
}, deleteFile = FALSE)

observeEvent(input$general_pageTitle, {
  if(length(input$general_pageTitle) && nchar(input$general_pageTitle)){
    rv$generalConfig$pageTitle <<- input$general_pageTitle
  }
})
observeEvent(input$general_theme, {
  rv$generalConfig$theme <<- input$general_theme
})
observeEvent(input$general_meta, {
  rv$generalConfig$excelIncludeMeta <<- input$general_meta
})
observeEvent(input$general_empty, {
  rv$generalConfig$excelIncludeEmptySheets <<- input$general_empty
})
observeEvent(c(input$default_scen_check, input$general_default_scen_name), {
  if(!nchar(input$general_default_scen_name) || identical(input$default_scen_check, FALSE))
    configJSON$defaultScenName <<- NULL
  if(nchar(input$general_default_scen_name) && identical(input$default_scen_check, TRUE))
    rv$generalConfig$defaultScenName <<- input$general_default_scen_name
  else
    rv$generalConfig$defaultScenName <<- NULL
})
observeEvent(input$general_defaultRendererOutput, {
  if(length(input$general_defaultRendererOutput) &&
     input$general_defaultRendererOutput %in% c("miroPivot", "datatable")){
    rv$generalConfig$defaultRendererOutput <<- input$general_defaultRendererOutput
  }else{
    rv$generalConfig$defaultRendererOutput <<- "miroPivot"
  }
})
observeEvent(input$widget_general_logo_upload, {
  inFile   <- input$widget_general_logo_upload
  filePath <- inFile$datapath
  fileName <- inFile$name
  if(!dir.exists(paste0(currentModelDir, .Platform$file.sep, "static_", modelName))){
    if(!dir.create(paste0(currentModelDir, .Platform$file.sep, "static_", modelName))){
      flog.error("A problem occurred creating directory: %s. Maybe you have insufficient permissions?", 
                 paste0(currentModelDir, .Platform$file.sep, "static_", modelName))
      showModal(modalDialog(lang$adminMode$general$modalDialog$title, 
                            lang$adminMode$general$modalDialog$content))
      return()
    }
  }else{
    filesToDelete <- file.path(currentModelDir, paste0("static_", modelName), 
                               rv$generalConfig$UILogo)
    filesFailedToDelete <- !suppressWarnings(file.remove(filesToDelete))
    if(any(filesFailedToDelete)){
      flog.warn("Problems removing files: '%s'. Do you lack the necessary permissions?", 
                paste(filesToDelete[filesFailedToDelete], collapse = "', '"))
    }
  }
  if(!file.copy(filePath, file.path(currentModelDir, paste0("static_", modelName), fileName), overwrite = TRUE)){
    flog.error("A problem occurred copying image (%s) to folder: %s. Maybe you have insufficient permissions?", 
               filePath, paste0(currentModelDir, .Platform$file.sep, "static_", modelName))
    showModal(modalDialog(lang$adminMode$general$modalDialog$title, 
                          lang$adminMode$general$modalDialog$content))
    return()
  }
  rv$generalConfig$UILogo <<- fileName
  rv$customLogoChanged <<- rv$customLogoChanged + 1L
})
observe({
  input$general_readmeEnableMath
  if(isFALSE(input$general_useReadme) ||
     !length(input$general_readmeTabtitle) ||
     !nchar(trimws(input$general_readmeTabtitle)) ||
     !length(input$general_readmeFileName) ||
     nchar(trimws(input$general_readmeFileName)) < 3L){
    configJSON$readme <<- NULL
    rv$generalConfig$readme <<- NULL
    disableEl(session, "#btEditReadme")
    return()
  }
  isolate(rv$generalConfig$readme <<- list(
    tabTitle = input$general_readmeTabtitle,
    filename = input$general_readmeFileName,
    enableMath = input$general_readmeEnableMath))
  enableEl(session, "#btEditReadme")
})
observeEvent(input$general_auto, {
  rv$generalConfig$autoGenInputGraphs <<- input$general_auto
})
observeEvent(input$general_save_duration, {
  rv$generalConfig$storeLogFilesDuration <<- input$general_save_duration
})

observeEvent(input$general_args, ignoreNULL = FALSE, {
  if(!length(input$general_args)){
    hideEl(session, "#invalidClArgsError")
    rv$generalConfig$extraClArgs <<- NULL
    configJSON$extraClArgs <<- NULL
    return()
  }
  invalidClArgs <- trimws(tolower(vapply(strsplit(input$general_args, " |="),
                                         "[[", character(1L), 1L, USE.NAMES = FALSE))) %in% reservedGMSOpt
  if(any(invalidClArgs)){
    showEl(session, "#invalidClArgsError")
    newClArgs <- input$general_args[!invalidClArgs]
    if(!length(newClArgs)){
      rv$generalConfig$extraClArgs <<- NULL
      configJSON$extraClArgs <<- NULL
    }else{
      rv$generalConfig$extraClArgs <- I(newClArgs)
    }
  }else{
    hideEl(session, "#invalidClArgsError")
    rv$generalConfig$extraClArgs <- I(input$general_args)
  }
})

observeEvent(input$general_scen, {
  rv$generalConfig$defCompMode <<- input$general_scen
})
observeEvent({input$pivotcomp_emptyUEL
  input$pivotcomp_enableHideEmptyCols}, {
  if(isTRUE(input$pivotcomp_enableHideEmptyCols)){
    rv$generalConfig$pivotCompSettings <- list(enableHideEmptyCols = TRUE)
    if(length(input$pivotcomp_emptyUEL) && !identical(input$pivotcomp_emptyUEL, "")){
      rv$generalConfig$pivotCompSettings$emptyUEL <- input$pivotcomp_emptyUEL
    }
  }else{
    configJSON$pivotCompSettings <<- NULL
    rv$generalConfig$pivotCompSettings <<- NULL
  }
})
observeEvent(input$general_act_upload, {
  rv$generalConfig$activateModules$loadLocal <<- input$general_act_upload
})
observeEvent(input$general_act_log, {
  rv$generalConfig$activateModules$logFile <<- input$general_act_log
})
observeEvent(input$general_act_lst, {
  rv$generalConfig$activateModules$lstFile <<- input$general_act_lst
})
observeEvent(input$general_act_attach, {
  rv$generalConfig$activateModules$attachments <<- input$general_act_attach
})
observeEvent(input$general_act_hcube, {
  rv$generalConfig$activateModules$hcube <<- input$general_act_hcube
})
observeEvent(input$general_aggregate, {
  rv$generalConfig$aggregateWidgets <<- input$general_aggregate
})
observeEvent(input$general_overwriteSheetOrderInput, {
  if(!identical(input$general_overwriteSheetOrderInput, unname(inputSymMultiDim))){
    #adjust widget ids
    sheetOrderTmp <- input$general_overwriteSheetOrderInput
    widgetGrps <- strsplit(sheetOrderTmp, "_widgets", fixed = TRUE)
    i <- 0L
    sheetOrderTmp <- vapply(widgetGrps, function(grpId){
      if(length(grpId) < 2L){
        if(identical(grpId, "")){
          return("_widgets")
        }
        return(grpId)
      }
      i <<- i + 1L
      return(paste0("_widgets", i))
    }, character(1L), USE.NAMES = FALSE)
    rv$generalConfig$overwriteSheetOrder$input <<- sheetOrderTmp
  }
})
observeEvent(input$general_overwriteSheetOrderOutput, {
  if(!identical(input$general_overwriteSheetOrderOutput, names(modelOut))){
    rv$generalConfig$overwriteSheetOrder$output <<- input$general_overwriteSheetOrderOutput
  }
})
lapply(c(modelInRaw[[scalarsFileName]]$symnames,
         modelOut[[scalarsOutName]]$symnames), function(name){
           observeEvent(input[[paste0("general_overwriteSymAlias_", name)]], {
             if(length(input[[paste0("general_overwriteSymAlias_", name)]]) && 
                nchar(input[[paste0("general_overwriteSymAlias_", name)]]) > 0L){
               newAlias <- input[[paste0("general_overwriteSymAlias_", name)]]
               if(!length(rv$generalConfig$overwriteAliases)){
                 rv$generalConfig$overwriteAliases <- list()
               }
               rv$generalConfig$overwriteAliases[[name]] <<- list(newAlias = newAlias)
             }else{
               rv$generalConfig$overwriteAliases[[name]] <<- NULL
               configJSON$overwriteAliases[[name]] <<- NULL
               if(!length(rv$generalConfig$overwriteAliases)){
                 rv$generalConfig$overwriteAliases <<- NULL
                 configJSON$overwriteAliases <<- NULL
               }
               return()
             }
           })
         })
lapply(c(names(modelInRaw), names(modelOut)), function(name){
  observeEvent(input[[paste0("general_overwriteSymAlias_", name)]], {
    if(length(input[[paste0("general_overwriteSymAlias_", name)]]) && 
       nchar(input[[paste0("general_overwriteSymAlias_", name)]]) > 0L){
      newAlias <- input[[paste0("general_overwriteSymAlias_", name)]]
      if(!length(rv$generalConfig$overwriteAliases)){
        rv$generalConfig$overwriteAliases <- list()
      }
      rv$generalConfig$overwriteAliases[[name]] <<- list(newAlias = newAlias)
    }else{
      rv$generalConfig$overwriteAliases[[name]] <<- NULL
      configJSON$overwriteAliases[[name]] <<- NULL
      if(!length(rv$generalConfig$overwriteAliases)){
        rv$generalConfig$overwriteAliases <<- NULL
        configJSON$overwriteAliases <<- NULL
      }
      return()
    }
  })
  observe({
    defaultAlias <- FALSE
    if(name %in% names(modelOut)){
      newHeaders <- unlist(lapply(seq_along(dataContract$outputSymbols[[name]]$headers), function(hdrIdx){
        input[[paste0("general_overwriteSymHeaders_", name, "_", hdrIdx)]]
      }))
      if(length(newHeaders) != length(dataContract$outputSymbols[[name]]$headers)){
        return()
      }
      if(any(newHeaders == "")){
        addClassEl(session, paste0("#general_overwriteSymHeaders_", name), "has-error")
        defaultAlias <- TRUE
      }else if(identical(newHeaders, vapply(dataContract$outputSymbols[[name]]$headers, "[[", 
                                            character(1L), "alias",
                                            USE.NAMES = FALSE))){
        removeClassEl(session, paste0("#general_overwriteSymHeaders_", name), "has-error")
        defaultAlias <- TRUE
      }else{
        removeClassEl(session, paste0("#general_overwriteSymHeaders_", name), "has-error")
      }
    }else{
      newHeaders <- unlist(lapply(seq_along(dataContract$inputSymbols[[name]]$headers), function(hdrIdx){
        input[[paste0("general_overwriteSymHeaders_", name, "_", hdrIdx)]]
      }))
      if(length(newHeaders) != length(dataContract$inputSymbols[[name]]$headers)){
        return()
      }
      if(any(newHeaders == "")){
        addClassEl(session, paste0("#general_overwriteSymHeaders_", name), "has-error")
        defaultAlias <- TRUE
      }else if(identical(newHeaders, vapply(dataContract$inputSymbols[[name]]$headers, "[[", 
                                      character(1L), "alias",
                                      USE.NAMES = FALSE))){
        removeClassEl(session, paste0("#general_overwriteSymHeaders_", name), "has-error")
        defaultAlias <- TRUE
      }else{
        removeClassEl(session, paste0("#general_overwriteSymHeaders_", name), "has-error")
      }
    }
    if(defaultAlias){
      rv$generalConfig$overwriteHeaderAliases[[name]] <<- NULL
      configJSON$overwriteHeaderAliases[[name]] <<- NULL
      if(!length(rv$generalConfig$overwriteHeaderAliases)){
        rv$generalConfig$overwriteHeaderAliases <<- NULL
        configJSON$overwriteHeaderAliases <<- NULL
      }
      return()
    }
    if(!length(rv$generalConfig$overwriteHeaderAliases)){
      rv$generalConfig$overwriteHeaderAliases <- list()
    }
    rv$generalConfig$overwriteHeaderAliases[[name]] <<- list(newHeaders = newHeaders)
  })
})
observeEvent(input$general_downloadTempFiles, {
  rv$generalConfig$activateModules$downloadTempFiles <<- input$general_downloadTempFiles
})

observeEvent(input$general_hidden, ignoreNULL = FALSE, {
  if(!length(input$general_hidden)){
    configJSON$hiddenOutputScalars <<- NULL
  }
  rv$generalConfig$hiddenOutputScalars <<- input$general_hidden
})
observeEvent(input$general_hiddenOutputSymbols, ignoreNULL = FALSE, {
  if(!length(input$general_hiddenOutputSymbols)){
    configJSON$hiddenOutputSymbols <<- NULL
  }
  rv$generalConfig$hiddenOutputSymbols <<- input$general_hiddenOutputSymbols
})

observeEvent(input$general_decimal, {
  rv$generalConfig$roundingDecimals <<- input$general_decimal
})
observeEvent(input$general_mirologfile, {
  if(!nchar(input$general_mirologfile))
    configJSON$miroLogFile <<- NULL
  if(length(input$general_mirologfile) && 
     nchar(trimws(input$general_mirologfile))){
    rv$generalConfig$miroLogFile <<- input$general_mirologfile
    return()
  }
  rv$generalConfig$miroLogFile <<- NULL
})
observeEvent(input$add_general, {
  if(length(input$add_general) < 3L){
    return()
  }
  arrayID  <- strsplit(input$add_general[3], "_")[[1]][2]
  arrayIdx <- indexMap$push(arrayID, input$add_general[1])
  arrayIdxAll <- indexMap$push(paste0(arrayID, "_full"), input$add_general[1])
  
  minMembers <- if(identical(arrayID, "inputWidgetGroups")) 0L else 1L
  
  if(length(input$add_general) < 3L || nchar(trimws(input$add_general[2])) < 1L){
    # name has no characters
    if(arrayIdx <= length(rv$generalConfig[[arrayID]])){
      rv$generalConfig[[arrayID]][[arrayIdx]] <<- NULL
      if(!length(rv$generalConfig[[arrayID]])){
        rv$generalConfig[[arrayID]] <<- NULL
      }
      if(identical(arrayID, "inputWidgetGroups")){
        updateSheetOrderInput(arrayIdxAll)
      }
      indexMap$pop(arrayID, input$add_general[1])
    }
    newName <- NULL
    showElReplaceTxt(session, paste0("#", input$add_general[3], input$add_general[1], "_err"), 
                     lang$adminMode$widgets$validate$val36)
  }else{
    newName <- input$add_general[2]
    if(arrayIdx <= length(rv$generalConfig[[arrayID]]) && length(rv$generalConfig[[arrayID]][[arrayIdx]])){
      rv$generalConfig[[arrayID]][[arrayIdx]]$name <- newName
      if(identical(arrayID, "inputWidgetGroups")){
        updateSheetOrderInput(arrayIdxAll, newName)
      }
    }else if(arrayIdxAll <= length(groupTemp[[arrayID]]) && 
             length(groupTemp[[arrayID]][[arrayIdxAll]]$members) > minMembers &&
             !any(groupTemp[[arrayID]][[arrayIdxAll]]$members %in% 
                  unlist(lapply(rv$generalConfig[[arrayID]][-arrayIdx], "[[", "members"), use.names = FALSE))){
      rv$generalConfig[[arrayID]][[arrayIdx]] <- list(name = newName, 
                                                      members = groupTemp[[arrayID]][[arrayIdxAll]]$members,
                                                      sameTab = isTRUE(groupTemp[[arrayID]][[arrayIdxAll]]$sameTab))
      if(identical(arrayID, "inputWidgetGroups")){
        updateSheetOrderInput(arrayIdxAll, newName)
      }
    }else{
      showElReplaceTxt(session, paste0("#group_member", 
                                       if(identical(arrayID, "inputGroups")) "In" 
                                       else if(identical(arrayID, "inputWidgetGroups")) "Widget" else "Out", 
                                       input$add_general[1], "_err"), 
                       lang$adminMode$widgets$validate[[if(minMembers == 0L) "val37a" else "val37"]])
    }
    hideEl(session, paste0("#", input$add_general[3], input$add_general[1], "_err"))
  }
  if(arrayIdxAll > length(groupTemp[[arrayID]])){
    groupTemp[[arrayID]][[arrayIdxAll]] <<- list(name = newName)
  }else{
    groupTemp[[arrayID]][[arrayIdxAll]]$name <<- newName
  }
})
validateSymbolLink <- function(sourceSym, targetSym, arrayId){
  if(!identical(vapply(modelInRaw[[targetSym]]$headers, 
                       "[[", character(1L), "type",
                       USE.NAMES = FALSE),
                vapply(modelOut[[sourceSym]]$headers, 
                       "[[", character(1L), "type",
                       USE.NAMES = FALSE))){
    showElReplaceTxt(session, paste0("#symlink_target", 
                                     arrayId, "_err"),
                     lang$adminMode$widgets$validate$val47)
  }else{
    hideEl(session, paste0("#symlink_target", 
                           arrayId, "_err"))
  }
}
observeEvent(input$add_symlink, {
  if(!length(inputSymMultiDim) || length(input$add_symlink) < 3L){
    return()
  }
  arrayIdx <- indexMap$push("symlink", input$add_symlink[1])
  
  if(!length(arrayIdx)){
    flog.error("Bad index for symbolLink detected: '%s'. Please report to GAMS!", arrayIdx)
    return()
  }
  if(arrayIdx <= length(rv$generalConfig$symbolLinks)){
    rv$generalConfig$symbolLinks[[arrayIdx]]$source <<- input$add_symlink[2]
    validateSymbolLink(input$add_symlink[2], 
                       rv$generalConfig$symbolLinks[[arrayIdx]]$target,
                       input$add_symlink[1])
  }else{
    rv$generalConfig$symbolLinks[[arrayIdx]] <<- list(source = input$add_symlink[2],
                                                      target = inputSymMultiDim[[1L]])
    validateSymbolLink(input$add_symlink[2], 
                       inputSymMultiDim[[1L]],
                       input$add_symlink[1])
  }
})
observeEvent(input$symlink_target, {
  if(length(input$symlink_target) < 2L){
    return()
  }
  arrayIdx <- indexMap$push("symlink", 
                            input$symlink_target[1])
  
  if(length(arrayIdx) && arrayIdx <= length(rv$generalConfig$symbolLinks)){
    targetSym <- input$symlink_target[2]
    rv$generalConfig$symbolLinks[[arrayIdx]]$target <<- targetSym
    sourceSym <- rv$generalConfig$symbolLinks[[arrayIdx]]$source
    validateSymbolLink(sourceSym, targetSym, input$symlink_target[1])
  }
})
observeEvent(input$remove_symlink, {
  if(length(input$remove_symlink) < 3L){
    return()
  }
  arrayIdx <- indexMap$pop("symlink", 
                           input$remove_symlink[3])
  if(length(arrayIdx) && arrayIdx <= length(rv$generalConfig$symbolLinks)){
    rv$generalConfig$symbolLinks[[arrayIdx]] <<- NULL
    if(!length(rv$generalConfig$symbolLinks)){
      rv$generalConfig$symbolLinks <<- NULL
    }
  }
})
observeEvent(input$add_script, {
  arrayID <- as.integer(input$add_script[1])
  if(is.na(arrayID)){
    return()
  }
  isHcubeScript <- identical(input$add_script[3], "scripts_hcube")
  if(!grepl("^[[:alnum:]]+$", input$add_script[2])){
    showElReplaceTxt(session, paste0("#", input$add_script[3], arrayID, "_err"), 
                     lang$adminMode$widgets$validate$val49)
    if(isHcubeScript){
      rv$generalConfig$scripts$hcube <- hcubeScriptValidator$
        removeKey(arrayID, "id")$
        getValidData()
    }else{
      rv$generalConfig$scripts$base <- baseScriptValidator$
        removeKey(arrayID, "id")$
        getValidData()
    }
    return()
  }
  if(isHcubeScript){
    validScriptIds <- hcubeScriptValidator$getValid("id")
  }else{
    validScriptIds <- baseScriptValidator$getValid("id")
  }
  if(input$add_script[2] %in% validScriptIds){
    showElReplaceTxt(session, paste0("#", input$add_script[3], arrayID, "_err"), 
                     lang$adminMode$widgets$validate$val50)
    if(isHcubeScript){
      rv$generalConfig$scripts$hcube <- hcubeScriptValidator$
        removeKey(arrayID, "id")$
        getValidData()
    }else{
      rv$generalConfig$scripts$base <- baseScriptValidator$
        removeKey(arrayID, "id")$
        getValidData()
    }
    return()
  }
  hideEl(session, paste0("#", input$add_script[3], arrayID, "_err"))
  if(isHcubeScript){
    rv$generalConfig$scripts$hcube <- hcubeScriptValidator$
      setVal(arrayID, "id", input$add_script[2])$
      getValidData()
  }else{
    rv$generalConfig$scripts$base <- baseScriptValidator$
      setVal(arrayID, "id", input$add_script[2])$
      getValidData()
  }
})
observeEvent(input$scriptsB_title, {
  if(length(input$scriptsB_title) < 2L){
    return()
  }
  arrayID <- as.integer(input$scriptsB_title[1])
  if(is.na(arrayID)){
    return()
  }
  val <- input$scriptsB_title[2]
  if(!length(val) || !nchar(val)){
    showElReplaceTxt(session, paste0("#scriptsB_title", arrayID, "_err"), 
                     lang$adminMode$widgets$validate$val51)
    return()
  }
  hideEl(session, paste0("#scriptsB_title", arrayID, "_err"))
  
  rv$generalConfig$scripts$base <- baseScriptValidator$
    setVal(arrayID, "tabTitle", val)$
    getValidData()
})
observeEvent(input$scriptsB_cmd, {
  if(length(input$scriptsB_cmd) < 2L){
    return()
  }
  arrayID <- as.integer(input$scriptsB_cmd[1])
  if(is.na(arrayID)){
    return()
  }
  val <- input$scriptsB_cmd[2]
  if(!length(val) || !nchar(val)){
    showElReplaceTxt(session, paste0("#scriptsB_cmd", arrayID, "_err"), 
                     lang$adminMode$widgets$validate$val52)
    return()
  }
  hideEl(session, paste0("#scriptsB_cmd", arrayID, "_err"))
  
  rv$generalConfig$scripts$base <- baseScriptValidator$
    setVal(arrayID, "command", val)$
    getValidData()
})
observeEvent(input$scriptsB_args, {
  if(length(input$scriptsB_args) < 1L){
    return()
  }
  arrayID <- as.integer(input$scriptsB_args[1])
  if(is.na(arrayID)){
    return()
  }
  if(length(input$scriptsB_args) < 2L){
    rv$generalConfig$scripts$base <- baseScriptValidator$
      removeKey(arrayID, "args")$
      getValidData()
    return()
  }
  rv$generalConfig$scripts$base <- baseScriptValidator$
    setVal(arrayID, "args", input$scriptsB_args[-1])$
    getValidData()
})
observeEvent(input$scriptsB_outFile, {
  if(length(input$scriptsB_outFile) < 2L){
    return()
  }
  arrayID <- as.integer(input$scriptsB_outFile[1])
  if(is.na(arrayID)){
    return()
  }
  val <- input$scriptsB_outFile[2]
  if(!length(val) || !nchar(val)){
    showElReplaceTxt(session, paste0("#scriptsB_outFile", arrayID, "_err"), 
                     lang$adminMode$widgets$validate$val53)
    return()
  }
  hideEl(session, paste0("#scriptsB_outFile", arrayID, "_err"))
  
  rv$generalConfig$scripts$base <- baseScriptValidator$
    setVal(arrayID, "outputFile", val)$
    getValidData()
})
observeEvent(input$scriptsB_markdown, {
  if(length(input$scriptsB_markdown) < 2L){
    return()
  }
  arrayID <- as.integer(input$scriptsB_markdown[1])
  if(is.na(arrayID)){
    return()
  }
  val <- as.logical(input$scriptsB_markdown[2])
  if(!length(val) || is.na(val) || val < -1L){
    rv$generalConfig$scripts$base <- baseScriptValidator$
      removeKey(arrayID, "markdown")$
      getValidData()
    return()
  }
  rv$generalConfig$scripts$base <- baseScriptValidator$
    setVal(arrayID, "markdown", val)$
    getValidData()
})
observeEvent(input$scriptsB_timeout, {
  if(length(input$scriptsB_timeout) < 2L){
    return()
  }
  arrayID <- as.integer(input$scriptsB_timeout[1])
  if(is.na(arrayID)){
    return()
  }
  val <- as.integer(input$scriptsB_timeout[2])
  if(!length(val) || is.na(val) || val < -1L){
    rv$generalConfig$scripts$base <- baseScriptValidator$
      removeKey(arrayID, "timeout")$
      getValidData()
    return()
  }
  rv$generalConfig$scripts$base <- baseScriptValidator$
    setVal(arrayID, "timeout", val)$
    getValidData()
})
observeEvent(input$scriptsH_title, {
  if(length(input$scriptsH_title) < 2L){
    return()
  }
  arrayID <- as.integer(input$scriptsH_title[1])
  if(is.na(arrayID)){
    return()
  }
  val <- input$scriptsH_title[2]
  if(!length(val) || !nchar(val)){
    showElReplaceTxt(session, paste0("#scriptsH_title", arrayID, "_err"), 
                     lang$adminMode$widgets$validate$val51)
    return()
  }
  hideEl(session, paste0("#scriptsH_title", arrayID, "_err"))
  
  rv$generalConfig$scripts$hcube <- hcubeScriptValidator$
    setVal(arrayID, "title", val)$
    getValidData()
})
observeEvent(input$scriptsH_cmd, {
  if(length(input$scriptsH_cmd) < 2L){
    return()
  }
  arrayID <- as.integer(input$scriptsH_cmd[1])
  if(is.na(arrayID)){
    return()
  }
  val <- input$scriptsH_cmd[2]
  if(!length(val) || !nchar(val)){
    showElReplaceTxt(session, paste0("#scriptsH_cmd", arrayID, "_err"), 
                     lang$adminMode$widgets$validate$val52)
    return()
  }
  hideEl(session, paste0("#scriptsH_cmd", arrayID, "_err"))
  
  rv$generalConfig$scripts$hcube <- hcubeScriptValidator$
    setVal(arrayID, "command", val)$
    getValidData()
})
observeEvent(input$scriptsH_args, {
  if(length(input$scriptsH_args) < 1L){
    return()
  }
  arrayID <- as.integer(input$scriptsH_args[1])
  if(is.na(arrayID)){
    return()
  }
  if(length(input$scriptsH_args) < 2L){
    rv$generalConfig$scripts$hcube <- hcubeScriptValidator$
      removeKey(arrayID, "args")$
      getValidData()
    return()
  }
  rv$generalConfig$scripts$hcube <- hcubeScriptValidator$
    setVal(arrayID, "args", input$scriptsH_args[-1])$
    getValidData()
})
observeEvent(input$scriptsH_outFile, {
  if(length(input$scriptsH_outFile) < 2L){
    return()
  }
  arrayID <- as.integer(input$scriptsH_outFile[1])
  if(is.na(arrayID)){
    return()
  }
  val <- input$scriptsH_outFile[2]
  if(!length(val) || !nchar(val)){
    showElReplaceTxt(session, paste0("#scriptsH_outFile", arrayID, "_err"), 
                     lang$adminMode$widgets$validate$val53)
    return()
  }
  hideEl(session, paste0("#scriptsH_outFile", arrayID, "_err"))
  
  rv$generalConfig$scripts$hcube <- hcubeScriptValidator$
    setVal(arrayID, "outputFile", val)$
    getValidData()
})
observeEvent(input$scriptsH_markdown, {
  if(length(input$scriptsH_markdown) < 2L){
    return()
  }
  arrayID <- as.integer(input$scriptsH_markdown[1])
  if(is.na(arrayID)){
    return()
  }
  val <- as.logical(input$scriptsH_markdown[2])
  if(!length(val) || is.na(val) || val < -1L){
    rv$generalConfig$scripts$hcube <- hcubeScriptValidator$
      removeKey(arrayID, "markdown")$
      getValidData()
    return()
  }
  rv$generalConfig$scripts$hcube <- hcubeScriptValidator$
    setVal(arrayID, "markdown", val)$
    getValidData()
})
observeEvent(input$scriptsH_timeout, {
  if(length(input$scriptsH_timeout) < 2L){
    return()
  }
  arrayID <- as.integer(input$scriptsH_timeout[1])
  if(is.na(arrayID)){
    return()
  }
  val <- as.integer(input$scriptsH_timeout[2])
  if(!length(val) || is.na(val) || val < -1L){
    rv$generalConfig$scripts$hcube <- hcubeScriptValidator$
      removeKey(arrayID, "timeout")$
      getValidData()
    return()
  }
  rv$generalConfig$scripts$hcube <- hcubeScriptValidator$
    setVal(arrayID, "timeout", val)$
    getValidData()
})
observeEvent(input$remove_script, {
  if(length(input$remove_script) < 3L){
    return()
  }
  arrayID <- as.integer(input$remove_script[3])
  if(is.na(arrayID)){
    return()
  }
  if(identical(input$remove_script[1], "scripts_hcube")){
    rv$generalConfig$scripts$hcube <- hcubeScriptValidator$
      removeEl(arrayID)$
      getValidData()
    if(!length(rv$generalConfig$scripts$hcube)){
      rv$generalConfig$scripts$hcube <- NULL
    }
  }else if(identical(input$remove_script[1], "scripts_base")){
    rv$generalConfig$scripts$base <- baseScriptValidator$
      removeEl(arrayID)$
      getValidData()
    if(!length(rv$generalConfig$scripts$base)){
      rv$generalConfig$scripts$base <- NULL
    }
  }
  if(!length(rv$generalConfig$scripts)){
    rv$generalConfig$scripts <- NULL
  }
})
observeEvent(input$add_attach, {
  arrayID <- as.integer(input$add_attach[1])
  if(is.na(arrayID)){
    return()
  }
  if(!identical(sanitizeFn(input$add_attach[2]), input$add_attach[2])){
    showElReplaceTxt(session, paste0("#", input$add_attach[3], arrayID, "_err"), 
                     lang$adminMode$widgets$validate$val57)
    rv$generalConfig$outputAttachments <- outputAttachmentsValidator$
      removeKey(arrayID, "filename")$
      getValidData()
    return()
  }
  if(input$add_attach[2] %in% outputAttachmentsValidator$getValid("filename")){
    showElReplaceTxt(session, paste0("#", input$add_attach[3], arrayID, "_err"), 
                     lang$adminMode$widgets$validate$val56)
    rv$generalConfig$outputAttachments <- outputAttachmentsValidator$
      removeKey(arrayID, "filename")$
      getValidData()
    return()
  }
  hideEl(session, paste0("#", input$add_attach[3], arrayID, "_err"))
  if(length(input$add_attach) > 1 && nchar(input$add_attach[2]) > 0L) {
    rv$generalConfig$outputAttachments <- outputAttachmentsValidator$
      setVal(arrayID, "filename", input$add_attach[2])$
      getValidData()
  }
})
observeEvent(input$outAttach_exec, {
  if(length(input$outAttach_exec) < 2L){
    return()
  }
  arrayID <- as.integer(input$outAttach_exec[1])
  if(is.na(arrayID)){
    return()
  }
  val <- identical(input$outAttach_exec[2], 1L)
  if(!length(val)){
    rv$generalConfig$outputAttachments <- outputAttachmentsValidator$
      removeKey(arrayID, "execPerm")$
      getValidData()
    return()
  }
  rv$generalConfig$outputAttachments <- outputAttachmentsValidator$
    setVal(arrayID, "execPerm", val)$
    getValidData()
})
observeEvent(input$outAttach_error, {
  if(length(input$outAttach_error) < 2L){
    return()
  }
  arrayID <- as.integer(input$outAttach_error[1])
  if(is.na(arrayID)){
    return()
  }
  val <- !identical(input$outAttach_error[2], 0L)
  if(!length(val)){
    rv$generalConfig$outputAttachments <- outputAttachmentsValidator$
      removeKey(arrayID, "throwError")$
      getValidData()
    return()
  }
  rv$generalConfig$outputAttachments <- outputAttachmentsValidator$
    setVal(arrayID, "throwError", val)$
    getValidData()
})
observeEvent(input$remove_attach, {
  if(length(input$remove_attach) < 3L){
    return()
  }
  arrayID <- as.integer(input$remove_attach[3])
  if(is.na(arrayID)){
    return()
  }
  rv$generalConfig$outputAttachments <- outputAttachmentsValidator$
    removeEl(arrayID)$
    getValidData()
  if(!length(rv$generalConfig$outputAttachments)){
    rv$generalConfig$outputAttachments <- NULL
  }
})
changeAndValidateGroupMembers <- function(arrayID, groupMembers, HTMLarrayID, minNoMembers = 2L){
  arrayIdx <- indexMap$push(arrayID, groupMembers[1])
  arrayIdxAll <- indexMap$push(paste0(arrayID, "_full"), groupMembers[1])
  
  if(length(groupMembers) > minNoMembers && 
     !any(groupMembers[-1] %in% unlist(lapply(rv$generalConfig[[arrayID]][-arrayIdx], "[[", "members"), use.names = FALSE))){
    newMembers <- groupMembers[2:length(groupMembers)]
    if(arrayIdx <= length(rv$generalConfig[[arrayID]]) && length(rv$generalConfig[[arrayID]][[arrayIdx]])){
      rv$generalConfig[[arrayID]][[arrayIdx]]$members <- newMembers
    }else if(arrayIdxAll <= length(groupTemp[[arrayID]]) && 
             length(groupTemp[[arrayID]][[arrayIdxAll]]$name)){
      newName <- groupTemp[[arrayID]][[arrayIdxAll]]$name
      rv$generalConfig[[arrayID]][[arrayIdx]] <- list(name = newName, 
                                                      members = newMembers,
                                                      sameTab = isTRUE(groupTemp[[arrayID]][[arrayIdxAll]]$sameTab))
      if(identical(arrayID, "inputWidgetGroups")){
        updateSheetOrderInput(arrayIdxAll, newName)
      }
    }else{
      showElReplaceTxt(session, paste0("#symbol_", arrayID, input$add_general[1], "_err"), 
                       lang$adminMode$widgets$validate$val36)
    }
    hideEl(session, paste0("#", HTMLarrayID, groupMembers[1], "_err"))
  }else{
    if(arrayIdx <= length(rv$generalConfig[[arrayID]])){
      rv$generalConfig[[arrayID]][[arrayIdx]] <<- NULL
      if(!length(rv$generalConfig[[arrayID]])){
        rv$generalConfig[[arrayID]] <<- NULL
      }
      if(identical(arrayID, "inputWidgetGroups")){
        updateSheetOrderInput(arrayIdxAll)
      }
      indexMap$pop(arrayID, groupMembers[1])
    }
    newMembers <- NULL
    showElReplaceTxt(session, paste0("#", HTMLarrayID, groupMembers[1], "_err"), 
                     if(identical(minNoMembers, 2L)) lang$adminMode$widgets$validate[[if(minNoMembers == 0L) "val37a" else "val37"]]
                     else lang$adminMode$widgets$validate$val60)
  }
  if(arrayIdxAll > length(groupTemp[[arrayID]])){
    groupTemp[[arrayID]][[arrayIdxAll]] <<- list(members = newMembers)
  }else{
    groupTemp[[arrayID]][[arrayIdxAll]]$members <<- newMembers
  }
}
updateSheetOrderInput <- function(arrayIdx, newName = NULL){
  orderItemId <- paste0("_widgets", arrayIdx)
  orderItemIdx <- match(orderItemId, inputTabs)
  if(is.na(orderItemIdx)){
    if(is.null(newName)){
      return()
    }
    inputTabs <<- c(inputTabs, 
                    setNames(orderItemId, 
                             newName))
  }else if(is.null(newName)){
    inputTabs <<- inputTabs[-orderItemIdx]
  }else{
    names(inputTabs)[orderItemIdx] <<- newName
  }
  newSheetOrder <- inputTabs
  if(length(input$general_overwriteSheetOrderInput)){
    newSheetOrder <- inputTabs[order(match(inputTabs, input$general_overwriteSheetOrderInput))]
  }
  updateSelectInput(session, "general_overwriteSheetOrderInput", 
                    choices = newSheetOrder, selected = newSheetOrder)
}
observeEvent(input$group_memberIn, {
  changeAndValidateGroupMembers("inputGroups", input$group_memberIn, 
                                "group_memberIn")
})
observeEvent(input$group_memberWidget, {
  changeAndValidateGroupMembers("inputWidgetGroups", input$group_memberWidget, 
                                "group_memberWidget", 1L)
})
observeEvent(input$group_memberOut, {
  changeAndValidateGroupMembers("outputGroups", input$group_memberOut, 
                                "group_memberOut")
})
observeEvent(input$group_sameTabIn, {
  if(length(input$group_sameTabIn) < 2L)
    return()
  
  arrayIdx <- indexMap$push("inputGroups", input$group_sameTabIn[1])
  arrayIdxAll <- indexMap$push("inputGroups_full", input$group_sameTabIn[1])
  newVal   <- isTRUE(as.logical(input$group_sameTabIn[2]))
  
  if(arrayIdx <= length(rv$generalConfig[["inputGroups"]]) &&
     length(rv$generalConfig[["inputGroups"]][[arrayIdx]])){
    rv$generalConfig[["inputGroups"]][[arrayIdx]]$sameTab <<- newVal
  }else if(arrayIdxAll <= length(groupTemp[["inputGroups"]]) &&
           length(groupTemp[["inputGroups"]][[arrayIdxAll]])){
    groupTemp[["inputGroups"]][[arrayIdxAll]]$sameTab <<- newVal
  }else{
    groupTemp[["inputGroups"]][[arrayIdxAll]] <<- list(sameTab = newVal)
  }
})
observeEvent(input$group_sameTabWidget, {
  if(length(input$group_sameTabWidget) < 2L)
    return()
  
  arrayIdx <- indexMap$push("inputWidgetGroups", input$group_sameTabWidget[1])
  arrayIdxAll <- indexMap$push("inputWidgetGroups_full", input$group_sameTabWidget[1])
  newVal   <- isTRUE(as.logical(input$group_sameTabWidget[2]))
  if(arrayIdx <= length(rv$generalConfig[["inputWidgetGroups"]]) &&
     length(rv$generalConfig[["inputWidgetGroups"]][[arrayIdx]])){
    rv$generalConfig[["inputWidgetGroups"]][[arrayIdx]]$sameTab <<- newVal
  }else if(arrayIdxAll <= length(groupTemp[["inputWidgetGroups"]]) &&
           length(groupTemp[["inputWidgetGroups"]][[arrayIdxAll]])){
    groupTemp[["inputWidgetGroups"]][[arrayIdxAll]]$sameTab <<- newVal
  }else{
    groupTemp[["inputWidgetGroups"]][[arrayIdxAll]] <<- list(sameTab = newVal)
  }
})
observeEvent(input$group_sameTabOut, {
  if(length(input$group_sameTabOut) < 2L)
    return()
  
  arrayIdx <- indexMap$push("outputGroups", input$group_sameTabOut[1])
  arrayIdxAll <- indexMap$push("outputGroups_full", input$group_sameTabOut[1])
  newVal   <- isTRUE(as.logical(input$group_sameTabOut[2]))
  
  if(arrayIdx <= length(rv$generalConfig[["outputGroups"]]) && 
     length(rv$generalConfig[["outputGroups"]][[arrayIdx]])){
    rv$generalConfig[["outputGroups"]][[arrayIdx]]$sameTab <<- newVal
  }else if(arrayIdxAll <= length(groupTemp[["outputGroups"]]) &&
           length(groupTemp[["outputGroups"]][[arrayIdxAll]])){
    groupTemp[["outputGroups"]][[arrayIdxAll]]$sameTab <<- newVal
  }else{
    groupTemp[["outputGroups"]][[arrayIdxAll]] <<- list(sameTab = newVal)
  }
})
observeEvent(input$remove_general, {
  arrayID <- strsplit(input$remove_general[1], "_")[[1]][2]
  arrayIdx <- indexMap$pop(arrayID, 
                           input$remove_general[2])
  arrayIdxAll <- indexMap$pop(paste0(arrayID, "_full"), 
                              input$remove_general[2])
  if(length(arrayIdx) &&
     arrayIdx <= length(rv$generalConfig[[arrayID]])){
    rv$generalConfig[[arrayID]][[arrayIdx]] <<- NULL
    if(!length(rv$generalConfig[[arrayID]])){
      rv$generalConfig[[arrayID]] <<- NULL
    }
    if(identical(arrayID, "inputWidgetGroups")){
      updateSheetOrderInput(arrayIdxAll)
    }
  }
  if(length(arrayIdxAll) &&
     arrayIdxAll <= length(groupTemp[[arrayID]])){
    groupTemp[[arrayID]][[arrayIdxAll]] <<- NULL
  }
})
observeEvent(input$btEditReadme, {
  req(length(rv$generalConfig$readme$filename) > 0L, 
      nchar(trimws(rv$generalConfig$readme$filename)) > 0L)
  readmeContent <- character(0L)
  readmeContentParsed <- character(0L)
  if(length(rv$generalConfig$readme$filename) && 
     file.exists(file.path(currentModelDir, 
                           rv$generalConfig$readme$filename))){
    readmeContent <- read_file(file.path(currentModelDir, 
                                         rv$generalConfig$readme$filename))
    readmeContentParsed <- HTML(markdownParser$
                                  parseFile(file.path(currentModelDir,
                                                      rv$generalConfig$
                                                        readme$filename)))
  }
  
  showModal(modalDialog(
    title = lang$adminMode$general$readme$dialogEdit$title,
    tags$div(class = "gmsalert gmsalert-error center-alert", id = "mdSaveError", 
             lang$adminMode$general$readme$dialogEdit$msgErrSave),
    fluidRow(
      column(6L,
             tags$div(class = "readme-preview-header", lang$adminMode$general$readme$dialogEdit$reamdeHeader),
             tags$textarea(id = "mdContent", class = "readme-wrapper readme-preview-markdown",
                           oninput = paste0("Miro.mdToHTML(this.value,'#mdConvertedContent',",
                                            if(isTRUE(input$general_readmeEnableMath)) "true" else "false", ")"),
                           readmeContent)
      ),
      column(6L, 
             tags$div(class = "readme-preview-header", lang$adminMode$general$readme$dialogEdit$markdownHeader),
             tags$div(id = "mdConvertedContent", 
                      class = "readme-wrapper readme-preview-output", readmeContentParsed)
      ),
      tags$script(paste0("setTimeout(function(){Miro.mdToHTML(document.getElementById('mdContent').value,'#mdConvertedContent',",
                         if(isTRUE(input$general_readmeEnableMath)) "true" else "false", ")},500)"))
    ),
    footer = tagList(
      modalButton(lang$adminMode$general$readme$dialogEdit$btCancel),
      tags$button(id = "btMdSave", lang$adminMode$general$readme$dialogEdit$btSave, onclick = "Miro.mdSave('#mdContent')",
                  class = "btn btn-default bt-highlight-1")),
    fade = TRUE, easyClose = FALSE, size = "l"
  ))
})
observeEvent(input$btMdSave, {
  req(length(rv$generalConfig$readme$filename))
  tryCatch({
    write_file(input$btMdSave, file.path(currentModelDir, 
                                         rv$generalConfig$readme$filename))
    removeModal()
  }, error = function(e){
    showHideEl(session, "mdSaveError", 4000L)
  })
})

#  =======================================
#          SAVE JSON (automatically)
#  =======================================
observeEvent(rv$generalConfig, {
  req(length(rv$generalConfig))
  configJSON$inputGroups <<- NULL
  configJSON$outputGroups <<- NULL
  configJSON$inputWidgetGroups <<- NULL
  if(length(rv$generalConfig$inputGroups) || 
     length(rv$generalConfig$outputGroups) || 
     length(rv$generalConfig$inputWidgetGroups)){
    newGeneralJSON <- rv$generalConfig
    newGeneralJSON$inputGroups[vapply(newGeneralJSON$inputGroups, is.null, logical(1L), USE.NAMES = FALSE)] <- NULL
    newGeneralJSON$inputWidgetGroups[vapply(newGeneralJSON$inputWidgetGroups, is.null, logical(1L), USE.NAMES = FALSE)] <- NULL
    newGeneralJSON$outputGroups[vapply(newGeneralJSON$outputGroups, is.null, logical(1L), USE.NAMES = FALSE)] <- NULL
    configJSON <<- modifyList(configJSON, newGeneralJSON)
  }else{
    configJSON <<- modifyList(configJSON, rv$generalConfig)
  }
  configJSON$symbolLinks <<- rv$generalConfig$symbolLinks
  configJSON$scripts <<- rv$generalConfig$scripts
  configJSON$outputAttachments <<- rv$generalConfig$outputAttachments
  write_json(configJSON, configJSONFileName, pretty = TRUE, auto_unbox = TRUE, null = "null")
})

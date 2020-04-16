rowtmp <- list()
isolate({
  indexMap <- IdIdxMap$new(list(inputGroups = seq_along(configJSON$inputGroups),
                                outputGroups = seq_along(configJSON$outputGroups),
                                symlink = seq_along(configJSON$symbolLinks)))
  
  groupTemp <- list(inputGroups = list(), outputGroups = list())
  rv$generalConfig$inputGroups <- configJSON$inputGroups
  rv$generalConfig$outputGroups <- configJSON$outputGroups
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
                                        "args", "outputFile", "timeout"),
                                      configJSON$scripts$base,
                                      requiredKeys = c("id", "tabTitle", 
                                                       "command", "outputFile"))
hcubeScriptValidator <- Validator$new(c("id", "title", "command", 
                                        "args", "outputFile", "timeout"),
                                      configJSON$scripts$hcube,
                                      requiredKeys = c("id", "title", 
                                                       "command", "outputFile"))

# set default values for array elements
if(length(configJSON$inputGroups))
  addArrayEl(session, "symbol_inputGroups", defaults = configJSON$inputGroups)
if(length(configJSON$outputGroups))
  addArrayEl(session, "symbol_outputGroups", defaults = configJSON$outputGroups)
if(length(configJSON$symbolLinks))
  addArrayEl(session, "symbol_links", defaults = configJSON$symbolLinks)
if(length(configJSON$scripts$base))
  addArrayEl(session, "scripts_base", defaults = configJSON$scripts$base)
if(length(configJSON$scripts$hcube))
  addArrayEl(session, "scripts_hcube", defaults = configJSON$scripts$hcube)

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
    filename = input$general_readmeFileName))
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
    rv$generalConfig$extraClArgs <<- NULL
    configJSON$extraClArgs <<- NULL
  }
  else{
    rv$generalConfig$extraClArgs <<- input$general_args
  }
})

observeEvent(input$general_scen, {
  rv$generalConfig$defCompMode <<- input$general_scen
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
observeEvent(input$general_aggregate, {
  rv$generalConfig$aggregateWidgets <<- input$general_aggregate
})
observeEvent(input$general_overwriteSheetOrderInput, {
  if(!identical(input$general_overwriteSheetOrderInput, unname(inputSymMultiDim))){
    rv$generalConfig$overwriteSheetOrder$input <<- input$general_overwriteSheetOrderInput
  }
})
observeEvent(input$general_overwriteSheetOrderOutput, {
  if(!identical(input$general_overwriteSheetOrderOutput, names(modelOut))){
    rv$generalConfig$overwriteSheetOrder$output <<- input$general_overwriteSheetOrderOutput
  }
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
  observeEvent(input[[paste0("general_overwriteSymHeaders_", name)]], ignoreNULL = FALSE, {
    newHeaders <- input[[paste0("general_overwriteSymHeaders_", name)]]
    defaultAlias <- FALSE
    if(name %in% names(modelOut)){
      if(length(newHeaders) != length(dataContract$outputSymbols[[name]]$headers) ||
         identical(newHeaders, vapply(dataContract$outputSymbols[[name]]$headers, "[[", 
                                      character(1L), "alias",
                                      USE.NAMES = FALSE))){
        defaultAlias <- TRUE
      }
    }else{
      if(length(newHeaders) != length(dataContract$inputSymbols[[name]]$headers) ||
         identical(newHeaders, vapply(dataContract$inputSymbols[[name]]$headers, "[[", 
                                      character(1L), "alias",
                                      USE.NAMES = FALSE))){
        defaultAlias <- TRUE
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
observeEvent(input$general_save_trace, {
  rv$generalConfig$saveTraceFile <<- input$general_save_trace
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
observeEvent(input$general_pivotcolor, {
  if(!nchar(input$general_pivotcolor))
    configJSON$pivottable$bgColor <<- "#00000000"
  if(nchar(input$general_pivotcolor))
    rv$generalConfig$pivottable$bgColor <<- input$general_pivotcolor
  else
    rv$generalConfig$pivottable$bgColor <<- NULL
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
  
  if(length(input$add_general) < 3L || nchar(trimws(input$add_general[2])) < 1L){
    # name has no characters
    if(arrayIdx <= length(rv$generalConfig[[arrayID]])){
      rv$generalConfig[[arrayID]][[arrayIdx]] <<- NULL
      if(!length(rv$generalConfig[[arrayID]])){
        rv$generalConfig[[arrayID]] <<- NULL
      }
    }
    newName <- NULL
    showElReplaceTxt(session, paste0("#", input$add_general[3], input$add_general[1], "_err"), 
                     lang$adminMode$widgets$validate$val36)
  }else{
    newName <- input$add_general[2]
    if(arrayIdx <= length(rv$generalConfig[[arrayID]]) && length(rv$generalConfig[[arrayID]][[arrayIdx]])){
      rv$generalConfig[[arrayID]][[arrayIdx]]$name <- newName
    }else if(arrayIdx <= length(groupTemp[[arrayID]]) && 
             length(groupTemp[[arrayID]][[arrayIdx]]$members) > 1L &&
             !any(groupTemp[[arrayID]][[arrayIdx]]$members %in% 
                  unlist(lapply(rv$generalConfig[[arrayID]][-arrayIdx], "[[", "members"), use.names = FALSE))){
      rv$generalConfig[[arrayID]][[arrayIdx]] <- list(name = newName, 
                                                      members = groupTemp[[arrayID]][[arrayIdx]]$members,
                                                      sameTab = isTRUE(groupTemp[[arrayID]][[arrayIdx]]$sameTab))
    }else{
      showElReplaceTxt(session, paste0("#group_member", 
                                       if(identical(arrayID, "inputGroups")) "In" else "Out", 
                                       input$add_general[1], "_err"), 
                       lang$adminMode$widgets$validate$val37)
    }
    hideEl(session, paste0("#", input$add_general[3], input$add_general[1], "_err"))
  }
  if(arrayIdx > length(groupTemp[[arrayID]])){
    groupTemp[[arrayID]][[arrayIdx]] <<- list(name = newName)
  }else{
    groupTemp[[arrayID]][[arrayIdx]]$name <<- newName
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
  if(!grepl("^[[:alnum:]]+$", input$add_script[2])){
    showElReplaceTxt(session, paste0("#", input$add_script[3], arrayID, "_err"), 
                     lang$adminMode$widgets$validate$val49)
    return()
  }
  isHcubeScript <- identical(input$add_script[3], "scripts_hcube")
  if(isHcubeScript){
    validScriptIds <- hcubeScriptValidator$getValid("id")
  }else{
    validScriptIds <- baseScriptValidator$getValid("id")
  }
  if(input$add_script[2] %in% validScriptIds){
    showElReplaceTxt(session, paste0("#", input$add_script[3], arrayID, "_err"), 
                     lang$adminMode$widgets$validate$val50)
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
changeAndValidateGroupMembers <- function(arrayID, groupMembers, HTMLarrayID){
  arrayIdx <- indexMap$push(arrayID, groupMembers[1])
  
  if(length(groupMembers) > 2L && 
     !any(groupMembers[-1] %in% unlist(lapply(rv$generalConfig[[arrayID]][-arrayIdx], "[[", "members"), use.names = FALSE))){
    newMembers <- groupMembers[2:length(groupMembers)]
    if(arrayIdx <= length(rv$generalConfig[[arrayID]]) && length(rv$generalConfig[[arrayID]][[arrayIdx]])){
      rv$generalConfig[[arrayID]][[arrayIdx]]$members <- newMembers
    }else if(arrayIdx <= length(groupTemp[[arrayID]]) && 
             length(groupTemp[[arrayID]][[arrayIdx]]$name)){
      rv$generalConfig[[arrayID]][[arrayIdx]] <- list(name = groupTemp[[arrayID]][[arrayIdx]]$name, 
                                                      members = newMembers,
                                                      sameTab = isTRUE(groupTemp[[arrayID]][[arrayIdx]]$sameTab))
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
    }
    newMembers <- NULL
    showElReplaceTxt(session, paste0("#", HTMLarrayID, groupMembers[1], "_err"), 
                     lang$adminMode$widgets$validate$val37)
  }
  if(arrayIdx > length(groupTemp[[arrayID]])){
    groupTemp[[arrayID]][[arrayIdx]] <<- list(members = newMembers)
  }else{
    groupTemp[[arrayID]][[arrayIdx]]$members <<- newMembers
  }
}
observeEvent(input$group_memberIn, {
  changeAndValidateGroupMembers("inputGroups", input$group_memberIn, 
                                "group_memberIn")
})
observeEvent(input$group_memberOut, {
  changeAndValidateGroupMembers("outputGroups", input$group_memberOut, 
                                "group_memberOut")
})
observeEvent(input$group_sameTabIn, {
  if(length(input$group_sameTabIn) < 2L)
    return()
  
  arrayIdx <- indexMap$push("inputGroups", input$group_sameTabIn[1])
  
  if(arrayIdx <= length(rv$generalConfig[["inputGroups"]]) &&
     length(rv$generalConfig[["inputGroups"]][[arrayIdx]])){
    rv$generalConfig[["inputGroups"]][[arrayIdx]]$sameTab <<- identical(input$group_sameTabIn[2], 1L)
  }else if(arrayIdx <= length(groupTemp[["inputGroups"]]) &&
           length(groupTemp[["inputGroups"]][[arrayIdx]])){
    groupTemp[["inputGroups"]][[arrayIdx]]$sameTab <<- identical(input$group_sameTabIn[2], 1L)
  }else{
    groupTemp[["inputGroups"]][[arrayIdx]] <<- list(sameTab = identical(input$group_sameTabIn[2], 1L))
  }
})
observeEvent(input$group_sameTabOut, {
  if(length(input$group_sameTabOut) < 2L)
    return()
  
  arrayIdx <- indexMap$push("outputGroups", input$group_sameTabOut[1])
  
  if(arrayIdx <= length(rv$generalConfig[["outputGroups"]]) && 
     length(rv$generalConfig[["outputGroups"]][[arrayIdx]])){
    rv$generalConfig[["outputGroups"]][[arrayIdx]]$sameTab <<- identical(input$group_sameTabOut[2], 1L)
  }else if(arrayIdx <= length(groupTemp[["outputGroups"]]) &&
           length(groupTemp[["outputGroups"]][[arrayIdx]])){
    groupTemp[["outputGroups"]][[arrayIdx]]$sameTab <<- identical(input$group_sameTabOut[2], 1L)
  }else{
    groupTemp[["outputGroups"]][[arrayIdx]] <<- list(sameTab = identical(input$group_sameTabOut[2], 1L))
  }
})
observeEvent(input$remove_general, {
  arrayID <- strsplit(input$remove_general[1], "_")[[1]][2]
  arrayIdx <- indexMap$pop(arrayID, 
                           input$remove_general[2])
  
  if(length(arrayIdx)){
    if(arrayIdx <= length(rv$generalConfig[[arrayID]])){
      rv$generalConfig[[arrayID]][[arrayIdx]] <<- NULL
      if(!length(rv$generalConfig[[arrayID]])){
        rv$generalConfig[[arrayID]] <<- NULL
      }
    }
    if(arrayIdx <= length(groupTemp[[arrayID]])){
      groupTemp[[arrayID]][[arrayIdx]] <<- NULL
    }
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
                           oninput="Miro.mdToHTML(this.value, '#mdConvertedContent')",
                           onload = "Miro.mdToHTML(this.value, '#mdConvertedContent')",
                           readmeContent)
      ),
      column(6L, 
             tags$div(class = "readme-preview-header", lang$adminMode$general$readme$dialogEdit$markdownHeader),
             tags$div(id = "mdConvertedContent", 
                      class = "readme-wrapper readme-preview-output", readmeContentParsed)
      )
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
  if(length(rv$generalConfig$inputGroups) || 
     length(rv$generalConfig$outputGroups)){
    newGeneralJSON <- rv$generalConfig
    newGeneralJSON$inputGroups[vapply(newGeneralJSON$inputGroups, is.null, logical(1L), USE.NAMES = FALSE)] <- NULL
    newGeneralJSON$outputGroups[vapply(newGeneralJSON$outputGroups, is.null, logical(1L), USE.NAMES = FALSE)] <- NULL
    configJSON <<- modifyList(configJSON, newGeneralJSON)
  }else{
    configJSON <<- modifyList(configJSON, rv$generalConfig)
  }
  configJSON$symbolLinks <<- rv$generalConfig$symbolLinks
  configJSON$scripts <<- rv$generalConfig$scripts
  write_json(configJSON, configJSONFileName, pretty = TRUE, auto_unbox = TRUE, null = "null")
})

rowtmp <- list()
isolate({
  groupIndexMap <- IdIdxMap$new(list(inputGroups = seq_along(configJSON$inputGroups),
                                     outputGroups = seq_along(configJSON$outputGroups)))

  groupTemp <- list(inputGroups = list(), outputGroups = list())
  rv$generalConfig$inputGroups <- configJSON$inputGroups
  rv$generalConfig$outputGroups <- configJSON$outputGroups
})
scalarSymbols <- setNames(c(names(modelIn), 
                            if(length(modelIn[[scalarsFileName]])) 
                              modelIn[[scalarsFileName]]$symnames),  
                          c(modelInAlias, 
                            if(length(modelIn[[scalarsFileName]])) 
                              modelIn[[scalarsFileName]]$symtext))
scalarSymbols <- scalarSymbols[scalarSymbols %in% scalarInputSym]
updateSelectInput(session, "general_hidden", choices = scalarSymbols)

langSpecific <- list()
langSpecific$theme <- c("Use system/browser settings" = "browser", 
                        "Light mode" = "light", "Dark mode" = "dark")
names(langSpecific$theme) <- lang$adminMode$general$theme$choices
langSpecific$language <- c("English" = "en", "German" = "de", "Chinese" = "cn")
names(langSpecific$language) <- lang$adminMode$general$language$choices
langSpecific$scen <- c("Split screen (suited for 2 scenarios to compare)" = "split", "Tab view 
                        (suited for > 2 scenarios to compare)" = "tab")
names(langSpecific$scen) <- lang$adminMode$general$scen$choices

removeUI(selector = "#interface_wrapper1 .shiny-input-container", multiple = TRUE)
removeUI(selector = "#module_wrapper1 .shiny-input-container", multiple = TRUE)

insertUI(selector = "#interface_wrapper1",
         tagList(
           tags$h2(lang$adminMode$general$ui$headerGeneral, class="option-category"),
           tags$div(
             radioButtons("general_theme", lang$adminMode$general$theme$label, 
                          choices = langSpecific$theme,
                          selected = if(length(configJSON$theme)) configJSON$theme else config$theme
             )),
           tags$label(class = "cb-label", "for" = "general_act_log", lang$adminMode$general$actLog$label),
           tags$div(
             tags$label(class = "checkbox-material", 
                        checkboxInput("general_act_log", value = if(length(configJSON$activateModules$logFile)) configJSON$activateModules$logFile else config$activateModules$logFile, label = NULL)
             )),
           tags$label(class = "cb-label", "for" = "general_act_lst", lang$adminMode$general$actLst$label),
           tags$div(
             tags$label(class = "checkbox-material", 
                        checkboxInput("general_act_lst", value = if(length(configJSON$activateModules$lstFile)) configJSON$activateModules$lstFile else config$activateModules$lstFile, label = NULL)
             )),
           tags$div(class = "option-wrapper info-position",
                    textInput("general_mirologfile", tags$div(lang$adminMode$general$mirologfile$label, 
                                                            tags$a("", class="info-wrapper", href="https://gams.com/miro/customize.html#miro-log", 
                                                                   tags$span(class="fas fa-info-circle", class="info-icon"), target="_blank")),
                              value = if(!is.null(configJSON$miroLogFile) && nchar(configJSON$miroLogFile)) configJSON$miroLogFile else ""
                    )),
           tags$div(class = "option-wrapper info-position",
                    selectInput("general_scen", tags$div(lang$adminMode$general$scen$label, 
                                                         tags$a("", class="info-wrapper", href="https://gams.com/miro/start.html#scenario-comparison", 
                                                                tags$span(class="fas fa-info-circle", class="info-icon"), target="_blank")), 
                                choices = langSpecific$scen,
                                selected = if(length(configJSON$defCompMode)) configJSON$defCompMode else config$defCompMode
                    )),
           tags$label(class = "cb-label", "for" = "general_auto",
                      lang$adminMode$general$auto$label),
           tags$div(
             tags$label(class = "checkbox-material", 
                        checkboxInput("general_auto", value = if(length(configJSON$autoGenInputGraphs)) configJSON$autoGenInputGraphs else config$autoGenInputGraphs, label = NULL)
             )),
           tags$div(class="option-wrapper",
                    colorPickerInput("general_pivotcolor", label = lang$adminMode$general$pivotcolor$label,
                                     value = if(length(configJSON$pivottable$bgColor)) configJSON$pivottable$bgColor else "rgb(255, 128, 0)"
                    )),
           tags$div(class="option-wrapper",
                    sliderInput("general_decimal", label = lang$adminMode$general$decimal$label,
                                min = 0, max = 6, step = 1, value = if(length(configJSON$roundingDecimals)) configJSON$roundingDecimals else config$roundingDecimals
                    )),
           tags$hr(),
           tags$h2(lang$adminMode$general$ui$headerLogo),
           tags$div(class = "option-wrapper", style = "margin-bottom: 5px;",
                    fileInput("widget_general_logo_upload", lang$adminMode$general$logo$label,
                              width = "100%",
                              multiple = FALSE,
                              accept = c(".png", ".PNG", ".jpg", ".JPG"))),
           tags$label(class = "cb-label", "for" = "general_logo_preview", style = "padding-left: 25px;", "Logo preview:",
                      tags$div(class="logo-wrapper",
                               imageOutput("general_logo_preview", height = "50px", width = "230px")
                      ))
         ), 
         where = "beforeEnd")
# set default values for input and output groups
if(length(configJSON$inputGroups))
  addArrayEl(session, "symbol_inputGroups", defaults = configJSON$inputGroups)
if(length(configJSON$outputGroups))
  addArrayEl(session, "symbol_outputGroups", defaults = configJSON$outputGroups)
insertUI(selector = "#interface_wrapper2",
         tagList(
           tags$h2(lang$adminMode$general$ui$headerScalars, class="option-category"),
           if(length(modelOut[[scalarsOutName]])){
             tags$div(class="option-wrapper",
                      tags$div(class = "info-position", selectInput("general_hidden", 
                                                                       tags$div(lang$adminMode$general$hidden$label, tags$a("", class="info-wrapper", href="https://gams.com/miro/customize.html#hidden-scalars", 
                                                                              tags$span(class="fas fa-info-circle", class="info-icon"), target="_blank")),
                                                                       choices = setNames(modelOut[[scalarsOutName]]$symnames, modelOut[[scalarsOutName]]$symtext), 
                                                                       selected = configJSON$hiddenOutputScalars, multiple = TRUE)
                      ))
           },
           tags$div(class="option-wrapper", title = lang$adminMode$general$aggregate$title,
                    tags$label(class = "cb-label", "for" = "general_aggregate", lang$adminMode$general$aggregate$label),
                    tags$div(
                      tags$label(class = "checkbox-material", 
                                 checkboxInput("general_aggregate", value = if(length(configJSON$aggregateWidgets)) configJSON$aggregateWidgets else config$aggregateWidgets, label = NULL)
                      ))
           ),
           tags$hr(),
           tags$div(class = "info-position",
                    tags$h2((lang$adminMode$general$ui$headerTabGrouping), 
                            tags$a(class="info-wrapper", style="top:-10px;", href="https://gams.com/miro/customize.html#tab-grouping", 
                                   tags$span(class="fas fa-info-circle", class="info-icon"), target="_blank"))
           ),
           tags$h4(lang$adminMode$general$ui$headerInputGroups),
           tags$div(class="option-wrapper-indented",
                    createArray(session, "symbol_inputGroups", lang$adminMode$general$groups$input, autoCreate = FALSE)),
           tags$h4(lang$adminMode$general$ui$headerOutputGroups),
           tags$div(class="option-wrapper-indented",
                    createArray(session, "symbol_outputGroups", lang$adminMode$general$groups$output, autoCreate = FALSE))
         ), 
         where = "beforeEnd")

insertUI(selector = "#module_wrapper1",
         tagList(
           tags$h2(lang$adminMode$general$ui$headerScenData, class="option-category"),
           tags$div(title = lang$adminMode$general$actUpload$title,
                    tags$label(class = "cb-label info-position", "for" = "general_act_upload", 
                               tags$div(lang$adminMode$general$actUpload$label, tags$a("", class="info-wrapper", href="https://gams.com/miro/customize.html#local-upload", 
                                                                                       tags$span(class="fas fa-info-circle", class="info-icon"), target="_blank"))),
                    tags$div(
                      tags$label(class = "checkbox-material", 
                                 checkboxInput("general_act_upload", value = if(length(configJSON$activateModules$loadLocal)) configJSON$activateModules$loadLocal else config$activateModules$loadLocal, label = NULL)
                      ))
           ),
           tags$div(class = "shiny-input-container",
                    tags$label(class = "cb-label info-position", "for" = "default_scen_check",
                               tags$div(lang$adminMode$general$defaultScenName$checkbox, tags$a("", class="info-wrapper", 
                                      href="https://gams.com/miro/customize.html#default-scenario", 
                                      tags$span(class="fas fa-info-circle", class="info-icon"), target="_blank"))),
                    tags$div(
                      tags$label(class = "checkbox-material", 
                                 checkboxInput("default_scen_check", label = NULL, value = if(length(configJSON$defaultScenName) && nchar(configJSON$defaultScenName)) TRUE else FALSE)
                      ))
           ),
           conditionalPanel(
             condition = "input.default_scen_check===true",
             tags$div(class = "option-wrapper", style = "padding-right:30px;padding-left:40px;",
                      textInput("general_default_scen_name", lang$adminMode$general$defaultScenName$label,
                                value = if(length(configJSON$defaultScenName)) configJSON$defaultScenName else NULL))),
           tags$div(title = lang$adminMode$general$meta$title,
                    tags$label(class = "cb-label", "for" = "general_meta",
                               lang$adminMode$general$meta$label),
                    tags$div(
                      tags$label(class = "checkbox-material", 
                                 checkboxInput("general_meta", value = if(length(configJSON$excelIncludeMeta)) configJSON$excelIncludeMeta else config$excelIncludeMeta, label = NULL)
                      ))
           ),
           tags$div(title = lang$adminMode$general$empty$title,
                    tags$label(class = "cb-label", "for" = "general_empty",
                               lang$adminMode$general$empty$label),
                    tags$div(
                      tags$label(class = "checkbox-material", 
                                 checkboxInput("general_empty", value = if(identical(configJSON$excelIncludeEmptySheets, FALSE)) FALSE else TRUE, label = NULL)
                      ))
           ),
           tags$div(title = lang$adminMode$general$actAttach$title,
                    tags$label(class = "cb-label info-position", "for" = "general_act_attach", 
                               tags$div(lang$adminMode$general$actAttach$label, tags$a("", class="info-wrapper", href="https://gams.com/miro/start.html#file-attachment", 
                                      tags$span(class="fas fa-info-circle", class="info-icon"), target="_blank"))),
                    tags$div(
                      tags$label(class = "checkbox-material", 
                                 checkboxInput("general_act_attach", value = if(length(configJSON$activateModules$attachments)) configJSON$activateModules$attachments else config$activateModules$attachments, label = NULL)
                      ))
           ),
           tags$div(class="option-wrapper",
                    sliderInput("general_save_duration", tags$div(lang$adminMode$general$saveDuration$label, 
                                                                  tags$a("", class="info-wrapper", href="https://gams.com/miro/customize.html#general-duration", 
                                                                         tags$span(class="fas fa-info-circle", class="info-icon"), target="_blank")),
                                min = 0, max = 999, step = 1, value = if(length(configJSON$storeLogFilesDuration)) configJSON$storeLogFilesDuration else config$storeLogFilesDuration
                    ))
         ), 
         where = "beforeEnd")

insertUI(selector = "#module_wrapper2",
         tagList(
           tags$h2(lang$adminMode$general$ui$headerComputation, class="option-category"),
           tags$label(class = "cb-label", "for" = "general_remote_execution", tags$div(lang$adminMode$general$remoteExecution$label, tags$a("", class="info-wrapper", 
                                                                                                                                            href="https://gams.com/miro/", 
                                                                                                                                            tags$span(class="fas fa-info-circle", class="info-icon"), target="_blank"))),
           tags$div(
             tags$label(class = "checkbox-material", 
                        checkboxInput("general_remote_execution", value = if(length(configJSON$activateModules$remoteExecution)) configJSON$activateModules$remoteExecution else config$activateModules$remoteExecution, label = NULL)
             )),
           tags$label(class = "cb-label", "for" = "general_hcubeSwitch", tags$div(lang$adminMode$general$hcubeSwitch$label, tags$a("", class="info-wrapper", 
                                                                                                                                   href="https://gams.com/miro/hypercube.html", 
                                                                                                                                   tags$span(class="fas fa-info-circle", class="info-icon"), target="_blank"))),
           tags$div(
             tags$label(class = "checkbox-material", 
                        checkboxInput("general_hcubeSwitch", value = if(length(configJSON$activateModules$hcubeSwitch)) configJSON$activateModules$hcubeSwitch else config$activateModules$hcubeSwitch, label = NULL)
             )),
           tags$div(title = lang$adminMode$general$saveTrace$title,
                    tags$label(class = "cb-label", "for" = "general_save_trace", lang$adminMode$general$saveTrace$label),
                    tags$div(
                      tags$label(class = "checkbox-material", 
                                 checkboxInput("general_save_trace", value = if(length(configJSON$saveTraceFile)) configJSON$saveTraceFile else config$saveTraceFile, label = NULL)
                      ))
           ),
           tags$div(title = lang$adminMode$general$saveTrace$title,
                    tags$label(class = "cb-label", "for" = "general_save_trace", lang$adminMode$general$saveTrace$label),
                    tags$div(
                      tags$label(class = "checkbox-material", 
                                 checkboxInput("general_save_trace", value = if(length(configJSON$saveTraceFile)) configJSON$saveTraceFile else config$saveTraceFile, label = NULL)
                      ))
           ),
           tags$label(class = "cb-label info-position", "for" = "general_parent", 
                      tags$div(lang$adminMode$general$parent$label, tags$a("", class="info-wrapper", 
                                                                           href="https://gams.com/miro/customize.html#include-parent", 
                                                                           tags$span(class="fas fa-info-circle", class="info-icon"), target="_blank"))),
           tags$div(
             tags$label(class = "checkbox-material", 
                        checkboxInput("general_parent", value = if(length(configJSON$includeParentDir)) configJSON$includeParentDir else config$includeParentDir, label = NULL)
             )),
           tags$div(class="option-wrapper",
                    selectizeInput("general_args", tags$div(lang$adminMode$general$args$label, 
                                                            tags$a("", class="info-wrapper", href="https://gams.com/miro/customize.html#include-parent", 
                                                                   tags$span(class="fas fa-info-circle", class="info-icon"), target="_blank")),
                                                            choices = configJSON$extraClArgs, selected = configJSON$extraClArgs, 
                                                            multiple = TRUE, options = list('create' = TRUE,'persist' = FALSE)))
         ), 
         where = "beforeEnd")

output$general_logo_preview <- renderImage({
  rv$customLogoChanged
  isolate({
    if(identical(rv$generalConfig$UILogo, "gams_logo.png") || !length(rv$generalConfig$UILogo)){
      filename <- normalizePath(file.path(getwd(), "www", "gams_logo.png"))
    }else{
      filename <- normalizePath(file.path(currentModelDir, "static", rv$generalConfig$UILogo))
    }
  })
  list(src = filename, height = "50px", width = "230px", alt = "custom logo")
}, deleteFile = FALSE)


observeEvent(input$general_language, {
  rv$generalConfig$language <<- input$general_language
})
observeEvent(input$general_theme, {
  rv$generalConfig$theme <<- input$general_theme
})
observeEvent(input$general_parent, {
  rv$generalConfig$includeParentDir <<- input$general_parent
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
observeEvent(input$general_logo, {
  if(identical(input$general_logo, FALSE)){
    rv$generalConfig$UILogo <<- "gams_logo.png"
    rv$customLogoChanged <<- rv$customLogoChanged + 1L
  }
})
observeEvent(input$widget_general_logo_upload, {
  inFile   <- input$widget_general_logo_upload
  filePath <- inFile$datapath
  fileName <- inFile$name
  if(!dir.exists(file.path(currentModelDir, "static"))){
    if(!dir.create(file.path(currentModelDir, "static"))){
      flog.error("A problem occurred creating directory: %s. Maybe you have insufficient permissions?", file.path(currentModelDir, "static"))
      showModal(modalDialog(lang$adminMode$general$modalDialog$title, lang$adminMode$general$modalDialog$content))
      return()
    }
  }else{
    filesToDelete <- list.files(file.path(currentModelDir, "static"), full.names = TRUE)
    filesFailedToDelete <- !file.remove(filesToDelete)
    if(any(filesFailedToDelete)){
      flog.error("Problems removing files: '%s'. Do you lack the necessary permissions?", paste(filesToDelete[filesFailedToDelete], collapse = "', '"))
      showModal(modalDialog(lang$adminMode$general$modalDialog$title, lang$adminMode$general$modalDialog$content))
      return()
    }
  }
  if(!file.copy(filePath, file.path(currentModelDir, "static", fileName))){
    flog.error("A problem occurred copying image (%s) to folder: %s. Maybe you have insufficient permissions?", filePath, file.path(currentModelDir, "static"))
    showModal(modalDialog(lang$adminMode$general$modalDialog$title, lang$adminMode$general$modalDialog$content))
    return()
  }
  rv$generalConfig$UILogo <<- fileName
  rv$customLogoChanged <<- rv$customLogoChanged + 1L
})

observeEvent(input$general_auto, {
  rv$generalConfig$autoGenInputGraphs <<- input$general_auto
})
observeEvent(input$general_save_duration, {
  rv$generalConfig$storeLogFilesDuration <<- input$general_save_duration
})

observeEvent(input$general_args, {
  req(length(input$general_args))
  rv$generalConfig$extraClArgs <<- input$general_args
})

observeEvent(input$general_scen, {
  rv$generalConfig$defCompMode <<- input$general_scen
})
observeEvent(input$general_act_strict, {
  rv$generalConfig$activateModules$strictmode <<- input$general_act_strict
})
observeEvent(input$general_act_upload, {
  rv$generalConfig$activateModules$loadLocal <<- input$general_act_upload
})
observeEvent(input$general_act_share_scen, {
  rv$generalConfig$activateModules$sharedScenarios <<- input$general_act_share_scen
})
observeEvent(input$general_act_hcube, {
  rv$generalConfig$activateModules$hcubeMode <<- input$general_act_hcube
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
  rv$generalConfig$overwriteSheetOrder$input <<- input$general_overwriteSheetOrderInput
})
observeEvent(input$general_overwriteSheetOrderOutput, {
  rv$generalConfig$overwriteSheetOrder$output <<- input$general_overwriteSheetOrderOutput
})
observeEvent(input$general_overwriteSheetOrderOutput, {
  rv$generalConfig$overwriteSheetOrder$output <<- input$general_overwriteSheetOrderOutput
})
lapply(c(names(modelInRaw), names(modelOut)), function(name){
  observeEvent(input[[paste0("general_overwriteSymAlias_", name)]], {
    if(length(input[[paste0("general_overwriteSymAlias_", name)]]) && 
       nchar(input[[paste0("general_overwriteSymAlias_", name)]]) > 0L){
      if(!length(rv$generalConfig$overwriteAliases)){
        rv$generalConfig$overwriteAliases <- list()
      }
      rv$generalConfig$overwriteAliases[[name]] <<- list(newAlias = input[[paste0("general_overwriteSymAlias_", name)]])
    }
  })
  observeEvent(input[[paste0("general_overwriteSymHeaders_", name)]], {
    i <- match(name, names(modelInRaw))
    if(is.na(i)){
      i <- match(name, names(modelOut))
      if(is.na(i)){
        return()
      }
      headerLen <- length(modelOut[[i]]$headers)
    }else{
      headerLen <- length(modelOut[[i]]$headers)
    }
    if(length(input[[paste0("general_overwriteSymHeaders_", name)]]) ==
       headerLen){
      if(!length(rv$generalConfig$overwriteAliases)){
        rv$generalConfig$overwriteHeaderAliases <- list()
      }
      rv$generalConfig$overwriteHeaderAliases[[name]] <<- list(newHeaders = input[[paste0("general_overwriteSymHeaders_", name)]])
    }
  })
})
observeEvent(input$general_remote_execution, {
  rv$generalConfig$activateModules$remoteExecution <<- input$general_remote_execution
})
observeEvent(input$general_hcubeSwitch, {
  rv$generalConfig$activateModules$hcubeSwitch <<- input$general_hcubeSwitch
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

observeEvent(input$general_decimal, {
  rv$generalConfig$roundingDecimals <<- input$general_decimal
})
observeEvent(input$general_pivotcolor, {
  if(!nchar(input$general_pivotcolor))
    configJSON$pivottable$bgColor <<- NULL
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
  arrayIdx <- groupIndexMap$push(arrayID, input$add_general[1])
  
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
                                                      members = groupTemp[[arrayID]][[arrayIdx]]$members)
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
changeAndValidateGroupMembers <- function(arrayID, groupMembers, HTMLarrayID){
  arrayIdx <- groupIndexMap$push(arrayID, groupMembers[1])
  
  if(length(groupMembers) > 2L && 
     !any(groupMembers[-1] %in% unlist(lapply(rv$generalConfig[[arrayID]][-arrayIdx], "[[", "members"), use.names = FALSE))){
    newMembers <- groupMembers[2:length(groupMembers)]
    if(arrayIdx <= length(rv$generalConfig[[arrayID]]) && length(rv$generalConfig[[arrayID]][[arrayIdx]])){
      rv$generalConfig[[arrayID]][[arrayIdx]]$members <- newMembers
    }else if(arrayIdx <= length(groupTemp[[arrayID]]) && 
             length(groupTemp[[arrayID]][[arrayIdx]]$name)){
      rv$generalConfig[[arrayID]][[arrayIdx]] <- list(name = groupTemp[[arrayID]][[arrayIdx]]$name, 
                                                      members = newMembers)
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
  changeAndValidateGroupMembers('inputGroups', input$group_memberIn, 
                                "group_memberIn")
})
observeEvent(input$group_memberOut, {
  changeAndValidateGroupMembers('outputGroups', input$group_memberOut, 
                                "group_memberOut")
})
observeEvent(input$remove_general, {
  arrayID <- strsplit(input$remove_general[1], "_")[[1]][2]
  arrayIdx <- groupIndexMap$pop(arrayID, 
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
  
  write_json(configJSON, configJSONFileName, pretty = TRUE, auto_unbox = TRUE, null = "null")
})

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
langSpecific$language <- c("English" = "en", "German" = "de", "Chinese" = "cn")
names(langSpecific$language) <- lang$adminMode$general$language$choices
langSpecific$skin <- c("black" = "black", "blue" = "blue", "purple" = "purple", "green" = "green", "red" = "red", "yellow" = "yellow")
names(langSpecific$skin) <- lang$adminMode$general$skin$choices
langSpecific$scen <- c("Split screen (suited for 2 scenarios to compare)" = "split", "Tab view 
                        (suited for > 2 scenarios to compare)" = "tab")
names(langSpecific$scen) <- lang$adminMode$general$scen$choices

removeUI(selector = "#general_wrapper .shiny-input-container", multiple = TRUE)
removeUI(selector = "#general_wrapper2 .shiny-input-container", multiple = TRUE)

insertUI(selector = "#general_wrapper",
         tagList(
           createArray(session, "symbol_inputGroups", "Divide input symbols into groups", autoCreate = FALSE),
           createArray(session, "symbol_outputGroups", "Divide output symbols into groups", autoCreate = FALSE),
           radioButtons("general_language", label = lang$adminMode$general$language$label,
                        choices = langSpecific$language, 
                        selected = if(length(configJSON$language)) configJSON$language else config$language),
           tags$div(style = "max-width:400px;",
                    selectInput("general_skin", lang$adminMode$general$skin$label, 
                                choices = langSpecific$skin,
                                selected = if(length(configJSON$pageSkin)) configJSON$pageSkin else config$pageSkin)),
           tags$div(style = "max-width:400px;",
                    selectInput("general_scen", lang$adminMode$general$scen$label, 
                                choices = langSpecific$scen,
                                selected = if(length(configJSON$defCompMode)) configJSON$defCompMode else config$defCompMode
                    )),
           tags$div(class = "shiny-input-container",
                    tags$label(class = "cb-label", "for" = "default_scen_check",
                               lang$adminMode$general$defaultScenName$checkbox),
                    tags$div(
                      tags$label(class = "checkbox-material", 
                                 checkboxInput("default_scen_check", label = NULL, value = if(length(configJSON$defaultScenName) && nchar(configJSON$defaultScenName)) TRUE else FALSE)
                      ))
           ),
           conditionalPanel(
             condition = "input.default_scen_check===true",
             tags$div(style = "max-width:400px;padding-right:30px;padding-left:40px;",
                      textInput("general_default_scen_name", lang$adminMode$general$defaultScenName$label,
                                value = if(length(configJSON$defaultScenName)) configJSON$defaultScenName else NULL))),
           tags$div(style = "max-width:400px;",
                    selectizeInput("general_args", lang$adminMode$general$args$label, 
                                   choices = configJSON$extraClArgs, selected = configJSON$extraClArgs, multiple = TRUE, options = list(
                                     'create' = TRUE,
                                     'persist' = FALSE))),
           tags$div(style = "max-width:400px;",
                    textInput("general_input_scalars", lang$adminMode$general$inputScalars$label, value = configJSON$scalarAliases$inputScalars,
                              placeholder = lang$adminMode$general$inputScalars$placeholder)),
           tags$div(style = "max-width:400px;",
                    textInput("general_output_scalars", lang$adminMode$general$outputScalars$label, value = configJSON$scalarAliases$outputScalars,
                              placeholder = lang$adminMode$general$outputScalars$placeholder)),
           if(length(modelOut[[scalarsOutName]])){
             tags$div(style = "max-width:400px;",
                      tags$div(selectInput("general_hidden", lang$adminMode$general$hidden$label,
                                           choices = setNames(modelOut[[scalarsOutName]]$symnames, modelOut[[scalarsOutName]]$symtext), 
                                           selected = configJSON$hiddenOutputScalars, multiple = TRUE)
                      ))
           },
           tags$div(style = "max-width:400px;",
                    colorPickerInput("general_pivotcolor", label = lang$adminMode$general$pivotcolor$label,
                                     value = if(length(configJSON$pivottable$bgColor)) configJSON$pivottable$bgColor else "rgb(255, 128, 0)"
                    )),
           tags$div(style = "max-width:400px;",
                    sliderInput("general_save_duration", label = lang$adminMode$general$saveDuration$label,
                                min = 0, max = 999, step = 1, value = if(length(configJSON$storeLogFilesDuration)) configJSON$storeLogFilesDuration else config$storeLogFilesDuration
                    )),
           tags$div(style = "max-width:400px;",
                    sliderInput("general_decimal", label = lang$adminMode$general$decimal$label,
                                min = 0, max = 6, step = 1, value = if(length(configJSON$roundingDecimals)) configJSON$roundingDecimals else config$roundingDecimals
                    )),
           tags$div(id = "logo_option_wrapper",
                    tags$div(style = "max-width:400px; display:inline-block; margin-bottom: 5px;",
                             fileInput("widget_general_logo_upload", lang$adminMode$general$logo$label,
                                       width = "100%",
                                       multiple = FALSE,
                                       accept = c(".png", ".PNG", ".jpg", ".JPG"))),
                    tags$div(style = "display:inline-block; margin: 15px 15px 15px 25px; margin-bottom: 15px; vertical-align: top; border-style: solid; border-color: #eeeeee; border-width: 1px;",
                             imageOutput("general_logo_preview", height = "50px", width = "230px")
                    ))
         ), 
         where = "beforeEnd")
# set default values for input and output groups
if(length(configJSON$inputGroups))
  addArrayEl(session, "symbol_inputGroups", defaults = configJSON$inputGroups)
if(length(configJSON$outputGroups))
  addArrayEl(session, "symbol_outputGroups", defaults = configJSON$outputGroups)

insertUI(selector = "#general_wrapper2",
         tagList(
           tags$div(title = lang$adminMode$general$actScen$title,
                    tags$label(class = "cb-label", "for" = "general_act_scen", lang$adminMode$general$actScen$label),
                    tags$div(
                      tags$label(class = "checkbox-material", 
                                 checkboxInput("general_act_scen", value = if(length(configJSON$activateModules$scenario)) configJSON$activateModules$scenario else config$activateModules$scenario, label = NULL)
                      ))
           ),
           tags$label(class = "cb-label", "for" = "general_act_strict", lang$adminMode$general$actStrict$label),
           tags$div(
             tags$label(class = "checkbox-material", 
                        checkboxInput("general_act_strict", value = if(length(configJSON$activateModules$strictmode)) configJSON$activateModules$strictmode else config$activateModules$strictmode, label = NULL)
             )),
           tags$div(title = lang$adminMode$general$actUpload$title,
                    tags$label(class = "cb-label", "for" = "general_act_upload", lang$adminMode$general$actUpload$label),
                    tags$div(
                      tags$label(class = "checkbox-material", 
                                 checkboxInput("general_act_upload", value = if(length(configJSON$activateModules$loadLocal)) configJSON$activateModules$loadLocal else config$activateModules$loadLocal, label = NULL)
                      ))
           ),
           tags$div(
             tags$label(class = "cb-label", "for" = "general_act_share_scen", lang$adminMode$general$actShareScen$label),
             tags$div(
               tags$label(class = "checkbox-material", 
                          checkboxInput("general_act_share_scen", value = if(length(configJSON$activateModules$sharedScenarios)) configJSON$activateModules$sharedScenarios else config$activateModules$sharedScenarios, label = NULL)
               ))
           ),
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
           tags$div(title = lang$adminMode$general$actAttach$title,
                    tags$label(class = "cb-label", "for" = "general_act_attach", lang$adminMode$general$actAttach$label),
                    tags$div(
                      tags$label(class = "checkbox-material", 
                                 checkboxInput("general_act_attach", value = if(length(configJSON$activateModules$attachments)) configJSON$activateModules$attachments else config$activateModules$attachments, label = NULL)
                      ))
           ),
           tags$div(title = lang$adminMode$general$aggregate$title,
                    tags$label(class = "cb-label", "for" = "general_aggregate", lang$adminMode$general$aggregate$label),
                    tags$div(
                      tags$label(class = "checkbox-material", 
                                 checkboxInput("general_aggregate", value = if(length(configJSON$aggregateWidgets)) configJSON$aggregateWidgets else config$aggregateWidgets, label = NULL)
                      ))
           ),
           tags$label(class = "cb-label", "for" = "general_parent", lang$adminMode$general$parent$label),
           tags$div(
             tags$label(class = "checkbox-material", 
                        checkboxInput("general_parent", value = if(length(configJSON$includeParentDir)) configJSON$includeParentDir else config$includeParentDir, label = NULL)
             )),
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
           tags$div(title = lang$adminMode$general$saveTrace$title,
                    tags$label(class = "cb-label", "for" = "general_save_trace", lang$adminMode$general$saveTrace$label),
                    tags$div(
                      tags$label(class = "checkbox-material", 
                                 checkboxInput("general_save_trace", value = if(length(configJSON$saveTraceFile)) configJSON$saveTraceFile else config$saveTraceFile, label = NULL)
                      ))
           ),
           tags$label(class = "cb-label", "for" = "general_auto",
                      lang$adminMode$general$auto$label),
           tags$div(
             tags$label(class = "checkbox-material", 
                        checkboxInput("general_auto", value = if(length(configJSON$autoGenInputGraphs)) configJSON$autoGenInputGraphs else config$autoGenInputGraphs, label = NULL)
             ))
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
observeEvent(input$general_skin, {
  rv$generalConfig$pageSkin <<- input$general_skin
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
  if(nchar(input$general_default_scen_name) && identical(input$default_scen_check, TRUE))
    rv$generalConfig$defaultScenName <<- input$general_default_scen_name
  else
    rv$generalConfig$defaultScenName <<- ""
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
observeEvent(input$general_act_scen, {
  rv$generalConfig$activateModules$scenario <<- input$general_act_scen
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
observeEvent(input$general_input_scalars, {
  if(nchar(input$general_input_scalars))
    rv$generalConfig$scalarAliases$inputScalars <<- input$general_input_scalars
  else
    rv$generalConfig$scalarAliases$inputScalars <<- NULL
})
observeEvent(input$general_output_scalars, {
  if(nchar(input$general_output_scalars))
    rv$generalConfig$scalarAliases$outputScalars <<- input$general_output_scalars
  else
    rv$generalConfig$scalarAliases$outputScalars <<- NULL
})
observeEvent(input$general_save_trace, {
  rv$generalConfig$saveTraceFile <<- input$general_save_trace
})

observeEvent(input$general_hidden, {
  rv$generalConfig$hiddenOutputScalars <<- input$general_hidden
})

observeEvent(input$general_decimal, {
  rv$generalConfig$roundingDecimals <<- input$general_decimal
})
observeEvent(input$general_pivotcolor, {
  rv$generalConfig$pivottable$bgColor <<- input$general_pivotcolor
})
observeEvent(input$add_general, {
  if(length(input$add_general) < 3L){
    return()
  }
  arrayID  <- strsplit(input$add_general[3], "_")[[1]][2]
  arrayIdx <- groupIndexMap$push(arrayID, input$add_general[1])
  print(input$add_general)
  print(arrayIdx)
  if(length(input$add_general) < 3L || nchar(trimws(input$add_general[2])) < 1L){
    # name has no characters
    if(arrayIdx <= length(rv$generalConfig[[arrayID]])){
      rv$generalConfig[[arrayID]][[arrayIdx]] <<- NULL
      if(!length(rv$generalConfig[[arrayID]])){
        rv$generalConfig[[arrayID]] <<- NULL
      }
    }
    newName <- NULL
  }else{
    newName <- input$add_general[2]
    if(arrayIdx <= length(rv$generalConfig[[arrayID]])){
      rv$generalConfig[[arrayID]][[arrayIdx]]$name <- newName
    }else if(length(groupTemp[[arrayID]][[arrayIdx]]) && 
             length(groupTemp[[arrayID]][[arrayIdx]]$members)){
      rv$generalConfig[[arrayID]][[arrayIdx]] <- list(name = newName, 
                                                      members = groupTemp[[arrayID]][[arrayIdx]]$members)
    }
  }
  print(newName)
  if(arrayIdx > length(groupTemp[[arrayID]])){
    groupTemp[[arrayID]][[arrayIdx]] <<- list(name = newName)
  }else{
    groupTemp[[arrayID]][[arrayIdx]]$name <<- newName
  }
  print(groupTemp)
})
changeAndValidateGroupMembers <- function(arrayID, groupMembers){
  arrayIdx <- groupIndexMap$push(arrayID, groupMembers[1])
  print(arrayIdx)
  print(groupMembers)
  print(rv$generalConfig[[arrayID]])
  if(length(groupMembers) > 2L && 
     !any(groupMembers %in% unlist(lapply(rv$generalConfig[[arrayID]][-arrayIdx], "[[", "members"), use.names = FALSE))){
    newMembers <- groupMembers[2:length(groupMembers)]
    print(newMembers)
    print(rv$generalConfig[[arrayID]])
    print(groupTemp[[arrayID]][[arrayIdx]])
    if(arrayIdx <= length(rv$generalConfig[[arrayID]])){
      rv$generalConfig[[arrayID]][[arrayIdx]]$members <- newMembers
    }else if(length(groupTemp[[arrayID]][[arrayIdx]]) && 
             length(groupTemp[[arrayID]][[arrayIdx]]$name)){
      rv$generalConfig[[arrayID]][[arrayIdx]] <- list(name = groupTemp[[arrayID]][[arrayIdx]]$name, 
                                                      members = newMembers)
    }
  }else{
    print("invalid groups")
    if(arrayIdx <= length(rv$generalConfig[[arrayID]])){
      rv$generalConfig[[arrayID]][[arrayIdx]] <<- NULL
      if(!length(rv$generalConfig[[arrayID]])){
        rv$generalConfig[[arrayID]] <<- NULL
      }
    }
    newMembers <- NULL
  }
  if(arrayIdx > length(groupTemp[[arrayID]])){
    groupTemp[[arrayID]][[arrayIdx]] <<- list(members = newMembers)
  }else{
    groupTemp[[arrayID]][[arrayIdx]]$members <<- newMembers
  }
}
observeEvent(input$group_memberIn, {
  changeAndValidateGroupMembers('inputGroups', input$group_memberIn)
})
observeEvent(input$group_memberOut, {
  changeAndValidateGroupMembers('outputGroups', input$group_memberOut)
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
  configJSON <<- modifyList(configJSON, rv$generalConfig)
  write_json(configJSON, configJSONFileName, pretty = TRUE, auto_unbox = TRUE)
})

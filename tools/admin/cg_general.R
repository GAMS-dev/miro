scalarSymbols <- setNames(c(names(modelIn), 
                            if(length(modelIn[[scalarsFileName]])) 
                              modelIn[[scalarsFileName]]$symnames),  
                          c(modelInAlias, 
                            if(length(modelIn[[scalarsFileName]])) 
                              modelIn[[scalarsFileName]]$symtext))
scalarSymbols <- scalarSymbols[scalarSymbols %in% scalarInputSym]
updateSelectInput(session, "general_hidden", choices = scalarSymbols)

removeUI(selector = "#general_wrapper .shiny-input-container", multiple = TRUE)
insertUI(selector = "#general_wrapper",
         tagList(
           radioButtons("general_language", label = "Language",
                        choices = c("English" = "en", "German" = "de"), 
                        selected = if(length(configJSON$language)) configJSON$language else "en"),
           selectInput("general_skin", "Skin to use for dashboard", 
                       choices = c("black", "blue", "purple", "green", "red", "yellow"),
                       selected = if(length(configJSON$pageSkin)) configJSON$pageSkin else "black"),
           tags$label(class = "cb-label", "for" = "general_parent",
                      "Include parent directory of the model folder 
           in your model runs (e.g. because several models share files)?"),
           tags$div(
             tags$label(class = "checkbox-material", 
                        checkboxInput("general_parent", value = configJSON$includeParentDir, label = NULL)
             )),
           tags$div(title = "Metadata contains information about the user name, the scenario name and the creation time of the scenario",
                    tags$label(class = "cb-label", "for" = "general_meta",
                               "Include a metadata sheet in the Excel file (when exporting a scenario)?"),
                    tags$div(
                      tags$label(class = "checkbox-material", 
                                 checkboxInput("general_meta", value = if(identical(configJSON$excelIncludeMeta, FALSE)) FALSE else TRUE, label = NULL)
                      ))
           ),
           tags$div(title = "Sheets can be empty e.g. when the exported scenario only contains input data.",
                    tags$label(class = "cb-label", "for" = "general_empty",
                               "Include empty sheets in the Excel file"),
                    tags$div(
                      tags$label(class = "checkbox-material", 
                                 checkboxInput("general_empty", value = if(identical(configJSON$excelIncludeEmptySheets, FALSE)) FALSE else TRUE, label = NULL)
                      ))
           ),
           fileInput("widget_general_logo_upload", "Upload a custom logo for your MIRO app: png/jpg file (best format: 4,6:1)",
                     width = "100%",
                     multiple = FALSE,
                     accept = c(".png", ".PNG", ".jpg", ".JPG")),
           imageOutput("general_logo_preview", height = "50px", width = "230px"),
           tags$label(class = "cb-label", "for" = "general_auto",
                      "Generate graphs for each input sheet automatically (pivot tool)"),
           tags$div(
             tags$label(class = "checkbox-material", 
                        checkboxInput("general_auto", value = if(identical(configJSON$autoGenInputGraphs, FALSE)) FALSE else TRUE, label = NULL)
             )),
           sliderInput("general_save_duration", label = "Duration the GAMS log and lst files are stored in the database (in days). 
            0 means files are not stored at all, 999 means files are stored indefinitely. 
            This setting is ignored when the attachment module is not active. Note that this is currently 
            only supported in the MIRO base mode.",
                       min = 0, max = 999, step = 1, value = if(length(configJSON$storeLogFilesDuration)) configJSON$storeLogFilesDuration else 7L
           ),
           selectizeInput("general_args", "Specify extra command line arguments that GAMS will be called with (e.g. limrow=10,threads=4)", 
                          choices = configJSON$extraClArgs, selected = configJSON$extraClArgs, multiple = TRUE, options = list(
                            'create' = TRUE,
                            'persist' = FALSE)),
           selectInput("general_scen", "Default scenario comparison mode.", 
                       choices = c("Split screen (suited for 2 scenarios to compare)" = "split", "Tab view 
                        (suited for > 2 scenarios to compare)" = "tab"),
                       selected = configJSON$defCompMode
           ),
           tags$div(title = "Save, delete and compare scenarios",
                    tags$label(class = "cb-label", "for" = "general_act_scen", "Activate scenario functionality"),
                    tags$div(
                      tags$label(class = "checkbox-material", 
                                 checkboxInput("general_act_scen", value = if(identical(configJSON$activateModules$scenario, FALSE)) FALSE else TRUE, label = NULL)
                      ))
           ),
           tags$label(class = "cb-label", "for" = "general_act_strict", "Launch App in strict mode? This results in throwing 
           error messages instead of accepting possibly faulty user entries."),
           tags$div(
             tags$label(class = "checkbox-material", 
                        checkboxInput("general_act_strict", value = if(identical(configJSON$activateModules$strictmode, FALSE)) FALSE else TRUE, label = NULL)
             )),
           tags$div(title = "Enables the user to use local data for GAMS runs",
                    tags$label(class = "cb-label", "for" = "general_act_upload", "Activate local data upload module?"),
                    tags$div(
                      tags$label(class = "checkbox-material", 
                                 checkboxInput("general_act_upload", value = if(identical(configJSON$activateModules$loadLocal, FALSE)) FALSE else TRUE, label = NULL)
                      ))
           ),
           tags$div(
             tags$label(class = "cb-label", "for" = "general_act_share_scen", "Enable scenario sharing between different users"),
             tags$div(
               tags$label(class = "checkbox-material", 
                          checkboxInput("general_act_share_scen", value = if(identical(configJSON$activateModules$sharedScenarios, FALSE)) FALSE else TRUE, label = NULL)
               ))
           ),
           tags$div(title = "Efficient generation of multiple scenarios. Designed for scenario runs and sensitivity analisis.",
                    tags$label(class = "cb-label", "for" = "general_act_hcube", "Activate Hypercube mode"),
                    tags$div(
                      tags$label(class = "checkbox-material", 
                                 checkboxInput("general_act_hcube", value = configJSON$activateModules$hcubeMode, label = NULL)
                      ))
           ),
           tags$label(class = "cb-label", "for" = "general_act_log", "Show log file in UI"),
           tags$div(
             tags$label(class = "checkbox-material", 
                        checkboxInput("general_act_log", value = if(identical(configJSON$activateModules$logFile, FALSE)) FALSE else TRUE, label = NULL)
             )),
           tags$label(class = "cb-label", "for" = "general_act_lst", "Show lst file in UI"),
           tags$div(
             tags$label(class = "checkbox-material", 
                        checkboxInput("general_act_lst", value = if(identical(configJSON$activateModules$lstFile, FALSE)) FALSE else TRUE, label = NULL)
             )),
           tags$div(title = "Can be files of any format. MIRO distinguishes between two types of attachments: attachments that can be seen and read by your GAMS model and files that can not be seen.",
                    tags$label(class = "cb-label", "for" = "general_act_attach", "Should users be allowed to add attachments to scenarios?"),
                    tags$div(
                      tags$label(class = "checkbox-material", 
                                 checkboxInput("general_act_attach", value = if(identical(configJSON$activateModules$attachments, FALSE)) FALSE else TRUE, label = NULL)
                      ))
           ),
           tags$div(title = "If not activated, each input widget is displayed in a separate page.",
                    tags$label(class = "cb-label", "for" = "general_aggregate", "Should all input widgets (slider, dropdown menu, etc.) be
                    aggregated on a single page?"),
                    tags$div(
                      tags$label(class = "checkbox-material", 
                                 checkboxInput("general_aggregate", value = if(identical(configJSON$aggregateWidgets, FALSE)) FALSE else TRUE, label = NULL)
                      ))
           ),
           textInput("general_input_scalars", "Alias for the input scalars table"),
           textInput("general_output_scalars", "Alias for the output scalars table"),
           tags$div(title = "For performance analysis with the integrated analysis tool PAVER, this option needs to be activated.",
                    tags$label(class = "cb-label", "for" = "general_save_trace", "Save trace file with each GAMS run (Hypercube mode)"),
                    tags$div(
                      tags$label(class = "checkbox-material", 
                                 checkboxInput("general_save_trace", value = if(identical(configJSON$saveTraceFile, FALSE)) FALSE else TRUE, label = NULL)
                      ))
           ),
           if(length(modelOut[[scalarsOutName]])){
             tags$div(selectInput("general_hidden", "Scalars that should not be displayed in the scalars table 
                     but can be used in graphs etc.",
                                  choices = setNames(modelOut[[scalarsOutName]]$symnames, modelOut[[scalarsOutName]]$symtext), 
                                  selected = configJSON$hiddenOutputScalars, multiple = TRUE)
             )
           },
           sliderInput("general_decimal", label = "Number of decimal places used for rounding output values.",
                       min = 0, max = 6, step = 1, value = if(length(configJSON$roundingDecimals)) configJSON$roundingDecimals else 2L
           ),
           colorPickerInput("general_pivotcolor", label = "Background color of row and column headers in pivot tables.",
                            value = if(length(configJSON$pivottable$bgColor)) configJSON$pivottable$bgColor else "rgb(255, 128, 0)"
           )
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
      showModal(modalDialog("Error copying image", "A problem occurred copying image. If this problem persists, please contact a system administrator."))
      return()
    }
  }else{
    filesToDelete <- list.files(file.path(currentModelDir, "static"), full.names = TRUE)
    filesFailedToDelete <- !file.remove(filesToDelete)
    if(any(filesFailedToDelete)){
      flog.error("Problems removing files: '%s'. Do you lack the necessary permissions?", paste(filesToDelete[filesFailedToDelete], collapse = "', '"))
      showModal(modalDialog("Error copying image", "A problem occurred copying image. If this problem persists, please contact a system administrator."))
      return()
    }
  }
  if(!file.copy(filePath, file.path(currentModelDir, "static", fileName))){
    flog.error("A problem occurred copying image (%s) to folder: %s. Maybe you have insufficient permissions?", filePath, file.path(currentModelDir, "static"))
    showModal(modalDialog("Error copying image", "A problem occurred copying image. If this problem persists, please contact a system administrator."))
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

#  =======================================
#          SAVE JSON (automatically)
#  =======================================
observeEvent(rv$generalConfig, {
  req(length(rv$generalConfig))
  configJSON <<- modifyList(configJSON, rv$generalConfig)
  write(toJSON(configJSON, pretty = TRUE, auto_unbox = TRUE), configJSONFileName)
})

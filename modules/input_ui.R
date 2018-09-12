# generate modalDialog used to import input data
observeEvent(input$btImport, {
  flog.debug("%s: Import input data button clicked.", uid)
  #disable button animation
  shinyjs::addClass("btImport", "no-animations")
  isInSolveMode <<- TRUE
  if(config$activateModules$scenario){
    # fetch list of saved scenarios
    # only load single scenario as not in comparison mode
    errMsg <- NULL
    tryCatch({
      scenMetadata <<- db$fetchScenList(noBatch = TRUE)
    }, error = function(e){
      flog.error("Problems fetching list of saved scenarios from database. Error message: %s.", e)
      errMsg <<- sprintf(lang$errMsg$fetchScenData$desc, modelIn.alias[i])
    })
    if(is.null(showErrorMsg(lang$errMsg$fetchScenData$title, errMsg))){
      return(NULL)
    }
  }
  # local tab for data upload
  tabLoadFromLocalFile <- tabPanel(lang$nav$dialogImport$tabLocal, value = "tb_importData_local",
                                   tags$div(class = "space"),
                                   tags$div(id = "loadLocal_content",
                                            fluidRow(
                                              column(12,
                                                     fileInput("localInput", lang$nav$dialogImport$descLocal, width = "100%",
                                                               multiple = FALSE,
                                                               accept = c("application/vnd.ms-excel", 
                                                                          "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", 
                                                                          ".xlsx")),
                                                     if(is.null(isolate(rv$active.sname))){
                                                       tagList(
                                                         hidden(tags$div(id = "local_badScenName", class = "errMsg", 
                                                                         lang$nav$dialogImport$badScenName)
                                                         ),
                                                         textInput("local_newScenName", 
                                                                   lang$nav$dialogImport$newScenName)
                                                       )
                                                     }
                                              )
                                            ),
                                            fluidRow(
                                              div(class= "choose-input", 
                                                  column(6,
                                                         tags$label(class = "checkbox-material flex-design", 
                                                                    'for'= "cbSelectManuallyLoc", 
                                                                    checkboxInput("cbSelectManuallyLoc", "", F), 
                                                                    lang$nav$dialogImport$cbSelectManually)
                                                  ),
                                                  column(6,
                                                         conditionalPanel(
                                                           condition = "input.cbSelectManuallyLoc == true",
                                                           selectInput("selInputDataLoc", lang$nav$dialogImport$selInputData, 
                                                                       setNames(as.list(names(modelIn.to.import)), 
                                                                                modelIn.to.import.alias), 
                                                                       multiple = TRUE, width = "100%")
                                                         )
                                                  )
                                              )
                                            ),
                                            fluidRow(
                                              tags$div(style = "text-align: center;",
                                                       shinyjs::disabled(
                                                         actionButton("btCheckSnameLocal", 
                                                                      lang$nav$dialogImport$okButton, 
                                                                      class = "btOrange")
                                                       )
                                              )
                                            )
                                   ),
                                   if(config$activateModules$scenario){
                                     hidden(
                                       tags$div(id = "loadLocal_scenNameExists",
                                                fluidRow(
                                                  tags$div(class = "errMsg",
                                                           lang$nav$dialogImport$scenNameExists
                                                  )
                                                ),
                                                fluidRow(
                                                  tags$div(style = "text-align: center;",
                                                           actionButton("btOverrideLocal", 
                                                                        lang$nav$dialogImport$overrideButton),
                                                           actionButton("btNewNameLocal", 
                                                                        lang$nav$dialogImport$newNameButton, 
                                                                        class = "btOrange")
                                                  )
                                                )
                                       )
                                     )},
                                   icon = icon("file"))
  
  # upload data from db tab
  if(config$activateModules$scenario){
    tabLoadFromDb <- tabPanel(lang$nav$dialogImport$tabDatabase, value = "tb_importData_remote",
                              fluidRow(
                                column(12,
                                       if(is.null(nrow(scenMetadata)) || !nrow(scenMetadata)){
                                         lang$nav$dialogLoadScen$descNoScen
                                       }else{
                                         list(
                                           tags$div(class = "space"),
                                           selectInput("selLoadScen", lang$nav$dialogLoadScen$selLoadScen, 
                                                       db$formatScenList(scenMetadata, stime.identifier, desc = TRUE), 
                                                       multiple = F, width = "100%"),
                                           tags$div(
                                             lang$nav$dialogLoadScen$sortBy,
                                             actionButton("btSortName", label = lang$nav$dialogLoadScen$btSortNameASC, 
                                                          icon = icon("sort-by-alphabet", lib = "glyphicon"), 
                                                          class = "scen-sort-by"), 
                                             actionButton("btSortTime", label = lang$nav$dialogLoadScen$btSortTimeASC, 
                                                          icon = icon("sort-by-order", lib = "glyphicon"), 
                                                          class = "scen-sort-by")
                                           ),
                                           fluidRow(
                                             div(class= "choose-input", 
                                                 column(6,
                                                        tags$label(class = "checkbox-material", 'for'= "cbSelectManually", 
                                                                   checkboxInput("cbSelectManually", "", F), 
                                                                   lang$nav$dialogImport$cbSelectManually)
                                                 ),
                                                 column(6,
                                                        conditionalPanel(
                                                          condition = "input.cbSelectManually == true",
                                                          selectInput("selInputData", lang$nav$dialogImport$selInputData, 
                                                                      setNames(as.list(names(modelIn.to.import)), 
                                                                               modelIn.to.import.alias), 
                                                                      multiple = TRUE, width = "100%")
                                                        )
                                                 )
                                             )
                                           ),
                                           tags$div(class = "small-space"),
                                           tags$div(style = "text-align: center;",
                                                    actionButton("btLoadScenConfirm", 
                                                                 lang$nav$dialogLoadScen$okButton, 
                                                                 class = "btOrange")
                                           )
                                           
                                         )
                                       }
                                )
                              ),
                              icon = icon("database")
    )
  }
  showModal(modalDialog(
    title = lang$nav$dialogImport$title,
    fluidRow(
      if(config$activateModules$scenario){
        tabBox(width = 12, id = "tb_importData", tabLoadFromDb, tabLoadFromLocalFile)
      }else{
        tabBox(width = 12, id = "tb_importData", tabLoadFromLocalFile)
      }
    )
  ))
  if(config$activateModules$scenario){
    if(identical(nrow(scenMetadata), 0L)){
      # no scenarios in database, so select local tab
      updateTabsetPanel(session, "tb_importData", selected = "tb_importData_local")
    }
    shinyjs::addClass("btSortTime", class = "scen-sort-by-selected")
  }
})
observeEvent(input$localInput$name, {
  flog.debug("A new input file with name: '%s' was uploaded.", 
             isolate(input$localInput$name))
  
  if(!is.null(isolate(input$localInput$name))){
    if(is.null(isolate(rv$active.sname))){
      updateTextInput(session, "local_newScenName", value = gsub("\\.[^\\.]+$", "", 
                                                                 isolate(input$localInput$name)))
    }
    shinyjs::enable("btCheckSnameLocal")
  }
})
observeEvent(input$btNewNameLocal, {
  flog.debug("Button to choose a different scenario name clicked.")
  shinyjs::hide("loadLocal_scenNameExists")
  shinyjs::show("loadLocal_content")
})
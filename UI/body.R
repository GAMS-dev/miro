## UI body

body <- dashboardBody(
  useShinyjs(),
  extendShinyjs("./JS/shinyjs.js"),
  tags$head(
    tags$link(type = "text/css", rel = "stylesheet", href = "gmswebui.css"),
    tags$script(src="shortcuts.js"),
    # css sheets that depend on data from config JSON file
    tags$style(HTML(paste0('
.main-header .logo {
  background-image: url("', config$UILogo, '");
}
.pvtRows, .pvtCols { 
  background-color: ', config$pivottable$bgColor, '; 
}')))),
  tabItems(
    tabItem(tabName = "inputData",
            fluidRow(
              box(title = list(
                shinyjs::hidden(tags$div(id = "dirtyFlagIcon", style = "display:inline;", icon("exclamation-triangle"))),
                textOutput("inputDataTitle", inline = T),
                tags$div(style = "float: right;", actionButton(inputId = "btRemove", class = "btClose", icon = icon("times"), label = ""))
              ), status="primary", solidHeader = TRUE, width = 12,
              do.call(tabsetPanel, c(id = "input.tabset", lapply(seq_along(inputTabs), function(tabId) {
                i <- inputTabs[[tabId]][1]
                tabPanel(
                  title=inputTabTitles[tabId],
                  value = paste0("input.tabset_", tabId),
                  tags$div(class="small-space"),
                  tags$div(class = "in-data-header",
                           tags$div(class = "in-buttons-wrapper",
                                    if(length(inputTabs[[tabId]]) == 1){
                                      if(!is.null(config.graphs.in[[i]])){
                                        tags$div(title = lang$nav$scen$tooltips$btGraphView, class = "scen-button-tt",
                                                 shinyjs::disabled(
                                                   actionButton(inputId = "btGraphIn" %+% i, icon = icon("bar-chart"), label = "",
                                                                class="scen-button")
                                                 )
                                        )
                                      }
                                    }
                           )
                  ),
                  tags$div(class="small-space"),
                  lapply(inputTabs[[tabId]], function(i){
                    has.dependency <- !is.null(modelIn.with.dep[[names(modelIn)[[i]]]])
                    switch(modelIn[[i]]$type,
                           hot = {
                             list(
                               tags$div(id = paste0("data-in_", i), {
                                 rhandsontable::rHandsontableOutput(paste0("in_", i))
                               }),
                               shinyjs::hidden(
                                 tags$div(id = paste0("graph-in_", i), class = "render-output", style = if(!is.null(config.graphs.in[[i]]$height)) sprintf("min-height: %s;", add.css.dim(config.graphs.in[[i]]$height, 5)),
                                          # loading animation
                                          if(config.graphs.in[[i]]$outType == "dtGraph" && config.graphs.in[[i]]$graph$tool == "plotly"){
                                            tags$img(src = "load.gif", class = "loading-input-r")
                                          }else if (config.graphs.in[[i]]$outType == "graph" && config.graphs.in[[i]]$graph$tool == "plotly"){
                                            tags$img(src = "load.gif", class = "loading-input")
                                          },
                                          tryCatch({
                                            renderDataUI(paste0("in_", i), type = config.graphs.in[[i]]$outType, 
                                                         graph.tool = config.graphs.in[[i]]$graph$tool, custom.options = config.graphs.in[[i]]$options,
                                                         height = config.graphs.in[[i]]$height, no.data.txt = lang$nav$outputScreen$boxResults$noData)
                                          }, error = function(e) {
                                            if(debug.mode){
                                              errMsg <- paste(sprintf(lang$errMsg$renderGraph$desc, modelIn.alias[i]), e, sep = "\n")
                                            }else{
                                              errMsg <- sprintf(lang$errMsg$renderGraph$desc, modelIn.alias[i])
                                            }
                                            showModal(modalDialog(
                                              title = lang$errMsg$renderGraph$title, HTML(addHtmlLineBreaks(errMsg))
                                            ))
                                          })
                                 )
                               ))
                           },
                           slider = {
                             if(has.dependency){
                               tagList(
                                 shinyjs::hidden(
                                   shiny::sliderInput(paste0("slider_", i), label = modelIn[[i]]$slider$label, min = NULL, max = NULL, 
                                                      value = if(length(modelIn[[i]]$slider$default) > 1) numeric(2L) else numeric(1L), step = 1, 
                                                      width = modelIn[[i]]$slider$width, ticks = if(is.null(modelIn[[i]]$slider$ticks)) TRUE else FALSE)
                                 ),
                                 tags$div(id = paste0("no_data_dep_", i), class = "in-no-data-dep",  lang$nav$inputScreen$noDataDep)
                               )
                             }else{
                               slider.name <- tolower(names(modelIn)[[i]])
                               shiny::sliderInput(paste0("slider_", i), label = modelIn[[i]]$slider$label, min = slider.values[[slider.name]]$min, max = slider.values[[slider.name]]$max, 
                                                  value = slider.values[[slider.name]]$def, step = slider.values[[slider.name]]$step, width = modelIn[[i]]$slider$width, 
                                                  ticks = if(is.null(modelIn[[i]]$slider$ticks)) TRUE else FALSE)
                             }
                           },
                           dropdown = {
                             if(has.dependency){
                               tagList(
                                 shinyjs::hidden(
                                   shiny::selectInput(paste0("dropdown_", i), label = modelIn[[i]]$dropdown$label, choices = character(0), selected = character(0),
                                                      multiple = if(identical(modelIn[[i]]$dropdown$multiple, TRUE)) TRUE else FALSE)
                                 ),
                                 tags$div(id = paste0("no_data_dep_", i), class = "in-no-data-dep", lang$nav$inputScreen$noDataDep)
                               )
                             }else{
                               choices <- modelIn[[i]]$dropdown$choices
                               # set aliases in case they are defined
                               if(!is.null(modelIn[[i]]$dropdown$aliases)){
                                 names(choices) <- modelIn[[i]]$dropdown$aliases
                               }
                               shiny::selectInput(paste0("dropdown_", i), label = modelIn[[i]]$dropdown$label, choices = choices, 
                                                  selected = modelIn[[i]]$dropdown$selected, multiple = if(identical(modelIn[[i]]$dropdown$multiple, TRUE)) TRUE else FALSE)
                             }
                           },
                           dropdowne = {
                             shiny::selectInput(paste0("dropdowne_", i), label = modelIn[[i]]$dropdowne$label, choices = character(0), selected = character(0),
                                                multiple = if(identical(modelIn[[i]]$dropdowne$multiple, TRUE)) TRUE else FALSE)
                           },
                           daterange = {
                             shiny::dateRangeInput(paste0("daterange_", i), label = modelIn[[i]]$daterange$label, start = modelIn[[i]]$daterange$start, 
                                                   end = modelIn[[i]]$daterange$end, min = modelIn[[i]]$daterange$min, max = modelIn[[i]]$daterange$max, 
                                                   format = modelIn[[i]]$daterange$format, startview = modelIn[[i]]$daterange$startview, 
                                                   weekstart = modelIn[[i]]$daterange$weekstart, language = config$language, 
                                                   separator = if(identical(modelIn[[i]]$daterange$separator, NULL)) " to " else modelIn[[i]]$daterange$separator, 
                                                   width = modelIn[[i]]$daterange$width, autoclose = if(identical(modelIn[[i]]$daterange$autoclose, F)) F else T)
                           },
                           date = {
                             shiny::dateInput(paste0("date_", i), label = modelIn[[i]]$date$label, value = modelIn[[i]]$date$value, min = modelIn[[i]]$date$min, 
                                              max = modelIn[[i]]$date$max, format = modelIn[[i]]$date$format, startview = modelIn[[i]]$date$startview, 
                                              weekstart = modelIn[[i]]$date$weekstart, language = config$language, width = modelIn[[i]]$date$width)
                           },
                           checkbox = {
                             if(has.dependency){
                               tagList(
                                 shinyjs::hidden(
                                   tags$div(id = paste0("cbDiv_", i),
                                            tags$label(class = "cb-label", 'for'= paste0("cb_", i), modelIn[[i]]$checkbox$label), 
                                            tags$div(
                                              tags$label(class = modelIn[[i]]$checkbox$class, 'for'= paste0("cb_", i), 
                                                         shiny::checkboxInput(paste0("cb_", i), label = "", value = NULL, 
                                                                              width = modelIn[[i]]$checkbox$width))
                                            )
                                   )
                                 ),
                                 tags$div(id = paste0("no_data_dep_", i), class = "in-no-data-dep",  lang$nav$inputScreen$noDataDep)
                               )
                             }else{
                               tagList(
                                 tags$label(class = "cb-label", 'for'= paste0("cb_", i), modelIn[[i]]$checkbox$label), 
                                 tags$div(
                                   tags$label(class = modelIn[[i]]$checkbox$class, 'for'= paste0("cb_", i), 
                                              shiny::checkboxInput(paste0("cb_", i), label = "", value = modelIn[[i]]$checkbox$value, 
                                                                   width = modelIn[[i]]$checkbox$width))
                                 )
                               )
                             }
                           }
                    )
                  }),
                  tags$div(class="small-space")
                )
              })))
              )
            )
    ),
    tabItem(tabName="gamsinter",
            fluidRow(
              box(title=lang$nav$gams$boxModelStatus$title, status="warning", solidHeader = TRUE, width=12,
                  textOutput("modelStatus"))
            ),
            fluidRow(
              box(title=lang$nav$gams$boxGamsOutput$title, status="warning", solidHeader = TRUE, width=12, collapsible = TRUE,
                  tabsetPanel(
                    tabPanel(title=lang$nav$gams$boxGamsOutput$gamsOutputTabset$logFile,
                             verbatimTextOutput("logStatus")),
                    tabPanel(title = lang$nav$gams$boxGamsOutput$gamsOutputTabset$lstFile,
                             verbatimTextOutput("listFile"))
                  ),
                  checkboxInput("logUpdate", label = lang$nav$gams$boxGamsOutput$gamsOutputTabset$logUpdate, value = T)
              )
            )
    ),
    tabItem(tabName = "outputData",
            #tabsetPanel(id="scenTabset",
            #tabPanel(title=lang$nav$outputScreen$tabCurrent, value = "results.current",
            fluidRow(
              box(title = list(
                shinyjs::hidden(tags$div(id = "dirtyFlagIconO", style = "display:inline;", icon("exclamation-triangle"))),
                textOutput("outputDataTitle", inline = T),
                tags$div(style = "float: right;", actionButton(inputId = "btRemoveO", class = "btClose", icon = icon("times"), label = ""))
              ), status="primary", solidHeader = TRUE, width = 12,
              tags$div(class="scen-header",
                       tags$div(class = "out-buttons-wrapper",
                                #shinyjs::disabled(
                                actionButton("outputTableView", icon("table"), 
                                             class="scen-button"),
                                downloadButton(outputId = "export_1", label = "",
                                               class="scen-button")
                                #, actionButton("save_0", icon("save"), 
                                #             class="scen-button"),
                                #actionButton("remove_0", icon("times"), 
                                #             class="scen-button")
                                #)
                       )
              ),
              do.call(tabsetPanel, c(id = "content.current", lapply(modelOut.to.display, function(sheet.name) {
                i <- match(sheet.name, names(modelOut))[1]
                tabPanel(
                  title = modelOut.alias[i],
                  value = paste0("content.current_", i),
                  tags$div(class="space"),
                  tags$div(id = paste0("graph-out_", i), class = "render-output", style = if(!is.null(config.graphs.out[[i]]$height)) sprintf("min-height: %s;", add.css.dim(config.graphs.out[[i]]$height, 5)),
                           renderDataUI(paste0("tab_",i), type = config.graphs.out[[i]]$outType, 
                                        graph.tool = config.graphs.out[[i]]$graph$tool, custom.options = config.graphs.out[[i]]$options,
                                        height = config.graphs.out[[i]]$height, no.data.txt = lang$nav$outputScreen$boxResults$noData)
                  ),
                  shinyjs::hidden(
                    tags$div(id = paste0("data-out_", i), class = "render-output",{
                      tryCatch({
                        renderDataUI(paste0("table-out_",i), type = "datatable", 
                                     no.data.txt = lang$nav$outputScreen$boxResults$noData)
                      }, error = function(e) {
                        if(debug.mode){
                          eMsg <<- paste(eMsg, paste(sprintf(lang$errMsg$renderTable$desc, name), e, sep = "\n"), sep = "\n")
                        }else{
                          eMsg <<- paste(eMsg, sprintf(lang$errMsg$renderTable$desc, name), sep = "\n")
                        }
                      })
                    })
                  ),
                  tags$div(class="space"))
              })))
              #)
              )
            )
    ),
    tabItem(tabName = "scenarios",
            shinyjs::hidden(
              tags$div(id = "scenSingleView",
                       tabsetPanel(id="scenTabset"),
                       tags$div(id = "noScen", lang$nav$scen$noScen)
              )
            ),
            fluidRow(
              tags$div(id = "scenSplitView",
                       box(width = 6, solidHeader = TRUE, status="primary", title = 
                             tagList(textOutput("title_2", inline = T), 
                                     tags$div(style = "float: right;", actionButton(inputId = "btScenSplit1_close", class = "btClose", icon = icon("times"), label = ""))), 
                           shinyjs::hidden(tags$div(id = "scenSplit1_content", generateScenarioTabsetSplit(2))), 
                           tags$div(id = "scenSplit1_open", actionButton("btScenSplit1_open", lang$nav$scen$split$load, class = "scenSplit-button-load"))),
                       box(width = 6, solidHeader = TRUE, status="primary", 
                           title = tagList(textOutput("title_3", inline = T), 
                                           tags$div(style = "float: right;", actionButton(inputId = "btScenSplit2_close", class = "btClose", icon = icon("times"), label = ""))),
                           shinyjs::hidden(tags$div(id = "scenSplit2_content", generateScenarioTabsetSplit(3))), 
                           tags$div(id = "scenSplit2_open", actionButton("btScenSplit2_open", lang$nav$scen$split$load, class = "scenSplit-button-load")))
              )
            )
    ),
    tabItem(tabName = "advanced",
            column(width = 4,
                   box(width = NULL, solidHeader = TRUE, status="primary", title = lang$nav$advanced$titleDownloadTemp, 
                       tags$hr(),
                       tags$span(style = "margin-right:15px;", lang$nav$advanced$downloadTempDesc),
                       actionButton("btDownloadTmpFiles", label = lang$nav$advanced$downloadTempButton),
                       tags$hr()
                   )
            )
    )
  )
)
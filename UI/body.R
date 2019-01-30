## UI body
getHypercubeJobsTableSkeleton <- function(id = NULL, content = NULL){
  tags$div(style = "max-height: 70vh;overflow:auto;margin-bottom:20px",
           tags$div(class = "gmsalert gmsalert-success", id = "fetchJobsDiscarded", 
                    lang$nav$hcubeMode$importJobsDialog$discardSuccess),
           tags$div(class = "gmsalert gmsalert-success", id = "fetchJobsImported", 
                    lang$nav$hcubeMode$importJobsDialog$importSuccess),
           tags$div(class = "gmsalert gmsalert-error", id = "fetchJobsError", 
                    lang$errMsg$unknownError),
           if(is.null(id)){
             content
           }else{
             uiOutput(id)
           }
  )
}
tabItemList <- list(
  tabItem(tabName = "inputData",
          fluidRow(
            box(title = list(
              tags$div(id = "dirtyFlagIcon", class = "inline-el", style = "display:none;", 
                       icon("exclamation-triangle")),
              textOutput("inputDataTitle", inline = TRUE),
              tags$div(style = "float: right;", 
                       HTML(paste0('<button type="button" class="btn btn-default bt-icon" 
                                   onclick="confirmModalShow(\'', 
                                   lang$nav$dialogRemoveScen$title, '\', \'', 
                                   lang$nav$dialogRemoveScen$desc, '\', \'', 
                                   lang$nav$dialogRemoveScen$cancelButton, '\', \'', 
                                   lang$nav$dialogRemoveScen$okButton, 
                                   '\', \'Shiny.setInputValue(\\\'btRemoveConfirm\\\', 1, {priority: \\\'event\\\'})\')">
                            <i class="fa fa-times"></i></button>'))
                       )
            ), status="primary", solidHeader = TRUE, width = 12L,
            do.call(tabsetPanel, c(id = "inputTabset", lapply(seq_along(inputTabs), function(tabId) {
              i <- inputTabs[[tabId]][1]
              tabPanel(
                title=inputTabTitles[tabId],
                value = paste0("inputTabset_", tabId),
                tags$div(class="small-space"),
                tags$div(class = "in-data-header",
                         tags$div(class = "in-buttons-wrapper",
                                  if(length(inputTabs[[tabId]]) == 1){
                                    if(!is.null(configGraphsIn[[i]])){
                                      tags$div(title = lang$nav$scen$tooltips$btGraphView, class = "scen-button-tt",
                                               tagAppendAttributes(
                                                 actionButton(inputId = "btGraphIn" %+% i, icon = icon("bar-chart"), label = NULL,
                                                              class="scen-button"), disabled = ""
                                               )
                                      )
                                    }
                                  }
                         )
                ),
                tags$div(class="small-space"),
                lapply(inputTabs[[tabId]], function(i){
                  hasDependency <- !is.null(modelInWithDep[[names(modelIn)[[i]]]])
                  switch(modelIn[[i]]$type,
                         hot = ,
                         dt = {
                           tagList(
                             tags$div(id = paste0("data-in_", i), {
                               if(modelIn[[i]]$type == "hot"){
                                 rHandsontableOutput(paste0("in_", i))
                               }else{
                                 dataTableOutput(paste0("in_", i))
                               }
                             }),
                             tags$div(id = paste0("graph-in_", i), class = "render-output", 
                                      style = paste0("padding:1px;display:none;", if(!is.null(configGraphsIn[[i]]$height)) 
                                        sprintf("min-height: %s;", addCssDim(configGraphsIn[[i]]$height, 5))),
                                      tryCatch({
                                        renderDataUI(paste0("in_", i), type = configGraphsIn[[i]]$outType, 
                                                     graphTool = configGraphsIn[[i]]$graph$tool, 
                                                     customOptions = configGraphsIn[[i]]$options,
                                                     height = configGraphsIn[[i]]$height, 
                                                     noDataTxt = lang$nav$outputScreen$boxResults$noData)
                                      }, error = function(e) {
                                        flog.error(paste0(sprintf(lang$errMsg$renderGraph$desc, modelInAlias[i]), e))
                                        errMsg <- sprintf(lang$errMsg$renderGraph$desc, modelInAlias[i])
                                        showErrorMsg(lang$errMsg$renderGraph$title, errMsg)
                                      })
                             ))
                         },
                         slider = {
                           if(hasDependency){
                             sliderStepSize <- 1L
                             slider         <- sliderInput(paste0("slider_", i),
                                                           label = modelIn[[i]]$slider$label, min = NULL, max = NULL, 
                                                           value = if(length(modelIn[[i]]$slider$default) > 1) 
                                                             numeric(2L) else numeric(1L), step = sliderStepSize, 
                                                           width = modelIn[[i]]$slider$width, 
                                                           ticks = if(is.null(modelIn[[i]]$slider$ticks)) TRUE else FALSE)
                             slider         <- tagList(tagAppendAttributes(slider, style = "display:none;"), 
                                                       tags$div(id = paste0("no_data_dep_", i), class = "in-no-data-dep",  
                                                                lang$nav$inputScreen$noDataDep))
                           }else{
                             sliderName     <- tolower(names(modelIn)[[i]])
                             sliderStepSize <- sliderValues[[sliderName]]$step
                             slider         <- sliderInput(paste0("slider_", i), 
                                                           label = modelIn[[i]]$slider$label, 
                                                           min = sliderValues[[sliderName]]$min, 
                                                           max = sliderValues[[sliderName]]$max, 
                                                           value = sliderValues[[sliderName]]$def, 
                                                           step = sliderValues[[sliderName]]$step, 
                                                           width = modelIn[[i]]$slider$width, 
                                                           ticks = if(is.null(modelIn[[i]]$slider$ticks)) TRUE else FALSE)
                           }
                           if(config$activateModules$hcubeMode){
                             if(identical(modelIn[[i]]$slider$double, TRUE)){
                               tagList(
                                 column(width = 8, style = "padding-left:0px;",
                                        slider
                                 ),
                                 column(width = 2, style = "min-width: 130px; min-height:100px;",
                                        tagList(
                                          tags$label(class = "cb-label", "for" = "hcubeMode_" %+% i, 
                                                     lang$nav$hcubeMode$sliderAllCombinations), 
                                          tags$div(
                                            tags$label(class = "checkbox-material", "for" = "hcubeMode_" %+% i, 
                                                       checkboxInput("hcubeMode_" %+% i, label = NULL, 
                                                                     value = modelIn[[i]]$checkbox$value))
                                          )
                                        )
                                 ),
                                 conditionalPanel("input.hcubeMode_" %+% i, 
                                                  column(width = 1, style = "min-width: 100px; min-height:100px;",
                                                         numericInput("hcubeStep_" %+% i, "Step size", 
                                                                      sliderStepSize, min = 0)
                                                  )
                                 )
                                 
                               )
                             }else if(identical(modelIn[[i]]$slider$single, TRUE)){
                               tagList(
                                 column(width = 10, style = "padding-left:0px;",
                                        slider
                                 ),
                                 column(width = 1, style = "min-width: 100px; min-height:100px;",
                                        numericInput("hcubeStep_" %+% i, "Step size", 
                                                     sliderStepSize, min = 0)
                                 )
                               )
                             }
                           }else{
                             slider
                           }
                         },
                         dropdown = {
                           if(hasDependency){
                             tagList(
                               tagAppendAttributes(selectInput(paste0("dropdown_", i),
                                                               label = modelIn[[i]]$dropdown$label, 
                                                               choices = character(0), selected = character(0),
                                                               multiple = if(identical(modelIn[[i]]$dropdown$multiple, 
                                                                                       TRUE)) TRUE else FALSE), style = "display:none;"),
                               tags$div(id = paste0("no_data_dep_", i), class = "in-no-data-dep", 
                                        style = "float: left;width: 100%;margin: 20px;", 
                                        lang$nav$inputScreen$noDataDep)
                             )
                           }else{
                             choices <- modelIn[[i]]$dropdown$choices
                             
                             if(!is.null(modelIn[[i]]$dropdown$aliases)){
                               names(choices) <- modelIn[[i]]$dropdown$aliases
                             }
                             selectInput(paste0("dropdown_", i), 
                                         label = modelIn[[i]]$dropdown$label, 
                                         choices = choices, 
                                         selected = modelIn[[i]]$dropdown$selected, 
                                         multiple = if(identical(modelIn[[i]]$dropdown$multiple, 
                                                                 TRUE)) TRUE else FALSE)
                           }
                         },
                         dropdowne = {
                           selectInput(paste0("dropdowne_", i), label = modelIn[[i]]$dropdowne$label, 
                                       choices = character(0), selected = character(0),
                                       multiple = if(identical(modelIn[[i]]$dropdowne$multiple, 
                                                               TRUE)) TRUE else FALSE)
                         },
                         daterange = {
                           dateRangeInput(paste0("daterange_", i), 
                                          label = modelIn[[i]]$daterange$label, 
                                          start = modelIn[[i]]$daterange$start, 
                                          end = modelIn[[i]]$daterange$end, 
                                          min = modelIn[[i]]$daterange$min, 
                                          max = modelIn[[i]]$daterange$max, 
                                          format = modelIn[[i]]$daterange$format, 
                                          startview = modelIn[[i]]$daterange$startview, 
                                          weekstart = modelIn[[i]]$daterange$weekstart, 
                                          language = config$language, 
                                          separator = if(identical(modelIn[[i]]$daterange$separator, 
                                                                   NULL)) " to " else modelIn[[i]]$daterange$separator, 
                                          width = modelIn[[i]]$daterange$width, 
                                          autoclose = if(identical(modelIn[[i]]$daterange$autoclose, 
                                                                   FALSE)) FALSE else TRUE)
                         },
                         date = {
                           dateInput(paste0("date_", i), label = modelIn[[i]]$date$label, 
                                     value = modelIn[[i]]$date$value, min = modelIn[[i]]$date$min, 
                                     max = modelIn[[i]]$date$max, format = modelIn[[i]]$date$format, 
                                     startview = modelIn[[i]]$date$startview, 
                                     weekstart = modelIn[[i]]$date$weekstart, language = config$language, 
                                     width = modelIn[[i]]$date$width)
                         },
                         checkbox = {
                           if(hasDependency){
                             tagList(
                               tags$div(id = paste0("cbDiv_", i), style = "display:none;",
                                        tags$label(class = "cb-label", "for" = paste0("cb_", i), 
                                                   modelIn[[i]]$checkbox$label), 
                                        tags$div(
                                          tags$label(class = modelIn[[i]]$checkbox$class, 
                                                     "for" = paste0("cb_", i), 
                                                     checkboxInput(paste0("cb_", i), 
                                                                   label = NULL, value = NULL, 
                                                                   width = modelIn[[i]]$checkbox$width))
                                        )
                               ),
                               tags$div(id = paste0("no_data_dep_", i), class = "in-no-data-dep",  
                                        lang$nav$inputScreen$noDataDep)
                             )
                           }else{
                             tagList(
                               tags$label(class = "cb-label", "for" = paste0("cb_", i), modelIn[[i]]$checkbox$label), 
                               tags$div(
                                 tags$label(class = modelIn[[i]]$checkbox$class, "for" = paste0("cb_", i), 
                                            checkboxInput(paste0("cb_", i), label = NULL, 
                                                          value = modelIn[[i]]$checkbox$value, 
                                                          width = modelIn[[i]]$checkbox$width))
                               )
                             )
                           }
                         },
                         textinput = {
                           textInput(paste0("text_", i), label = modelIn[[i]]$textinput$label, 
                                     value = modelIn[[i]]$textinput$value,
                                     width = modelIn[[i]]$textinput$width,
                                     placeholder = modelIn[[i]]$textinput$placeholder)
                         }
                  )
                }),
                tags$div(class="small-space")
              )
            })))
            )
          )
  ),
  tabItem(tabName = "scenarios",
          tags$div(id = "scen-tab-view", style = "display:none;",
                   tabsetPanel(id="scenTabset"),
                   tags$div(id = "no-scen", lang$nav$scen$noScen)
          ),
          fluidRow(
            tags$div(id = "scen-split-view",
                     box(width = 6, solidHeader = TRUE, status="primary", title = 
                           tagList(textOutput("title_2", inline = T), 
                                   tags$div(style = "float: right;", 
                                            actionButton(inputId = "btScenSplit1_close", 
                                                         class = "bt-icon",
                                                         icon = icon("times"), 
                                                         label = NULL))), 
                         tags$div(id = "scenSplit1_content", style = "display:none;", 
                                  generateScenarioTabsetSplit(2)), 
                         tags$div(id = "scenSplit1_open", 
                                  actionButton("btScenSplit1_open", lang$nav$scen$split$load, 
                                               class = "scenSplit-button-load"))),
                     box(width = 6, solidHeader = TRUE, status="primary", 
                         title = tagList(textOutput("title_3", inline = T), 
                                         tags$div(style = "float: right;", 
                                                  actionButton(inputId = "btScenSplit2_close", 
                                                               class = "bt-icon", icon = icon("times"), label = NULL))),
                         tags$div(id = "scenSplit2_content", style = "display:none;", generateScenarioTabsetSplit(3)), 
                         tags$div(id = "scenSplit2_open", 
                                  actionButton("btScenSplit2_open", lang$nav$scen$split$load, 
                                               class = "scenSplit-button-load")))
            )
          )
  )
)
if(config$activateModules$hcubeMode){
  tabItemList <- c(tabItemList, list(
    tabItem(tabName = "loadResults",
            fluidRow(
              box(title = lang$nav$hcubeLoad$title, status="primary", 
                  solidHeader = TRUE, width = 12, style="overflow-x: auto",
                  tags$div(id = "loadContent",
                           tags$div(id = "selectorsWrapper"
                           ),
                           tags$div(id = "buttonsWrapper", class = "item-or-query",
                                    actionButton("btNewBlock", label = "OR")),
                           tags$div(class = "item-or-query",
                                    actionButton("btSendQuery", label = "Query database", 
                                                 class = "bt-highlight-1")
                           )
                  ),
                  genSpinner(id = "hyperQueryLoad", hidden = TRUE, absolute = FALSE),
                  tags$div(style = "min-height: 80px;", dataTableOutput("hcubeLoadResults")),
                  tags$div(id = "hcubeLoadNoData", 
                           style = "text-align:center;font-size:16px;font-weight:bold;margin:20px;display:none;",
                           lang$nav$hcubeLoad$noData),
                  tags$div(id = "hcubeLoadButtons", style = "display:none;padding:30px 0 50px 0;",
                    tags$div(class = "col-sm-6",
                             tags$div(
                                      actionButton("hcubeLoadSelected", lang$nav$hcubeLoad$chooseSelectedButton , 
                                                   class = "bt-highlight-1"),
                                      actionButton("hcubeLoadCurrent", lang$nav$hcubeLoad$chooseCurrentButton , 
                                                   class = "bt-highlight-1"),
                                      actionButton("hcubeLoadAll", lang$nav$hcubeLoad$chooseAllButton, 
                                                   class = "bt-highlight-1")
                             )
                    ),
                    tags$div(class = "col-sm-6", style = "text-align:right;",
                             tags$div(id = "showHashOnlyOne", class = "gmsalert gmsalert-error", style = "bottom:10%;",
                                      lang$nav$hcubeLoad$msgOnlyOneHash),
                             tags$div(id = "showHashError", class = "gmsalert gmsalert-error", style = "bottom:10%;",
                                      lang$errMsg$unknownError),
                             actionButton("btShowHash", 
                                          lang$nav$hcubeLoad$showHashButton)
                    )
                  )
              )
            )
    ),
    tabItem(tabName = "importData",
            fluidRow(
              box(title = tagList(lang$nav$hcubeImport$title, 
                                  tags$div(style = "float: right;", 
                                           actionButton(inputId = "refreshActiveJobs", 
                                                        class = "bt-icon", 
                                                        icon = icon("refresh"), label = NULL))),
                  status="primary", solidHeader = TRUE, width = 12,
                  genSpinner("jImport_load", absolute = FALSE),
                  getHypercubeJobsTableSkeleton(id = "jImport_output"),
                  tags$div(class = "col-sm-6",
                           actionButton("btShowHistory", 
                                        lang$nav$hcubeImport$btShowHistory)
                  ),
                  tags$div(class = "col-sm-6", style = "text-align:right;",
                           actionButton("btManualImport", 
                                        lang$nav$hcubeImport$btManualImport)
                  )
              )
            )
    ),
    tabItem(tabName = "hcubeAnalyze",
            box(width = NULL, solidHeader = TRUE, status="primary", title = lang$nav$hcubeAnalyze$title, 
                tabsetPanel(id = "tabs_paver_results",
                            tabPanel("Index", value = "index",
                                     tags$div(style = "overflow: auto; height: 75vh;",
                                              tags$div(id = "paverLoad", class = "centered-div",
                                                       style = "display:none;",
                                                       lang$nav$hcubeAnalyze$loadMsg,
                                                       genSpinner(hidden = TRUE),
                                                       actionButton("btPaverInterrupt", lang$nav$hcubeAnalyze$btCancel)
                                              ),
                                              tags$div(id = "paverFail", class = "gmsalert gmsalert-error", 
                                                       lang$nav$hcubeAnalyze$failMsg
                                              ),
                                              tags$div(id = "newPaverRunButton", class = "centered-div",
                                                       actionButton("btNewPaverRun", lang$nav$hcubeAnalyze$btNew)
                                              ),
                                              htmlOutput("paverResults")
                                     )
                            ))
            )
    )
  ))
}else{
  tabItemList <- c(tabItemList, list(
    tabItem(tabName="gamsinter",
            fluidRow(
              box(title=lang$nav$gams$boxModelStatus$title, status="warning", solidHeader = TRUE, width=12,
                  textOutput("modelStatus"))
            ),
            if(any(config$activateModules$logFile, config$activateModules$lstFile)){
              if(config$activateModules$logFile && config$activateModules$lstFile){
                logTabset <- tabsetPanel(
                  tabPanel(title=lang$nav$gams$boxGamsOutput$gamsOutputTabset$logFile,
                           verbatimTextOutput("logStatus")),
                  tabPanel(title = lang$nav$gams$boxGamsOutput$gamsOutputTabset$lstFile,
                           verbatimTextOutput("listFile"))
                )
              }else if(config$activateModules$lstFile){
                logTabset <- tabsetPanel(
                  tabPanel(title = lang$nav$gams$boxGamsOutput$gamsOutputTabset$lstFile,
                           verbatimTextOutput("listFile"))
                )
              }else{
                logTabset <- tabsetPanel(
                  tabPanel(title=lang$nav$gams$boxGamsOutput$gamsOutputTabset$logFile,
                           verbatimTextOutput("logStatus"))
                )
              }
              fluidRow(
                box(title=lang$nav$gams$boxGamsOutput$title, status="warning", solidHeader = TRUE, 
                    width=12, collapsible = TRUE,
                    logTabset,
                    checkboxInput("logUpdate", label = lang$nav$gams$boxGamsOutput$gamsOutputTabset$logUpdate, 
                                  value = T)
                )
              )
            }
    ),
    tabItem(tabName = "outputData",
            fluidRow(
              box(title = list(
                tags$div(id = "dirtyFlagIconO", class = "inline-el", style = "display:none;", 
                         icon("exclamation-triangle")),
                textOutput("outputDataTitle", inline = T),
                tags$div(style = "float: right;", 
                         HTML(paste0('<button type="button" class="btn btn-default bt-icon" 
                                   onclick="confirmModalShow(\'', 
                                     lang$nav$dialogRemoveScen$title, '\', \'', 
                                     lang$nav$dialogRemoveScen$desc, '\', \'', 
                                     lang$nav$dialogRemoveScen$cancelButton, '\', \'', 
                                     lang$nav$dialogRemoveScen$okButton, 
                                     '\', \'Shiny.setInputValue(\\\'btRemoveConfirm\\\', 1, {priority: \\\'event\\\'})\')">
                            <i class="fa fa-times"></i></button>'))
                         )
              ), status="primary", solidHeader = TRUE, width = 12,
              tags$div(class="scen-header",
                       tags$div(class = "out-buttons-wrapper",
                                actionButton("btDownloadTmpFiles", icon("folder-open"), 
                                             class="scen-button"),
                                actionButton("outputTableView", icon("table"), 
                                             class="scen-button")
                       )
              ),
              do.call(tabsetPanel, c(id = "contentCurrent", 
                                     lapply(modelOutToDisplay, function(sheetName) {
                                       i <- match(sheetName, names(modelOut))[1]
                                       tabPanel(
                                         title = modelOutAlias[i],
                                         value = paste0("contentCurrent_", i),
                                         tags$div(class="space"),
                                         tags$div(id = paste0("graph-out_", i), class = "render-output", 
                                                  style = if(!is.null(configGraphsOut[[i]]$height)) 
                                                    sprintf("min-height: %s;", addCssDim(configGraphsOut[[i]]$height, 5)),
                                                  renderDataUI(paste0("tab_",i), type = configGraphsOut[[i]]$outType, 
                                                               graphTool = configGraphsOut[[i]]$graph$tool, 
                                                               customOptions = configGraphsOut[[i]]$options,
                                                               height = configGraphsOut[[i]]$height, 
                                                               noDataTxt = lang$nav$outputScreen$boxResults$noData)
                                         ),
                                         tags$div(id = paste0("data-out_", i), class = "render-output", style = "display:none;",{
                                           tryCatch({
                                             renderDataUI(paste0("table-out_",i), type = "datatable", 
                                                          noDataTxt = lang$nav$outputScreen$boxResults$noData)
                                           }, error = function(e) {
                                             flog.error(paste0(sprintf(lang$errMsg$renderTable$desc, name), e))
                                             eMsg <<- paste(eMsg, sprintf(lang$errMsg$renderTable$desc, name), sep = "\n")
                                           })
                                         }),
                                         tags$div(class="space"))
                                     })))
              )
            )
    )
  ))
}
body <- dashboardBody({
  if(dir.exists(paste0(currentModelDir, "static"))){
    addResourcePath("custom", paste0(currentModelDir, "static"))
  }
  tagList(
  tags$head(
    if(config$activateModules$hcubeMode){
      tagList(
        tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.5/MathJax.js?config=TeX-MML-AM_CHTML", 
                    type = "application/javascript"),
        tags$script(type = "text/x-mathjax-config", {
          "MathJax.Hub.Config({
            skipStartupTypeset: true,
            tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']]}
          });"
        })
      )
    },
    tags$link(type = "text/css", rel = "stylesheet", href = "gmswebui.css"),
    tags$script(src = "shortcuts.js", type = "application/javascript"),
    tags$script(src = "gmswebui.js", type = "application/javascript"),
    
    # css sheets that depend on data from config JSON file
    # Logo ratio should be 4,6 (width/height)
    tags$style(HTML(paste0('
.main-header .logo {
  background-image: url("', if(dir.exists(paste0(currentModelDir, "static"))) "custom/", config$UILogo, '");
}
.pvtRows, .pvtCols { 
  background-color: ', config$pivottable$bgColor, '; 
}')))),
  if(config$activateModules$hcubeMode){
  HTML('<!-- Creates modal dialog for images generated by paver -->
<div class="modal fade" id="imagemodal" tabindex="-1" role="dialog" aria-labelledby="myModalLabel" aria-hidden="true">
  <div class="modal-dialog">
    <div class="modal-content" style="width:685px;">
      <div class="modal-header">
        <button type="button" class="close" data-dismiss="modal"><span aria-hidden="true">&times;</span><span class="sr-only">Close</span></button>
        <h4 class="modal-title" id="myModalLabel"></h4>
      </div>
      <div class="modal-body">
        <img src="" id="imagepreview" >
      </div>
      <div class="modal-footer">
        <button type="button" class="btn btn-default" data-dismiss="modal">Close</button>
      </div>
    </div>
</div>
</div>')},
  HTML(paste0('<!-- Creates modal dialog for confirm messages -->
<div class="modal fade" id="confirmModal" tabindex="-1" role="dialog" aria-labelledby="myModalLabel" aria-hidden="true">
  <div class="modal-dialog">
    <div class="modal-content" style="width:685px;">
      <div class="modal-header">
        <h4 class="modal-title"></h4>
      </div>
      <div class="modal-body">
      </div>
      <div class="modal-footer">
      </div>
    </div>
</div>
</div>
<div id="loading-screen"><div class="lds-ellipsis" style="position:relative;top:50%;left:50%">
       <div></div><div></div><div></div><div></div></div></div><div class="gmsalert gmsalert-error" id="hcubeRunning">', 
              lang$errMsg$hcubeLaunch$hcubeRunning, '</div>', '<div class="gmsalert gmsalert-error" id="hcubeLaunchError">', 
              lang$errMsg$hcubeLaunch$launchError, '</div>')),
  do.call(tabItems, tabItemList)
)})
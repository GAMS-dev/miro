## UI body
langSpecificUI <- list()
langSpecificUI$tableType <- c("input table" = "hot", "output table" = "dt")
names(langSpecificUI$tableType) <- lang$adminMode$tables$ui$choices
langSpecificUI$symbolType <- c("Symbol" = "gams", "New GAMS option" = "go", 
                               "New double dash parameter" = "dd")
names(langSpecificUI$symbolType) <- lang$adminMode$widgets$ui$choices
inputTabs <- c(inputSymMultiDim, 
               setNames("_widgets", 
                        lang$nav$inputScreen$widgetTabTitle))
if(length(configJSON$overwriteSheetOrder$input)){
  tabIdsTmp <- match(configJSON$overwriteSheetOrder$input, inputTabs)
  if(any(is.na(tabIdsTmp))){
    flog.error("Invalid input symbol(s) in 'overwriteSheetOrder' found. Resetting to original sheet order.")
  }else{
    inputTabs <- inputTabs[tabIdsTmp]
  }
}
outputTabs <- setNames(names(modelOut), modelOutAlias)
if(length(configJSON$overwriteSheetOrder$output)){
  tabIdsTmp <- match(configJSON$overwriteSheetOrder$output, outputTabs)
  if(any(is.na(tabIdsTmp))){
    flog.error("Invalid output symbol(s) in 'overwriteSheetOrder' found. Resetting to original sheet order.")
  }else{
    outputTabs <- outputTabs[tabIdsTmp]
  }
}

header_admin <- dashboardHeader(
  tags$li(class = "dropdown", 
          tags$a(href="#", class="dropdown-toggle", "data-toggle" = "dropdown", 
                 lang$nav$header$help$title, tags$span(class="caret")),
          tags$ul(class = "dropdown-menu", role="menu",
                  tags$li(tags$a(href = "https://www.gams.com/miro/", 
                                 target = "_blank", lang$nav$header$help$doc)),
                  tags$li(tags$a(href = "https://forum.gamsworld.org/viewforum.php?f=14", 
                                 target = "_blank", lang$nav$header$help$forum)),
                  tags$li(HTML(paste0('<a href="#" class="action-button" onclick="Miro.confirmModalShow(\'',
                                      'About MIRO\', \'', 
                                      htmltools::htmlEscape(aboutDialogText), '\', \'Cancel\');">',
                                      lang$nav$header$help$about, '</a>')
                  )))),
  title=paste0(lang$adminMode$uiR$adminPanel," (", modelName, ")"), disable = FALSE)

sidebar_admin <- dashboardSidebar(
  sidebarMenu(id="sidebarMenuId",
              menuItem(lang$adminMode$uiR$general, tabName = "new_gen", icon = icon("cogs")),
              menuItem(lang$adminMode$uiR$table, tabName="tables_gen", icon = icon("table")),
              menuItem(lang$adminMode$uiR$widgets, tabName="new_widget", icon = icon("sliders-h")),
              menuItem(lang$adminMode$uiR$graphs, tabName = "new_graph", icon = icon("chart-bar")),
              menuItem(lang$adminMode$uiR$database, tabName="db_management", icon = icon("database"))
  )
)


body_admin <- dashboardBody({
  tagList(
    tags$head(
      tags$link(type = "text/css", rel = "stylesheet", href = paste0("skin_", config$theme, ".css")),
      tags$link(type = "text/css", rel = "stylesheet", href = "bootstrap-colorpicker.min.css"),
      tags$script(src = "autoNumeric.min.js", type = "application/javascript"),
      tags$script(src = "bootstrap-colorpicker.min.js", type = "application/javascript"),
      tags$script(src = "miro_admin.js", type = "application/javascript"),
      tags$style(HTML(paste0('
.main-header .logo {
                             background-image: url("gams_logo.png");
}')))),
    HTML('<!-- Creates modal dialog for confirm messages -->
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
       <div></div><div></div><div></div><div></div></div></div>'),
    tabItems(
      tabItem(tabName = "db_management",
              fluidRow(
                box(title = lang$adminMode$database$title, status="primary", solidHeader = TRUE, width = 12,
                    tags$div(id = "removeSuccess", class = "gmsalert gmsalert-success",
                             lang$adminMode$database$removeSuccess),
                    tags$div(id = "restoreSuccess", class = "gmsalert gmsalert-success",
                             lang$adminMode$database$restoreSuccess),
                    tags$div(id = "restoreNoData", class = "gmsalert gmsalert-error",
                             lang$adminMode$database$restoreNoData),
                    tags$div(id = "restoreInvalidData", class = "gmsalert gmsalert-error",
                             lang$adminMode$database$restoreInvalidData),
                    tags$div(id = "maxRowError", class = "gmsalert gmsalert-error",
                             lang$adminMode$database$maxRowError),
                    tags$div(id = "unknownError", class = "gmsalert gmsalert-error",
                             lang$errMsg$unknownError),
                    tags$div(class = "space"),
                    tags$label("for" = "db_backup_wrapper", lang$adminMode$database$backup),
                    tags$div(id = "db_backup_wrapper", lang$adminMode$database$backupWrapper),
                    downloadButton("dbSaveAll", label = lang$adminMode$database$dbSaveAll),
                    tags$div(class = "space"),
                    tags$hr(),
                    tags$div(class = "space"),
                    tags$label("for" = "db_restore_wrapper", lang$adminMode$database$restore),
                    tags$div(id = "db_restore_wrapper", lang$adminMode$database$restoreWrapper,
                             tags$div(style = "max-width:400px;",
                                      fileInput("dbBackupZip", label = NULL, 
                                                accept = c(".zip", "application/zip", 
                                                           "application/octet-stream", 
                                                           "application/x-zip-compressed", 
                                                           "multipart/x-zip"))),
                             actionButton("restoreDb", lang$adminMode$database$restoreDb)
                    ),
                    tags$div(class = "space"),
                    tags$hr(),
                    tags$div(class = "space"),
                    tags$label("for" = "db_remove_orphans_wrapper", lang$adminMode$database$remove),
                    tags$div(id = "db_remove_orphans_wrapper", lang$adminMode$database$removeOrphansWrapper,
                             HTML(paste0('<br><button type="button" class="btn btn-default"', 
                                         ' onclick="Miro.confirmModalShow(\'', lang$adminMode$database$removeOrphansDialogTitle, 
                                         '\', \'', lang$adminMode$database$removeOrphansDialogDesc,
                                         '\', \'', lang$adminMode$database$removeOrphansDialogCancel, '\', ',
                                         '\'', lang$adminMode$database$removeOrphansDialogConfirm, 
                                         '\', \'Shiny.setInputValue(\\\'removeDbOrphans\\\', 1, {priority: \\\'event\\\'});\')">',
                                         lang$adminMode$database$removeOrphansDialogBtn, '</button>'
                             ))
                    ),
                    tags$div(class = "space"),
                    tags$div(id = "db_remove_wrapper", lang$adminMode$database$removeWrapper,
                             HTML(paste0('<br><button type="button" class="btn btn-default"', 
                                         ' onclick="Miro.confirmModalShow(\'', lang$adminMode$database$removeDialogTitle, 
                                         '\', \'', lang$adminMode$database$removeDialogDesc,
                                         '\', \'', lang$adminMode$database$removeDialogCancel, '\', ',
                                         '\'', lang$adminMode$database$removeDialogConfirm, 
                                         '\', \'Shiny.setInputValue(\\\'removeDbTables\\\', 1, {priority: \\\'event\\\'});\')">',
                                         lang$adminMode$database$removeDialogBtn, '</button>'
                             ))
                    )
                )
              )
      ),
      tabItem(tabName = "new_graph",
              fluidRow(
                box(title = lang$adminMode$graphs$ui$title, status="primary", solidHeader = TRUE, width = 12,
                    tags$div(id = "graphUpdateSuccess", class = "gmsalert gmsalert-success", lang$adminMode$graphs$ui$graphUpdateSuccess),
                    tags$div(id = "graphValidationErr", class = "gmsalert gmsalert-error"),
                    tags$div(id = "unknownErrorGraphs", class = "gmsalert gmsalert-error",
                             lang$errMsg$unknownError),
                    tags$div(class = "col-sm-6",
                                      tags$h4(id = "previewDataInputToggle", class = "box-title", 
                                              icon("minus"), style = "cursor:pointer;font-weight:bold;", 
                                              onclick = "Miro.slideToggleEl({id: '#previewDataInputWrapper', 
                                              toggleIconDiv: '#previewDataInputToggle'})"),
                                      tags$div(id = "previewDataInputWrapper", 
                                               tabsetPanel(
                                                 tabPanel(lang$nav$dialogImport$tabDatabase,
                                                          tags$div(class = "space"),
                                                          tags$div(id = "noDbScen", lang$nav$dialogLoadScen$descNoScen),
                                                          tags$div(id = "dbScen", 
                                                                   selectInput("scenList", lang$nav$dialogLoadScen$selLoadScen, 
                                                                               c(), 
                                                                               multiple = FALSE, width = "100%"),
                                                                   actionButton("dbInput", lang$nav$dialogLoadScen$okButton),
                                                                   tags$div(class = "space"))),
                                                 tabPanel(lang$nav$dialogImport$tabLocal,
                                                          tags$div(class = "space"),
                                                          fileInput("localInput", lang$adminMode$graphs$ui$localInput,
                                                                    width = "100%",
                                                                    multiple = FALSE,
                                                                    accept = c("application/vnd.ms-excel", 
                                                                               "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", 
                                                                               ".xlsx", ".gdx"))))
                                      ),
                             tags$div(id = "preview_wrapper", style = "display:none;",
                                      tags$div(class = "two-col-wrapper",
                                               tags$div(class = "two-col-left",
                                                        selectInput("gams_symbols", lang$adminMode$graphs$ui$gamsSymbols,
                                                                    choices = NULL)),
                                               tags$div(class = "two-col-right",
                                                        selectInput("chart_tool", lang$adminMode$graphs$ui$tool, 
                                                                    setNames(c("pie", "bar", "scatter", "line", "bubble", "hist", "dygraphs", "leaflet", "timevis", "pivot", "valuebox", "custom"), 
                                                                             lang$adminMode$graphs$ui$choices)))
                                      ),
                                      tags$hr(),
                                      tags$div(class = "side-tab col-sm-3",
                                               tags$div(class = "side-tab-item"),
                                               tags$div(id = "toolCategories", class="tool-categories-wrapper",
                                                        tags$ul(class = "category-list",
                                                                #pie
                                                                tags$li(id = "categoryPie1", class = "category-btn category-btn-pie category-btn-active", `data-cat`="1",
                                                                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$main)),
                                                                tags$li(id = "categoryPie2", class = "category-btn category-btn-pie", `data-cat`="2", 
                                                                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$general)),
                                                                #bar 
                                                                tags$li(id = "categoryBar1", class = "category-btn category-btn-bar", `data-cat`="3",
                                                                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$xaxis)),
                                                                tags$li(id = "categoryBar2", class = "category-btn category-btn-bar", `data-cat`="4",
                                                                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$yaxis)),
                                                                tags$li(id = "categoryBar3", class = "category-btn category-btn-bar", `data-cat`="5",
                                                                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$filterDomain)),
                                                                tags$li(id = "categoryBar5", class = "category-btn category-btn-bar", `data-cat`="7",
                                                                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$bars)),
                                                                tags$li(id = "categoryBar4", class = "category-btn category-btn-bar", `data-cat`="6",
                                                                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$general)),
                                                                #scatter 
                                                                tags$li(id = "categoryScatter1", class = "category-btn category-btn-scatter", `data-cat`="8",
                                                                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$xaxis)),
                                                                tags$li(id = "categoryScatter2", class = "category-btn category-btn-scatter", `data-cat`="9",
                                                                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$yaxis)),
                                                                tags$li(id = "categoryScatter3", class = "category-btn category-btn-scatter", `data-cat`="10",
                                                                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$filterDomain)),
                                                                tags$li(id = "categoryScatter4", class = "category-btn category-btn-scatter", `data-cat`="11",
                                                                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$animation),
                                                                        title = lang$adminMode$graphs$animationOptions$title),
                                                                tags$li(id = "categoryScatter5", class = "category-btn category-btn-scatter", `data-cat`="12",
                                                                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$general)),
                                                                #line 
                                                                tags$li(id = "categoryLine1", class = "category-btn category-btn-line", `data-cat`="13",
                                                                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$xaxis)),
                                                                tags$li(id = "categoryLine2", class = "category-btn category-btn-line", `data-cat`="14",
                                                                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$yaxis)),
                                                                tags$li(id = "categoryLine3", class = "category-btn category-btn-line", `data-cat`="15",
                                                                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$filterDomain)),
                                                                tags$li(id = "categoryLine4", class = "category-btn category-btn-line", `data-cat`="16",
                                                                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$animation),
                                                                        title = lang$adminMode$graphs$animationOptions$title),
                                                                tags$li(id = "categoryLine5", class = "category-btn category-btn-line", `data-cat`="17",
                                                                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$general)),
                                                                #bubble 
                                                                tags$li(id = "categoryBubble1", class = "category-btn category-btn-bubble", `data-cat`="18",
                                                                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$xaxis)),
                                                                tags$li(id = "categoryBubble2", class = "category-btn category-btn-bubble", `data-cat`="19",
                                                                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$yaxis)),
                                                                tags$li(id = "categoryBubble3", class = "category-btn category-btn-bubble", `data-cat`="20",
                                                                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$filterDomain)),
                                                                tags$li(id = "categoryBubble4", class = "category-btn category-btn-bubble", `data-cat`="21",
                                                                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$animation),
                                                                        title = lang$adminMode$graphs$animationOptions$title),
                                                                tags$li(id = "categoryBubble5", class = "category-btn category-btn-bubble", `data-cat`="22",
                                                                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$general)),
                                                                #hist 
                                                                tags$li(id = "categoryHist1", class = "category-btn category-btn-hist", `data-cat`="23",
                                                                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$xaxis)),
                                                                tags$li(id = "categoryHist2", class = "category-btn category-btn-hist", `data-cat`="24",
                                                                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$histogram)),
                                                                tags$li(id = "categoryHist2", class = "category-btn category-btn-hist", `data-cat`="25",
                                                                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$filterDomain)),
                                                                tags$li(id = "categoryHist2", class = "category-btn category-btn-hist", `data-cat`="26",
                                                                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$general)),
                                                                #dygraphs 
                                                                tags$li(id = "categoryDygraphs1", class = "category-btn category-btn-dygraphs", `data-cat`="27",
                                                                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$xaxis)),
                                                                tags$li(id = "categoryDygraphs1", class = "category-btn category-btn-dygraphs", `data-cat`="28",
                                                                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$yaxis)),
                                                                tags$li(id = "categoryDygraphs1", class = "category-btn category-btn-dygraphs", `data-cat`="29",
                                                                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$filterDomain)),
                                                                tags$li(id = "categoryDygraphs1", class = "category-btn category-btn-dygraphs", `data-cat`="30",
                                                                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$event)),
                                                                tags$li(id = "categoryDygraphs1", class = "category-btn category-btn-dygraphs", `data-cat`="31",
                                                                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$limit)),
                                                                tags$li(id = "categoryDygraphs1", class = "category-btn category-btn-dygraphs", `data-cat`="32",
                                                                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$annotation)),
                                                                tags$li(id = "categoryDygraphs1", class = "category-btn category-btn-dygraphs", `data-cat`="33",
                                                                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$shading)),
                                                                tags$li(id = "categoryDygraphs1", class = "category-btn category-btn-dygraphs", `data-cat`="34",
                                                                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$rangeSelector)),
                                                                tags$li(id = "categoryDygraphs1", class = "category-btn category-btn-dygraphs", `data-cat`="35",
                                                                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$highlight)),
                                                                tags$li(id = "categoryDygraphs1", class = "category-btn category-btn-dygraphs", `data-cat`="36",
                                                                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$general)),
                                                                #leaflet 
                                                                tags$li(id = "categoryLeaflet1", class = "category-btn category-btn-leaflet", `data-cat`="37",
                                                                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$markers)),
                                                                tags$li(id = "categoryLeaflet1", class = "category-btn category-btn-leaflet", `data-cat`="38",
                                                                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$flows)),
                                                                tags$li(id = "categoryLeaflet1", class = "category-btn category-btn-leaflet", `data-cat`="39",
                                                                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$minicharts)),
                                                                tags$li(id = "categoryLeaflet1", class = "category-btn category-btn-leaflet", `data-cat`="40",
                                                                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$groupsLayers)),
                                                                tags$li(id = "categoryLeaflet1", class = "category-btn category-btn-leaflet", `data-cat`="41",
                                                                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$filter)),
                                                                tags$li(id = "categoryLeaflet1", class = "category-btn category-btn-leaflet", `data-cat`="42",
                                                                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$general)),
                                                                #timevis 
                                                                tags$li(id = "categoryTimevis1", class = "category-btn category-btn-timevis", `data-cat`="43",
                                                                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$data)),
                                                                tags$li(id = "categoryTimevis1", class = "category-btn category-btn-timevis", `data-cat`="44",
                                                                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$filter)),
                                                                tags$li(id = "categoryTimevis1", class = "category-btn category-btn-timevis", `data-cat`="45",
                                                                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$general)),
                                                                #pivot 
                                                                tags$li(id = "categoryPivot1", class = "category-btn category-btn-pivot", `data-cat`="46",
                                                                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$data)),
                                                                tags$li(id = "categoryPivot1", class = "category-btn category-btn-pivot", `data-cat`="47",
                                                                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$general)),
                                                                #custom 
                                                                tags$li(id = "categoryCustom1", class = "category-btn category-btn-custom", `data-cat`="48",
                                                                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$main)),
                                                                #valuebox 
                                                                tags$li(id = "categoryValuebox1", class = "category-btn category-btn-valuebox", `data-cat`="49",
                                                                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$main))
                                                        )
                                               ),
                                               tags$div(class="save-delete-wrapper",
                                                        actionButton("deleteGraph", lang$adminMode$graphs$ui$deleteGraph, icon("trash-alt"), class="save-delete-delete-btn full-width"),
                                                        actionButton("saveGraph", lang$adminMode$graphs$ui$saveGraph, icon("save"), class="save-delete-save-btn full-width")
                                               )
                                      ),
                                      tags$div(class="main-tab", style = "padding: 7px; max-height:66vh",
                                               tags$div(id = "tool_options"),
                                               tags$div(class = "space")
                                      )
                             )
                    ),
                    tags$div(class = "col-sm-6 preview-outer-wrapper",
                             tags$div(id = "preview-error", class = "err-msg"),
                             tags$div(id = "preview-content-plotly", 
                                      renderDataUI("preview_output_plotly", type = "graph", 
                                                   graphTool = "plotly", 
                                                   filterOptions = list(label = NULL, 
                                                                        multiple = TRUE, 
                                                                        col = "a"),
                                                   height = 400, 
                                                   noDataTxt = lang$nav$outputScreen$boxResults$noData)),
                             tags$div(id="pieValues", class = "config-message", lang$adminMode$graphs$validate$pieValues),
                             tags$div(id = "preview-content-dygraphs", style = "display:none;",
                                      renderDataUI("preview_output_dygraphs", type = "graph", 
                                                   graphTool = "dygraphs", 
                                                   filterOptions = list(label = NULL, 
                                                                        multiple = TRUE, 
                                                                        col = "a"), 
                                                   height = 400, 
                                                   noDataTxt = lang$nav$outputScreen$boxResults$noData)),
                             tags$div(id = "preview-content-leaflet", style = "display:none;",
                                      renderDataUI("preview_output_leaflet", type = "graph", 
                                                   graphTool = "leaflet",
                                                   filterOptions = list(label = NULL, 
                                                                        multiple = TRUE, 
                                                                        col = "a"), 
                                                   height = 400, 
                                                   noDataTxt = lang$nav$outputScreen$boxResults$noData)),
                             tags$div(id = "preview-content-pivot", style = "display:none; overflow:auto;",
                                      renderDataUI("preview_output_pivot", type = "pivot",
                                                   height = 400, 
                                                   noDataTxt = lang$nav$outputScreen$boxResults$noData)),
                             tags$div(id = "preview-content-timevis", style = "display:none; overflow:auto;",
                                      renderDataUI("preview_output_timevis", type = "graph", 
                                                   graphTool = "timevis", 
                                                   filterOptions = list(label = NULL, 
                                                                        multiple = TRUE, 
                                                                        col = "a"), 
                                                   height = 400, 
                                                   noDataTxt = lang$nav$outputScreen$boxResults$noData)),
                             tags$div(id = "preview-content-custom", style = "display:none; overflow:auto;text-align:left;",
                                      tags$h4(paste0(modelName,"_custom.R ", lang$adminMode$uiR$custom$skeleton)),
                                      verbatimTextOutput("preview_output_custom"),
                                      tags$h4(lang$adminMode$uiR$custom$steps),
                                      tags$ol(
                                        tags$li(sprintf(lang$adminMode$uiR$custom$li1, modelName, modelName)), 
                                        tags$li(sprintf(lang$adminMode$uiR$custom$li2, modelName)), 
                                        tags$li(lang$adminMode$uiR$custom$li3a, 
                                                tags$a(href = "https://gams.com/miro/customize.html#custom-renderers", 
                                                       lang$adminMode$uiR$custom$li3b, target = "_blank"),".")
                                      ),
                                      tags$div(sprintf(lang$adminMode$uiR$custom$description1, modelName)),
                                      tags$h4(lang$adminMode$uiR$custom$description2),
                                      tags$div(lang$adminMode$uiR$custom$description3)
                             ),
                             if(scalarsOutName %in% names(modelOut)){
                               tags$div(id = "preview-content-valuebox", style = "display:none;text-align:left",
                                        renderDataUI("preview_output_valuebox", type = "valuebox", 
                                                     height = 400, customOptions = list(count = modelOut[[scalarsOutName]]$count),
                                                     noDataTxt = lang$nav$outputScreen$boxResults$noData))}
                    )
                )
              )
      ),
      tabItem(tabName = "new_widget",
              fluidRow(
                box(title = lang$adminMode$widgets$ui$title, status="primary", solidHeader = TRUE, width = 12,
                    tags$div(id = "widgetUpdateSuccess", class = "gmsalert gmsalert-success", lang$adminMode$widgets$ui$widgetUpdateSuccess),
                    tags$div(id = "widgetValidationErr", class = "gmsalert gmsalert-error"),
                    tags$div(id = "unknownErrorWidgets", class = "gmsalert gmsalert-error",
                             lang$errMsg$unknownError),
                    tags$div(class = "space"),
                    tags$div(class="main-tab", 
                             tags$div(style = "padding-bottom: 20px;",
                                      tabsetPanel(id="widget_symbol_type",
                                                  tabPanel(lang$adminMode$widgets$ui$gams, value = "gams"),
                                                  tabPanel(lang$adminMode$widgets$ui$go, value = "go"),
                                                  tabPanel(lang$adminMode$widgets$ui$dd, value = "dd")
                                      )),
                             tags$div(class = "col-sm-6",
                                      tags$div(id = "noSymbolMsg", class="config-message", 
                                               lang$adminMode$widgets$ui$noSymbolMsg),
                                      tags$div(id = "noWidgetMsg", class="config-message", 
                                               lang$adminMode$widgets$ui$noWidgetMsg),
                                      tags$div(id = "noWidgetConfigMsg", class="config-message", 
                                               lang$adminMode$widgets$ui$noWidgetConfigMsg),
                                      tags$div(id = "optionConfigMsg", class="config-message"),
                                      tags$div(id = "doubledashConfigMsg", class="config-message"),
                                      tags$div(class="main-tab", style = "min-height: 600px;",
                                               tags$div(class = "row",
                                                        tags$div(class = "col-sm-6 col-6-left",
                                                                 conditionalPanel(
                                                                   condition = "input.widget_symbol_type == 'gams'",
                                                                   tags$div(title = lang$adminMode$widgets$ui$inputSymbolTooltip,
                                                                            selectInput("widget_symbol", lang$adminMode$widgets$ui$inputSymbol, 
                                                                                        choices = c())
                                                                   )),
                                                                 conditionalPanel(
                                                                   condition = "input.widget_symbol_type == 'go'",
                                                                   tags$div(title = lang$adminMode$widgets$ui$widgetGoTooltip,
                                                                            textInput("widget_go", lang$adminMode$widgets$ui$widgetGo)
                                                                   )),
                                                                 conditionalPanel(
                                                                   condition = "input.widget_symbol_type == 'dd'",
                                                                   tags$div(title = lang$adminMode$widgets$ui$widgetDdTooltip,
                                                                            textInput("widget_dd", lang$adminMode$widgets$ui$widgetDd)
                                                                   ))),
                                                        tags$div(class = "col-sm-6 col-6-right",
                                                                 selectInput("widget_type", lang$adminMode$widgets$ui$widgetType, choices = c()))),
                                               tags$hr(),
                                               tags$div(id = "widget_options"),
                                               tags$div(class = "space")
                                      )
                             ),
                             tags$div(class = "col-sm-6",
                                      tags$div(style = "margin-bottom:50px;text-align:right;",
                                               actionButton("deleteWidget", "Delete", icon("trash-alt"), class = "save-delete-delete-btn",
                                                            style = "width:100px;"),
                                               actionButton("saveWidget", "Save", icon("save"), class = "save-delete-save-btn",
                                                            style = "width:100px;")),
                                      uiOutput("widget_preview")
                             ))
                )
              )
      ),
      tabItem(tabName = "new_gen",
              fluidRow(
                box(title = lang$adminMode$general$ui$title, status="primary", solidHeader = TRUE, width = 12,
                    tags$div(class = "space"),
                    tabsetPanel(
                      tabPanel(lang$adminMode$general$ui$tabInterface, 
                               tags$div(class = "col-sm-6", style = "padding-top: 20px;",
                                        tags$div(class="main-tab",
                                                 tags$div(id = "interface_wrapper1"),
                                                 tags$div(class = "space")
                                        )
                               ),
                               tags$div(class = "col-sm-6", style = "padding-top: 20px;",
                                        tags$div(class="main-tab",
                                                 tags$div(id = "interface_wrapper2"),
                                                 tags$div(class = "space")
                                        )
                               )),
                      tabPanel(lang$adminMode$general$ui$tabSymbols, 
                               tags$div(class = "col-sm-6", style = "padding-top: 20px;",
                                        tags$div(class="main-tab",
                                                 tags$h2(lang$adminMode$general$ui$headerSymbolNaming, 
                                                         tags$a(class="info-wrapper", style="top:-10px;", href="https://gams.com/miro/customize.html#naming", 
                                                                tags$span(class="fas fa-info-circle", class="info-icon"), target="_blank"), class="option-category"),
                                                 tags$h4(lang$adminMode$general$overwriteSymbolAliases$input, class="option-category"),
                                                 tags$div(class = "small-space"),
                                                 lapply(names(inputSymHeaders), function(name){
                                                   if(name %in% names(configJSON$overwriteAliases)){
                                                     symAlias <- configJSON$overwriteAliases[[name]]$newAlias
                                                   }else{
                                                     symAlias <- modelInRaw[[name]]$alias
                                                   }
                                                   if(!name %in% names(configJSON$overwriteHeaderAliases) ||
                                                      length(modelInRaw[[name]]$headers) != length(symHeaders)){
                                                     symHeaders <- names(inputSymHeaders[[name]])
                                                   }else{
                                                     symHeaders <- configJSON$overwriteHeaderAliases[[name]]$newHeaders
                                                   }
                                                   
                                                   tags$div(
                                                     column(6L, tags$div(paste0(name, " (", 
                                                                                paste0(names(modelInRaw[[name]]$headers), 
                                                                                       collapse = ","), ")"))),
                                                     column(6L, 
                                                            textInput(paste0("general_overwriteSymAlias_", name), 
                                                                      lang$adminMode$general$overwriteSymbolAliases$label,
                                                                      symAlias),
                                                            selectizeInput(paste0("general_overwriteSymHeaders_", name), 
                                                                           lang$adminMode$general$overwriteSymbolHeaders$label,
                                                                           choices = symHeaders, selected = symHeaders,
                                                                           multiple = TRUE,  options = list(
                                                                             'maxItems' = length(symHeaders),
                                                                             'create' = TRUE,
                                                                             plugins = list("restore_on_backspace"))))
                                                   )
                                                 }),
                                                 tags$div(class = "space"),
                                                 tags$h4(lang$adminMode$general$overwriteSymbolAliases$output, class="option-category"),
                                                 tags$div(class = "small-space"),
                                                 lapply(names(modelOut), function(name){
                                                   if(name %in% names(configJSON$overwriteAliases)){
                                                     symAlias <- configJSON$overwriteAliases[[name]]$newAlias
                                                   }else{
                                                     symAlias <- modelOut[[name]]$alias
                                                   }
                                                   symHeaders <- configJSON$overwriteHeaderAliases[[name]]$newHeaders
                                                   if(!name %in% names(configJSON$overwriteHeaderAliases) ||
                                                      length(symHeaders) != length(modelOut[[name]]$headers)){
                                                     symHeaders <- outputSymHeaders[[name]]
                                                   }
                                                   
                                                   tags$div(
                                                     column(6L, tags$div(paste0(name, " (", 
                                                                                paste0(names(modelOut[[name]]$headers), 
                                                                                       collapse = ","), ")"))),
                                                     column(6L, 
                                                            textInput(paste0("general_overwriteSymAlias_", name), 
                                                                      lang$adminMode$general$overwriteSymbolAliases$label,
                                                                      symAlias),
                                                            selectizeInput(paste0("general_overwriteSymHeaders_", name), 
                                                                           lang$adminMode$general$overwriteSymbolHeaders$label,
                                                                           choices = symHeaders, selected = symHeaders,
                                                                           multiple = TRUE,  options = list(
                                                                             'maxItems' = length(symHeaders),
                                                                             'create' = TRUE,
                                                                             plugins = list("restore_on_backspace"))))
                                                   )
                                                 }),
                                                 tags$div(class = "space")
                                        )
                               ),
                               tags$div(class = "col-sm-6", style = "padding-top: 20px;",
                                        tags$div(class="main-tab",
                                                 tags$h2(lang$adminMode$general$ui$headerSymbolGrouping, 
                                                         tags$a(class="info-wrapper", style="top:-10px;", href="https://gams.com/miro/customize.html#tab-ordering", 
                                                                tags$span(class="fas fa-info-circle", class="info-icon"), target="_blank"), class="option-category"),
                                                 tags$div(class = "option-wrapper",
                                                          selectizeInput("general_overwriteSheetOrderInput", lang$adminMode$general$overwriteSheetOrder$input, 
                                                                         choices = inputTabs,
                                                                         selected = inputTabs,
                                                                         multiple = TRUE, 
                                                                         options = list(plugins = list("drag_drop", "no_delete"))),
                                                          selectizeInput("general_overwriteSheetOrderOutput", lang$adminMode$general$overwriteSheetOrder$output, 
                                                                         choices = outputTabs, 
                                                                         selected = outputTabs, 
                                                                         multiple = TRUE, options = list(plugins = list("drag_drop", "no_delete")))
                                                 ),
                                                 tags$div(class = "space"),
                                                 tags$div(class = "option-wrapper",
                                                   tags$div(class = "info-position",
                                                            tags$h2((lang$adminMode$general$ui$headerTabGrouping), 
                                                                    tags$a(class="info-wrapper", style="top:-10px;", href="https://gams.com/miro/customize.html#tab-grouping", 
                                                                           tags$span(class="fas fa-info-circle", class="info-icon"), target="_blank"))
                                                   ),
                                                   tags$h4(lang$adminMode$general$ui$headerInputGroups),
                                                   tags$div(class="option-wrapper-indented",
                                                            createArray(NULL, "symbol_inputGroups", 
                                                                        lang$adminMode$general$groups$input, 
                                                                        autoCreate = FALSE)),
                                                   tags$h4(lang$adminMode$general$ui$headerOutputGroups),
                                                   tags$div(class="option-wrapper-indented",
                                                            createArray(NULL, "symbol_outputGroups", 
                                                                        lang$adminMode$general$groups$output, 
                                                                        autoCreate = FALSE))
                                                 ),
                                                 tags$div(class = "space"),
                                                 tags$div(class = "option-wrapper",
                                                          tags$div(class = "info-position",
                                                                   tags$h2((lang$adminMode$general$ui$headerTabSymlinks), 
                                                                           tags$a(class="info-wrapper", style="top:-10px;", href="https://gams.com/miro/customize.html#tab-symlinks", 
                                                                                  tags$span(class="fas fa-info-circle", class="info-icon"), target="_blank"))
                                                          ),
                                                          tags$div(class="option-wrapper-indented",
                                                                   createArray(NULL, "symbol_links", 
                                                                               lang$adminMode$general$symlinks$label, 
                                                                               autoCreate = FALSE))
                                                 )
                                        )
                               )),
                      tabPanel(lang$adminMode$general$ui$tabModules, 
                               tags$div(class = "col-sm-6", style = "padding-top: 20px;",
                                        tags$div(class="main-tab",
                                                 tags$div(id = "module_wrapper1"),
                                                 tags$div(class = "space")
                                        )      
                               ),
                               tags$div(class = "col-sm-6", style = "padding-top: 20px;",
                                        tags$div(class="main-tab",
                                                 tags$div(id = "module_wrapper2"),
                                                 tags$div(class = "space")
                                        )      
                               )),
                      tabPanel(lang$adminMode$general$ui$tabScripts, 
                               tags$div(class = "col-sm-6", style = "padding-top: 20px;",
                                        tags$div(class="main-tab",
                                                 tags$h2(lang$adminMode$general$ui$headerBaseScripts),
                                                 tags$div(class="option-wrapper-indented",
                                                          createArray(NULL, "scripts_base", 
                                                                      lang$adminMode$general$scripts$base, 
                                                                      autoCreate = FALSE)),
                                                 tags$div(class = "space")
                                        )      
                               ),
                               tags$div(class = "col-sm-6", style = "padding-top: 20px;",
                                        tags$div(class="main-tab",
                                                 tags$h2(lang$adminMode$general$ui$headerHcubeScripts),
                                                 tags$div(class="option-wrapper-indented",
                                                          createArray(NULL, "scripts_hcube", 
                                                                      lang$adminMode$general$scripts$hcube, 
                                                                      autoCreate = FALSE)),
                                                 tags$div(class = "space")
                                        )      
                               ))
                    )
                )
              )
      ),
      tabItem(tabName = "tables_gen",
              fluidRow(
                box(title = lang$adminMode$tables$ui$title, status="primary", solidHeader = TRUE, width = 12,
                    tags$div(id = "tableWidgetUpdateSuccess", class = "gmsalert gmsalert-success", lang$adminMode$widgets$ui$widgetTableUpdateSuccess),
                     tags$div(id = "tableValidationErr", class = "gmsalert gmsalert-error"),
                    # tags$div(id = "unknownErrorTables", class = "gmsalert gmsalert-error",
                    #          lang$errMsg$unknownError),
                    tags$div(class = "space"),
                    tags$div(style = "padding-bottom: 20px;",
                             tabsetPanel(id="table_type",
                                         tabPanel(lang$adminMode$tables$ui$input, value = "hot"),
                                         tabPanel(lang$adminMode$tables$ui$output, value = "dt"),
                                         tabPanel(lang$adminMode$tables$ui$symbol, value = "symbol")
                             )),
                    tags$div(class = "col-sm-6",
                             tags$div(id = "noTableSymbolMsg", class="config-message", 
                                      lang$adminMode$widgets$ui$noSymbolMsg),
                             tags$div(class="main-tab table-tab",
                                      tags$div(
                                        conditionalPanel(
                                          condition = "input.table_type == 'symbol'",
                                          tags$div(title = lang$adminMode$widgets$ui$tableTooltip, class = "option-wrapper",
                                                   selectInput("table_symbol", lang$adminMode$widgets$ui$inputSymbol, 
                                                               choices = c())),
                                          tags$hr()
                                        )),
                                      tags$div(id = "table_wrapper"),
                                      tags$div(id = "pivotColsRestriction", class="config-message", 
                                               lang$adminMode$widgets$ui$pivotColsRestriction),
                                      tags$div(class = "space")
                             )
                    ),
                    tags$div(class = "col-sm-6", style = "text-align:right;overflow:auto;",
                             tags$div(id = "preview-output-hot", 
                                      rHandsontableOutput("table_preview_hot")),
                             tags$div(id = "preview-output-dt", style = "display:none;",
                                      renderDataUI("table_preview_dt", type = "datatable", 
                                                   graphTool = "plotly", 
                                                   height = 700, 
                                                   noDataTxt = lang$nav$outputScreen$boxResults$noData)),
                             tags$div(id = "preview-output-tableWidget", style = "display:none;",
                                      tags$div(style = "margin-bottom:50px;text-align:right;",
                                               actionButton("deleteTableWidget", lang$adminMode$tables$ui$resetTable, icon("undo"), class = "save-delete-delete-btn",
                                                            style = "width:100px;"),
                                               actionButton("saveTableWidget", lang$adminMode$tables$ui$saveTable, icon("save"), class = "save-delete-save-btn",
                                                            style = "width:100px;")),
                                      DTOutput("dt_preview"),
                                      rHandsontableOutput("hot_preview"))
                    )
                )
              )
      )
    )
  )})
ui_admin <- dashboardPage(header_admin, sidebar_admin, body_admin, skin = "black")
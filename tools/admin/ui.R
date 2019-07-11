## UI body
langSpecificUI <- list()
langSpecificUI$tableType <- c("input table" = "hot", "output table" = "dt")
names(langSpecificUI$tableType) <- lang$adminMode$tables$ui$choices
langSpecificUI$symbolType <- c("Symbol" = "gams", "New GAMS option" = "go", 
                               "New double dash parameter" = "dd")
names(langSpecificUI$symbolType) <- lang$adminMode$widgets$ui$choices

header_admin <- dashboardHeader(
  tags$li(class = "dropdown", 
          tags$a(href="#", class="dropdown-toggle", "data-toggle" = "dropdown", 
                 lang$nav$header$help$title, tags$span(class="caret")),
          tags$ul(class = "dropdown-menu", role="menu",
                  tags$li(tags$a(href = "https://www.gams.com/miro/", 
                                 target = "_blank", lang$nav$header$help$doc)),
                  tags$li(HTML(paste0('<a href="#" class="action-button" onclick="Miro.confirmModalShow(\'',
                                      'About MIRO\', \'', 
                                      htmltools::htmlEscape(aboutDialogText), '\', \'Cancel\')">About</a>')
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
      tags$link(type = "text/css", rel = "stylesheet", href = "miro.css"),
      tags$link(type = "text/css", rel = "stylesheet", href = "bootstrap-colorpicker.min.css"),
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
                    tags$label("for" = "db_remove_wrapper", lang$adminMode$database$remove),
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
                    tags$div(class = "space"),
                    tags$div(class = "col-sm-6",
                             tags$div(style = "max-height:800px;max-height: 80vh;overflow:auto;padding-right:30px;",
                                      fileInput("localInput", lang$adminMode$graphs$ui$localInput,
                                                width = "100%",
                                                multiple = FALSE,
                                                accept = c("application/vnd.ms-excel", 
                                                           "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", 
                                                           ".xlsx", ".gdx")),
                                      tags$div(id = "preview_wrapper", style = "display:none;",
                                               selectInput("gams_symbols", lang$adminMode$graphs$ui$gamsSymbols,
                                                           choices = NULL),
                                               textInput("chart_title", lang$adminMode$graphs$ui$chartTitle),
                                               numericInput("chart_height", lang$adminMode$graphs$ui$height, min = 0L, value = 700),
                                               selectInput("chart_tool", lang$adminMode$graphs$ui$tool, 
                                                           setNames(c("plotly", "dygraphs", "leaflet", "timevis", "pivot", "valuebox"), 
                                                                    lang$adminMode$graphs$ui$choices)),
                                               tags$div(id = "tool_options"),
                                               tags$div(style = "height:100px;")
                                      )
                             )
                    ),
                    tags$div(class = "col-sm-6", style = "text-align:right;",
                             tags$div(id = "preview-error", class = "err-msg"),
                             tags$div(id = "preview-content-plotly", 
                                      renderDataUI("preview_output_plotly", type = "graph", 
                                                   graphTool = "plotly", 
                                                   height = 400, 
                                                   noDataTxt = lang$nav$outputScreen$boxResults$noData)),
                             tags$div(id = "preview-content-dygraph", style = "display:none;",
                                      renderDataUI("preview_output_dygraph", type = "graph", 
                                                   graphTool = "dygraphs", 
                                                   height = 400, 
                                                   noDataTxt = lang$nav$outputScreen$boxResults$noData)),
                             tags$div(id = "preview-content-leaflet", style = "display:none;",
                                      renderDataUI("preview_output_leaflet", type = "graph", 
                                                   graphTool = "leaflet", 
                                                   height = 400, 
                                                   noDataTxt = lang$nav$outputScreen$boxResults$noData)),
                             tags$div(id = "preview-content-pivot", style = "display:none; overflow:auto;",
                                      renderDataUI("preview_output_pivot", type = "pivot",
                                                   height = 400, 
                                                   noDataTxt = lang$nav$outputScreen$boxResults$noData)),
                             tags$div(id = "preview-content-timevis", style = "display:none; overflow:auto;",
                                      renderDataUI("preview_output_timevis", type = "graph", 
                                                   graphTool = "timevis", 
                                                   height = 400, 
                                                   noDataTxt = lang$nav$outputScreen$boxResults$noData)),
                             if(scalarsOutName %in% names(modelOut)){
                               tags$div(id = "preview-content-valuebox", style = "display:none;",
                                        renderDataUI("preview_output_valuebox", type = "valuebox", 
                                                     height = 400, customOptions = list(count = modelOut[[scalarsOutName]]$count),
                                                     noDataTxt = lang$nav$outputScreen$boxResults$noData))},
                             tags$div(style = "margin-top: 50px; margin-bottom:50px;",
                                      actionButton("deleteGraph", "Delete", icon("trash-alt")),
                                      actionButton("saveGraph", "Save", icon("save")))
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
                    tags$div(class = "col-sm-6",
                             tags$div(style = "padding-bottom: 20px;",
                                      radioButtons("widget_symbol_type", label = lang$adminMode$widgets$ui$symbolType,
                                                   choices = langSpecificUI$symbolType, 
                                                   selected = "gams", inline = TRUE)),
                             tags$div(id = "noWidgetConfigMsg", style = "padding: 15px;margin-bottom: 20px;font-weight: bold;
                                      text-align: center;font-size: 12pt;background: #3c8dbcb0;display: none;", 
                                      lang$adminMode$widgets$ui$noWidgetConfigMsg),
                             tags$div(id = "optionConfigMsg", style = "padding: 15px;margin-bottom: 20px;font-weight: bold;
                                      text-align: center;font-size: 12pt;background: #3c8dbcb0;display: none;", 
                                      lang$adminMode$widgets$ui$optionConfigMsg),
                             tags$div(id = "doubledashConfigMsg", style = "padding: 15px;margin-bottom: 20px;font-weight: bold;
                                      text-align: center;font-size: 12pt;background: #3c8dbcb0;display: none;", 
                                      lang$adminMode$widgets$ui$doubledashConfigMsg),
                             tags$div(style = "max-height:800px;max-height: 80vh;overflow:auto;padding-right:30px;",
                                      conditionalPanel(
                                        condition = "input.widget_symbol_type == 'gams'",
                                        tags$div(style = "max-width:400px;",
                                                 selectInput("widget_symbol", lang$adminMode$widgets$ui$inputSymbol, 
                                                             choices = c()))
                                      ),
                                      conditionalPanel(
                                        condition = "input.widget_symbol_type == 'go'",
                                        tags$div(style = "max-width:400px;",
                                                 textInput("widget_go", lang$adminMode$widgets$ui$widgetGo))
                                      ),
                                      conditionalPanel(
                                        condition = "input.widget_symbol_type == 'dd'",
                                        tags$div(style = "max-width:400px;",
                                                 textInput("widget_dd", lang$adminMode$widgets$ui$widgetDd))
                                      ),
                                      tags$div(style = "max-width:400px;",
                                               selectInput("widget_type", lang$adminMode$widgets$ui$widgetType, choices = c())),
                                      tags$div(id = "widget_options"),
                                      tags$div(style = "height:100px;")
                             )
                    ),
                    tags$div(class = "col-sm-6",
                             uiOutput("widget_preview"),
                             rHandsontableOutput("hot_preview"),
                             DTOutput("dt_preview"),
                             tags$div(style = "margin-top: 50px; margin-bottom:50px;text-align:right;",
                                      actionButton("deleteWidget", "Delete", icon("trash-alt")),
                                      actionButton("saveWidget", "Save", icon("save")))
                    )
                )
              )
      ),
      tabItem(tabName = "new_gen",
              fluidRow(
                box(title = lang$adminMode$general$ui$title, status="primary", solidHeader = TRUE, width = 12,
                    tags$div(class = "space"),
                    tags$div(class = "col-sm-6",
                             tags$div(style = "max-height:800px;max-height:80vh;overflow:auto;padding-right:30px;",
                                      tags$div(id = "general_wrapper"),
                                      tags$div(style = "height:100px;")
                             )
                    ),
                    tags$div(class = "col-sm-6",
                             tags$div(style = "max-height:800px;max-height:80vh;overflow:auto;padding-right:30px;",
                                      tags$div(id = "general_wrapper2"),
                                      tags$div(style = "height:100px;")
                             )      
                    )
                )
              )
      ),
      tabItem(tabName = "tables_gen",
              fluidRow(
                box(title = lang$adminMode$tables$ui$title, status="primary", solidHeader = TRUE, width = 12,
                    tags$div(class = "space"),
                    tags$div(class = "col-sm-6",
                             tags$div(style = "max-height:800px;max-height: 80vh;overflow:auto;padding-right:30px;",
                                      tags$div(style = "padding-bottom: 20px;",
                                               radioButtons("table_type", label = lang$adminMode$tables$ui$tableType, inline = TRUE,
                                                            choices = langSpecificUI$tableType, 
                                                            selected = "hot")),
                                      tags$div(id = "table_wrapper"),
                                      tags$div(style = "height:100px;")
                             )
                    ),
                    tags$div(class = "col-sm-6", style = "text-align:right;overflow:auto;",
                             tags$div(id = "preview-output-hot", 
                                      rHandsontableOutput("table_preview_hot")),
                             tags$div(id = "preview-output-dt", style = "display:none;",
                                      renderDataUI("table_preview_dt", type = "datatable", 
                                                   graphTool = "plotly", 
                                                   height = 700, 
                                                   noDataTxt = lang$nav$outputScreen$boxResults$noData))
                    )
                )
              )
      )
    )
  )})
ui_admin <- dashboardPage(header_admin, sidebar_admin, body_admin, skin = "black")
## UI body
header_admin <- dashboardHeader(
  tags$li(class = "dropdown", 
          tags$a(href="#", class="dropdown-toggle", "data-toggle" = "dropdown", 
                 "Help", tags$span(class="caret")),
          tags$ul(class = "dropdown-menu", role="menu",
                  tags$li(tags$a(href = "https://www.gams.com/miro/", 
                                 target = "_blank", "Documentation")),
                  tags$li(HTML(paste0('<a href="#" class="action-button" onclick="confirmModalShow(\'',
                                      'About MIRO\', \'', 
                                      htmltools::htmlEscape(aboutDialogText), '\', \'Cancel\')">About</a>')
                  )))),
  title=paste0("GAMS MIRO admin panel (", modelName, ")"), disable = FALSE)

sidebar_admin <- dashboardSidebar(
  sidebarMenu(id="sidebarMenuId",
              menuItem("General settings", tabName = "new_gen", icon = icon("cogs")),
              menuItem("Table settings", tabName="tables_gen", icon = icon("sliders-h")),
              menuItem("Configure graphs", tabName = "new_graph", icon = icon("chart-bar")),
              menuItem("Configure widgets", tabName="new_widget", icon = icon("sliders-h")),
              menuItem("Database management", tabName="db_management", icon = icon("database"))
  )
)

body_admin <- dashboardBody({
  addResourcePath("admin", "tools/admin/resources")
  tagList(
    tags$head(
      tags$link(type = "text/css", rel = "stylesheet", href = "miro.css"),
      tags$link(type = "text/css", rel = "stylesheet", href = "admin/bootstrap-colorpicker.min.css"),
      tags$script(src = "mirosc.js", type = "application/javascript"),
      tags$script(src = "miro.js", type = "application/javascript"),
      tags$script(src = "admin/miro_admin.js", type = "application/javascript"),
      tags$script(src = "admin/bootstrap-colorpicker.min.js", type = "application/javascript"),
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
                box(title = "Database management", status="primary", solidHeader = TRUE, width = 12,
                    tags$div(id = "removeSuccess", class = "gmsalert gmsalert-success",
                             "Database tables removed successfully"),
                    tags$div(id = "restoreSuccess", class = "gmsalert gmsalert-success",
                             "Data restored successfully"),
                    tags$div(id = "restoreNoData", class = "gmsalert gmsalert-error",
                             "No csv files found in the archive provided. Nothing was restored."),
                    tags$div(id = "restoreInvalidData", class = "gmsalert gmsalert-error",
                             "At least one of the tables is invalid. Nothing was restored."),
                    tags$div(id = "maxRowError", class = "gmsalert gmsalert-error",
                             "The maximum number of rows to export was exceeded for atleast 1 table. Please back up the database manually!"),
                    tags$div(id = "unknownError", class = "gmsalert gmsalert-error",
                             "An unexpected error occurred. Maybe your database is not empty?"),
                    tags$div(class = "space"),
                    tags$div("You want to create a backup of the database? Click the button below to get a zip archive with all the database tables saved into csv files. Be aware that it is faster to backup your database using the native backup tool of your DBMS. This means that in case of big databases you should backup your database manually!"),
                    downloadButton("dbSaveAll", label = "Save database tables"),
                    tags$div(class = "space"),
                    tags$hr(),
                    tags$div(class = "space"),
                    tags$div("You want to restore the database with data from an existing zip file? Please select the file you want to use to restore the database below.",
                             fileInput("dbBackupZip", "Select a backup zip file", 
                                       accept = c(".zip", "application/zip", 
                                                  "application/octet-stream", 
                                                  "application/x-zip-compressed", 
                                                  "multipart/x-zip")),
                             actionButton("restoreDb", "Restore")
                    ),
                    tags$div(class = "space"),
                    tags$hr(),
                    tags$div(class = "space"),
                    tags$div("You want to remove all the tables that belong to your model (e.g. because the schema changed)?",
                             HTML(paste0('<button type="button" class="btn btn-default"', 
                                         ' onclick="confirmModalShow(\'Remove database tables\', \'Are you sure that you want to delete all database tables? ',
                                         'This can not be undone! You might want to save the database first before proceeding.\', \'Cancel\', ',
                                         '\'Remove tables\', \'Shiny.setInputValue(\\\'removeDbTables\\\', 1, {priority: \\\'event\\\'});\')">Delete all database tables</button>'
                             ))
                    )
                )
              )
      ),
      tabItem(tabName = "new_graph",
              fluidRow(
                box(title = "Configure graphs", status="primary", solidHeader = TRUE, width = 12,
                    tags$div(id = "graphUpdateSuccess", class = "gmsalert gmsalert-success", "Graph configuration was updated successfully"),
                    tags$div(id = "unknownErrorGraphs", class = "gmsalert gmsalert-error",
                             "An unexpected error occurred. If this problem persists, please contact the system administrator."),
                    tags$div(class = "space"),
                    tags$div(class = "col-sm-6",
                             tags$div(style = "max-height:800px;max-height: 80vh;overflow:auto;padding-right:30px;",
                                      fileInput("localInput", "Upload an Excel spreadsheet with data that will be used for preview purposes",
                                                width = "100%",
                                                multiple = FALSE,
                                                accept = c("application/vnd.ms-excel", 
                                                           "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet", 
                                                           ".xlsx")),
                                      tags$div(id = "preview_wrapper", style = "display:none;",
                                               selectInput("gams_symbols", "Select which GAMS symbol to plot",
                                                           choices = NULL),
                                               textInput("chart_title", "Choose a title for your chart"),
                                               numericInput("chart_height", "Choose a height for your chart (px)", min = 0L, value = 700),
                                               selectInput("chart_tool", "Select the charting tool you want to use", 
                                                           setNames(c("plotly", "dygraphs", "leaflet"), 
                                                                    c("Diagram Tool", "Time Series Diagram Tool", "Map chart"))),
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
                             tags$div(style = "margin-top: 50px; margin-bottom:50px;",
                                      actionButton("saveGraph", "Save", icon("save")))
                    )
                )
              )
      ),
      tabItem(tabName = "new_widget",
              fluidRow(
                box(title = "Configure input widgets", status="primary", solidHeader = TRUE, width = 12,
                    tags$div(id = "widgetUpdateSuccess", class = "gmsalert gmsalert-success", "Widget configuration was updated successfully"),
                    tags$div(id = "widgetValidationErr", class = "gmsalert gmsalert-error"),
                    tags$div(id = "unknownErrorWidgets", class = "gmsalert gmsalert-error",
                             "An unexpected error occurred. If this problem persists, please contact the system administrator."),
                    tags$div(class = "space"),
                    tags$div(class = "col-sm-6",
                             radioButtons("widget_symbol_type", label = "Symbol type to configure",
                                          choices = list("Symbol" = "gams", "GAMS option" = "go", 
                                                         "Double dash parameter" = "dd"), 
                                          selected = "gams"),
                             tags$div(id = "noWidgetConfigMsg", style = "padding: 15px;font-weight: bold;
                                      text-align: center;font-size: 12pt;background: orange;display: none;", 
                                      "Currently, there is no widget configured for this symbol"),
                             tags$div(style = "max-height:800px;max-height: 80vh;overflow:auto;padding-right:30px;",
                                      conditionalPanel(
                                        condition = "input.widget_symbol_type == 'gams'",
                                        selectInput("widget_symbol", "Which input symbol would you like to create a widget for?", 
                                                    choices = c())
                                      ),
                                      conditionalPanel(
                                        condition = "input.widget_symbol_type == 'go'",
                                        textInput("widget_clPar", "Name of the GAMS option (e.g. 'LP' or 'OptCR')")
                                      ),
                                      conditionalPanel(
                                        condition = "input.widget_symbol_type == 'dd'",
                                        textInput("widget_clPar", "Name of the double-dash parameter (without '--')")
                                      ),
                                      selectInput("widget_type", "Select the type of widget you want to use", choices = c()),
                                      tags$div(id = "widget_options"),
                                      tags$div(style = "height:100px;")
                             )
                    ),
                    tags$div(class = "col-sm-6",
                             uiOutput("widget_preview"),
                             rHandsontableOutput("table_preview"),
                             tags$div(style = "margin-top: 50px; margin-bottom:50px;text-align:right;",
                                      actionButton("deleteWidget", "Delete", icon("trash-alt")),
                                      actionButton("saveWidget", "Save", icon("save")))
                    )
                )
              )
      ),
      tabItem(tabName = "new_gen",
              fluidRow(
                box(title = "General settings", status="primary", solidHeader = TRUE, width = 12,
                    tags$div(class = "space"),
                    tags$div(style = "max-height:800px;max-height:80vh;overflow:auto;padding-right:30px;",
                            tags$div(id = "general_wrapper"),
                            tags$div(style = "height:100px;")
                    )
                )
              )
      ),
      tabItem(tabName = "tables_gen",
        fluidRow(
          box(title = "General table settings", status="primary", solidHeader = TRUE, width = 12,
              tags$div(class = "space"),
              tags$div(class = "col-sm-6",
                       tags$div(style = "max-height:800px;max-height: 80vh;overflow:auto;padding-right:30px;",
                                radioButtons("table_type", label = "Which table type would you like to configure?",
                                             choices = c("input table" = "hot", "output table" = "dt"), 
                                             selected = "hot"),
                                tags$div(id = "table_wrapper"),
                                tags$div(style = "height:100px;")
                       )
              ),
              tags$div(class = "col-sm-6", style = "text-align:right;",
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
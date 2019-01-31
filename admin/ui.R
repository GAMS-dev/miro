## UI body
header_admin <- dashboardHeader(
  tags$li(class = "dropdown", 
          tags$a(href="#", class="dropdown-toggle", "data-toggle" = "dropdown", 
                 "Help", tags$span(class="caret")),
          tags$ul(class = "dropdown-menu", role="menu",
                  tags$li(tags$a(href = "https://www.gams.com/webui/", 
                                 target = "_blank", "Documentation")),
                  tags$li(HTML(paste0('<a href="#" class="action-button" onclick="confirmModalShow(\'',
                                      'About GAMS WebUI\', \'', 
                                      htmltools::htmlEscape(aboutDialogText), '\', \'Cancel\')">About</a>')
                  )))),
  title=paste0("GAMS WebUI admin panel (", modelName, ")"), disable = FALSE)
sidebar_admin <- dashboardSidebar(
  sidebarMenu(id="sidebarMenuId",
 #             menuItem("Configuration generator", tabName="config_gen", icon = icon("gear")),
              menuItem("Database management", tabName="db_management", icon = icon("database"))
  )
)
body_admin <- dashboardBody({
  addResourcePath("admin", "admin/resources")
  tagList(
    tags$head(
      tags$link(type = "text/css", rel = "stylesheet", href = "miro.css"),
#      tags$link(type = "text/css", rel = "stylesheet", href = "admin/spectrum.css"),
#      tags$link(type = "text/css", rel = "stylesheet", href = "admin/bootstrap-datetimepicker.min.css"),
#      tags$link(type = "text/css", rel = "stylesheet", href = "admin/alpaca.min.css"),
      tags$script(src = "mirosc.js", type = "application/javascript"),
      tags$script(src = "miro.js", type = "application/javascript"),
#      tags$script(src = "admin/spectrum.js", type = "application/javascript"),
#      tags$script(src = "admin/moment.min.js", type = "application/javascript"),
#      tags$script(src = "admin/bootstrap-datetimepicker.min.js", type = "application/javascript"),
#      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/handlebars.js/4.0.5/handlebars.min.js", type = "application/javascript"),
#      tags$script(src = "admin/alpaca.min.js", type = "application/javascript"),
      tags$script(src = "admin/config-gen.js", type = "application/javascript"),
      
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
      )#,
     #  tabItem(tabName = "config_gen",
     #          fluidRow(
     #            box(title = "Configuration generator", status="primary", solidHeader = TRUE, width = 12,
     #                tags$div(id = "updateConfigSuccess", class = "gmsalert gmsalert-success",
     #                         "The configuration was updated successfully"),
     #                tags$div(id = "updateConfigError", class = "gmsalert gmsalert-success",
     #                         "An unexpected error occurred while updating your configuration. If this problem persists, please contact the system administrator."),
     #                tags$div(class = "space"),
     #                tags$div(id = "configGenForm", ""),
     #                tagAppendAttributes(actionButton("btConfigGenNew", "Update config"),
     #                                    style = "display:none;", 
     #                                    onclick = "$('#configGenForm').show();$(this).hide();")
     #            )
     #          )
     #  )
    )
  )})
ui_admin <- dashboardPage(header_admin, sidebar_admin, body_admin, skin = "black")
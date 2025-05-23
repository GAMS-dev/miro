## UI body
langSpecificUI <- list()
langSpecificUI$tableType <- c("input table" = "hot", "output table" = "dt")
names(langSpecificUI$tableType) <- lang$adminMode$tables$ui$choices
langSpecificUI$symbolType <- c(
  "Symbol" = "gams", "New GAMS option" = "go",
  "New double dash parameter" = "dd"
)
names(langSpecificUI$symbolType) <- lang$adminMode$widgets$ui$choices
langSpecificUI$theme <- c(
  "Use system/browser settings" = "browser",
  "Light mode" = "light", "Dark mode" = "dark"
)
names(langSpecificUI$theme) <- lang$adminMode$general$theme$choices
langSpecificUI$scen <- c(
  "Split view (suited for 2 scenarios to compare)" = "split",
  "Tab view (suited for > 2 scenarios to compare)" = "tab",
  "Pivot view (suited for > 2 scenarios to compare)" = "pivot"
)
names(langSpecificUI$scen) <- lang$adminMode$general$scen$choices
inputTabs <- c(
  inputSymMultiDim,
  setNames(
    "_widgets",
    lang$nav$inputScreen$widgetTabTitle
  )
)
inputTabs <- inputTabs[!inputTabs %in% inputWidgets]
customScalarInputWidgets <- names(modelIn)[vapply(modelIn, function(el) {
  if (identical(el$type, "custom")) {
    return(TRUE)
  }
  return(FALSE)
}, logical(1L), USE.NAMES = FALSE)]
inputTabs <- c(inputTabs, customScalarInputWidgets)
if (length(configJSON$inputWidgetGroups)) {
  inputTabs <- c(
    inputTabs,
    setNames(
      paste0("_widgets", seq_along(configJSON$inputWidgetGroups)),
      vapply(configJSON$inputWidgetGroups,
        "[[", character(1L),
        "name",
        USE.NAMES = FALSE
      )
    )
  )
}
if (length(configJSON$overwriteSheetOrder$input)) {
  tabIdsTmp <- match(configJSON$overwriteSheetOrder$input, inputTabs)
  if (all(is.na(tabIdsTmp))) {
    flog.info("No valid input symbols in 'overwriteSheetOrder' found. Resetting to original sheet order.")
  } else {
    inputTabsTmp <- inputTabs[tabIdsTmp[!is.na(tabIdsTmp)]]
    inputTabs <- c(inputTabsTmp, inputTabs[!inputTabs %in% inputTabsTmp])
  }
}
outputTabs <- setNames(names(modelOut), modelOutAlias)
if (length(configJSON$overwriteSheetOrder$output)) {
  tabIdsTmp <- match(configJSON$overwriteSheetOrder$output, outputTabs)
  if (all(is.na(tabIdsTmp))) {
    flog.info("No valid output symbols in 'overwriteSheetOrder' found. Resetting to original sheet order.")
  } else {
    outputTabsTmp <- outputTabs[tabIdsTmp[!is.na(tabIdsTmp)]]
    outputTabs <- c(outputTabsTmp, outputTabs[!outputTabs %in% outputTabsTmp])
  }
}

header_admin <- dashboardHeader(
  tags$li(
    class = "dropdown",
    tags$a(
      href = "#", class = "dropdown-toggle", "data-toggle" = "dropdown",
      lang$nav$header$help$title, tags$span(class = "caret")
    ),
    tags$ul(
      class = "dropdown-menu", role = "menu",
      tags$li(tags$a(
        href = "https://www.gams.com/miro/",
        target = "_blank", lang$nav$header$help$doc
      )),
      tags$li(tags$a(
        href = "https://forum.gams.com/c/gams-miro/12",
        target = "_blank", lang$nav$header$help$forum
      )),
      tags$li(
        tags$a(
          hred = "#",
          class = "action-button",
          onclick = paste0(
            "Miro.confirmModalShow('About GAMS MIRO','",
            aboutDialogText,
            "', 'Cancel');"
          ),
          lang$nav$header$help$about
        )
      )
    )
  ),
  title = paste0(lang$adminMode$uiR$configMode, " (", modelName, ")"), disable = FALSE
)

sidebar_admin <- dashboardSidebar(
  sidebarMenu(
    id = "miroSidebar",
    menuItem(lang$adminMode$uiR$general, tabName = "new_gen", icon = icon("gear")),
    menuItem(lang$adminMode$uiR$symbol, tabName = "symbol_conf", icon = icon("pen-to-square")),
    menuItem(lang$adminMode$uiR$table, tabName = "tables_gen", icon = icon("table")),
    menuItem(lang$adminMode$uiR$widgets, tabName = "new_widget", icon = icon("sliders")),
    menuItem(lang$adminMode$uiR$graphs, tabName = "new_graph", icon = icon("chart-bar")),
    menuItem(lang$adminMode$uiR$colors, tabName = "colors", icon = icon("palette")),
    menuItem(lang$adminMode$uiR$database, tabName = "db_management", icon = icon("database"))
  )
)


body_admin <- dashboardBody({
  if (dir.exists(paste0(currentModelDir, .Platform$file.sep, "static_", modelName))) {
    addResourcePath(paste0("static_", modelName), paste0(
      currentModelDir, .Platform$file.sep,
      "static_", modelName
    ))
  }
  tagList(
    tags$head(
      tags$meta(
        name = "color-scheme",
        content = if (identical(config$theme, "browser")) "dark light" else "normal"
      ),
      if (!is.null(config$themeColors) && length(config$themeColors)) {
        cssLines <- sprintf(
          "--%s:%s;",
          gsub("_", "-", names(config$themeColors), fixed = TRUE),
          unname(config$themeColors)
        )
        htmltools::tags$style(
          HTML(paste0(":root{", paste(cssLines, collapse = ""), "}"))
        )
      } else if (identical(miroColorTheme, "custom")) {
        customColorCss <- read_file(file.path(miroWorkspace, "colors_custom.css"))
        htmltools::tags$style(
          HTML(customColorCss)
        )
      } else {
        tags$link(type = "text/css", rel = "stylesheet", href = paste0("colors_", miroColorTheme, ".css"))
      },
      tags$link(type = "text/css", rel = "stylesheet", href = if (identical(config$customColorTheme, TRUE)) {
        paste0("static_", modelName, "/custom_theme.css")
      } else {
        paste0("default_", config$theme, ".css")
      }),
      tags$script(`defer src` = "showdown.min.js", type = "application/javascript"),
      tags$script(`defer src` = "mathjax-extension.js", type = "application/javascript"),
      tags$script(`defer src` = "miro_admin.js", type = "application/javascript"),
      tags$link(type = "text/css", rel = "stylesheet", href = "katex.min.css"),
      tags$script(type = "application/javascript", `defer src` = "katex.min.js"),
      tags$script(type = "application/javascript", `defer src` = "auto-render.min.js"),
      tags$style(HTML(paste0('
.main-header .logo {
                             background-image: url("gams_logo.png");
}
.shiny-output-error, .shiny-output-error:before{
visibility:visible;
}
.custom-renderer-boilerplate {
white-space:pre-wrap;
font-family: Menlo,Monaco,Consolas,Courier New,monospace;
font-size: 12px;
}')))
    ),
    HTML('<!-- Creates modal dialog for confirm messages -->
       <div class="modal fade" id="confirmModal" tabindex="-1" role="dialog" aria-labelledby="myModalLabel" aria-hidden="true">
       <div class="modal-dialog">
       <div class="modal-content">
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
      tabItem(
        tabName = "db_management",
        fluidRow(
          box(
            title = lang$adminMode$database$title, status = "primary", solidHeader = TRUE, width = 12,
            tags$div(id = "removeSuccess", class = "gmsalert gmsalert-success center-alert"),
            tags$div(
              id = "unknownError", class = "gmsalert gmsalert-error center-alert",
              lang$errMsg$unknownError
            ),
            tags$div(class = "space"),
            if (identical(Sys.getenv("MIRO_DB_TYPE"), "postgres")) {
              tags$h4(HTML(sprintf(
                lang$adminMode$database$infoPostgres,
                paste0("<code>", db$getInfo()$name, "</code>"),
                paste0("<code>", db$getInfo()$loc, "</code>")
              )))
            } else {
              tagList(
                tags$h4(HTML(sprintf(
                  lang$adminMode$database$infoSQLite,
                  paste0("<code>", db$getInfo()$loc, "</code>")
                ))),
                tags$hr(),
                tags$div(class = "space"),
                tags$label("for" = "db_backup_wrapper", lang$adminMode$database$backup),
                tags$div(id = "db_backup_wrapper", lang$adminMode$database$backupWrapper),
                downloadButton("btDownloadBackup", label = lang$adminMode$database$btDownloadBackup),
                tags$div(class = "space")
              )
            },
            tags$hr(),
            tags$div(class = "space"),
            tags$label("for" = "db_remove_wrapper", lang$adminMode$database$remove),
            tags$div(
              id = "db_remove_wrapper", lang$adminMode$database$removeWrapper,
              tags$div(removeDbTablesButton("removeAllButton"))
            )
          )
        )
      ),
      tabItem(
        tabName = "new_graph",
        fluidRow(
          box(
            title = lang$adminMode$graphs$ui$title, status = "primary", solidHeader = TRUE, width = 12, id = "config-main-tab-graph",
            tags$div(id = "graphUpdateSuccess", class = "gmsalert gmsalert-success center-alert", lang$adminMode$graphs$ui$graphUpdateSuccess),
            tags$div(id = "graphValidationErr", class = "gmsalert gmsalert-error center-alert"),
            tags$div(
              id = "unknownErrorGraphs", class = "gmsalert gmsalert-error center-alert",
              lang$adminMode$graphs$ui$gamsSymbols
            ),
            fluidRow(
              tags$div(
                class = "col-sm-6",
                tags$h4(
                  id = "previewDataInputToggle", class = "box-title",
                  icon("minus"), style = "cursor:pointer;font-weight:bold;",
                  onclick = "Miro.slideToggleEl({id: '#previewDataInputWrapper',
                                              toggleIconDiv: '#previewDataInputToggle'})"
                )
              ),
              tags$div(
                class = "col-sm-6",
                tags$div(
                  class = "btn-group btn-group-right", role = "group",
                  tags$div(
                    title = lang$adminMode$graphs$ui$toggleLeft,
                    actionButton("toggleFullscreenRight", HTML("<i class='fas fa-edit'></i> <i class='fas fa-expand'></i>"),
                      class = "toggle-fullscreen-btn toggle-config-view-right"
                    )
                  ),
                  tags$div(
                    title = lang$adminMode$graphs$ui$toggleRight,
                    actionButton("toggleFullscreenLeft", HTML("<i class='fas fa-expand'></i> <i class='fas fa-chart-pie'></i>"),
                      class = "toggle-fullscreen-btn toggle-config-view-left"
                    )
                  )
                )
              )
            ),
            tags$div(
              class = "col-sm-6", id = "config-left-graph",
              tags$div(
                id = "previewDataInputWrapper",
                tabsetPanel(
                  tabPanel(
                    lang$nav$dialogImport$tabDatabase,
                    tags$div(class = "space"),
                    tags$div(id = "noDbScen", lang$nav$dialogLoadScen$descNoScen),
                    tags$div(
                      id = "dbScen",
                      selectizeInput("scenList", lang$nav$dialogLoadScen$selLoadScen,
                        c(),
                        multiple = FALSE, width = "100%",
                        options = list(dropdownParent = "body")
                      ),
                      actionButton("dbInput", lang$nav$dialogLoadScen$okButton),
                      tags$div(class = "space")
                    )
                  ),
                  tabPanel(
                    lang$nav$dialogImport$tabLocal,
                    tags$div(class = "space"),
                    fileInput("localInput", lang$adminMode$graphs$ui$localInput,
                      width = "100%",
                      multiple = FALSE,
                      accept = c(
                        "application/vnd.ms-excel",
                        "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                        ".xlsx", ".xls", ".xlsm", ".gdx"
                      )
                    )
                  )
                )
              ),
              tags$div(
                id = "preview_wrapper", style = "display:none;",
                tags$div(
                  class = "two-col-wrapper",
                  tags$div(
                    class = "two-col-left",
                    selectInput("gams_symbols", lang$adminMode$graphs$ui$gamsSymbols,
                      choices = NULL
                    )
                  ),
                  tags$div(
                    class = "two-col-right",
                    selectInput("chart_tool", lang$adminMode$graphs$ui$tool, choices = c())
                  )
                ),
                tags$hr(),
                tags$div(
                  class = "side-tab col-sm-3",
                  tags$div(class = "side-tab-item"),
                  tags$div(
                    id = "toolCategories", class = "tool-categories-wrapper",
                    tags$ul(
                      class = "category-list",
                      # pie
                      tags$li(
                        id = "categoryPie1", class = "category-btn category-btn-pie category-btn-active", `data-cat` = "1",
                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$main)
                      ),
                      tags$li(
                        id = "categoryPie2", class = "category-btn category-btn-pie", `data-cat` = "2",
                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$general)
                      ),
                      # bar
                      tags$li(
                        id = "categoryBar1", class = "category-btn category-btn-bar", `data-cat` = "3",
                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$xaxis)
                      ),
                      tags$li(
                        id = "categoryBar6", class = "category-btn category-btn-bar", `data-cat` = "50",
                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$ydata)
                      ),
                      tags$li(
                        id = "categoryBar2", class = "category-btn category-btn-bar", `data-cat` = "4",
                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$yaxis)
                      ),
                      tags$li(
                        id = "categoryBar5", class = "category-btn category-btn-bar", `data-cat` = "7",
                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$bars)
                      ),
                      tags$li(
                        id = "categoryBar3", class = "category-btn category-btn-bar", `data-cat` = "5",
                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$filterDomain)
                      ),
                      tags$li(
                        id = "categoryBar4", class = "category-btn category-btn-bar", `data-cat` = "6",
                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$general)
                      ),
                      # scatter
                      tags$li(
                        id = "categoryScatter1", class = "category-btn category-btn-scatter", `data-cat` = "8",
                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$xaxis)
                      ),
                      tags$li(
                        id = "categoryScatter6", class = "category-btn category-btn-scatter", `data-cat` = "51",
                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$ydata)
                      ),
                      tags$li(
                        id = "categoryScatter2", class = "category-btn category-btn-scatter", `data-cat` = "9",
                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$yaxis)
                      ),
                      tags$li(
                        id = "categoryScatter3", class = "category-btn category-btn-scatter", `data-cat` = "10",
                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$filterDomain)
                      ),
                      tags$li(
                        id = "categoryScatter4", class = "category-btn category-btn-scatter", `data-cat` = "11",
                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$animation),
                        title = lang$adminMode$graphs$animationOptions$title
                      ),
                      tags$li(
                        id = "categoryScatter5", class = "category-btn category-btn-scatter", `data-cat` = "12",
                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$general)
                      ),
                      # line
                      tags$li(
                        id = "categoryLine1", class = "category-btn category-btn-line", `data-cat` = "13",
                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$xaxis)
                      ),
                      tags$li(
                        id = "categoryLine6", class = "category-btn category-btn-line", `data-cat` = "52",
                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$ydata)
                      ),
                      tags$li(
                        id = "categoryLine2", class = "category-btn category-btn-line", `data-cat` = "14",
                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$yaxis)
                      ),
                      tags$li(
                        id = "categoryLine3", class = "category-btn category-btn-line", `data-cat` = "15",
                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$filterDomain)
                      ),
                      tags$li(
                        id = "categoryLine4", class = "category-btn category-btn-line", `data-cat` = "16",
                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$animation),
                        title = lang$adminMode$graphs$animationOptions$title
                      ),
                      tags$li(
                        id = "categoryLine5", class = "category-btn category-btn-line", `data-cat` = "17",
                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$general)
                      ),
                      # bubble
                      tags$li(
                        id = "categoryBubble1", class = "category-btn category-btn-bubble", `data-cat` = "18",
                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$xaxis)
                      ),
                      tags$li(
                        id = "categoryBubble6", class = "category-btn category-btn-bubble", `data-cat` = "53",
                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$ydata)
                      ),
                      tags$li(
                        id = "categoryBubble2", class = "category-btn category-btn-bubble", `data-cat` = "19",
                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$yaxis)
                      ),
                      tags$li(
                        id = "categoryBubble3", class = "category-btn category-btn-bubble", `data-cat` = "20",
                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$filterDomain)
                      ),
                      tags$li(
                        id = "categoryBubble4", class = "category-btn category-btn-bubble", `data-cat` = "21",
                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$animation),
                        title = lang$adminMode$graphs$animationOptions$title
                      ),
                      tags$li(
                        id = "categoryBubble5", class = "category-btn category-btn-bubble", `data-cat` = "22",
                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$general)
                      ),
                      # hist
                      tags$li(
                        id = "categoryHist1", class = "category-btn category-btn-hist", `data-cat` = "23",
                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$data)
                      ),
                      tags$li(
                        id = "categoryHist2", class = "category-btn category-btn-hist", `data-cat` = "24",
                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$histogram)
                      ),
                      tags$li(
                        id = "categoryHist2", class = "category-btn category-btn-hist", `data-cat` = "25",
                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$filterDomain)
                      ),
                      tags$li(
                        id = "categoryHist2", class = "category-btn category-btn-hist", `data-cat` = "26",
                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$general)
                      ),
                      # dygraphs
                      tags$li(
                        id = "categoryDygraphs1", class = "category-btn category-btn-dygraphs", `data-cat` = "27",
                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$xaxis)
                      ),
                      tags$li(
                        id = "categoryDygraphs2", class = "category-btn category-btn-dygraphs", `data-cat` = "28",
                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$ydata)
                      ),
                      tags$li(
                        id = "categoryDygraphs3", class = "category-btn category-btn-dygraphs", `data-cat` = "54",
                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$yaxis)
                      ),
                      tags$li(
                        id = "categoryDygraphs4", class = "category-btn category-btn-dygraphs", `data-cat` = "29",
                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$filter)
                      ),
                      tags$li(
                        id = "categoryDygraphs5", class = "category-btn category-btn-dygraphs", `data-cat` = "30",
                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$event)
                      ),
                      tags$li(
                        id = "categoryDygraphs6", class = "category-btn category-btn-dygraphs", `data-cat` = "31",
                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$limit)
                      ),
                      tags$li(
                        id = "categoryDygraphs7", class = "category-btn category-btn-dygraphs", `data-cat` = "32",
                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$annotation)
                      ),
                      tags$li(
                        id = "categoryDygraphs8", class = "category-btn category-btn-dygraphs", `data-cat` = "33",
                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$shading)
                      ),
                      tags$li(
                        id = "categoryDygraphs9", class = "category-btn category-btn-dygraphs", `data-cat` = "34",
                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$rangeSelector)
                      ),
                      tags$li(
                        id = "categoryDygraphs10", class = "category-btn category-btn-dygraphs", `data-cat` = "35",
                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$highlight)
                      ),
                      tags$li(
                        id = "categoryDygraphs12", class = "category-btn category-btn-dygraphs", `data-cat` = "55",
                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$legend)
                      ),
                      tags$li(
                        id = "categoryDygraphs11", class = "category-btn category-btn-dygraphs", `data-cat` = "36",
                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$general)
                      ),
                      # leaflet
                      tags$li(
                        id = "categoryLeaflet1", class = "category-btn category-btn-leaflet", `data-cat` = "37",
                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$markers)
                      ),
                      tags$li(
                        id = "categoryLeaflet1", class = "category-btn category-btn-leaflet", `data-cat` = "38",
                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$flows)
                      ),
                      tags$li(
                        id = "categoryLeaflet1", class = "category-btn category-btn-leaflet", `data-cat` = "39",
                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$minicharts)
                      ),
                      tags$li(
                        id = "categoryLeaflet1", class = "category-btn category-btn-leaflet", `data-cat` = "40",
                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$groupsLayers)
                      ),
                      tags$li(
                        id = "categoryLeaflet1", class = "category-btn category-btn-leaflet", `data-cat` = "41",
                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$filter)
                      ),
                      tags$li(
                        id = "categoryLeaflet1", class = "category-btn category-btn-leaflet", `data-cat` = "42",
                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$general)
                      ),
                      # timevis
                      tags$li(
                        id = "categoryTimevis1", class = "category-btn category-btn-timevis", `data-cat` = "43",
                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$data)
                      ),
                      tags$li(
                        id = "categoryTimevis1", class = "category-btn category-btn-timevis", `data-cat` = "44",
                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$filter)
                      ),
                      tags$li(
                        id = "categoryTimevis1", class = "category-btn category-btn-timevis", `data-cat` = "45",
                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$general)
                      ),
                      # pivot
                      tags$li(
                        id = "categoryPivot1", class = "category-btn category-btn-pivot", `data-cat` = "46",
                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$data)
                      ),
                      tags$li(
                        id = "categoryPivot1", class = "category-btn category-btn-pivot", `data-cat` = "47",
                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$general)
                      ),
                      # custom
                      tags$li(
                        id = "categoryCustom1", class = "category-btn category-btn-custom", `data-cat` = "48",
                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$main)
                      ),
                      tags$li(
                        id = "categoryCustom2", class = "category-btn category-btn-custom", `data-cat` = "48a",
                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$advanced)
                      ),
                      # valuebox
                      tags$li(
                        id = "categoryValuebox1", class = "category-btn category-btn-valuebox", `data-cat` = "49",
                        tags$div(class = "side-tab-item", lang$adminMode$graphs$toolCategories$main)
                      )
                    )
                  ),
                  tags$div(
                    class = "save-delete-wrapper",
                    actionButton("deleteGraph", lang$adminMode$graphs$ui$deleteGraph, icon("trash-can"), class = "save-delete-delete-btn full-width"),
                    actionButton("saveGraph", lang$adminMode$graphs$ui$saveGraph, icon("floppy-disk"), class = "save-delete-save-btn full-width")
                  )
                ),
                tags$div(
                  class = "main-tab", style = "padding: 7px; max-height:66vh",
                  tags$div(id = "tool_options"),
                  tags$div(class = "space")
                )
              )
            ),
            tags$div(
              class = "col-sm-6 preview-outer-wrapper", id = "config-right-graph",
              tags$div(
                style = "text-align: left;",
                uiOutput("rendererLabelWrapper")
              ),
              tags$div(id = "preview-error", class = "err-msg"),
              tags$div(
                id = "preview-content-plotly", style = "overflow: auto;",
                renderDataUI("preview_output_plotly",
                  type = "graph",
                  graphTool = "plotly",
                  filterOptions = list(
                    label = NULL,
                    multiple = TRUE,
                    col = "a"
                  ),
                  height = 400
                )
              ),
              tags$div(id = "pieValues", class = "config-message", lang$adminMode$graphs$validate$pieValues),
              tags$div(
                id = "preview-content-dygraphs", style = "display:none;",
                renderDataUI("preview_output_dygraphs",
                  type = "graph",
                  graphTool = "dygraphs",
                  filterOptions = list(
                    label = NULL,
                    multiple = TRUE,
                    col = "a"
                  ),
                  height = 400
                )
              ),
              tags$div(
                id = "preview-content-leaflet", style = "display:none;",
                renderDataUI("preview_output_leaflet",
                  type = "graph",
                  graphTool = "leaflet",
                  filterOptions = list(
                    label = NULL,
                    multiple = TRUE,
                    col = "a"
                  ),
                  height = 400
                )
              ),
              tags$div(
                id = "preview-content-miropivot", style = "display:none; overflow:auto;text-align:left;",
                tags$div(
                  style = "display:none",
                  tags$input(id = "preview_output_miropivot-miroPivot-symbol_name", type = "text")
                ),
                renderDataUI("preview_output_miropivot",
                  type = "miropivot",
                  height = 400, customOptions = list()
                )
              ),
              tags$div(
                id = "preview-content-timevis", style = "display:none; overflow:auto;",
                renderDataUI("preview_output_timevis",
                  type = "graph",
                  graphTool = "timevis",
                  filterOptions = list(
                    label = NULL,
                    multiple = TRUE,
                    col = "a"
                  ),
                  height = 400
                )
              ),
              tags$div(
                id = "preview-content-custom", style = "display:none; overflow:auto;text-align:left;",
                tags$button(
                  class = "btn btn-default", type = "button",
                  onclick = "Shiny.setInputValue('btUpdateCustomRendererOutput',1,{priority:'event'});",
                  lang$adminMode$graphs$customOptions$btUpdate
                ),
                tags$div(
                  style = "display:none",
                  tags$input(id = "preview_output_custom-symbol_name", type = "text")
                ),
                uiOutput("preview_custom_renderer")
              ),
              if (scalarsOutName %in% names(modelOut)) {
                tags$div(
                  id = "preview-content-valuebox", style = "display:none;text-align:left",
                  renderDataUI("preview_output_valuebox",
                    type = "valuebox",
                    height = 400, customOptions = list(count = modelOut[[scalarsOutName]]$count)
                  )
                )
              }
            )
          )
        )
      ),
      tabItem(
        tabName = "new_widget",
        fluidRow(
          box(
            title = lang$adminMode$widgets$ui$title, status = "primary", solidHeader = TRUE, width = 12,
            tags$div(id = "widgetUpdateSuccess", class = "gmsalert gmsalert-success center-alert", lang$adminMode$widgets$ui$widgetUpdateSuccess),
            tags$div(id = "widgetValidationErr", class = "gmsalert gmsalert-error center-alert"),
            tags$div(
              id = "unknownErrorWidgets", class = "gmsalert gmsalert-error center-alert",
              lang$errMsg$unknownError
            ),
            tags$div(class = "space"),
            tags$div(
              class = "main-tab",
              tags$div(
                style = "padding-bottom: 20px;",
                tabsetPanel(
                  id = "widget_symbol_type",
                  tabPanel(lang$adminMode$widgets$ui$gams, value = "gams"),
                  tabPanel(lang$adminMode$widgets$ui$go, value = "go"),
                  tabPanel(lang$adminMode$widgets$ui$dd, value = "dd")
                )
              ),
              tags$div(
                class = "col-sm-6", id = "config-left-widget",
                tags$div(
                  id = "noSymbolMsg", class = "config-message",
                  lang$adminMode$widgets$ui$noSymbolMsg
                ),
                tags$div(
                  id = "noWidgetMsg", class = "config-message",
                  lang$adminMode$widgets$ui$noWidgetMsg
                ),
                tags$div(
                  id = "noWidgetConfigMsg", class = "config-message",
                  lang$adminMode$widgets$ui$noWidgetConfigMsg
                ),
                tags$div(id = "externalConfigMsg", class = "config-message"),
                tags$div(
                  class = "main-tab", style = "min-height: 600px;",
                  tags$div(
                    class = "row",
                    tags$div(
                      class = "col-sm-6 col-6-left",
                      conditionalPanel(
                        condition = "input.widget_symbol_type == 'gams'",
                        tags$div(
                          title = lang$adminMode$widgets$ui$inputSymbolTooltip,
                          selectInput("widget_symbol", lang$adminMode$widgets$ui$inputSymbol,
                            choices = c()
                          )
                        )
                      ),
                      conditionalPanel(
                        condition = "input.widget_symbol_type == 'go'",
                        tags$div(
                          title = lang$adminMode$widgets$ui$widgetGoTooltip,
                          textInput("widget_go", lang$adminMode$widgets$ui$widgetGo)
                        )
                      ),
                      conditionalPanel(
                        condition = "input.widget_symbol_type == 'dd'",
                        tags$div(
                          title = lang$adminMode$widgets$ui$widgetDdTooltip,
                          textInput("widget_dd", lang$adminMode$widgets$ui$widgetDd)
                        )
                      )
                    ),
                    tags$div(
                      class = "col-sm-6 col-6-right",
                      selectInput("widget_type", lang$adminMode$widgets$ui$widgetType, choices = c())
                    )
                  ),
                  tags$hr(),
                  tags$div(id = "widget_options"),
                  tags$div(
                    id = "pivotColsRestriction", class = "config-message",
                    lang$adminMode$widgets$ui$pivotColsRestriction
                  ),
                  tags$div(class = "space")
                )
              ),
              tags$div(
                class = "col-sm-6", id = "config-right-widget",
                tags$div(
                  style = "margin-bottom:50px;text-align:right;",
                  actionButton("deleteWidget", lang$adminMode$graphs$ui$deleteGraph, icon("trash-can"),
                    class = "save-delete-delete-btn",
                    style = "width:100px;"
                  ),
                  actionButton("saveWidget", lang$adminMode$graphs$ui$saveGraph, icon("floppy-disk"),
                    class = "save-delete-save-btn",
                    style = "width:100px;"
                  )
                ),
                uiOutput("widgetTableLabelWrapper"),
                uiOutput("widget_preview"),
                DTOutput("bigdata_preview"),
                tags$div(
                  style = "display:none",
                  tags$input(id = "preview_inputTable_pivot-miroPivot-symbol_name", type = "text")
                ),
                renderDataUI("preview_inputTable_pivot",
                  type = "miropivot", height = 400,
                  showNoDataTxt = FALSE,
                  customOptions = list(
                    "_input_" = TRUE
                  )
                ),
                rHandsontableOutput("hot_preview")
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "new_gen",
        fluidRow(
          box(
            title = lang$adminMode$general$ui$title, status = "primary", solidHeader = TRUE, width = 12,
            tags$div(class = "space"),
            tabsetPanel(
              tabPanel(
                lang$adminMode$general$ui$tabInterface,
                tags$div(
                  class = "col-sm-6", style = "padding-top: 20px;",
                  tags$div(
                    class = "main-tab",
                    tags$div(tagList(
                      tags$h2(lang$adminMode$general$ui$headerGeneral, class = "option-category"),
                      tags$div(
                        class = "option-wrapper",
                        textInput("general_pageTitle", labelTooltip(
                          lang$adminMode$general$pageTitle$label,
                          lang$adminMode$general$pageTitle$tooltip,
                          "https://gams.com/miro/configuration_general.html#application-title"
                        ),
                        value = if (!is.null(configJSON$pageTitle) && nchar(configJSON$pageTitle)) configJSON$pageTitle else configJSON$modelTitle
                        )
                      ),
                      tags$div(
                        class = "option-wrapper", style = "margin-bottom: 5px;",
                        fileInput("widget_general_logo_upload", labelTooltip(
                          lang$adminMode$general$logo$label,
                          lang$adminMode$general$logo$tooltip,
                          "https://gams.com/miro/configuration_general.html#app-logo"
                        ),
                        width = "100%",
                        multiple = FALSE,
                        accept = c(".png", ".PNG", ".jpg", ".JPG"),
                        placeholder = lang$adminMode$general$logo$placeholder
                        )
                      ),
                      tags$label(
                        class = "cb-label", "for" = "general_logo_preview", lang$adminMode$general$logo$header,
                        tags$div(
                          class = "logo-wrapper",
                          imageOutput("general_logo_preview", height = "50px")
                        )
                      ),
                      tags$div(class = "space"),
                      tags$div(class = "space"),
                      tags$h2(lang$adminMode$general$ui$headerAppearance, class = "option-category"),
                      tags$h4(lang$adminMode$general$themeColors$light),
                      tags$div(
                        class = "option-wrapper",
                        selectInput("general_theme", labelTooltip(
                          lang$adminMode$general$theme$label,
                          lang$adminMode$general$ui$tooltipDocs,
                          "https://gams.com/miro/configuration_general.html#dark-mode"
                        ),
                        choices = langSpecificUI$theme,
                        selected = if (length(configJSON$theme)) configJSON$theme else config$theme
                        )
                      ),
                      tags$div(
                        checkboxInput_SIMPLE("general_customCss",
                          labelTooltip(
                            lang$adminMode$general$customCss$label,
                            lang$adminMode$general$customCss$tooltip,
                            "https://gams.com/miro/configuration_general.html#custom-css"
                          ),
                          value = identical(configJSON$customCss, TRUE)
                        )
                      ),
                      tags$b(lang$adminMode$general$maxTabsExpanded$title),
                      fluidRow(
                        tags$div(
                          class = "col-sm-6",
                          numericInput(
                            "general_maxTabsExpandedInput",
                            lang$nav$sidebarMenu$inputScreen,
                            5L,
                            min = 1L,
                            step = 1L,
                            value = if (is.null(configJSON$layoutSettings$maxTabsExpandedInput)) 5L else configJSON$layoutSettings$maxTabsExpandedInput
                          )
                        ),
                        tags$div(
                          class = "col-sm-6",
                          numericInput(
                            "general_maxTabsExpandedOutput",
                            lang$nav$sidebarMenu$outputScreen,
                            5L,
                            min = 1L,
                            step = 1L,
                            value = if (is.null(configJSON$layoutSettings$maxTabsExpandedOutput)) 5L else configJSON$layoutSettings$maxTabsExpandedOutput
                          )
                        ),
                        tags$div(
                          class = "col-sm-6",
                          numericInput(
                            "general_maxTabsExpandedTabComp",
                            lang$adminMode$general$maxTabsExpanded$tabComp,
                            5L,
                            min = 1L,
                            step = 1L,
                            value = if (is.null(configJSON$layoutSettings$maxTabsExpandedTabComp)) 5L else configJSON$layoutSettings$maxTabsExpandedTabComp
                          )
                        ),
                        tags$div(
                          class = "col-sm-6",
                          numericInput(
                            "general_maxTabsExpandedSplitComp",
                            lang$adminMode$general$maxTabsExpanded$splitComp,
                            5L,
                            min = 1L,
                            step = 1L,
                            value = if (is.null(configJSON$layoutSettings$maxTabsExpandedSplitComp)) 5L else configJSON$layoutSettings$maxTabsExpandedSplitComp
                          )
                        ),
                        tags$div(
                          class = "col-sm-6",
                          numericInput(
                            "general_maxTabsExpandedPivotComp",
                            lang$adminMode$general$maxTabsExpanded$pivotComp,
                            5L,
                            min = 1L,
                            step = 1L,
                            value = if (is.null(configJSON$layoutSettings$maxTabsExpandedPivotComp)) 5L else configJSON$layoutSettings$maxTabsExpandedPivotComp
                          )
                        )
                      )
                    )),
                    tags$div(class = "space")
                  )
                ),
                tags$div(
                  class = "col-sm-6", style = "padding-top: 20px;",
                  tags$div(
                    class = "main-tab",
                    tags$div(tagList(
                      tags$h2(lang$adminMode$general$readme$label,
                        tags$a("",
                          title = paste0(
                            lang$adminMode$general$readme$readmeTooltip, " - ",
                            tolower(lang$adminMode$general$ui$tooltipDocs)
                          ), class = "info-header",
                          href = "https://gams.com/miro/configuration_general.html#app-readme",
                          tags$span(
                            class = "fas fa-circle-info", class = "info-icon",
                            role = "presentation",
                            `aria-label` = "More information"
                          ), target = "_blank"
                        ),
                        class = "option-category info-position"
                      ),
                      checkboxInput_SIMPLE("general_useReadme",
                        lang$adminMode$general$readme$useReadme,
                        value = length(configJSON$readme$filename) > 0L
                      ),
                      conditionalPanel(
                        condition = "input.general_useReadme===true",
                        tags$div(
                          class = "option-wrapper option-wrapper-indented", style = "padding-left:25px;",
                          textInput("general_readmeTabtitle", lang$adminMode$general$readme$tabTitle,
                            value = if (!is.null(configJSON$readme$tabTitle) && nchar(configJSON$readme$tabTitle)) {
                              configJSON$readme$tabTitle
                            } else {
                              ""
                            }
                          )
                        ),
                        tags$div(
                          class = "option-wrapper info-position option-wrapper-indented", style = "padding-left:25px;",
                          textInput("general_readmeFileName", lang$adminMode$general$readme$fileName,
                            value = if (!is.null(configJSON$readme$filename) && nchar(configJSON$readme$filename)) {
                              configJSON$readme$filename
                            } else {
                              ""
                            }
                          )
                        ),
                        tags$div(
                          class = "option-wrapper info-position option-wrapper-indented", style = "padding-left:25px;",
                          checkboxInput_SIMPLE(
                            "general_readmeEnableMath", lang$adminMode$general$readme$enableMath,
                            isTRUE(configJSON$readme$enableMath)
                          )
                        ),
                        tags$div(class = "option-wrapper info-position option-wrapper-indented", style = "padding-left:25px;", {
                          editButtonArgs <- list(
                            inputId = "btEditReadme",
                            label = lang$adminMode$general$readme$btEdit
                          )
                          if (!length(configJSON$readme$filename) ||
                            !nchar(trimws(configJSON$readme$filename))) {
                            editButtonArgs$disabled <- ""
                          }
                          do.call("actionButton", editButtonArgs)
                        })
                      ),
                      tags$div(class = "space"),
                      tags$div(class = "space"),
                      tags$h2(lang$adminMode$general$ui$headerRenderers, class = "option-category"),
                      tags$div(
                        class = "option-wrapper",
                        checkboxInput_SIMPLE("general_auto",
                          lang$adminMode$general$auto$label,
                          value = if (length(configJSON$autoGenInputGraphs)) {
                            configJSON$autoGenInputGraphs
                          } else {
                            config$autoGenInputGraphs
                          }
                        )
                      ),
                      tags$div(
                        class = "option-wrapper", style = "margin-bottom: 5px;",
                        selectInput("general_defaultRendererOutput", lang$adminMode$general$defaultRenderer$labelOutput,
                          setNames(c("miroPivot", "datatable"), lang$adminMode$general$defaultRenderer$choicesOutput),
                          selected = configJSON$defaultRendererOutput
                        )
                      )
                    )),
                    tags$div(class = "space")
                  )
                )
              ),
              tabPanel(
                lang$adminMode$general$ui$tabData,
                tags$div(
                  class = "col-sm-6", style = "padding-top: 20px;",
                  tags$div(
                    class = "main-tab",
                    tags$h2(lang$adminMode$general$ui$headerImport, class = "option-category"),
                    checkboxInput_SIMPLE("default_scen_check",
                      labelTooltip(
                        lang$adminMode$general$defaultScenName$checkbox,
                        paste0(
                          lang$adminMode$general$defaultScenName$tooltip, " - ",
                          tolower(lang$adminMode$general$ui$tooltipDocs)
                        ),
                        "https://gams.com/miro/configuration_general.html#default-scenario"
                      ),
                      value = if (length(configJSON$defaultScenName) &&
                        nchar(configJSON$defaultScenName)) {
                        TRUE
                      } else {
                        FALSE
                      }
                    ),
                    conditionalPanel(
                      condition = "input.default_scen_check===true",
                      tags$div(
                        class = "option-wrapper", style = "margin-top:-10px;padding-left: 25px;",
                        textInput("general_default_scen_name", lang$adminMode$general$defaultScenName$label,
                          value = if (length(configJSON$defaultScenName)) configJSON$defaultScenName else "default"
                        )
                      )
                    ),
                    checkboxInput_SIMPLE("general_act_upload",
                      labelTooltip(
                        lang$adminMode$general$actUpload$label,
                        paste0(
                          lang$adminMode$general$actUpload$title, " - ",
                          tolower(lang$adminMode$general$ui$tooltipDocs)
                        ),
                        "https://gams.com/miro/configuration_general.html#local-upload"
                      ),
                      value = if (length(configJSON$activateModules$loadLocal)) {
                        configJSON$activateModules$loadLocal
                      } else {
                        config$activateModules$loadLocal
                      }
                    ),
                    checkboxInput_SIMPLE("general_act_attach",
                      labelTooltip(
                        lang$adminMode$general$actAttach$label,
                        paste0(
                          lang$adminMode$general$actAttach$title, " - ",
                          tolower(lang$adminMode$general$ui$tooltipDocs)
                        ),
                        "https://gams.com/miro/start.html#file-attachment"
                      ),
                      value = if (length(configJSON$activateModules$attachments)) {
                        configJSON$activateModules$attachments
                      } else {
                        config$activateModules$attachments
                      }
                    ),
                    tags$div(class = "space"),
                    tags$div(
                      style = "max-width: 500px;",
                      tags$h4(
                        lang$adminMode$general$ui$headerOutputAttach,
                        tags$a("",
                          title = lang$adminMode$general$ui$tooltipDocs,
                          class = "info-wrapper",
                          href = "https://gams.com/miro/configuration_general.html#general-output-attach",
                          tags$span(
                            class = "fas fa-circle-info", class = "info-icon",
                            role = "presentation",
                            `aria-label` = "More information"
                          ), target = "_blank"
                        )
                      ),
                      createArray(NULL, "general_output_attach",
                        lang$adminMode$general$outputAttach$label,
                        autoCreate = FALSE
                      )
                    ),
                    tags$div(class = "space")
                  )
                ),
                tags$div(
                  class = "col-sm-6", style = "padding-top: 20px;",
                  tags$div(
                    class = "main-tab",
                    tags$h2(lang$adminMode$general$ui$headerExport, class = "option-category"),
                    checkboxInput_SIMPLE("general_downloadTempFiles",
                      labelTooltip(
                        lang$adminMode$general$downloadTempFiles$label,
                        lang$adminMode$general$ui$tooltipDocs,
                        "https://gams.com/miro/configuration_general.html#general-temp-dir"
                      ),
                      value = if (length(configJSON$activateModules$downloadTempFiles)) {
                        configJSON$activateModules$downloadTempFiles
                      } else {
                        config$activateModules$downloadTempFiles
                      }
                    ),
                    checkboxInput_SIMPLE("general_meta",
                      labelTooltip(
                        lang$adminMode$general$meta$label,
                        paste0(
                          lang$adminMode$general$meta$title, " - ",
                          tolower(lang$adminMode$general$ui$tooltipDocs)
                        ),
                        "https://gams.com/miro/configuration_general.html#include-metadata"
                      ),
                      value = if (length(configJSON$excelIncludeMeta)) {
                        configJSON$excelIncludeMeta
                      } else {
                        config$excelIncludeMeta
                      }
                    ),
                    checkboxInput_SIMPLE("general_empty",
                      labelTooltip(
                        lang$adminMode$general$empty$label,
                        paste0(
                          lang$adminMode$general$empty$title, " - ",
                          tolower(lang$adminMode$general$ui$tooltipDocs)
                        ),
                        "https://gams.com/miro/configuration_general.html#include-empty"
                      ),
                      value = if (identical(configJSON$excelIncludeEmptySheets, FALSE)) {
                        FALSE
                      } else {
                        TRUE
                      }
                    ),
                    tags$hr(),
                    tags$div(
                      class = "option-wrapper",
                      sliderInput("general_decimal",
                        tags$div(
                          lang$adminMode$general$decimal$label,
                          tags$a("",
                            title = lang$adminMode$general$decimal$tooltip, class = "info-wrapper",
                            href = "https://www.gams.com/miro/configuration_general.html#decimal-places",
                            tags$span(
                              class = "fas fa-circle-info", class = "info-icon",
                              role = "presentation",
                              `aria-label` = "More information"
                            ), target = "_blank"
                          )
                        ),
                        min = 0, max = 6, step = 1, value = if (length(configJSON$roundingDecimals)) {
                          configJSON$roundingDecimals
                        } else {
                          config$roundingDecimals
                        }
                      )
                    ),
                    tags$div(class = "space")
                  )
                )
              ),
              tabPanel(
                lang$adminMode$general$ui$tabSubmission,
                tags$div(
                  class = "col-sm-12", style = "padding-top: 20px;",
                  tags$div(
                    class = "main-tab",
                    tags$div(tagList(
                      checkboxInput_SIMPLE("general_act_hcube",
                        labelTooltip(
                          lang$adminMode$general$hcubeModule$label,
                          lang$adminMode$general$hcubeModule$tooltip,
                          "https://gams.com/miro/configuration_general.html#activate-hcube"
                        ),
                        value = isTRUE(configJSON$activateModules$hcube)
                      ),
                      tags$div(
                        class = "option-wrapper",
                        tags$div(
                          id = "invalidClArgsError",
                          class = "err-msg",
                          sprintf(
                            lang$adminMode$widgets$validate[["val61"]],
                            paste(reservedGMSOpt,
                              collapse = "', '"
                            )
                          )
                        ),
                        selectizeInput("general_args",
                          tags$div(
                            lang$adminMode$general$args$label,
                            tags$a("",
                              title = lang$adminMode$general$ui$tooltipDocs,
                              class = "info-wrapper",
                              href = "https://gams.com/miro/configuration_general.html#command-line-args",
                              tags$span(
                                class = "fas fa-circle-info", class = "info-icon",
                                role = "presentation",
                                `aria-label` = "More information"
                              ), target = "_blank"
                            )
                          ),
                          choices = configJSON$extraClArgs, selected = configJSON$extraClArgs,
                          multiple = TRUE, options = list("create" = TRUE, "persist" = FALSE)
                        )
                      )
                    )),
                    tags$div(class = "space")
                  )
                )
              ),
              tabPanel(
                lang$adminMode$general$ui$tabLogs,
                tags$div(
                  class = "col-sm-12", style = "padding-top: 20px;",
                  tags$div(
                    class = "main-tab",
                    checkboxInput_SIMPLE("general_act_log",
                      lang$adminMode$general$actLog$label,
                      value = if (length(configJSON$activateModules$logFile)) {
                        configJSON$activateModules$logFile
                      } else {
                        config$activateModules$logFile
                      }
                    ),
                    conditionalPanel(
                      condition = "input.general_act_log == true",
                      checkboxInput_SIMPLE("general_mirologParsingStdout",
                        labelTooltip(
                          lang$adminMode$general$mirologStdout,
                          NULL,
                          "https://gams.com/miro/configuration_general.html#miro-log-syntax"
                        ),
                        value = identical(config$parseLogForMiroLogSyntax, TRUE)
                      )
                    ),
                    checkboxInput_SIMPLE("general_act_lst",
                      lang$adminMode$general$actLst$label,
                      value = if (length(configJSON$activateModules$lstFile)) {
                        configJSON$activateModules$lstFile
                      } else {
                        config$activateModules$lstFile
                      }
                    ),
                    tags$div(
                      class = "option-wrapper info-position",
                      conditionalPanel(
                        condition = "input.general_act_log===true && input.general_mirologParsingStdout===true && input.general_mirologfile?.length > 0",
                        tags$div(
                          class = "err-msg",
                          lang$adminMode$general$mirologfile$miroLogExclusiveError
                        )
                      ),
                      textInput("general_mirologfile",
                        labelTooltip(
                          lang$adminMode$general$mirologfile$label,
                          lang$adminMode$general$mirologfile$tooltip,
                          "https://gams.com/miro/configuration_general.html#miro-log"
                        ),
                        value = if (!is.null(configJSON$miroLogFile) && nchar(configJSON$miroLogFile)) {
                          configJSON$miroLogFile
                        } else {
                          ""
                        }
                      )
                    ),
                    tags$div(
                      class = "option-wrapper",
                      sliderInput("general_save_duration",
                        tags$div(
                          lang$adminMode$general$saveDuration$label,
                          tags$a("",
                            title = lang$adminMode$general$ui$tooltipDocs,
                            class = "info-wrapper",
                            href = "https://gams.com/miro/configuration_general.html#general-duration",
                            tags$span(
                              class = "fas fa-circle-info", class = "info-icon",
                              role = "presentation",
                              `aria-label` = "More information"
                            ), target = "_blank"
                          )
                        ),
                        min = 0, max = 999, step = 1,
                        value = if (length(configJSON$storeLogFilesDuration)) {
                          configJSON$storeLogFilesDuration
                        } else {
                          config$storeLogFilesDuration
                        }
                      )
                    ),
                    tags$div(class = "space")
                  )
                )
              ),
              tabPanel(
                lang$adminMode$general$ui$tabComparison,
                tags$div(
                  class = "col-sm-12", style = "padding-top: 20px;",
                  tags$div(
                    class = "main-tab",
                    tags$div(
                      class = "option-wrapper info-position",
                      selectInput("general_scen", tags$div(
                        lang$adminMode$general$scen$label,
                        tags$a("",
                          title = lang$adminMode$general$ui$tooltipDocs,
                          class = "info-wrapper",
                          href = "https://gams.com/miro/start.html#scenario-comparison",
                          tags$span(
                            class = "fas fa-circle-info", class = "info-icon",
                            role = "presentation",
                            `aria-label` = "More information"
                          ), target = "_blank"
                        )
                      ),
                      choices = c(
                        langSpecificUI$scen,
                        setNames(
                          vapply(config[["customCompareModules"]],
                            "[[", character(1L),
                            "id",
                            USE.NAMES = FALSE
                          ),
                          vapply(config[["customCompareModules"]],
                            "[[", character(1L), "label",
                            USE.NAMES = FALSE
                          )
                        )
                      ),
                      selected = if (length(configJSON$defCompMode)) configJSON$defCompMode else config$defCompMode
                      )
                    ),
                    tags$div(class = "space"),
                    tags$div(
                      class = "option-wrapper",
                      tags$h4(lang$adminMode$general$ui$headerPivotcompare, class = "option-category"),
                      tags$div(lang$adminMode$general$ui$descriptionPivotcompare),
                      getMIROPivotOptions(configJSON$pivotCompSettings,
                        prefix = "pivotcomp_",
                        pivotComp = TRUE
                      )
                    ),
                    tags$div(class = "space")
                  )
                )
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "symbol_conf",
        fluidRow(
          box(
            title = lang$adminMode$general$ui$title, status = "primary", solidHeader = TRUE, width = 12,
            tags$div(class = "space"),
            tabsetPanel(
              tabPanel(
                lang$adminMode$general$ui$headerSymbolNaming,
                tags$div(
                  class = "col-sm-6", style = "padding-top: 20px;",
                  tags$div(
                    class = "main-tab", style = "max-height: 70vh;",
                    tags$h2(
                      lang$adminMode$general$overwriteSymbolAliases$input,
                      tags$a(
                        title = lang$adminMode$general$ui$tooltipDocs, class = "info-wrapper", style = "top:-10px;", href = "https://gams.com/miro/configuration_symbols.html#naming",
                        tags$span(
                          class = "fas fa-circle-info", class = "info-icon",
                          role = "presentation",
                          `aria-label` = "More information"
                        ), target = "_blank"
                      )
                    ),
                    tags$div(class = "small-space"),
                    lapply(names(inputSymHeaders), function(name) {
                      if (name %in% names(configJSON$overwriteAliases)) {
                        symAlias <- configJSON$overwriteAliases[[name]]$newAlias
                      } else {
                        symAlias <- modelInRaw[[name]]$alias
                      }
                      if (!name %in% names(configJSON$overwriteHeaderAliases) ||
                        length(dataContract$inputSymbols[[name]]$headers) != length(inputSymHeaders[[name]])) {
                        symHeaders <- names(inputSymHeaders[[name]])
                      } else {
                        symHeaders <- configJSON$overwriteHeaderAliases[[name]]$newHeaders
                      }

                      tags$div(
                        column(6L, tags$div(paste0(
                          name, " (",
                          paste0(names(modelInRaw[[name]]$headers),
                            collapse = ", "
                          ), ")"
                        ))),
                        column(
                          6L,
                          textInput(
                            paste0("general_overwriteSymAlias_", name),
                            lang$adminMode$general$overwriteSymbolAliases$label,
                            symAlias
                          ),
                          tags$label(lang$adminMode$general$overwriteSymbolHeaders$label),
                          tags$div(
                            id = paste0("general_overwriteSymHeaders_", name),
                            class = "form-group shiny-input-container",
                            lapply(seq_along(symHeaders), function(hdrIdx) {
                              tags$input(
                                id = paste0("general_overwriteSymHeaders_", name, "_", hdrIdx),
                                type = "text", class = "form-control shiny-input-text",
                                style = "margin-bottom:3px;",
                                placeholder = modelInRaw[[name]]$headers[[hdrIdx]]$alias,
                                value = symHeaders[[hdrIdx]]
                              )
                            })
                          )
                        )
                      )
                    }),
                    lapply(seq_along(modelInRaw[[scalarsFileName]]$symnames), function(idx) {
                      name <- modelInRaw[[scalarsFileName]]$symnames[idx]
                      if (name %in% names(configJSON$overwriteAliases)) {
                        symAlias <- configJSON$overwriteAliases[[name]]$newAlias
                      } else {
                        symAlias <- modelInRaw[[scalarsFileName]]$symtext[idx]
                      }
                      tags$div(
                        column(6L, tags$div(name)),
                        column(
                          6L,
                          textInput(
                            paste0("general_overwriteSymAlias_", name),
                            lang$adminMode$general$overwriteSymbolAliases$label,
                            symAlias
                          )
                        )
                      )
                    }),
                    tags$div(class = "space")
                  )
                ),
                tags$div(
                  class = "col-sm-6", style = "padding-top: 20px;",
                  tags$div(
                    class = "main-tab", style = "max-height: 70vh;",
                    tags$h2(lang$adminMode$general$overwriteSymbolAliases$output),
                    tags$div(class = "small-space"),
                    lapply(names(modelOut), function(name) {
                      if (name %in% names(configJSON$overwriteAliases)) {
                        symAlias <- configJSON$overwriteAliases[[name]]$newAlias
                      } else {
                        symAlias <- modelOut[[name]]$alias
                      }
                      symHeaders <- configJSON$overwriteHeaderAliases[[name]]$newHeaders
                      if (!name %in% names(configJSON$overwriteHeaderAliases) ||
                        length(symHeaders) != length(dataContract$outputSymbols[[name]]$headers)) {
                        symHeaders <- outputSymHeaders[[name]]
                      }

                      tags$div(
                        column(6L, tags$div(paste0(
                          name, " (",
                          paste0(names(modelOut[[name]]$headers),
                            collapse = ", "
                          ), ")"
                        ))),
                        column(
                          6L,
                          textInput(
                            paste0("general_overwriteSymAlias_", name),
                            lang$adminMode$general$overwriteSymbolAliases$label,
                            symAlias
                          ),
                          tags$label(lang$adminMode$general$overwriteSymbolHeaders$label),
                          tags$div(
                            id = paste0("general_overwriteSymHeaders_", name),
                            class = "form-group shiny-input-container",
                            lapply(seq_along(symHeaders), function(hdrIdx) {
                              tags$input(
                                id = paste0("general_overwriteSymHeaders_", name, "_", hdrIdx),
                                type = "text", class = "form-control shiny-input-text",
                                style = "margin-bottom:3px;",
                                placeholder = modelOut[[name]]$headers[[hdrIdx]]$alias,
                                value = symHeaders[[hdrIdx]]
                              )
                            })
                          )
                        )
                      )
                    }),
                    lapply(seq_along(modelOut[[scalarsOutName]]$symnames), function(idx) {
                      name <- modelOut[[scalarsOutName]]$symnames[idx]
                      if (name %in% names(configJSON$overwriteAliases)) {
                        symAlias <- configJSON$overwriteAliases[[name]]$newAlias
                      } else {
                        symAlias <- modelOut[[scalarsOutName]]$symtext[idx]
                      }
                      tags$div(
                        column(6L, tags$div(name)),
                        column(
                          6L,
                          textInput(
                            paste0("general_overwriteSymAlias_", name),
                            lang$adminMode$general$overwriteSymbolAliases$label,
                            symAlias
                          )
                        )
                      )
                    }),
                    tags$div(class = "space")
                  )
                )
              ),
              tabPanel(
                lang$adminMode$general$ui$tabNameOrder,
                tags$div(
                  class = "col-sm-6", style = "padding-top: 20px;",
                  tags$div(
                    class = "main-tab", style = "max-height: 70vh;",
                    tags$h2(lang$adminMode$general$overwriteSymbolAliases$input),
                    tags$div(class = "small-space"),
                    tags$div(class = "small-space"),
                    selectizeInput("general_overwriteSheetOrderInput",
                      labelTooltip(
                        lang$adminMode$general$overwriteSheetOrder$input,
                        lang$adminMode$general$ui$tooltipDocs,
                        "https://gams.com/miro/configuration_symbols.html#tab-ordering"
                      ),
                      choices = inputTabs,
                      selected = inputTabs,
                      multiple = TRUE,
                      options = list(plugins = list("drag_drop", "no_delete"))
                    ),
                    tags$div(class = "space"),
                    tags$div(
                      tags$div(
                        class = "info-position",
                        tags$h4(
                          title = lang$adminMode$general$ui$tooltipDocs,
                          lang$adminMode$general$ui$headerInputGroups,
                          tags$a(
                            class = "info-wrapper", style = "top:-10px;", href = "https://gams.com/miro/configuration_symbols.html#tab-grouping",
                            tags$span(
                              class = "fas fa-circle-info", class = "info-icon",
                              role = "presentation",
                              `aria-label` = "More information"
                            ), target = "_blank"
                          )
                        )
                      ),
                      tags$div(
                        createArray(NULL, "symbol_inputGroups",
                          lang$adminMode$general$groups$input,
                          autoCreate = FALSE
                        )
                      ),
                      tags$div(class = "space"),
                      tags$h4(lang$adminMode$general$ui$headerInputWidgetGroups),
                      tags$div(
                        createArray(NULL, "symbol_inputWidgetGroups",
                          lang$adminMode$general$groups$widgets,
                          autoCreate = FALSE
                        )
                      )
                    ),
                    tags$div(class = "space"),
                    tags$div(
                      title = lang$adminMode$general$aggregate$title,
                      checkboxInput_SIMPLE("general_aggregate",
                        lang$adminMode$general$aggregate$label,
                        value = if (length(configJSON$aggregateWidgets)) {
                          configJSON$aggregateWidgets
                        } else {
                          config$aggregateWidgets
                        }
                      )
                    ),
                    tags$div(class = "space")
                  )
                ),
                tags$div(
                  class = "col-sm-6", style = "padding-top: 20px;",
                  tags$div(
                    class = "main-tab", style = "max-height: 70vh;",
                    tags$h2(lang$adminMode$general$overwriteSymbolAliases$output),
                    tags$div(class = "small-space"),
                    tags$div(class = "small-space"),
                    selectizeInput("general_overwriteSheetOrderOutput",
                      labelTooltip(
                        lang$adminMode$general$overwriteSheetOrder$output,
                        lang$adminMode$general$ui$tooltipDocs,
                        "https://gams.com/miro/configuration_symbols.html#tab-ordering"
                      ),
                      choices = outputTabs,
                      selected = outputTabs,
                      multiple = TRUE, options = list(plugins = list("drag_drop", "no_delete"))
                    ),
                    tags$div(class = "space"),
                    tags$div(
                      tags$div(
                        class = "info-position",
                        tags$h4(
                          title = lang$adminMode$general$ui$tooltipDocs,
                          lang$adminMode$general$ui$headerOutputGroups,
                          tags$a(
                            class = "info-wrapper", style = "top:-10px;", href = "https://gams.com/miro/configuration_symbols.html#tab-grouping",
                            tags$span(
                              class = "fas fa-circle-info", class = "info-icon",
                              role = "presentation",
                              `aria-label` = "More information"
                            ), target = "_blank"
                          )
                        )
                      ),
                      tags$div(
                        createArray(NULL, "symbol_outputGroups",
                          lang$adminMode$general$groups$output,
                          autoCreate = FALSE
                        )
                      )
                    ),
                    tags$div(class = "space")
                  )
                )
              ),
              tabPanel(
                lang$adminMode$general$ui$tabSymbolLinks,
                tags$div(
                  class = "col-sm-6", style = "padding-top: 20px;",
                  tags$div(
                    class = "main-tab",
                    tags$div(
                      tags$div(class = "space"),
                      tags$h4(lang$adminMode$general$ui$headerSymbolDisplay, class = "option-category"),
                      if (length(modelOut)) {
                        tags$div(
                          tags$div(
                            class = "info-position",
                            selectInput("general_hiddenOutputSymbols",
                              tags$div(
                                lang$adminMode$general$hiddenOutputSymbols$label,
                                tags$a("",
                                  class = "info-wrapper",
                                  href = "https://gams.com/miro/configuration_symbols.html#hidden-symbols",
                                  tags$span(
                                    class = "fas fa-circle-info", class = "info-icon",
                                    role = "presentation",
                                    `aria-label` = "More information"
                                  ),
                                  target = "_blank"
                                )
                              ),
                              choices = outputSymMultiDimChoices,
                              selected = configJSON$hiddenOutputSymbols[configJSON$hiddenOutputSymbols %in% outputSymMultiDimChoices],
                              multiple = TRUE
                            )
                          )
                        )
                      },
                      if (length(modelOut[[scalarsOutName]])) {
                        tags$div(
                          tags$div(
                            class = "info-position",
                            selectInput("general_hidden",
                              tags$div(
                                lang$adminMode$general$hiddenOutputScalars$label,
                                tags$a("",
                                  class = "info-wrapper",
                                  href = "https://gams.com/miro/configuration_symbols.html#hidden-scalars",
                                  tags$span(
                                    class = "fas fa-circle-info", class = "info-icon",
                                    role = "presentation",
                                    `aria-label` = "More information"
                                  ), target = "_blank"
                                )
                              ),
                              choices = setNames(modelOut[[scalarsOutName]]$symnames, modelOut[[scalarsOutName]]$symtext),
                              selected = configJSON$hiddenOutputScalars, multiple = TRUE
                            )
                          )
                        )
                      },
                      tags$div(class = "space"),
                      tags$div(
                        class = "info-position",
                        tags$h4(
                          title = lang$adminMode$general$ui$tooltipDocs, lang$adminMode$general$ui$headerTabSymlinks,
                          tags$a(
                            class = "info-header", href = "https://gams.com/miro/configuration_symbols.html#symbol-links",
                            tags$span(
                              class = "fas fa-circle-info", class = "info-icon",
                              role = "presentation",
                              `aria-label` = "More information"
                            ), target = "_blank"
                          )
                        )
                      ),
                      tags$div(
                        createArray(NULL, "symbol_links",
                          lang$adminMode$general$symlinks$label,
                          autoCreate = FALSE
                        )
                      )
                    ),
                    tags$div(class = "space")
                  )
                )
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "tables_gen",
        fluidRow(
          box(
            title = lang$adminMode$tables$ui$title, status = "primary", solidHeader = TRUE, width = 12,
            tags$div(id = "tableWidgetUpdateSuccess", class = "gmsalert gmsalert-success center-alert", lang$adminMode$widgets$ui$widgetTableUpdateSuccess),
            tags$div(class = "space"),
            tags$div(
              style = "padding-bottom: 20px;",
              tabsetPanel(
                id = "table_type",
                tabPanel(lang$adminMode$tables$ui$input, value = "hot"),
                tabPanel(lang$adminMode$tables$ui$output, value = "dt"),
                tabPanel(lang$adminMode$tables$ui$symbol, value = "symbol")
              )
            ),
            tags$div(
              class = "col-sm-6",
              tags$div(
                id = "noTableSymbolMsg", class = "config-message",
                lang$adminMode$widgets$ui$noSymbolMsg
              ),
              tags$div(
                class = "main-tab table-tab",
                tags$div(
                  conditionalPanel(
                    condition = "input.table_type == 'symbol'",
                    tags$div(
                      title = lang$adminMode$tables$ui$tableTooltip, class = "option-wrapper",
                      selectInput("table_symbol", lang$adminMode$tables$ui$outputSymbols,
                        choices = c()
                      )
                    ),
                    tags$hr()
                  )
                ),
                tags$div(id = "table_wrapper"),
                tags$div(class = "space")
              )
            ),
            tags$div(
              class = "col-sm-6", style = "text-align:right;overflow:auto;",
              tags$div(
                id = "preview-output-hot",
                rHandsontableOutput("table_preview_hot")
              ),
              tags$div(
                id = "preview-output-dt", style = "display:none;",
                renderDataUI("table_preview_dt",
                  type = "datatable",
                  graphTool = "plotly",
                  height = 700
                )
              ),
              tags$div(
                id = "preview-output-tableWidget", style = "display:none;text-align:left;",
                tags$div(
                  style = "margin-bottom:50px;text-align:right;",
                  actionButton("deleteTableWidget",
                    lang$adminMode$tables$ui$resetTable,
                    icon("rotate-left"),
                    class = "save-delete-delete-btn",
                    style = "width:100px;"
                  ),
                  actionButton("saveTableWidget", lang$adminMode$tables$ui$saveTable,
                    icon("floppy-disk"),
                    class = "save-delete-save-btn",
                    style = "width:100px;"
                  )
                ),
                uiOutput("tableLabelWrapper"),
                DTOutput("outputTable_preview"),
                DTOutput("dt_preview")
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "colors",
        fluidRow(
          box(
            title = lang$adminMode$colors$title, status = "primary", solidHeader = TRUE, width = 12,
            tags$div(class = "space"),
            tags$div(
              class = "col-sm-6", style = "padding-top: 20px;",
              tags$div(
                class = "main-tab",
                tags$div(
                  tags$h2(lang$adminMode$colors$themeColors$title, class = "option-category"),
                  uiOutput("themeColorsUI")
                )
              )
            ),
            tags$div(
              class = "col-sm-6", style = "padding-top: 20px;",
              tags$div(
                class = "main-tab",
                checkboxInput_MIRO("generateMiroServerTheme", lang$adminMode$colors$miroServerColors$checkbox, value = FALSE),
                conditionalPanel(
                  condition = "input.generateMiroServerTheme===true",
                  tags$div(
                    tags$h2(
                      class = "option-category",
                      labelTooltip(
                        lang$adminMode$colors$miroServerColors$title,
                        lang$adminMode$colors$miroServerColors$titleTooltip
                      )
                    ),
                    uiOutput("miroServerColorsUI")
                  )
                )
              )
            )
          )
        )
      )
    )
  )
})
ui_admin <- dashboardPage(header_admin, sidebar_admin, body_admin, skin = "black")

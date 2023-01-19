# UI sidebar
compareModeList <- c(list(
  tags$li(tags$a(
    href = "#",
    class = "change-dd-button",
    "data-btn-selector" = "#btSelectCompareMode",
    "data-btn-text" = lang$nav$sidebarButtons$splitView,
    "data-action-id" = "btSplitView",
    "data-action-val" = "splitView",
    "data-is-clickable" = "false",
    "data-view" = "split", lang$nav$sidebarButtons$splitView
  )),
  tags$li(tags$a(
    href = "#",
    class = "change-dd-button",
    "data-btn-selector" = "#btSelectCompareMode",
    "data-btn-text" = lang$nav$sidebarButtons$pivotView,
    "data-action-id" = "btSplitView",
    "data-action-val" = "pivotView",
    "data-is-clickable" = "false",
    "data-view" = "pivot", lang$nav$sidebarButtons$pivotView
  )),
  tags$li(tags$a(
    href = "#",
    class = "change-dd-button",
    "data-btn-selector" = "#btSelectCompareMode",
    "data-btn-text" = lang$nav$sidebarButtons$tabView,
    "data-action-id" = "btSplitView",
    "data-action-val" = "tabView",
    "data-is-clickable" = "false",
    "data-view" = "tab", lang$nav$sidebarButtons$tabView
  ))
), lapply(config[["customCompareModules"]], function(compareModuleConfig) {
  tags$li(tags$a(
    href = "#",
    class = "change-dd-button",
    "data-btn-selector" = "#btSelectCompareMode",
    "data-btn-text" = compareModuleConfig[["label"]],
    "data-action-id" = "btSplitView",
    "data-action-val" = compareModuleConfig[["id"]],
    "data-is-clickable" = "false",
    "data-view" = compareModuleConfig[["id"]], compareModuleConfig[["label"]]
  ))
}))

sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "miroSidebar",
    menuItem(lang$nav$sidebarMenu$inputScreen, tabName = "inputData", icon = icon("sliders")),
    menuItem(lang$nav$sidebarMenu$outputScreen, tabName = "outputData", icon = icon("chart-pie")),
    if (!config$activateModules$readonlyMode) menuItem(lang$nav$sidebarMenu$gams, tabName = "gamsinter", icon = icon("gear")),
    menuItem(lang$nav$sidebarMenu$load, tabName = "loadResults", icon = icon("database")),
    menuItem(lang$nav$sidebarMenu$scen, tabName = "scenarios", icon = icon("table-columns")),
    actionButton("btImport", lang$nav$sidebarButtons$importInput,
      width = "85%",
      class = "bt-highlight-3 btn-custom",
      style = "display:block;"
    ),
    if (!config$activateModules$readonlyMode) {
      if (config$activateModules$remoteExecution) {
        tags$div(
          class = "btn-group btSolve", style = "width:100%",
          tags$button(
            class = "btn btn-default bt-highlight-2", type = "button", id = "btSolve",
            style = "width:168px;margin:6px 0px 6px 15px;border-right:0px;",
            onclick = "Shiny.setInputValue('btSolve',1,{priority:'event'});",
            lang$nav$sidebarButtons$solve
          ),
          tags$button(
            class = "btn btn-default bt-highlight-2 dropdown-toggle", `data-toggle` = "dropdown",
            style = "margin:6px 0px 6px 0;display:block;",
            tags$span(class = "caret"),
            tags$span(class = "sr-only", "toggle dropdown")
          ),
          tags$ul(
            class = "dropdown-menu dropdown-sidebar", role = "menu",
            tags$li(tags$a(
              href = "#",
              class = "change-dd-button",
              "data-btn-selector" = "#btSolve",
              "data-btn-text" = lang$nav$sidebarButtons$solve,
              "data-action-id" = "btSolve",
              lang$nav$sidebarButtons$solve
            )),
            tags$li(tags$a(
              href = "#",
              class = "change-dd-button",
              "data-btn-selector" = "#btSolve",
              "data-btn-text" = lang$nav$sidebarButtons$submitJob,
              "data-action-id" = "btSubmitJob",
              lang$nav$sidebarButtons$submitJob
            )),
            if (identical(config$activateModules$hcube, TRUE)) {
              tags$li(tags$a(
                href = "#", class = "change-dd-button",
                class = "change-dd-button",
                "data-btn-selector" = "#btSolve",
                "data-btn-text" = lang$nav$sidebarButtons$submitHcJob,
                "data-action-id" = "btSubmitHcJob",
                lang$nav$sidebarButtons$submitHcJob
              ))
            }
          )
        )
      } else {
        actionButton("btSolve", lang$nav$sidebarButtons$solve,
          width = "85%", class = "bt-highlight-2 btSolve", style = "display:block;"
        )
      }
    },
    tagAppendAttributes(
      actionButton("btInterrupt", lang$nav$sidebarButtons$interrupt,
        width = "85%", class = "bt-highlight-2", style = "display:block;"
      ),
      disabled = ""
    ),
    tagList(
      tags$div(
        class = "btn-group btSplitView", style = "width:100%",
        tags$button(
          class = "btn btn-default bt-highlight-2 dropdown-toggle btn-cmp-mode-dropdown", `data-toggle` = "dropdown",
          tags$span(
            id = "btSelectCompareMode",
            if (identical(config$defCompMode, "tab")) {
              lang$nav$sidebarButtons$tabView
            } else if (identical(config$defCompMode, "pivot")) {
              lang$nav$sidebarButtons$pivotView
            } else {
              lang$nav$sidebarButtons$splitView
            }
          ), tags$span(class = "caret"),
          tags$span(class = "sr-only", "toggle dropdown")
        ),
        tags$ul(
          class = "dropdown-menu dropdown-sidebar", role = "menu",
          compareModeList
        )
      ),
      actionButton("btCompareScen",
        class = "bt-highlight-3", lang$nav$sidebarButtons$compareStart,
        width = "85%", style = "display:block;",
        "data-noshow" = if (identical(config$defCompMode, "pivot")) "true" else "false"
      )
    )
  )
)

# UI sidebar
sidebar <- dashboardSidebar(
  sidebarMenu(
    id = "sidebarMenuId",
    menuItem(lang$nav$sidebarMenu$inputScreen, tabName = "inputData", icon = icon("sliders-h")),
    menuItem(lang$nav$sidebarMenu$outputScreen, tabName = "outputData", icon = icon("chart-pie")),
    menuItem(lang$nav$sidebarMenu$gams, tabName = "gamsinter", icon = icon("cog")),
    menuItem(lang$nav$sidebarMenu$load, tabName = "loadResults", icon = icon("database")),
    menuItem(lang$nav$sidebarMenu$scen, tabName = "scenarios", icon = icon("columns")),
    actionButton("btImport", lang$nav$sidebarButtons$importInput,
      width = "85%",
      class = "bt-highlight-3 btn-custom",
      style = "display:block;"
    ),
    if (config$activateModules$remoteExecution) {
      tags$div(
        class = "btn-group btSolve", style = "width:100%",
        tags$button(
          class = "btn btn-default bt-highlight-2", type = "button", id = "btSolve",
          style = "width:173px;margin:6px 0px 6px 15px;border-right:0px;",
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
          class = "dropdown-menu dropdown-sidebar", role = "menu", style = "margin:6px 0px 6px 15px;position:relative;width:85%;",
          tags$li(tags$a(
            href = "#", onclick = paste0(
              "Miro.changeDDButtonEvent('",
              htmltools::htmlEscape(lang$nav$sidebarButtons$solve),
              "', '#btSolve', 'btSolve');"
            ),
            lang$nav$sidebarButtons$solve
          )),
          tags$li(tags$a(
            href = "#", onclick = paste0(
              "Miro.changeDDButtonEvent('",
              htmltools::htmlEscape(lang$nav$sidebarButtons$submitJob),
              "', '#btSolve', 'btSubmitJob');"
            ),
            lang$nav$sidebarButtons$submitJob
          )),
          if (identical(config$activateModules$hcube, TRUE)) {
            tags$li(tags$a(
              href = "#", onclick = paste0(
                "Miro.changeDDButtonEvent('",
                htmltools::htmlEscape(lang$nav$sidebarButtons$submitHcJob),
                "', '#btSolve', 'btSubmitHcJob');"
              ),
              lang$nav$sidebarButtons$submitHcJob
            ))
          }
        )
      )
    } else {
      actionButton("btSolve", lang$nav$sidebarButtons$solve,
        width = "85%", class = "bt-highlight-2 btSolve", style = "display:block;"
      )
    },
    tagAppendAttributes(actionButton("btInterrupt", lang$nav$sidebarButtons$interrupt,
      width = "85%", class = "bt-highlight-2", style = "display:block;"
    ),
    disabled = ""
    ),
    tagList(
      tags$div(
        class = "btn-group btSplitView", style = "width:100%",
        tags$button(
          class = "btn btn-default bt-highlight-2 dropdown-toggle", `data-toggle` = "dropdown",
          style = "width:85%;display:block;margin: 6px 5px 6px 15px;",
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
          class = "dropdown-menu dropdown-sidebar", role = "menu", style = "margin:6px 0px 6px 15px;position:relative;width:85%;",
          tags$li(tags$a(
            href = "#", onclick = paste0("Miro.changeDDButtonEvent('", htmltools::htmlEscape(lang$nav$sidebarButtons$splitView), "','#btSelectCompareMode','btSplitView','splitView',false);"),
            "data-view" = "split", lang$nav$sidebarButtons$splitView
          )),
          tags$li(tags$a(
            href = "#", onclick = paste0("Miro.changeDDButtonEvent('", htmltools::htmlEscape(lang$nav$sidebarButtons$pivotView), "','#btSelectCompareMode','btSplitView','pivotView',false);"),
            "data-view" = "pivot", lang$nav$sidebarButtons$pivotView
          )),
          tags$li(tags$a(
            href = "#",
            onclick = paste0("Miro.changeDDButtonEvent('", htmltools::htmlEscape(lang$nav$sidebarButtons$tabView), "','#btSelectCompareMode','btSplitView','tabView',false);"),
            "data-view" = "tab", lang$nav$sidebarButtons$tabView
          ))
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

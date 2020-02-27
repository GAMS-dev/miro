# UI sidebar
if(LAUNCHHCUBEMODE){
  sidebar <- dashboardSidebar(
    sidebarMenu(id="sidebarMenuId",
                menuItem(lang$nav$sidebarMenu$hcube$configureRun, tabName="inputData", icon = icon("th")),
                menuItem(lang$nav$sidebarMenu$hcube$import, tabName="importData", icon = icon("upload")),
                menuItem(lang$nav$sidebarMenu$hcube$load, tabName = "loadResults", icon = icon("folder-open", 
                                                                                               lib = "glyphicon")),
                menuItem(lang$nav$sidebarMenu$scen, tabName = "scenarios", icon = icon("copy")),
                menuItem(lang$nav$sidebarMenu$hcube$analyze, tabName="hcubeAnalyze", icon = icon("pie-chart")),
                actionButton("btImport", lang$nav$sidebarButtons$importInput, width = "85%", 
                             class = paste0("bt-highlight-3", if(!length(config$defaultScenName)) " glow-animation"), 
                             style = "display:block;"),
                actionButton("btSolve", lang$nav$sidebarButtons$solveHcube, width = "85%", 
                             class = "bt-highlight-2 btSolve", style = "display:block;"),
                actionButton("btSplitView", class = "bt-highlight-3", 
                             if(identical(config$defCompMode, "split"))
                               lang$nav$sidebarButtons$tabView
                             else
                               lang$nav$sidebarButtons$splitView, width = "85%", 
                             class = "bt-highlight-2", style = "display:block;"),
                tagAppendAttributes(actionButton("btCompareScen", 
                                                 class = "bt-highlight-3", 
                                                 lang$nav$sidebarButtons$compareStart, 
                                                 width = "85%", style = "display:block;"), 
                                    disabled = "")
    )
  )
}else{
  sidebar <- dashboardSidebar(
    sidebarMenu(id="sidebarMenuId",
                menuItem(lang$nav$sidebarMenu$inputScreen, tabName="inputData", icon = icon("th")),
                menuItem(lang$nav$sidebarMenu$outputScreen, tabName="outputData", icon = icon("dashboard")),
                menuItem(lang$nav$sidebarMenu$gams, tabName="gamsinter", icon = icon("cog", lib = "glyphicon")),
                menuItem(lang$nav$sidebarMenu$scen, tabName = "scenarios", icon = icon("copy")),
                actionButton("btImport", lang$nav$sidebarButtons$importInput, width = "85%", 
                             class = paste0("bt-highlight-3", if(!length(config$defaultScenName)) " glow-animation"),
                             style = "display:block;"),
                
                if(config$activateModules$remoteExecution)
                  tags$div(class = "btn-group btSolve", style = "width:100%",
                           tags$button(class = "btn btn-default bt-highlight-2", type = "button", id = "btSolve", 
                                       style = "width:173px;margin:6px 0px 6px 15px;border-right:0px;",
                                       onclick = "Shiny.setInputValue('btSolve',1,{priority:'event'});", 
                                       lang$nav$sidebarButtons$solve),
                           tags$button(class = "btn btn-default bt-highlight-2 dropdown-toggle", `data-toggle` = "dropdown",
                                       style = "margin:6px 0px 6px 0;display:block;",
                                       tags$span(class = "caret"),
                                       tags$span(class = "sr-only", "toggle dropdown")),
                           tags$ul(class = "dropdown-menu dropdown-sidebar", role = "menu", style = "margin:6px 0px 6px 15px;position:relative;width:85%;",
                                   tags$li(tags$a(href = "#", onclick = paste0("Miro.changeDDButtonEvent('", 
                                                                               htmltools::htmlEscape(lang$nav$sidebarButtons$solve), 
                                                                               "', '#btSolve', 'btSolve');"),
                                                  lang$nav$sidebarButtons$solve)),
                                   tags$li(tags$a(href = "#", onclick = paste0("Miro.changeDDButtonEvent('", 
                                                                               htmltools::htmlEscape(lang$nav$sidebarButtons$submitJob), 
                                                                               "', '#btSolve', 'btSubmitJob');"),
                                                  lang$nav$sidebarButtons$submitJob))))
                else
                  actionButton("btSolve", lang$nav$sidebarButtons$solve, 
                               width = "85%", class = "bt-highlight-2 btSolve", style = "display:block;"), 
                tagAppendAttributes(actionButton("btInterrupt", lang$nav$sidebarButtons$interrupt, 
                                                 width = "85%", class = "bt-highlight-2", style = "display:block;"), 
                                    disabled = ""),
                tagList(
                  actionButton("btSplitView", 
                               if(identical(config$defCompMode, "split"))
                                 lang$nav$sidebarButtons$tabView
                               else
                                 lang$nav$sidebarButtons$splitView, width = "85%", 
                               class = "bt-highlight-2", style = "display:block;"),
                  actionButton("btCompareScen", class = "bt-highlight-3", lang$nav$sidebarButtons$compareStart, 
                               width = "85%", style = "display:block;")
                )
    )
  )
}

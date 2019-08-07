# UI sidebar
if(!identical(config$activateModules$hcubeMode, TRUE)){
  sidebar <- dashboardSidebar(
    sidebarMenu(id="sidebarMenuId",
                menuItem(lang$nav$sidebarMenu$inputScreen, tabName="inputData", icon = icon("th")),
                menuItem(lang$nav$sidebarMenu$outputScreen, tabName="outputData", icon = icon("dashboard")),
                menuItem(lang$nav$sidebarMenu$gams, tabName="gamsinter", icon = icon("cog", lib = "glyphicon")),
                if(config$activateModules$scenario){
                  menuItem(lang$nav$sidebarMenu$scen, tabName = "scenarios", icon = icon("copy"))
                },
                actionButton("btImport", lang$nav$sidebarButtons$importInput, width = "85%", 
                             class = "bt-highlight-3 glow-animation", style = "display:block;"),
                tagAppendAttributes(actionButton("btSolve", lang$nav$sidebarButtons$solve, 
                                                 width = "85%", class = "bt-highlight-2", style = "display:block;"), 
                                    disabled = ""),
                tagAppendAttributes(actionButton("btInterrupt", lang$nav$sidebarButtons$interrupt, 
                                                 width = "85%", class = "bt-highlight-2", style = "display:block;"), 
                                    disabled = ""),
                if(config$activateModules$scenario){
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
                }
    )
  )
}else{
  sidebar <- dashboardSidebar(
    sidebarMenu(id="sidebarMenuId",
                menuItem(lang$nav$sidebarMenu$hcube$configureRun, tabName="inputData", icon = icon("th")),
                menuItem(lang$nav$sidebarMenu$hcube$import, tabName="importData", icon = icon("upload")),
                menuItem(lang$nav$sidebarMenu$hcube$load, tabName = "loadResults", icon = icon("folder-open", 
                                                                                               lib = "glyphicon")),
                menuItem(lang$nav$sidebarMenu$scen, tabName = "scenarios", icon = icon("copy")),
                menuItem(lang$nav$sidebarMenu$hcube$analyze, tabName="hcubeAnalyze", icon = icon("pie-chart")),
                actionButton("btImport", lang$nav$sidebarButtons$importInput, width = "85%", 
                             class = "bt-highlight-3 glow-animation", style = "display:block;"),
                actionButton("btSolve", lang$nav$sidebarButtons$solveHcube, width = "85%", 
                             class = "bt-highlight-2", style = "display:block;"),
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
}

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
                menuItem(lang$nav$sidebarMenu$advanced, tabName="advanced", icon = icon("ellipsis-h")),
                actionButton("btImport", lang$nav$sidebarButtons$importInput, width = "85%", class = "btHighlight3 glow-animation", style = "display:block;"),
                tagAppendAttributes(actionButton("btSolve", lang$nav$sidebarButtons$solve, 
                                                 width = "85%", class = "btHighlight2", style = "display:block;"), disabled = ""),
                tagAppendAttributes(actionButton("btInterrupt", lang$nav$sidebarButtons$interrupt, 
                                                 width = "85%", class = "btHighlight2", style = "display:block;"), disabled = ""),
                if(config$activateModules$scenario){
                  tagList(
                    conditionalPanel("input.btSplitView%2 != " %+% if(identical(config$defCompMode, "split")) "0" else "1",
                                     actionButton("btLoadScen", lang$nav$sidebarButtons$load, width = "85%", class = "btHighlight2", style = "display:block;")),
                    actionButton("btSplitView", lang$nav$sidebarButtons$tabView, width = "85%", class = "btHighlight2", style = "display:block;"),
                    actionButton("btCompareScen", class = "btHighlight3", lang$nav$sidebarButtons$compareStart, width = "85%", style = "display:block;")
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
                actionButton("btSolve", lang$nav$sidebarButtons$solveHcube, width = "85%", class = "btHighlight2", style = "display:block;"),
                conditionalPanel("input.btSplitView%2 != " %+% if(identical(config$defCompMode, "split")) "0" else "1",
                                 actionButton("btLoadScen", class = "btHighlight3", lang$nav$sidebarButtons$load, width = "85%", class = "btHighlight2", style = "display:block;")),
                actionButton("btSplitView", class = "btHighlight3", lang$nav$sidebarButtons$tabView, width = "85%", class = "btHighlight2", style = "display:block;"),
                tagAppendAttributes(actionButton("btCompareScen", class = "btHighlight3", lang$nav$sidebarButtons$compareStart, width = "85%", style = "display:block;"), disabled = "")
    )
  )
}

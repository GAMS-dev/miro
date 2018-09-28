# UI sidebar
if(!identical(config$activateModules$batchMode, TRUE)){
  sidebar <- dashboardSidebar(
    sidebarMenu(id="sidebarMenuId",
                menuItem(lang$nav$sidebarMenu$inputScreen, tabName="inputData", icon = icon("th")),
                menuItem(lang$nav$sidebarMenu$outputScreen, tabName="outputData", icon = icon("dashboard")),
                menuItem(lang$nav$sidebarMenu$gams, tabName="gamsinter", icon = icon("cog", lib = "glyphicon")),
                if(config$activateModules$scenario){
                  menuItem(lang$nav$sidebarMenu$scen, tabName = "scenarios", icon = icon("copy"))
                },
                menuItem(lang$nav$sidebarMenu$advanced, tabName="advanced", icon = icon("ellipsis-h")),
                actionButton("btImport", lang$nav$sidebarButtons$importInput, width = "85%", class = "glow-animation"),
                shinyjs::disabled(
                actionButton("btSolve", lang$nav$sidebarButtons$solve, width = "85%", class = "btHighlight1"),
                actionButton("btInterrupt", lang$nav$sidebarButtons$interrupt, width = "85%", class = "btHighlight1")  
                ),
                #if(config$activateModules$scenario){
                #  shinyjs::disabled(
                #    actionButton("btSaveInput", lang$nav$sidebarButtons$saveInputDb, width = "85%")
                #  )
                #},
                if(config$activateModules$scenario){
                  tagList(
                    conditionalPanel("input.btSplitView%2 != " %+% if(identical(config$defCompMode, "split")) "0" else "1",
                                     actionButton("btLoadScen", lang$nav$sidebarButtons$load, width = "85%", class = "btHighlight1")),
                    actionButton("btSplitView", lang$nav$sidebarButtons$tabView, width = "85%", class = "btHighlight1"),
                    shinyjs::disabled(
                      actionButton("btSave", lang$nav$sidebarButtons$save, width = "85%")
                    ),
                    actionButton("btSaveAs", lang$nav$sidebarButtons$saveAs, width = "85%", class = "btHighlight1"),
                    #downloadButton("export_1", lang$nav$sidebarButtons$exportData, width = "85%", class = "dl-button"),
                    actionButton("btCompareScen", lang$nav$sidebarButtons$compareStart, width = "85%"),
                    shinyjs::disabled(
                      actionButton("btDelete", lang$nav$sidebarButtons$delete, width = "85%")
                    )
                  )
                }
    )
  )
}else{
  sidebar <- dashboardSidebar(
    sidebarMenu(id="sidebarMenuId",
                menuItem(lang$nav$sidebarMenu$batch$configureRun, tabName="inputData", icon = icon("th")),
                menuItem(lang$nav$sidebarMenu$batch$import, tabName="importData", icon = icon("upload")),
                menuItem(lang$nav$sidebarMenu$batch$load, tabName = "loadResults", icon = icon("folder-open", 
                                                                                               lib = "glyphicon")),
                menuItem(lang$nav$sidebarMenu$scen, tabName = "scenarios", icon = icon("copy")),
                menuItem(lang$nav$sidebarMenu$batch$analyze, tabName="batchAnalyze", icon = icon("pie-chart")),
                actionButton("btImport", lang$nav$sidebarButtons$importInput, width = "85%", class = "glow-animation"),
                actionButton("btSolve", lang$nav$sidebarButtons$solve, width = "85%", class = "btHighlight1"),
                conditionalPanel("input.btSplitView%2 != " %+% if(identical(config$defCompMode, "split")) "0" else "1",
                                 actionButton("btLoadScen", lang$nav$sidebarButtons$load, width = "85%", class = "btHighlight1")),
                actionButton("btSplitView", lang$nav$sidebarButtons$tabView, width = "85%", class = "btHighlight1"),
                shinyjs::disabled(
                  actionButton("btCompareScen", lang$nav$sidebarButtons$compareStart, width = "85%")
                )
    )
  )
}

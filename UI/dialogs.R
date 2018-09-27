showReadonlyDialog <- function(){
  showModal(modalDialog(
    title = lang$nav$dialogReadonly$title,
    lang$nav$dialogReadonly$desc,
    footer = tagList(
      modalButton(lang$nav$dialogReadonly$cancelButton),
      actionButton("btSaveReadonly", label = lang$nav$dialogReadonly$okButton, class = "btHighlight1")),
    fade = TRUE, easyClose = FALSE
  ))
}

showNewScenDialog <- function(tmpScenName){
  showModal(modalDialog(
    title = lang$nav$dialogNewScen$title,
    textInput("scenName", lang$nav$dialogNewScen$desc,
              value = tmpScenName),
    shinyjs::hidden(
      tags$div(id = "bad.scen.name", class = "errMsg", lang$nav$dialogNewScen$badName),
      tags$div(id = "scen.exits", class = "errMsg", lang$nav$dialogNewScen$scenExits)
    ),
    footer = tagList(
      tags$div(id = "dialogSaveInit",
               modalButton(lang$nav$dialogNewScen$cancelButton),
               actionButton("btCheckName", lang$nav$dialogNewScen$okButton, class = "btHighlight1")
      ),
      shinyjs::hidden(
        tags$div(id = "dialogSaveConfirm",
                 actionButton("btNewName", lang$nav$dialogNewScen$btNewName),
                 actionButton("btSaveConfirm", lang$nav$dialogNewScen$btOverride, class = "btHighlight1")
        )
      )
    ),
    fade = TRUE, easyClose = FALSE))
}

showCloseScenDialog <- function(scenId){
  showModal(modalDialog(
    title = lang$nav$dialogCloseScen$title,
    lang$nav$dialogCloseScen$desc,
    footer = tagList(
      modalButton(lang$nav$dialogCloseScen$cancelButton),
      actionButton("btCloseFinal_" %+% scenId, lang$nav$dialogCloseScen$okButton, class = "btHighlight1")),
    fade=FALSE, easyClose=FALSE))
}

showRemoveDeletedScenFromUIDialog <- function(){
  showModal(modalDialog(
    title = lang$nav$dialogDeleteScen$removeFromUI$title,
    lang$nav$dialogDeleteScen$removeFromUI$desc,
    footer = tagList(
      modalButton(lang$nav$dialogDeleteScen$removeFromUI$cancelButton),
      actionButton("btRemoveDeletedConfirm", label = lang$nav$dialogDeleteScen$removeFromUI$okButton, class = "btHighlight1")),
    fade=TRUE, easyClose=FALSE))
}

showRemoveActiveScenFromUIDialog <- function(){
  showModal(modalDialog(
    title = lang$nav$dialogRemoveScen$title,
    lang$nav$dialogRemoveScen$desc,
    footer = tagList(
      modalButton(lang$nav$dialogRemoveScen$cancelButton),
      actionButton("btRemoveConfirm", label = lang$nav$dialogRemoveScen$okButton, class = "btHighlight1")),
    fade=TRUE, easyClose=FALSE))
}

showDeleteScenDialog <- function(){
  showModal(modalDialog(
    title = lang$nav$dialogDeleteScen$title,
    lang$nav$dialogDeleteScen$desc,
    footer = tagList(
      modalButton(lang$nav$dialogDeleteScen$cancelButton),
      actionButton("btDeleteConfirm", lang$nav$dialogDeleteScen$okButton, class = "btHighlight1")),
    fade=TRUE, easyClose=FALSE))
}

showRemoveExistingOutputDataDialog <- function(){
  showModal(modalDialog(
    title = lang$nav$dialogExistingOutput$title,
    lang$nav$dialogExistingOutput$desc,
    footer = tagList(
      modalButton(lang$nav$dialogExistingOutput$cancelButton),
      actionButton("btSaveOutput", label = lang$nav$dialogExistingOutput$saveOutputButton),
      actionButton("btRemoveOutput", label = lang$nav$dialogExistingOutput$discardOutputButton, class = "btHighlight1")),
    fade = TRUE, easyClose = FALSE))
}
showLoadScenDialog <- function(dbScenList, uiScenList, isInSplitView, noDBPanel = FALSE){
  tabPanelUI <- NULL
  tabPanelDB <- NULL
  if(isInSplitView && length(uiScenList)){
    tabPanelUI <- tabPanel(lang$nav$dialogLoadScen$tabUI, icon = icon("file"),
                           value = "loadScenUI",
                           tags$div(class = "space"),
                           selectInput("selLoadScenUI", lang$nav$dialogLoadScen$selLoadScen, 
                                       uiScenList, 
                                       multiple = FALSE, width = "100%")
    )
  }
  if(!noDBPanel){
    tabPanelDB <- tabPanel(lang$nav$dialogLoadScen$tabDB, icon = icon("database"),
                           value = "loadScenDb",
                           tags$div(class = "space"),
                           selectInput("selLoadScen", lang$nav$dialogLoadScen$selLoadScen, 
                                       dbScenList, 
                                       multiple = if(isInSplitView) FALSE else TRUE, width = "100%"),
                           tags$div(class = "space"),
                           tags$div(
                             lang$nav$dialogLoadScen$sortBy,
                             actionButton("btSortName", label = lang$nav$dialogLoadScen$btSortNameASC, 
                                          icon = icon("sort-by-alphabet", lib = "glyphicon"), class = "scen-sort-by"), 
                             actionButton("btSortTime", label = lang$nav$dialogLoadScen$btSortTimeASC, 
                                          icon = icon("sort-by-order", lib = "glyphicon"), class = "scen-sort-by")
                           )
    )
  }
  showModal(modalDialog(
    title = lang$nav$dialogLoadScen$title,
    if(is.null(tabPanelUI)){
      tabsetPanel(id = "tabsetLoadScen",
                  tabPanelDB
      )
    }else if(is.null(tabPanelDB)){
      tabsetPanel(id = "tabsetLoadScen",
                  tabPanelUI
      )
    }else{
      tabsetPanel(id = "tabsetLoadScen",
                  tabPanelDB, tabPanelUI
      )
    },
    footer = tagList(
      modalButton(lang$nav$dialogLoadScen$cancelButton),
      actionButton("btLoadScenConfirm", lang$nav$dialogLoadScen$okButton, 
                   class = "btHighlight1")
      ),
    fade = TRUE, easyClose = FALSE
  ))
  shinyjs::addClass("btSortTime", class = "scen-sort-by-selected")
}
######## BATCH MODE

showBatchLoadMethodDialog <- function(attribs = NULL, maxSolversPaver = ""){
  showModal(modalDialog(
    title = lang$nav$batchMode$configPaverDialog$title,
    tags$div(id="batchLoadMethod",
             if(length(sidsToLoad) <= maxConcurentLoad){
               lang$nav$batchMode$configPaverDialog$selectMethod
             }else{
               lang$nav$batchMode$configPaverDialog$maxScenWarning1 %+% 
               lang$nav$batchMode$configPaverDialog$maxScenWarning2
             }
    ),
    hidden(tags$div(id="configPaver",
                    lang$nav$batchMode$configPaverDialog$desc,
                    hidden(tags$div(id = "configPaverMaxSolversErr", class = "errMsg",
                                    sprintf(lang$nav$batchMode$configPaverDialog$tooManySolvers, 
                                            maxSolversPaver))),
                    selectInput("selPaverAttribs", lang$nav$batchMode$configPaverDialog$selAttribs, 
                                attribs, multiple = TRUE, width = "100%")
    )),
    hidden(tags$div(id="deleteTrace",
                    lang$nav$batchMode$configPaverDialog$delTrace
    )),
    footer = tagList(
      modalButton(lang$nav$batchMode$configPaverDialog$cancelButton),
      actionButton("btPaverConfig", lang$nav$batchMode$configPaverDialog$paverButton),
      hidden(actionButton("btPaver", lang$nav$batchMode$configPaverDialog$runButton, 
                          class = "btHighlight1")),
      if(length(sidsToLoad) <= maxConcurentLoad)
        actionButton("btBatchLoad", lang$nav$batchMode$configPaverDialog$interactiveButton)
    ),
    fade = T, easyClose = F
  ))
}
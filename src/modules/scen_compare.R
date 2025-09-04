# scenario comparison
observeEvent(
  input$btCompareScen,
  rv$btCompareScen <- isolate(rv$btCompareScen + 1L)
)
observeEvent(virtualActionButton(rv$btCompareScen), {
  if (isInCompareMode) {
    flog.debug("%s: Compare scenario button clicked (comparison mode stopped).", uid)
    isInCompareMode <<- FALSE
    updateActionButton(session, "btCompareScen", label = lang$nav$sidebarButtons$compareStart)
    if (identical(currentCompMode, "split")) {
      lapply(2:3, function(i) obsCompare[[i]]$suspend())
    } else {
      lapply(seq_len(maxNumberScenarios + 3), function(i) obsCompare[[i]]$suspend())
    }
  } else {
    flog.debug("%s: Compare scenario button clicked (comparison mode started).", uid)
    isInCompareMode <<- TRUE
    updateActionButton(session, "btCompareScen", label = lang$nav$sidebarButtons$compareStop)
    if (identical(currentCompMode, "split")) {
      lapply(2:3, function(i) obsCompare[[i]]$resume())
    } else {
      lapply(4:(maxNumberScenarios + 3), function(i) obsCompare[[i]]$resume())
    }
  }
})

activeScenTabId <- reactiveVal(NULL)

observeEvent(input$scenTabset, {
  req(input$scenTabset)
  new_id <- as.integer(gsub("[^0-9]", "", input$scenTabset))
  activeScenTabId(new_id)
})

output$active_tab_actions_ui <- renderUI({
  scenId <- activeScenTabId()
  req(scenId)

  metaTmp <- scenData$getById("meta", refId = tabIdToRef(scenId), drop = TRUE)
  req(metaTmp)

  tagList(
    tags$div(
      class = "content-date-wrapper",
      tags$span(id = paste0("cmpScenDate_", scenId), format(as.POSIXct(metaTmp[["_stime"]][1]), "%Y-%m-%d %H:%M:%S"))
    ),
    tags$div(
      class = "content-buttons-wrapper",
      tags$button(
        title = lang$nav$scen$tooltips$btRefresh, class = "btn btn-default bt-icon",
        type = "button", onclick = paste0("Shiny.setInputValue('btRefreshComp',", scenId, ",{priority:'event'})"),
        icon("rotate")
      ),
      HTML(paste0(
        '<button type="button" class="btn btn-default bt-icon" title="', lang$nav$scen$tooltips$btExport,
        '" onclick="Shiny.setInputValue(\'btExportScen\', ', scenId, ', {priority: \'event\'})"><i class="fas fa-download" role="presentation" aria-label="',
        lang$nav$scen$tooltips$btExport, '"></i></button>'
      )),
      tags$button(
        title = lang$nav$scen$tooltips$btTableView, class = "btn btn-default bt-icon",
        id = paste0("btScenTableView", scenId), type = "button",
        onclick = paste0("Shiny.setInputValue('btScenTableView',", scenId, ",{priority:'event'})"),
        icon("chart-bar")
      )
    ),
    tags$div(
      class = "header-close-button-wrapper",
      tags$a(
        id = "btCmpTabCloseAll", href = "#",
        class = "btn btn-default bt-icon action-button",
        title = lang$nav$scen$btCloseAll,
        onclick = paste0(
          "Miro.confirmModalShow('",
          lang$nav[["dialogCloseAllScen"]]$title, "', '",
          lang$nav[["dialogCloseAllScen"]]$desc, "', '",
          lang$nav[["dialogCloseAllScen"]]$cancelButton, "', '",
          lang$nav[["dialogCloseAllScen"]]$okButton,
          "','Shiny.setInputValue(\\'btCmpTabCloseAll\\',1,{priority:\\'event\\'})')"
        ),
        icon("square-xmark")
      )
    )
  )
})

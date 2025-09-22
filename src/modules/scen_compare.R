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

observeEvent(input$scenTabset, {
  scenId <- suppressWarnings(as.integer(gsub("[^0-9]", "", input$scenTabset)))
  if (length(scenId) != 1 || is.na(scenId)) {
    flog.error(
      "Invalid value received for input$scenTabset: %s. Possibly attempt to tamper with the app!",
      input$scenTabset
    )
    setContent(session, "#tabHeaderActionButtonsWrapper", "")
    return()
  }

  metaTmp <- scenData$getById("meta", refId = tabIdToRef(scenId), drop = TRUE)
  if (!length(metaTmp) || nrow(metaTmp) != 1L) {
    flog.warn("input$scenTabset tabset pointed to scenario that does not (longer) exist: %s", scenId)
    setContent(session, "#tabHeaderActionButtonsWrapper", "")
    return()
  }

  setContent(session, "#tabHeaderActionButtonsWrapper", htmltools::doRenderTags(tagList(
    tags$div(
      class = "content-date-wrapper",
      tags$span(id = paste0("cmpScenDate_", scenId), format(as.POSIXct(metaTmp[["_stime"]][1]), "%Y-%m-%d %H:%M:%S"))
    ),
    tags$div(
      class = "content-buttons-wrapper",
      tags$div(
        id = paste0("refreshSandbox_", scenId),
        tags$button(
          title = lang$nav$scen$tooltips$btRefresh, class = "btn btn-default bt-icon",
          type = "button", onclick = paste0("Shiny.setInputValue('btRefreshComp',", scenId, ",{priority:'event'})"),
          icon("rotate")
        )
      ),
      tags$button(
        type = "button",
        class = "btn btn-default bt-icon",
        title = lang$nav$scen$tooltips$btExport,
        onclick = paste0("Shiny.setInputValue('btExportScen',", scenId, ",{priority:'event'})"),
        tags$i(class = "fas fa-download", role = "presentation", `aria-label` = lang$nav$scen$tooltips$btExport)
      ),
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
        class = "btn btn-default bt-icon",
        title = lang$nav$scen$btCloseAll,
        onclick = paste0(
          "Miro.confirmModalShow(", toJSON(list(
            title = lang$nav[["dialogCloseAllScen"]]$title,
            desc = lang$nav[["dialogCloseAllScen"]]$desc,
            cancelTxt = lang$nav[["dialogCloseAllScen"]]$cancelButton,
            confirmTxt = lang$nav[["dialogCloseAllScen"]]$okButton,
            confirmCallKey = "btCmpTabCloseAll",
            confirmCallVal = 1
          ), auto_unbox = TRUE), ")"
        ),
        icon("square-xmark")
      )
    )
  )))
})

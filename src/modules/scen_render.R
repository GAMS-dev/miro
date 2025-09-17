renderScenInCompMode <- function(scenId, refreshData = FALSE) {
  scenIdLong <- paste0("scen_", scenId, "_")
  if (scenId %in% c(2, 3)) {
    # hide button and show content
    showEl(session, paste0("#scenSplit", scenId - 1L, "_content"))
    hideEl(session, paste0("#scenSplit", scenId - 1L, "_open"))
  } else if (!refreshData) {
    # add new Scenario tab
    insertScenTab("scenTabset", generateScenarioTabsetMulti(scenId), "scen_add", "before",
      scenID = scenId, scenButtonLang = c(
        list(tooltip = lang$nav$scen$tooltips$btClose),
        lang$nav[["dialogCloseScen"]]
      ),
      immediate = TRUE
    )
    numberScenTabs <<- numberScenTabs + 1L
    if (numberScenTabs == 1L) {
      showEl(session, "#btCmpTabCloseAll")
      hideEl(session, "#cmpTabNoScenWrapper")
    } else {
      enableEl(session, "#btCompareScen")
    }
  }
  # update title and date
  metaTmp <- scenData$getById("meta", refId = tabIdToRef(scenId), drop = TRUE)
  fullTitle <- paste0(
    if (!identical(uid, metaTmp[["_uid"]][1])) paste0(metaTmp[["_uid"]][1], ": "),
    metaTmp[["_sname"]][1]
  )

  showElReplaceTxt(
    session, paste0("#cmpScenTitle_", scenId),
    fullTitle,
    title = fullTitle
  )
  showElReplaceTxt(
    session, paste0("#cmpScenDate_", scenId),
    format(as.POSIXct(metaTmp[["_stime"]][1]), "%Y-%m-%d %H:%M:%S")
  )

  if (scenId %in% c(2, 3)) {
    buttonsUI <- tags$div(
      class = "header-action-buttons",
      tags$div(
        id = paste0("refreshSandbox_", scenId),
        tags$button(
          title = lang$nav$scen$tooltips$btRefresh, class = "btn btn-default bt-icon",
          type = "button", onclick = paste0("Shiny.setInputValue('btRefreshComp',", scenId, ",{priority:'event'})"),
          tags$i(class = "fas fa-rotate", role = "presentation", `aria-label` = lang$nav$scen$tooltips$btRefresh)
        )
      ),
      HTML(paste0(
        '<button type="button" class="btn btn-default bt-icon" title="', lang$nav$scen$tooltips$btExport,
        '" onclick="Shiny.setInputValue(\'btExportScen\', ', scenId, ', {priority: \'event\'})"><i class="fas fa-download" role="presentation" aria-label="',
        lang$nav$scen$tooltips$btExport, '"></i></button>'
      )),
      tags$button(
        title = lang$nav$scen$tooltips$btTableView, class = "btn btn-default bt-icon",
        id = paste0("btScenTableView", scenId), type = "button",
        onclick = paste0("Shiny.setInputValue('btScenTableView', ", scenId, ", {priority:'event'})"),
        tags$i(class = "fa fa-chart-bar", role = "presentation", `aria-label` = lang$nav$scen$tooltips$btTableView)
      )
    )

    removeUI(selector = paste0("#split", scenId - 1L, "_actionButtonsPlaceholder > div"), immediate = TRUE)
    insertUI(
      selector = paste0("#split", scenId - 1L, "_actionButtonsPlaceholder"),
      where = "afterBegin",
      ui = buttonsUI,
      immediate = TRUE
    )
  }

  loadDynamicTabContent(session, scenId,
    getSheetnamesByTabsetId(scenId),
    initEnv = TRUE
  )
}

closeScenSplitBox <- function(tabsetId) {
  removeUI(selector = paste0("#split", tabsetId - 1L, "_actionButtonsPlaceholder > div"), immediate = TRUE)

  tabsetIdChar <- as.character(tabsetId)
  if (!is.null(dynamicUILoaded$dynamicTabsets[[paste0("tab_", tabsetIdChar)]])) {
    dynamicUILoaded$dynamicTabsets[[paste0("tab_", tabsetIdChar)]][["content"]][] <<- FALSE
  }
  views$clearConf(tabsetIdChar)
  scenData$clear(if (identical(tabsetId, 2L)) "cmpSplitL" else "cmpSplitR")

  # show button and hide content
  resetCompTabset(tabsetIdChar)
  hideEl(session, paste0("#cmpScenTitle_", tabsetIdChar))
  hideEl(session, paste0("#scenSplit", tabsetId - 1L, "_content"))
  showEl(session, paste0("#scenSplit", tabsetId - 1L, "_open"))
}

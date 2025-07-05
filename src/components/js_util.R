showEl <- function(session, id, inline = FALSE) {
  session$sendCustomMessage("gms-showEl", list(id = id, inline = inline))
}
showLoadingScreen <- function(session, delay) {
  session$sendCustomMessage("gms-showLoadingScreen", delay)
}
hideLoadingScreen <- function(session) {
  session$sendCustomMessage("gms-hideLoadingScreen", list())
}
initializeMiroLogParser <- function(session, logTabId, containerId, tabSheetMap) {
  inputScalarsTmp <- NULL
  if (scalarsFileName %in% names(ioConfig$modelIn)) {
    inputScalarsTmp <- I(ioConfig$modelIn[[scalarsFileName]]$symnames)
  }
  session$sendCustomMessage("gms-initializeMiroLogParser", list(
    logTabId = logTabId,
    containerId = containerId,
    tabSheetMap = I(tabSheetMap),
    inputSymbolNames = I(names(ioConfig$modelIn)),
    inputScalars = inputScalarsTmp
  ))
}
showElReplaceTxt <- function(session, id, txt) {
  session$sendCustomMessage("gms-showElReplaceTxt", list(id = id, txt = htmltools::htmlEscape(txt)))
}
setContent <- function(session, selector, htmlContent) {
  session$sendCustomMessage("gms-setContent", list(selector = selector, content = htmlContent))
}
setTextContent <- function(session, selector, htmlContent, keepChildNodes = FALSE) {
  session$sendCustomMessage("gms-setTextContent", list(selector = selector, content = htmlContent, keepChildNodes = keepChildNodes))
}
setAttributes <- function(session, selectors, attribute, values) {
  stopifnot(identical(length(selectors), length(values)))
  session$sendCustomMessage(
    "gms-setAttribs",
    list(
      selectors = I(selectors),
      attr = attribute, vals = I(values)
    )
  )
}
switchCompareMode <- function(session, mode, numberScenTabs, compareModuleConfigs) {
  if (!identical(length(mode), 1L) || !is.character(mode)) {
    flog.error("switchCompareMode: Invalid mode argument. This is likely an attempt to tamper with the app!")
    return()
  }
  if (identical(mode, "pivotView")) {
    hideEl(session, ".scen-compare-tab-wrapper")
    showEl(session, "#scen-pivot-view")
    hideEl(session, "#btCompareScen")
    setAttributes(session, "#btCompareScen", "data-noshow", "true")
    setTextContent(session, "#btSelectCompareMode", lang$nav$sidebarButtons$pivotView)
    return()
  }
  if (mode %in% names(compareModuleConfigs)) {
    hideEl(session, ".scen-compare-tab-wrapper")
    showEl(session, paste0("#scen-", mode, "-view"))
    hideEl(session, "#btCompareScen")
    setAttributes(session, "#btCompareScen", "data-noshow", "true")
    setTextContent(session, "#btSelectCompareMode", compareModuleConfigs[[mode]][["label"]])
    return()
  }
  showEl(session, "#btCompareScen")
  setAttributes(session, "#btCompareScen", "data-noshow", "false")
  if (identical(mode, "splitView")) {
    enableEl(session, "#btCompareScen")
    hideEl(session, ".scen-compare-tab-wrapper")
    showEl(session, "#scen-split-view")
    setTextContent(session, "#btSelectCompareMode", lang$nav$sidebarButtons$splitView)
  } else {
    if (numberScenTabs < 2) {
      disableEl(session, "#btCompareScen")
    } else {
      enableEl(session, "#btCompareScen")
    }
    hideEl(session, ".scen-compare-tab-wrapper")
    showEl(session, "#scen-tab-view")
    setTextContent(session, "#btSelectCompareMode", lang$nav$sidebarButtons$tabView)
  }
}
hideEl <- function(session, id) {
  session$sendCustomMessage("gms-hideEl", id)
}
setCssEl <- function(session, id, css) {
  session$sendCustomMessage("gms-setCss", list(id = id, css = css))
}
showHideEl <- function(session, id, delay = 2000, msg = NULL) {
  session$sendCustomMessage("gms-showHideEl", list(id = id, delay = delay, msg = msg))
}
enableEl <- function(session, id) {
  session$sendCustomMessage("gms-enableEl", id)
}
scrollDown <- function(session, id) {
  session$sendCustomMessage("gms-scrollDown", id)
}
disableEl <- function(session, id) {
  session$sendCustomMessage("gms-disableEl", id)
}
slideToggleEl <- function(session, id, duration = 400, toggleIconDiv = NULL) {
  session$sendCustomMessage(
    "gms-slideToggleEl",
    list(
      id = id, duration = duration,
      toggleIconDiv = toggleIconDiv
    )
  )
}
toggleEl <- function(session, id) {
  session$sendCustomMessage("gms-toggleEl", id)
}
addClassEl <- function(session, id, class) {
  session$sendCustomMessage("gms-addClassEl", list(id = id, newclass = class))
}
removeClassEl <- function(session, id, class) {
  session$sendCustomMessage("gms-removeClassEl", list(id = id, oldclass = class))
}
emptyEl <- function(session, id) {
  session$sendCustomMessage("gms-emptyEl", id)
}
appendEl <- function(session, id, content, text = TRUE, scroll = FALSE,
                     triggerChange = FALSE) {
  session$sendCustomMessage("gms-appendEl", list(
    id = id, content = content,
    text = text, scroll = scroll,
    triggerChange = triggerChange
  ))
}
hideModal <- function(session, delay = 1L) {
  session$sendCustomMessage("gms-hideModal", delay)
}
updateAttachList <- function(session, id, fileName, token, labelCb, allowExec = FALSE) {
  session$sendCustomMessage("gms-updateAttachList", list(
    name = fileName, id = id,
    token = token, labelCb = labelCb,
    allowExec = allowExec
  ))
}
fitTitleInBox <- function(session, id) {
  session$sendCustomMessage("gms-fitTitleInBox", id)
}
switchTab <- function(session, id) {
  session$sendCustomMessage("gms-switchTab", id)
}
toJSString <- function(string) {
  return(toJSON(string, auto_unbox = TRUE))
}

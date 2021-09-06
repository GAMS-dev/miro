# modified original shiny tabsetPanel function (see: https://github.com/rstudio/shiny/blob/master/R/bootstrap.R)
# to add more flexibility in MIRO
MIROtabBox <- function(tabs, id = NULL, selected = NULL,
                       maxTabsExpanded = 5L, btCollapsedTabs = "",
                       noTabsGrouped = -1L, hideTabs = FALSE) {
  content <- MIROtabsetPanel(tabs, id, selected,
    maxTabsExpanded, btCollapsedTabs,
    noTabsGrouped,
    hideTabs = hideTabs
  )

  content$attribs$class <- "nav-tabs-custom"

  div(class = "col-sm-12 col-mobile", content)
}
MIROtabsetPanel <- function(tabs, id = NULL, selected = NULL,
                            maxTabsExpanded = 5L, btCollapsedTabs = "",
                            noTabsGrouped = -1L, onclick = NULL, hideTabs = FALSE) {
  foundSelected <- FALSE
  tabs <- lapply(tabs, function(div) {
    if (foundSelected || is.character(div)) {
      # Strings are not selectable items
    } else {
      # Base case: regular tab item. If the `selected` argument is
      # provided, check for a match in the existing tabs; else,
      # mark first available item as selected
      if (is.null(selected)) {
        foundSelected <<- TRUE
        div <- markTabAsSelected(div)
      } else {
        if (is.null(div$attribs$`data-value`) || isTRUE(is.na(div$attribs$`data-value`))) {
          tabValue <- div$attribs$title
        } else {
          tabValue <- div$attribs$`data-value`
        }
        if (identical(selected, tabValue)) {
          foundSelected <<- TRUE
          div <- markTabAsSelected(div)
        }
      }
    }
    return(div)
  })

  # add input class if we have an id
  if (!is.null(id)) ulClass <- "nav nav-tabs shiny-tab-input tabs-ul-mobile"

  noTabs <- length(tabs)
  if (noTabs > maxTabsExpanded + 1L) {
    noExpandedTabs <- maxTabsExpanded
  } else {
    noExpandedTabs <- noTabs
  }
  tabsetId <- shiny:::p_randomInt(1000, 10000)
  expandedTabs <- lapply(seq_len(noExpandedTabs), MIRObuildTabItem,
    tabsetId = tabsetId, tabs = tabs, noTabsGrouped = noTabsGrouped,
    onclick = onclick
  )
  liTagList <- lapply(expandedTabs, "[[", 1)
  divTagList <- lapply(expandedTabs, "[[", 2)

  if (noTabs > maxTabsExpanded + 1L) {
    if (length(ulClass)) {
      ulClass <- paste(ulClass, "nav-tabs-dropdown")
    }
    collapsedTabIds <- seq(
      from = noExpandedTabs + 1L,
      to = noTabs
    )
    ddTabs <- lapply(collapsedTabIds, MIRObuildTabItem,
      tabsetId = tabsetId,
      tabs = tabs, noTabsGrouped = noTabsGrouped,
      onclick = onclick
    )
    ddLiTagList <- lapply(ddTabs, "[[", 1)
    divTagList <- c(divTagList, lapply(ddTabs, "[[", 2))

    liTagList <- c(
      liTagList,
      list(tags$li(
        class = "dropdown max-tabs-dropdown-label",
        tags$a(
          class = "dropdown-toggle",
          `data-toggle` = "dropdown",
          `data-defaultLabel` = btCollapsedTabs,
          role = "button",
          `aria-haspopup` = "true",
          `aria-expanded` = "false",
          href = "#", btCollapsedTabs,
          onclick = "Miro.resetDropdownFilter(this)",
          tags$i(
            class = "fa fa-angle-double-right",
            `aria-hide` = "true"
          )
        ),
        tags$ul(
          class = "dropdown-menu maxTabsDropdown",
          c(
            list(tags$input(
              type = "text",
              placeholder = lang$renderers$dropdownFilter$placeholder,
              class = "form-control miro-dropdown-filter",
              onkeyup = "Miro.filterMiroDropdown(this)"
            )),
            ddLiTagList
          )
        )
      ))
    )
  }

  tabNavList <- tags$div(class = "tabs-ul-mobile-wrapper", tags$ul(
    class = ulClass, id = id, style = if (hideTabs) "display:none",
    `data-tabsetid` = tabsetId, liTagList
  ))

  tabContent <- tags$div(
    class = "tab-content tab-mobile",
    `data-tabsetid` = tabsetId, divTagList
  )

  # create the tab div
  tags$div(class = "tabbable", tabNavList, tabContent)
}
markTabAsSelected <- function(x) {
  attr(x, "selected") <- TRUE
  x
}
insertScenTab <- function(inputId, tab, target,
                          position = c("before", "after"), select = FALSE,
                          scenID = NULL, scenButtonLang = NULL,
                          immediate = FALSE,
                          session = getDefaultReactiveDomain()) {
  force(target)
  force(select)
  position <- match.arg(position)
  inputId <- session$ns(inputId)

  # Barbara -- August 2017
  # Note: until now, the number of tabs in a tabsetPanel (or navbarPage
  # or navlistPanel) was always fixed. So, an easy way to give an id to
  # a tab was simply incrementing a counter. (Just like it was easy to
  # give a random 4-digit number to identify the tabsetPanel). Since we
  # can only know this in the client side, we'll just pass `id` and
  # `tsid` (TabSetID) as dummy values that will be fixed in the JS code.
  item <- MIRObuildTabItem("id", "tsid", TRUE,
    divTag = tab,
    scenID = scenID, scenButtonLang = scenButtonLang
  )

  callback <- function() {
    session$sendInsertTab(
      inputId = inputId,
      liTag = shiny:::processDeps(item$liTag, session),
      divTag = shiny:::processDeps(item$divTag, session),
      menuName = NULL,
      target = target,
      position = position,
      select = select
    )
  }
  if (immediate) callback() else session$onFlush(callback, once = TRUE)
}

MIRObuildTabItem <- function(index, tabsetId, tabs = NULL,
                             divTag = NULL, scenID = NULL, scenButtonLang = NULL,
                             noTabsGrouped = -1L, onclick = NULL) {
  divTag <- if (!is.null(divTag)) divTag else tabs[[index]]
  # tabPanel item: create the tab's liTag and divTag
  tabId <- paste("tab", tabsetId, index, sep = "-")
  liTag <- tags$li(
    class = "tabs-li-mobile",
    tags$a(
      href = paste("#", tabId, sep = ""),
      onclick = onclick,
      class = if (noTabsGrouped > -1L) {
        paste("miro-tabset-group-", if (index <= noTabsGrouped) "1" else "2", sep = "")
      },
      `data-toggle` = "tab",
      `data-value` = divTag$attribs$`data-value`,
      divTag$attribs$title,
      if (!is.null(scenID)) {
        tags$button(
          class = "btn btn-default bt-icon", type = "button",
          title = scenButtonLang[["tooltip"]], style = "margin-bottom:3px;margin-left:10px;",
          onclick = paste0(
            "Miro.confirmModalShow('",
            scenButtonLang[["title"]], "', '",
            scenButtonLang[["desc"]], "', '",
            scenButtonLang[["cancelButton"]], "', '",
            scenButtonLang[["okButton"]],
            "', 'Shiny.setInputValue(\\'btScenClose\\',", scenID,
            ",{priority:\\'event\\'})')"
          ),
          tags$i(
            class = "fa fa-times",
            role = "presentation",
            `aria-label` = scenButtonLang[["title"]]
          )
        )
      }
    )
  )
  # if this tabPanel is selected item, mark it active
  if (isTRUE(attr(divTag, "selected", exact = TRUE))) {
    liTag$attribs$class <- "active"
    divTag$attribs$class <- "tab-pane active"
  }
  divTag$attribs$id <- tabId
  divTag$attribs$title <- NULL

  return(list(liTag = liTag, divTag = divTag))
}

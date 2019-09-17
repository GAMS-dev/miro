# modified original shiny tabsetPanel function (see: https://github.com/rstudio/shiny/blob/master/R/bootstrap.R)
# to add more flexibility in MIRO
MIROtabBox <- function(tabs, id = NULL, selected = NULL, 
                       maxTabsExpanded = 10L, btCollapsedTabs = "")
{
  content <- MIROtabsetPanel(tabs, id, selected, 
                             maxTabsExpanded, btCollapsedTabs)
  
  content$attribs$class <- "nav-tabs-custom"
  
  div(class = "col-sm-12", content)
}
MIROtabsetPanel <- function(tabs, id = NULL, selected = NULL, 
                            maxTabsExpanded = 10L, btCollapsedTabs = "")
{
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
        tabValue <- div$attribs$`data-value` %OR% div$attribs$title
        if (identical(selected, tabValue)) {
          foundSelected <<- TRUE
          div <- markTabAsSelected(div)
        }
      }
    }
    return(div)
  })
  
  # add input class if we have an id
  if (!is.null(id)) ulClass <- "nav nav-tabs shiny-tab-input"
  
  noTabs <- length(tabs)
  if(noTabs > maxTabsExpanded){
    noExpandedTabs <- maxTabsExpanded
  }else{
    noExpandedTabs <- noTabs
  }
  tabsetId <- shiny:::p_randomInt(1000, 10000)
  expandedTabs <- lapply(seq_len(noExpandedTabs), MIRObuildTabItem,
                         tabsetId = tabsetId, tabs = tabs)
  liTagList <- lapply(expandedTabs, "[[", 1)
  divTagList <- lapply(expandedTabs, "[[", 2)
  
  if(noTabs > maxTabsExpanded){
    collapsedTabIds <- seq(from = noExpandedTabs + 1L,
                           to = noTabs)
    ddTabs <- lapply(collapsedTabIds, MIRObuildTabItem,
                     tabsetId = tabsetId, 
                     tabs = tabs)
    ddLiTagList <- lapply(ddTabs, "[[", 1)
    divTagList <- c(divTagList, lapply(ddTabs, "[[", 2))
    
    liTagList <- c(liTagList, 
                   list(tags$li(class = "dropdown",
                                tags$a(class = "dropdown-toggle", 
                                       `data-toggle` = "dropdown",
                                       role = "button",
                                       `aria-haspopup` = "true",
                                       `aria-expanded` = "false",
                                       href = "#", btCollapsedTabs,
                                       tags$i(class = "fa fa-angle-double-right")),
                                tags$ul(class = "dropdown-menu maxTabsDropdown",
                                        ddLiTagList))))
  }
  
  tabNavList <- tags$ul(class = ulClass, id = id,
                        `data-tabsetid` = tabsetId, liTagList)
  
  tabContent <- tags$div(class = "tab-content",
                         `data-tabsetid` = tabsetId, divTagList)
  
  # create the tab div
  tags$div(class = "tabbable", tabNavList, tabContent)
}
markTabAsSelected <- function(x) {
  attr(x, "selected") <- TRUE
  x
}
MIROinsertTab <- function(inputId, tab, target,
                      position = c("before", "after"), select = FALSE,
                      buttonID = NULL, buttonTT = NULL,
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
  item <- MIRObuildTabItem("id", "tsid", TRUE, divTag = tab, 
                           buttonID = buttonID, buttonTT = buttonTT)
  
  callback <- function() {
    session$sendInsertTab(
      inputId = inputId,
      liTag = shiny:::processDeps(item$liTag, session),
      divTag = shiny:::processDeps(item$divTag, session),
      menuName = NULL,
      target = target,
      position = position,
      select = select)
  }
  session$onFlush(callback, once = TRUE)
}

MIRObuildTabItem <- function(index, tabsetId, tabs = NULL, 
                             divTag = NULL, buttonID = NULL, buttonTT = NULL) {
  
  divTag <- if (!is.null(divTag)) divTag else tabs[[index]]

  # tabPanel item: create the tab's liTag and divTag
  tabId <- paste("tab", tabsetId, index, sep = "-")
  liTag <- tags$li(
    tags$a(
      href = paste("#", tabId, sep = ""),
      `data-toggle` = "tab",
      `data-value` = divTag$attribs$`data-value`,
      divTag$attribs$title,
      if(!is.null(buttonID))
        actionButton(inputId = buttonID, title = buttonTT,
                     style = "margin-bottom:3px;margin-left:10px;",
                     class = "bt-icon",
                     icon = icon("times"), 
                     label = NULL)
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
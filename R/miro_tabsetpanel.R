# modified original shiny tabsetPanel function (see: https://github.com/rstudio/shiny/blob/master/R/bootstrap.R)
# to add more flexibility in MIRO
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
MIRObuildTabItem <- function(index, tabsetId, foundSelected, tabs = NULL, 
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
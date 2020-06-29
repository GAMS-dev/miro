getIndexLists <- function(setIndices, options = list()){
  unassignedSetIndices <- setIndices
  indices <- list(rows = character(0L),
                  cols = character(0L),
                  filter = character(0L),
                  aggregations = character(0L))
  
  for(id in c("cols", "filter", "aggregations")) {
    indexIds <- match(names(options[[id]]), unassignedSetIndices)
    indices[[id]] <- unassignedSetIndices[indexIds]
    if(length(indexIds))
      unassignedSetIndices <- unassignedSetIndices[-indexIds]
  }
  indices[["rows"]] <- unassignedSetIndices
  return(indices)
}
genIndexList <- function(indexList) {
  return(lapply(seq_along(indexList), function(idx){
    tags$li(class = "drop-index-item", "data-rank-id" = indexList[idx],
            names(indexList)[idx])
  }))
}
createBootstrapDropdownChoices <- function(el, eventId, deleteEventId = NULL){
  tags$li(id = paste0(eventId, "_", el$id), style = "display:flex;position:relative;",
          tags$a(class="dropdown-item", role = "button", htmltools::htmlEscape(el$alias), 
                 style = "width: 100%",
                 onClick = paste0("Shiny.setInputValue('", eventId, "','",
                                  el$id, "',{priority:\'event\'});")),
          if(!is.null(deleteEventId))
            tags$a(role = "button", icon("times"), class = "miro-pivot-delete-view-button",
                   onClick = paste0("Shiny.setInputValue('", deleteEventId, "','",
                                    el$id, "',{priority:\'event\'});")))
}

miroPivotOutput <- function(id, height = NULL, options = NULL, path = NULL){
  ns <- NS(id)
  
  if(is.null(options[["_metadata_"]]$headers)){
    unassignedSetIndices <- c()
  }else{
    unassignedSetIndices <- setNames(names(options[["_metadata_"]]$headers),
                                     vapply(options[["_metadata_"]]$headers, "[[", character(1L),
                                            "alias", USE.NAMES = FALSE))
  }
  
  noNumericCol <- sum(vapply(options[["_metadata_"]]$headers, function(header){
    return(identical(header$type, "numeric"))}, logical(1L)))
  
  if(noNumericCol > 1L){
    # symbol is table and needs to be pivoted
    unassignedSetIndices <- c(unassignedSetIndices[seq_len(length(unassignedSetIndices) - noNumericCol)], 
                              Header = "Hdr")
  }else{
    unassignedSetIndices <- unassignedSetIndices[-length(unassignedSetIndices)]
  }
  
  indices <- getIndexLists(unassignedSetIndices, options)
  
  aggregationFunctions <- if(identical(options[["_metadata_"]]$symtype, "parameter"))
    setNames(c("sum", "count", "mean", "median", "min", "max"), 
             c(options$lang$aggregationFunctions$sum,
               options$lang$aggregationFunctions$count,
               options$lang$aggregationFunctions$mean,
               options$lang$aggregationFunctions$median,
               options$lang$aggregationFunctions$min,
               options$lang$aggregationFunctions$max)) 
  else
    setNames("count", options$lang$aggregationFunctions$count)
  
  tags$div(id = ns("container"),
           if(length(options$domainFilter$domains)){
             fluidRow(style = "margin:0",
                      do.call(tabsetPanel, 
                              c(id = ns("domainFilter"), 
                                selected = options$domainFilter$default,
                                lapply(options$domainFilter$domains, function(domain){
                                  domainId <- match(domain, unassignedSetIndices)
                                  return(tabPanel(title = names(unassignedSetIndices)[domainId], 
                                                  value = domain))
                                })))
             )
           },
           fluidRow(style = "margin:0", 
                    column(width = 2L, style = "padding: 1em;",
                           tags$ul(id = ns("filterIndexList"), 
                                   class="drop-index-list filter-index-list",
                                   genIndexList(indices$filter)
                           ),
                           if(isTRUE(options$enablePersistentViews)){
                             tagList(
                               actionButton(ns("saveView"), label = NULL, icon = icon("plus-square"), 
                                            title = options$lang$btNewView),
                               downloadButton(ns("downloadCsv"), label = NULL,
                                              title = options$lang$btDownloadCsv),
                               tags$div(class="dropdown", style = "margin-top:10px;",
                                        tags$button(class="btn btn-default dropdown-toggle",
                                                    type = "button", id=ns("toggleViewButton"),
                                                    `data-toggle`="dropdown", `aria-haspopup`="true",
                                                    `aria-expanded` = "false", style = "width:100%",
                                                    options$lang$btLoadView, tags$span(class = "caret")),
                                        tags$ul(id = ns("savedViewsDD"), class = "dropdown-menu",
                                                `aria-labelledby` = ns("toggleViewButton"),
                                                createBootstrapDropdownChoices(list(id = "c21f969b5f03d33d43e04f8f136e7682", 
                                                                                    alias = "default"), 
                                                                               ns("savedViews"))))
                             )
                           }else{
                             downloadButton(ns("downloadCsv"), label = options$lang$btDownloadCsv)
                           }
                    ),
                    column(width = 4L, style = "padding: 1em;", 
                           tags$div(id = ns("filterDropdowns"), class = "miro-pivot-filter")),
                    column(width = 4L, style = "padding: 1em;", 
                           tags$div(id = ns("aggregateDropdowns"), class = "miro-pivot-filter")),
                    column(width = 2L, style = "padding: 1em;",
                           tags$ul(id = ns("aggregationIndexList"), class="drop-index-list aggregation-index-list",
                                   genIndexList(indices$aggregations)),
                           selectInput(ns("aggregationFunction"), label = NULL, 
                                       choices = aggregationFunctions,
                                       selected = if(length(options$aggregationFunction)) 
                                         options$aggregationFunction else aggregationFunctions[[1]]))),
           fluidRow(style = "margin:0", 
                    column(width = 2L,
                           selectInput(ns("pivotRenderer"), "", 
                                       setNames(c("table", "bar", "stackedbar", "line", "radar"),
                                                c(options$lang$renderer$table,
                                                  options$lang$renderer$bar,
                                                  options$lang$renderer$stackedbar,
                                                  options$lang$renderer$line,
                                                  options$lang$renderer$radar)),
                                       selected = if(length(options$pivotRenderer))
                                         options$pivotRenderer else "table")),
                    column(width = 6L, style = "padding: 1em;",
                           tags$ul(id = ns("colIndexList"), class="drop-index-list vertical-index-list",
                                   genIndexList(indices$cols))),
                    column(width = 4L, style = "padding: 1em;", 
                           tags$div(id = ns("colDropdowns"), class = "miro-pivot-filter"))),
           fluidRow(style = "margin:0", column(width = 2L, style = "padding: 1em;", 
                                               tags$ul(id = ns("rowIndexList"), class="drop-index-list",
                                                       genIndexList(indices$rows))),
                    column(width = 10L,
                           style = "min-height: 400px;",
                           tags$div(id = ns("errMsg"), class = "gmsalert gmsalert-error", 
                                    style = "position:static;margin-bottom:5px;"),
                           genSpinner(ns("loadPivotTable"), hidden = TRUE),
                           DTOutput(ns("pivotTable")),
                           chartjsOutput(ns("pivotChart"), height = "40px")
                    )),
           sortable_js(ns("filterIndexList"), 
                       options = sortable_options(group = ns("indices"), supportPointer = FALSE,
                                                  onLoad = sortable_js_capture_input(ns("filterIndexList")),
                                                  onSort = sortable_js_capture_input(ns("filterIndexList")))),
           sortable_js(ns("rowIndexList"), 
                       options = sortable_options(group = ns("indices"), supportPointer = FALSE,
                                                  onLoad = sortable_js_capture_input(ns("rowIndexList")),
                                                  onSort = sortable_js_capture_input(ns("rowIndexList")))),
           sortable_js(ns("colIndexList"), 
                       options = sortable_options(group = ns("indices"), supportPointer = FALSE, direction = "vertical", 
                                                  onLoad = sortable_js_capture_input(ns("colIndexList")),
                                                  onSort = sortable_js_capture_input(ns("colIndexList")))),
           sortable_js(ns("aggregationIndexList"), 
                       options = sortable_options(group = ns("indices"), supportPointer = FALSE,
                                                  onLoad = sortable_js_capture_input(ns("aggregationIndexList")),
                                                  onSort = sortable_js_capture_input(ns("aggregationIndexList"))))
  )
}

renderMiroPivot <- function(input, output, session, data, options = NULL, path = NULL, roundPrecision = 2L, rendererEnv = NULL){ 
  ns <- session$ns
  
  valueColName <- names(data)[length(data)]
  allPlaceholder <- setNames("", options$lang$allPlaceholder)
  
  initFilter <- TRUE
  initData <- TRUE
  initRenderer <- TRUE
  numericCols <- vapply(data, class, character(1L), USE.NAMES = FALSE) %in% c("numeric", "integer")
  if(sum(numericCols) > 1L){
    # data is already pivoted
    data <- pivot_longer(data, names(data)[numericCols], names_to = "Hdr", 
                         values_to = "value", names_repair = "unique")
    valueColName <- "value"
  }else if(sum(numericCols) == 0L){
    # data is a set -> drop last column and replace with 1
    data[, length(data)] <- 1L
    names(data) <- c(names(data)[-length(data)], "value")
    valueColName <- "value"
  }
  noColDim <- 1L
  setIndices <- names(data)[-length(data)]
  
  noRowDim <- length(data) - 1L
  
  updateFilter <- reactiveVal(1L)
  noUpdateFilterEl <- logical(length(setIndices))
  names(noUpdateFilterEl) <- setIndices
  
  setIndexAliases <- vapply(options[["_metadata_"]]$headers, "[[", character(1L),
                            "alias", USE.NAMES = FALSE)[seq_along(setIndices)]
  if(sum(numericCols) > 1L){
    setIndexAliases[length(setIndices)] <- "Header"
  }
  
  indices <- character(0L)
  # we need to update aggregation functions in case the symbol type is not available when rendering the UI
  # (e.g. in Configuration Mode)
  aggregationFunctions <- if(identical(options[["_metadata_"]]$symtype, "parameter"))
    setNames(c("sum", "count", "mean", "median", "min", "max"), 
             c(options$lang$aggregationFunctions$sum,
               options$lang$aggregationFunctions$count,
               options$lang$aggregationFunctions$mean,
               options$lang$aggregationFunctions$median,
               options$lang$aggregationFunctions$min,
               options$lang$aggregationFunctions$max)) 
  else
    setNames("count", options$lang$aggregationFunctions$count)
  
  customChartColors <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", 
                         "#fb9a99", "#e31a1c", "#fdbf6f", "#ff7f00", 
                         "#cab2d6", "#6a3d9a", "#ffff99", "#b15928", 
                         "#FCDCDB", "#f9b9b7", "#D5D3DA", "#ada9b7", 
                         "#e2546b", "#66101F", "#E0ABD7", "#c45ab3", 
                         "#8bf2fe", "#1BE7FF", "#a1d1b6", "#4C9F70", 
                         "#F6FAA9", "#f0f757", "#d3b499", "#9E6D42", 
                         "#50caf3", "#086788", "#eee49d", "#E0CA3C", 
                         "#dbcac7", "#BA9790", "#f69e84", "#EB4511", 
                         "#ccadf1", "#9B5DE5", "#A1FB8B", "#47fa1a", 
                         "#8dadd0", "#38618c", "#fcebea", "#fad8d6", 
                         "#a6b474", "#373d20", "#a248ce", "#210b2c", 
                         "#f37ea9", "#d81159", "#68f7f7", "#08bdbd", 
                         "#98feb1", "#35ff69", "#d27193", "#6d213c", 
                         "#edfab1", "#dcf763", "#feb46f", "#e06c00", 
                         "#f3ebaa", "#e9d758", "#c0c7c7", "#829191", 
                         "#f3cac5", "#E8998D", "#c7dac9", "#91b696", 
                         "#BE99A4", "#714955", "#7c7ccf", "#2a2a72", 
                         "#7efee0", "#00ffc5", "#c28eb1", "#6c3a5c", 
                         "#df7192", "#8b1e3f", "#95D86B", "#3E721D")
  
  resetView <- function(options, domainFilterDomains){
    unassignedSetIndices <- setNames(setIndices, 
                                     setIndexAliases)
    
    indices <<- getIndexLists(unassignedSetIndices, options)
    for(indexEl in list(c("filter", "filterIndexList"),
                        c("rows", "rowIndexList"), 
                        c("cols", "colIndexList"),
                        c("aggregations", "aggregationIndexList"))) {
      session$sendCustomMessage("gms-updateSortable", 
                                list(id = ns(indexEl[[2]]), 
                                     children = lapply(genIndexList(indices[[indexEl[[1]]]]), 
                                                       as.character)))
    }
    
    if(length(options[["pivotRenderer"]])){
      updateSelectInput(session, "pivotRenderer", selected = options[["pivotRenderer"]])
    }else{
      updateSelectInput(session, "pivotRenderer", selected = "table")
    }
    if(length(domainFilterDomains)){
      if(length(options[["domainFilter"]][["default"]])){
        updateTabsetPanel(session, "domainFilter", 
                          selected = options[["domainFilter"]][["default"]])
      }else{
        updateTabsetPanel(session, "domainFilter", 
                          selected = domainFilterDomains[[1]])
      }
    }
    if(initData){
      # we need to update aggregation functions in case the symbol type is not available when rendering the UI
      # (e.g. in Configuration Mode)
      if(length(options[["aggregationFunction"]])){
        updateSelectInput(session, "aggregationFunction", choices = aggregationFunctions, 
                          selected = options[["aggregationFunction"]])
      }else{
        updateSelectInput(session, "aggregationFunction", choices = aggregationFunctions, 
                          selected = aggregationFunctions[[1]])
      }
    }else{
      if(length(options[["aggregationFunction"]])){
        updateSelectInput(session, "aggregationFunction",
                          selected = options[["aggregationFunction"]])
      }else{
        updateSelectInput(session, "aggregationFunction",
                          selected = aggregationFunctions[[1]])
      }
      initFilter <<- TRUE
      initData   <<- TRUE
      initRenderer <<- TRUE
      options$resetOnInit <<- TRUE
      noUpdateFilterEl[] <<- FALSE
    }
  }
  
  if(isTRUE(options$resetOnInit)){
    resetView(options, options[["domainFilter"]]$domains)
  }
  
  setIndexAliases <- as.list(setIndexAliases)
  names(setIndexAliases) <- setIndices
  currentView <- options
  
  if(isTRUE(options$enablePersistentViews)){
    if(!length(rendererEnv[[ns(options[["_metadata_"]]$symname)]])){
      rendererEnv[[ns(options[["_metadata_"]]$symname)]] <- list("c21f969b5f03d33d43e04f8f136e7682" =
                                                                   list(name = "default"))
    }
    
    rendererEnv[[ns("saveView")]] <- observe({
      if(is.null(input$saveView) || initData || input$saveView == 0L){
        return()
      }
      showModal(modalDialog(tags$div(id = ns("errUniqueName"), class = "gmsalert gmsalert-error", 
                                     style = "position:relative",
                                     options$lang$errUniqueViewName),
                            textInput(ns("newViewName"),
                                      options$lang$newViewLabel), 
                            footer = tagList(
                              modalButton(options$lang$newViewBtCancel),
                              actionButton(ns("saveViewConfirm"), options$lang$newViewBtSave, 
                                           class = "bt-highlight-1 bt-gms-confirm")
                            ),
                            fade = TRUE, easyClose = FALSE, size = "s", 
                            title = options$lang$newViewTitle))
    })
    rendererEnv[[ns("saveViewConfirm")]] <- observe({
      if(is.null(input$saveViewConfirm) || initData || 
         input$saveViewConfirm == 0L){
        return()
      }
      isolate({
        if(identical(input$newViewName, "")){
          return()
        }
        if(input$newViewName %in% vapply(rendererEnv[[ns(options[["_metadata_"]]$symname)]], 
                                         "[[", character(1L), "name", USE.NAMES = FALSE)){
          showEl(session, paste0("#", ns("errUniqueName")))
          return()
        }
        newViewConfig <- list(name = input$newViewName,
                              aggregationFunction = input$aggregationFunction,
                              pivotRenderer = input$pivotRenderer,
                              domainFilter = list(default = input$domainFilter))
        for(indexEl in list(c("rows", "rowIndexList"))){
          indexVal <- input[[indexEl[[2]]]]
          if(length(indexVal)){
            newViewConfig[[indexEl[[1]]]] <- indexVal
          }
        }
        for(indexEl in list(c("aggregations", "aggregationIndexList"), 
                            c("filter", "filterIndexList"),
                            c("cols", "colIndexList"))){
          indexVal <- input[[indexEl[[2]]]]
          if(length(indexVal)){
            filterElList <- lapply(indexVal, function(el){
              return(input[[paste0("filter_", el)]])
            })
            names(filterElList) <- indexVal
            newViewConfig[[indexEl[[1]]]] <- filterElList
          }
        }
        viewId <- digest::digest(input$newViewName, 
                                 "md5", serialize = FALSE)
        insertUI(paste0("#", ns("savedViewsDD")), 
                 createBootstrapDropdownChoices(list(id = viewId, 
                                                     alias = newViewConfig$name), 
                                                ns("savedViews"), ns("deleteView")), 
                 where = "beforeEnd")
        newViewConfig <- list(newViewConfig)
        names(newViewConfig) <- viewId
        rendererEnv[[ns(options[["_metadata_"]]$symname)]] <<- c(rendererEnv[[ns(options[["_metadata_"]]$symname)]],
                                                                 newViewConfig)
      })
      removeModal(session)
    })
    rendererEnv[[ns("deleteView")]] <- observe({
      if(is.null(input$deleteView) || initData){
        return()
      }
      viewId <- input$deleteView
      if(length(viewId) != 1L || 
         !viewId %in% names(rendererEnv[[ns(options[["_metadata_"]]$symname)]])){
        flog.error("Invalid view id: '%s' attempted to be removed This looks like an attempt to tamper with the app!",
                   input$deleteView)
        return()
      }
      rendererEnv[[ns(options[["_metadata_"]]$symname)]][[viewId]] <- NULL
      removeUI(paste0("#", ns("savedViews"), "_", viewId))
    })
    rendererEnv[[ns("savedViews")]] <- observe({
      if(is.null(input$savedViews) || initData){
        return()
      }
      viewId <- input$savedViews
      if(length(viewId) != 1L || 
         !viewId %in% names(rendererEnv[[ns(options[["_metadata_"]]$symname)]])){
        flog.error("Invalid view id: '%s' attempted to be loaded. This looks like an attempt to tamper with the app!",
                   input$savedViews)
        return()
      }
      currentView <<- if(identical(viewId, "c21f969b5f03d33d43e04f8f136e7682")) options else 
        rendererEnv[[ns(options[["_metadata_"]]$symname)]][[viewId]]
      resetView(currentView, options[["domainFilter"]]$domains)
    })
  }
  
  output$downloadCsv <- downloadHandler(filename = paste0(options[["_metadata_"]]$symname, ".csv"),
                                        content = function(file) {
                                          write_csv(dataToRender(), file, na = "")
                                        })
  rendererEnv[[ns("filterDropdowns")]] <- observe({
    filterElements <- filteredData()$filterElements
    initData <- initFilter
    if(initFilter && isTRUE(options$resetOnInit)){
      filterIndexList <- unname(indices[["filter"]])
      aggregationIndexList <- unname(indices[["aggregations"]])
      colIndexList <- unname(indices[["cols"]])
    }else{
      filterIndexList <- isolate(input$filterIndexList)
      aggregationIndexList <- isolate(input$aggregationIndexList)
      colIndexList <- isolate(input$colIndexList)
    }
    getFilterDropdowns <- function(filterIndex, optionId = "filter"){
      allowEmpty <- optionId %in% c("aggregations", "cols")
      if(initData && length(currentView[[optionId]][[filterIndex]])){
        currentFilterVal <- currentView[[optionId]][[filterIndex]]
        if(!identical(isolate(input[[paste0("filter_", filterIndex)]]), currentFilterVal))
          noUpdateFilterEl[[filterIndex]] <<- TRUE
      }else{
        currentFilterVal <- isolate(input[[paste0("filter_", filterIndex)]])
      }
      availableFilterVal <- currentFilterVal %in% filterElements[[filterIndex]]
      if(any(availableFilterVal)) {
        selectedFilterVal <- currentFilterVal[availableFilterVal]
      }else{
        if(allowEmpty){
          selectedFilterVal <- ""
        }else{
          selectedFilterVal <- filterElements[[filterIndex]][1]
          if(identical(isolate(input[[paste0("filter_", filterIndex)]]), selectedFilterVal)){
            noUpdateFilterEl[[filterIndex]] <<- FALSE
          }else{
            noUpdateFilterEl[[filterIndex]] <<- TRUE
          }
        }
      }
      selectizeInput(ns(paste0("filter_", filterIndex)), setIndexAliases[[filterIndex]], 
                     choices = if(allowEmpty) c(allPlaceholder, filterElements[[filterIndex]])
                     else filterElements[[filterIndex]], 
                     selected = selectedFilterVal, multiple = TRUE,
                     options = list('plugins' = list('remove_button')))
    }
    initFilter <<- FALSE
    session$
      sendCustomMessage("gms-populateMiroPivotFilters", 
                        list(ns = ns(""),
                             filter = htmltools::doRenderTags(
                               lapply(filterIndexList, getFilterDropdowns)),
                             aggregations = htmltools::doRenderTags(
                               lapply(aggregationIndexList, getFilterDropdowns, optionId = "aggregations")),
                             cols = htmltools::doRenderTags(
                               lapply(colIndexList, getFilterDropdowns, optionId = "cols"))))
  })
  
  lapply(setIndices, function(filterIndex){
    rendererEnv[[ns(paste0("filter_", filterIndex))]] <- observe({
      if(is.null(input[[paste0("filter_", filterIndex)]]) || initData){
        if(!filterIndex %in% isolate(c(input$aggregationIndexList, input$colIndexList))){
          return()
        }
      }
      if(isFALSE(noUpdateFilterEl[[filterIndex]])){
        isolate({
          newVal <- updateFilter() + 1L
          updateFilter(newVal)
        })
      }else{
        noUpdateFilterEl[[filterIndex]] <<- FALSE
      }
    })
  })
  
  filteredData <- reactive({
    updateFilter()
    filterIndexList <- input$filterIndexList
    aggFilterIndexList <- input$aggregationIndexList
    colFilterIndexList <- input$colIndexList
    if(initFilter && isTRUE(options$resetOnInit)){
      filterIndexList <- unname(indices[["filter"]])
      aggFilterIndexList <- unname(indices[["aggregations"]])
      colFilterIndexList <- unname(indices[["cols"]])
    }
    filterIndexList <- c(filterIndexList, aggFilterIndexList, colFilterIndexList)
    if(length(options$domainFilter$domains) && length(input$domainFilter)){
      dataTmp <- data %>% filter(.data[[input$domainFilter]] != if(length(options$domainFilter$filterVal)) 
        options$domainFilter$filterVal else "\U00A0")
      if(!length(filterIndexList)){
        return(list(data = dataTmp, filterElements = list()))
      }
    }else{
      if(!length(filterIndexList)){
        return(list(data = data, filterElements = list()))
      }
      dataTmp <- data
    }
    
    filterElements <- vector("list", length(filterIndexList))
    names(filterElements) <- filterIndexList
    multiFilterIndices <- c()
    
    for(filterIndex in filterIndexList){
      filterElements[[filterIndex]] <- sort(unique(dataTmp[[filterIndex]]))
      optionId <- "filter"
      if(filterIndex %in% aggFilterIndexList){
        optionId <- "aggregations"
      }else if(filterIndex %in% colFilterIndexList){
        optionId <- "cols"
      }
      if(initFilter && length(currentView[[optionId]][[filterIndex]])){
        filterVal <- currentView[[optionId]][[filterIndex]]
      }else{
        filterVal <- isolate(input[[paste0("filter_", filterIndex)]])
      }
      if(!any(filterVal %in% filterElements[[filterIndex]])){
        if(filterIndex %in% c(aggFilterIndexList, colFilterIndexList)){
          # nothing selected = no filter for aggregations/cols
          next
        }
        filterVal <- filterElements[[filterIndex]][[1]]
      }
      if(any(is.na(match(filterIndex, names(dataTmp))))){
        flog.warn("Attempt to tamper with the app detected! User entered: '%s' as filter index",
                  filterIndex)
        stop("Attempt to tamper with the app detected!", call. = FALSE)
      }
      if(length(filterVal) > 1L){
        multiFilterIndices <- c(multiFilterIndices, filterIndex)
        filterExpression <- paste0(".[[\"", filterIndex, "\"]]%in%c('",
                                   paste(gsub("'", "\\'", filterVal, fixed = TRUE),  collapse = "','"),
                                   "')")
      }else{
        filterExpression <- paste0(".[[\"", filterIndex, "\"]]=='",
                                   gsub("'", "\\'", filterVal, fixed = TRUE),
                                   "'")
      }
      dataTmp <- dataTmp %>% filter(
        !!rlang::parse_expr(filterExpression))
    }
    return(list(data = dataTmp, filterElements = filterElements, 
                multiFilterIndices = multiFilterIndices))
  })
  
  dataToRender <- reactive({
    dataTmp <- filteredData()$data
    rowIndexList <- input$rowIndexList
    colIndexList <- input$colIndexList
    aggIndexList <- isolate(input$aggregationIndexList)
    multiFilterIndices <- filteredData()$multiFilterIndices
    aggregationFunction <- NULL
    
    if(is.null(rowIndexList)){
      rowIndexList <- setIndices
    }
    if(initData){
      if(isTRUE(options$resetOnInit)){
        rowIndexList <- unname(indices[["rows"]])
        colIndexList <- unname(indices[["cols"]])
        aggIndexList <- unname(indices[["aggregations"]])
        if(length(currentView[["aggregationFunction"]])){
          aggregationFunction <- currentView[["aggregationFunction"]]
        }
      }
      initData <<- FALSE
    }
    if(length(rowIndexList) + length(filteredData()$filterElements) != length(setIndices)){
      return()
    }
    rowIndexList <- c(rowIndexList, 
                      multiFilterIndices[!multiFilterIndices %in% c(aggIndexList, colIndexList)])
    if(length(aggIndexList)){
      if(identical(options[["_metadata_"]]$symtype, "parameter")){
        aggregationFunctionTmp <- input$aggregationFunction
        if(is.null(aggregationFunction)){
          aggregationFunction <- aggregationFunctionTmp
        }
        if(!aggregationFunction %in% c("sum", "count", "min", "max", "mean", "median")){
          flog.warn("Attempt to tamper with the app detected! User entered: '%s' as aggregation function.",
                    aggregationFunction)
          stop("Attempt to tamper with the app detected!", call. = FALSE)
        }
      }else{
        aggregationFunction <- "count"
      }
      dataTmp <- dataTmp %>% group_by(!!!rlang::syms(c(rowIndexList, colIndexList))) %>% 
        summarise(value = !!rlang::parse_expr(
          if(identical(aggregationFunction, "count")) 
            "sum(!is.na(value))"
          else
            paste0(aggregationFunction, "(value, na.rm = TRUE)")), .groups = "drop_last")
    }
    if(length(rowIndexList)){
      dataTmp <- dataTmp %>% select(!!!c(rowIndexList, colIndexList, valueColName)) %>% 
        arrange(!!!rlang::syms(rowIndexList))
    }else{
      dataTmp <- dataTmp %>% select(!!!c(colIndexList, valueColName))
    }
    if(length(colIndexList)){
      # note that names_sep is not an ASCII full stop, but UNICODE U+2024
      dataTmp <- dataTmp %>% 
        pivot_wider(names_from = !!colIndexList, values_from = value, names_sep = "\U2024", 
                    names_sort = TRUE, names_repair = "unique")
    }
    attr(dataTmp, "noRowHeaders") <- length(rowIndexList)
    return(dataTmp)
  })
  
  # ===================================================
  #        CHART RENDERER
  # ===================================================
  
  output$pivotChart <- renderChartjs({
    pivotRenderer <- input$pivotRenderer
    if(initRenderer && isTRUE(options$resetOnInit)){
      if(length(currentView[["pivotRenderer"]])){
        pivotRenderer <- currentView[["pivotRenderer"]]
      }else{
        return()
      }
    }
    if(!pivotRenderer %in% c("line", "bar", "stackedbar", "radar")){
      return()
    }
    showEl(session, paste0("#", ns("loadPivotTable")))
    dataTmp <- dataToRender()
    if(!length(dataTmp)){
      return()
    }
    rowHeaderLen <- attr(dataTmp, "noRowHeaders")
    noSeries <- length(dataTmp) - rowHeaderLen
    noError <- TRUE
    if(noSeries > 40L){
      showElReplaceTxt(session, paste0("#", ns("errMsg")), sprintf(options$lang$colTruncationWarning, "40"))
      noError <- FALSE
    }
    if(nrow(dataTmp) > 500L){
      showElReplaceTxt(session, paste0("#", ns("errMsg")), sprintf(options$lang$rowTruncationWarning, "500"))
      dataTmp <- slice(dataTmp, 1:500L)
      noError <- FALSE
    }
    if(noError){
      hideEl(session, paste0("#", ns("errMsg")))
    }
    labels <- do.call(paste, c(dataTmp[seq_len(rowHeaderLen)], list(sep = ".")))
    if(!length(labels)){
      labels <- "value"
    }
    if(identical(pivotRenderer, "line")){
      chartJsObj <- chartjs(customColors = customChartColors) %>% 
        cjsLine(labels = labels)
    }else if(identical(pivotRenderer, "stackedbar")){
      chartJsObj <- chartjs(customColors = customChartColors) %>% 
        cjsBar(labels = labels, stacked = TRUE)
    }else if(identical(pivotRenderer, "radar")){
      chartJsObj <- chartjs(customColors = customChartColors) %>% 
        cjsRadar(labels = labels)
    }else{
      chartJsObj <- chartjs(customColors = customChartColors) %>% 
        cjsBar(labels = labels)
    }
    for(i in seq_len(min(noSeries, 40L))){
      chartJsObj <- cjsSeries(chartJsObj, dataTmp[[rowHeaderLen + i]], 
                              label = names(dataTmp)[rowHeaderLen + i])
    }
    hideEl(session, paste0("#", ns("loadPivotTable")))
    
    return(chartJsObj %>% cjsLegend())
  })
  
  # ===================================================
  #        TABLE RENDERER
  # ===================================================
  
  output$pivotTable <- renderDT({
    pivotRenderer <- input$pivotRenderer
    if(initRenderer && isTRUE(options$resetOnInit)){
      if(length(currentView[["pivotRenderer"]])){
        pivotRenderer <- currentView[["pivotRenderer"]]
      }else{
        pivotRenderer <- "table"
      }
      initRenderer <<- FALSE
    }
    if(!identical(pivotRenderer, "table")){
      return()
    }
    changeHeightEl(session, paste0("#", ns("pivotChart")), 1, 500)
    showEl(session, paste0("#", ns("loadPivotTable")))
    dataTmp <- dataToRender()
    if(!length(dataTmp)){
      return()
    }
    if(length(dataTmp) > 500){
      showElReplaceTxt(session, paste0("#", ns("errMsg")), sprintf(options$lang$colTruncationWarning, "500"))
      dataTmp <- dataTmp[, 1:500]
    }else{
      hideEl(session, paste0("#", ns("errMsg")))
    }
    noRowHeaders <- attr(dataTmp, "noRowHeaders")
    hideEl(session, paste0("#", ns("loadPivotTable")))
    
    datatable(dataTmp, extensions = c("Scroller", "FixedColumns"), 
              selection = "none",
              container = DTbuildColHeaderContainer(names(dataTmp), 
                                                    noRowHeaders, 
                                                    unlist(setIndexAliases[names(dataTmp)[seq_len(noRowHeaders)]], 
                                                           use.names = FALSE)),
              options = list(fnDrawCallback = if(noRowHeaders > 1) JS(paste0("function ( settings ) {
    var api = this.api();
    var cols = api.columns([", paste(seq_len(noRowHeaders - 1L) - 1L, collapse = ","), "], {page:'current'} ).nodes();
    api.columns([", paste(seq_len(noRowHeaders - 1L) - 1L, collapse = ","), "], 
      {page:'current'} ).data().each( function ( colData, i ) {
       var last = null;
       $.each(colData, function ( j, group ) {
          if ( last !== group || (i > 0 && $(cols[i - 1]).eq( j ).text() !== '')) {
              last = group;
              $(cols[i]).eq( j ).css({
                position: '-webkit-sticky',
                position: 'sticky',
                top: 0
              });
              $(cols[i]).eq( j ).addClass('tableScrollCell');
          } else {
            $(cols[i]).eq( j ).css(
                    'border-top', 'none'
                ).text(
                  ''
              );
          }
      } );
    });
}")),
                             scrollY = 400, scrollX = TRUE, scrollCollapse = TRUE,
                             scroller = list(loadingIndicator = FALSE),
                             fixedColumns = list(leftColumns = noRowHeaders)), rownames = FALSE) %>%
      formatRound(seq(noRowHeaders + 1, length(dataTmp)), 
                  digits = roundPrecision)
  })
}

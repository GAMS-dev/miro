renderTimevisGraph <- function(data, configData = NULL, options, height = NULL,
                               input = NULL, filterCol = NULL) {
  if (inherits(data, "data.frame")) {
    data <- type_convert(data, cols())
  }
  
  renderTimevis({
    if (length(filterCol) && length(input$data_filter)) {
      if (isTRUE(options$filter$date)) {
        filterTmp <- as.POSIXct(input$data_filter)
        data <- filter(data, between(
          !!filterCol,
          filterTmp[1],
          max(filterTmp[1], filterTmp[2])
        ))
      } else {
        data <- filter(data, !!filterCol %in% input$data_filter)
      }
    }
    
    p <- timevis()
    
    if (!length(options$series[[1]]$start)) {
      stop("No start data found!", call. = FALSE)
    }
    
    id <- seq_along(data[[options$series[[1]]$start]])
    content <- data[[options$series[[1]]$content]]
    start <- data[[options$series[[1]]$start]]
    end <- NULL
    type <- NULL
    title <- NULL
    group <- NULL
    subgroup <- NULL
    
    if (length(options$series[[1]]$end)) {
      end <- data[[options$series[[1]]$end]]
    }
    if (!identical(length(end), length(start))) {
      end <- vector("numeric", length(id))
      end[] <- NA_real_
    }
    
    if (length(options$series[[1]]$title)) {
      title <- data[[options$series[[1]]$title]]
    }
    if (!identical(length(title), length(start))) {
      title <- vector("numeric", length(id))
      title[] <- NA_real_
    }
    
    if (length(options$series[[1]]$group)) {
      group <- data[[options$series[[1]]$group]]
    }
    if (!identical(length(group), length(start))) {
      group <- vector("numeric", length(id))
      group[] <- NA_real_
    }
    
    if (length(options$series[[1]]$subgroup)) {
      subgroup <- data[[options$series[[1]]$subgroup]]
    }
    if (!identical(length(subgroup), length(start))) {
      subgroup <- vector("numeric", length(id))
      subgroup[] <- NA_real_
    }
    
    if (length(options$series[[1]]$type)) {
      type <- vector("character", length(id))
      type[] <- options$series[[1]]$type
    }
    
    data <- tibble(
      id = id,
      content = content,
      start = start,
      end = end,
      type = type,
      title = title,
      group = group,
      subgroup = subgroup
    )
    
    groups <- NULL
    gId <- NULL
    gContent <- NULL
    gTitle <- NULL
    gSubgroupOrder <- NULL
    
    if (!is.null(group) && all(!is.na(group))) {
      gId <- unique(group)
    }
    
    if (length(options$series[[1]]$gContent)) {
      gContent <- data[[options$series[[1]]$gContent]]
      if (!identical(length(gContent), length(gId))) {
        gContent <- vector("numeric", length(gId))
        gContent[] <- "group"
      }
    } else {
      gContent <- gId
    }
    
    if (length(options$series[[1]]$groupTitle)) {
      gTitle <- unique(data[[options$series[[1]]$groupTitle]])
    }
    if (!identical(length(gTitle), length(gId))) {
      gTitle <- vector("numeric", length(gId))
      gTitle[] <- NA_real_
    }
    
    if (length(options$series[[1]]$subgroupOrder)) {
      gSubgroupOrder <- unique(data[[options$series[[1]]$subgroupOrder]])
    }
    if (!identical(length(gSubgroupOrder), length(gId))) {
      gSubgroupOrder <- vector("numeric", length(gId))
      gSubgroupOrder[] <- NA_real_
    }
    
    if (!is.null(gId) && all(!is.na(gId))) {
      groups <- tibble(
        id = gId,
        content = gContent,
        title = gTitle,
        subgroupOrder = gSubgroupOrder
      )
    }
    
    p <- timevis(data,
                 groups = groups,
                 showZoom = options$showZoom,
                 zoomFactor = options$zoomFactor,
                 fit = options$fit,
                 width = options$width,
                 height = options$height,
                 elementId = options$elementId,
                 options = list(
                   selectable = options$editable,
                   editable = options$editable,
                   multiselect = options$multiselect,
                   showCurrentTime = options$showCurrentTime
                 )
    )
    
    for (j in seq_along(options$custom)) {
      p <- addCustomTime(p,
                         time = options$custom[[j]]$time,
                         itemId = paste0("timeline_", j)
      )
    }
    
    p
  })
}

miroTimevisOutput <- function(id, height = NULL, options = NULL, path = NULL) {
  ns <- NS(id)
  
  dataGraph <- timevisOutput(ns("graph"), height = if (length(height)) height else "70vh")
  
  if (length(options$filter$col)) {
    return(tags$div(
      class = "data-filter-wrapper",
      if (isTRUE(options$filter$date)) {
        dateRangeInput(
          ns("data_filter"),
          options$filter$label
        )
      } else {
        selectInput(
          ns("data_filter"),
          options$filter$label,
          choices = c(),
          multiple = isTRUE(options$filter$multiple)
        )
      },
      dataGraph
    ))
  }
  
  dataGraph
}

renderMiroTimevis <- function(id, data, options = NULL, path = NULL,
                              roundPrecision = 2L, rendererEnv = NULL,
                              views = NULL, outputScalarsFull = NULL) {
  moduleServer(
    id,
    function(input, output, session) {
      filterCol <- NULL
      
      if (length(options$filter) && options$filter$col %in% names(data)) {
        filterCol <- as.name(options$filter$col)
        
        if (isTRUE(options$filter$date)) {
          choices <- data[[options$filter$col]]
          updateDateRangeInput(session, "data_filter",
                               min = choices[1],
                               max = choices[length(choices)],
                               start = choices[1],
                               end = choices[length(choices)]
          )
        } else {
          choices <- data[[options$filter$col]]
          updateSelectInput(session, "data_filter",
                            choices = choices,
                            selected = choices[1]
          )
        }
      }
      
      output$graph <- renderTimevisGraph(data,
                                         configData = outputScalarsFull,
                                         options = options,
                                         input = input,
                                         filterCol = if (!is.null(filterCol)) filterCol
      )
    }
  )
}
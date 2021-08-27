mirorenderer_repOutput <- function(id, height = NULL, options = NULL, path = NULL) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$style(HTML("
              .custom-css .small-box,
              .custom-css .info-box {
                cursor: pointer;
              }
              .custom-css .small-box:hover,
              .custom-css .info-box:hover {
                filter: brightness(85%);
              }
              .custom-css .no-chart {
                  cursor: default!important;
              }
              .custom-css .no-chart:hover {
                  filter: none!important;
              }
              .custom-dropdown .selectize-input {
                min-height: 22px;
                padding: 1px 5px;
                width: 60px;
              }
              .custom-dropdown .selectize-control.single .selectize-input:after{
                      right: 5px;
              }
              .custom-dropdown .form-group,
              .custom-dropdown .selectize-control {
                margin-bottom:0;
              }
              .custom-dropdown-full-width .selectize-input {
                padding-right: 12px;
                width: 170px;
              }
              .custom-info-box {
                background-color: #ecf0f5;
              }
              @media (prefers-color-scheme:dark){
                .custom-info-box {
                  background-color: #292d33;
                  box-shadow: 0 1px 1px #1d2121;
                }
              }
              .box-styles {
                  flex-grow: 1;
                  padding-left:0px;
                  padding-right:0px;
              }
              @media only screen and (max-width: 1199px){
                  .custom-info-box {
                      min-height: 60px;
                      margin-bottom: 10px;
                  }
                  .custom-info-box .info-box-icon {
                      font-size: 35px;
                      height: 60px;
                      line-height: 60px;
                      width: 60px;
                  }
                  .custom-info-box .info-box-content {
                      margin-left: 60px;
                      padding: 5px 10px;
                  }
                  .custom-info-box .info-box-text {
                      font-size: 13px;
                      white-space: nowrap!important;
                  }
                  .valueboxes .box-styles > div{
                     padding-right: 5px;
                     padding-left: 5px;
                  }
                  .padding-custom {
                      padding-left:5px;
                      padding-right:5px;
                  }
              }
              @media only screen and (min-width: 1200px) and (max-width: 1713px){
                  .custom-info-box {
                      min-height: 75px;
                      margin-bottom: 10px;
                  }
                  .custom-info-box .info-box-icon {
                      font-size: 35px;
                      height: 75px;
                      line-height: 75px;
                      width: 75px;
                  }
                  .custom-info-box .info-box-content {
                      margin-left: 75px;
                      padding: 5px 10px;
                  }
                  .custom-info-box .info-box-text {
                      font-size: 13px;
                  }
                  .valueboxes .box-styles > div{
                     padding-right: 5px;
                     padding-left: 5px;
                  }
                  .padding-custom {
                      padding-left:5px;
                      padding-right:5px;
                  }
              }
              .custom-flex-row {
                  display: flex;
                  flex-wrap: wrap;
              }
              ")),
      tags$script(JS(paste0("$(document).on('click', '#", ns("valueboxes"), " .shiny-html-output.is-clickable',
              function(){
                  Shiny.setInputValue('", ns("showChart"), "',this.id,{ priority:'event'})
              });")))
    ),
    tags$div(
      class = "container custom-css", style = "width:100%;padding-left: 0;padding-right: 0;",
      fluidRow(
        id = ns("valueboxes"), class = "display-flex valueboxes custom-flex-row", style = "margin:0px;",
        column(12,
          class = "box-styles col-lg-3 col-md-6 col-sm-6",
          shinydashboard::valueBoxOutput(ns("profit"), width = 12) %>%
            tagAppendAttributes(class = "is-clickable")
        ),
        column(12,
          class = "box-styles col-lg-3 col-md-6 col-sm-6",
          shinydashboard::valueBoxOutput(ns("revenue"), width = 12) %>%
            tagAppendAttributes(class = "is-clickable")
        ),
        column(12,
          class = "box-styles col-lg-3 col-md-6 col-sm-6",
          shinydashboard::valueBoxOutput(ns("cost"), width = 12) %>%
            tagAppendAttributes(class = "is-clickable")
        ),
        column(12,
          class = "box-styles col-lg-3 col-md-6 col-sm-6",
          shinydashboard::valueBoxOutput(ns("landuse"), width = 12)
        )
      ),
      fluidRow(
        class = "custom-flex-row", style = "margin:0px;",
        column(4,
          class = "col-lg-4 col-md-6 col-sm-12 col-xs-12 col-lg-push-8", style = "flex-grow: 1;",
          tags$h4("Settings", class = "highlight-block"),
          tags$div(
            class = "row", style = "margin:0px;",
            tags$div(
              class = "custom-dropdown custom-dropdown-full-width", style = "float:right;",
              selectInput(ns("settingsType"),
                label = NULL,
                choices = c("Settings", "Yield Factor realization"), selected = "Settings"
              )
            )
          ),
          tags$div(
            style = "overflow: hidden;max-height:220px;",
            tableOutput(ns("settings")),
            plotly::plotlyOutput(ns("yfRealizations"), height = 220)
          )
        ),
        column(4,
          class = "col-lg-4 col-md-6 col-sm-12 col-xs-12 col-lg-pull-4", style = "flex-grow: 1;",
          tags$h4(textOutput(ns("kpiTitle")), class = "highlight-block"),
          tags$div(style = "height:250px;", plotly::plotlyOutput(ns("kpiCharts"), height = 250))
        ),
        column(4,
          class = "col-lg-4 col-md-6 col-sm-12 col-xs-12 col-lg-pull-4", style = "flex-grow: 1;",
          tags$h4("Crop Planted [acres]", class = "highlight-block"),
          tags$div(style = "height: 250px;", chartjs::chartjsOutput(ns("cropComp"), height = "50px"))
        )
      ),
      fluidRow(
        class = "custom-flex-row", style = "margin:0px;",
        column(12,
          style = "flex-grow: 1; min-width:400px; margin-top:15px;",
          tags$h4("Crop Report", class = "highlight-block"),
          tabsetPanel(
            type = "tabs",
            tabPanel(
              "Yield / Planted / Seed Cost",
              tags$div(
                class = "custom-dropdown", style = "float:right;",
                selectInput(ns("chartTypeYield"),
                  label = NULL,
                  choices = c("Pie", "Bar"), selected = "Bar"
                )
              ),
              fluidRow(
                class = "custom-flex-row",
                column(12, class = "col-lg-4 col-md-6 col-sm-12", tags$div(style = "height: 250px;", chartjs::chartjsOutput(ns("yield"), height = "50px"))),
                column(12, class = "col-lg-4 col-md-6 col-sm-12", tags$div(style = "height: 250px;", chartjs::chartjsOutput(ns("planted"), height = "50px"))),
                column(12, class = "col-lg-4 col-md-6 col-sm-12", tags$div(style = "height: 250px;", chartjs::chartjsOutput(ns("seedcost"), height = "50px")))
              )
            ),
            tabPanel(
              "Sales / Purchases",
              tags$div(
                class = "custom-dropdown", style = "float:right;",
                selectInput(ns("chartTypeSales"),
                  label = NULL,
                  choices = c("Pie", "Bar"), selected = "Bar"
                )
              ),
              fluidRow(
                class = "custom-flex-row",
                column(12, class = "col-lg-3 col-md-6 col-sm-12", tags$div(style = "height: 250px;", chartjs::chartjsOutput(ns("sold"), height = "50px"))),
                column(12, class = "col-lg-3 col-md-6 col-sm-12", tags$div(style = "height: 250px;", chartjs::chartjsOutput(ns("sales"), height = "50px"))),
                column(12, class = "col-lg-3 col-md-6 col-sm-12", tags$div(style = "height: 250px;", chartjs::chartjsOutput(ns("purchased"), height = "50px"))),
                column(12, class = "col-lg-3 col-md-6 col-sm-12", tags$div(style = "height: 250px;", chartjs::chartjsOutput(ns("pcost"), height = "50px")))
              )
            )
          )
        )
      )
    )
  )
}

renderMirorenderer_rep <- function(input, output, session, data, options = NULL, path = NULL, rendererEnv = NULL, views = NULL, outputScalarsFull = NULL, ...) {
  yfsdId <- match("_gmspar_yfsd", outputScalarsFull[["scalar"]])
  if (!is.na(yfsdId)) {
    outputScalarsFull[["description"]][yfsdId] <- "Yield Factor Standard Deviation"
  }
  settingsData <- outputScalarsFull %>%
    filter(scalar != "_gmsopt_emp") %>%
    select(c("description", "value"))
  kpiData <- data$rep
  cdData <- data$cd
  pricecurveData <- data$pricecurve
  repcData <- data$repc
  repcompareData <- data$repcompare
  repfinanceData <- data$repfinance

  names(settingsData) <- c("Setting", "Value")
  output$settings <- renderTable(settingsData,
    spacing = "xs",
    colnames = TRUE, digits = 1, width = "100%"
  )

  output$yfRealizations <- plotly::renderPlotly({
    if (identical(input$settingsType, "Settings")) {
      showEl(session, paste0("#", session$ns("settings")))
      return()
    } else {
      hideEl(session, paste0("#", session$ns("settings")))
    }
    dataTmp <- data$s_yf
    plotlyObj <- plot_ly(dataTmp,
      type = "histogram", histnorm = "probability",
      marker = list(color = "#a6cee3"),
      height = 220,
      nbinsx = 25, alpha = 0.6
    ) %>%
      add_histogram(x = dataTmp$value, name = "Yield Factor") %>%
      layout(
        yaxis = list(
          title = "Probability",
          zeroline = TRUE, zerolinewidth = 0.5, zerolinecolor = "#e5e5e5",
          gridcolor = "#e5e5e5",
          titlefont = list(
            family = "'Helvetica Neue', 'Helvetica', 'Arial', sans-serif",
            size = 14
          )
        ),
        xaxis = list(
          title = "Yield Factor",
          zeroline = FALSE,
          titlefont = list(
            family = "'Helvetica Neue', 'Helvetica', 'Arial', sans-serif",
            size = 14
          )
        ), showlegend = FALSE, paper_bgcolor = "transparent",
        plot_bgcolor = "transparent"
      )
    return(plotlyObj)
  })

  # Boxes for  KPIs (custom infobox)
  infoBoxCustom <- function(value = NULL, symbol = NULL, position = "end",
                            title, subtitle = NULL,
                            icon = shiny::icon("bar-chart"), color = "aqua", width = 4, href = NULL,
                            fill = FALSE, customColor = NULL, hasChart = TRUE) {
    shinydashboard:::validateColor(color)
    shinydashboard:::tagAssert(icon, type = "i")

    colorClass <- paste0("bg-", color)

    boxContent <- div(
      class = "info-box custom-info-box",
      class = if (fill) colorClass,
      class = if (isFALSE(hasChart)) "no-chart",
      span(
        class = "info-box-icon",
        class = if (!fill) colorClass,
        style = if (!is.null(customColor)) {
          paste0("background-color:", customColor, "!important;")
        } else {
          ""
        },
        icon
      ),
      div(
        class = "info-box-content",
        span(class = "info-box-text", title),
        if (!is.null(value)) {
          span(
            class = "info-box-number",
            if (!is.null(symbol)) {
              if (!is.null(position) && identical(position, "start")) {
                paste0(symbol, value)
              } else {
                paste0(value, symbol)
              }
            } else {
              value
            }
          )
        },
        if (!is.null(subtitle)) p(subtitle)
      )
    )

    if (!is.null(href)) {
      boxContent <- a(href = href, boxContent)
    }

    div(
      class = if (!is.null(width)) paste0("col-sm-", width),
      boxContent
    )
  }


  valBoxNames <- c("profit", "revenue", "cost", "landuse")
  valBoxColors <- c("navy", "red", "olive", "yellow")
  valBoxIcons <- c("coins", "exchange-alt", "money-bill", "seedling")
  valBoxTitles <- c("Profit", "Revenue", "Cost", "Landuse")
  valBoxSymbol <- c("€", "€", "€", "%")
  valBoxSymPosition <- c("end", "end", "end", "end")
  hasChart <- c(TRUE, TRUE, TRUE, FALSE)



  lapply(1:length(valBoxNames), function(i) {
    output[[valBoxNames[i]]] <- renderValueBox({
      valueTmp <- kpiData %>%
        dplyr::filter(rh == valBoxNames[i]) %>%
        pull(value)
      infoBoxCustom(
        value = format(round(valueTmp, 0), big.mark = "."),
        symbol = valBoxSymbol[i], position = valBoxSymPosition[i],
        title = valBoxTitles[i],
        color = valBoxColors[i], icon = icon(valBoxIcons[i]),
        customColor = if (identical(i, 1L)) "#848991" else NULL,
        hasChart = hasChart[i]
      )
    })
  })


  output$kpiTitle <- renderText({
    reportToRender <- substr(input$showChart, nchar(session$ns("")) + 1L, nchar(input$showChart))
    if (!length(reportToRender)) {
      reportToRender <- "profit"
    }
    titleTmp <- valBoxTitles[match(
      reportToRender,
      valBoxNames
    )]
    textToRender <- paste0(titleTmp, " by Scenario")
    return(textToRender)
  })

  output$kpiCharts <- plotly::renderPlotly({
    reportToRender <- substr(input$showChart, nchar(session$ns("")) + 1L, nchar(input$showChart))
    if (!length(reportToRender)) {
      reportToRender <- "profit"
    }
    dataTmp <- repfinanceData %>% filter(rh == reportToRender)
    plotlyObj <- plot_ly(dataTmp,
      type = "histogram", histnorm = "probability",
      marker = list(color = "#a6cee3"),
      height = 250,
      nbinsx = 25, color = dataTmp$rh, alpha = 0.6
    ) %>%
      add_histogram(x = dataTmp$value, name = reportToRender) %>%
      layout(
        yaxis = list(
          title = "Probability",
          zeroline = TRUE, zerolinewidth = 0.5, zerolinecolor = "#e5e5e5",
          gridcolor = "#e5e5e5",
          titlefont = list(
            family = "'Helvetica Neue', 'Helvetica', 'Arial', sans-serif",
            size = 14
          )
        ),
        xaxis = list(
          title = paste(toupper(substr(reportToRender, 1, 1)), substr(reportToRender, 2, nchar(reportToRender)), " [€]", sep = ""),
          zeroline = FALSE,
          titlefont = list(
            family = "'Helvetica Neue', 'Helvetica', 'Arial', sans-serif",
            size = 14
          )
        ), showlegend = FALSE, paper_bgcolor = "transparent",
        plot_bgcolor = "transparent"
      )
    return(plotlyObj)
  })

  output$cropComp <- chartjs::renderChartjs({
    dataTmp <- repcompareData
    chartJsObj <- chartjs::chartjs() %>%
      chartjs::cjsBar(labels = dataTmp$c) %>%
      chartjs::cjsSeries(dataTmp$planted_s, label = "Stochastic") %>%
      chartjs::cjsSeries(dataTmp$planted_d, label = "Deterministic") %>%
      chartjs::cjsLegend()
    chartJsObj$x$options$maintainAspectRatio <- FALSE
    return(chartJsObj)
  })

  cropRepYield <- c("yield", "planted", "seedcost")
  cropRepYieldNames <- c("Crop Yield [tons]", "Crop Planted [acres]", "Seed Cost [€]")
  cropRepSales <- c("sold", "sales", "purchased", "pcost")
  cropRepSalesNames <- c("Crop Sold [tons]", "Crop Revenue [€]", "Crop Purchased [tons]", "Purchase Cost [€]")

  lapply(1:length(cropRepYield), function(i) {
    output[[cropRepYield[i]]] <- chartjs::renderChartjs({
      dataTmp <- repcData %>% select(c("c", cropRepYield[i]))
      chartJsObj <- chartjs::chartjs(title = cropRepYieldNames[i])
      if (identical(input$chartTypeYield, "Bar")) {
        chartJsObj <- chartjs::cjsBar(chartJsObj, labels = dataTmp$c) %>%
          chartjs::cjsLegend(display = FALSE)
        #
      } else {
        chartJsObj <- chartjs::cjsDoughnut(chartJsObj, labels = dataTmp$c, cutout = "50%") %>%
          chartjs::cjsLegend()
      }
      chartJsObj <- chartjs::cjsSeries(chartJsObj, dataTmp[[cropRepYield[i]]])
      chartJsObj$x$options$maintainAspectRatio <- FALSE
      return(chartJsObj)
    })
  })
  lapply(1:length(cropRepSales), function(i) {
    output[[cropRepSales[i]]] <- chartjs::renderChartjs({
      dataTmp <- repcData %>% select(c("c", cropRepSales[i]))
      chartJsObj <- chartjs::chartjs(title = cropRepSalesNames[i])
      if (identical(input$chartTypeSales, "Bar")) {
        chartJsObj <- chartjs::cjsBar(chartJsObj, labels = dataTmp$c) %>%
          chartjs::cjsLegend(display = FALSE)
      } else {
        chartJsObj <- chartjs::cjsDoughnut(chartJsObj, labels = dataTmp$c, cutout = "50%") %>%
          chartjs::cjsLegend()
      }
      chartJsObj <- chartjs::cjsSeries(chartJsObj, dataTmp[[cropRepSales[i]]])
      chartJsObj$x$options$maintainAspectRatio <- FALSE
      return(chartJsObj)
    })
  })
}

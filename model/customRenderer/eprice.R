epriceOutput <- function(id, height = NULL, options = NULL, path = NULL){
  ns <- NS(id)
  tagList(
    column(width = 5,
           plotlyOutput_spinner(ns("graph1"), height = height)
    ),
    column(width = 5, 
           fluidRow(
             selectInput(ns("bus_selector"), choices = c(), label = "Select buses", multiple = TRUE)
           ),
           fluidRow(
             plotlyOutput_spinner(ns("graph2"), height = height)
           )
    )
  )
}

renderEprice <- function(input, output, session, data, options = NULL, path = NULL){
  colTimePeriodName <- names(data)[2]
  PriceDf <- data %>% rename(Buses = 'Set of buses', TimePeriods = colTimePeriodName) %>% 
    mutate(Buses = as.numeric(Buses), TimePeriods = as.numeric(TimePeriods)) %>%
    arrange(Buses, TimePeriods)
  PriceDf_spread <- PriceDf %>% spread(2, 3) 
  Bus <- unique(PriceDf[[1]])
  TimePeriod <- unique(PriceDf[[2]])
  Price <- data.matrix(PriceDf_spread %>% select(-1))

  output$graph1 <- renderPlotly(plot_ly(x = ~TimePeriod, y = ~Bus, z = ~Price) %>% add_surface())
  updateSelectInput(session, "bus_selector", choices = PriceDf$Buses, selected = PriceDf$Buses[[1]])
  Price_filtered <- PriceDf
  observeEvent(input$bus_selector, {
    Price_filtered <- Price_filtered %>% dplyr::filter(Buses %in% input$bus_selector) %>% mutate(Buses = as.character(Buses))
    output$graph2 <- renderPlotly(plot_ly(Price_filtered, x = ~TimePeriods, y = Price_filtered[[3]], type = 'scatter', mode = 'lines', color = ~Buses))
  })
}
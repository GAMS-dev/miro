mirorenderer_battery_powerOutput <- function(id, height = NULL, options = NULL, path = NULL){
    ns <- NS(id)
plotOutput(ns("cumsumPlot"))
}

renderMirorenderer_battery_power <- function(input, output, session, data, options = NULL, path = NULL, rendererEnv = NULL, views = NULL, outputScalarsFull = NULL, ...){
battery_power <- data$battery_power$level
storage_level <- -cumsum(battery_power)

max_storage <- data[["_scalarsve_out"]] %>%
    filter(scalar == "battery_storage") %>%
    pull(level)

output$cumsumPlot <- renderPlot({
    barplot(storage_level, col="lightblue", ylab="Energy Capacity in kWh" , 
        names.arg=data$battery_power$j, las = 2, ylim=c(0,max_storage+10),
        main = "Storage level of the BESS")
    grid()
})

}

mirorenderer_nurseassignmentsOutput <- function(id, height = NULL, options = NULL, path = NULL){
    ns <- NS(id)
    timevisOutput(ns("tvis_shifts"))
}
 
 renderMirorenderer_nurseassignments <- function(input, output, session, data, options = NULL, path = NULL, views = NULL, ...){
    data_full <- left_join(data$nurseassignments, data$shiftdata)
    colnames(data_full) <- c("nurse", "shift", "department", 
        "day", "level", "marginal", "lower", "upper", "scale", 
        "start.time", "end.time", "minimum.requirement", "maximum.requirement")
    data_full <- data_full %>% filter(level == 1)
    int_wday <- as.POSIXlt(Sys.Date())$wday
    print(int_wday)
    date_start_week <- as.POSIXlt((Sys.Date() + 8 - int_wday))
    vec_dates <- seq.POSIXt(date_start_week, length.out = 7, 
        by = "days") %>% as.POSIXct()
    vec_str_days <- c("monday", "tuesday", "wednesday", "thursday", 
        "friday", "saturday", "sunday")
    vec_nurses <- data_full %>% pull(nurse) %>% unique()
    vec_nurses_id <- seq_along(vec_nurses)
    names(vec_nurses_id) <- vec_nurses
    data_full <- data_full %>% mutate(content = department, Date = case_when(day == 
        "monday" ~ vec_dates[1], day == "tuesday" ~ vec_dates[2], 
        day == "wednesday" ~ vec_dates[3], day == "thursday" ~ 
            vec_dates[4], day == "friday" ~ vec_dates[5], day == 
            "saturday" ~ vec_dates[6], day == "sunday" ~ vec_dates[7]), 
        start = Date + start.time * 3600, end = case_when(start.time > 
            end.time ~ Date + (24 + end.time) * 3600, TRUE ~ 
            Date + end.time * 3600), group = vec_nurses_id[nurse])
    print(head(data_full))
    output$tvis_shifts <- renderTimevis(timevis(data_full, groups = data.frame(id = vec_nurses_id, 
        content = vec_nurses), options = list(stack = FALSE, width = "800%")))
}

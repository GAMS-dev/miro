mirorenderer_nurseassignmentsOutput <- function(id, height = NULL, options = NULL, path = NULL){
    ns <- NS(id)
    fluidPage(
        tabsetPanel(
            tabPanel(ns("Employee View"),
                timevisOutput(ns("tvis_shifts_nurse"))
            ),
            tabPanel(ns("Department View"),
                timevisOutput(ns("tvis_shifts_department"))
            ),
            selected = ns("Employee View"),
            type = "tabs"
        )
    )
}
 
 renderMirorenderer_nurseassignments <- function(input, output, session, data, options = NULL, path = NULL, views = NULL, ...){
    data_full <- left_join(data$nurseassignments, data$shiftdata)
    colnames(data_full) <- c("nurse", "shift", "department", 
        "day", "level", "marginal", "lower", "upper", "scale", 
        "start.time", "end.time", "minimum.requirement", "maximum.requirement")
    data_full <- data_full %>% filter(level == 1)
    data_full[data_full$department == "Cardiac_Care", "department"] <- "Cardiac Care"
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
    vec_departments <- data_full %>% pull(department) %>% unique()
    vec_departments_id <- seq_along(vec_departments)
    names(vec_departments_id) <- vec_departments
    df_groups_nurses <- data.frame(id = vec_nurses_id, content = vec_nurses)
    df_groups_departments <- data.frame(id = vec_departments_id, content = vec_departments)

    data_full <- data_full %>% mutate(content = department, Date = case_when(day == 
        "monday" ~ vec_dates[1], day == "tuesday" ~ vec_dates[2], 
        day == "wednesday" ~ vec_dates[3], day == "thursday" ~ 
            vec_dates[4], day == "friday" ~ vec_dates[5], day == 
            "saturday" ~ vec_dates[6], day == "sunday" ~ vec_dates[7]), 
        start = Date + start.time * 3600, end = case_when(start.time > 
            end.time ~ Date + (24 + end.time) * 3600, TRUE ~ 
            Date + end.time * 3600), 
            group_nurse = vec_nurses_id[nurse],
            group_department = vec_departments_id[department])
    
    data_nurses <- data_full %>% select(content, Date, start, end, group_nurse) %>% rename(group = group_nurse)
    data_departments <- data_full %>% select(nurse, Date, start, end, group_department) %>% rename(content = nurse, group = group_department)
    output$tvis_shifts_nurse <- renderTimevis(
        timevis(
            data_nurses, groups = df_groups_nurses, 
            options = list(stack = FALSE, width = "800%")
        )
    )
    output$tvis_shifts_department <- renderTimevis(
        timevis(
            data_departments, groups = df_groups_departments, 
            options = list(stack = TRUE, width = "800%")
        )
    )
}

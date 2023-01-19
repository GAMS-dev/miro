mirorenderer_nurseassignmentsOutput <- function(id, height = NULL, options = NULL, path = NULL) {
  ns <- NS(id)
  fluidPage(tabsetPanel(
    tabPanel("Employee View",
      value = ns("employeeView"),
      tags$div(
        style = "margin-top: 15px;",
        selectInput(ns("si_employee"),
          "Select Employee(s)", NULL,
          multiple = TRUE
        )
      ),
      tags$div(timevis::timevisOutput(ns("tvis_shifts_nurse")))
    ),
    tabPanel(
      "Department View",
      tags$div(
        style = "margin-top: 15px;",
        selectInput(
          ns("si_department"),
          "Select Department(s)", NULL
        )
      ),
      tags$div(
        timevis::timevisOutput(ns("tvis_shifts_department"))
      )
    ),
    type = "tabs"
  ))
}

renderMirorenderer_nurseassignments <- function(input, output, session, data, options = NULL, path = NULL, rendererEnv = NULL, views = NULL, outputScalarsFull = NULL, ...) {
  # data preparation
  data_full <- left_join(data$nurseassignments, data$shiftdata)
  colnames(data_full) <- c(
    "nurse", "shift", "department",
    "day", "level", "marginal", "lower", "upper", "scale",
    "start.time", "end.time", "minimum.requirement", "maximum.requirement"
  )
  data_full <- data_full %>% filter(level == 1)
  data_full[data_full$department == "Cardiac_Care", "department"] <- "Cardiac Care"

  int_wday <- as.POSIXlt(Sys.Date())$wday
  date_start_week <- as.POSIXlt((Sys.Date() + 8 - int_wday))
  vec_dates <- seq.POSIXt(date_start_week,
    length.out = 7,
    by = "days"
  ) %>% as.POSIXct()
  vec_str_days <- c(
    "monday", "tuesday", "wednesday", "thursday",
    "friday", "saturday", "sunday"
  )

  vec_nurses <- data_full %>%
    pull(nurse) %>%
    unique()
  vec_nurses_id <- seq_along(vec_nurses)
  names(vec_nurses_id) <- vec_nurses
  vec_departments <- data_full %>%
    pull(department) %>%
    unique()
  vec_departments_id <- seq_along(vec_departments)
  names(vec_departments_id) <- vec_departments
  updateSelectInput(session, inputId = "si_employee", choices = vec_nurses, selected = NULL)
  updateSelectInput(session, inputId = "si_department", choices = vec_departments)
  data_full <- data_full %>% mutate(
    content = department,
    Date = case_when(
      day ==
        "monday" ~ vec_dates[1], day == "tuesday" ~ vec_dates[2],
      day == "wednesday" ~ vec_dates[3], day == "thursday" ~
        vec_dates[4], day == "friday" ~ vec_dates[5], day ==
        "saturday" ~ vec_dates[6], day == "sunday" ~ vec_dates[7]
    ),
    start = Date + start.time * 3600,
    end = case_when(start.time >
      end.time ~ Date + (24 + end.time) * 3600, TRUE ~
      Date + end.time * 3600), group_nurse = vec_nurses_id[nurse],
    group_department = vec_departments_id[department],
    title = paste0(start, " - ", end)
  )


  # Employee View
  nurseTmp <- data_full %>%
    select(content, Date, start, end, group_nurse, title) %>%
    rename(group = group_nurse)
  groupTmp <- tibble(
    id = vec_nurses_id,
    content = names(vec_nurses_id)
  )

  output$tvis_shifts_nurse <- timevis::renderTimevis({
    if (length(input$si_employee)) {
      nurseTmp <- nurseTmp %>% filter(group %in% vec_nurses_id[input$si_employee])
      groupTmp <- groupTmp %>%
        filter(id %in% vec_nurses_id[input$si_employee])
    }

    timevis::timevis(nurseTmp,
      groups = groupTmp,
      options = list(stack = FALSE)
    )
  })

  # Department View
  dataTmp <- data_full %>%
    select(nurse, Date, start, end, group_department, title) %>%
    rename(content = nurse, group = group_department)

  groupsTmp <- tibble(
    id = vec_departments_id,
    content = names(vec_departments_id)
  )
  output$tvis_shifts_department <- timevis::renderTimevis({
    if (length(input$si_department)) {
      dataTmp <- dataTmp %>%
        filter(group %in% vec_departments_id[input$si_department])

      groupsTmp <- groupsTmp %>%
        filter(id %in% vec_departments_id[input$si_department])
    }
    timevis::timevis(
      dataTmp,
      groups = groupsTmp,
      options = list(stack = TRUE)
    )
  })
}

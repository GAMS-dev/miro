mirorenderer_existingcohortgroupmapOutput <- function(id, height = NULL, options = NULL, path = NULL){
    ns <- NS(id)
    tagList(
        tags$style(".rank-list{color: #000;}"),
        tags$b("Current cohort assignment. You can drag students between the lists."), 
        fluidRow(column(width = 12, tags$div(selectizeInput(ns("groupFilter"), 
            label = NULL, choices = c(), multiple = TRUE, options = list(placeholder = "Filter names by groups")), 
            style = "margin-top:15pt;"))), fluidRow(column(width = 12, 
            uiOutput(ns("cohortsOut")))))
}
 
 renderMirorenderer_existingcohortgroupmap <- function(input, output, session, data, options = NULL, path = NULL, rendererEnv = NULL, views = NULL, ...){
    force(data)
    studentsA <- filter(data, c == "A")
    studentsA <- unique(studentsA$s)
    studentsB <- filter(data, c == "B")
    studentsB <- unique(studentsB$s)
    if (!is.null(data$g)) {
        updateSelectizeInput(session, "groupFilter", choices = sort(unique(data$g)))
    }
    output$cohortsOut <- renderUI({
        cohortLists <- sortable::bucket_list(header = NULL, group_name = session$ns("cohort_list"), 
            orientation = "horizontal", sortable::add_rank_list(text = "Cohort A", 
                labels = unique(studentsFiltered()$s[studentsFiltered()$c == 
                  "A"]), input_id = session$ns("cohort_a")), 
            sortable::add_rank_list(text = "Cohort B", labels = unique(studentsFiltered()$s[studentsFiltered()$c == 
                "B"]), input_id = session$ns("cohort_b")))
        return(cohortLists)
    })
    markUnsaved <- FALSE
    updateList <- reactiveVal(0)
    dataRet <- data
    rendererEnv[[session$ns("cohortsObs")]] <- observe({
        force(input$cohort_b)
        if (is.null(input$cohort_b) || identical(input$cohort_b, 
            dataRet$s[dataRet$c == "B"])) {
            return()
        }
        isolate({
            dataRet$c[dataRet$s %in% studentsFiltered()$s] <<- "A"
            dataRet$c[dataRet$s %in% input$cohort_b & dataRet$s %in% 
                studentsFiltered()$s] <<- "B"
            newVal <- updateList() + 1L
            updateList(newVal)
        })
    })
    studentsFiltered <- reactive({
        if (is.null(input$groupFilter)) {
            return(dataRet)
        }
        filter(dataRet, g %in% c(input$groupFilter))
    })
    return(reactive({
        updateList()
        return(dataRet)
    }))
}

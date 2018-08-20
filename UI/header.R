# UI header
header <- dashboardHeader(title=config$pageTitle, disable = if(isShinyProxy) TRUE else FALSE)
library('methods')
if(!'shiny'%in%installed.packages()){
install.packages('shiny',repos='https://cloud.r-project.org',dependencies=TRUE)}
shiny::runApp(launch.browser=TRUE)
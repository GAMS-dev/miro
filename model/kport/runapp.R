library('methods')
if(!'shiny'%in%installed.packages()[, 'Package']){
install.packages('shiny',repos='https://cloud.r-project.org',dependencies=TRUE)}
shiny::runApp(launch.browser=TRUE)
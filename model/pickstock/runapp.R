library('methods')
.libPaths(c('C:\\GAMS\\win64\\25.2\\library', .libPaths()))
if(!'shiny'%in%installed.packages()){
install.packages('shiny',repos='https://cloud.r-project.org',dependencies=TRUE)}
shiny::runApp(launch.browser=TRUE)
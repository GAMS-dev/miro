
if(identical(tolower(Sys.info()[["sysname"]]), "windows")){
    assign(".lib.loc", Sys.getenv("R_LIB_PATHS"), envir = environment(.libPaths))
}
suppressMessages(shiny::runApp(
  Sys.getenv("RE_SHINY_PATH"),
  host = "127.0.0.1",
  launch.browser = identical(Sys.getenv("LAUNCHINBROWSER"), "true"),
  port = as.integer(Sys.getenv("RE_SHINY_PORT"))
))
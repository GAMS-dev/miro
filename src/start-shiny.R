
if (identical(tolower(Sys.info()[["sysname"]]), "windows")) {
  assign(".lib.loc", Sys.getenv("R_LIB_PATHS"), envir = environment(.libPaths))
}
ret <- suppressMessages(shiny::runApp(
  Sys.getenv("RE_SHINY_PATH"),
  host = "127.0.0.1",
  launch.browser = identical(Sys.getenv("LAUNCHINBROWSER"), "true"),
  port = as.integer(Sys.getenv("RE_SHINY_PORT"))
))
if (is.integer(ret)) {
  quit("no", ret)
} else {
  quit("no", 0L)
}

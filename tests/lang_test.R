app <- ShinyDriver$new("../", loadTimeout = 20000)
app$snapshotInit(paste0("lang_test_", Sys.getenv("MIRO_LANG")))
Sys.sleep(1L)
app$snapshot(items = list(input = paste0("slider_", c(2L, 2L + 1L))), 
             screenshot = TRUE)
app$stop()

app <- ShinyDriver$new("../", loadTimeout = 20000)
app$snapshotInit(paste0("lang_test_", Sys.getenv("MIRO_LANG")))
app$snapshot(items = list(output = c("inputDataTitle")), screenshot = TRUE)
app$stop()

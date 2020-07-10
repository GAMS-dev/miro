app <- ShinyDriver$new("../", loadTimeout = 1000000)
app$snapshotInit("config_mode_general_settings")

Sys.sleep(2)
app$snapshot(screenshot = TRUE)
app$stop()
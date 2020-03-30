app <- ShinyDriver$new("../", loadTimeout = 10000)
app$snapshotInit(paste0("solve_model_test_", Sys.getenv("GMSMODELNAME")))
if(Sys.getenv("GMSMODELNAME") %in% c("sudoku")){
  app$setInputs(btImport = "click")
  Sys.sleep(1)
  app$setInputs(btLoadScenConfirm = "click")
}
Sys.sleep(2)
app$setInputs(btSolve = "click")
Sys.sleep(10)
app$snapshot(items = list(output = "outputDataTitle"),
             screenshot = TRUE)
expect_error(app$findElement("#outputTableView")$click(), NA)
app$stop()

app <- ShinyDriver$new("../../", loadTimeout = 20000)
app$snapshotInit(paste0("solve_model_test_", Sys.getenv("GMSMODELNAME")))
if(Sys.getenv("GMSMODELNAME") %in% c("sudoku", "tsp", "kport")){
  app$setInputs(btImport = "click")
  Sys.sleep(1)
  app$setInputs(btLoadScenConfirm = "click")
  if(Sys.getenv("GMSMODELNAME") %in% c("tsp")){
    app$setInputs(btOverwriteScen = "click")
  }
}
Sys.sleep(2)
if(Sys.getenv("GMSMODELNAME") %in% c("kport")){
  Sys.sleep(2)
}
app$snapshot(items = list(output = "inputDataTitle"),
             screenshot = TRUE)
app$setInputs(btSolve = "click")
Sys.sleep(10)
expect_error(app$findElement("#outputTableView")$click(), NA)
app$stop()

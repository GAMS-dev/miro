app <- ShinyDriver$new("../", loadTimeout = 10000)
app$snapshotInit(paste0("compare_scen_tab_test_", Sys.getenv("GMSMODELNAME")))

app$setInputs(sidebarMenuId = "scenarios", wait_ = FALSE, values_ = FALSE)
app$setInputs(btSplitView = "click")
app$findElement("#btLoadScen")$click()
Sys.sleep(2)
app$setInputs(selLoadScen = c("1_user", "2_user"), wait_ = FALSE, values_ = FALSE)
app$setInputs(btLoadScenConfirm = "click")
Sys.sleep(2)
app$snapshot(items = list(output = c("title_4", "title_5")), screenshot = TRUE)
app$setInputs(table_5 = "click")
if(identical(Sys.getenv("GMSMODELNAME"), "pickstock_live")){
  app$setInputs(contentScen_5 = "contentScen_5_2")
}
switch(Sys.getenv("GMSMODELNAME"), 
       "pickstock" = {
         vals <- app$getAllValues()
         noStocks_table <- length(vals$input[["tab_5_1-datatable_rows_all"]])
         noStocks_graph <- length(jsonlite::fromJSON(vals$output[["tab_5_1-graph"]])$x$data$labels[[1]])
         expect_gt(noStocks_table, 1)
         expect_gt(noStocks_graph, 1)
       },
       "pickstock_live" = {
         vals <- app$getAllValues()
         noStocks_table <- length(vals$input[["tab_5_2-datatable_rows_all"]])
         noStocks_graph <- length(jsonlite::fromJSON(vals$output[["tab_5_2-graph"]])$x$data$labels[[1]])
         expect_gt(noStocks_table, 1)
         expect_gt(noStocks_graph, 1)
       },
       "transport" = ,
       "transport_live" = {
         app$snapshot(items = list(output = c("tab_5_1-custom-trnsport")), screenshot = TRUE)
       })
app$stop()
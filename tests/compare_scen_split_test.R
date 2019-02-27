app <- ShinyDriver$new("../", loadTimeout = 10000)
app$snapshotInit(paste0("compare_scen_split_test_", Sys.getenv("GMSMODELNAME")))

app$setInputs(btImport = "click")
Sys.sleep(0.5)
app$findElement("#btLoadScenConfirm2")$click()
app$snapshot(items = list(output = c("in_1")), screenshot = TRUE)
app$findElement("a[data-value='scenarios']")$click()
app$setInputs(btScenSplit1_open = "click")
Sys.sleep(0.5)
app$setInputs(btLoadScenConfirm = "click")
Sys.sleep(2)
app$setInputs(btScenSplit2_open = "click")
Sys.sleep(0.5)
app$setInputs(btLoadScenConfirm = "click")
Sys.sleep(2)
switch(Sys.getenv("GMSMODELNAME"), 
       "pickstock" = {
          vals <- app$getAllValues()$input
          noStocks_left <- length(vals[["tab_2_1-datatable_rows_all"]])
          noStocks_right <- length(vals[["tab_3_1-datatable_rows_all"]])
          expect_gt(noStocks_left, 1)
          expect_gt(noStocks_right, 1)
       },
       "pickstock_live" = {
         app$setInputs(btCompareScen = "click")
         app$setInputs(contentScen_2 = "contentScen_2_2")
         Sys.sleep(2)
         vals <- app$getAllValues()$input
         noStocks_left <- length(vals[["tab_2_2-datatable_rows_all"]])
         noStocks_right <- length(vals[["tab_3_2-datatable_rows_all"]])
         expect_gt(noStocks_left, 1)
         expect_gt(noStocks_right, 1)
       },
       "transport" = ,
       "transport_live" = {
         app$setInputs(btCompareScen = "click")
         app$setInputs(table_2 = "click")
         Sys.sleep(2)
         vals <- app$getAllValues()$input
         noRows_left  <- length(vals[["table_tab_2_1-datatable_rows_all"]])
         noRows_right <- length(vals[["table_tab_3_1-datatable_rows_all"]])
         expect_equal(noRows_left, 6)
         expect_equal(noRows_right, 6)
       })
app$snapshot(items = list(output = c("title_2", "title_3")), screenshot = TRUE)
app$stop()

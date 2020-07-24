app <- ShinyDriver$new("../", loadTimeout = 20000)
app$snapshotInit("miropivot_test")

getData <- function(){
  return(jsonlite::fromJSON(app$getAllValues()$output[["tab_1-miroPivot-pivotChart"]])$x$data$datasets$data)
}
Sys.sleep(2)
app$findElement("a[data-value='outputData']")$click()
Sys.sleep(1)
app$snapshot(items = list(output = "outputDataTitle"), 
             screenshot = TRUE)
expect_equal(getData(), list(c(NA, 300), c(275, 50), c(275, NA)))
app$stop()

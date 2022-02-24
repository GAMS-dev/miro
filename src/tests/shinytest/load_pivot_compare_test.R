app <- ShinyDriver$new("../../", loadTimeout = 20000)
app$snapshotInit("load_pivot_compare_test")

currentUser <- Sys.info()[["user"]]

app$findElement('a[data-value="scenarios"]')$click()
app$findElement(".btSplitView button")$click()
app$findElements(".btSplitView a[data-view='tab']")[[1]]$click()
Sys.sleep(0.5)

app$findElement("#cmpTabNoScenWrapper .action-button")$click()
Sys.sleep(0.5)
scenToSelect <- paste0(c("1_", "3_"), currentUser)
app$setInputs(selLoadScen = scenToSelect, wait_ = FALSE, values_ = FALSE)
app$setInputs(btLoadScenConfirm = "click")
Sys.sleep(2)

# scenarios loaded in tab view: 1, 3

# load scenarios into split view (1 from tab view, 1 from db)
app$findElement(".btSplitView button")$click()
app$findElements(".btSplitView a[data-view='split']")[[1]]$click()

app$setInputs(btScenSplit1_open = "click")
Sys.sleep(0.5)
app$setInputs(tabsetLoadScen = "loadScenUI")
expect_options(getSelectizeOptions(app, "#selLoadScenUI"), scenToSelect)
app$setInputs(selLoadScenUI = paste0("1_", currentUser))
app$setInputs(btLoadScenConfirm = "click")
Sys.sleep(0.5)

app$setInputs(btScenSplit2_open = "click")
Sys.sleep(0.5)
expect_options(
  getSelectizeOptions(app, "#selLoadScen"),
  paste0(c("2_", "3_", "4_"), currentUser)
)
app$setInputs(selLoadScen = paste0("4_", currentUser))
app$setInputs(btLoadScenConfirm = "click")
Sys.sleep(0.5)
expect_true(app$waitFor("$('#cmpScenTitle_2').text()==='default1';", timeout = 50))
expect_true(app$waitFor("$('#cmpScenTitle_3').text()==='default4';", timeout = 50))
# scenarios loaded in split view: 1, 4

# load scenarios into pivot view
app$findElement(".btSplitView button")$click()
app$findElements(".btSplitView a[data-view='pivot']")[[1]]$click()

app$findElement("#pivotCompBtWrapper button")$click()
Sys.sleep(0.5)
expect_options(
  app$getValue("selLoadScen"),
  c(scenToSelect, paste0("4_", currentUser))
)
app$setInputs(btLoadScenConfirm = "click")
Sys.sleep(1)
app$setInputs(contentScen_0 = "contentScen_0_4")
Sys.sleep(0.5)
app$setInputs("tab_0_3-miroPivot-pivotRenderer" = "stackedbar")
Sys.sleep(0.5)
expect_chartjs(
  app,
  "tab_0_3-miroPivot-pivotChart",
  c(600, 350, 600, 350, 600, 350),
  c(
    "default1.San-Diego",
    "default1.Seattle",
    "default3.San-Diego",
    "default3.Seattle",
    "default4.San-Diego",
    "default4.Seattle"
  )
)
expect_error(app$setInputs(`tab_0_3-miroPivot-hideEmptyCols` = TRUE), NA)
app$findElements("#scen-pivot-view .box-title button")[[1]]$click()
Sys.sleep(0.5)
expect_options(
  app$getValue("selLoadScen"),
  c(scenToSelect, paste0("4_", currentUser))
)
app$setInputs(selLoadScen = paste0(c("1_", "2_"), currentUser))
app$setInputs(btLoadScenConfirm = "click")
Sys.sleep(0.5)
app$setInputs("tab_0_3-miroPivot-pivotRenderer" = "stackedbar")
Sys.sleep(0.5)
expect_chartjs(
  app,
  "tab_0_3-miroPivot-pivotChart",
  c(600, 350, 600, 350),
  c(
    "default1.San-Diego",
    "default1.Seattle",
    "default2.San-Diego",
    "default2.Seattle"
  )
)
app$findElements("#scen-pivot-view .box-title button")[[1]]$click()
Sys.sleep(0.5)
expect_true(app$waitFor("$('#cmpScenTitle_2').text()==='default1';", timeout = 50))
expect_true(app$waitFor("$('#cmpScenTitle_3').text()==='default4';", timeout = 50))
app$snapshot(items = list(output = c("inputDataTitle")), screenshot = TRUE)
expect_options(
  app$getAllValues()$input$selLoadScen,
  paste0(c("1_", "2_"), currentUser)
)
app$findElement("button[data-dismiss='modal']")$click()
Sys.sleep(0.5)
# scenarios loaded in pivot view: 1, 2

# check that both scenarios loaded in tab view as well as pivot view
# are displayed in UI tab in split view
app$findElement(".btSplitView button")$click()
app$findElements(".btSplitView a[data-view='split']")[[1]]$click()
app$setInputs(btScenSplit1_close = "click")
Sys.sleep(0.5)
app$setInputs(btScenSplit1_open = "click")
Sys.sleep(0.5)
app$setInputs(tabsetLoadScen = "loadScenUI")
expect_options(
  getSelectizeOptions(app, "#selLoadScenUI"),
  paste0(c("1_", "2_", "3_"), currentUser)
)
app$findElement("button[data-dismiss='modal']")$click()
Sys.sleep(0.5)

# check that both scenarios loaded in split view as well as pivot view
# are selected when loading scenario in tab view
app$findElement(".btSplitView button")$click()
app$findElements(".btSplitView a[data-view='tab']")[[1]]$click()
Sys.sleep(0.5)

app$findElement("#btLoadScen")$click()
Sys.sleep(0.5)
expect_options(
  getSelectizeOptions(app, "#selLoadScen"),
  paste0(c("2_", "4_", "sb_"), currentUser)
)
app$findElement("button[data-dismiss='modal']")$click()
Sys.sleep(0.5)

app$findElement(".btSplitView button")$click()
app$findElements(".btSplitView a[data-view='pivot']")[[1]]$click()
app$setInputs(btScenPivot_close = "click")
# scenarios loaded in pivot view: none

# check that both scenarios loaded in tab view as well as pivot view
# are displayed in UI tab in split view
app$findElement(".btSplitView button")$click()
app$findElements(".btSplitView a[data-view='split']")[[1]]$click()

app$setInputs(btScenSplit1_open = "click")
Sys.sleep(0.5)
app$setInputs(tabsetLoadScen = "loadScenUI")
expect_options(
  getSelectizeOptions(app, "#selLoadScenUI"),
  paste0(c("1_", "3_"), currentUser)
)
app$findElement("button[data-dismiss='modal']")$click()
Sys.sleep(0.5)

# close all in tab view
app$findElement(".btSplitView button")$click()
app$findElements(".btSplitView a[data-view='tab']")[[1]]$click()
Sys.sleep(0.5)
expect_true(app$waitFor("$('#btCmpTabCloseAll').click();true;", timeout = 50))
Sys.sleep(0.5)
expect_true(app$waitFor("$('.modal-footer .bt-gms-confirm').click();true;", timeout = 50))
Sys.sleep(0.5)
expect_true(app$waitFor("$('#scenTabset li').length===1&&$('#cmpTabNoScenWrapper').is(':visible');", timeout = 50))
app$findElement("#cmpTabNoScenWrapper .action-button")$click()
Sys.sleep(0.5)
expect_true(startsWith(as.character(app$getValue("selLoadScen")), "4_"))

app$stop()

app <- ShinyDriver$new("../../", loadTimeout = 20000)
app$snapshotInit("hcube_engine_test")

authHeader <- paste0("Basic ", 
                     processx::base64_encode(charToRaw(
                       paste0(Sys.getenv("ENGINE_USER"), 
                              ":", Sys.getenv("ENGINE_PASSWORD")))))
getLatestJobDetails <- function(token = NULL){
  httr::content(httr::GET(paste0(Sys.getenv("ENGINE_URL"), "/hypercube/",
                                 if(length(token)) paste0("?hypercube_token=", token)),
                          httr::add_headers(Authorization = authHeader),
                          httr::timeout(2L)),
                type = "application/json", 
                encoding = "utf-8")$results[[1]]
}

context("UI tests - Hypercube Mode with Engine")

#load base scenario
Sys.sleep(3)
app$findElement("#btRemove1")$click()
Sys.sleep(0.5)
app$findElement(".modal-footer .bt-gms-confirm")$click()
Sys.sleep(1)
app$setInputs(btImport = "click")
Sys.sleep(1)
app$setInputs(btLoadScenConfirm = "click")
Sys.sleep(2)
#check/set some widget configurations
app$setInputs(inputTabset = "inputTabset_1")
app$setInputs(inputTabset1 = "inputTabset1_1")
Sys.sleep(1)
app$setInputs(slider_3 = c(1,10), hcubeStep_3 = 1)
app$snapshot(items = list(input = c("slider_3", "hcubeStep_3")), screenshot = TRUE)
app$setInputs(inputTabset1 = "inputTabset1_2")
Sys.sleep(1)
app$setInputs(slider_4 = c(75))
app$setInputs(inputTabset1 = "inputTabset1_4")
Sys.sleep(1)
app$setInputs(hcubeMode_7 = FALSE)
app$setInputs(inputTabset = "inputTabset_3")
Sys.sleep(1)
app$setInputs(dropdown_5 = c("CPLEX"))

app$findElement("#btRemoteExecLogin")$click()
Sys.sleep(1)
app$setInputs(remoteCredUrl = Sys.getenv("ENGINE_URL"))
app$setInputs(remoteCredUser = Sys.getenv("ENGINE_USER"))
app$setInputs(remoteCredPass = Sys.getenv("ENGINE_PASSWORD"))
app$setInputs(remoteCredNs = Sys.getenv("ENGINE_NS"))
app$setInputs(remoteCredReg = FALSE)
app$setInputs(remoteCredRemember = FALSE)
Sys.sleep(0.5)
app$findElement("#shiny-modal .bt-gms-confirm")$click()
Sys.sleep(1)
expect_false(app$waitFor("$('#shiny-modal .btn-default').is(':visible');", timeout = 50))

# 1) submit
app$setInputs(btSolve = "click")
Sys.sleep(2)
addSelectizeOption(app, "#newHcubeTags", "a")
addSelectizeOption(app, "#newHcubeTags", "b")
addSelectizeOption(app, "#newHcubeTags", "c")
addSelectizeOption(app, "#newHcubeTags", "d")
selectSelectizeOption(app, "#newHcubeTags", "a")
selectSelectizeOption(app, "#newHcubeTags", "b")
selectSelectizeOption(app, "#newHcubeTags", "c")
selectSelectizeOption(app, "#newHcubeTags", "d")
app$setInputs(btHcubeAll = "click")
Sys.sleep(7)
app$findElement("#sidebarItemExpanded a[data-value='importData']")$click()
app$findElement('#refreshActiveJobs')$click()
Sys.sleep(1)
expect_true(app$waitFor(paste0("$('#jImport_output td')[0].textContent==='", Sys.info()[["user"]], "'"), timeout = 50)) 
expect_true(app$waitFor("$('#jImport_output td')[2].innerText==='abcd'", timeout = 50))
expect_error(app$findElements("#jImport_output button[onclick*='showJobProgress']")[[1]]$click(), NA)
Sys.sleep(1)
expect_true(app$waitFor("$('#shiny-modal .progress-bar.progress-bar-striped.active').is(':visible');", 50))
expect_error(app$findElement("#shiny-modal .btn-default")$click(), NA)
token <- getLatestJobDetails()$token
timeout <- 600L
repeat{
  if(getLatestJobDetails(token)$status == 10L){
    break
  }
  if(timeout < 560L){
    print("Engine busy.. Waiting..")
  }
  Sys.sleep(2L)
  timeout <- timeout - 2L
  if(timeout <= 0L){
    stop("Engine seems to be busy. Try again later..")
  }
}
app$findElement('#refreshActiveJobs')$click()
Sys.sleep(1)
expect_error(app$findElements("#jImport_output button[onclick*='downloadJob']")[[1]]$click(), NA)
Sys.sleep(10)
if(app$waitFor("$('#shiny-modal').is(':visible');", 50)){
  expect_error(app$findElement("#shiny-modal #btHcubeImportAll")$click(), NA)
}
Sys.sleep(7)
expect_false(identical(app$waitFor("$('#jImport_output td')[2].innerText==='abcd'", timeout = 50), TRUE))

app$stop()

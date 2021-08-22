renderScenInCompMode <- function(scenId, refreshData = FALSE){
  scenIdLong <- paste0("scen_", scenId, "_")
  if(scenId %in% c(2, 3)){
    # hide button and show content
    showEl(session, paste0("#scenSplit", scenId - 1L, "_content"))
    hideEl(session, paste0("#scenSplit", scenId - 1L, "_open"))
  }else if(!refreshData){
    # add new Scenario tab
    insertScenTab("scenTabset", generateScenarioTabsetMulti(scenId), "scen_add", "before",
                  scenID = scenId, scenButtonLang = c(list(tooltip = lang$nav$scen$tooltips$btClose),
                                                      lang$nav[["dialogCloseScen"]]),
                  immediate = TRUE)
    numberScenTabs <<- numberScenTabs + 1L
    if(numberScenTabs == 1L){
      showEl(session, "#btCmpTabCloseAll")
      hideEl(session, "#cmpTabNoScenWrapper")
    }else{
      enableEl(session, "#btCompareScen")
    }
  }
  # update title and date
  metaTmp <- scenData$getById("meta", refId = tabIdToRef(scenId), drop = TRUE)
  showElReplaceTxt(session, paste0("#cmpScenTitle_", scenId),
                   paste0(if(!identical(uid, metaTmp[["_uid"]][1])) paste0(metaTmp[["_uid"]][1], ": "),
                          metaTmp[["_sname"]][1]))
  showElReplaceTxt(session, paste0("#cmpScenDate_", scenId),
                   metaTmp[["_stime"]][1])
  loadDynamicTabContent(session, scenId,
                        getSheetnamesByTabsetId(scenId),
                        initEnv = TRUE)
}

if(identical(tolower(Sys.info()[["sysname"]]), "windows")){
  assign(".lib.loc", Sys.getenv("R_LIB_PATHS"), envir = environment(.libPaths))
}
suppressWarnings(suppressMessages(library(httr, verbose = FALSE, quietly = TRUE)))

currentMIROVersion <- strsplit(commandArgs(TRUE)[[1L]], ".", fixed = TRUE)[[1L]]

versionNo <- GET('https://miro.gams.com/info', timeout(10L))
if(!identical(status_code(versionNo), 200L)){
  quit('no', status = 1)
}
MIROVersionLatestTmp <- strsplit(trimws(content(versionNo)),
                                 ".", fixed = TRUE)[[1L]]
if(length(MIROVersionLatestTmp) != 3L){
  quit('no', status = 1)
}
if(MIROVersionLatestTmp[1] > currentMIROVersion[1] ||
   (MIROVersionLatestTmp[1] == currentMIROVersion[1] && 
    MIROVersionLatestTmp[2] > currentMIROVersion[2]) ||
   (MIROVersionLatestTmp[1] == currentMIROVersion[1] && 
    MIROVersionLatestTmp[2] == currentMIROVersion[2] && 
    MIROVersionLatestTmp[3] > currentMIROVersion[3])){
  MIROVersionLatest <- paste0("<br/><br/><b style='color:#f90;'>A new version of GAMS MIRO is available! The latest version is: v.",
                               MIROVersionLatestTmp[1], ".",
                               MIROVersionLatestTmp[2], ".",
                               MIROVersionLatestTmp[3], 
                               "</b><br/>To download the latest version, click <a href='https://gams.com/miro/' target='_blank'>here</a>")
  write(MIROVersionLatest, stdout())
  quit('no', status = 0)
}
quit('no', status = '1')


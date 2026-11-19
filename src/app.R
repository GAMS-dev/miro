# version number
MIROVersion <- "2.14.0"
APIVersion <- "1"
MIRORDate <- "Nov 19 2026"

if (dir.exists("bytecode")) {
  # use MIRO's R files byte-compiled at build time (build/scripts/compile-r.R)
  source <- function(file, local = FALSE, ...) {
    envir <- if (isTRUE(local)) parent.frame() else if (isFALSE(local)) globalenv() else local
    bytecodeFile <- file.path("bytecode", paste0(sub("^\\./", "", file), "c"))
    if (file.exists(bytecodeFile)) {
      return(compiler::loadcmp(bytecodeFile, envir = envir))
    }
    base::source(file, local = envir, ...)
  }
  # main.R is evaluated as a whole (not via source()) as it returns early via return()
  eval(readRDS(file.path("bytecode", "main.rds")))
} else {
  eval(parse("main.R", encoding = "UTF-8", keep.source = TRUE))
}

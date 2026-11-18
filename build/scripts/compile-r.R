# byte-compile MIRO's R sources, so they don't need to be JIT-compiled on every start
# usage: Rscript compile-r.R <MIRO src directory> <output directory>
args <- commandArgs(trailingOnly = TRUE)
unlink(args[2], recursive = TRUE)
dir.create(args[2], recursive = TRUE)
outDir <- normalizePath(args[2])
setwd(args[1])
files <- c("global.R", list.files(c("components", "modules", "UI"), "\\.R$", recursive = TRUE, full.names = TRUE))
for (file in files) {
  outFile <- file.path(outDir, paste0(file, "c"))
  dir.create(dirname(outFile), recursive = TRUE, showWarnings = FALSE)
  # version 2 does not record the native encoding, so strings are loaded as by source()
  compiler::cmpfile(file, outFile, version = 2)
}

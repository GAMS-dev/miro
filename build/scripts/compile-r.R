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
# main.R is compiled as a single block, so app.R can evaluate it as a whole
saveRDS(compiler::compile(as.call(c(as.name("{"), parse("main.R", encoding = "UTF-8")))),
  file.path(outDir, "main.rds"),
  version = 2
)

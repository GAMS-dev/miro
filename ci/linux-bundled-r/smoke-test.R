pkgs <- setdiff(rownames(installed.packages(.Library)), "tcltk")
for (pkg in pkgs) suppressPackageStartupMessages(library(pkg, character.only = TRUE))
message(length(pkgs), " packages loaded from ", R.home())
stopifnot(curl::curl_fetch_memory("https://cloud.r-project.org")$status_code == 200L)
stopifnot(
  startsWith(curl::curl_version()$ssl_version, "OpenSSL/3"), startsWith(openssl::openssl_config()$version, "OpenSSL 3"),
  nzchar(openssl::md4("x"))
)
message("HTTPS works")
png(plotFile <- tempfile(fileext = ".png"))
plot(1:10, main = "MIRO")
invisible(dev.off())
stopifnot(file.size(plotFile) > 0L)
message("PNG device works")

port <- 3839L
ws <- tempfile()
dir.create(ws)
miro <- processx::process$new(
  file.path(R.home(), "bin", "R"),
  c("--no-echo", "--no-restore", "--vanilla", "-f", file.path("src", "start-shiny.R")),
  env = c("current",
    RE_SHINY_PORT = port, RE_SHINY_PATH = file.path(getwd(), "src"),
    MIRO_MODEL_PATH = file.path(getwd(), "src", "model", "transport", "transport.gms"),
    MIRO_MODE = "base", MIRO_USE_TMP = "true", MIRO_WS_PATH = ws, MIRO_DB_PATH = ws,
    MIRO_LOG_PATH = ws, LAUNCHINBROWSER = "false", LANG = "C.UTF-8", LC_ALL = "C.UTF-8"
  ),
  stdout = "|", stderr = "2>&1"
)
res <- NULL
for (i in seq_len(180L)) {
  if (!miro$is_alive()) break
  res <- tryCatch(curl::curl_fetch_memory(paste0("http://127.0.0.1:", port)), error = function(e) NULL)
  if (!is.null(res)) break
  Sys.sleep(1)
}
invisible(miro$kill(close_connections = FALSE))
if (is.null(res) || res$status_code != 200L || !grepl("GAMS MIRO", rawToChar(res$content), fixed = TRUE)) {
  stop("MIRO did not start. Output:\n", miro$read_all_output())
}
message("MIRO UI served on port ", port)

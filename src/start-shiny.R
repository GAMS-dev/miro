launchBrowserParam <- identical(Sys.getenv("LAUNCHINBROWSER"), "true")
if (identical(tolower(Sys.info()[["sysname"]]), "windows")) {
  assign(".lib.loc", Sys.getenv("R_LIB_PATHS"), envir = environment(.libPaths))
} else if (identical(launchBrowserParam, TRUE) && !identical(Sys.info()["sysname"], "Darwin")) {
  launchBrowserParam <- function(url) {
    # Modified version of utils::browseURL (Copyright (C) 2022 The R Foundation for Statistical Computing)
    # under GPL-3 license
    # it was modified to change to user's home directory before spawning browser
    # since AppArmor would prevent spawning browser from directory that is not accessible
    # as is the case when running inside AppImage
    # (e.g. calls like `xdg-open https://gams.com` from inside AppImage would fail,
    # thus we cd to user's home directory first)

    browser <- getOption("browser")

    ## Unix-alike, character "browser"
    if (.Platform$GUI == "AQUA" ||
      length(grep("^(localhost|):", Sys.getenv("DISPLAY")))) {
      isLocal <- TRUE
    } else {
      isLocal <- FALSE
    }

    ## escape characters.  ' can occur in URLs, so we must use " to
    ## delimit the URL.  We need to escape $, but "`\ do not occur in
    ## valid URLs (RFC 2396, on the W3C site).
    .shQuote <- function(string) {
      paste0('"', gsub("$", "\\$", string, fixed = TRUE), '"')
    }
    quotedUrl <- .shQuote(url)
    remoteCmd <- if (isLocal) {
      switch(basename(browser),
        "gnome-moz-remote" = ,
        "open" = quotedUrl,
        "galeon" = paste("-x", quotedUrl),
        "kfmclient" = paste("openURL", quotedUrl),
        "mozilla" = ,
        "opera" = {
          paste0(
            "-remote \"openURL(",
            ## Quote ',' and ')' ...
            gsub("([,)$])", "%\\1", url), ")\""
          )
        },
        quotedUrl
      )
    } else {
      quotedUrl
    }
    system(paste(
      "cd ~ &&", browser, remoteCmd, "> /dev/null 2>&1 ||",
      browser, quotedUrl, "&"
    ))
  }
}
ret <- suppressMessages(shiny::runApp(
  Sys.getenv("RE_SHINY_PATH"),
  host = "127.0.0.1",
  launch.browser = launchBrowserParam,
  port = as.integer(Sys.getenv("RE_SHINY_PORT"))
))
if (is.integer(ret)) {
  quit("no", ret)
} else {
  quit("no", 0L)
}

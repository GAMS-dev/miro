hex_to_hsl <- function(hex) {
  hls <- as(colorspace::hex2RGB(hex), "HLS")
  cc <- colorspace::coords(hls)
  setNames(
    c(
      unname(cc[, "H"]),
      unname(cc[, "S"]) * 100,
      unname(cc[, "L"]) * 100
    ),
    c("h", "s", "l")
  )
}

hue <- function(hex) unname(hex_to_hsl(hex)["h"])
saturation <- function(hex) unname(hex_to_hsl(hex)["s"])

hsl_hex <- function(h, s, l) {
  colorspace::hex(colorspace::HLS(H = h, S = s / 100, L = l / 100))
}

hex_to_rgba <- function(hex, alpha = 1) {
  hex <- as.character(hex)
  rgb <- col2rgb(hex)
  alpha <- format(alpha, trim = TRUE)
  sprintf(
    "rgba(%d,%d,%d,%s)",
    rgb["red", ], rgb["green", ], rgb["blue", ], alpha
  )
}

lighten <- function(col, p, method = "absolute") {
  colorspace::lighten(col, amount = p / 100, method, space = "HCL")
}
darken <- function(col, p, method = "absolute") {
  colorspace::darken(col, amount = p / 100, method, space = "HCL")
}

fade <- function(col, p) scales::alpha(col, p / 100)

luma <- function(hex) {
  rgb <- col2rgb(hex) / 255
  c <- ifelse(rgb <= .03928, rgb / 12.92, ((rgb + .055) / 1.055)^2.4)
  (0.2126 * c[1, ] + 0.7152 * c[2, ] + 0.0722 * c[3, ]) * 100
}

contrast <- function(bg, dark = "#000000", light = "#ffffff") {
  unname(ifelse(luma(bg) > 50, dark, light))
}

rel_luma <- function(hex) {
  rgb <- farver::decode_colour(hex, to = "rgb") / 255
  c <- ifelse(rgb <= .03928, rgb / 12.92,
    ((rgb + .055) / 1.055)^2.4
  )
  0.2126 * c[, 1] + 0.7152 * c[, 2] + 0.0722 * c[, 3]
}

contrast_ratio <- function(col1, col2) {
  L1 <- rel_luma(col1)
  L2 <- rel_luma(col2)
  if (L1 < L2) {
    tmp <- L1
    L1 <- L2
    L2 <- tmp
  }
  (L1 + 0.05) / (L2 + 0.05)
}

good_contrast <- function(fg, bg, threshold = 3.5) {
  contrast_ratio(fg, bg) >= threshold
}

boolean <- function(x) {
  val <- as.logical(x)[1]
  if (is.na(val) || length(val) == 0) FALSE else val
}

make_hsl <- function(color, l, s) hsl_hex(hue(color), s, l)

resolveColor <- function(val, default) {
  if (length(val) && nzchar(val)) val else default
}

getThemeColors <- function(cssInput) {
  if (is.null(cssInput)) {
    return(NULL)
  }

  if ((is.list(cssInput) || is.atomic(cssInput)) && !is.null(names(cssInput))) {
    varNames <- gsub("-", "_", names(cssInput))
    varValues <- as.character(unlist(cssInput, use.names = FALSE))
    return(as.list(setNames(varValues, varNames)))
  }

  cssLines <- if (length(cssInput) == 1 && file.exists(cssInput)) {
    readLines(cssInput, warn = FALSE)
  } else if (is.character(cssInput)) {
    as.character(cssInput)
  } else {
    return(NULL)
  }

  regexPattern <- '^\\s*--([[:alnum:]_-]+)\\s*:\\s*"?([#0-9A-Fa-f]{3,8})"?\\s*;?\\s*$'

  matchResults <- regexec(regexPattern, cssLines)
  parts <- regmatches(cssLines, matchResults)
  validParts <- parts[vapply(parts, length, integer(1)) == 3]

  if (!length(validParts)) {
    return(NULL)
  }
  varNames <- gsub("-", "_", vapply(validParts, `[`, FUN.VALUE = "", 2))
  varValues <- vapply(validParts, `[`, FUN.VALUE = "", 3)
  as.list(setNames(varValues, varNames))
}

baseColors <- list(
  primary_color           = "#3c8dbc",
  secondary_color         = "#f39619",
  sidebar_color           = "#1d2121",
  navbar_color            = "#ffffff",
  body_bg_color           = "#ECF1F4",
  alert_color             = "#d11a2a",
  main_bg                 = "#ffffff",
  console_text_color      = "#333333",
  primary_color_dark      = "#00adb5",
  secondary_color_dark    = "#f39619",
  sidebar_color_dark      = "#1d1f20",
  navbar_color_dark       = "#1d2020",
  body_bg_color_dark      = "#292D32",
  alert_color_dark        = "#d11a2a",
  main_bg_dark            = "#393e46",
  console_text_color_dark = "#3c8dbc",
  widget_bg_dark          = "#848991",
  text_color              = "#eeeeee",
  text_color_dark         = "#eeeeee"
)

themeCss <- if (!is.null(configJSON$themeColors) && length(configJSON$themeColors)) {
  configJSON$themeColors
} else {
  globalTheme <- normalizePath(file.path(getwd(), "www", paste0("colors_", miroColorTheme, ".css")))
  if (file.exists(globalTheme)) globalTheme else NULL
}

if (!is.null(themeCss)) {
  parsedColors <- getThemeColors(themeCss)
  commonKeys <- intersect(names(parsedColors), names(baseColors))
  if (length(commonKeys) > 0) {
    baseColors[commonKeys] <- parsedColors[commonKeys]
  }
}

customBaseColors <- reactive({
  list(
    primary_color           = resolveColor(input$primary_color, baseColors$primary_color),
    secondary_color         = resolveColor(input$secondary_color, baseColors$secondary_color),
    sidebar_color           = resolveColor(input$sidebar_color, baseColors$sidebar_color),
    navbar_color            = resolveColor(input$navbar_color, baseColors$navbar_color),
    body_bg_color           = resolveColor(input$body_bg_color, baseColors$body_bg_color),
    alert_color             = resolveColor(input$alert_color, baseColors$alert_color),
    main_bg                 = "#ffffff",
    console_text_color      = resolveColor(input$console_text_color, baseColors$console_text_color),
    primary_color_dark      = resolveColor(input$primary_color_dark, baseColors$primary_color_dark),
    secondary_color_dark    = resolveColor(input$secondary_color_dark, baseColors$secondary_color_dark),
    sidebar_color_dark      = resolveColor(input$sidebar_color_dark, baseColors$sidebar_color_dark),
    navbar_color_dark       = resolveColor(input$navbar_color_dark, baseColors$navbar_color_dark),
    body_bg_color_dark      = resolveColor(input$body_bg_color_dark, baseColors$body_bg_color_dark),
    alert_color_dark        = resolveColor(input$alert_color_dark, baseColors$alert_color_dark),
    main_bg_dark            = resolveColor(input$main_bg_dark, baseColors$main_bg_dark),
    console_text_color_dark = resolveColor(input$console_text_color_dark, baseColors$console_text_color_dark),
    widget_bg_dark          = resolveColor(input$widget_bg_dark, baseColors$widget_bg_dark),
    text_color              = "#eeeeee",
    text_color_dark         = "#eeeeee"
  )
})

derive_palette <- function(b = baseColors) {
  v <- list()
  # main colors
  primary_color <- b$primary_color
  secondary_color <- b$secondary_color
  sidebar_color <- b$sidebar_color
  navbar_color <- b$navbar_color
  body_bg_color <- b$body_bg_color
  alert_color <- b$alert_color
  console_text_color <- b$console_text_color
  main_bg <- b$main_bg
  primary_color_dark <- b$primary_color_dark
  secondary_color_dark <- b$secondary_color_dark
  sidebar_color_dark <- b$sidebar_color_dark
  navbar_color_dark <- b$navbar_color_dark
  body_bg_color_dark <- b$body_bg_color_dark
  alert_color_dark <- b$alert_color_dark
  console_text_color_dark <- b$console_text_color_dark
  main_bg_dark <- b$main_bg_dark
  widget_bg_dark <- b$widget_bg_dark
  text_color <- b$text_color
  text_color_dark <- b$text_color_dark
  v[["primary_color"]] <- primary_color
  v[["console_text_color"]] <- console_text_color
  v[["secondary_color"]] <- secondary_color
  v[["alert_color"]] <- alert_color
  v[["sidebar_color"]] <- sidebar_color
  v[["navbar_color"]] <- navbar_color
  v[["body_bg_color"]] <- body_bg_color
  v[["main_bg"]] <- main_bg
  v[["primary_color_dark"]] <- primary_color_dark
  v[["console_text_color_dark"]] <- console_text_color_dark
  v[["secondary_color_dark"]] <- secondary_color_dark
  v[["alert_color_dark"]] <- alert_color_dark
  v[["sidebar_color_dark"]] <- sidebar_color_dark
  v[["navbar_color_dark"]] <- navbar_color_dark
  v[["body_bg_color_dark"]] <- body_bg_color_dark
  v[["main_bg_dark"]] <- main_bg_dark
  v[["widget_bg_dark"]] <- widget_bg_dark
  v[["text_color"]] <- text_color
  v[["text_color_dark"]] <- text_color_dark

  # booleans
  boolean_486b1f24 <- boolean(luma(b$sidebar_color) > 50)
  boolean_1250eb71 <- boolean(luma(b$primary_color) > 50)
  boolean_19a3fbf5 <- boolean(hue(b$primary_color_dark) == 0 & saturation(b$primary_color_dark) == 0)
  boolean_880ef169 <- boolean(hue(b$main_bg_dark) == 0 & saturation(b$main_bg_dark) == 0)
  boolean_9cccc45f <- boolean(hue(b$alert_color) == 0 & saturation(b$alert_color) == 0)
  boolean_a6a32216 <- boolean(luma(b$primary_color_dark) > 50)
  boolean_b05e9519 <- boolean(hue(b$primary_color) == 0 & saturation(b$primary_color) == 0)
  boolean_2e46d206 <- boolean(hue(b$secondary_color) == 0 & saturation(b$secondary_color) == 0)

  # primary colors
  fade_193caeb6 <- fade(b$primary_color, 69)
  v[["fade_193caeb6"]] <- fade_193caeb6
  fade_f865be76 <- fade(b$primary_color, 6)
  v[["fade_f865be76"]] <- fade_f865be76
  lighten_76740698 <- lighten(b$primary_color, 15)
  v[["lighten_76740698"]] <- lighten_76740698
  darken_cd55fe88 <- darken(b$primary_color, 6.5)
  v[["darken_cd55fe88"]] <- darken_cd55fe88
  darken_8f2deed1 <- darken(b$primary_color, 6)
  v[["darken_8f2deed1"]] <- darken_8f2deed1
  darken_c2f685b8 <- darken(b$primary_color, 5)
  v[["darken_c2f685b8"]] <- darken_c2f685b8
  darken_c33ee2c5 <- darken(b$primary_color, 10)
  v[["darken_c33ee2c5"]] <- darken_c33ee2c5
  if_0f08a484 <- if (boolean_b05e9519) hsl_hex(hue(b$primary_color), 0, 45) else hsl_hex(hue(b$primary_color), 39, 51)
  v[["if_0f08a484"]] <- if_0f08a484
  darken_1673785d <- darken(if_0f08a484, 10)
  v[["darken_1673785d"]] <- darken_1673785d
  if_09b7c5a1 <- if (boolean_b05e9519) make_hsl(b$primary_color, 30, 0) else if (boolean_1250eb71) make_hsl(b$primary_color, 40, 84) else make_hsl(b$primary_color, 40, 40)
  v[["if_09b7c5a1"]] <- if_09b7c5a1
  if_1f2de522 <- if (boolean_b05e9519) make_hsl(b$primary_color, 20, 0) else make_hsl(b$primary_color, 34, 96)
  v[["if_1f2de522"]] <- if_1f2de522
  if_249e30a5 <- if (boolean_b05e9519) make_hsl(b$primary_color, 18, 0) else make_hsl(b$primary_color, 31, 85)
  v[["if_249e30a5"]] <- if_249e30a5
  if_2db96ace <- if (boolean_b05e9519) make_hsl(b$primary_color, 15, 0) else make_hsl(b$primary_color, 25, 85)
  v[["if_2db96ace"]] <- if_2db96ace
  if_30a3c04d <- if (boolean_b05e9519) make_hsl(b$primary_color, 76, 0) else make_hsl(b$primary_color, 88, 52)
  v[["if_30a3c04d"]] <- if_30a3c04d
  if_4ab14308 <- if (boolean_b05e9519) make_hsl(b$primary_color, 13, 0) else make_hsl(b$primary_color, 16, 21)
  v[["if_4ab14308"]] <- if_4ab14308
  if_5ac7f651 <- if (boolean_b05e9519) make_hsl(b$primary_color, 50, 0) else make_hsl(b$primary_color, 57, 69)
  v[["if_5ac7f651"]] <- if_5ac7f651
  if_8e425bfe <- if (boolean_b05e9519) make_hsl(b$primary_color, 80, 0) else make_hsl(b$primary_color, 94, 26)
  v[["if_8e425bfe"]] <- if_8e425bfe
  if_91176f10 <- if (boolean_b05e9519) make_hsl(b$primary_color, 44, 0) else make_hsl(b$primary_color, 49, 100)
  v[["if_91176f10"]] <- if_91176f10
  if_ae6884db <- if (boolean_b05e9519) make_hsl(b$primary_color, 50, 0) else make_hsl(b$primary_color, 73, 32)
  v[["if_ae6884db"]] <- if_ae6884db
  if_b2819be2 <- if (boolean_b05e9519) make_hsl(b$primary_color, 30, 0) else if (boolean_1250eb71) make_hsl(b$primary_color, 40, 84) else make_hsl(b$primary_color, 35, 40)
  v[["if_b2819be2"]] <- if_b2819be2
  if_b3ce36b1 <- if (boolean_b05e9519) make_hsl(b$primary_color, 25, 0) else make_hsl(b$primary_color, 46, 75)
  v[["if_b3ce36b1"]] <- if_b3ce36b1
  if_c6c8bc62 <- if (boolean_b05e9519) make_hsl(b$primary_color, 40, 0) else make_hsl(b$primary_color, 45, 100)
  v[["if_c6c8bc62"]] <- if_c6c8bc62
  if_d440a364 <- if (boolean_b05e9519) make_hsl(b$primary_color, 51, 0) else make_hsl(b$primary_color, 74, 66)
  v[["if_d440a364"]] <- if_d440a364
  if_ed1f2135 <- if (boolean_b05e9519) make_hsl(b$primary_color, 10, 0) else make_hsl(b$primary_color, 14, 85)
  v[["if_ed1f2135"]] <- if_ed1f2135
  if_ed914f1d <- if (abs(luma(b$main_bg) - luma(b$primary_color)) < 15) contrast(b$main_bg) else b$primary_color
  v[["if_ed914f1d"]] <- if_ed914f1d
  if_7dea713b <- if (abs(luma(b$primary_color) - luma("#333333")) < 25) contrast(b$primary_color) else "#333333"
  v[["if_7dea713b"]] <- if_7dea713b
  if_5bc80e62 <- if (good_contrast("#ffffff", b$primary_color)) "#ffffff" else contrast(b$primary_color)
  v[["if_5bc80e62"]] <- if_5bc80e62
  if_8d03604f <- if (good_contrast(b$primary_color, "#ffffff", threshold = 3)) b$primary_color else contrast("#ffffff")
  v[["if_8d03604f"]] <- if_8d03604f
  lighten_6a0cbe6d <- lighten(darken_cd55fe88, 40)
  v[["lighten_6a0cbe6d"]] <- lighten_6a0cbe6d
  darken_28f7e606 <- darken(darken_cd55fe88, 5)
  v[["darken_28f7e606"]] <- darken_28f7e606
  darken_686ca693 <- darken(darken_cd55fe88, 7.5)
  v[["darken_686ca693"]] <- darken_686ca693
  darken_618fc861 <- darken(darken_cd55fe88, 10)
  v[["darken_618fc861"]] <- darken_618fc861
  darken_111221b2 <- darken(darken_cd55fe88, 15)
  v[["darken_111221b2"]] <- darken_111221b2
  darken_b5221a2c <- darken(darken_cd55fe88, 17)
  v[["darken_b5221a2c"]] <- darken_b5221a2c
  if_dc5cf808 <- if (abs(luma(if_b2819be2) - luma("#ffffff")) < 15) contrast(if_b2819be2) else "#ffffff"
  v[["if_dc5cf808"]] <- if_dc5cf808
  if_fa310009 <- if (abs(luma(fade_193caeb6) - luma("#333333")) < 8) contrast(fade_193caeb6) else "#333333"
  v[["if_fa310009"]] <- if_fa310009
  darken_ec72c039 <- darken(darken_28f7e606, 12)
  v[["darken_ec72c039"]] <- darken_ec72c039
  darken_4b80b5c4 <- darken(darken_28f7e606, 25)
  v[["darken_4b80b5c4"]] <- darken_4b80b5c4
  notification_bg <- if (abs(luma("#ffffff") - luma(lighten(b$primary_color, 40))) > 5) lighten(b$primary_color, 40) else lighten(b$primary_color, 35)
  v[["notification_bg"]] <- notification_bg
  notification_text <- if (good_contrast(darken(b$primary_color, 39), notification_bg)) darken(b$primary_color, 39) else contrast(notification_bg)
  v[["notification_text"]] <- notification_text
  notification_border <- darken(b$primary_color, 7)
  v[["notification_border"]] <- notification_border

  fade_041bf36e <- fade(b$primary_color_dark, 69)
  v[["fade_041bf36e"]] <- fade_041bf36e
  fade_09c43b35 <- fade(b$primary_color_dark, 75)
  v[["fade_09c43b35"]] <- fade_09c43b35
  fade_b4a0a1c1 <- fade(b$primary_color, 60)
  v[["fade_b4a0a1c1"]] <- fade_b4a0a1c1
  lighten_360db171 <- lighten(b$primary_color_dark, 15)
  v[["lighten_360db171"]] <- lighten_360db171
  darken_0efbf01c <- darken(b$primary_color_dark, 2)
  v[["darken_0efbf01c"]] <- darken_0efbf01c
  darken_eccec2f4 <- darken(b$primary_color_dark, 5)
  v[["darken_eccec2f4"]] <- darken_eccec2f4
  darken_a8b8ac97 <- darken(b$primary_color_dark, 6)
  v[["darken_a8b8ac97"]] <- darken_a8b8ac97
  darken_411e4a78 <- darken(b$primary_color_dark, 6.5)
  v[["darken_411e4a78"]] <- darken_411e4a78
  darken_e354f9b5 <- darken(b$primary_color_dark, 10)
  v[["darken_e354f9b5"]] <- darken_e354f9b5
  darken_57c981c3 <- darken(b$primary_color_dark, 36, method = "relative")
  v[["darken_57c981c3"]] <- darken_57c981c3
  if_190bbef8 <- if (abs(luma(fade(b$primary_color_dark, 6)) - luma(b$main_bg_dark)) < 5) fade(b$primary_color_dark, 10) else fade(b$primary_color_dark, 6)
  v[["if_190bbef8"]] <- if_190bbef8
  if_7f94f006 <- if (boolean_19a3fbf5) hsl_hex(hue(b$primary_color_dark), 0, 45) else hsl_hex(hue(b$primary_color_dark), 34, 60)
  v[["if_7f94f006"]] <- if_7f94f006
  if_8215301a <- if (abs(luma(b$main_bg_dark) - luma(darken(b$primary_color_dark, 10))) < 8) lighten(b$primary_color_dark, 50) else darken(b$primary_color_dark, 10)
  v[["if_8215301a"]] <- if_8215301a
  if_d2f31bc0 <- if (abs(luma(b$main_bg_dark) - luma(b$primary_color_dark)) < 15) contrast(b$main_bg_dark) else b$primary_color_dark
  v[["if_d2f31bc0"]] <- if_d2f31bc0
  if_ea8983c1 <- if (abs(luma(b$primary_color_dark) - luma(b$text_color_dark)) < 8) contrast(b$primary_color_dark) else b$text_color_dark
  v[["if_ea8983c1"]] <- if_ea8983c1
  if_deaa6f9b <- if (boolean_19a3fbf5) "#1d1f20" else if (boolean_a6a32216) hsl_hex(hue(b$primary_color_dark), 84, 40) else darken_0efbf01c
  v[["if_deaa6f9b"]] <- if_deaa6f9b
  if_e44576b0 <- if (boolean_b05e9519) hsl_hex(hue(b$primary_color), 0, 35) else hsl_hex(hue(b$primary_color), 76, 62)
  v[["if_e44576b0"]] <- if_e44576b0
  if_e0e5aa9e <- if (abs(luma(b$primary_color_dark) - luma("#eeeeee")) < 25) contrast(b$primary_color_dark) else "#eeeeee"
  v[["if_e0e5aa9e"]] <- if_e0e5aa9e
  if_v8c57865 <- if (good_contrast(b$text_color_dark, b$primary_color_dark)) b$text_color_dark else contrast(b$primary_color_dark)
  v[["if_v8c57865"]] <- if_v8c57865
  darken_e71e1e83 <- darken(darken_411e4a78, 5)
  v[["darken_e71e1e83"]] <- darken_e71e1e83
  darken_553812b2 <- darken(darken_411e4a78, 7.5)
  v[["darken_553812b2"]] <- darken_553812b2
  darken_618fc862 <- darken(darken_411e4a78, 10)
  v[["darken_618fc862"]] <- darken_618fc862
  darken_87dfbfea <- darken(darken_411e4a78, 15)
  v[["darken_87dfbfea"]] <- darken_87dfbfea
  fade_304ae92d <- fade(if_e44576b0, 75)
  v[["fade_304ae92d"]] <- fade_304ae92d
  darken_8bbacd34 <- darken(if_7f94f006, 10)
  v[["darken_8bbacd34"]] <- darken_8bbacd34
  darken_e89a054b <- darken(if_deaa6f9b, 5, method = "relative")
  v[["darken_e89a054b"]] <- darken_e89a054b
  if_e33a6962 <- if (abs(luma(if_deaa6f9b) - luma("#333333")) < 15) contrast(if_deaa6f9b) else "#333333"
  v[["if_e33a6962"]] <- if_e33a6962
  if_9ad4a640 <- if (abs(luma(fade_041bf36e) - luma("#eeeeee")) < 8) contrast(fade_041bf36e) else "#eeeeee"
  v[["if_9ad4a640"]] <- if_9ad4a640
  notification_bg_dark <- if (good_contrast(b$text_color_dark, darken(b$primary_color_dark, 35))) darken(b$primary_color_dark, 35) else darken(b$primary_color_dark, 55)
  v[["notification_bg_dark"]] <- notification_bg_dark

  # secondary colors
  darken_75d40b33 <- darken(b$secondary_color, 5)
  v[["darken_75d40b33"]] <- darken_75d40b33
  darken_3fd83bc2 <- darken(b$secondary_color, 6)
  v[["darken_3fd83bc2"]] <- darken_3fd83bc2
  darken_8e82a497 <- darken(b$secondary_color, 10)
  v[["darken_8e82a497"]] <- darken_8e82a497
  if_10618845 <- if (good_contrast(b$secondary_color, b$sidebar_color, threshold = 1.1)) b$secondary_color else contrast(b$sidebar_color, light = lighten(b$secondary_color, 20), dark = darken(b$secondary_color, 20))
  v[["if_10618845"]] <- if_10618845
  if_1b7f3b8b <- if (abs(luma(b$sidebar_color) - luma(b$secondary_color)) < 8) contrast(b$sidebar_color) else "#ADADAD"
  v[["if_1b7f3b8b"]] <- if_1b7f3b8b
  if_6f85b072 <- if (abs(luma(b$secondary_color) - luma("#333333")) < 8) contrast(b$secondary_color) else "#333333"
  v[["if_6f85b072"]] <- if_6f85b072
  if_b318da33 <- if (boolean_2e46d206) hsl_hex(hue(b$secondary_color), 0, 85) else hsl_hex(hue(b$secondary_color), 77, 90)
  v[["if_b318da33"]] <- if_b318da33
  if_035e91c6 <- if (abs(luma(if_b318da33) - luma(b$secondary_color)) < 15) contrast(if_b318da33) else b$secondary_color
  v[["if_035e91c6"]] <- if_035e91c6
  if_4bc80e64 <- if (abs(luma(b$secondary_color) - luma(b$text_color)) < 15) contrast(b$secondary_color) else b$text_color
  v[["if_4bc80e64"]] <- if_4bc80e64
  darken_29c4aefc <- darken(if_b318da33, 15)
  v[["darken_29c4aefc"]] <- darken_29c4aefc

  darken_19dd7eda <- darken(b$secondary_color_dark, 5)
  v[["darken_19dd7eda"]] <- darken_19dd7eda
  darken_daccffb8 <- darken(b$secondary_color_dark, 6)
  v[["darken_daccffb8"]] <- darken_daccffb8
  darken_a780cc60 <- darken(b$secondary_color_dark, 10)
  v[["darken_a780cc60"]] <- darken_a780cc60
  if_94dad415 <- if (good_contrast(b$secondary_color_dark, b$sidebar_color_dark, threshold = 1.1)) b$secondary_color_dark else contrast(b$sidebar_color_dark, light = lighten(b$secondary_color_dark, 20), dark = darken(b$secondary_color_dark, 20))
  v[["if_94dad415"]] <- if_94dad415
  if_f57ecf46 <- if (abs(luma(b$sidebar_color_dark) - luma(b$secondary_color_dark)) < 8) contrast(b$sidebar_color_dark) else "#ADADAD"
  v[["if_f57ecf46"]] <- if_f57ecf46
  if_5905ebb4 <- if (abs(luma(b$secondary_color_dark) - luma("#eeeeee")) < 8) contrast(b$secondary_color_dark) else "#eeeeee"
  v[["if_5905ebb4"]] <- if_5905ebb4
  if_dde4565a <- if (boolean_2e46d206) hsl_hex(hue(b$secondary_color_dark), 0, 20) else hsl_hex(hue(b$secondary_color_dark), 50, 25)
  v[["if_dde4565a"]] <- if_dde4565a
  if_3105643b <- if (abs(luma(if_dde4565a) - luma(b$secondary_color_dark)) < 15) contrast(if_dde4565a) else b$secondary_color_dark
  v[["if_3105643b"]] <- if_3105643b
  darken_2ad94c65 <- darken(if_dde4565a, 15)
  v[["darken_2ad94c65"]] <- darken_2ad94c65

  # sidebar colors
  darken_v73ofn52 <- darken(b$sidebar_color, 1)
  v[["darken_v73ofn52"]] <- darken_v73ofn52
  darken_b05e8d17 <- darken(b$sidebar_color, 2)
  v[["darken_b05e8d17"]] <- darken_b05e8d17
  darken_8e899f91 <- darken(b$sidebar_color, 3)
  v[["darken_8e899f91"]] <- darken_8e899f91
  darken_c806e739 <- darken(b$sidebar_color, 4)
  v[["darken_c806e739"]] <- darken_c806e739
  darken_28c2c354 <- darken(b$sidebar_color, 5)
  v[["darken_28c2c354"]] <- darken_28c2c354
  darken_12518b85 <- darken(b$sidebar_color, 7)
  v[["darken_12518b85"]] <- darken_12518b85
  lighten_959ccfc9 <- lighten(b$sidebar_color, 5)
  v[["lighten_959ccfc9"]] <- lighten_959ccfc9
  lighten_f6fb5849 <- lighten(b$sidebar_color, 10)
  v[["lighten_f6fb5849"]] <- lighten_f6fb5849
  lighten_196d441a <- lighten(b$sidebar_color, 20)
  v[["lighten_196d441a"]] <- lighten_196d441a
  lighten_919393a0 <- lighten(b$sidebar_color, 60)
  v[["lighten_919393a0"]] <- lighten_919393a0
  if_f5e54611 <- if (boolean_486b1f24) "#333333" else lighten_919393a0
  v[["if_f5e54611"]] <- if_f5e54611
  if_038054cd <- if (boolean_486b1f24) darken(b$sidebar_color, 2) else lighten(b$sidebar_color, 2)
  v[["if_038054cd"]] <- if_038054cd
  if_1b7f3b8c <- if (good_contrast("#eeeeee", b$sidebar_color)) "#eeeeee" else contrast(b$sidebar_color, dark = "#ADADAD", light = "#ADADAD")
  v[["if_1b7f3b8c"]] <- if_1b7f3b8c
  lighten_39320a59 <- lighten(lighten_959ccfc9, 40)
  v[["lighten_39320a59"]] <- lighten_39320a59
  sidebar_contrast_light <- if (boolean_486b1f24) "#333333" else "#ffffff"
  v[["sidebar_contrast_light"]] <- sidebar_contrast_light
  sidebar_contrast_light2 <- if (boolean_486b1f24) "#222222" else "#ffffff"
  v[["sidebar_contrast_light2"]] <- sidebar_contrast_light2

  navbar_contrast_light <- if (good_contrast("#333333", b$navbar_color)) "#333333" else "#eeeeee"
  v[["navbar_contrast_light"]] <- navbar_contrast_light

  lighten_a006ac60 <- lighten(b$sidebar_color_dark, 2)
  v[["lighten_a006ac60"]] <- lighten_a006ac60
  lighten_4d79a66d <- lighten(b$sidebar_color_dark, 5)
  v[["lighten_4d79a66d"]] <- lighten_4d79a66d
  lighten_8b122426 <- lighten(b$sidebar_color_dark, 10)
  v[["lighten_8b122426"]] <- lighten_8b122426
  lighten_49e49d58 <- lighten(b$sidebar_color_dark, 20)
  v[["lighten_49e49d58"]] <- lighten_49e49d58
  lighten_4f1df671 <- lighten(b$sidebar_color_dark, 60)
  v[["lighten_4f1df671"]] <- lighten_4f1df671
  if_24167817 <- if (good_contrast(lighten_4f1df671, b$sidebar_color_dark)) lighten_4f1df671 else contrast(b$sidebar_color_dark, dark = "#333333")
  v[["if_24167817"]] <- if_24167817
  darken_dc24222b <- darken(b$sidebar_color_dark, 3)
  v[["darken_dc24222b"]] <- darken_dc24222b
  darken_7911e270 <- darken(b$sidebar_color_dark, 4)
  v[["darken_7911e270"]] <- darken_7911e270
  darken_78904a7c <- darken(b$sidebar_color_dark, 5)
  v[["darken_78904a7c"]] <- darken_78904a7c
  darken_f6592ee2 <- darken(b$sidebar_color_dark, 7)
  v[["darken_f6592ee2"]] <- darken_f6592ee2
  if_1b7f3b8d <- if (good_contrast("#eeeeee", b$sidebar_color_dark)) "#eeeeee" else contrast(b$sidebar_color_dark, dark = "#ADADAD", light = "#ADADAD")
  v[["if_1b7f3b8d"]] <- if_1b7f3b8d
  if_417fecde <- if (good_contrast(lighten(b$sidebar_color_dark, 3), b$sidebar_color_dark, threshold = 1.05)) lighten(b$sidebar_color_dark, 3) else contrast(b$sidebar_color_dark, dark = darken(b$sidebar_color_dark, 3), light = lighten(b$sidebar_color_dark, 3))
  v[["if_417fecde"]] <- if_417fecde
  sidebar_contrast_dark <- if (good_contrast("#ffffff", b$sidebar_color_dark)) "#ffffff" else contrast(b$sidebar_color_dark, dark = "#333333")
  v[["sidebar_contrast_dark"]] <- sidebar_contrast_dark
  sidebar_contrast_dark2 <- if (good_contrast("#ffffff", b$sidebar_color_dark)) "#ffffff" else contrast(b$sidebar_color_dark, dark = "#222222")
  v[["sidebar_contrast_dark2"]] <- sidebar_contrast_dark2
  lighten_c5da3167 <- lighten(lighten_4d79a66d, 40)
  v[["lighten_c5da3167"]] <- lighten_c5da3167

  # alert colors
  darken_68f1341c <- darken(b$alert_color, 16, method = "relative")
  v[["darken_68f1341c"]] <- darken_68f1341c
  if_cf579cbb <- if (boolean_9cccc45f) hsl_hex(hue(b$alert_color), 0, 85) else hsl_hex(hue(b$alert_color), 77, 90)
  v[["if_cf579cbb"]] <- if_cf579cbb
  if_ec44ba6b <- if (abs(luma(if_cf579cbb) - luma(b$alert_color)) < 15) contrast(if_cf579cbb) else b$alert_color
  v[["if_ec44ba6b"]] <- if_ec44ba6b
  darken_4879a36f <- darken(b$alert_color_dark, 16, method = "relative")
  v[["darken_4879a36f"]] <- darken_4879a36f
  if_ba47b56b <- if (boolean_9cccc45f) hsl_hex(hue(b$alert_color_dark), 0, 20) else hsl_hex(hue(b$alert_color_dark), 50, 25)
  v[["if_ba47b56b"]] <- if_ba47b56b
  if_3b6cb002 <- if (abs(luma(if_ba47b56b) - luma(b$alert_color_dark)) < 15) contrast(if_ba47b56b) else b$alert_color_dark
  v[["if_3b6cb002"]] <- if_3b6cb002
  darken_8df68996 <- darken(if_ba47b56b, 15)
  v[["darken_8df68996"]] <- darken_8df68996
  darken_a59823b8 <- darken(if_cf579cbb, 15)
  v[["darken_a59823b8"]] <- darken_a59823b8

  # main background colors
  lighten_9ae39279 <- lighten(b$main_bg_dark, 5)
  v[["lighten_9ae39279"]] <- lighten_9ae39279
  darken_62f5e2b6 <- darken(b$main_bg_dark, 1, method = "relative")
  v[["darken_62f5e2b6"]] <- darken_62f5e2b6
  darken_d35a8f8e <- darken(b$main_bg_dark, 20)
  v[["darken_d35a8f8e"]] <- darken_d35a8f8e
  if_72f3068c <- if (abs(luma(b$main_bg_dark) - luma(darken(if_8215301a, 10))) < 8) lighten(if_8215301a, 10) else darken(if_8215301a, 10)
  v[["if_72f3068c"]] <- if_72f3068c
  if_c1a0bfe3 <- if (boolean_880ef169) hsl_hex(hue(b$main_bg_dark), 0, 15) else hsl_hex(hue(b$main_bg_dark), 10, 18)
  v[["if_c1a0bfe3"]] <- if_c1a0bfe3
  if_f1a3b180 <- if (boolean_880ef169) hsl_hex(hue(b$main_bg_dark), 0, 12) else hsl_hex(hue(b$main_bg_dark), 10, 18)
  v[["if_f1a3b180"]] <- if_f1a3b180
  if_4a0e097b <- if (abs(luma(if_f1a3b180) - luma(b$primary_color_dark)) < 8) "#A9A9A9" else if_f1a3b180
  v[["if_4a0e097b"]] <- if_4a0e097b
  fade_ece660c0 <- fade(if_f1a3b180, 56)
  v[["fade_ece660c0"]] <- fade_ece660c0
  if_851d5a45 <- if (abs(luma(b$main_bg_dark) - luma(b$console_text_color_dark)) < 15) contrast(b$main_bg_dark) else b$console_text_color_dark
  v[["if_851d5a45"]] <- if_851d5a45
  fade_5cff14c3 <- fade(b$console_text_color_dark, 90)
  v[["fade_5cff14c3"]] <- fade_5cff14c3
  if_f37627e5 <- if (abs(luma(darken_d35a8f8e) - luma(b$console_text_color_dark)) < 15) contrast(darken_d35a8f8e, "#000000", "#cdcdcd") else b$console_text_color_dark
  v[["if_f37627e5"]] <- if_f37627e5
  if_cb57bb52 <- if (abs(luma(darken_d35a8f8e) - luma(b$console_text_color_dark)) < 15) contrast(darken_d35a8f8e, "#000000", "#cdcdcd") else console_text_color_dark
  v[["if_cb57bb52"]] <- if_cb57bb52

  # widget background colors
  fade_102be7f4 <- fade(b$widget_bg_dark, 17)
  v[["fade_102be7f4"]] <- fade_102be7f4
  darken_970a799e <- darken(b$widget_bg_dark, 10)
  v[["darken_970a799e"]] <- darken_970a799e
  darken_41c1769c <- darken(b$widget_bg_dark, 39, method = "relative")
  v[["darken_41c1769c"]] <- darken_41c1769c
  darken_a6b030af <- darken(darken_41c1769c, 3)
  v[["darken_a6b030af"]] <- darken_a6b030af
  darken_9e0118e0 <- darken(darken_41c1769c, 11, method = "relative")
  v[["darken_9e0118e0"]] <- darken_9e0118e0
  lighten_27d70c85 <- lighten(darken_41c1769c, 6, method = "relative")
  v[["lighten_27d70c85"]] <- lighten_27d70c85
  lighten_37654577 <- lighten(darken_41c1769c, 10, method = "relative")
  v[["lighten_37654577"]] <- lighten_37654577

  if_a8c57869 <- if (abs(luma("#030303") - luma(b$text_color_dark)) < 15) contrast("#030303") else b$text_color_dark
  v[["if_a8c57869"]] <- if_a8c57869

  v
}
css_name <- function(id) {
  out <- id
  out <- gsub("_", "-", out)
  if (startsWith(out, "--")) out else paste0("--", out)
}
serialise <- function(x) {
  if (is.logical(x)) {
    as.character(as.integer(x))
  } else if (is.numeric(x)) {
    format(x, trim = TRUE)
  } else {
    x
  }
}

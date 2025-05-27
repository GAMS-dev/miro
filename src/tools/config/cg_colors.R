customBaseColors <- reactive({
  list(
    miro_primary_color           = resolveColor(input$primary_color, baseColors$miro_primary_color),
    miro_secondary_color         = resolveColor(input$secondary_color, baseColors$miro_secondary_color),
    miro_sidebar_color           = resolveColor(input$sidebar_color, baseColors$miro_sidebar_color),
    miro_navbar_color            = resolveColor(input$navbar_color, baseColors$miro_navbar_color),
    miro_body_bg_color           = resolveColor(input$body_bg_color, baseColors$miro_body_bg_color),
    miro_alert_color             = resolveColor(input$alert_color, baseColors$miro_alert_color),
    miro_main_bg                 = "#ffffff",
    miro_console_text_color      = resolveColor(input$console_text_color, baseColors$miro_console_text_color),
    miro_primary_color_dark      = resolveColor(input$primary_color_dark, baseColors$miro_primary_color_dark),
    miro_secondary_color_dark    = resolveColor(input$secondary_color_dark, baseColors$miro_secondary_color_dark),
    miro_sidebar_color_dark      = resolveColor(input$sidebar_color_dark, baseColors$miro_sidebar_color_dark),
    miro_navbar_color_dark       = resolveColor(input$navbar_color_dark, baseColors$miro_navbar_color_dark),
    miro_body_bg_color_dark      = resolveColor(input$body_bg_color_dark, baseColors$miro_body_bg_color_dark),
    miro_alert_color_dark        = resolveColor(input$alert_color_dark, baseColors$miro_alert_color_dark),
    miro_main_bg_dark            = resolveColor(input$main_bg_dark, baseColors$miro_main_bg_dark),
    miro_console_text_color_dark = resolveColor(input$console_text_color_dark, baseColors$miro_console_text_color_dark),
    miro_widget_bg_dark          = resolveColor(input$widget_bg_dark, baseColors$miro_widget_bg_dark),
    miro_text_color              = "#eeeeee",
    miro_text_color_dark         = "#eeeeee"
  )
})

derivePalette <- function(b = baseColors) {
  v <- list()
  # main colors
  primary_color <- b$miro_primary_color
  secondary_color <- b$miro_secondary_color
  sidebar_color <- b$miro_sidebar_color
  navbar_color <- b$miro_navbar_color
  body_bg_color <- b$miro_body_bg_color
  alert_color <- b$miro_alert_color
  console_text_color <- b$miro_console_text_color
  main_bg <- b$miro_main_bg
  primary_color_dark <- b$miro_primary_color_dark
  secondary_color_dark <- b$miro_secondary_color_dark
  sidebar_color_dark <- b$miro_sidebar_color_dark
  navbar_color_dark <- b$miro_navbar_color_dark
  body_bg_color_dark <- b$miro_body_bg_color_dark
  alert_color_dark <- b$miro_alert_color_dark
  console_text_color_dark <- b$miro_console_text_color_dark
  main_bg_dark <- b$miro_main_bg_dark
  widget_bg_dark <- b$miro_widget_bg_dark
  text_color <- b$miro_text_color
  text_color_dark <- b$miro_text_color_dark
  v[["miro_primary_color"]] <- primary_color
  v[["miro_console_text_color"]] <- console_text_color
  v[["miro_secondary_color"]] <- secondary_color
  v[["miro_alert_color"]] <- alert_color
  v[["miro_sidebar_color"]] <- sidebar_color
  v[["miro_navbar_color"]] <- navbar_color
  v[["miro_body_bg_color"]] <- body_bg_color
  v[["miro_main_bg"]] <- main_bg
  v[["miro_primary_color_dark"]] <- primary_color_dark
  v[["miro_console_text_color_dark"]] <- console_text_color_dark
  v[["miro_secondary_color_dark"]] <- secondary_color_dark
  v[["miro_alert_color_dark"]] <- alert_color_dark
  v[["miro_sidebar_color_dark"]] <- sidebar_color_dark
  v[["miro_navbar_color_dark"]] <- navbar_color_dark
  v[["miro_body_bg_color_dark"]] <- body_bg_color_dark
  v[["miro_main_bg_dark"]] <- main_bg_dark
  v[["miro_widget_bg_dark"]] <- widget_bg_dark
  v[["miro_text_color"]] <- text_color
  v[["miro_text_color_dark"]] <- text_color_dark

  # booleans
  boolean_486b1f24 <- boolean(luma(b$miro_sidebar_color) > 50)
  boolean_1250eb71 <- boolean(luma(b$miro_primary_color) > 50)
  boolean_19a3fbf5 <- boolean(hue(b$miro_primary_color_dark) == 0 & saturation(b$miro_primary_color_dark) == 0)
  boolean_880ef169 <- boolean(hue(b$miro_main_bg_dark) == 0 & saturation(b$miro_main_bg_dark) == 0)
  boolean_9cccc45f <- boolean(hue(b$miro_alert_color) == 0 & saturation(b$miro_alert_color) == 0)
  boolean_a6a32216 <- boolean(luma(b$miro_primary_color_dark) > 50)
  boolean_b05e9519 <- boolean(hue(b$miro_primary_color) == 0 & saturation(b$miro_primary_color) == 0)
  boolean_2e46d206 <- boolean(hue(b$miro_secondary_color) == 0 & saturation(b$miro_secondary_color) == 0)

  # primary colors
  fade_193caeb6 <- fade(b$miro_primary_color, 69)
  v[["miro_fade_193caeb6"]] <- fade_193caeb6
  fade_f865be76 <- fade(b$miro_primary_color, 6)
  v[["miro_fade_f865be76"]] <- fade_f865be76
  lighten_76740698 <- lighten(b$miro_primary_color, 15)
  v[["miro_lighten_76740698"]] <- lighten_76740698
  darken_cd55fe88 <- darken(b$miro_primary_color, 6.5)
  v[["miro_darken_cd55fe88"]] <- darken_cd55fe88
  darken_8f2deed1 <- darken(b$miro_primary_color, 6)
  v[["miro_darken_8f2deed1"]] <- darken_8f2deed1
  darken_c2f685b8 <- darken(b$miro_primary_color, 5)
  v[["miro_darken_c2f685b8"]] <- darken_c2f685b8
  darken_c33ee2c5 <- darken(b$miro_primary_color, 10)
  v[["miro_darken_c33ee2c5"]] <- darken_c33ee2c5
  if_0f08a484 <- if (boolean_b05e9519) hslHex(hue(b$miro_primary_color), 0, 45) else hslHex(hue(b$miro_primary_color), 39, 51)
  v[["miro_if_0f08a484"]] <- if_0f08a484
  darken_1673785d <- darken(if_0f08a484, 10)
  v[["miro_darken_1673785d"]] <- darken_1673785d
  if_09b7c5a1 <- if (boolean_b05e9519) makeHsl(b$miro_primary_color, 30, 0) else if (boolean_1250eb71) makeHsl(b$miro_primary_color, 40, 84) else makeHsl(b$miro_primary_color, 40, 40)
  v[["miro_if_09b7c5a1"]] <- if_09b7c5a1
  if_1f2de522 <- if (boolean_b05e9519) makeHsl(b$miro_primary_color, 20, 0) else makeHsl(b$miro_primary_color, 34, 96)
  v[["miro_if_1f2de522"]] <- if_1f2de522
  if_249e30a5 <- if (boolean_b05e9519) makeHsl(b$miro_primary_color, 18, 0) else makeHsl(b$miro_primary_color, 31, 85)
  v[["miro_if_249e30a5"]] <- if_249e30a5
  if_2db96ace <- if (boolean_b05e9519) makeHsl(b$miro_primary_color, 15, 0) else makeHsl(b$miro_primary_color, 25, 85)
  v[["miro_if_2db96ace"]] <- if_2db96ace
  if_30a3c04d <- if (boolean_b05e9519) makeHsl(b$miro_primary_color, 76, 0) else makeHsl(b$miro_primary_color, 88, 52)
  v[["miro_if_30a3c04d"]] <- if_30a3c04d
  if_4ab14308 <- if (boolean_b05e9519) makeHsl(b$miro_primary_color, 13, 0) else makeHsl(b$miro_primary_color, 16, 21)
  v[["miro_if_4ab14308"]] <- if_4ab14308
  if_5ac7f651 <- if (boolean_b05e9519) makeHsl(b$miro_primary_color, 50, 0) else makeHsl(b$miro_primary_color, 57, 69)
  v[["miro_if_5ac7f651"]] <- if_5ac7f651
  if_8e425bfe <- if (boolean_b05e9519) makeHsl(b$miro_primary_color, 80, 0) else makeHsl(b$miro_primary_color, 94, 26)
  v[["miro_if_8e425bfe"]] <- if_8e425bfe
  if_91176f10 <- if (boolean_b05e9519) makeHsl(b$miro_primary_color, 44, 0) else makeHsl(b$miro_primary_color, 49, 100)
  v[["miro_if_91176f10"]] <- if_91176f10
  if_ae6884db <- if (boolean_b05e9519) makeHsl(b$miro_primary_color, 50, 0) else makeHsl(b$miro_primary_color, 73, 32)
  v[["miro_if_ae6884db"]] <- if_ae6884db
  if_b2819be2 <- if (boolean_b05e9519) makeHsl(b$miro_primary_color, 30, 0) else if (boolean_1250eb71) makeHsl(b$miro_primary_color, 40, 84) else makeHsl(b$miro_primary_color, 35, 40)
  v[["miro_if_b2819be2"]] <- if_b2819be2
  if_b3ce36b1 <- if (boolean_b05e9519) makeHsl(b$miro_primary_color, 25, 0) else makeHsl(b$miro_primary_color, 46, 75)
  v[["miro_if_b3ce36b1"]] <- if_b3ce36b1
  if_c6c8bc62 <- if (boolean_b05e9519) makeHsl(b$miro_primary_color, 40, 0) else makeHsl(b$miro_primary_color, 45, 100)
  v[["miro_if_c6c8bc62"]] <- if_c6c8bc62
  if_d440a364 <- if (boolean_b05e9519) makeHsl(b$miro_primary_color, 51, 0) else makeHsl(b$miro_primary_color, 74, 66)
  v[["miro_if_d440a364"]] <- if_d440a364
  if_ed1f2135 <- if (boolean_b05e9519) makeHsl(b$miro_primary_color, 10, 0) else makeHsl(b$miro_primary_color, 14, 85)
  v[["miro_if_ed1f2135"]] <- if_ed1f2135
  if_ed914f1d <- if (abs(luma(b$miro_main_bg) - luma(b$miro_primary_color)) < 15) contrast(b$miro_main_bg) else b$miro_primary_color
  v[["miro_if_ed914f1d"]] <- if_ed914f1d
  if_7dea713b <- if (abs(luma(b$miro_primary_color) - luma("#333333")) < 25) contrast(b$miro_primary_color) else "#333333"
  v[["miro_if_7dea713b"]] <- if_7dea713b
  if_5bc80e62 <- if (goodContrast("#ffffff", b$miro_primary_color)) "#ffffff" else contrast(b$miro_primary_color)
  v[["miro_if_5bc80e62"]] <- if_5bc80e62
  if_8d03604f <- if (goodContrast(b$miro_primary_color, "#ffffff", threshold = 3)) b$miro_primary_color else contrast("#ffffff")
  v[["miro_if_8d03604f"]] <- if_8d03604f
  lighten_6a0cbe6d <- lighten(darken_cd55fe88, 40)
  v[["miro_lighten_6a0cbe6d"]] <- lighten_6a0cbe6d
  darken_28f7e606 <- darken(darken_cd55fe88, 5)
  v[["miro_darken_28f7e606"]] <- darken_28f7e606
  darken_686ca693 <- darken(darken_cd55fe88, 7.5)
  v[["miro_darken_686ca693"]] <- darken_686ca693
  darken_618fc861 <- darken(darken_cd55fe88, 10)
  v[["miro_darken_618fc861"]] <- darken_618fc861
  darken_111221b2 <- darken(darken_cd55fe88, 15)
  v[["miro_darken_111221b2"]] <- darken_111221b2
  darken_b5221a2c <- darken(darken_cd55fe88, 17)
  v[["miro_darken_b5221a2c"]] <- darken_b5221a2c
  if_dc5cf808 <- if (abs(luma(if_b2819be2) - luma("#ffffff")) < 15) contrast(if_b2819be2) else "#ffffff"
  v[["miro_if_dc5cf808"]] <- if_dc5cf808
  if_fa310009 <- if (abs(luma(fade_193caeb6) - luma("#333333")) < 8) contrast(fade_193caeb6) else "#333333"
  v[["miro_if_fa310009"]] <- if_fa310009
  darken_ec72c039 <- darken(darken_28f7e606, 12)
  v[["miro_darken_ec72c039"]] <- darken_ec72c039
  darken_4b80b5c4 <- darken(darken_28f7e606, 25)
  v[["miro_darken_4b80b5c4"]] <- darken_4b80b5c4
  notification_bg <- if (abs(luma("#ffffff") - luma(lighten(b$miro_primary_color, 40))) > 5) lighten(b$miro_primary_color, 40) else lighten(b$miro_primary_color, 35)
  v[["miro_notification_bg"]] <- notification_bg
  notification_text <- if (goodContrast(darken(b$miro_primary_color, 39), notification_bg)) darken(b$miro_primary_color, 39) else contrast(notification_bg)
  v[["miro_notification_text"]] <- notification_text
  notification_border <- darken(b$miro_primary_color, 7)
  v[["miro_notification_border"]] <- notification_border

  fade_041bf36e <- fade(b$miro_primary_color_dark, 69)
  v[["miro_fade_041bf36e"]] <- fade_041bf36e
  fade_09c43b35 <- fade(b$miro_primary_color_dark, 75)
  v[["miro_fade_09c43b35"]] <- fade_09c43b35
  fade_b4a0a1c1 <- fade(b$miro_primary_color, 60)
  v[["miro_fade_b4a0a1c1"]] <- fade_b4a0a1c1
  lighten_360db171 <- lighten(b$miro_primary_color_dark, 15)
  v[["miro_lighten_360db171"]] <- lighten_360db171
  darken_0efbf01c <- darken(b$miro_primary_color_dark, 2)
  v[["miro_darken_0efbf01c"]] <- darken_0efbf01c
  darken_eccec2f4 <- darken(b$miro_primary_color_dark, 5)
  v[["miro_darken_eccec2f4"]] <- darken_eccec2f4
  darken_a8b8ac97 <- darken(b$miro_primary_color_dark, 6)
  v[["miro_darken_a8b8ac97"]] <- darken_a8b8ac97
  darken_411e4a78 <- darken(b$miro_primary_color_dark, 6.5)
  v[["miro_darken_411e4a78"]] <- darken_411e4a78
  darken_e354f9b5 <- darken(b$miro_primary_color_dark, 10)
  v[["miro_darken_e354f9b5"]] <- darken_e354f9b5
  darken_57c981c3 <- darken(b$miro_primary_color_dark, 36, method = "relative")
  v[["miro_darken_57c981c3"]] <- darken_57c981c3
  if_190bbef8 <- if (abs(luma(fade(b$miro_primary_color_dark, 6)) - luma(b$miro_main_bg_dark)) < 5) fade(b$miro_primary_color_dark, 10) else fade(b$miro_primary_color_dark, 6)
  v[["miro_if_190bbef8"]] <- if_190bbef8
  if_7f94f006 <- if (boolean_19a3fbf5) hslHex(hue(b$miro_primary_color_dark), 0, 45) else hslHex(hue(b$miro_primary_color_dark), 34, 60)
  v[["miro_if_7f94f006"]] <- if_7f94f006
  if_8215301a <- if (abs(luma(b$miro_main_bg_dark) - luma(darken(b$miro_primary_color_dark, 10))) < 8) lighten(b$miro_primary_color_dark, 50) else darken(b$miro_primary_color_dark, 10)
  v[["miro_if_8215301a"]] <- if_8215301a
  if_d2f31bc0 <- if (abs(luma(b$miro_main_bg_dark) - luma(b$miro_primary_color_dark)) < 15) contrast(b$miro_main_bg_dark) else b$miro_primary_color_dark
  v[["miro_if_d2f31bc0"]] <- if_d2f31bc0
  if_ea8983c1 <- if (abs(luma(b$miro_primary_color_dark) - luma(b$miro_text_color_dark)) < 8) contrast(b$miro_primary_color_dark) else b$miro_text_color_dark
  v[["miro_if_ea8983c1"]] <- if_ea8983c1
  if_deaa6f9b <- if (boolean_19a3fbf5) "#1d1f20" else if (boolean_a6a32216) hslHex(hue(b$miro_primary_color_dark), 84, 40) else darken_0efbf01c
  v[["miro_if_deaa6f9b"]] <- if_deaa6f9b
  if_e44576b0 <- if (boolean_b05e9519) hslHex(hue(b$miro_primary_color), 0, 35) else hslHex(hue(b$miro_primary_color), 76, 62)
  v[["miro_if_e44576b0"]] <- if_e44576b0
  if_e0e5aa9e <- if (abs(luma(b$miro_primary_color_dark) - luma("#eeeeee")) < 25) contrast(b$miro_primary_color_dark) else "#eeeeee"
  v[["miro_if_e0e5aa9e"]] <- if_e0e5aa9e
  if_v8c57865 <- if (goodContrast(b$miro_text_color_dark, b$miro_primary_color_dark)) b$miro_text_color_dark else contrast(b$miro_primary_color_dark)
  v[["miro_if_v8c57865"]] <- if_v8c57865
  darken_e71e1e83 <- darken(darken_411e4a78, 5)
  v[["miro_darken_e71e1e83"]] <- darken_e71e1e83
  darken_553812b2 <- darken(darken_411e4a78, 7.5)
  v[["miro_darken_553812b2"]] <- darken_553812b2
  darken_618fc862 <- darken(darken_411e4a78, 10)
  v[["miro_darken_618fc862"]] <- darken_618fc862
  darken_87dfbfea <- darken(darken_411e4a78, 15)
  v[["miro_darken_87dfbfea"]] <- darken_87dfbfea
  fade_304ae92d <- fade(if_e44576b0, 75)
  v[["miro_fade_304ae92d"]] <- fade_304ae92d
  darken_8bbacd34 <- darken(if_7f94f006, 10)
  v[["miro_darken_8bbacd34"]] <- darken_8bbacd34
  darken_e89a054b <- darken(if_deaa6f9b, 5, method = "relative")
  v[["miro_darken_e89a054b"]] <- darken_e89a054b
  if_e33a6962 <- if (abs(luma(if_deaa6f9b) - luma("#333333")) < 15) contrast(if_deaa6f9b) else "#333333"
  v[["miro_if_e33a6962"]] <- if_e33a6962
  if_9ad4a640 <- if (abs(luma(fade_041bf36e) - luma("#eeeeee")) < 8) contrast(fade_041bf36e) else "#eeeeee"
  v[["miro_if_9ad4a640"]] <- if_9ad4a640
  notification_bg_dark <- if (goodContrast(b$miro_text_color_dark, darken(b$miro_primary_color_dark, 35))) darken(b$miro_primary_color_dark, 35) else darken(b$miro_primary_color_dark, 55)
  v[["miro_notification_bg_dark"]] <- notification_bg_dark

  # secondary colors
  darken_75d40b33 <- darken(b$miro_secondary_color, 5)
  v[["miro_darken_75d40b33"]] <- darken_75d40b33
  darken_3fd83bc2 <- darken(b$miro_secondary_color, 6)
  v[["miro_darken_3fd83bc2"]] <- darken_3fd83bc2
  darken_8e82a497 <- darken(b$miro_secondary_color, 10)
  v[["miro_darken_8e82a497"]] <- darken_8e82a497
  if_10618845 <- if (goodContrast(b$miro_secondary_color, b$miro_sidebar_color, threshold = 1.1)) b$miro_secondary_color else contrast(b$miro_sidebar_color, light = lighten(b$miro_secondary_color, 20), dark = darken(b$miro_secondary_color, 20))
  v[["miro_if_10618845"]] <- if_10618845
  if_1b7f3b8b <- if (abs(luma(b$miro_sidebar_color) - luma(b$miro_secondary_color)) < 8) contrast(b$miro_sidebar_color) else "#ADADAD"
  v[["miro_if_1b7f3b8b"]] <- if_1b7f3b8b
  if_6f85b072 <- if (abs(luma(b$miro_secondary_color) - luma("#333333")) < 8) contrast(b$miro_secondary_color) else "#333333"
  v[["miro_if_6f85b072"]] <- if_6f85b072
  if_b318da33 <- if (boolean_2e46d206) hslHex(hue(b$miro_secondary_color), 0, 85) else hslHex(hue(b$miro_secondary_color), 77, 90)
  v[["miro_if_b318da33"]] <- if_b318da33
  if_035e91c6 <- if (abs(luma(if_b318da33) - luma(b$miro_secondary_color)) < 15) contrast(if_b318da33) else b$miro_secondary_color
  v[["miro_if_035e91c6"]] <- if_035e91c6
  if_4bc80e64 <- if (abs(luma(b$miro_secondary_color) - luma(b$miro_text_color)) < 15) contrast(b$miro_secondary_color) else b$miro_text_color
  v[["miro_if_4bc80e64"]] <- if_4bc80e64
  darken_29c4aefc <- darken(if_b318da33, 15)
  v[["miro_darken_29c4aefc"]] <- darken_29c4aefc

  darken_19dd7eda <- darken(b$miro_secondary_color_dark, 5)
  v[["miro_darken_19dd7eda"]] <- darken_19dd7eda
  darken_daccffb8 <- darken(b$miro_secondary_color_dark, 6)
  v[["miro_darken_daccffb8"]] <- darken_daccffb8
  darken_a780cc60 <- darken(b$miro_secondary_color_dark, 10)
  v[["miro_darken_a780cc60"]] <- darken_a780cc60
  if_94dad415 <- if (goodContrast(b$miro_secondary_color_dark, b$miro_sidebar_color_dark, threshold = 1.1)) b$miro_secondary_color_dark else contrast(b$miro_sidebar_color_dark, light = lighten(b$miro_secondary_color_dark, 20), dark = darken(b$miro_secondary_color_dark, 20))
  v[["miro_if_94dad415"]] <- if_94dad415
  if_f57ecf46 <- if (abs(luma(b$miro_sidebar_color_dark) - luma(b$miro_secondary_color_dark)) < 8) contrast(b$miro_sidebar_color_dark) else "#ADADAD"
  v[["miro_if_f57ecf46"]] <- if_f57ecf46
  if_5905ebb4 <- if (abs(luma(b$miro_secondary_color_dark) - luma("#eeeeee")) < 8) contrast(b$miro_secondary_color_dark) else "#eeeeee"
  v[["miro_if_5905ebb4"]] <- if_5905ebb4
  if_dde4565a <- if (boolean_2e46d206) hslHex(hue(b$miro_secondary_color_dark), 0, 20) else hslHex(hue(b$miro_secondary_color_dark), 50, 25)
  v[["miro_if_dde4565a"]] <- if_dde4565a
  if_3105643b <- if (abs(luma(if_dde4565a) - luma(b$miro_secondary_color_dark)) < 15) contrast(if_dde4565a) else b$miro_secondary_color_dark
  v[["miro_if_3105643b"]] <- if_3105643b
  darken_2ad94c65 <- darken(if_dde4565a, 15)
  v[["miro_darken_2ad94c65"]] <- darken_2ad94c65

  # sidebar colors
  darken_v73ofn52 <- darken(b$miro_sidebar_color, 1)
  v[["miro_darken_v73ofn52"]] <- darken_v73ofn52
  darken_b05e8d17 <- darken(b$miro_sidebar_color, 2)
  v[["miro_darken_b05e8d17"]] <- darken_b05e8d17
  darken_8e899f91 <- darken(b$miro_sidebar_color, 3)
  v[["miro_darken_8e899f91"]] <- darken_8e899f91
  darken_c806e739 <- darken(b$miro_sidebar_color, 4)
  v[["miro_darken_c806e739"]] <- darken_c806e739
  darken_28c2c354 <- darken(b$miro_sidebar_color, 5)
  v[["miro_darken_28c2c354"]] <- darken_28c2c354
  darken_12518b85 <- darken(b$miro_sidebar_color, 7)
  v[["miro_darken_12518b85"]] <- darken_12518b85
  lighten_959ccfc9 <- lighten(b$miro_sidebar_color, 5)
  v[["miro_lighten_959ccfc9"]] <- lighten_959ccfc9
  lighten_f6fb5849 <- lighten(b$miro_sidebar_color, 10)
  v[["miro_lighten_f6fb5849"]] <- lighten_f6fb5849
  lighten_196d441a <- lighten(b$miro_sidebar_color, 20)
  v[["miro_lighten_196d441a"]] <- lighten_196d441a
  lighten_919393a0 <- lighten(b$miro_sidebar_color, 60)
  v[["miro_lighten_919393a0"]] <- lighten_919393a0
  if_f5e54611 <- if (boolean_486b1f24) "#333333" else lighten_919393a0
  v[["miro_if_f5e54611"]] <- if_f5e54611
  if_038054cd <- if (boolean_486b1f24) darken(b$miro_sidebar_color, 2) else lighten(b$miro_sidebar_color, 2)
  v[["miro_if_038054cd"]] <- if_038054cd
  if_1b7f3b8c <- if (goodContrast("#eeeeee", b$miro_sidebar_color)) "#eeeeee" else contrast(b$miro_sidebar_color, dark = "#ADADAD", light = "#ADADAD")
  v[["miro_if_1b7f3b8c"]] <- if_1b7f3b8c
  lighten_39320a59 <- lighten(lighten_959ccfc9, 40)
  v[["miro_lighten_39320a59"]] <- lighten_39320a59
  sidebar_contrast_light <- if (boolean_486b1f24) "#333333" else "#ffffff"
  v[["miro_sidebar_contrast_light"]] <- sidebar_contrast_light
  sidebar_contrast_light2 <- if (boolean_486b1f24) "#222222" else "#ffffff"
  v[["miro_sidebar_contrast_light2"]] <- sidebar_contrast_light2

  navbar_contrast_light <- if (goodContrast("#333333", b$miro_navbar_color)) "#333333" else "#eeeeee"
  v[["miro_navbar_contrast_light"]] <- navbar_contrast_light

  lighten_a006ac60 <- lighten(b$miro_sidebar_color_dark, 2)
  v[["miro_lighten_a006ac60"]] <- lighten_a006ac60
  lighten_4d79a66d <- lighten(b$miro_sidebar_color_dark, 5)
  v[["miro_lighten_4d79a66d"]] <- lighten_4d79a66d
  lighten_8b122426 <- lighten(b$miro_sidebar_color_dark, 10)
  v[["miro_lighten_8b122426"]] <- lighten_8b122426
  lighten_49e49d58 <- lighten(b$miro_sidebar_color_dark, 20)
  v[["miro_lighten_49e49d58"]] <- lighten_49e49d58
  lighten_4f1df671 <- lighten(b$miro_sidebar_color_dark, 60)
  v[["miro_lighten_4f1df671"]] <- lighten_4f1df671
  if_24167817 <- if (goodContrast(lighten_4f1df671, b$miro_sidebar_color_dark)) lighten_4f1df671 else contrast(b$miro_sidebar_color_dark, dark = "#333333")
  v[["miro_if_24167817"]] <- if_24167817
  darken_dc24222b <- darken(b$miro_sidebar_color_dark, 3)
  v[["miro_darken_dc24222b"]] <- darken_dc24222b
  darken_7911e270 <- darken(b$miro_sidebar_color_dark, 4)
  v[["miro_darken_7911e270"]] <- darken_7911e270
  darken_78904a7c <- darken(b$miro_sidebar_color_dark, 5)
  v[["miro_darken_78904a7c"]] <- darken_78904a7c
  darken_f6592ee2 <- darken(b$miro_sidebar_color_dark, 7)
  v[["miro_darken_f6592ee2"]] <- darken_f6592ee2
  if_1b7f3b8d <- if (goodContrast("#eeeeee", b$miro_sidebar_color_dark)) "#eeeeee" else contrast(b$miro_sidebar_color_dark, dark = "#ADADAD", light = "#ADADAD")
  v[["miro_if_1b7f3b8d"]] <- if_1b7f3b8d
  if_417fecde <- if (goodContrast(lighten(b$miro_sidebar_color_dark, 3), b$miro_sidebar_color_dark, threshold = 1.05)) lighten(b$miro_sidebar_color_dark, 3) else contrast(b$miro_sidebar_color_dark, dark = darken(b$miro_sidebar_color_dark, 3), light = lighten(b$miro_sidebar_color_dark, 3))
  v[["miro_if_417fecde"]] <- if_417fecde
  sidebar_contrast_dark <- if (goodContrast("#ffffff", b$miro_sidebar_color_dark)) "#ffffff" else contrast(b$miro_sidebar_color_dark, dark = "#333333")
  v[["miro_sidebar_contrast_dark"]] <- sidebar_contrast_dark
  sidebar_contrast_dark2 <- if (goodContrast("#ffffff", b$miro_sidebar_color_dark)) "#ffffff" else contrast(b$miro_sidebar_color_dark, dark = "#222222")
  v[["miro_sidebar_contrast_dark2"]] <- sidebar_contrast_dark2
  lighten_c5da3167 <- lighten(lighten_4d79a66d, 40)
  v[["miro_lighten_c5da3167"]] <- lighten_c5da3167

  # alert colors
  darken_68f1341c <- darken(b$miro_alert_color, 16, method = "relative")
  v[["miro_darken_68f1341c"]] <- darken_68f1341c
  if_cf579cbb <- if (boolean_9cccc45f) hslHex(hue(b$miro_alert_color), 0, 85) else hslHex(hue(b$miro_alert_color), 77, 90)
  v[["miro_if_cf579cbb"]] <- if_cf579cbb
  if_ec44ba6b <- if (abs(luma(if_cf579cbb) - luma(b$miro_alert_color)) < 15) contrast(if_cf579cbb) else b$miro_alert_color
  v[["miro_if_ec44ba6b"]] <- if_ec44ba6b
  darken_4879a36f <- darken(b$miro_alert_color_dark, 16, method = "relative")
  v[["miro_darken_4879a36f"]] <- darken_4879a36f
  if_ba47b56b <- if (boolean_9cccc45f) hslHex(hue(b$miro_alert_color_dark), 0, 20) else hslHex(hue(b$miro_alert_color_dark), 50, 25)
  v[["miro_if_ba47b56b"]] <- if_ba47b56b
  if_3b6cb002 <- if (abs(luma(if_ba47b56b) - luma(b$miro_alert_color_dark)) < 15) contrast(if_ba47b56b) else b$miro_alert_color_dark
  v[["miro_if_3b6cb002"]] <- if_3b6cb002
  darken_8df68996 <- darken(if_ba47b56b, 15)
  v[["miro_darken_8df68996"]] <- darken_8df68996
  darken_a59823b8 <- darken(if_cf579cbb, 15)
  v[["miro_darken_a59823b8"]] <- darken_a59823b8

  # main background colors
  lighten_9ae39279 <- lighten(b$miro_main_bg_dark, 5)
  v[["miro_lighten_9ae39279"]] <- lighten_9ae39279
  darken_62f5e2b6 <- darken(b$miro_main_bg_dark, 1, method = "relative")
  v[["miro_darken_62f5e2b6"]] <- darken_62f5e2b6
  darken_d35a8f8e <- darken(b$miro_main_bg_dark, 20)
  v[["miro_darken_d35a8f8e"]] <- darken_d35a8f8e
  if_72f3068c <- if (abs(luma(b$miro_main_bg_dark) - luma(darken(if_8215301a, 10))) < 8) lighten(if_8215301a, 10) else darken(if_8215301a, 10)
  v[["miro_if_72f3068c"]] <- if_72f3068c
  if_c1a0bfe3 <- if (boolean_880ef169) hslHex(hue(b$miro_main_bg_dark), 0, 15) else hslHex(hue(b$miro_main_bg_dark), 10, 18)
  v[["miro_if_c1a0bfe3"]] <- if_c1a0bfe3
  if_f1a3b180 <- if (boolean_880ef169) hslHex(hue(b$miro_main_bg_dark), 0, 12) else hslHex(hue(b$miro_main_bg_dark), 10, 18)
  v[["miro_if_f1a3b180"]] <- if_f1a3b180
  if_4a0e097b <- if (abs(luma(if_f1a3b180) - luma(b$miro_primary_color_dark)) < 8) "#A9A9A9" else if_f1a3b180
  v[["miro_if_4a0e097b"]] <- if_4a0e097b
  fade_ece660c0 <- fade(if_f1a3b180, 56)
  v[["miro_fade_ece660c0"]] <- fade_ece660c0
  if_851d5a45 <- if (abs(luma(b$miro_main_bg_dark) - luma(b$miro_console_text_color_dark)) < 15) contrast(b$miro_main_bg_dark) else b$miro_console_text_color_dark
  v[["miro_if_851d5a45"]] <- if_851d5a45
  fade_5cff14c3 <- fade(b$miro_console_text_color_dark, 90)
  v[["miro_fade_5cff14c3"]] <- fade_5cff14c3
  if_f37627e5 <- if (abs(luma(darken_d35a8f8e) - luma(b$miro_console_text_color_dark)) < 15) contrast(darken_d35a8f8e, "#000000", "#cdcdcd") else b$miro_console_text_color_dark
  v[["miro_if_f37627e5"]] <- if_f37627e5
  if_cb57bb52 <- if (abs(luma(darken_d35a8f8e) - luma(b$miro_console_text_color_dark)) < 15) contrast(darken_d35a8f8e, "#000000", "#cdcdcd") else console_text_color_dark
  v[["miro_if_cb57bb52"]] <- if_cb57bb52

  # widget background colors
  fade_102be7f4 <- fade(b$miro_widget_bg_dark, 17)
  v[["miro_fade_102be7f4"]] <- fade_102be7f4
  darken_970a799e <- darken(b$miro_widget_bg_dark, 10)
  v[["miro_darken_970a799e"]] <- darken_970a799e
  darken_41c1769c <- darken(b$miro_widget_bg_dark, 39, method = "relative")
  v[["miro_darken_41c1769c"]] <- darken_41c1769c
  darken_a6b030af <- darken(darken_41c1769c, 3)
  v[["miro_darken_a6b030af"]] <- darken_a6b030af
  darken_9e0118e0 <- darken(darken_41c1769c, 11, method = "relative")
  v[["miro_darken_9e0118e0"]] <- darken_9e0118e0
  lighten_27d70c85 <- lighten(darken_41c1769c, 6, method = "relative")
  v[["miro_lighten_27d70c85"]] <- lighten_27d70c85
  lighten_37654577 <- lighten(darken_41c1769c, 10, method = "relative")
  v[["miro_lighten_37654577"]] <- lighten_37654577

  if_a8c57869 <- if (abs(luma("#030303") - luma(b$miro_text_color_dark)) < 15) contrast("#030303") else b$miro_text_color_dark
  v[["miro_if_a8c57869"]] <- if_a8c57869

  v
}
cssName <- function(id) {
  out <- id
  out <- gsub("_", "-", out, fixed = TRUE)
  if (startsWith(out, "--")) out else paste0("--", out)
}
serializeColors <- function(x) {
  if (is.logical(x)) {
    as.character(as.integer(x))
  } else if (is.numeric(x)) {
    format(x, trim = TRUE)
  } else {
    x
  }
}

palette <- reactive(derivePalette(customBaseColors()))

isDefaultPalette <- reactive({
  cb <- customBaseColors()
  def <- baseColors[names(cb)]
  all(mapply(identical, cb, def))
})

observeEvent(palette(),
  {
    vars <- palette()
    msg <- setNames(
      lapply(vars, serializeColors),
      vapply(names(vars), cssName, "")
    )
    session$sendCustomMessage("update-css", msg)
  },
  ignoreInit = TRUE
)

observeEvent(input$advanced_light,
  {
    req(!input$advanced_light)

    sidebarColor <- hslHex(hue(input$primary_color), 6, 12)
    updateColorPickerInput(session, "sidebar_color", value = sidebarColor)

    isNeutral <- boolean(hue(input$primary_color) == 0 & saturation(input$primary_color) == 0)
    if (isNeutral) {
      bodyBgColor <- makeHsl(input$primary_color, 80, 0)
    } else {
      bodyBgColor <- makeHsl(input$primary_color, 94, 26)
    }
    updateColorPickerInput(session, "body_bg_color", value = bodyBgColor)
  },
  ignoreInit = TRUE
)

observeEvent(input$primary_color, {
  if (identical(input$primary_color, "")) {
    updateColorPickerInput(session, "primary_color", value = customBaseColors()$miro_primary_color)
    return()
  }

  req(!input$advanced_light)
  sidebarColor <- hslHex(hue(input$primary_color), 6, 12)
  updateColorPickerInput(session, "sidebar_color", value = sidebarColor)

  isNeutral <- boolean(hue(input$primary_color) == 0 & saturation(input$primary_color) == 0)
  if (isNeutral) {
    bodyBgColor <- makeHsl(input$primary_color, 80, 0)
  } else {
    bodyBgColor <- makeHsl(input$primary_color, 94, 26)
  }
  updateColorPickerInput(session, "body_bg_color", value = bodyBgColor)
})

observeEvent(input$advanced_dark,
  {
    req(!input$advanced_dark)

    sidebarColorDark <- hslHex(hue(input$primary_color_dark), 6, 12)
    updateColorPickerInput(session, "sidebar_color_dark", value = sidebarColorDark)

    isNeutral <- boolean(hue(input$primary_color_dark) == 0 & saturation(input$primary_color_dark) == 0)
    if (isNeutral) {
      navBarColorDark <- hslHex(hue(input$primary_color_dark), 0, 10)
    } else {
      navBarColorDark <- hslHex(hue(input$primary_color_dark), 6, 12)
    }
    updateColorPickerInput(session, "navbar_color_dark", value = navBarColorDark)

    isNeutral <- boolean(hue(input$main_bg_dark) == 0 & saturation(input$main_bg_dark) == 0)
    if (isNeutral) {
      bodyBgColorDark <- hslHex(hue(input$main_bg_dark), 0, 12)
    } else {
      bodyBgColorDark <- hslHex(hue(input$main_bg_dark), 10, 18)
    }
    updateColorPickerInput(session, "body_bg_color_dark", value = bodyBgColorDark)
  },
  ignoreInit = TRUE
)

observeEvent(input$primary_color_dark, {
  if (identical(input$primary_color_dark, "")) {
    updateColorPickerInput(session, "primary_color_dark", value = customBaseColors()$miro_primary_color_dark)
    return()
  }

  req(!input$advanced_dark)
  sidebarColorDark <- hslHex(hue(input$primary_color_dark), 6, 12)
  updateColorPickerInput(session, "sidebar_color_dark", value = sidebarColorDark)

  isNeutral <- boolean(hue(input$primary_color_dark) == 0 & saturation(input$primary_color_dark) == 0)
  if (isNeutral) {
    navBarColorDark <- hslHex(hue(input$primary_color_dark), 0, 10)
  } else {
    navBarColorDark <- hslHex(hue(input$primary_color_dark), 6, 12)
  }
  updateColorPickerInput(session, "navbar_color_dark", value = navBarColorDark)
})

observeEvent(input$main_bg_dark, {
  if (identical(input$main_bg_dark, "")) {
    updateColorPickerInput(session, "main_bg_dark", value = customBaseColors()$miro_main_bg_dark)
    return()
  }

  req(!input$advanced_dark)

  isNeutral <- boolean(hue(input$main_bg_dark) == 0 & saturation(input$main_bg_dark) == 0)
  if (isNeutral) {
    bodyBgColorDark <- hslHex(hue(input$main_bg_dark), 0, 12)
  } else {
    bodyBgColorDark <- hslHex(hue(input$main_bg_dark), 10, 18)
  }
  updateColorPickerInput(session, "body_bg_color_dark", value = bodyBgColorDark)
})

lapply(
  setdiff(
    names(baseColors),
    c(
      "miro_primary_color", "miro_primary_color_dark", "miro_main_bg_dark",
      "miro_text_color", "miro_text_color_dark"
    )
  ), function(id) {
    observeEvent(input[[id]], {
      if (identical(input[[id]], "")) {
        updateColorPickerInput(session, id,
          value = customBaseColors()[[id]]
        )
      }
    })
  }
)
observeEvent(input$removeColors, {
  if (length(configJSON$themeColors)) {
    showModal(modalDialog(
      title = lang$adminMode$colors$themeButtons$removeColorsTitle,
      lang$adminMode$colors$themeButtons$removeColorsText,
      footer = tagList(
        modalButton(lang$adminMode$colors$themeButtons$removeColorsCancel),
        actionButton("removeColorsConfirm", lang$adminMode$colors$themeButtons$removeColorsConfirm, class = "bt-remove")
      ),
      fade = TRUE, easyClose = TRUE
    ))
  } else {
    showNotification(lang$adminMode$colors$themeButtons$removeColorsNote2, type = "message")
  }
})
removeThemeColorsTrigger <- reactiveVal(0)
observeEvent(input$removeColorsConfirm, {
  configJSON$themeColors <<- NULL
  rv$generalConfig$themeColors <<- NULL
  removeThemeColorsTrigger(removeThemeColorsTrigger() + 1)
  removeModal()
  showNotification(lang$adminMode$colors$themeButtons$removeColorsNote1, type = "message")
})
observeEvent(input$saveLocal, {
  rv$generalConfig$themeColors <- palette()
  showNotification(lang$adminMode$colors$themeButtons$saveLocalNote, type = "message")
})
observeEvent(input$saveGlobal, {
  outFile <- file.path(miroWorkspace, "colors_custom.css")

  if (file.exists(outFile)) {
    showModal(modalDialog(
      title = lang$adminMode$colors$themeButtons$saveGlobalModalTitle,
      sprintf(
        lang$adminMode$colors$themeButtons$saveGlobalModalText,
        basename(outFile)
      ),
      footer = tagList(
        modalButton(lang$adminMode$colors$themeButtons$saveGlobalModalCancel),
        actionButton("overwriteGlobal", lang$adminMode$colors$themeButtons$saveGlobalModalOverwrite, class = "bt-highlight-1")
      ),
      fade = TRUE, easyClose = TRUE
    ))
  } else {
    vars <- palette()
    msg <- setNames(
      lapply(vars, serializeColors),
      vapply(names(vars), cssName, "")
    )
    properties <- paste0(names(msg), ":", msg, collapse = ";\n  ")
    css <- paste0(":root{\n  ", properties, ";\n}")
    writeLines(css, con = outFile)
    showNotification(paste0(lang$adminMode$colors$themeButtons$saveGlobalModalNote2, " ", lang$adminMode$colors$themeButtons$saveGlobalModalNoteDesc), duration = 8, type = "message")
  }
})

observeEvent(input$overwriteGlobal, {
  outFile <- file.path(miroWorkspace, "colors_custom.css")
  vars <- palette()
  msg <- setNames(
    lapply(vars, serializeColors),
    vapply(names(vars), cssName, "")
  )
  properties <- paste0(names(msg), ":", msg, collapse = ";\n  ")
  css <- paste0(":root{\n  ", properties, ";\n}")

  writeLines(css, con = outFile)
  removeModal()
  showNotification(paste0(lang$adminMode$colors$themeButtons$saveGlobalModalNote1, " ", lang$adminMode$colors$themeButtons$saveGlobalModalNoteDesc), duration = 8, type = "message")
})

output$downloadTheme <- downloadHandler(
  filename = function() {
    "colors_custom.css"
  },
  content = function(file) {
    pal <- palette()

    miroServerColors <- list(
      miro_server_primary_color        = input$server_primary_color,
      miro_server_body_bg              = input$server_body_bg,
      miro_server_main_bg              = input$server_main_bg,
      miro_server_login_header_bg      = input$server_login_header_bg,
      miro_server_primary_color_dark   = input$server_primary_color_dark,
      miro_server_body_bg_dark         = input$server_body_bg_dark,
      miro_server_main_bg_dark         = input$server_main_bg_dark,
      miro_server_login_header_bg_dark = input$server_login_header_bg_dark
    )

    miroServerColors <- miroServerColors[!vapply(miroServerColors, is.null, logical(1))]

    vars <- c(pal, miroServerColors)

    msg <- setNames(
      lapply(vars, serializeColors),
      vapply(names(vars), cssName, "")
    )
    properties <- paste0(names(msg), ":", msg, collapse = ";\n  ")
    css <- paste0(":root{\n  ", properties, ";\n}")
    writeLines(css, con = file)
  },
  contentType = "text/css"
)

app <- ShinyDriver$new("../../", loadTimeout = 20000)
app$snapshotInit("config_mode_general_test")

app$snapshot(items = list(input = "deleteGraph"), screenshot = TRUE)
Sys.sleep(1)
jsonPath <- file.path("..", "model", "pickstock_configuration", "conf_pickstock_configuration")
configRaw <- suppressWarnings(jsonlite::fromJSON(file.path(jsonPath, "pickstock_configuration_expected.json"),
                                                  simplifyDataFrame = FALSE,
                                                  simplifyMatrix = FALSE))
configNew <- suppressWarnings(jsonlite::fromJSON(file.path(jsonPath, "pickstock_configuration.json"),
                                                 simplifyDataFrame = FALSE,
                                                 simplifyMatrix = FALSE))

#interface tab (I)
expect_identical(configRaw$pageTitle, configNew$pageTitle)
expect_identical(configRaw$theme, configNew$theme)
expect_identical(configRaw$activateModules$logFile, configNew$activateModules$logFile)
expect_identical(configRaw$activateModules$lstFile, configNew$activateModules$lstFile)
expect_identical(configRaw$miroLogFile, configNew$miroLogFile)
expect_identical(configRaw$defCompMode, configNew$defCompMode)
expect_identical(configRaw$pivotCompSettings, list(enableHideEmptyCols = TRUE, emptyUEL = "&"))
expect_identical(configRaw$autoGenInputGraphs, configNew$autoGenInputGraphs)
expect_identical(configRaw$roundingDecimals, configNew$roundingDecimals)

#interface tab (II)
expect_identical(configRaw$UILogo, configNew$UILogo)
expect_identical(configRaw$defaultRendererOutput, configNew$defaultRendererOutput)
expect_identical(configRaw$readme$tabTitle, configNew$readme$tabTitle)
expect_identical(configRaw$readme$filename, configNew$readme$filename)
expect_identical(configRaw$readme$enableMath, configNew$readme$enableMath)

#modules tab (I)
expect_identical(configRaw$activateModules$loadLocal, configNew$activateModules$loadLocal)
expect_identical(configRaw$defaultScenName, configNew$defaultScenName)
expect_identical(configRaw$excelIncludeMeta, configNew$excelIncludeMeta)
expect_identical(configRaw$excelIncludeEmptySheets, configNew$excelIncludeEmptySheets)
expect_identical(configRaw$activateModules$attachments, configNew$activateModules$attachments)
expect_identical(configRaw$storeLogFilesDuration, configNew$storeLogFilesDuration)
expect_identical(configRaw$outputAttachments[[1]]$filename, configNew$outputAttachments[[1]]$filename)
expect_identical(configRaw$outputAttachments[[1]]$execPerm, configNew$outputAttachments[[1]]$execPerm)
expect_identical(configRaw$outputAttachments[[1]]$throwError, configNew$outputAttachments[[1]]$throwError)
expect_identical(configRaw$outputAttachments[[2]]$filename, configNew$outputAttachments[[2]]$filename)
expect_identical(configRaw$outputAttachments[[2]]$execPerm, configNew$outputAttachments[[2]]$execPerm)
expect_identical(configRaw$outputAttachments[[2]]$throwError, configNew$outputAttachments[[2]]$throwError)

#modules tab (II)
expect_identical(configRaw$activateModules$downloadTempFiles, configNew$activateModules$downloadTempFiles)
expect_identical(configRaw$extraClArgs, configNew$extraClArgs)

#symbol configuration tab (I: Naming)
expect_identical(configRaw$overwriteAliases$price$newAlias, configNew$overwriteAliases$price$newAlias)
expect_identical(configRaw$overwriteAliases[["_scalars_out"]]$newAlias, configNew$overwriteAliases[["_scalars_out"]]$newAlias)
expect_identical(configRaw$overwriteAliases$stock_weight$newAlias, configNew$overwriteAliases$stock_weight$newAlias)
expect_identical(configRaw$overwriteAliases$dowvsindex$newAlias, configNew$overwriteAliases$dowvsindex$newAlias)
expect_identical(configRaw$overwriteAliases$abserror$newAlias, configNew$overwriteAliases$abserror$newAlias)
expect_identical(configRaw$overwriteAliases$pricemerge$newAlias, configNew$overwriteAliases$pricemerge$newAlias)
expect_identical(configRaw$overwriteHeaderAliases$price$newHeaders, configNew$overwriteHeaderAliases$price$newHeaders)
expect_identical(configRaw$overwriteHeaderAliases[["_scalars_out"]]$newHeaders, configNew$overwriteHeaderAliases[["_scalars_out"]]$newHeaders)
expect_identical(configRaw$overwriteHeaderAliases[["_scalars_out"]]$newHeaders, configNew$overwriteHeaderAliases[["_scalars_out"]]$newHeaders)
expect_identical(configRaw$overwriteHeaderAliases[["error_ratio"]]$newHeaders, configNew$overwriteHeaderAliases[["error_ratio"]]$newHeaders)
expect_identical(configRaw$overwriteHeaderAliases[["trainingdays"]]$newHeaders, configNew$overwriteHeaderAliases[["trainingdays"]]$newHeaders)

#symbol configuration tab (II: Ordering, tab grouping, symbol display, Use output data as input data)
expect_identical(configRaw$overwriteSheetOrder$input, configNew$overwriteSheetOrder$input)
expect_identical(configRaw$overwriteSheetOrder$output, configNew$overwriteSheetOrder$output)
expect_identical(configRaw$inputWidgetGroups[[1]]$name, configNew$inputWidgetGroups[[1]]$name)
expect_identical(configRaw$inputWidgetGroups[[1]]$members, configNew$inputWidgetGroups[[1]]$members)
expect_identical(configRaw$outputGroups[[1]]$name, configNew$outputGroups[[1]]$name)
expect_identical(configRaw$outputGroups[[1]]$members, configNew$outputGroups[[1]]$members)
expect_identical(configRaw$hiddenOutputScalars, configNew$hiddenOutputScalars)
expect_identical(configRaw$aggregateWidgets, configNew$aggregateWidgets)
expect_identical(configRaw$symbolLinks[[1]]$source, configNew$symbolLinks[[1]]$source)
expect_identical(configRaw$symbolLinks[[1]]$target, configNew$symbolLinks[[1]]$target)

#analysis scripts tab
expect_identical(configRaw$scripts$hcube[[1]]$title, configNew$scripts$hcube[[1]]$title)
expect_identical(configRaw$scripts$hcube[[1]]$id, configNew$scripts$hcube[[1]]$id)
expect_identical(configRaw$scripts$hcube[[1]]$command, configNew$scripts$hcube[[1]]$command)
expect_identical(configRaw$scripts$hcube[[1]]$args, configNew$scripts$hcube[[1]]$args)
expect_identical(configRaw$scripts$hcube[[1]]$outputFile, configNew$scripts$hcube[[1]]$outputFile)

app$findElement("a[data-value='Symbol configuration']")$click()
app$setInputs("general_overwriteSymHeaders_price_1" = "2nd header")
Sys.sleep(1L)
configNew <- suppressWarnings(jsonlite::fromJSON(file.path(jsonPath, "pickstock_configuration.json"),
                                                 simplifyDataFrame = FALSE,
                                                 simplifyMatrix = FALSE))
expect_identical(configNew$overwriteHeaderAliases$price$newHeaders, c("2nd header", "2nd header", "3rd header"))
app$setInputs("general_overwriteSymHeaders_price_1" = "")
Sys.sleep(1L)
configNew <- suppressWarnings(jsonlite::fromJSON(file.path(jsonPath, "pickstock_configuration.json"),
                                                 simplifyDataFrame = FALSE,
                                                 simplifyMatrix = FALSE))
expect_identical(configNew$overwriteHeaderAliases$price$newHeaders, NULL)

expect_true(app$waitFor("$('#general_overwriteSymHeaders_price').hasClass('has-error')", timeout = 50))

app$stop()

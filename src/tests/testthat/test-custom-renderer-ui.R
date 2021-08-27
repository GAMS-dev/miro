context("UI tests - Custom renderers")
skip_if(
  identical(Sys.getenv("GAMS_SYS_DIR"), ""),
  "GAMS_SYS_DIR environment variable not set. Skipping tests."
)

createTestDb()

testModelPath <- file.path(testDir, "model", "transport_custom_map")
Sys.setenv(MIRO_MODEL_PATH = file.path(
  testModelPath,
  "transport_custom_map.gms"
))
Sys.setenv(MIRO_MODE = "base")
if (!identical(Sys.getenv("MIRO_TEST_GAMS_LICE"), "")) {
  saveAdditionalGamsClArgs(
    testModelPath, "transport_custom_map",
    paste0('license="', Sys.getenv("MIRO_TEST_GAMS_LICE"), '"')
  )
}
test_that(
  "Custom renderers with multiple (hidden) datasets work",
  expect_pass(testApp(file.path(testDir, ".."), "multiple_symbol_renderer",
    compareImages = FALSE
  ))
)
if (!identical(Sys.getenv("MIRO_TEST_GAMS_LICE"), "")) {
  file.rename(
    file.path(testModelPath, "conf_transport_custom_map", "transport_custom_map_tmp.json"),
    file.path(testModelPath, "conf_transport_custom_map", "transport_custom_map.json")
  )
}

Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE"))

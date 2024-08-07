context(paste0("UI tests - Startup performance - ", Sys.info()[["sysname"]]))
skip_if(
  identical(Sys.getenv("SKIP_PERFORMANCE_TESTS"), "true"),
  "Skipping performance tests since SKIP_PERFORMANCE_TESTS is set."
)

createTestDb()

Sys.setenv(MIRO_MODEL_PATH = file.path(
  testDir, "model", "transport_large",
  "transport.gms"
))
Sys.setenv(MIRO_MODE = "base")
pr <- PerformanceReporter$new()
pr$measure(
  "startup",
  test_that(
    "MIRO app transport_large starts",
    {
      source(file.path(testDir, "shinytest", "startup_performance_test.R"), local = TRUE)
    }
  )
)
pr$publish()

Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "MIRO_MODE"))

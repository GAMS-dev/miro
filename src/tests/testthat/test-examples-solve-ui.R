context("UI tests - Example models solve")
skip_if(identical(Sys.getenv("GAMS_SYS_DIR"), ""),
        "GAMS_SYS_DIR environment variable not set. Skipping tests.")

additionalGamsClArgs <- character(0L)
if(!identical(Sys.getenv("MIRO_TEST_GAMS_LICE"), "")){
  additionalGamsClArgs <- paste0('license="', Sys.getenv("MIRO_TEST_GAMS_LICE"), '"')
  if(identical(Sys.getenv("CI_JOB_IMAGE"), "hub.gams.com:443/gamsmiro-ci:latest") &&
     !file.exists(file.path(Sys.getenv("GAMS_SYS_DIR"), "gamslice.txt"))){
    # workaround for cutstock example as GAMS parameters like license
    # are not forwarded to bch facility with latest GAMS versions (34).
    if(!file.copy(Sys.getenv("MIRO_TEST_GAMS_LICE"), file.path(Sys.getenv("GAMS_SYS_DIR"), "gamslice.txt"))){
      warning("Could not copy GAMS license to GAMS system directory. Cutstock test is likely to fail!")
    }
  }
}

for(modelToTest in c("pickstock", "transport", "sudoku", "farming", "inscribedsquare", "tsp", "cpack", "lubrication", "kport", "cutstock")){
  createTestDb()
  miroModelDir <- file.path(testDir, "..", "model", modelToTest)
  Sys.setenv(MIRO_MODEL_PATH = file.path(miroModelDir,  paste0(modelToTest, ".gms")))
  Sys.setenv(GMSMODELNAME = modelToTest)
  if(modelToTest %in% c("pickstock", "kport")){
    extraClArgs <- c(additionalGamsClArgs, "MIP=CBC")
  }else{
    extraClArgs <- c()
  }
  if(length(additionalGamsClArgs)){
    saveAdditionalGamsClArgs(miroModelDir, modelToTest, c(additionalGamsClArgs, extraClArgs))
  }
  for(testFile in c("solve_model_test")){
    file.copy(file.path(testDir, "shinytest", paste0(testFile, ".R")),
              file.path(testDir, "shinytest", paste0(testFile, "_", modelToTest, ".R")),
              overwrite = TRUE)
    
  }
  test_that(sprintf("Example app: '%s' solves: ", modelToTest),
            expect_pass(testApp(file.path(testDir, ".."), paste0("solve_model_test_", modelToTest),
                                compareImages = FALSE)))
  if(modelToTest == "pickstock"){
    test_that("Interupting model run works.",
              expect_pass(testApp(file.path(testDir, ".."), "interrupt_model_test",
                                  compareImages = FALSE)))
  }
  if(length(additionalGamsClArgs)){
    file.rename(file.path(miroModelDir, paste0("conf_", modelToTest), paste0(modelToTest, "_tmp.json")),
                file.path(miroModelDir, paste0("conf_", modelToTest), paste0(modelToTest, ".json")))
  }
}
Sys.unsetenv(c("MIRO_MODEL_PATH", "MIRO_DB_PATH", "GMSMODELNAME"))

context("UI tests - Example models solve")

testDir <- file.path(getwd(), "..")

createTestDb()

if(identical(Sys.getenv("GAMS_SYS_DIR"), "")){
  message("GAMS_SYS_DIR environment variable not set. Skipping tests.")
}else{
  additionalGamsClArgs <- character(0L)
  if(!identical(Sys.getenv("MIRO_TEST_GAMS_LICE"), "")){
    additionalGamsClArgs <- paste0('license="', Sys.getenv("MIRO_TEST_GAMS_LICE"), '"')
  }
  Sys.setenv(MIRO_DB_PATH = testDir)
  if(file.exists(file.path(Sys.getenv("MIRO_DB_PATH"), "miro.sqlite3"))){
    if(unlink(file.path(Sys.getenv("MIRO_DB_PATH"), "miro.sqlite3"), force = TRUE)){
      stop("Could not remove old database SQLite file for tests")
    }
  }
  for(modelToTest in c("pickstock", "transport", "sudoku", "farming", "inscribedsquare", "tsp", "cpack", "lubrication", "kport")){
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
}
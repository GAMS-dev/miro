context("Unit tests - Hypercube builder class")
library(tidyr)
library(dplyr)

source("../../components/hcube_builder.R")

ioConfig <<- list(modelInRaw = list(`_scalars` = list(symnames = c("maxstock", "trainingdays", "solver", "clearvalueset"),
                                                      symtypes = c("parameter", "parameter", "set", "set"))),
                  modelIn = list(solver = list(type = "dropdown", dropdown = list(clearValue = FALSE))),
                  DDPar = c("__cl__gmspar_daterange_lo",
                            "__cl__gmspar_daterange_up",
                            "_gmspar_date",
                            "_gmspar_sliderrange_lo",
                            "_gmspar_sliderrange_up"),
                  GMSOpt = c("_gmsopt_lsttitleleftaligned"))

test_that("Generating hashes works", {
          hcubeBuilder <- HcubeBuilder$new(list(price = "--HCUBE_STATIC_price= 52d53711271c55d29fa6e21806171679", 
                                                maptest = "--HCUBE_STATIC_maptest= 19e47bfcc0e7d456f945ffb04fe5dba0", 
                                                `__cl__gmsopt_lsttitleleftaligned` = "lsttitleleftaligned= \"1\"", 
                                                `__cl__gmspar_date` = "--date= \"2020-07-15\"",
                                                `__cl__gmspar_daterange_lo` = NA_character_, 
                                                `__cl__gmspar_daterange_up` = NA_character_,
                                                `__cl__gmspar_numericinput` = "--numericinput= \"4000.56\"", 
                                                `__cl__gmspar_sliderrange_lo` = "--sliderrange_lo= \"7\"", 
                                                `__cl__gmspar_sliderrange_up` = "--sliderrange_up= \"22\"", 
                                                `__cl__gmspar_textinput` = NA_character_,
                                                maxstock = "--HCUBE_SCALARV_maxstock= 3", 
                                                trainingdays = "--HCUBE_SCALARV_trainingdays= 99",
                                                solver = "--HCUBE_SCALARV_solver= \"CPLEX\"", 
                                                clearvalueset = "--HCUBE_SCALARV_clearvalueset= \"element text\"",
                                                `__attach_doW_vs_index.csv` = "--HCUBE_STATIC_doW_vs_index.csv= 19e47bfcc0e7d456f945ffb04fe5dba0",
                                                `__attach_a.csv` = "--HCUBE_STATIC_a.csv= 19e47bfcc0e7d456f945ffb04fe5dba0"))
          hcubeBuilder$push("solver", c("CBC", "CONOPT"))
          # scrambling order of cl args/attachments should not change hashes
          expectedHashes <- c("f632c97f9bd8cef9bc7f5f1a8901b444ebf8918b10d29dff51973480fa846660",
                              "5d142ee84af0c08cdb4662a0febd7ee0c889132aa339194c16b0bd0a22769164")
          expect_identical(hcubeBuilder$generateScenHashes(),
                           expectedHashes)
          hcubeBuilder$setDataHashes(list(price = "--HCUBE_STATIC_price= 52d53711271c55d29fa6e21806171679", 
                                          maptest = "--HCUBE_STATIC_maptest= 19e47bfcc0e7d456f945ffb04fe5dba0", 
                                          `__attach_a.csv` = "--HCUBE_STATIC_a.csv= 19e47bfcc0e7d456f945ffb04fe5dba0",
                                          `__cl__gmspar_date` = "--date= \"2020-07-15\"",
                                          `__cl__gmspar_daterange_up` = NA_character_,
                                          `__cl__gmspar_sliderrange_lo` = "--sliderrange_lo= \"7\"", 
                                          `__cl__gmspar_sliderrange_up` = "--sliderrange_up= \"22\"", 
                                          maxstock = "--HCUBE_SCALARV_maxstock= 3", 
                                          trainingdays = "--HCUBE_SCALARV_trainingdays= 99",
                                          clearvalueset = "--HCUBE_SCALARV_clearvalueset= \"element text\"",
                                          `__attach_doW_vs_index.csv` = "--HCUBE_STATIC_doW_vs_index.csv= 19e47bfcc0e7d456f945ffb04fe5dba0",
                                          `__cl__gmspar_textinput` = NA_character_,
                                          `__cl__gmspar_daterange_lo` = NA_character_,
                                          `__cl__gmspar_numericinput` = "--numericinput= \"4000.56\"",
                                          `__cl__gmsopt_lsttitleleftaligned` = "lsttitleleftaligned= \"1\""))
          expect_identical(hcubeBuilder$generateScenHashes(),
                           expectedHashes)
          hcubeBuilder <- HcubeBuilder$new(list(price = "--HCUBE_STATIC_price= 52d53711271c55d29fa6e21806171679", 
                                                maptest = "--HCUBE_STATIC_maptest= 19e47bfcc0e7d456f945ffb04fe5dba0", 
                                                `__attach_a.csv` = "--HCUBE_STATIC_a.csv= 19e47bfcc0e7d456f945ffb04fe5dba0",
                                                `__cl__gmspar_date` = "--date= \"2020-07-15\"",
                                                `__cl__gmspar_daterange_up` = NA_character_,
                                                `__cl__gmspar_sliderrange_lo` = "--sliderrange_lo= \"7\"", 
                                                `__cl__gmspar_sliderrange_up` = "--sliderrange_up= \"22\"", 
                                                maxstock = "--HCUBE_SCALARV_maxstock= 3", 
                                                trainingdays = "--HCUBE_SCALARV_trainingdays= 99",
                                                solver = "--HCUBE_SCALARV_solver= \"CPLEX\"",
                                                clearvalueset = "--HCUBE_SCALARV_clearvalueset= \"element text\"",
                                                `__attach_doW_vs_index.csv` = "--HCUBE_STATIC_doW_vs_index.csv= 19e47bfcc0e7d456f945ffb04fe5dba0",
                                                `__cl__gmspar_textinput` = NA_character_,
                                                `__cl__gmspar_daterange_lo` = NA_character_,
                                                `__cl__gmspar_numericinput` = "--numericinput= \"4000.56\"",
                                                `__cl__gmsopt_lsttitleleftaligned` = "lsttitleleftaligned= \"1\""))
          hcubeBuilder$push("solver", c("CBC", "CONOPT"))
          expect_identical(hcubeBuilder$generateScenHashes(),
                           expectedHashes)
          
          # calculating all combinations works
          hcubeBuilder$push("trainingdays", c(1.1,2,99))
          expect_identical(hcubeBuilder$generateScenHashes(),
                           c("61840f5bb06ddf9e349bc62703a1cb1ba4a94e4d9c6651569df57f4661212b08",
                             "ecef7eee1d5ef95ac2c998d9a9a3fbc9a8684f4f988e3b77e54b233470c1303a",
                             "f632c97f9bd8cef9bc7f5f1a8901b444ebf8918b10d29dff51973480fa846660",
                             "629f9d3d1014328c924b0a07622d7d428a3c0c5ae9db6d8a357444d3e8c2463f",
                             "7bd907fdb11644a279d736ccf1a11e7feb343afd3fbb4f1b9bbe7ab36b091929",
                             "5d142ee84af0c08cdb4662a0febd7ee0c889132aa339194c16b0bd0a22769164"))
          expect_identical(hcubeBuilder$getNoScen(), 6L)
})

test_that("Getting scalars table works", {
  hcubeBuilder <- HcubeBuilder$new(list(price = "--HCUBE_STATIC_price= 52d53711271c55d29fa6e21806171679", 
                                        maptest = "--HCUBE_STATIC_maptest= 19e47bfcc0e7d456f945ffb04fe5dba0", 
                                        `__cl__gmsopt_lsttitleleftaligned` = "lsttitleleftaligned= \"1\"", 
                                        `__cl__gmspar_date` = "--date= \"2020-07-15\"",
                                        `__cl__gmspar_daterange_lo` = NA_character_, 
                                        `__cl__gmspar_daterange_up` = NA_character_,
                                        `__cl__gmspar_numericinput` = "--numericinput= \"4000.56\"", 
                                        `__cl__gmspar_sliderrange_lo` = "--sliderrange_lo= \"7\"", 
                                        `__cl__gmspar_sliderrange_up` = "--sliderrange_up= \"22\"", 
                                        `__cl__gmspar_textinput` = NA_character_,
                                        maxstock = "--HCUBE_SCALARV_maxstock= 3", 
                                        trainingdays = "--HCUBE_SCALARV_trainingdays= 99",
                                        solver = "--HCUBE_SCALARV_solver= \"CPLEX\"", 
                                        clearvalueset = "--HCUBE_SCALARV_clearvalueset= \"element text\"",
                                        `__attach_doW_vs_index.csv` = "--HCUBE_STATIC_doW_vs_index.csv= 19e47bfcc0e7d456f945ffb04fe5dba0",
                                        `__attach_a.csv` = "--HCUBE_STATIC_a.csv= 19e47bfcc0e7d456f945ffb04fe5dba0"))
  hcubeBuilder$push("solver", c("CBC", "CONOPT"))$generateScenHashes()
  expectedHashes <- c("f632c97f9bd8cef9bc7f5f1a8901b444ebf8918b10d29dff51973480fa846660",
                      "5d142ee84af0c08cdb4662a0febd7ee0c889132aa339194c16b0bd0a22769164")
  expect_identical(hcubeBuilder$getHcubeScalars(),
                   tibble(`_hash` = expectedHashes,
                          scalar = c("solver", "solver"),
                          value = c("CBC", "CONOPT")))
  hcubeBuilder$push("trainingdays", c(1.123,10000))$push("maxstock", c(2.8,3.8))$generateScenHashes()
  expect_identical(hcubeBuilder$getHcubeScalars(),
                   tibble(`_hash` = c("f010631b31e03486a486f06ae6b206af1cfe89926932d7db9f614285d18b7ab3",
                                      "f010631b31e03486a486f06ae6b206af1cfe89926932d7db9f614285d18b7ab3",
                                      "f010631b31e03486a486f06ae6b206af1cfe89926932d7db9f614285d18b7ab3",
                                      "6412700864bbdb2b4f8e15857cfd162d4277f63d9146c60962b5139fe864a000",
                                      "6412700864bbdb2b4f8e15857cfd162d4277f63d9146c60962b5139fe864a000",
                                      "6412700864bbdb2b4f8e15857cfd162d4277f63d9146c60962b5139fe864a000",
                                      "b3794f8293c4663afbae4b9dc75e3427955d9ca10898e3d8beeaff3328d6e27a",
                                      "b3794f8293c4663afbae4b9dc75e3427955d9ca10898e3d8beeaff3328d6e27a",
                                      "b3794f8293c4663afbae4b9dc75e3427955d9ca10898e3d8beeaff3328d6e27a",
                                      "513fe3d8b81702614b9a84794d0e73756add75b3d20efb58a66febe3c8378a3c",
                                      "513fe3d8b81702614b9a84794d0e73756add75b3d20efb58a66febe3c8378a3c",
                                      "513fe3d8b81702614b9a84794d0e73756add75b3d20efb58a66febe3c8378a3c",
                                      "140c43eff962357d79d31b5e3a1816372120af7d719fa0cc4c8a075680ed0235",
                                      "140c43eff962357d79d31b5e3a1816372120af7d719fa0cc4c8a075680ed0235",
                                      "140c43eff962357d79d31b5e3a1816372120af7d719fa0cc4c8a075680ed0235",
                                      "8442142d4a4de8d46c3e3143cd3d9d29d0261d4cde648e547a5a71aebe756a35",
                                      "8442142d4a4de8d46c3e3143cd3d9d29d0261d4cde648e547a5a71aebe756a35",
                                      "8442142d4a4de8d46c3e3143cd3d9d29d0261d4cde648e547a5a71aebe756a35",
                                      "4aed728ab2e629cd10812b76a7fca74a2d34c80037d7fb0ae9b61319a083e64f",
                                      "4aed728ab2e629cd10812b76a7fca74a2d34c80037d7fb0ae9b61319a083e64f",
                                      "4aed728ab2e629cd10812b76a7fca74a2d34c80037d7fb0ae9b61319a083e64f",
                                      "9fc88943e6d86a234504ef043b91202f4bab2ab38888ede948e5d762dcf611f0",
                                      "9fc88943e6d86a234504ef043b91202f4bab2ab38888ede948e5d762dcf611f0",
                                      "9fc88943e6d86a234504ef043b91202f4bab2ab38888ede948e5d762dcf611f0"),
                          scalar = c("maxstock", "trainingdays", "solver", "maxstock", 
                                     "trainingdays", "solver", "maxstock", "trainingdays", "solver", 
                                     "maxstock", "trainingdays", "solver", "maxstock", "trainingdays", 
                                     "solver", "maxstock", "trainingdays", "solver", "maxstock", "trainingdays", 
                                     "solver", "maxstock", "trainingdays", "solver"),
                          value = c("2.8", "1.123", "CBC", "3.8", "1.123", "CBC", "2.8", "10000", "CBC",
                                    "3.8", "10000", "CBC", "2.8", "1.123", "CONOPT", "3.8", "1.123",
                                    "CONOPT", "2.8", "10000", "CONOPT", "3.8", "10000", "CONOPT")))
})

test_that("Slider ranges work", {
  hcubeBuilder <- HcubeBuilder$new(list(price = "--HCUBE_STATIC_price= 52d53711271c55d29fa6e21806171679", 
                                        maptest = "--HCUBE_STATIC_maptest= 19e47bfcc0e7d456f945ffb04fe5dba0", 
                                        `__cl__gmsopt_lsttitleleftaligned` = "lsttitleleftaligned= \"1\"", 
                                        `__cl__gmspar_date` = "--date= \"2020-07-15\"",
                                        `__cl__gmspar_daterange_lo` = NA_character_, 
                                        `__cl__gmspar_daterange_up` = NA_character_,
                                        `__cl__gmspar_numericinput` = "--numericinput= \"4000.56\"", 
                                        `__cl__gmspar_sliderrange_lo` = "--sliderrange_lo= \"7\"", 
                                        `__cl__gmspar_sliderrange_up` = "--sliderrange_up= \"22\"", 
                                        `__cl__gmspar_textinput` = NA_character_,
                                        maxstock = "--HCUBE_SCALARV_maxstock= 3", 
                                        trainingdays = "--HCUBE_SCALARV_trainingdays= 99",
                                        solver = "--HCUBE_SCALARV_solver= \"CPLEX\"", 
                                        clearvalueset = "--HCUBE_SCALARV_clearvalueset= \"element text\"",
                                        `__attach_doW_vs_index.csv` = "--HCUBE_STATIC_doW_vs_index.csv= 19e47bfcc0e7d456f945ffb04fe5dba0",
                                        `__attach_a.csv` = "--HCUBE_STATIC_a.csv= 19e47bfcc0e7d456f945ffb04fe5dba0"))
  hcubeBuilder$pushRange("_gmspar_sliderrange_lo", "_gmspar_sliderrange_up",
                         c(3.3,7.901), allCombinations = FALSE)
  expect_identical(hcubeBuilder$generateScenHashes(), "dbef29d99c1599a9b5c59b171bcda71f13c4df299b9a9b57fd4e018a93a5189c")
  expect_identical(hcubeBuilder$getNoScen(), 1L)
  combinations <- getCombinationsSlider(1, 2, 0.5)
  expect_identical(combinations, list(min = c(1,1.5,2,1,1.5,1), max = c(2,2,2,1.5,1.5,1)))
  hcubeBuilder$pushRange("_gmspar_sliderrange_lo", "_gmspar_sliderrange_up",
                         combinations, allCombinations = TRUE)$generateScenHashes()
  expect_identical(hcubeBuilder$getNoScen(), 6L)
  expect_identical(hcubeBuilder$getHcubeScalars(),
                   tibble(`_hash` = c("5ec08dc65d30d8c83e224cb686d727affa92b878341aa5f4582fb6a970f4cd92",
                                      "5ec08dc65d30d8c83e224cb686d727affa92b878341aa5f4582fb6a970f4cd92",
                                      "9e3bb7bb25633f5957fd4ab4ed98195111daab6bca120dd0a00999202a5d5c57",
                                      "9e3bb7bb25633f5957fd4ab4ed98195111daab6bca120dd0a00999202a5d5c57",
                                      "231df34300b05f5956fefd1b9f8d8ad99f7ee6021b95029b240d2b0cdd79efb6",
                                      "231df34300b05f5956fefd1b9f8d8ad99f7ee6021b95029b240d2b0cdd79efb6",
                                      "f97451cc6bcd14fa900ca4de90b1706c664c659bb05dc4ed728a773af2acc727",
                                      "f97451cc6bcd14fa900ca4de90b1706c664c659bb05dc4ed728a773af2acc727",
                                      "e88848088daadd148208f6749a75efdac51cec620541f0ec1aaa19f53a8eca3d",
                                      "e88848088daadd148208f6749a75efdac51cec620541f0ec1aaa19f53a8eca3d",
                                      "f49f751176909d9e2a941c20d00ffb6e9f2796eaaf75f568d71515834e75eaec",
                                      "f49f751176909d9e2a941c20d00ffb6e9f2796eaaf75f568d71515834e75eaec"),
                          scalar = c("__cl__gmspar_sliderrange_lo", "__cl__gmspar_sliderrange_up", 
                                     "__cl__gmspar_sliderrange_lo", "__cl__gmspar_sliderrange_up", 
                                     "__cl__gmspar_sliderrange_lo", "__cl__gmspar_sliderrange_up", 
                                     "__cl__gmspar_sliderrange_lo", "__cl__gmspar_sliderrange_up", 
                                     "__cl__gmspar_sliderrange_lo", "__cl__gmspar_sliderrange_up", 
                                     "__cl__gmspar_sliderrange_lo", "__cl__gmspar_sliderrange_up"),
                          value = c("1", "2", "1.5", "2", "2", "2", "1", "1.5", "1.5", "1.5", "1", "1")))
})


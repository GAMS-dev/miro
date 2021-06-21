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
scalarDf <- tibble(`_gmsopt_lsttitleleftaligned` = "1",
                   `_gmspar_date` = "2020-07-15",
                   `_gmspar_daterange_lo` = NA_character_,
                   `_gmspar_daterange_up` = NA_character_,
                   `_gmspar_numericinput` = "4000.56",
                   `_gmspar_sliderrange_lo` = "7",
                   `_gmspar_sliderrange_up` = "22",
                   `_gmspar_textinput` = NA_character_,
                   maxstock = "3",
                   trainingdays = "7",
                   solver = "CPLEX",
                   clearvalueset = "element text")

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
                                        `__xattach_doW_vs_index.csv` = "--HCUBE_STATIC_doW_vs_index.csv= 19e47bfcc0e7d456f945ffb04fe5dba0",
                                        `__xattach_a.csv` = "--HCUBE_STATIC_a.csv= 19e47bfcc0e7d456f945ffb04fe5dba0"),
                                   scalarDf)
  hcubeBuilder$push("solver", c("CBC", "CONOPT"))
  # scrambling order of cl args/attachments should not change hashes
  expectedHashes <- c("c1d7921fac38e27f5db15a8ed404900063be7c368dcd78634eefcef8be42a1d0",
                      "e64efedd378b24591a3d468b1c64af9afecc8e213dc821bed8e4dc26ec19f99a")
  expect_identical(hcubeBuilder$generateScenHashes(),
                   expectedHashes)
  hcubeBuilder$setDataHashes(list(price = "--HCUBE_STATIC_price= 52d53711271c55d29fa6e21806171679", 
                                  maptest = "--HCUBE_STATIC_maptest= 19e47bfcc0e7d456f945ffb04fe5dba0", 
                                  `__xattach_a.csv` = "--HCUBE_STATIC_a.csv= 19e47bfcc0e7d456f945ffb04fe5dba0",
                                  `__cl__gmspar_date` = "--date= \"2020-07-15\"",
                                  `__cl__gmspar_daterange_up` = NA_character_,
                                  `__cl__gmspar_sliderrange_lo` = "--sliderrange_lo= \"7\"", 
                                  `__cl__gmspar_sliderrange_up` = "--sliderrange_up= \"22\"", 
                                  maxstock = "--HCUBE_SCALARV_maxstock= 3", 
                                  trainingdays = "--HCUBE_SCALARV_trainingdays= 99",
                                  clearvalueset = "--HCUBE_SCALARV_clearvalueset= \"element text\"",
                                  `__xattach_doW_vs_index.csv` = "--HCUBE_STATIC_doW_vs_index.csv= 19e47bfcc0e7d456f945ffb04fe5dba0",
                                  `__cl__gmspar_textinput` = NA_character_,
                                  `__cl__gmspar_daterange_lo` = NA_character_,
                                  `__cl__gmspar_numericinput` = "--numericinput= \"4000.56\"",
                                  `__cl__gmsopt_lsttitleleftaligned` = "lsttitleleftaligned= \"1\""),
                             scalarDf)
  expect_identical(hcubeBuilder$generateScenHashes(),
                   expectedHashes)
  hcubeBuilder <- HcubeBuilder$new(list(price = "--HCUBE_STATIC_price= 52d53711271c55d29fa6e21806171679", 
                                        maptest = "--HCUBE_STATIC_maptest= 19e47bfcc0e7d456f945ffb04fe5dba0", 
                                        `__xattach_a.csv` = "--HCUBE_STATIC_a.csv= 19e47bfcc0e7d456f945ffb04fe5dba0",
                                        `__cl__gmspar_date` = "--date= \"2020-07-15\"",
                                        `__cl__gmspar_daterange_up` = NA_character_,
                                        `__cl__gmspar_sliderrange_lo` = "--sliderrange_lo= \"7\"", 
                                        `__cl__gmspar_sliderrange_up` = "--sliderrange_up= \"22\"", 
                                        maxstock = "--HCUBE_SCALARV_maxstock= 3", 
                                        trainingdays = "--HCUBE_SCALARV_trainingdays= 99",
                                        solver = "--HCUBE_SCALARV_solver= \"CPLEX\"",
                                        clearvalueset = "--HCUBE_SCALARV_clearvalueset= \"element text\"",
                                        `__xattach_doW_vs_index.csv` = "--HCUBE_STATIC_doW_vs_index.csv= 19e47bfcc0e7d456f945ffb04fe5dba0",
                                        `__cl__gmspar_textinput` = NA_character_,
                                        `__cl__gmspar_daterange_lo` = NA_character_,
                                        `__cl__gmspar_numericinput` = "--numericinput= \"4000.56\"",
                                        `__cl__gmsopt_lsttitleleftaligned` = "lsttitleleftaligned= \"1\""),
                                   scalarDf)
  hcubeBuilder$push("solver", c("CBC", "CONOPT"))
  expect_identical(hcubeBuilder$generateScenHashes(),
                   expectedHashes)
  
  # calculating all combinations works
  hcubeBuilder$push("trainingdays", c(1.1,2,99))
  expect_identical(hcubeBuilder$generateScenHashes(),
                   c("191f29a7a6a62e21d783bc9500946bc9468ba46c6d986b6dceaee838e77edb52",
                     "30087f64e400422bcd109f8be674fe12f454abffec2ba5991d8c5686a08011e4",
                     "c1d7921fac38e27f5db15a8ed404900063be7c368dcd78634eefcef8be42a1d0",
                     "bdf2fda5ec409b8bb29a2715304afda3a5d12fc228e1eaef1a8431726f397244",
                     "bb0da8c47b7b79135721c0e5421344649c971da79e8ba5f1068f43aaef823bbe",
                     "e64efedd378b24591a3d468b1c64af9afecc8e213dc821bed8e4dc26ec19f99a"))
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
                                        `__xattach_doW_vs_index.csv` = "--HCUBE_STATIC_doW_vs_index.csv= 19e47bfcc0e7d456f945ffb04fe5dba0",
                                        `__xattach_a.csv` = "--HCUBE_STATIC_a.csv= 19e47bfcc0e7d456f945ffb04fe5dba0"),
                                   scalarDf)
  hcubeBuilder$push("solver", c("CBC", "CONOPT"))$generateScenHashes()
  expectedHashes <- c("c1d7921fac38e27f5db15a8ed404900063be7c368dcd78634eefcef8be42a1d0",
                      "e64efedd378b24591a3d468b1c64af9afecc8e213dc821bed8e4dc26ec19f99a")
  scalars <- hcubeBuilder$getHcubeScalars()
  expect_identical(nrow(scalars), 12L * 2L)
  expect_identical(unique(scalars[[1]]), expectedHashes)
  expect_identical(dplyr::select(tidyr::pivot_wider(scalars, names_from = "scalar", values_from = "value"),
                                 solver)[[1]], c("CBC", "CONOPT"))
  expect_identical( dplyr::distinct(
    dplyr::select(
      tidyr::pivot_wider(scalars, names_from = "scalar", values_from = "value"),
      -c(solver, `_hash`))), dplyr::select(scalarDf, !solver))
  
  hcubeBuilder$push("trainingdays", c(1.123,10000))$push("maxstock", c(2.8,3.8))$generateScenHashes()
  expectedHashes <- c("ed1eb93a9339bf3c5aca3f010745c16768240bcfd52e21d397d04e256e79b30e",
                      "f89674b63acd3771a98f6cdf340610c1b51d001ee8205cf22d009403006aa863",
                      "138f960e39e811660b350c106ca697071f6c84ad14e97edfee2f1ed008f33711",
                      "26611485fde3356b62b16266014d0f5bda1eec2420480061e80674a57b3184f8",
                      "d82b1588a9a53edd6dd733896bcc8c189fbf686f3238c28ddbe23283dd35871f",
                      "c8070d029311ce8aff7fbe724916137f45088098d06fbe00cc55b206fc2cdcad",
                      "df6161041e1f1bd560f37b63b3a76bee67bbd38d7fb269de329353adbf4c3489",
                      "9a4e8d6a5c6eaea2593bf754b471cba575f7ca366e0f0f6f16e8343c4c4ea176")
  scalars <- hcubeBuilder$getHcubeScalars()
  expect_identical(nrow(scalars), 12L * 8L)
  expect_identical(unique(scalars[[1]]), expectedHashes)
  expect_identical(dplyr::select(tidyr::pivot_wider(scalars, names_from = "scalar", values_from = "value"),
                                 solver)[[1]],
                   c("CBC", "CBC", "CBC", "CBC", "CONOPT", "CONOPT", "CONOPT", "CONOPT"))
  expect_identical(dplyr::select(tidyr::pivot_wider(scalars, names_from = "scalar", values_from = "value"),
                                 trainingdays)[[1]],
                   c("1.123", "1.123", "10000", "10000", "1.123", "1.123", "10000", "10000"))
  expect_identical(dplyr::select(tidyr::pivot_wider(scalars, names_from = "scalar", values_from = "value"),
                                 maxstock)[[1]],
                   c("2.8", "3.8", "2.8", "3.8", "2.8", "3.8", "2.8", "3.8"))
  expect_identical(dplyr::distinct(
    dplyr::select(
      tidyr::pivot_wider(scalars, names_from = "scalar", values_from = "value"),
      -c(trainingdays, maxstock, solver, `_hash`))),
    dplyr::select(scalarDf, !c(trainingdays, solver, maxstock)))
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
                                        `__xattach_doW_vs_index.csv` = "--HCUBE_STATIC_doW_vs_index.csv= 19e47bfcc0e7d456f945ffb04fe5dba0",
                                        `__xattach_a.csv` = "--HCUBE_STATIC_a.csv= 19e47bfcc0e7d456f945ffb04fe5dba0"),
                                   scalarDf)
  hcubeBuilder$pushRange("_gmspar_sliderrange_lo", "_gmspar_sliderrange_up",
                         c(3.3,7.901), allCombinations = FALSE)
  expect_identical(hcubeBuilder$generateScenHashes(), "760739f9efeae17d714ca083f2b10b8f300716eb3bcf6bcdd29230cc8420c1aa")
  expect_identical(hcubeBuilder$getNoScen(), 1L)
  combinations <- getCombinationsSlider(1, 2, 0.5)
  expect_identical(combinations, list(min = c(1,1.5,2,1,1.5,1), max = c(2,2,2,1.5,1.5,1)))
  hcubeBuilder$pushRange("_gmspar_sliderrange_lo", "_gmspar_sliderrange_up",
                         combinations, allCombinations = TRUE)$generateScenHashes()
  expect_identical(hcubeBuilder$getNoScen(), 6L)
  
  expectedHashes <- c("a18852ec481a49db2ffbc4781a3529970ed8e079d5b02307da787fcae66c48e4",
                      "8349bf7e2f06adc7d48e134f29e4f18f2690b0c2efe8e1e0e816e148df47e870",
                      "c36610cf83ec456de62f371198473b9fb3da7ac504b000c4972b631dd7307e85",
                      "db12d6d2693e8aeb9c163ead52feb71da052c592aadb0c4c5aefbcd1d3e01a5a",
                      "0b957a2d40ffb1e5aafdd384dc2e00b1a82a8cdc96983d9d44198166e2100a0d",
                      "87cc681689777a2e8add35ec9ad39bc06e560c1e516ae9f6a76564ed38190c13")
  scalars <- hcubeBuilder$getHcubeScalars()
  expect_identical(nrow(scalars), 12L * 6L)
  expect_identical(unique(scalars[[1]]), expectedHashes)
  expect_identical(dplyr::select(tidyr::pivot_wider(scalars, names_from = "scalar", values_from = "value"),
                                 `_gmspar_sliderrange_lo`)[[1]], c("1", "1.5", "2", "1", "1.5", "1"))
  expect_identical(dplyr::select(tidyr::pivot_wider(scalars, names_from = "scalar", values_from = "value"),
                                 `_gmspar_sliderrange_up`)[[1]], c("2", "2", "2", "1.5", "1.5", "1"))
  expect_identical(dplyr::distinct(
    dplyr::select(
      tidyr::pivot_wider(scalars, names_from = "scalar", values_from = "value"),
      -c(`_gmspar_sliderrange_lo`, `_gmspar_sliderrange_up`, `_hash`))),
    dplyr::select(scalarDf, !c(`_gmspar_sliderrange_lo`, `_gmspar_sliderrange_up`)))
})

test_that("Subsetting scenario hashes works", {
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
                                          `__xattach_doW_vs_index.csv` = "--HCUBE_STATIC_doW_vs_index.csv= 19e47bfcc0e7d456f945ffb04fe5dba0",
                                          `__xattach_a.csv` = "--HCUBE_STATIC_a.csv= 19e47bfcc0e7d456f945ffb04fe5dba0"),
                                     scalarDf)
    scenHashes <- hcubeBuilder$push("solver", c("CBC", "CONOPT"))$push("maxstock", c(1, 3, 5, 12))$generateScenHashes()
    hcubeBuilder$removeScen(scenHashes[c(2,4,7)])
    expect_identical(hcubeBuilder$getNoScen(), length(scenHashes) - 3L)
    scalars <- hcubeBuilder$getHcubeScalars()
    scenHashesScalars <- scalars[["_hash"]]
    expect_identical(length(unique(scenHashesScalars)), length(scenHashes) - 3L)
    expect_identical(unique(scenHashesScalars), scenHashes[c(1, 3, 5, 6, 8)])
    expect_identical(dplyr::select(tidyr::pivot_wider(scalars, names_from = "scalar", values_from = "value"),
                                   solver)[[1]], c("CBC", "CBC", "CONOPT", "CONOPT", "CONOPT"))
    expect_identical(dplyr::select(tidyr::pivot_wider(scalars, names_from = "scalar", values_from = "value"),
                                   maxstock)[[1]], c("1", "5", "1", "3", "12"))
    expect_identical(dplyr::distinct(
      dplyr::select(
        tidyr::pivot_wider(scalars, names_from = "scalar", values_from = "value"),
        -c(maxstock, solver, `_hash`))),
      dplyr::select(scalarDf, !c(maxstock, solver)))
    
    hcubeBuilder$setDataHashes(list(price = "--HCUBE_STATIC_price= 52d53711271c55d29fa6e21806171679", 
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
                                    `__xattach_doW_vs_index.csv` = "--HCUBE_STATIC_doW_vs_index.csv= 19e47bfcc0e7d456f945ffb04fe5dba0",
                                    `__xattach_a.csv` = "--HCUBE_STATIC_a.csv= 19e47bfcc0e7d456f945ffb04fe5dba0"),
                               scalarDf)
    scenHashes <- hcubeBuilder$push("solver", c("CBC", "CONOPT"))$push("maxstock", c(1, 3, 5, 12))$generateScenHashes()
    expect_identical(hcubeBuilder$getNoScen(), length(scenHashes))
    scalars <- hcubeBuilder$getHcubeScalars()
    scenHashesScalars <- scalars[["_hash"]]
    expect_identical(length(unique(scenHashesScalars)), length(scenHashes))
})


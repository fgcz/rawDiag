#R


library(rawDiag)
library(testthat)

rawrr::sampleFilePath() |> rawDiag::readRaw() -> S

test_that("check read.raw", {
	expect_true(is.rawDiag(S))
})



test_that("check plotCycleTime", {
  
  plotCycleTime(S) -> gp
  
  expect_identical(rawDiag:::.cycleTime(S)$CycleTime, gp$data$CycleTime)
  
})


test_that("check plotInjectionTime", {
  
  plotInjectionTime(S) -> gp
  
  expect_identical(S$InjectionTime, gp$data$InjectionTime)
  
})

test_that("check .fillNAgaps", {
  gt <- c(NA, 1, 2, 3, 3, 4, 5, 5, 5, 5, 6)
 
  input <- c(NA, 1, 2, 3, NA, 4, 5, NA, NA, NA, 6)
 
  expect_equal(gt, rawDiag:::.fillNAgaps(input))
})


test_that("check .calculatioMasterScan", {
 gt <- c(529, 529, 529, 529, 529, 529, NA, 551, 551, 551, 551, 551, 551,
 551, 551, 551, 551, 551, 551, 551, 551, 551, 551, 551, 551, 551, 551, 551, NA,573)
 
 output <- (S |> rawDiag:::.calculatioMasterScan())$MasterScan |> tail(30)
 expect_equal(gt, output)
})

#R

context("plot*")

library(rawDiag)
library(testthat)

rawrr::sampleFilePath() |> rawDiag::read.raw() -> S

test_that("check read.raw", {
	expect_true(is.rawDiag(S))
})



test_that("check CycleTime", {
  
  plotCycleTime(S) -> gp
  
  expect_identical(rawDiag:::.cycleTime(S)$CycleTime, gp$data$CycleTime)
  
})


test_that("check InjectionTime", {
  
  plotInjectionTime(S) -> gp
  
  expect_identical(S$InjectionTime, gp$data$InjectionTime)
  
})

#R

context("read.raw")


test_that("check read.raw function.", {
  library(rawDiag)
  
  rawfile <- file.path(path.package(package = 'rawDiag'), 'extdata', 'sample.raw')
  RAW <- read.raw(file = rawfile)
  
  expect_s3_class(RAW, 'rawDiag')
  expect_true(is.rawDiag(RAW))
  expect_true(nrow(RAW) == 573)
  expect_true(ncol(RAW) == 21)
  
  RAW <- read.raw(file = rawfile, rawDiag = FALSE)
  expect_s3_class(RAW, 'rawDiag')
  expect_false(is.rawDiag(RAW))
  expect_true(ncol(RAW) == 82)
})
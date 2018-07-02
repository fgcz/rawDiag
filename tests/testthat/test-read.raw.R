#R

context("read.raw")


test_that("check read.raw function.", {
  library(rawDiag)
  
  rawfile <- file.path(path.package(package = 'rawDiag'), 'extdata', 'sample.raw')
  RAW1 <- read.raw(file = rawfile)
  
  expect_s3_class(RAW1, 'rawDiag')
  expect_true(is.rawDiag(RAW1))
  expect_true(nrow(RAW1) == 573)
  expect_true(ncol(RAW1) == 21)
  
  RAW2 <- read.raw(file = rawfile, rawDiag = FALSE)
  expect_s3_class(RAW2, 'rawDiag')
  expect_false(is.rawDiag(RAW2))
  expect_true(ncol(RAW2) == 82)
  
  # system2_call = TRUE should always work
  RAW3 <- read.raw(file = rawfile, system2_call = TRUE)
  expect_s3_class(RAW3, 'rawDiag')
  expect_true(is.rawDiag(RAW3))
  expect_true(nrow(RAW3) == 573)
  expect_true(ncol(RAW3) == 21)
  
  RAW4 <- read.raw(file = rawfile, system2_call = TRUE, rawDiag = FALSE)
  expect_s3_class(RAW4, 'rawDiag')
  expect_false(is.rawDiag(RAW4))
  expect_true(ncol(RAW4) == 82)
})
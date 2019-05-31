#R

context("read.raw")


test_that("check rDotNet.", {
  library(rawDiag)
  
  rawfile <- file.path(path.package(package = 'rawDiag'), 'extdata', 'sample.raw')

  RAWcs <- csReadScans(rawfile)
  expect_true(length(RAWcs) == 574)

  idx <- 1:574
  RAWcs <- csReadScans(rawfile, idx)
  RAWf <- readScans(rawfile, idx)

  rv<- lapply(idx, function(i){
	expect_equal(RAWf[[i]]$intensity, RAWcs[[i]]$intensity)
	expect_equal(RAWf[[i]]$mZ, RAWcs[[i]]$mZ)
	expect_equal(RAWf[[i]]$charge, RAWcs[[i]]$charge)
	expect_equal(RAWf[[i]]$monoisotopicMz, RAWcs[[i]]$monoisotopicMz)
	expect_equal(RAWf[[i]]$pepmass, RAWcs[[i]]$pepmass)
  	expect_s3_class(RAWf[[i]], 'peaklist')
  	expect_s3_class(RAWcs[[i]], 'peaklist')
	NULL
  })

})

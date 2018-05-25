#R

context("mzR")


test_that("compare scans 5000:5010 mzR and WU163763", {
  library(rawDiag)
  library(mzR)
  
  mzML <- "20180220_04_S174020_Pierce_HeLa_Protein_Digest_Std_5000_5010.mzML"
  mzML <- file.path(path.package(package = "rawDiag"), "extdata", mzML)
  RAW <- rawDiag:::as.rawDiag.mzR(openMSfile(mzML))

  data(WU163763)
  idx <- which((WU163763$filename == "20180220_04_S174020_Pierce_HeLa_Protein_Digest_Std.raw" 
                & WU163763$scanNumber %in% 5000:5010))

  
  lapply(names(WU163763)[c(2:6)], function(x){expect_identical(WU163763[idx, x], RAW[,x ])})
})
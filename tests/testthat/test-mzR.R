#R

context("mzR")

if(require('mzR')){
  
  
  test_that("compare scans 5000:5010 mzR and WU163763", {
    library(rawDiag)
    library(mzR)
    
    mzML <- "04_S174020_5000_5010.mzML"
    mzML <- file.path(path.package(package = "rawDiag"), "extdata", mzML)
    RAW <- rawDiag:::as.rawDiag.mzR(openMSfile(mzML))
    
    data(WU163763)
    idx <- which((WU163763$filename == "04_S174020" 
                  & WU163763$scanNumber %in% 5000:5010))
    
    df <- WU163763[idx,]
    expect_true(all.equal(df$StartTime, RAW$StartTime))
    
  })
  
}

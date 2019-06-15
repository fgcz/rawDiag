#R 
# FGCZ, TK,CP 2019-06-14


#' Force an peaklistSet Object to Belong to a Class DataFrame
#'
#' @param x an \code{\link[protViz]{peaklistSet}}} object.
#' @param ... 
#'
#' @author TK,CP 2019-06-14
#' @return \code{\link[S4Vectors]{DataFrane} object}
#' @export as.peaklistSet.DataFrame
#' @examples 
#' library(rawDiag)
#' (rawfile <- file.path(path.package(package = 'rawDiag'), 'extdata', 'sample.raw'))
#' PLS <-readScans(rawfile)
#' DF <- as.peaklistSet.DataFrame(PLS)
#' DF$fromFile = as.integer(1)
#' 
#' if(require(Spectra)){
#'   rawDiagSample <- MsBackendDataFrame()
#'   BE <- backendInitialize(object=rawDiagSample, files=rawfile, spectraData=DF)
#' }
as.peaklistSet.DataFrame <- function(x, ...){
  if (!require(S4Vectors)){ warning("pkg 'S4Vectors' not installed."); return (NULL) }
  
  n <- length(x)
  df <- DataFrame(
    msLevel = sapply(x, function(y){as.integer(y$msLevel)}),
    charge = sapply(x, function(y){as.integer(y$charge)}),
    rtime = sapply(x, function(y){y$rtinseconds}),
    scanIndex = sapply(x, function(y){y$scan}),
    polarity = sapply(x, function(y){as.integer(y$polarity)}),
    id = rep(NA, n),
    name = sapply(x, function(y){y$title}))
  df$mz <- lapply(x, function(y){y$mZ})
  df$intensity <- lapply(x, function(y){y$intensity})
  
  df
}

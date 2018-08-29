#R


#' extracting XIC of a given set of peptides
#'
#' @param rawfilename
#' @param pepSeq 
#' @author CP/BR
#' @description ALLES wird gut
#' @return
#' @export
#' @importFrom protViz parentIonMass peakplot
#' @examples
.XICextraction <- function(rawfilename="/Users/cp/data//Data_small/F3_20170823_02_autoQC01_threshold.raw",
                           pepSeq=c("AGGSSEPVTGLADK", "GDLDAASYYAPVR", "FLLQFGAQGSPLFK"), debug=FALSE){
  
  pepSeq.n <- length(pepSeq)
  stopifnot(pepSeq.n > 0)
  
  mass2Hplus <- (parentIonMass(pepSeq) + 1.008) / 2
  
  X <- readXICs(rawfile = rawfilename, masses = mass2Hplus)
  metadata <- read.raw(rawfilename)
  
  if (debug){
    op <- par(mfrow = c(3, 1))
    rv <- lapply(1:3, function(i){plot(X[[i]], main = pepSeq[i])})
  }
  
  # PlotMassHeatmap(metadata)
  if(debug){
    rv <- lapply(mass2Hplus, function(m){
      plot(metadata$StartTime, metadata$PrecursorMass - m, pch = '.',
           ylim = c(-0.25,0.25), main = paste("[m+2H]2+ =", m));
      abline(h = 0, col = "#77000088");
    })
  }
 
  
  HCDIons <- function(b, y){
    Hydrogen <- 1.007825
    Oxygen <- 15.994915
    Nitrogen <- 14.003074
    return(cbind(b, y))
  }
  
  scanNumbers <- lapply(mass2Hplus, function(m){
    metadata$scanNumber[which(abs(metadata$PrecursorMass-m)<0.1)]
    })
  
  # compute a psm score
  bestMatchingMS2Scan <- sapply(1:pepSeq.n, function(i){
    PL <- readScans(rawfilename, scans = scanNumbers[[i]])
    pp <- lapply(PL, function(x){psm(pepSeq[i], x, FUN = HCDIons, plot = FALSE)})
    score <- sapply(pp, function(x){sum(abs(x$mZ.Da.error) < 0.1)}) #find best scoring spectra
    #score <- sapply(1:length(PL), function(j){
    #  sum(PL[[j]]$intensity[abs(pp[[j]]$mZ.Da.error) < 0.1])
    #  }) 
    
    bestFirstMatch <- which(max(score) == score)[1]
    
    if(debug){
      plot(score, main = pepSeq[i])
      abline(v = bestFirstMatch, col = "red")
    }
    scanNumbers[[i]][bestFirstMatch]
  })
  
  
  PL <- readScans(rawfilename, scans = bestMatchingMS2Scan)
  
  # APEX MS2
  APEXms2 <- sapply(1:pepSeq.n, function(i){
    t <- metadata$StartTime[bestMatchingMS2Scan[i]]; t
    })
  
 
  # compute APEX
  APEX.max <- sapply(1:pepSeq.n, function(i){
    # determine the XIC interval
    idx <- (APEXms2[i] - 0.5) < X[[i]]$times & X[[i]]$times < (APEXms2[i] + 0.5) 
    max(X[[i]]$intensities[idx])[1]
  })
  
  # compute APEX
  APEX.t <- sapply(1:pepSeq.n, function(i){
    # determine the XIC interval
    idx <- (APEXms2[i] - 0.5) < X[[i]]$times & X[[i]]$times < (APEXms2[i] + 0.5) 
    X[[i]]$times[idx][(X[[i]]$intensities[idx] == APEX.max[i])][1]
  })
  
  
  APEX.APU<- sapply(1:pepSeq.n, function(i){
    # determine the XIC interval
    idx <- (APEXms2[i] - 0.5) < X[[i]]$times & X[[i]]$times < (APEXms2[i] + 0.5) 
    
    # determine AUC
    x <- X[[i]]$times[idx]
    y <- X[[i]]$intensities[idx]
    sum(diff(x) * (head(y, -1) + tail(y, -1))) / 2
  })
  
  # TODO(cp): do the fit using lm and determine the AUC
  
  if(debug){
    op <- par(mfrow = c(3, 1), mar=c(5,4,4,1))
    
    p <- lapply(1:pepSeq.n,
                function(i){
                  t <- metadata$StartTime[bestMatchingMS2Scan[i]];
                  plot(X[[i]], xlim = c(t - 0.5, t + 0.5)); 
                  abline(v=t, col='red')
                  
                  idx <- (t - 0.5) < X[[i]]$times & X[[i]]$times < (t + 0.5)
                
                  axis(3, APEX.t[i], APEX.t[i])
                  t
                })
  }
 
  rv <- data.frame(APEXms2 = APEXms2, pepSeq = pepSeq, mass2Hplus = mass2Hplus,
                   APEX.t = APEX.t,
                   APEX.max = APEX.max,
                   APEX.APU = APEX.APU)
  rv$rawfilename <- basename(rawfilename)
  return(rv)
}
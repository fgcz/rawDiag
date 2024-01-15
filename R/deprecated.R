#R

.checkRawrr <- function(){
  if (isFALSE(rawrr::.checkDllInMonoPath())){
      rawrr::installRawFileReaderDLLs()
  }
  if (isFALSE(file.exists(rawrr:::.rawrrAssembly()))){
     rawrr::installRawrrExe()
  }
}

#' @export
PlotLockMassCorrection <- function(x, method = 'trellis'){
    .Deprecated("plotLockMassCorrection")
    plotLockMassCorrection(x, method)
}

#' @export
PlotPrecursorHeatmap <- function(x, method = 'overlay', bins = 80){
    .Deprecated("plotPrecursorHeatmap")
    plotPrecursorHeatmap(x, method, bins)
} 

#' @export
PlotTicBasepeak <- function(x, method = 'trellis'){
    .Deprecated("plotTicBasepeak")
    plotTicBasepeak(x, method)
}


#' @export
PlotCycleTime <- function(x, method = 'trellis'){
    .Deprecated("plotCycleTime")
    plotCycleTime(x, method)
}

#' @export
PlotInjectionTime <- function(x, method = 'trellis'){
    .Deprecated("plotInjectionTime")
    plotInjectionTime(x, method)
}



#' @export
PlotMzDistribution <- function(x, method = 'trellis'){
    .Deprecated("plotMzDistribution")
    plotMzDistribution(x, method)
}

#' @export
PlotMassDistribution <- function(x, method = 'trellis'){
    .Deprecated("plotMassDistribution")
    plotMassDistribution(x, method)
}


#' @export
PlotChargeState <- function(x, method = 'trellis'){
  .Deprecated("plotChargeState")
  plotChargeState(x, method)
}

#' @export
PlotScanTime <- function(x, method = 'trellis'){
  .Deprecated("plotScanTime")
  plotScanTime(x, method)
}


#' @inherit plotLockMassCorrection params return references author title
#' @export
PlotLockMassCorrection <- function(x, method = 'trellis'){
	.Deprecated("plotLockMassCorrection")
	plotLockMassCorrection(x, method)
}


#' @inherit plotPrecursorHeatmap params return references author title
#' @export
PlotPrecursorHeatmap <- function(x, method = 'overlay', bins = 80){
	.Deprecated("plotPrecursorHeatmap")
	plotPrecursorHeatmap(x, method, bins)
}	

#' @inherit plotTicBasepeak params return references author title
#' @export
PlotTicBasepeak <- function(x, method = 'trellis'){
	.Deprecated("plotTicBasepeak")
	plotTicBasepeak(x, method)
}


#' @inherit plotCycleTime params return references author title
#' @export
PlotCycleTime <- function(x, method = 'trellis'){
	.Deprecated("plotCycleTime")
	plotCycleTime(x, method)
}

#' @inherit plotInjectionTime params return references author title
#' @export
PlotInjectionTime <- function(x, method = 'trellis'){
	.Deprecated("plotInjectionTime")
	plotInjectionTime(x, method)
}



#' @inherit plotMassDistribution params return references author title
#' @export
PlotMzDistribution <- function(x, method = 'trellis'){ 
    .Deprecated("plotMzDistribution")
    plotMzDistribution(x, method)
}

#' @inherit plotMassDistribution params return references author title
#' @export
PlotMassDistribution <- function(x, method = 'trellis'){ 
	.Deprecated("plotMassDistribution")
	plotMassDistribution(x, method)
}


#' @inherit plotChargeState params return references author title
#' @export
PlotChargeState <- function(x, method = 'trellis'){ 
  .Deprecated("plotChargeState")
  plotChargeState(x, method)
}

#' @inherit plotChargeState params return references author title
#' @export
PlotScanTime <- function(x, method = 'trellis'){ 
  .Deprecated("plotScanTime")
  plotScanTime(x, method)
}
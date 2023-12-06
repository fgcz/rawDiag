#R

#' Reads selected raw file trailer for rawDiag plot functions
#' 
#' @inheritParams rawrr::readIndex
#' @param msgFUN this function is used for logging information while composing
#' the resulting data.frame. It can also be used for shiny progress bar. The 
#' default is using the \code{message}.
#' @return a \code{data.frame} containing the selected trailer information.
#' @author Christian Panse (2016-2023)
#' @export 
#' @examples
#' rawrr::sampleFilePath() |> rawDiag::read.raw()
#' @importFrom rawrr readIndex readTrailer readChromatogram
read.raw <- function(rawfile, msgFUN = function(x){message(x)}){
  message("reading index for ", basename(rawfile), "...")
  
  rawfile |> 
    rawrr::readIndex() -> rawrrIndex
  rawrrIndex$rawfile <- basename(rawfile)
  
  rawfile |>
    rawrr::readTrailer() -> trailerNames
  
  msgFUN("determining ElapsedScanTimesec ...")
  rawrrIndex$ElapsedScanTimesec <- c(diff(rawrrIndex$StartTime), NA)
  
  if ("LM m/z-Correction (ppm):" %in% trailerNames){
    msgFUN("reading trailer LM m/z-Correction (ppm) ...")
    rawfile |> 
      rawrr::readTrailer("LM m/z-Correction (ppm):") |> 
      as.numeric() -> LMCorrection
    rawrrIndex$LMCorrection <- LMCorrection
  }
  
  if ("AGC:" %in% trailerNames){
    msgFUN("reading trailer AGC ...")
    rawfile |> 
      rawrr::readTrailer("AGC:") -> AGC
    rawrrIndex$AGC <- AGC
  }
  
  if ("AGC PS Mode:" %in% trailerNames){
    msgFUN("reading trailer AGC PS Mode ...")
    rawfile |> 
      rawrr::readTrailer("AGC PS Mode:") -> PrescanMode
    rawrrIndex$PrescanMode <- PrescanMode
  }
  
  if ("FT Resolution:" %in% trailerNames){
    msgFUN("reading trailer FT Resolution ...")
    rawfile |> 
      rawrr::readTrailer("FT Resolution:") |>
      as.numeric() -> FTResolution
    rawrrIndex$FTResolution <- FTResolution
  }
  
  if ("Ion Injection Time (ms):" %in% trailerNames){
    msgFUN("reading trailer Ion Injection Time (ms) ...")
    rawfile |> 
      rawrr::readTrailer("Ion Injection Time (ms):") |>
      as.numeric() -> IonInjectionTime
    rawrrIndex$IonInjectionTime <- IonInjectionTime
  }
  
  msgFUN("reading TIC ...")
  rawrrIndex$TIC <- NA
  rawfile |> rawrr::readChromatogram(type = 'tic') -> tic
  rawrrIndex$TIC[rawrrIndex$MSOrder == "Ms"] <- tic$intensities
  
  msgFUN("reading BasePeakIntensity ...")
  rawrrIndex$BasePeakIntensity <- NA
  rawfile |> rawrr::readChromatogram(type = 'bpc') -> bpc
  rawrrIndex$BasePeakIntensity[rawrrIndex$MSOrder == "Ms"] <- bpc$intensities
  
  rawrrIndex |> validate_read.raw() 
}


.rawDiagColumns <- function(){
    c("scan", "scanType", "StartTime", "precursorMass",
      "MSOrder", "charge", "masterScan", "dependencyType", 
      "TIC", "BasePeakIntensity", "ElapsedScanTimesec", "rawfile", "AGC",
      "LMCorrection", "PrescanMode", "FTResolution") |>
        sort()
}

#' Is an Object an rawDiag Object?
#'
#' @inheritParams methods::is
#' @return a boolean
#' @author Christian Panse 2018
#' @examples
#' rawrr::sampleFilePath() |> rawDiag::read.raw() |> rawDiag::is.rawDiag()
#' 
#' @export 
is.rawDiag <- function(object){
    cn <- .rawDiagColumns()
    
    msg <- cn[! cn %in% colnames(object)]
    if (length(msg) > 0){
        message(paste("missing column name(s):", paste(msg, collapse = ", ")))
        return(FALSE)
    }

    return(TRUE)
}

validate_read.raw <- function(x){
  validateIndex <- TRUE
  
  if (!is.data.frame(x)){
    message("Object is not a 'data.frame'.")
    valideIndex <- FALSE
  }
  
  IndexColNames <- .rawDiagColumns()
  
  for (i in IndexColNames){
    if (!(i %in% colnames(x))){
      msg <- sprintf("Missing column %s.", i)
      warning(msg)
      valideIndex <- FALSE
    }
  }
  
  # stopifnot(valideIndex)
  return(x)
}

#' Lock Mass Correction Plot
#' 
#' @param x a \code{data.frame} object adhering to the specified criteria for the \code{is.rawDiag} function.
#' @param method specifying the plot method 'trellis' | 'violin' | 'overlay'. The default is 'trellis'.
#' @return a \code{ggplot2} object.
#' @author Christian Trachsel (2017), Christian Panse (2023)
#' @references rawDiag \doi{10.1021/acs.jproteome.8b00173}
#' @examples 
#' rawrr::sampleFilePath() |>
#'   read.raw() |>
#'   plotLockMassCorrection()
#' @importFrom ggplot2 ggplot aes_string geom_hline geom_line labs scale_x_continuous facet_wrap theme_light
#' @export
plotLockMassCorrection <- function(x, method = 'trellis'){
  stopifnot("LMCorrection" %in% colnames(x))

  x |>
    base::subset(x['MSOrder'] == "Ms") -> x
  
  if (method %in% c('trellis')){
    x |>
      ggplot2::ggplot(ggplot2::aes(x = .data$StartTime , y = .data$LMCorrection)) +
      ggplot2::geom_hline(yintercept = c(-5, 5), colour = "red3", linetype = "longdash") +
      ggplot2::geom_line(linewidth = 0.3) +
      ggplot2::geom_line(stat = "smooth",
                         method= "gam",
                         formula = y ~ s(x, bs ="cs"),
                         colour = "deepskyblue3", se = FALSE) -> gp
  }else if(method %in% c('overlay')){
    x |>
      ggplot2::ggplot(ggplot2::aes_string(x = .data$StartTime , y = .data$LMCorrection, colour = .data$rawfile)) +
      ggplot2::geom_hline(yintercept = c(-5, 5), colour = "red3", linetype = "longdash") +
      ggplot2::geom_line(linewidth = 0.3) +
      ggplot2::geom_line(stat = "smooth",
                         method= "gam",
                         formula = y ~ s(x, bs ="cs"),
                         colour = "deepskyblue3", se = FALSE) -> gp
  }else{
    x |>
      ggplot2::ggplot(ggplot2::aes_string(x = "StartTime" , y = "LMCorrection")) +
      ggplot2::geom_hline(yintercept = c(-5, 5), colour = "red3", linetype = "longdash") + 
      ggplot2::geom_violin() -> gp
  }
  gp +
    ggplot2::labs(title = "Lock mass correction plot") +
    ggplot2::labs(subtitle = "Plotting lock mass correction value versus retention time") +
    ggplot2::labs(x = "Retention Time [min]", y = "Lock Mass Correction [ppm]") +
    ggplot2::scale_x_continuous(breaks = base::pretty(8)) +
    # scale_y_continuous(breaks = scales::pretty_breaks(8), limits = c(-10, 10)) +
    ggplot2::facet_wrap(~ rawfile) +
    ggplot2::theme_light() -> gp
  
  gp
}

#' Precursor Mass versus StartTime MS2 based hexagons
#' 
#' @inheritParams plotLockMassCorrection
#' @param bins number of bins in both vertical and horizontal directions. default is 80.
#' @return a ggplot2 object.
#' @author Christian Trachsel (2017)
#' @export
#' @note TODO: define bin with dynamically as h= 2x IQR x n e-1/3 or number of bins (max-min)/h
#' @importFrom ggplot2 ggplot aes_string geom_hex labs scale_fill_gradientn theme_light
#' @importFrom grDevices colorRampPalette
#' @examples 
#' rawrr::sampleFilePath() |> read.raw() |> plotPrecursorHeatmap()
plotPrecursorHeatmap <- function(x, method = 'overlay', bins = 80){
  spectral <- c("#5E4FA2",
                "#3288BD",
                "#66C2A5",
                "#ABDDA4",
                "#E6F598",
                "#FFFFBF",
                "#FEE08B",
                "#FDAE61",
                "#F46D43",
                "#D53E4F",
                "#9E0142")
  
  spectralramp <- colorRampPalette(spectral)
  
  x |>
    base::subset(x['MSOrder'] == "Ms2") |>
    ggplot2::ggplot(ggplot2::aes_string(x = 'StartTime', y = 'precursorMass')) + 
    ggplot2::labs(x = "Retention Time [min]", y = "Precursor Mass [Da]") +
    ggplot2::geom_hex(bins = bins) + 
    ggplot2::scale_fill_gradientn(colours = spectralramp(32)) + 
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(8)) + 
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(15)) + 
    ggplot2::theme_light() -> gp
  
  if (method == 'trellis'){
    gp +
      ggplot2::facet_wrap(~ rawfile) -> gp
  }
  
  gp +
    ggplot2::labs(title = "precursorMass versus StartTime hexagons MS2") 
}

#' Total Ion Count and Base Peak Plot
#' 
#' @description Function for displaying the Total Ion Cound (TIC) and Base
#' Peak chromatogram of a mass spectrometry measurement. 
#' Multiple files are handled by faceting based on rawfile name.
#'
#' @inheritParams plotLockMassCorrection
#' @return a ggplot2 object for graphing the TIC and the Base Peak chromatogram.
#' @export
#' @author Christian Trachsel (2017), Christian Panse (20231130) refactored
#' @importFrom ggplot2 ggplot aes_string geom_line labs scale_x_continuous facet_wrap theme_light
#' @importFrom reshape2 melt
#' @examples
#' rawrr::sampleFilePath() |> read.raw() |> plotTicBasepeak()
plotTicBasepeak <- function(x, method = 'trellis'){
  stopifnot(method %in% c('trellis', 'violin', 'overlay'))
  
  x[, c('StartTime', 'TIC', 'BasePeakIntensity', 'rawfile')] |>
    base::subset(x$MSOrder == "Ms") |>
    reshape2::melt(id.vars = c("StartTime", "rawfile")) -> xx

  if (method == 'trellis'){
    xx |>
      ggplot2::ggplot(ggplot2::aes_string(x = "StartTime", y = "value")) +
      ggplot2::geom_line(linewidth = 0.3) +
      ggplot2::facet_wrap(rawfile ~ variable, scales = "free", ncol = 2) +
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(8)) +
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(8)) -> gp
  }else if(method =='violin'){
    xx |>
      ggplot2::ggplot(ggplot2::aes(x = "rawfile", y = "value")) + 
      ggplot2::geom_violin() +
      ggplot2::facet_grid(variable ~ ., scales = "free") +
      #ggplot2::scale_y_continuous(trans = scales::log10_trans()) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) -> gp
  }else if (method  == 'overlay'){
    xx |>
      ggplot2::ggplot(ggplot2::aes_string(x = "StartTime", y = "value", colour = "rawfile")) +
      ggplot2::geom_line(size = 0.3) +
      ggplot2::facet_grid(variable ~ ., scales = "free") +
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(8)) +
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(8)) +
      ggplot2::theme(legend.position="top") -> gp
  }else{NULL}
  
  gp +
    ggplot2::labs(title = "Total Ion Count and Base-Peak plot") +
    ggplot2::labs(subtitle = "Plotting the TIC and base peak density for all mass spectrometry runs") +
    ggplot2::labs(x = "Retention Time [min]", y = "Intensity Counts [arb. unit]") +
    ggplot2::theme_light() 
}


#' Calculate MS Cycle Time
#' @description Graphs the lock mass deviations along RT.
#' @inheritParams plotLockMassCorrection
#' @details TODO: quan
#' tile part needed? If no MS1 scan is present? -> DIA take lowest window as cycle indicator?
#'
#' @importFrom stats na.omit
#' @author Christian Trachsel (2017), Christian Panse (20231201) refactored
#' @return calculates the time of all ms cycles and the 95% quantile value there of. 
#' the cycle time is defined as the time between two consecutive MS1 scans
.cycleTime <- function(x){
  x |>
    subset(x$MSOrder == "Ms") -> xx
  split(xx, xx$rawfile) |>
    lapply(function(o){
      o$CycleTime <- c(NA, (o$StartTime * 60) |> diff())
      o$quan <- quantile(o$CycleTime, probs = 0.95, na.rm = TRUE)
      o[, c('StartTime', 'CycleTime',  'quan', 'rawfile')]
    }) |>
    Reduce(f = rbind) |> 
    na.omit()
} 

#' Plot Cycle Time
#' 
#' @inheritParams plotLockMassCorrection
#' 
#' @description  Graphs the time difference between two consecutive MS1 scans
#' (cycle time) with respect to RT (scatter plots) or its density (violin).
#' A smooth curve graphs the trend. The 95th percentile is indicated by a red
#' dashed line.
#' 
#' @return a \code{\link{ggplot2}} object.
#' @importFrom ggplot2 ggplot aes geom_point geom_line scale_x_continuous scale_y_continuous geom_hline theme_light
#' @importFrom scales pretty_breaks
#' @importFrom dplyr left_join
#' @importFrom stats quantile na.omit
#' @importFrom rlang .data
#' @export 
#' @examples
#' rawrr::sampleFilePath() |> read.raw() |> plotCycleTime()
plotCycleTime <- function(x, method = 'trellis'){
  xx <- .cycleTime(x)
  
  if (method == 'trellis'){
    xx |> 
      ggplot2::ggplot(ggplot2::aes(x = .data$StartTime, y = .data$CycleTime)) + 
      ggplot2::geom_point(shape = ".") +
      ggplot2::geom_line(stat = "smooth", method = "gam", formula = y ~ s(x, bs= "cs"),
                         colour = "deepskyblue3", se = FALSE) +
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(8)) +
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(8)) +
      ggplot2::geom_hline(ggplot2::aes(yintercept = .data$quan, group = .data$rawfile),
                          colour = "red3", linetype = "longdash") +
      ggplot2::facet_grid(rawfile ~ ., scales = "free") +
      ggplot2::labs(subtitle = "Plotting the caclulated cycle time of each cycle vs retention time") + 
      ggplot2::labs(x = "Retention Time [min]", y = "Cycle Time [sec]") -> gp
  }else if (method == 'violin'){
    xx |>
      ggplot2::ggplot(ggplot2::aes(x = .data$rawfile, y = .data$CycleTime)) + 
      ggplot2::geom_violin()  +
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(8)) +
      ggplot2::labs(subtitle = "Plotting the cycle time density of all mass spectrometry runs") +
      ggplot2::labs(x = "rawfile", y = "Cycle Time [sec]") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) -> gp
  } else if(method == 'overlay'){
    xx|>
      ggplot2::ggplot(ggplot2::aes(x = .data$StartTime, y = .data$CycleTime, colour = .data$rawfile)) + 
      ggplot2::geom_point(size = 0.5) +
      ggplot2::geom_line(ggplot2::aes(group = .data$rawfile,
                                             colour = .data$rawfile),
                         stat = "smooth", method = "gam",
                         formula = y ~ s(x, bs= "cs"), se = FALSE) +
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(8)) +
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(8)) +
      ggplot2::labs(subtitle = "Plotting the caclulated cycle time of each cycle vs retention time") +
      ggplot2::labs(x = "Retention Time [min]", y = "Cycle Time [sec]") +
      ggplot2::theme(legend.position="top") -> gp
    
  }else{NULL}
  gp + 
    ggplot2::labs(title = "Cycle time plot") +
    ggplot2::labs(x = "Retention Time [min]", y = "Cycle Time [sec]") +
    ggplot2::theme_light() 
}



#' Plot Injection Time
#'
#' @inheritParams plotLockMassCorrection
#' 
#' @return a \code{\link{ggplot2}} object.
#' @export
#' @importFrom ggplot2 ggplot aes geom_point geom_line scale_x_continuous scale_y_continuous geom_hline theme_light
#' @importFrom rlang .data
#' @examples
#' rawrr::sampleFilePath() |> read.raw() |> plotInjectionTime()
plotInjectionTime <- function(x, method = 'trellis'){
  if (method == 'trellis'){
    maxtimes <- x |>
      dplyr::group_by(.data$rawfile, .data$MSOrder) |>
      dplyr::summarise(maxima = max(.data$IonInjectionTime))
    
    ggplot2::ggplot(x, ggplot2::aes(x = .data$StartTime, y = .data$IonInjectionTime)) +
      ggplot2::geom_hline(data = maxtimes, ggplot2::aes(yintercept = .data$maxima),
                          colour = "red3", linetype = "longdash") +
      ggplot2::geom_point(shape = ".") +
      ggplot2::geom_line(stat = "smooth", method = "gam", formula = y ~ s(x, bs= "cs"),
                         colour = "deepskyblue3", se = FALSE) +
      ggplot2::facet_grid(rawfile ~ MSOrder, scales = "free") +
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks((n = 8))) +
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks((n = 8))) +
      ggplot2::labs(subtitle = "Plotting injection time against retention time for MS and MSn level") +
      ggplot2::labs(x = "Retentione Time [min]", y = "Injection Time [ms]") -> gp
  }else if (method == 'violin'){
    ggplot2::ggplot(x, ggplot2::aes(x = .data$rawfile, y = .data$IonInjectionTime)) +
      ggplot2::geom_violin() +
      ggplot2::facet_grid(MSOrder ~ .) +
      ggplot2::labs(subtitle = "Plotting retention time resolved injection time density for each mass spectrometry run") +
      ggplot2::labs(x = "rawfile", y = "Injection Time [ms]") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) -> gp
  }else if(method == 'overlay'){
    ggplot2::ggplot(x, ggplot2::aes(x = .data$StartTime, y = .data$IonInjectionTime, colour = .data$rawfile)) +
      ggplot2::geom_point(size = 0.5, alpha = 0.1) +
      ggplot2::geom_line(ggplot2::aes(group = .data$rawfile, colour = .data$rawfile),
                stat = "smooth",
                method = "gam",
                formula = y ~ s(x, bs= "cs"),
                se = FALSE) +
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks((n = 8))) +
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks((n = 8))) +
      ggplot2::facet_grid(~ .data$MSOrder, scales = "free") +
      ggplot2::labs(subtitle = "Plotting injection time against retention time for MS and MSn level") +
      ggplot2::labs(x = "Retentione Time [min]", y = "Injection Time [ms]") +
      ggplot2::theme(legend.position = "top") -> gp
  }else{NULL}
  
  gp + ggplot2::labs(title = "Injection time plot") +
    ggplot2::theme_light()
}

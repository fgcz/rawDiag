#R
# contains selected rawDiag plot methods
# 

#' reads selected raw file trailer for rawDiag plot functions
#' 
#' @param rawfile the name of the raw file containing the mass
#' spectrometry data from the Thermo Fisher Scientific instrument.
#' @author Christian Panse (2016-2023)
#' @export 
#' @importFrom rawrr readIndex readTrailer
read.raw <- function(rawfile){
  message("reading index for ", basename(rawfile), "...")
  
  rawfile |> 
    rawrr::readIndex() -> rawrrIndex
  rawrrIndex$rawfile <- basename(rawfile)
  
  rawfile |>
    rawrr::readTrailer() -> trailerNames
  
  message("determining ElapsedScanTimesec ...")
  rawrrIndex$ElapsedScanTimesec <- c(diff(rawrrIndex$StartTime), NA)
  
  if ("LM m/z-Correction (ppm):" %in% trailerNames){
    message("reading trailer LM m/z-Correction (ppm) ...")
    rawfile |> 
      rawrr::readTrailer("LM m/z-Correction (ppm):") |> 
      as.numeric() -> LMCorrection
    rawrrIndex$LMCorrection <- LMCorrection
  }
  
  if ("AGC:" %in% trailerNames){
    message("reading trailer AGC ...")
    rawfile |> 
      rawrr::readTrailer("AGC:") -> AGC
    rawrrIndex$AGC <- AGC
  }
  
  if ("AGC PS Mode:" %in% trailerNames){
    message("reading trailer AGC PS Mode ...")
    rawfile |> 
      rawrr::readTrailer("AGC PS Mode:") -> PrescanMode
    rawrrIndex$PrescanMode <- PrescanMode
  }
  
  if ("FT Resolution:" %in% trailerNames){
    message("reading trailer FT Resolution ...")
    rawfile |> 
      rawrr::readTrailer("FT Resolution:") |>
      as.numeric() -> FTResolution
    rawrrIndex$FTResolution <- FTResolution
  }
  
  message("reading TIC ...")
  rawrrIndex$TIC <- NA
  rawfile |> rawrr::readChromatogram(type = 'tic') -> tic
  rawrrIndex$TIC[rawrrIndex$MSOrder == "Ms"] <- tic$intensities
  
  message("reading BasePeakIntensity ...")
  rawrrIndex$BasePeakIntensity <- NA
  rawfile |> rawrr::readChromatogram(type = 'bpc') -> bpc
  rawrrIndex$BasePeakIntensity[rawrrIndex$MSOrder == "Ms"] <- bpc$intensities
  
  rawrrIndex
}

#' lock mass correction plot
#' 
#' @param x a \code{\link{data.frame}} fullfilling the \code{is.rawDiag} column naming criteria.
#' @param method a character 'trellis', 'violin' or 'overlay'.
#' @return a ggplot2 object
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
      ggplot2::ggplot(ggplot2::aes_string(x = "StartTime" , y = "LMCorrection")) +
      ggplot2::geom_hline(yintercept = c(-5, 5), colour = "red3", linetype = "longdash") +
      ggplot2::geom_line(linewidth = 0.3) +
      ggplot2::geom_line(stat = "smooth",
                         method= "gam",
                         formula = y ~ s(x, bs ="cs"),
                         colour = "deepskyblue3", se = FALSE) -> gp
  }else if(method %in% c('overlay')){
    x |>
      ggplot2::ggplot(ggplot2::aes_string(x = "StartTime" , y = "LMCorrection", colour = "rawfile")) +
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

#' precursorMass versus StartTime hexagons MS2
#' 
#' @inheritParams plotLockMassCorrection
#' @param bins number of bins in both vertical and horizontal directions. default is 80.
#' @return a ggplot2 object.
#' @author Christian Trachsel (2017)
#' @export
#' @note TODO: define bin with dynamically as h= 2x IQR x n e-1/3 or number of bins (max-min)/h
#' @importFrom ggplot2 ggplot aes_string geom_hex labs scale_fill_gradientn theme_light
#' @importFrom grDevices colorRampPalette
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

#' TIC and Base Peak plot function
#' 
#' @description Function for displaying the Total Ion Cound (TIC) and Base
#' Peak chromatogram of a mass spectrometry measurement. 
#' Multiple files are handled by faceting based on rawfile name.
#'
#' @inheritParams plotLockMassCorrection
#' @return a ggplot object for graphing the TIC and the Base Peak chromatogram
#' @export
#' @author Christian Trachsel (2017), Christian Panse (20231130) refactored
#' @importFrom ggplot2 ggplot aes_string geom_line labs scale_x_continuous facet_wrap theme_light
#' @importFrom reshape2 melt
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
#'
#' @inheritParams plotLockMassCorrection
#' @details TODO: qunatile part needed? If no MS1 scan is present? -> DIA take lowest window as cycle indicator?
#'
#' @importFrom dplyr filter_at select_at group_by_at mutate_at summarise_at any_vars vars ungroup
#' @importFrom stats na.omit
#' @author Christian Trachsel (2017), Christian Panse (20231201) refactored
#' @return calculates the time of all ms cycles and the 95% quantile value there of. 
#' the cycle time is defined as the time between two consecutive MS1 scans
.calcCycleTime <- function(x){
  
  x |>
    dplyr::filter_at(dplyr::vars("MSOrder"), dplyr::any_vars(. == "Ms")) |>
    dplyr::select_at(dplyr::vars("StartTime", "rawfile")) |>
    dplyr::group_by_at(dplyr::vars("rawfile")) |>
    dplyr::mutate_at(dplyr::vars("StartTime"), list("CycleTime" = ~ (. - lag(.)) * 60)) |>
    stats::na.omit() -> xx
  
  xx |> 
    group_by_at("rawfile") |>
    dplyr::summarise_at(dplyr::vars("CycleTime"), list("quan" = ~ quantile(., probs = 0.95))) -> xxx
  

  dplyr::left_join(xx, xxx, by = "rawfile") |>
    dplyr::ungroup() 
}

#' cycle time plot
#' 
#' @inheritParams plotLockMassCorrection
#' 
#' @description graphs cycle time versus rt.
#' each item represents the time for one scan cycle.
#' @return a \code{\link{ggplot2}} object.
#' @importFrom ggplot2 ggplot aes_string geom_point geom_line scale_x_continuous scale_y_continuous geom_hline theme_light
#' @importFrom scales pretty_breaks
#' @importFrom dplyr left_join
#' @importFrom stats quantile na.omit
#' @export 
plotCycleTime <- function(x, method = 'trellis'){
  xx <- .calcCycleTime(x)
  
  if (method == 'trellis'){
    xx |>
      ggplot2::ggplot(ggplot2::aes_string(x = "StartTime", y = "CycleTime")) + 
      ggplot2::geom_point(shape = ".") +
      ggplot2::geom_line(stat = "smooth", method = "gam", formula = y ~ s(x, bs= "cs"), colour = "deepskyblue3", se = FALSE) +
      ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(8)) +
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(8)) +
      ggplot2::geom_hline(ggplot2::aes_string(yintercept = "quan", group = "rawfile"), colour = "red3", linetype = "longdash") +
      ggplot2::facet_grid(rawfile ~ ., scales = "free") +
      ggplot2::labs(subtitle = "Plotting the caclulated cycle time of each cycle vs retention time") + 
      ggplot2::labs(x = "Retention Time [min]", y = "Cycle Time [sec]") -> gp
  }else if (method == 'violin'){
    #xx |>
      #dplyr::select_at(dplyr::vars("rawfile", "quan")) |>
      #distinct() -> dots
    xx |>
      ggplot2::ggplot(ggplot2::aes_string(x = "rawfile", y = "CycleTime")) + 
      ggplot2::geom_violin()  +
      ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(8)) +
      ggplot2::labs(subtitle = "Plotting the cycle time density of all mass spectrometry runs") +
      ggplot2::labs(x = "rawfile", y = "Cycle Time [sec]") +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90)) -> gp
  } else if(method == 'overlay'){
    xx|>
      ggplot2::ggplot(ggplot2::aes_string(x = "StartTime", y = "CycleTime", colour = "rawfile")) + 
      ggplot2::geom_point(size = 0.5) +
      ggplot2::geom_line(ggplot2::aes_string(group = "rawfile",
                                             colour = "rawfile"),
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

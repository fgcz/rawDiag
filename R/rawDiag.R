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
  
  rawrrIndex$ElapsedScanTimesec <- c(diff(rawrrIndex$StartTime), NA)
  
  if ("LM m/z-Correction (ppm):" %in% trailerNames){
    message("reading LM m/z-Correction (ppm) ...")
    rawfile |> 
      rawrr::readTrailer("LM m/z-Correction (ppm):") |> 
      as.numeric() -> LMCorrection
    rawrrIndex$LMCorrection <- LMCorrection
  }
  
  if ("AGC:" %in% trailerNames){
    message("reading AGC ...")
    rawfile |> 
      rawrr::readTrailer("AGC:") -> AGC
    rawrrIndex$AGC <- AGC
  }
  
  if ("AGC PS Mode:" %in% trailerNames){
    message("reading PrescanMode ...")
    rawfile |> 
      rawrr::readTrailer("AGC PS Mode:") -> PrescanMode
    rawrrIndex$PrescanMode <- PrescanMode
  }
  
  if ("FT Resolution:" %in% trailerNames){
    message("reading FTResolution ...")
    rawfile |> 
      rawrr::readTrailer("FT Resolution:") |>
      as.numeric() -> FTResolution
    rawrrIndex$FTResolution <- FTResolution
  }
  
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
      ggplot2::geom_line(size = 0.3) +
      ggplot2::geom_line(stat = "smooth",
                         method= "gam",
                         formula = y ~ s(x, bs ="cs"),
                         colour = "deepskyblue3", se = FALSE) -> gp
  }else if(method %in% c('overlay')){
    x |>
      ggplot2::ggplot(ggplot2::aes_string(x = "StartTime" , y = "LMCorrection", colour = "rawfile")) +
      ggplot2::geom_hline(yintercept = c(-5, 5), colour = "red3", linetype = "longdash") +
      ggplot2::geom_line(size = 0.3) +
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

#' PrecursorMass versus StartTime hexagons MS2
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
    ggplot2::theme_light()
  
  if (method == 'trellis'){
    gp <- gp + ggplot2::facet_wrap(~ rawfile) 
  }
  
  gp
}


#R visualiz Thermo raw file data with R @Functional Genomics Center Zurich 

# Hack to handle 
# no visible binding for global variable '.'
. = NULL

# AUTHORS: Christian Trachsel, Christian Panse, Witold Wolski 2017

#---- Data staging----

#define colours


.darkTheme <- function(){
  theme(legend.position = 'none') +
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none",
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank()) +
    theme(plot.title = element_blank()) +
    theme(plot.subtitle = element_blank()) +
    theme(strip.background = element_blank()) +
    theme(strip.text = element_blank()) +
    theme(plot.background = element_rect(fill = "black")) +
    theme(panel.spacing = unit(-1, "lines"))
}

.getDesiredColNames <- function(){
  c("filename",
    "scanNumber",
    "StartTime",
    "BasePeakMass",
    "BasePeakIntensity",
    "TIC",
    "ScanType",
    "CycleNumber",
    "MSOrder",
    "MassAnalyzer",
    "PrecursorMass",
    "ChargeState",
    "IonInjectionTimems",
    "FTResolution",
    "MasterScanNumber",
    "LMCorrection", #"LMCorrectionppm", "LMmZCorrectionppm",
    "ElapsedScanTimesec",
    "transient",
    "AGC",
    "AGCMode",
    "PrescanMode")
}

#data loading and integrity check

#' Is an Object from a rawDiag class?
#'
#' @param object any R object.
#'
#' @return a boolean
#' @importFrom stats na.omit quantile
#' @export is.rawDiag
is.rawDiag <- function(object){
  cn <- .getDesiredColNames()
 
 msg <- cn[! cn %in% colnames(object)]
 if (length(msg) > 0){
   warning(paste("missing column name(s):", paste(msg, collapse = ", ")))
   return(FALSE)
 }
 
 return(TRUE)
}

#' Force an Object to Belong to a Class \code{rawDiag}
#'
#' @param object any R object.
#'
#' @return a \code{data.frame} having the column names defined in 
#' \code{rawDiag:::.getDesiredColNames}.
#' 
#' @export as.rawDiag
as.rawDiag <- function(object){
  if (! is.rawDiag(object)){
    if ("MasterScanNumber" %in% colnames(object)){
      message("NA values replaced in MasterScanNumber ")
      object <- dplyr::mutate(object, MasterScanNumber = replace(MasterScanNumber, MasterScanNumber == 0, NA))
    } else if (!"MasterScanNumber" %in% colnames(object)){
      message("MasterScanNumber calculated")
      object$MasterScanNumber <- .CalculatioMasterScan(object)
    }
    
    if (!"ElapsedScanTimesec" %in% colnames(object)){
      object <- object %>% 
        dplyr::mutate(ElapsedScanTimesec = (lead(object$StartTime) - object$StartTime) * 60) 
    }

    if ("LMCorrectionppm" %in% colnames(object)){
      message("renamed LMCorrectionppm to LMCorrection")
      object <- dplyr::rename(object, LMCorrection = LMCorrectionppm)
    } else if ("LMmZCorrectionppm" %in% colnames(object)){
      message("renamed LMmZCorrectionppm to LMCorrection")
      object <- dplyr::rename(object, LMCorrection = LMmZCorrectionppm)
    }
    
    if ("AGC" %in% colnames(object) & !"AGCPSMode" %in% colnames(object)){
      message("copied AGC to AGCMode")
      object$AGCMode <- object$AGC
    } else {
      object$AGCMode <- NA
    }

    if ("AGCPSMode" %in% colnames(object)){
      message("renamed AGCPSMode to PrescanMode")
      object <- dplyr::rename_at(object, vars("AGCPSMode"), funs(paste("PrescanMode")))
      object$PrescanMode[1] <- 1
    } else {
      message("calculated PrescanMode values")
      object$PrescanMode <- .CalculatePrescan(object)

    }
    
    # TODO(cp,ct): maybe NULL because of ggplot  
    if (! "LMCorrection" %in% colnames(object)){
        object$LMCorrection <- NA
        warning("Lock mass correction values not found in raw file!!")
    }
    
    if ("OrbitrapResolution" %in% colnames(object)){
      object <- dplyr::rename_at(object, vars("OrbitrapResolution"), funs(paste("FTResolution")))
    }
  }


  for (cn in .getDesiredColNames()){
    if (!cn %in% colnames(object)){
      object[cn] <- NA
    }
  }
  

  object <- .calc.transient(object)
  object[, colnames(object) %in% .getDesiredColNames()]
}

#' mzR reader method
#'
#' @param object an mzR object
#'
#' @return an \code{\link{rawDiag}} object
#' @author Christian Panse <cp@fgcz.ethz.ch>, Witold E.Wolski <wew@fgcz.ethz.ch>
#' @export as.rawDiag.mzR
#' @example 
#' 
#' 
#' \dontrun{
#' library(mzR); 
#' library(rawDiag)
#' mzML <- "20180220_04_S174020_Pierce_HeLa_Protein_Digest_Std_5000_5010.mzML"
#' mzML <- file.path(path.package(package = "rawDiag"), "extdata", mzML)
#' RAW <- rawDiag:::as.rawDiag.mzR(openMSfile(mzML))
#' }
as.rawDiag.mzR <- function(object){
  #time.start <- Sys.time()
  runInfo <- runInfo(object)
  rv <- lapply(1:runInfo$scanCount, 
               function(x){data.frame(header(object, x))})
  rv <- bind_rows(rv)
  
  #time.end <- Sys.time()
  #message(time.start - time.end)
  
  rv <- rv[, c('acquisitionNum', 'retentionTime', 'basePeakMZ',
               'basePeakIntensity', 'totIonCurrent', 'msLevel', 'precursorMZ',
               'precursorCharge')]
  
  
  colnames(rv) <- c('scanNumber', 'StartTime', 'BasePeakMass',
                    'BasePeakIntensity', 'TIC', 'MSOrder', 'PrecursorMass',
                    'ChargeState')
  
  rv$MSOrder[rv$MSOrder == 1] <- "Ms" 
  rv$MSOrder[rv$MSOrder == 2] <- "Ms2" 
  rv$StartTime <- rv$StartTime / 60
  
  rv$filename <- basename(fileName(object))
  
  as.rawDiag(rv)
}

.read.thermo.raw.ssh <- function(file, mono = TRUE, hostname = "fgcz-r-021.uzh.ch",
                                 exec = "mono ~cpanse/bin/fgcz_raw.exe",
                                 user='cpanse', argv="qc"){
  
  cmd <- paste(exec, file, argv)
  
  ssh_cmd <- paste("ssh ", user, "@", hostname, " '", cmd, "'", sep="")
  
  message(ssh_cmd)
  
  as.rawDiag(read.csv(pipe(ssh_cmd), sep='\t',
                        stringsAsFactors = FALSE,
                        header = TRUE))
  
}

#' Reading Bruker tdf files
#'
#' @param filea filename if the tdf file
#'
#' @return a rawDiagobject
#' @import RSQLite
#' @export read.tdf
#' 
#' 
#' @details 
#' 
#'   Id LargestPeakMz AverageMz MonoisotopicMz Charge ScanNumber Intensity Parent
#' 1  1      827.5638  828.0039       827.5638      1   623.2419      3239      3
#' 2  2      727.6347  727.9152       727.6347      1   682.1215      2610      3
#' 
#' 
read.tdf <- function(filename){
  con <- dbConnect(RSQLite::SQLite(), filename)
  rv <- dbGetQuery(con, "SELECT * FROM Precursors a INNER JOIN Frames b on a.id == b.id;");
  dbDisconnect(con)
  
  
  rv <- rv[, c('Id','Time','ScanNumber','Intensity','SummedIntensities',
               'MonoisotopicMz', 'Charge', 'MsMsType')];
  colnames(rv) <- c('scanNumber','StartTime','BasePeakMass','BasePeakIntensity',
                    'totIonCurrent', 'PrecursorMass','ChargeState','MSOrder')
  rv$filename <- basename(filename)
  rv$MSOrder[rv$MSOrder == 0] <- "Ms"
  rv$MSOrder[rv$MSOrder == 8] <- "Ms2"
  as.rawDiag(rv)
}

#' mass spec reader function 
#'
#' @importFrom utils read.csv
#' @param file the name of the Thermo Fisher Scietific raw file which the data
#' are to be read from.  
#' @param mono a boolean indicates if the mono enviroment should be used.
#' @param mono_path define the PATH where the ThermoFisher.CommonCore* dll files
#' are located; default is \code{""} (empty string) ; no MONO_PATH is set.
#' @param rawDiag boolean; if TRUE (default) the output is coerced to an rawDiag object.
#' if FALSE the function will return all available columns.
#' @param exe the adapter software; default is set to
#' \code{file.path(path.package(package = "rawDiag"), "exec/fgcz_raw.exe")}
#' @param argv argument vector for the adapter.
#' @param method the adapter type. default is 'thermo' which read Thermo Fisher Scietific raw files.
#' @param ssh boolean, if ssh pipe should be used. defaul is FALSE
#'@author Christian Panse <cp@fgcz.ethz.ch>, 2017, 2018
#' @references \itemize{
#' \item  \url{http://planetorbitrap.com/rawfilereader}
#' \item \url{http://www.mono-project.com/docs/advanced/assemblies-and-the-gac/}
#' }
#' @description this function is a generic adapter function for 
#'   reading mass spectrometric measurement. It requieres the 
#'   \href{http://planetorbitrap.com/rawfilereader}{New RawFileReader} dll files
#'   \itemize{
#'   \item ThermoFisher.CommonCore.BackgroundSubtraction.dll, 
#'   \item ThermoFisher.CommonCore.MassPrecisionEstimator.dll 
#'   \item ThermoFisher.CommonCore.RawFileReader.dll
#'   }
#'   be installed on the system. This can be done by using 
#'   the Global Assembly Cache (GAC) or by setting the
#'   \code{MONO_PATH} enviroment.
#'
#'   
#' @return a \code{data.frame}.
#' 
#' @export read.raw
#'
#' @examples
#' read.raw
read.raw <- function(file, mono = if(Sys.info()['sysname'] %in% c("Darwin", "Linux")) TRUE else FALSE, 
                     exe = file.path(path.package(package = "rawDiag"),
                                     "exec/fgcz_raw.exe"),  
                     mono_path = "",
                     rawDiag = TRUE,
                     argv = "qc",
                     method = "thermo", ssh = FALSE){
  
  rv <- NULL
  if (mono_path != ''){
    print(Sys.setenv(MONO_PATH = mono_path))
  }
    
  if (method == "thermo" && ssh){
    return(.read.thermo.raw.ssh(file))
  }
  else if (method == "thermo"){
    # message(paste("start", Sys.time(), sep = ":"))
    cmd <- paste(exe, file, argv)
    
    if (mono){
      cmd <- paste("mono", cmd)
    }
    
    message(paste ("executing", cmd, "..."))
    
    if(rawDiag){
    rv <- as.rawDiag(read.csv(pipe(cmd), 
                                sep='\t', stringsAsFactors = FALSE, header = TRUE))
    }else{
      rv <- read.csv(pipe(cmd), 
                                sep='\t', stringsAsFactors = FALSE, header = TRUE)
    }
  }
  
  class(rv) <- c(class(rv), 'rawDiag')
  rv
}

#' rawDiag Summaries
#'
#' @param object  a \code{\link{data.frame}} fullfilling the \code{\link{is.rawDiag}} column naming criteria.
#'
#' @return a table.
#' @export summary.rawDiag 
#' @method rawDiag summary
summary.rawDiag <- function(object){
  table(object$filename, object$MSOrder)
}
# ----Colors----

#spectral
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
colorvector <- spectralramp(32)


# ----Computations----

#' Fill NA values with last previous value
#'
#' @param x a vector of values 
#'
#' @return a vector with any NA values replaced with the last previous actuall value
#' @export fillNAgaps
#'
#' @examples
#' v1 <- c(NA, 1, 2, 3, NA, 4, 5, NA, NA, NA, 6)
#' v2 <- fillNAgaps(v1)
fillNAgaps <- function(x) {
  goodVals <- c(NA, x[!is.na(x)])
  fillIdx <- cumsum(!is.na(x))+1
  res <- goodVals[fillIdx]
  return(res)
}

#' Calculate Master Scan Number
#'
#' @param x a \code{\link{data.frame}} fullfilling the \code{\link{is.rawDiag}} column naming criteria.
#'
#' @return calculates the MS1 master scan number of an MS2 scan and populates the MasterScanNumber
#' with it
.CalculatioMasterScan <- function(x){
  
  res <- x %>% 
    mutate(MasterScanNumber = dplyr::case_when(MSOrder == "Ms" ~ scanNumber)) %>%  
    mutate(MasterScanNumber = fillNAgaps(.$MasterScanNumber)) %>% 
    dplyr::mutate(MasterScanNumber = replace(MasterScanNumber, scanNumber == MasterScanNumber, NA)) %>% 
    dplyr::pull(MasterScanNumber)
  return(res)
}

#TODO: supply cases as list
#patterns <- list(
#x %% 35 == 0 ~ "fizz buzz",
#x %% 5 == 0 ~ "fizz",
#x %% 7 == 0 ~ "buzz",
#TRUE ~ as.character(x)
#)
#case_when(!!! patterns)
.calc.transient <- function(x){

  stopifnot(is.rawDiag(x))
  
  res <- x %>%
    dplyr::mutate(FTResolution = replace(FTResolution, 
                                         MassAnalyzer == "MassAnalyzerITMS", NA)) %>% 
    dplyr::mutate(transient = dplyr::case_when(FTResolution == 7500 ~ 16,
                                               FTResolution == 15000 ~ 32, 
                                               FTResolution == 17500 ~ 64,
                                               FTResolution == 30000 ~ 64,
                                               FTResolution == 45000 ~ 96,
                                               FTResolution == 50000 ~ 96,
                                               FTResolution == 35000 ~ 128,
                                               FTResolution == 60000 ~ 128,
                                               FTResolution == 70000 ~ 256,
                                               FTResolution == 120000 ~ 256,
                                               FTResolution == 140000 ~ 512,
                                               FTResolution == 240000 ~ 512
                                              )
                 )
  return(res)
} 

#TODO:qunatile part needed? If no MS1 scan is present? -> DIA take lowest window as cycle indicator?
#' Calculate MS Cycle Time
#'
#' @param x a \code{\link{data.frame}} fullfilling the \code{\link{is.rawDiag}} column naming criteria.
#'
#' @return calculates the time of all ms cycles and the 95% quantile value there of. 
#' the cycle time is defined as the time between two consecutive MS1 scans
#' @export calc.cycle.time
calc.cycle.time <- function(x){
  
  df <- x %>% 
    dplyr::filter_at(vars("MSOrder"), any_vars(. == "Ms")) %>% 
    dplyr::select_at(vars("StartTime", "filename")) %>% 
    dplyr::group_by_at(vars("filename")) %>% 
    dplyr::mutate_at(vars("StartTime"), funs("CycleTime" = (. - lag(.))*60)) %>% 
    na.omit()
  
  df2 <- df %>% 
    group_by_at("filename") %>% 
    summarise_at(vars("CycleTime"),funs("quan" = quantile), probs = 0.95)
  res <- dplyr::left_join(df, df2, by = "filename") %>% 
    ungroup()
  
  return(res)
}

#' Calculate moving average of scan frequency
#'
#' @param x a \code{\link{data.frame}} fullfilling the \code{\link{is.rawDiag}} column naming criteria.
#'
#' @return a \code{data.frame}.
ScanFrequMovingOver <- function(x){
  res <- x %>%
    dplyr::ungroup() %>% 
    dplyr::select_at(vars("filename", "MSOrder","StartTime")) %>% 
    dplyr::mutate_at(vars("StartTime"), funs("Time"= ceiling(.*60))) %>% 
    dplyr::mutate_at(vars("MSOrder"), funs("ms" = (. == "Ms"))) %>% 
    dplyr::mutate_at(vars("MSOrder"), funs("ms2" = (. == "Ms2"))) %>% 
    dplyr::group_by_at(vars("filename","Time")) %>% 
    dplyr::mutate_at(vars("ms", "ms2"), funs(cumsum(.))) %>% 
    dplyr::summarise_at(vars("ms", "ms2"), funs(max(.))) %>% 
    tidyr::gather(key = "Type", value = "Counts", c("ms", "ms2")) %>% 
    dplyr::ungroup() %>% 
    dplyr::group_by_at(vars("filename", "Type")) %>% 
    dplyr::mutate_at(vars("Counts"), funs("Frequency" =  as.numeric(stats::filter(., rep( 1/30, 30), sides = 2)))) %>% 
    dplyr::mutate_at(vars("Time"), funs(./60))
  return(res)
}  

.CalculatePrescan <- function(x){
  res <- x %>% 
    dplyr::select_at(vars("AGCMode", "filename", "scanNumber")) %>% 
    mutate(PrescanIDX = dplyr::case_when(AGCMode == "On" ~ scanNumber)) %>%  
    mutate(PrescanIDX = fillNAgaps(.$PrescanIDX)) %>% 
    mutate(PrescanMode = PrescanIDX - scanNumber) %>% 
    mutate(PrescanMode = replace(PrescanMode, PrescanMode == 0, 1)) %>% 
    dplyr::pull(PrescanMode)
  return(res)
}  

# ----Plots----

#' TIC and Base Peak plot function
#' 
#' @description  Function for displaying the Total Ion Cound (TIC) and Base Peak chromatogram of a mass spectrometry measurement. 
#' Multiple files are handled by faceting based on filename.
#'
#' @param x a \code{\link{data.frame}} fullfilling the \code{\link{is.rawDiag}} column naming criteria.
#' @param method a character 'trellis', 'violin' or 'overlay'.
#' @return a ggplot object for graphing the TIC and the Base Peak chromatogram
#' @aliases tic.basepeak.overlay tic.basepeak.violin
#' @import ggplot2
#' @importFrom ggplot2 facet_wrap geom_violin scale_y_continuous facet_grid aes_string theme_light geom_line stat_summary
#' @export PlotTicBasepeak
PlotTicBasepeak <- function(x, method = 'trellis'){
  if (method == 'trellis'){
    df <- x %>% 
      dplyr::filter_at(vars("MSOrder"), any_vars( . == "Ms")) %>% 
      dplyr::select_at(vars("StartTime", "TIC", "BasePeakIntensity", "filename")) %>% 
      dplyr::rename_at(vars("BasePeakIntensity"), funs(as.character("Base_Peak"))) %>% 
      tidyr::gather(key = "Type", value = "Intensity", c("TIC", "Base_Peak"))
    
    df$Type <- factor(df$Type, levels = c("TIC", "Base_Peak"))
    
    figure <- ggplot(df,aes_string(x = "StartTime", y = "Intensity")) +
      geom_line(size = 0.3) +
      facet_wrap(filename ~ Type, scales = "free", ncol = 2) +
      labs(title = "TIC and Base-Peak plot") +
      labs(subtitle = "Plotting TIC intensity and base peak intensity against retention time") +
      labs(x = " Retention Time [min]", y = "Intensity Counts [arb. unit]") +
      scale_x_continuous(breaks = scales::pretty_breaks(10)) +
      scale_y_continuous(breaks = scales::pretty_breaks(8)) +
      theme_light() 
    return(figure)
    
  }else if(method =='violin'){
    df <- x %>% 
      dplyr::filter_at(vars("MSOrder"), any_vars( . == "Ms")) %>% 
      dplyr::select_at(vars("StartTime", "TIC", "BasePeakIntensity", "filename")) %>% 
      dplyr::rename_at(vars("BasePeakIntensity"), funs(as.character("Base_Peak"))) %>% 
      tidyr::gather(key = "Type", value = "Intensity", c("TIC", "Base_Peak"))
    df$Type <- factor(df$Type, levels = c("TIC", "Base_Peak"))
    
    figure <- ggplot(df, aes_string(x = "filename", y = "Intensity")) + 
      geom_violin() +
      facet_grid(Type~., scales = "free") +
      stat_summary(fun.y = mean , geom = "point", colour = "red") +
      scale_y_continuous(breaks = scales::pretty_breaks(8)) +
      theme_light() +
      theme(axis.text.x = element_text(angle = 90))
    return(figure)
    
  }else if (method  == 'overlay'){
    df <- x %>% 
      dplyr::filter_at(vars("MSOrder"), any_vars( . == "Ms")) %>% 
      dplyr::select_at(vars("StartTime", "TIC", "BasePeakIntensity", "filename")) %>% 
      dplyr::rename_at(vars("BasePeakIntensity"), funs(as.character("Base_Peak"))) %>% 
      tidyr::gather(key = "Type", value = "Intensity", c("TIC", "Base_Peak"))
    df$Type <- factor(df$Type, levels = c("TIC", "Base_Peak"))
    
    figure <- ggplot(df, aes_string(x = "StartTime", y = "Intensity", colour = "filename")) +
      geom_line(size = 0.3) +
      facet_grid(Type~., scales = "free") +
      theme_light() +
      theme(legend.position="top")
    return(figure)
    
  }else{NULL}
}

#' cycle time
#' 
#' @param x a \code{\link{data.frame}} fullfilling the \code{\link{is.rawDiag}} column naming criteria.
#' @param method a character 'trellis', 'violin' or 'overlay'.
#' @description graphs cycle time versus rt. each item represents the time for one scan cycle.
#' @return a \code{\link{ggplot}} object.
#' @aliases cycle.time.violin cycle.time.overlay
#' @export PlotCycleTime
#' @import dplyr
PlotCycleTime <- function(x, method = 'trellis'){
  if (method == 'trellis'){
    df <- calc.cycle.time(x = x)
    
    figure <- ggplot(df, aes_string(x = "StartTime", y = "CycleTime")) + 
      geom_point(shape = ".") +
      geom_line(stat = "smooth", method = "gam", formula = y ~ s(x, bs= "cs"), colour = "deepskyblue3", se = FALSE) +
      labs(title = "Cycle time plot", subtitle = "Caclulated cycle time vs retention time") +
      labs(x = "Retention Time [min]", y = "Cycle Time [sec]") +
      geom_hline(aes_string(yintercept = "quan", group = "filename"), colour = "red3", linetype = "longdash")+
      scale_x_continuous(breaks = scales::pretty_breaks(8)) +
      scale_y_continuous(breaks = scales::pretty_breaks(8)) +
      theme_light() + 
      facet_grid(filename~., scales = "free")
    return(figure)
    
  }else if (method == 'violin'){
    df <- calc.cycle.time(x=x)
    dots <- df %>%
      dplyr::select_at(vars("filename", "quan")) %>% 
      distinct()
    
    figure <- ggplot(df, aes_string(x = "filename", y = "CycleTime", colour = "filename")) + 
      geom_violin()  +
      stat_summary(fun.y = mean , geom = "point", colour = "red") +
      geom_point(data = dots, aes_string(x = "filename", y = "quan"), colour = "black")+
      scale_y_continuous(breaks = scales::pretty_breaks(8)) +
      theme_light() +
      theme(axis.text.x=element_blank(), legend.position = "top")
    # theme(axis.text.x = element_text(angle = 90))
    return(figure)
    
  } else if(method == 'overlay'){
    df <- calc.cycle.time(x=x)
    
    figure <- ggplot(df, aes_string(x = "StartTime", y = "CycleTime", colour = "filename")) + 
      geom_point(size = 0.5) +
      geom_line(aes_string(group = "filename", colour = "filename"), stat = "smooth", method = "gam", formula = y ~ s(x, bs= "cs"), se = FALSE) +
      scale_y_continuous(breaks = scales::pretty_breaks(8)) +
      theme_light() +
      theme(legend.position="top")
    return(figure)
    
  }else{NULL}
}

#' mz distribution
#'
#' @param x a \code{\link{data.frame}} fullfilling the \code{\link{is.rawDiag}} column naming criteria.
#' @param method a character 'trellis', 'violin' or 'overlay'.
#' @return a ggplot objecy
#' @aliases mz.distribution.overlay mz.distribution.violin
#' @export PlotMzDistribution
PlotMzDistribution <- function(x, method='trellis'){
  if (method == 'trellis'){
    df <- x %>% 
      dplyr::filter(MSOrder == "Ms2")
    
    figure <- ggplot(df, aes_string(x = "StartTime", y = "PrecursorMass")) + 
      geom_point(shape = ".") +
      facet_grid(filename~., scales = "free") +
      geom_line(stat = "smooth", method = "gam", formula = y ~ s(x, bs= "cs"), size = 1.1, alpha = 0.6, colour = "deepskyblue3", se = FALSE) +
      scale_x_continuous(breaks = scales::pretty_breaks(8)) +
      scale_y_continuous(breaks = scales::pretty_breaks(8)) +
      labs(title = "Retention Time to m/z correlation plot") +
      labs(subtitle = "Plotting retention time against m/z value of all selected precursors") +
      labs(x = "Retention Time", y = "Presursor m/z value") +
      theme_light() +
      labs(subtitle = "") +
      scale_fill_manual(values = color.vector.2) + 
      scale_color_manual(values = color.vector.2) + 
      scale_x_continuous(breaks = scales::pretty_breaks(5)) +
      theme(legend.position = 'top') +
      theme(title = element_text(size = 36)) +
      theme(axis.title = element_text(size = 20)) +
      theme(axis.text.x = element_text(face="bold", size=14)) +
      theme(axis.text.y = element_text(face="bold", size=14)) +
      theme(axis.line = element_line(size = 1)) +
      theme(panel.grid.major = element_line(colour = "gray"))
    
    return(figure)
  }else if (method == 'violin'){
    df <- x %>% 
      dplyr::filter(MSOrder == "Ms2")
    
    figure <- ggplot(df, aes_string(x = "filename", y = "PrecursorMass")) + 
      geom_violin() +
      stat_summary(fun.y = max , geom = "point", colour = "red") +
      scale_y_continuous(breaks = scales::pretty_breaks(8)) +
      labs(title = "Retention Time to m/z correlation plot") +
      labs(subtitle = "Plotting retention time against m/z value of all selected precursors") +
      labs(x = "Retention Time", y = "Presursor m/z value") +
      theme_light() +
      theme(axis.text.x = element_text(angle = 90))
    return(figure)
  }else if (method == 'overlay'){
    df <- x %>% 
      dplyr::filter(MSOrder == "Ms2")
    
    figure <- ggplot(df, aes_string(x = "StartTime", y = "PrecursorMass", colour = "filename")) + 
      geom_point(size = 0.5) +
      #geom_line(stat = "smooth",
      #          method = "gam",
      #          formula = y ~ s(x, bs= "cs"),
      #          size = 1.1, alpha = 0.6,
      #          colour = "deepskyblue3", se = FALSE) +
      scale_x_continuous(breaks = scales::pretty_breaks(8)) +
      scale_y_continuous(breaks = scales::pretty_breaks(8)) +
      labs(title = "Retention Time to m/z correlation plot") +
      labs(subtitle = "Plotting retention time against m/z value of all selected precursors") +
      labs(x = "Retention Time", y = "Presursor m/z value") +
      theme_light() +
      theme(legend.position="top")
    
    return(figure)
  }else{NULL}}

#TODO: make ChargeState robust for DIA data -> if not in inclusion list all 0! ->set to 2?
#TODO: only display charge states 1-8 and sum charge states >8 into one factor


#'mass distribution plot
#'
#' @description plots the mass frequency in dependency to the charge state
#' @param x a \code{\link{data.frame}} fullfilling the \code{\link{is.rawDiag}} column naming criteria.
#' @param method a character 'trellis', 'violin' or 'overlay'.
#'
#' @return a ggplot object.
#' @aliases mass.distribution.violin mass.distribution.overlay
#' @export PlotMassDistribution
#' @examples 
#'  library(ggplot2)
#'  data(WU163763)
#'  
#'  PlotMassDistribution(WU163763, method = 'trellis') +
#'    facet_wrap(~ filename, ncol = 3)
#'  
#'  PlotMassDistribution(WU163763, method = 'violin') +
#'    theme(legend.position = 'none')
#'    
#'  PlotMassDistribution(WU163763, method = 'overlay') +
#'    theme(legend.position = 'none')  
PlotMassDistribution <- function(x, method = 'trellis'){ 
  if (method == 'trellis'){
    res <- x %>% dplyr::filter_at(vars("MSOrder"), any_vars(. == "Ms2")) %>% 
      dplyr::select_at(vars("ChargeState", "PrecursorMass", "filename"))
    
    res$deconv <-  round((res$PrecursorMass -1.00782) * res$ChargeState, 0)
    
    res <- dplyr::mutate_at(res, vars("ChargeState"), funs(factor(.)))
    
    ncolor <- length(unique(res$ChargeState))
    
    figure <- ggplot(res, aes_string(x = "deconv", fill = "ChargeState", colour = "ChargeState")) +
      geom_histogram(binwidth = 100, alpha = .3, position = "identity") +
      labs(title = "Precursor mass to charge frequency plot ") +
      labs(subtitle = "Plotting frequency of precursor masses for each charge state") +
      labs(x = "Precursor mass [neutral mass]", y = "Frequency [counts]") +
      labs(fill = "Charge State", colour = "Charge State") +
      scale_x_continuous(breaks = scales::pretty_breaks(8)) +
      coord_cartesian(xlim = c(min(res$deconv), 10000)) +
      theme_light() + 
      facet_wrap(~filename)
    
    return(figure)
  }else if (method == 'violin'){ #mz.frequency.violin
    res <- x %>% dplyr::filter_at(vars("MSOrder"), any_vars(. == "Ms2")) %>% 
      dplyr::select_at(vars("ChargeState", "PrecursorMass", "filename"))
    
    res$deconv <-  round((res$PrecursorMass -1.00782)* res$ChargeState, 0)
    
    res <- dplyr::mutate_at(res, vars("ChargeState"), funs(factor(.)))
    
    figure <- ggplot(res, aes_string(x = "ChargeState", y = "deconv", fill = "filename")) +
      geom_violin() +
      #geom_histogram(binwidth = 100, alpha = .3, position = "identity") +
      labs(title = "Precursor mass to charge frequency plot ") +
      labs(subtitle = "Plotting frequency of precursor masses for each charge state") +
      labs(x = "Charge State ", y = "Mass [neutral mass]") +
      labs(fill = "Charge State", colour = "Charge State") +
      #scale_x_continuous(breaks = scales::pretty_breaks(8)) +
      theme_light() +
      theme(legend.position = "top")
    #facet_wrap(~filename)
    
    return(figure)
  }else if (method == 'overlay'){ #mz.frequency.overlay
    res <-  x %>% 
      dplyr::filter_at(vars("MSOrder"), any_vars(. == "Ms2")) %>% 
      dplyr::select_at(vars("ChargeState", "PrecursorMass", "filename")) %>% 
      dplyr::mutate("deconv" = round((PrecursorMass -1.00782)*ChargeState, 0)) %>% 
      dplyr::mutate_at(vars("ChargeState"), funs(factor(.)))
    
    figure <- ggplot(res, aes_string(x = "deconv", colour = "filename")) +
      geom_line(stat = "density") +
      #geom_density(aes(y= ..density.. )) + with base line along x axis with density value y= 0
      #geom_histogram(binwidth = 100, alpha = .3, position = "identity") +
      labs(title = "Precursor mass to charge frequency plot ") +
      labs(subtitle = "Plotting frequency of precursor masses for each charge state") +
      labs(x = "Precursor mass [neutral mass]", y = "Frequency [counts]") +
      labs(fill = "Charge State", colour = "Charge State") +
      scale_x_continuous(breaks = scales::pretty_breaks(12)) +
      coord_cartesian(xlim = c(min(res$deconv), 10000)) +
      theme_light() +
      theme(legend.position = "top")
    #facet_wrap(~filename)
    
    return(figure)
  }else{NULL}
}

#' charge state overview
#'
#' @param x a \code{\link{data.frame}} fullfilling the \code{\link{is.rawDiag}} column naming criteria.
#' @param method a character 'trellis', 'violin' or 'overlay'.
#'
#' @return a ggplot object.
#' @aliases charge.state.overlay charge.state.violin
#' @export PlotChargeState
PlotChargeState <- function(x, method='trellis'){
  if (method == 'trellis'){
    res <- x %>% 
      dplyr::filter(MSOrder == "Ms2") %>% 
      dplyr::group_by_at(vars("filename")) %>% 
      dplyr::count(ChargeState) %>% 
      dplyr::ungroup() %>% 
      dplyr::rename_at(vars("n"), funs(as.character("Counts")))
    
    res$percentage <- (100 / sum(res$Counts)) * res$Counts
    
    xbreaks <- unique(res$ChargeState)
    
    figure <- ggplot(res, aes_string(x = "ChargeState", y = "percentage")) +
      geom_bar(stat = "identity", fill = "deepskyblue2") +
      geom_text(aes_string(label = "Counts"), vjust=-0.3, size=3.5) +
      scale_x_continuous(breaks = xbreaks) +
      scale_y_continuous(breaks = scales::pretty_breaks(15), expand = c(0, 0), limits = c(0, (max(res$percentage))+3)) +
      labs(title = "Charge state plot") +
      labs(subtitle = "Plotting the number of occurrences of all selected precursor charge states") +
      labs(x = "Charge States", y = "Percent [%]") +
      theme_light() +
      facet_wrap(~filename)
    
    return(figure)
  }else if(method =='overlay'){
    res <- x %>% 
      dplyr::filter(MSOrder == "Ms2") %>% 
      dplyr::group_by_at(vars("filename")) %>% 
      dplyr::count(ChargeState) %>% 
      dplyr::ungroup() %>% 
      dplyr::rename_at(vars("n"), funs(as.character("Counts")))
    
    res$percentage <- (100 / sum(res$Counts)) * res$Counts
    
    xbreaks <- unique(res$ChargeState)
    
    figure <- ggplot(res, aes_string(x = "ChargeState", y = "percentage", fill = "filename")) +
      geom_bar(stat = "identity", position = position_dodge(), colour = "black") +
      #geom_text(aes_string(label = "Counts"), vjust=-0.3, size=3.5, position = position_dodge(width = 0.9)) +
      scale_x_continuous(breaks = xbreaks) +
      scale_y_continuous(breaks = scales::pretty_breaks(15), expand = c(0, 0), limits = c(0, (max(res$percentage))+3)) +
      labs(title = "Charge state plot") +
      labs(subtitle = "Plotting the number of occurrences of all selected precursor charge states") +
      labs(x = "Charge States", y = "Percent [%]") +
      theme_light()+
      theme(legend.position = "top")
    
    return(figure)
  }else if (method =='violin'){
    res <- x %>% 
      dplyr::filter(MSOrder == "Ms2") 
    
    figure <- ggplot(res, aes_string(x = "filename", y = "ChargeState", fill = "filename")) +
      geom_boxplot(width=.05, outlier.colour = "black", position = position_dodge())+ 
      geom_violin(position = position_dodge())+
      #geom_violin() +
      scale_y_continuous(breaks = scales::pretty_breaks(15)) +
      labs(title = "Charge state plot") +
      labs(subtitle = "Plotting the number of occurrences of all selected precursor charge states") +
      labs(x = "filename", y = "Charge State") +
      theme_light() +
      theme(axis.text.x=element_blank(), legend.position = "top")
    
    return(figure)
  }else{NULL}}



#' scan event
#' @description plots time for each scan event
#' @param x a \code{\link{data.frame}} fullfilling the \code{\link{is.rawDiag}} column naming criteria.
#' @param method a character 'trellis', 'violin' or 'overlay'.
#' @aliases scan.time.overlay scan.time.violin
#' @return a ggplot object.
#' @export PlotScanTime
#' @import tidyr
PlotScanTime <- function(x, method='trellis'){
  if(method == 'trellis'){
    res <- x %>% 
      dplyr::mutate(ElapsedScanTimesec = ElapsedScanTimesec * 1000)
    
    figure <- ggplot(res, aes_string(x = "StartTime", y = "ElapsedScanTimesec")) +
      geom_point(shape = ".") +
      facet_wrap(filename ~ MSOrder + MassAnalyzer, scales = "free") +
      geom_line(stat = "smooth", method = "gam",
                formula = y~s(x), colour = "deepskyblue3", se = FALSE) +
      labs(title = "Scan time plot",
           subtitle = "Plotting the elapsed scan time for each individual scan") +
      labs(x = "Retentione Time [min]", y = "Scan Time [ms]") +
      scale_x_continuous(breaks = scales::pretty_breaks((n = 8))) +
      scale_y_continuous(breaks = scales::pretty_breaks((n = 8))) +
      theme_light() +
      geom_hline(data = res, aes_string(yintercept = "transient"), colour = "red3")
    
    return(figure)
  }else if (method == 'violin'){
    # TODO
    # stopifnot(is.rawfile(x))
    res <- x %>% 
      dplyr::mutate_at(vars("ElapsedScanTimesec"), funs(.*1000)) %>% 
      dplyr::select_at(vars("ElapsedScanTimesec", "filename", "MassAnalyzer", "MSOrder")) %>% 
      na.omit()
    
    figure <- ggplot(res, aes_string(x = "filename", y = "ElapsedScanTimesec")) +
      geom_violin()  +
      facet_grid(MSOrder + MassAnalyzer~., scales = "free") +
      stat_summary(fun.y = max , geom = "point", colour = "red") +
      scale_y_continuous(breaks = scales::pretty_breaks(8)) +
      labs(y = "Elapsed Scan Time [mili seconds]")
    theme_light() +
      theme(axis.text.x = element_text(angle = 90))
    return(figure)
  }else if (method == 'overlay'){
    figure <- ggplot(x, aes_string(x = "StartTime", y = "ElapsedScanTimesec", colour = "filename")) +
      geom_point(size = 0.5) +
      geom_line(aes_string(group = "filename", colour = "filename"), stat = "smooth", method = "gam", formula = y ~ s(x, bs= "cs"), se = FALSE) +
      facet_grid(~ MSOrder + MassAnalyzer, scales = "free") +
      theme_light() +
      theme(legend.position="top")
    return(figure)
  }
  else{NULL}}




#' injection.time
#'
#' @param x a \code{\link{data.frame}} fullfilling the \code{\link{is.rawDiag}} column naming criteria.
#' @param method a character 'trellis' or 'overlay'.
#' @aliases injection.time.overlay
#' @return ggplot object
#' @export PlotInjectionTime
PlotInjectionTime <- function(x, method='trellis'){
  if (method == 'trellis'){
    maxtimes <- x %>% 
      dplyr::group_by(filename, MSOrder) %>% 
      dplyr::summarise(maxima = max(IonInjectionTimems))
    
    figure <- ggplot(x, aes_string(x = "StartTime", y = "IonInjectionTimems")) +
      geom_hline(data = maxtimes, aes_string(yintercept = "maxima"), colour = "red3", linetype = "longdash") +
      geom_point(shape = ".") +
      geom_line(stat = "smooth", method = "gam", formula = y ~ s(x, bs= "cs"), colour = "deepskyblue3", se = FALSE) +
      facet_grid(filename ~ MSOrder, scales = "free") +
      scale_y_continuous(breaks = scales::pretty_breaks((n = 8))) +
      scale_x_continuous(breaks = scales::pretty_breaks((n = 8))) +
      labs(title = "Injection time plot", subtitle = "Plotting injection time against retention time for MS and MSn level") +
      labs(x = "Retentione Time [min]", y = "Injection Time [ms]") +
      theme_light()
    return(figure)
  }
  else if (method == 'violin'){
    figure <- ggplot(x, aes_string(x = "filename", y = "IonInjectionTimems")) +
      geom_violin() +
      facet_grid(MSOrder~.) +
      stat_summary(fun.y = max , geom = "point", colour = "red") +
      theme_light() +
      theme(axis.text.x = element_text(angle = 90))
    return(figure)
  }else if(method == 'overlay'){
    figure <- ggplot(x, aes_string(x = "StartTime", y = "IonInjectionTimems", colour = "filename")) +
      geom_point(size = 0.5) +
      geom_line(aes_string(group = "filename", colour = "filename"), stat = "smooth", method = "gam", formula = y ~ s(x, bs= "cs"), se = FALSE) +
      facet_grid(~ MSOrder, scales = "free") +
      theme_light() +
      theme(legend.position="top")
    return(figure)
  }else{NULL}
}

#' lock mass correction plot
#'
#' @param x  a \code{\link{data.frame}} fullfilling the \code{\link{is.rawDiag}} column naming criteria.
#' @param method a character 'trellis', 'violin' or 'overlay'.
#' @aliases lm.correction.violin lm.correction.overlay
#' @return a ggplot object.
#' @export PlotLockMassCorrection
PlotLockMassCorrection <- function(x, method='trellis'){
  if (method == 'trellis'){
    res <- x %>% 
      dplyr::filter(MSOrder == "Ms")
    figure <- ggplot(res, aes_string(x = "StartTime" , y = "LMCorrection")) +
      geom_hline(yintercept = c(-5,5), colour = "red3", linetype = "longdash") +
      geom_line(size = 0.3) +
      geom_line(stat = "smooth", method= "gam", formula = y ~ s(x, bs ="cs"), colour = "deepskyblue3", se = FALSE) +
      labs(title = "Lock mass correction plot", subtitle = "Plotting lock mass correction value over time") +
      labs(x = "Retention Time [min]", y = "Lock Mass Correction [ppm]") +
      scale_x_continuous(breaks = scales::pretty_breaks(8)) +
      scale_y_continuous(breaks = scales::pretty_breaks(8), limits = c(-10,10)) +
      facet_wrap(~filename)+
      theme_light()
    return(figure)
  }else if(method == 'violin'){
    res <- x %>% 
      dplyr::filter(MSOrder == "Ms")
    figure <- ggplot(res, aes_string(x = "filename", y = "LMCorrection")) +
      geom_hline(yintercept = c(-5,5), colour = "red3", linetype = "longdash") +
      geom_violin()+
      theme_light() +
      theme(axis.text.x = element_text(angle = 90))
    return(figure)
  }else if (method == 'overlay'){
    res <- x %>% 
      dplyr::filter(MSOrder == "Ms")
    figure <- ggplot(res, aes_string(x = "StartTime" , y = "LMCorrection", colour = "filename")) +
      geom_hline(yintercept = c(-5,5), colour = "red3", linetype = "longdash") +
      geom_line(size = 0.3) +
      geom_line(stat = "smooth", method= "gam", formula = y ~ s(x, bs ="cs"), colour = "deepskyblue3", se = FALSE) +
      labs(title = "Lock mass correction plot", subtitle = "Plotting lock mass correction value over time") +
      labs(x = "Retention Time [min]", y = "Lock Mass Correction [ppm]") +
      scale_x_continuous(breaks = scales::pretty_breaks(8)) +
      scale_y_continuous(breaks = scales::pretty_breaks(8), limits = c(-10,10)) +
      theme_light()
    return(figure)
  }else{NULL}}




#' cycle.load plot
#'
#' @param x a \code{\link{data.frame}} fullfilling the \code{\link{is.rawDiag}} column naming criteria.
#' @param method a character 'trellis', 'violin' or 'overlay'.
#' @aliases cycle.load.overlay cycle.load.violin
#' @return a ggplot object.
#' @import scales
#' @export PlotCycleLoad
PlotCycleLoad <- function(x, method = 'trellis'){ #old name ms2.distribution
  if (method == 'trellis'){
    MS2 <- x %>% 
      dplyr::filter(MSOrder == "Ms2") %>% 
      dplyr::group_by_at(vars("filename")) %>% 
      dplyr::count(MasterScanNumber) %>% 
      dplyr::rename(scanNumber = MasterScanNumber)
    MS <- x %>% 
      dplyr::select(StartTime, scanNumber)
    res <- dplyr::inner_join(MS, MS2, by = "scanNumber")
    
    
    figure <- ggplot(res, aes_string(x = "StartTime", y = "n")) +
      geom_point(shape = ".") +
      geom_line(stat = "smooth", method = "loess", span = 0.2, colour = "deepskyblue3", se = FALSE) +
      facet_wrap(~filename) +
      scale_y_continuous(breaks = scales::pretty_breaks(8)) +
      scale_x_continuous(breaks = scales::pretty_breaks(8)) +
      coord_cartesian(ylim = c(0, max(res$n)+1)) +
      labs(title = "Time resolved number of Ms2 scans") +
      labs(subtitle = "Plotting the number of Ms2 per Ms1 scan versus retention time") +
      labs(x = "Retention Time [min]", y = "Number of Ms2 per Ms1 [counts]") +
      theme_light()
    return(figure)
  }else if(method == 'violin'){
    MS2 <- x %>% 
      dplyr::filter(MSOrder == "Ms2") %>% 
      dplyr::group_by_at(vars("filename")) %>% 
      dplyr::count(MasterScanNumber) %>% 
      dplyr::rename(scanNumber = MasterScanNumber)
    MS <- x %>% 
      dplyr::select(StartTime, scanNumber)
    res <- dplyr::inner_join(MS, MS2, by = "scanNumber") %>% 
      dplyr::mutate_at(vars("filename"), funs(as.factor(.))) %>% 
      dplyr::mutate_at(vars("n"), funs(as.numeric(.)))
    
    figure <- ggplot(res, aes_string(x = "filename", y = "n")) +
      geom_violin() +
      coord_cartesian(ylim = c(0, max(res$n)+1)) +
      labs(title = "Time resolved number of Ms2 scans") +
      labs(subtitle = "Plotting the number of Ms2 per Ms1 scan versus retention time") +
      labs(x = "Retention Time [min]", y = "Number of Ms2 per Ms1 [counts]") +
      theme_light()
    return(figure)
  }else if (method == 'overlay'){
    MS2 <- x %>% 
      dplyr::filter(MSOrder == "Ms2") %>% 
      dplyr::group_by_at(vars("filename")) %>% 
      dplyr::count(MasterScanNumber) %>% 
      dplyr::rename(scanNumber = MasterScanNumber)
    MS <- x %>% 
      dplyr::select(StartTime, scanNumber)
    res <- dplyr::inner_join(MS, MS2, by = "scanNumber")
    
    figure <- ggplot(res, aes_string(x = "StartTime", y = "n", colour = "filename")) +
      geom_point(shape = ".") +
      geom_line(aes_string(group = "filename", colour = "filename"),stat = "smooth", method = "loess", span = 0.2, se = FALSE) +
      scale_y_continuous(breaks = scales::pretty_breaks(8)) +
      scale_x_continuous(breaks = scales::pretty_breaks(8)) +
      coord_cartesian(ylim = c(0, max(res$n)+1)) +
      labs(title = "Time resolved number of Ms2 scans") +
      labs(subtitle = "Plotting the number of Ms2 per Ms1 scan versus retention time") +
      labs(x = "Retention Time [min]", y = "Number of Ms2 per Ms1 [counts]") +
      theme_light() +
      theme(legend.position = "top")
    return(figure)
  }else{NULL}}


#mods for cycle based plots and prescans

#' scan frequency plot
#' @param x a \code{\link{data.frame}} fullfilling the \code{\link{is.rawDiag}} column naming criteria.
#' @param method a character 'trellis', 'violin' or 'overlay'.
#' @aliases scan.frequency.violin scan.frequency.overlay
#' @return a trellis like plot object displaying the scan frequency of a mass spec run
#' @export PlotScanFrequency 
PlotScanFrequency <- function(x, method = 'trellis'){
  if (method == 'trellis'){
    res <- x %>% 
      dplyr::ungroup() %>% 
      ScanFrequMovingOver(.)
    
    figure <- ggplot(res, aes_string(x = "Time", y = "Frequency")) +
      geom_line() +
      facet_wrap(~filename+ Type) +
      scale_y_continuous(breaks = scales::pretty_breaks(16))
    
    return(figure)  
  }else if (method == 'violin'){
    res <- x %>% 
      dplyr::ungroup() %>% 
      ScanFrequMovingOver(.) %>% 
      dplyr::ungroup() %>% 
      dplyr::filter_at(vars("Type"), any_vars(. == "ms2"))
    
    figure <- ggplot(res, aes_string(x = "filename", y = "Counts")) +
      geom_violin()
    
    return(figure)  
  }else if (method =='overlay'){
    res <- x %>% 
      dplyr::ungroup() %>% 
      ScanFrequMovingOver(.)
    
    figure <- ggplot(res, aes_string(x = "Time", y = "Frequency", colour = "Type")) +
      facet_wrap(~filename) +
      geom_line() +
      theme_light() +
      theme(legend.position = "top")
    
    return(figure)  
  }else{NULL}
}

#' PrecursorMass versus StartTime hexagons MS2
#'
#' @param x a \code{\link{data.frame}} fullfilling the \code{\link{is.rawDiag}} column naming criteria.
#' @param bins number of bins in both vertical and horizontal directions. default is 80.
#' @param method plot method. 
#' @return a ggplot object.
#' @export PlotPrecursorHeatmap
#' @note TODO: define bin with dynamically as h= 2x IQR x n e-1/3 or number of bins (max-min)/h
#' @import hexbin
PlotPrecursorHeatmap <- function(x, method = 'overlay', bins = 80){
  
  res <- x %>% 
    dplyr::filter_at(vars("MSOrder"), any_vars(. == "Ms2"))
  
  gp <- ggplot(res, aes_string(x = 'StartTime', y = 'PrecursorMass')) + 
    geom_hex(bins = bins) + 
    scale_fill_gradientn(colours = colorvector) + 
    scale_x_continuous(breaks = scales::pretty_breaks(8)) + 
    scale_y_continuous(breaks = scales::pretty_breaks(15)) + 
    theme_light()
  
  if (method == 'trellis'){
    gp <- gp + facet_wrap(~filename) 
  }
  gp
}

#' mass heatmap
#'
#' @param x a \code{\link{data.frame}} fullfilling the \code{\link{is.rawDiag}} column naming criteria.
#' @param bins number of bins in both vertical and horizontal directions. default is 80.
#' @param method plot method. 
#' @description graphs a deconvoluted heatmap of the StartTime
#' @return a gglot object.
#' 
#' @export PlotMassHeatmap
PlotMassHeatmap <- function(x, method='trellis', bins = 80){ #rename to mass.heatmap
  
  res <- x %>% 
    dplyr::filter_at(vars("MSOrder"), any_vars(. == "Ms2")) %>% 
    dplyr::mutate("deconv" = round((PrecursorMass - 1.00782) * ChargeState, 0)) %>% 
    dplyr::filter_at(vars("deconv"), any_vars(. <= 10000))
  
  gp <- ggplot(res, aes_string(x = 'StartTime', y = 'deconv')) + 
    geom_hex(bins = bins ) +
    scale_fill_gradientn(colours = colorvector) +
    scale_x_continuous(breaks = scales::pretty_breaks(8)) + 
    scale_y_continuous(breaks = scales::pretty_breaks(15)) + 
    theme_light() +
    coord_cartesian(ylim = c(500, 10000))
  
  
  if (method == 'trellis'){
    gp <- gp + facet_wrap(~filename) 
  }
  gp
}

# ----Letter Figs----
.letter_figure1 <- function(){
  exactScanSpeedEst <- 
    function(nMS1 = 1, tMS1 = 138/1000 , nMS2 = 18 , tMS2 = 24/1000){ 
      (1 / (nMS1 * tMS1 + nMS2 * tMS2)) * nMS2 
    }
  
  roughScanSpeedEst <- 
    function(nMS2 = 1, tMS2 = 64/1000){ 
      1 / rep(tMS2, length(nMS2))
    }
  
  exactScanSpeedEstR15 <- 
    function(nMS1 = 1, tMS1 = 138/1000 , nMS2 = 18 , tMS2 = 35/1000){ 
      (1 / (nMS1 * tMS1 + nMS2 * tMS2)) * nMS2 
    }
  
  roughScanSpeedEst <- 
    function(nMS2 = 1, tMS2 = 64/1000 ){ 
      1 / rep(tMS2, length(nMS2))
    }
  
  nMS2 <- c(12, 18, 36, 72, 72:120)
  
  ScanSpeed <- rbind(
    data.frame(nMS2 = nMS2, 
               scanSpeed = exactScanSpeedEst(nMS2 = nMS2, 
                                             tMS2 = 24/1000), 
               func = 'exact', R=7500),
    data.frame(nMS2 = nMS2, 
               scanSpeed = exactScanSpeedEst(nMS2 = nMS2, 
                                             tMS2 = 35/1000), 
               func = 'exact', R=15000),
    data.frame(nMS2 = nMS2, 
               scanSpeed = roughScanSpeedEst(nMS2 = nMS2, 
                                             tMS2 = 24/1000), 
               func = 'rough', R=7500),
    data.frame(nMS2 = nMS2, 
               scanSpeed = roughScanSpeedEst(nMS2 = nMS2, 
                                             tMS2 = 35/1000), 
               func = 'rough', R=15000))
  
  stopifnot(require(lattice))
  cv <- 1-1:7/10
  t<-trellis.par.get("strip.background")
  t$col<-(rgb(cv,cv,cv))
  trellis.par.set("strip.background",t)
  # MScanSpeed <- c(20.09654, 30.24786)
  
  MScanSpeed <- list(
    R15000 <- c(20.3239509105305,20.3273087594134,20.3226444972288,20.0798890164562,20.066800689259,20.042351139192,19.5538114059853,19.4682387272898,19.3874799962346,20.5326530612245,20.5255102040816,20.527806122449),
    R7500 <- c(30.6791765637371,30.7659144893112,30.6657957244656,29.6024506988321,29.5382730231668,29.530078498947,28.2862130623,28.2683987574132,28.1165489974583,32.4190476190476,32.5380102040816,32.5643707482993))
  
  p <- xyplot(scanSpeed ~ nMS2|paste("R =",R), group=func, data=ScanSpeed, 
         ylim=c(min(MScanSpeed[[1]])-1,45),
         xlab=expression(n[MS2]),
         ylab=expression(f[MS2]),
         type='b',
         auto.key = list(columns=2),
         panel = function(x, y, ...) {
           panel.grid(h=-1, v=-1)
           panel.xyplot(x, y, ...)
           pn = panel.number()
           panel.points(rep(12, length(MScanSpeed[[pn]])), MScanSpeed[[pn]], pch=16, col='black',cex=0.5)
           panel.points(12, mean(MScanSpeed[[pn]]), cex=4, pch='-', col='black')
         },
         scales=list(
          
           x=list(
             #tck=c(12, 18, 72, 120),
             #at=nMS2[1:3]
             at = c(12, 18, 36, 72, 120)
           )
         ))
  p
}

# ----TechNote Figs----
.overview <- function(prefix="primer"){
  
  data(WU163763)
  WU <- WU163763
  WU <- WU[WU$filename %in% unique(WU$filename)[1:2], ]
  
  lapply(ls("package:rawDiag")[grepl("Plot", ls("package:rawDiag"))], 
         function(fn){
           lapply(c('trellis', 'violin', 'overlay'), function(a){
             pngFileName <- paste(paste(prefix, fn, a, sep='-'), "png", sep='.')
             
             message(pngFileName)
             
             if (!file.exists(pngFileName)){
               gp <- get(fn)(WU, a)  +
                 theme(legend.position = 'none') + 
                 theme(axis.line=element_blank(),
                       axis.text.x=element_blank(),
                       axis.text.y=element_blank(),
                       axis.ticks=element_blank(),
                       axis.title.x=element_blank(),
                       axis.title.y=element_blank(),
                       legend.position="none",
                       panel.background=element_blank(),
                       panel.border=element_blank(),
                       panel.grid.major=element_blank(),
                       panel.grid.minor=element_blank(),
                       plot.background=element_blank()) +
                 theme(plot.title = element_blank()) +
                 theme(plot.subtitle = element_blank()) +
                 theme(strip.background = element_blank()) +
                 theme(strip.text = element_blank())
               if (!is.null(gp)){
                 png(pngFileName, 240, 240)
                 print(gp)
                 dev.off()}
             }
           })}
         
  )
}

#' ASMS_benchmark_figure_1
#'
#' @return xyplot
.ASMS_benchmark_figure_1 <- function(){
  library(lattice)
  data(benchmark)
  cv <- 1 - 2:7/10
  t <- trellis.par.get("strip.background")
  t$col <- (rgb(cv,cv,cv))
  
  t$col<-(rgb(cv,cv,cv))
  trellis.par.set("strip.background",t)
  
  S <- rbind(b.Linux, b.Apple, X.Linux)
  
  S$IO.throuput <- sum(unique(S$nrow)) / S$overall.runtime
  
  xyplot(overall.runtime ~ ncpu | system, 
         subset=system=='Linux', 
         group=method,
         panel=function(...){
           
           panel.xyplot(...)
           panel.xyplot(..., type='a', lwd=2)
         },
         
         data=S, auto.key = TRUE, 
         xlab = 'number of utilized cores',
         ylab = 'overall runtime [s]',
         scales=list(y = list(log = TRUE, at=c(1, 30, 60, 120, 180, 300, 600, 1800, 3600, 3600 * 1.5))))
}

#' ASMS_benchmark_figure_2
#'
#' @return xyplot
.ASMS_benchmark_figure_2 <- function(){
  library(lattice)
  data(benchmark)
  cv <- 1 - 2:7 / 10
  t <- trellis.par.get("strip.background")
  t$col <- (rgb(cv,cv,cv))
  
  t$col<-(rgb(cv,cv,cv))
  trellis.par.set("strip.background",t)
  S <- rbind(b.Linux, b.Apple, X.Linux)
  
  S$IO.throuput <- sum(unique(S$nrow)) / S$overall.runtime
  
  xyplot(IO.throuput ~ ncpu|system, 
         subset=system=='Linux', group=method, 
         data=S, auto.key = TRUE, 
         xlab = 'number of utilized cores',
         ylab = 'processing frequency [Hz]',
         panel=function(...){
           
           panel.xyplot(...)
           panel.xyplot(..., type='a', lwd=2)
         },
         scales=list(y = list(log=TRUE, at=c(500,1000,2000,4000,8000,16000,32000,64000, 1280000))))
}

.technote_benchmark_figure_1 <- function(){
  data(benchmark)
  
  gp <- ggplot(rbind(b.Linux, b.Apple, X.Linux), aes(y = overall.runtime, 
                                                     x = ncpu, 
                                                     colour = method)) + 
  geom_line(size = 1.3) +
  facet_wrap(~system) + 
  coord_trans( y = "log10") +
  scale_colour_manual(values = c("cornflowerblue", "magenta")) + 
  scale_y_continuous(breaks = c(90, 120, 180, 240, 480, 600, 900, 1800, 3600, 5400)) +
  labs(x = "number of used processes", y = "overall runtime [sec]",
        subtitle='overall runtime') +
  theme_light() +
  theme(legend.position = "top") +
    theme(title = element_text(size = 24)) +
    theme(axis.title = element_text(size = 14)) +
    theme(legend.text = element_text(size = 8)) +
    theme(legend.title = element_text(size = 10, face = "bold"))
  
  gp
  
  #gp <- ggplot(rbind(b.Linux, b.Apple, X.Linux), aes(y=overall.runtime, x=ncpu,
  #                                                   group=ncpu), 
  #             color=method) + 
  #  coord_trans(y = "log10") +
  #  scale_y_continuous(breaks = c(90, 120, 180, 240, 480, 600, 900, 1800, 3600, 1800 + 3600)) +
  #  stat_summary(fun.y = mean, geom = "line", aes( group = 1)) + #colour = "deepskyblue2") +
  #  geom_boxplot() +
  #  labs(x = "number of used processes", y = "overall  time [in seconds] of read.raw",
  #       subtitle='overall runtime') +
  #  theme_light()
  
  #gp + facet_grid( .  ~ system * method ) 
}

.technote_benchmark_figure_2 <- function(){
  data(benchmark)
  
  b.Linux$IO.throuput <- sum(unique(b.Linux$nrow)) / b.Linux$overall.runtime
  X.Linux$IO.throuput <- sum(unique(X.Linux$nrow)) / X.Linux$overall.runtime 
  
  b.Apple$IO.throuput <- sum(unique(b.Apple$nrow)) / b.Apple$overall.runtime 
  
  gp <- ggplot(rbind(b.Linux, b.Apple, X.Linux), aes(y = IO.throuput, 
                                                     x = ncpu, 
                                                     colour = method)) + 
    geom_line(size = 1.3) +
    facet_wrap(~system) + 
    coord_trans( y = "log10") +
    scale_colour_manual(values = c("cornflowerblue", "magenta")) + 
    scale_y_continuous(breaks = c(500, 1000, 2000, 4000, 8000, 16000, 32000, 64000)) +
    labs(x = "number of used processes", y = "processing frequency [Hz]", 
         subtitle='IO throughput') +
    theme_light() +
    theme(legend.position = "top") +
    theme(title = element_text(size = 24)) +
    theme(axis.title = element_text(size = 14)) +
    theme(legend.text = element_text(size = 8)) +
    theme(legend.title = element_text(size = 10, face = "bold"))
  
  gp
  
  #gp <- ggplot(rbind(b.Linux, b.Apple, X.Linux), aes(y=IO.throuput, x=ncpu, group=ncpu)) + 
  #  stat_summary(fun.y = mean, geom = "line", aes( group = 1)) + #, colour = "deepskyblue2") +
  #  geom_boxplot() +
  #  coord_trans(y = "log10") +
  #  scale_y_continuous(breaks = c(500, 1000, 2000, 4000, 8000, 16000, 32000, 64000)) +
  #  labs(x = "number of used processes", y = "IO throughput  [number of scan info / s]", 
  #       subtitle='IO throughput') +
  #  theme_light() 
  # coord_trans(y = "log10") +
  #  scale_y_continuous(breaks = c(0.1, 0.25, 0.5, 1.0, 1.25)) +
  
  # annotate("text", x = 40, y = 0.20, label = paste("max throughput =", round(max(b.Linux$IO.throuput), 2), "GBytes/s")) +
  #  annotate("text", x = 40, y = 0.15, label = paste("min throughput =", round(min(b.Linux$IO.throuput), 2), "GBytes/s"))
  
  #gp + facet_grid( .  ~ system * method)
}
#remove
.technote_example_figure_1 <- function(x){
  res <- precursor.heatmap(x)  
  
  res +
    labs(title = "A)", subtitle = "") +
    labs(x = "Time [min] ", y = "Precursor Mass [m/z]") +
    theme(title = element_text(size = 36)) +
    theme(axis.title = element_text(size = 20)) +
    theme(axis.text.x = element_text(face="bold", size=14)) +
    theme(axis.text.y = element_text(face="bold", size=14)) +
    theme(axis.line = element_line(size = 1)) +
    theme(panel.grid.major = element_line(colour = "gray"))

}

#remove
.technote_example_figure_2 <- function(x){
  res <- .charge.states(x)  
  
  res +
    labs(title = "B)", subtitle = "") +
    labs(x = "Precursor charge state", y = "Percentag [%]") +
    theme(title = element_text(size = 36)) +
    theme(axis.title = element_text(size = 20)) +
    theme(axis.text.x = element_text(face="bold", size=14)) +
    theme(axis.text.y = element_text(face="bold", size=14)) +
    theme(axis.line = element_line(size = 1)) +
    theme(panel.grid.major = element_line(colour = "gray"))
  
}

#remove
.technote_example_figure_3 <- function(x, theme_title_element_text_size = 15){
  res <- .cycle.time(x)  
  
  res +
    labs(title = "C)", subtitle = "") +
    labs(x = "Time [min]", y = "Cycle Time [sec]") +
    theme(title = element_text(size = theme_title_element_text_size)) +
    theme(axis.title = element_text(size = 20)) +
    theme(axis.text.x = element_text(face="bold", size=14)) +
    theme(axis.text.y = element_text(face="bold", size=14)) +
    theme(axis.line = element_line(size = 1)) +
    theme(panel.grid.major = element_line(colour = "gray"))
  
}


.technote_application_figure_2 <- function(x){
  MS2 <- x %>% 
    dplyr::filter(MSOrder == "Ms2") %>% 
    dplyr::group_by_at(vars("filename")) %>% 
    dplyr::count(MasterScanNumber) %>% 
    dplyr::rename(scanNumber = MasterScanNumber)
  MS <- x %>% 
    dplyr::select(StartTime, scanNumber)
  res <- dplyr::inner_join(MS, MS2, by = "scanNumber") %>% 
    mutate(TopN = case_when(filename == "04_S174020" ~ 36,
                            filename == "05_S174020" ~ 72,
                            filename == "07_S174020" ~ 36,
                            filename == "08_S174020" ~ 72,
                            filename == "09_S174020" ~ 18,
                            filename == "11_S174020" ~ 72,
                            filename == "12_S174020" ~ 36,
                            filename == "13_S174020" ~ 18,
                            filename == "16_S174020" ~ 18)
    ) %>% 
    mutate(Replicate = case_when(filename == "04_S174020" ~ "R1",
                                 filename == "05_S174020" ~ "R1",
                                 filename == "07_S174020" ~ "R2",
                                 filename == "08_S174020" ~ "R2",
                                 filename == "09_S174020" ~ "R1",
                                 filename == "11_S174020" ~ "R3",
                                 filename == "12_S174020" ~ "R3",
                                 filename == "13_S174020" ~ "R2",
                                 filename == "16_S174020" ~ "R3")
    ) %>% 
    mutate(Filetag = paste(TopN, Replicate, sep = "_")) %>% 
    filter(Filetag == "18_R1" | Filetag == "36_R1" |Filetag == "72_R1")
  
  
  figure <- ggplot(res, aes_string(x = "StartTime", y = "n", colour = "Filetag")) +
    geom_point(shape = ".") +
    #geom_line(stat = "smooth", method = "loess", span = 0.2, aes(colour = "Filetag"), se = FALSE) +
    #facet_wrap(~Filetag) +
    scale_color_manual(values = color.vector[c(2,5,8)])+
    scale_y_continuous(breaks = scales::pretty_breaks(8)) +
    scale_x_continuous(breaks = scales::pretty_breaks(8)) +
    coord_cartesian(ylim = c(0, max(res$n)+1)) +
    labs(title = "B") +
    labs(x = "Time [min]", y = "Number of Ms2 per cycle [counts]") +
    theme_light() +
    theme(legend.position = "none") +
    theme(title = element_text(size = 19)) +
    theme(axis.title = element_text(size = 20)) +
    theme(axis.text.x = element_text(face="bold", size=14)) +
    theme(axis.text.y = element_text(face="bold", size=14)) +
    theme(axis.line = element_line(size = 1)) +
    theme(panel.grid.major = element_line(colour = "gray"))
  return(figure)
}

.technote_application_figure_1 <- function(x){
  res <- x %>%   
    dplyr::ungroup() %>% 
    ScanFrequMovingOver(.) %>% 
    mutate(TopN = case_when(filename == "04_S174020" ~ 36,
                            filename == "05_S174020" ~ 72,
                            filename == "07_S174020" ~ 36,
                            filename == "08_S174020" ~ 72,
                            filename == "09_S174020" ~ 18,
                            filename == "11_S174020" ~ 72,
                            filename == "12_S174020" ~ 36,
                            filename == "13_S174020" ~ 18,
                            filename == "16_S174020" ~ 18)
    ) %>% 
    mutate(Replicate = case_when(filename == "04_S174020" ~ "R1",
                                 filename == "05_S174020" ~ "R1",
                                 filename == "07_S174020" ~ "R2",
                                 filename == "08_S174020" ~ "R2",
                                 filename == "09_S174020" ~ "R1",
                                 filename == "11_S174020" ~ "R3",
                                 filename == "12_S174020" ~ "R3",
                                 filename == "13_S174020" ~ "R2",
                                 filename == "16_S174020" ~ "R3")
    ) %>% 
    mutate(Filetag = paste(TopN, Replicate, sep = "_")) %>% 
    ungroup() %>% 
    filter_at(vars("Type"), any_vars(. == "ms2"))
  
  figure <- ggplot(res, aes_string(x = "Time", y = "Frequency", colour = "Filetag")) +
    geom_line(size = 0.5) +
    scale_color_manual(values = color.vector)+ 
    labs(title = "A") +
    labs(x = "Time [min]", y = "Frequency [Hz]") +
    scale_y_continuous(breaks = scales::pretty_breaks(8))+
    scale_x_continuous(breaks = scales::pretty_breaks(6))+
    theme_light() +
    theme(legend.position = "none") +
    theme(title = element_text(size = 19)) +
    theme(axis.title = element_text(size = 20)) +
    theme(axis.text.x = element_text(face="bold", size=14)) +
    theme(axis.text.y = element_text(face="bold", size=14)) +
    theme(axis.line = element_line(size = 1)) +
    theme(panel.grid.major = element_line(colour = "gray"))
  return(figure)
}

#' technote's application figure 3
#'
#' @param x 
#'
#' @return a ggplot opbject
.technote_application_figure_3 <- function(x){
  res <- x %>% 
    dplyr::select_at(vars("proteins", "peptides", "TopN")) %>% 
    dplyr::mutate_at(vars("peptides"), funs(./10)) %>% 
    tidyr::gather(key = "Type", value = "counts", c("proteins", "peptides")) %>% 
    dplyr::mutate_at(vars("Type"), funs(as.factor(.)))
  
  ggplot(res, aes_string(x = "TopN", y = "counts")) + 
    geom_point(aes_string( colour = "Type")) +
    geom_line(data = DescValues, aes(x = TopN, y = proteins_mean, group = 1))+
    geom_line(data = DescValues, aes(x = TopN, y = (peptides_mean/10), group = 1))+
    scale_color_manual(values = c("#2171B5", "#D94801"))+
    labs(title = "C") +
    labs(x = "TopN ", y = "Counts") +
    annotate("text", x = "36", y = 4200, label = "# Proteins", colour = "#D94801", size = 8)+
    annotate("text", x = "36", y = 3600, label = "# Peptides [div by 10]", colour = "#2171B5", size = 8)+
    scale_y_continuous(breaks = scales::pretty_breaks(5))+
    theme_light() +
    theme(legend.position = "none") +
    theme(title = element_text(size = 28)) +
    theme(axis.title = element_text(size = 30)) +
    theme(axis.text.x = element_text(face="bold", size=21)) +
    theme(axis.text.y = element_text(face="bold", size=21)) +
    theme(axis.line = element_line(size = 1.5)) +
    theme(panel.grid.major = element_line(colour = "gray")) 
} 

#' technote's application figure 3
#'
#' @param x 
#'
#' @return a ggplot opbject
.technote_application_figure_4 <- function(x, y){
  x <- x %>% 
    mutate(TopN = case_when(filename == "04_S174020" ~ 36,
                            filename == "05_S174020" ~ 72,
                            filename == "07_S174020" ~ 36,
                            filename == "08_S174020" ~ 72,
                            filename == "09_S174020" ~ 18,
                            filename == "11_S174020" ~ 72,
                            filename == "12_S174020" ~ 36,
                            filename == "13_S174020" ~ 18,
                            filename == "16_S174020" ~ 18)
    ) %>% 
    mutate(Replicate = case_when(filename == "04_S174020" ~ "R1",
                                 filename == "05_S174020" ~ "R1",
                                 filename == "07_S174020" ~ "R2",
                                 filename == "08_S174020" ~ "R2",
                                 filename == "09_S174020" ~ "R1",
                                 filename == "11_S174020" ~ "R3",
                                 filename == "12_S174020" ~ "R3",
                                 filename == "13_S174020" ~ "R2",
                                 filename == "16_S174020" ~ "R3")
    ) %>% 
    mutate(Filetag = paste(TopN, Replicate, sep = "_")) %>% 
    ungroup()
  
  y <- dplyr::arrange(y, TopN) %>% 
    dplyr::pull(psm)
  
  res <- x %>% 
    dplyr::group_by_at(vars("Filetag", "TopN")) %>% 
    dplyr::filter_at(vars("MSOrder"), any_vars(. == "Ms2")) %>% 
    dplyr::summarise_at(vars("scanNumber"), funs(n()))
  
  res$psm <- y
  
  res <- res %>% 
    tidyr::gather(key= "Type", value = "counts", c("scanNumber", "psm")) %>% 
    dplyr::mutate_at(vars("Type", "TopN"), funs(as.factor(.)))
  
  res2 <- res %>% 
    dplyr::group_by_at(vars("TopN", "Type")) %>% 
    summarise_at(vars("counts"), funs(mean(.))) %>% 
    tidyr::spread(key = "Type", value = "counts")


  
  ggplot(res, aes_string(x = "TopN", y = "counts")) + 
    geom_point(aes_string( colour = "Type")) +
    geom_line(data = DescValues, aes(x = TopN, y = psm_mean, group = 1))+
    geom_line(data = res2, aes(x = TopN, y = (scanNumber), group = 1))+
    labs(title = "D") +
    labs(x = "TopN ", y = "Counts") +
    annotate("text", x = "36", y =80000, label = "# PSM", colour = "#2171B5", size = 8)+
    annotate("text", x = "36", y = 110000, label = "# MS2 Scans", colour ="#D94801", size = 8)+
    scale_y_continuous(breaks = scales::pretty_breaks(5))+
    scale_color_manual(values = c("#2171B5", "#D94801"))+
    theme_light() +
    theme(legend.position = "none") +
    theme(title = element_text(size = 28)) +
    theme(axis.title = element_text(size = 30)) +
    theme(axis.text.x = element_text(face="bold", size=21)) +
    theme(axis.text.y = element_text(face="bold", size=21)) +
    theme(axis.line = element_line(size = 1.5)) +
    theme(panel.grid.major = element_line(colour = "gray"))
} 

#' technote's application figure 5
#'
#' @param x 
#'
#' @return a ggplot opbject
.technote_application_figure_5 <- function(x){
  res <- x %>% 
    mutate(TopN = case_when(filename == "04_S174020" ~ 36,
                            filename == "05_S174020" ~ 72,
                            filename == "07_S174020" ~ 36,
                            filename == "08_S174020" ~ 72,
                            filename == "09_S174020" ~ 18,
                            filename == "11_S174020" ~ 72,
                            filename == "12_S174020" ~ 36,
                            filename == "13_S174020" ~ 18,
                            filename == "16_S174020" ~ 18)
    ) %>% 
    mutate(Replicate = case_when(filename == "04_S174020" ~ "R1",
                                 filename == "05_S174020" ~ "R1",
                                 filename == "07_S174020" ~ "R2",
                                 filename == "08_S174020" ~ "R2",
                                 filename == "09_S174020" ~ "R1",
                                 filename == "11_S174020" ~ "R3",
                                 filename == "12_S174020" ~ "R3",
                                 filename == "13_S174020" ~ "R2",
                                 filename == "16_S174020" ~ "R3")
    ) %>% 
    mutate(Filetag = paste(TopN, Replicate, sep = "_")) %>% 
    ungroup()
  
  res <- res %>% 
    dplyr::filter_at(vars("StartTime"), any_vars(.>= 15 & .<= 70)) %>% 
    dplyr::group_by_at(vars("Filetag", "MSOrder")) %>% 
    dplyr::summarise_at(vars("ElapsedScanTimesec"), funs(Time = round((sum(.)/60),1)))


  
  ggplot(res, aes_string(x = "Filetag", y = "Time")) + 
    geom_bar(aes_string(fill = "MSOrder"), stat = "identity")+
    labs(title = "E") +
    labs(x = "TopN ", y = "Time [min]") +
    scale_y_continuous(breaks = scales::pretty_breaks(8))+
    scale_x_discrete(labels = c("18", "18", "18", "36", "36", "36", "72", "72", "72"))+
    scale_fill_manual(values = c("#2171B5", "#D94801"))+
    theme_light() +
    theme(legend.position = "none") +
    theme(title = element_text(size = 28)) +
    theme(axis.title = element_text(size = 30)) +
    theme(axis.text.x = element_text(face="bold", size=21)) +
    theme(axis.text.y = element_text(face="bold", size=21)) +
    theme(axis.line = element_line(size = 1.5)) +
    theme(panel.grid.major = element_line(colour = "gray"))
} 


.technote_viz_figure_1 <- function(x){
  
  res <- x %>% dplyr::filter_at(vars("MSOrder"), any_vars(. == "Ms2")) %>% 
    dplyr::select_at(vars("ChargeState", "PrecursorMass", "filename"))
  
  res$deconv <-  round((res$PrecursorMass -1.00782)* res$ChargeState, 0)
  
  res <- dplyr::mutate_at(res, vars("ChargeState"), funs(factor(.)))
  
  figure <- ggplot(res, aes_string(x = "deconv", fill = "ChargeState", colour = "ChargeState")) +
    geom_histogram(binwidth = 100, alpha = .3, position = "identity") +
    facet_wrap(~filename, ncol = 3) +
    labs(title = "A ") +
    labs(x = "Neutral mass [Da]", y = "Counts") +
    labs(fill = "Charge State", colour = "Charge State") +
    scale_fill_manual(values = color.vector.2)+ 
    scale_color_manual(values = color.vector.2)+ 
    scale_x_continuous(breaks = scales::pretty_breaks(5)) +
    coord_cartesian(xlim = c(min(res$deconv), 10000)) +
    theme_light() + 
    theme(legend.position = 'none') +
    theme(title = element_text(size = 30)) +
    #theme(legend.title = element_text(size = 14)) +
    #theme(legend.key.size = unit(2, "mm"))+
    theme(legend.text = element_text(size = 12)) +
    theme(axis.title = element_text(size = 30)) +
    theme(axis.text.x = element_text(face="bold", size=21)) +
    theme(axis.text.y = element_text(face="bold", size=21)) +
    theme(axis.line = element_line(size = 1)) +
    theme(panel.grid.major = element_line(colour = "gray"))

  return(figure)
}


.technote_viz_figure_2 <- function(x){
  res <-  x %>% 
    dplyr::filter_at(vars("MSOrder"), any_vars(. == "Ms2")) %>% 
    dplyr::select_at(vars("ChargeState", "PrecursorMass", "filename")) %>% 
    dplyr::mutate("deconv" = round((PrecursorMass -1.00782)*ChargeState, 0)) %>% 
    dplyr::mutate_at(vars("ChargeState"), funs(factor(.)))
  
  figure <- ggplot(res, aes_string(x = "deconv", colour = "filename")) +
    geom_line(stat = "density") +
    #geom_density(aes(y= ..density.. )) +
    labs(title = "B ") +
    labs(x = "Neutral Mass [Da]", y = "Density") +
    labs(fill = "Charge State", colour = "Charge State") +
    scale_y_continuous(labels = scales::scientific)+
    scale_color_manual(values = color.vector) +
    coord_cartesian(xlim = c(min(res$deconv), 10000)) +
    theme_light() +
    theme(legend.position = "top") +
    theme(plot.title = element_text(hjust = -0.055))+
    theme(title = element_text(size = 28)) +
    theme(axis.title = element_text(size = 30)) +
    theme(axis.text.x = element_text(face="bold", size=21)) +
    theme(axis.text.y = element_text(face="bold", size=21)) +
    theme(axis.line = element_line(size = 1)) +
    theme(panel.grid.major = element_line(colour = "gray"))

  return(figure)}


.technote_viz_figure_3 <- function(x){
  res <- x %>% dplyr::filter_at(vars("MSOrder"), any_vars(. == "Ms2")) %>% 
    dplyr::select_at(vars("ChargeState", "PrecursorMass", "filename"))
  
  res$deconv <-  round((res$PrecursorMass -1.00782)* res$ChargeState, 0)
  
  res <- dplyr::mutate_at(res, vars("ChargeState"), funs(factor(.)))
  
  figure <- ggplot(res, aes_string(x = "ChargeState", y = "deconv", fill = "filename")) +
    geom_violin() +
    labs(title = "C ") +
    labs(x = "Charge State ", y = "Neutral mass [Da]") +
    labs(fill = "Charge State", colour = "Charge State") +
    scale_fill_manual(values = color.vector) +
    theme_light() +
    theme(legend.position = "top") +
    theme(title = element_text(size = 28)) +
    theme(axis.title = element_text(size = 30)) +
    theme(axis.text.x = element_text(face="bold", size=21)) +
    theme(axis.text.y = element_text(face="bold", size=21)) +
    theme(axis.line = element_line(size = 1)) +
    theme(panel.grid.major = element_line(colour = "gray"))

  return(figure)
}

# ----Load data----

#' WU163763
#' @description a data set generated from reading \url{https://fgcz-bfabric.uzh.ch/bfabric/userlab/show-workunit.html?id=163763}.
#' @return a \code{\link{data.frame}} fullfilling the \code{\link{is.rawDiag}} column naming criteria.
#' @export getWU163763
getWU163763 <- function(){
  data(WU163763)
  return(WU163763)
}

# ----Benchmark----
#' benchmar read.raw
#'
#' @param f 
#' @param maxncpu 
#' @param exe 
#' @param rdata 
#' @importFrom parallel mclapply
#' @return a nested list object.
#' @examples
#' \dontrun{
#' f <- f[grep("raw$",f<-list.files())]
#' .benchmark(f, maxncpu = 8, exe='~/bin/fgcz_raw.exe', mono=TRUE, rdata='/tmp/b2.RData')
#' }
.benchmark <- function(f, maxncpu = c(16, 32, 64), 
                       exe = "~/RiderProjects/fgcz-raw/bin/Debug/fgcz_raw.exe", 
                       rdata = tempfile(fileext = ".RData")){
  if(require(parallel)){
    benchmark.rawDiag  <- lapply(maxncpu, 
                                 function(ncpu){
                                   ostart <- Sys.time()
                                   r <- do.call('rbind',  
                                                mclapply(f, function(file){
                                                  
                                                  start <- Sys.time()
                                                  
                                                  S <- read.raw(file, mono = TRUE, exe = exe); 
                                                  
                                                  end <- Sys.time()
                                                  
                                                  rv <- data.frame(file = file, 
                                                                   runtime = end - start,
                                                                   nrow  =nrow(S))
                                                  rv
                                                }, mc.cores = ncpu))
                                   
                                   r$ncpu <- ncpu
                                   r$start.time <-  ostart
                                   r$end.time <- Sys.time()
                                   
                                   return(r)
                                 })
    
    message(paste("writting result to", rdata, "..."))
    save(benchmark.rawDiag, file=rdata) 
    
    return(benchmark.rawDiag)
  }
}

.benchmark.mzR <- function(f, maxncpu = c(16, 32, 64), 
                       rdata = tempfile(fileext = ".RData")){
  if(require(parallel)){
    benchmark.rawDiag  <- lapply(maxncpu, 
                                 function(ncpu){
                                   ostart <- Sys.time()
                                   r <- do.call('rbind',  
                                                mclapply(f, function(file){
                                                  
                                                  start <- Sys.time()
                                                  
						  S <- as.rawDiag.mzR(openMSfile(file))
                                                  
                                                  end <- Sys.time()
                                                  
                                                  rv <- data.frame(file = file, 
                                                                   runtime = end - start,
                                                                   nrow  =nrow(S))
                                                  rv
                                                }, mc.cores = ncpu))
                                   
                                   r$ncpu <- ncpu
                                   r$start.time <-  ostart
                                   r$end.time <- Sys.time()
                                   
                                   return(r)
                                 })
    
    message(paste("writting result to", rdata, "..."))
    save(benchmark.rawDiag, file=rdata) 
    
    return(benchmark.rawDiag)
  }
}

# ----Superfluously?----

#TODO: use position dodge for step plot? -> plot obsolete!
.charge_state_quantiles <- function(x){
  res <- x %>% 
    dplyr::filter(MSOrder == "Ms2") %>% 
    dplyr::select(ChargeState, filename) %>% 
    dplyr::group_by(filename) %>% 
    dplyr::mutate_at(vars(ChargeState), funs(steps = cume_dist(.))) %>% 
    dplyr::group_by(ChargeState) %>% 
    dplyr::do(unique(.))
  # zero <- data.frame(ChargeState = 0, steps = 0)
  #res <- dplyr::bind_rows(zero, res)
  xbreaks <- res$ChargeState
  
  figure <- ggplot(res, aes_string(x = "ChargeState", y = "steps")) +
    geom_step(colour = "deepskyblue2", size = 1.3, alpha = 0.3) +
    geom_point(shape = 18, colour = "black") +
    geom_hline(yintercept = 0.95, colour = "red3", linetype = "longdash") +
    geom_text(aes_string(label = signif("steps", 3)), vjust = -0.5, hjust = 1) +
    scale_y_continuous(breaks = scales::pretty_breaks(10)) +
    scale_x_continuous(breaks = xbreaks) +
    labs(title = "Cumulative charge state percentage plot") +
    labs(x = "Charge States", y = "Percent [%]") +
    theme_light() +
    theme(legend.position = "top")
  return(figure)
}

.ms2_frequency <- function(x){
  NoMS2 <- x %>% 
    dplyr::filter(MSOrder == "Ms") %>% 
    dplyr::count() %>% 
    dplyr::rename(Counts = n) %>% 
    dplyr::pull()
  res <- x %>% 
    dplyr::filter(MSOrder == "Ms2") %>% 
    dplyr::count(MasterScanNumber) %>% 
    dplyr::count(n) %>% 
    dplyr::rename(NumberOfMS2Scans = n, Counts = nn) %>% 
    rbind(c(0, NoMS2 -sum(.$Counts))) %>% 
    dplyr::arrange(NumberOfMS2Scans) %>% 
    dplyr::mutate(percentage = signif((100/sum(Counts) * Counts), 2))
  xbreaks <- res$NumberOfMS2Scans
  if(max(res$NumberOfMS2Scans >=25)){
    res <- res %>% 
      dplyr::bind_cols(parts = unlist(cut(.$NumberOfMS2Scans, breaks = c(-1,25,50,75,100))))
    levels(res$parts) <- list("1-25" = levels(res$parts)[1], 
                              "26-50" = levels(res$parts)[2], 
                              "51-75" = levels(res$parts)[3], 
                              "76-100" = levels(res$parts)[4]) 
    res <- res %>% 
      dplyr::select(NumberOfMS2Scans, Counts, percentage) %>% 
      dplyr::mutate(parts = as.factor("ALL")) %>% 
      dplyr::bind_rows(res) %>% 
      dplyr::mutate(x_min = case_when(parts == "ALL" ~ 1,
                                      parts == "1-25" ~ 1,
                                      parts == "26-50" ~ 26,
                                      parts == "51-75" ~ 51,
                                      parts == "76-100" ~ 76)) %>% 
      dplyr::mutate(x_max = case_when(parts == "ALL" ~ max(res$NumberOfMS2Scans),
                                      parts == "1-25" ~ 25,
                                      parts == "26-50" ~ 50,
                                      parts == "51-75" ~ 75,
                                      parts == "76-100" ~ 100))
    res$parts <- factor(res$parts, levels = c("ALL", "1-25", "26-50", "51-75", "76-100"))  
    
    figure <- ggplot(res, aes_string(x = "NumberOfMS2Scans", y = "percentage")) + 
      geom_bar(stat = "identity", fill = "deepskyblue2") + 
      geom_text(aes_string(label = "Counts"), vjust=-0.3, size=3.5) + 
      facet_wrap(~parts, scales = "free", nrow = 5, ncol = 1) +
      ylim(0, max(res$percentage+5)) + 
      geom_blank(aes_string(x = "x_min")) +
      geom_blank(aes_string(x = "x_max"))
  } else {
    figure <- ggplot(res, aes_string(x = "NumberOfMS2Scans", y = "percentage")) +
      geom_bar(stat = "identity", fill = "deepskyblue2") +
      geom_text(aes_string(label = "Counts"), vjust=-0.3, size=3.5)
  }
  figure +
    scale_x_continuous(breaks = xbreaks) +
    labs(title = "Cycle load plot") +
    labs(subtitle = "Plotting the number of MS2 scans associated with each MS1 scan") +
    labs(x = "Number of MS2 associated with an MS1 scan", y = "Percentage [%]") +
    theme_light()
}

.ms_data_points <- function(x){
  binSize <- 15
  binNumber <- ceiling(nrow(x) / binSize)
  binVector <- rep(1:binNumber, each = binSize)
  res <- x %>% 
    dplyr::filter(MSOrder == "Ms") %>% 
    dplyr::select(StartTime) %>% 
    dplyr::mutate(CycleTime = (dplyr::lead(StartTime) - StartTime)*60) %>% 
    dplyr::filter(!is.na(CycleTime)) %>% 
    dplyr::mutate("10sec" = floor(10/CycleTime)) %>% 
    dplyr::mutate("20sec" = floor(20/CycleTime)) %>% 
    dplyr::mutate("30sec" = floor(30/CycleTime)) %>% 
    dplyr::select(StartTime, "10sec", "20sec", "30sec") %>% 
    dplyr::mutate(Bins = binVector[1:nrow(.)]) %>% 
    dplyr::group_by(Bins) %>% 
    dplyr::summarise_all(funs(mean)) %>% 
    dplyr::mutate_at(c("10sec","20sec","30sec"), funs(floor)) %>% 
    tidyr::gather("PeakWidthAtBaseline", "Points", 3:5)
  
  figure <- ggplot(res, aes_string(x = "StartTime", y = "Points", colour = "PeakWidthAtBaseline")) +
    geom_point(size = 0.3) +
    scale_colour_manual(values = c("red3", "darkorchid3", "deepskyblue3")) +
    scale_y_continuous(breaks = scales::pretty_breaks((n = 20))) + 
    scale_x_continuous(breaks = scales::pretty_breaks((n = 8))) +
    labs(title ="Point Over Chromatographic Peak") + 
    labs(subtitle = "Plotting the number of Ms data points over different preselected chromatographic peak widths") +
    labs(x = "Retention Time", y = "Points over Peak") +
    labs(colour = "Peak width") +
    theme_light()
  return(figure)
}



.overview_ <- function(prefix="primer"){
  
  data(WU163763)
  WU <- WU163763
  WU <- WU[WU$filename %in% unique(WU$filename)[1:2], ]
  
  lapply(ls("package:rawDiag")[grepl("Plot", ls("package:rawDiag"))], 
           function(fn){
             lapply(c('trellis', 'violin', 'overlay'), function(a){
               pngFileName <- paste(paste(prefix, fn, a, sep='-'), "png", sep='.')
               
               message(pngFileName)
               
               if (!file.exists(pngFileName)){
                 gp <- get(fn)(WU, a)  +
                   theme(legend.position = 'none') + 
                   theme(axis.line=element_blank(),
                         axis.text.x=element_blank(),
                         axis.text.y=element_blank(),
                         axis.ticks=element_blank(),
                         axis.title.x=element_blank(),
                         axis.title.y=element_blank(),
                         legend.position="none",
                         panel.background=element_blank(),
                         panel.border=element_blank(),
                         panel.grid.major=element_blank(),
                         panel.grid.minor=element_blank(),
                         plot.background=element_blank()) +
                   theme(plot.title = element_blank()) +
                   theme(plot.subtitle = element_blank()) +
                   theme(strip.background = element_blank()) +
                   theme(strip.text = element_blank())
                 if (!is.null(gp)){
                   png(pngFileName, 240, 240)
                   print(gp)
                   dev.off()}
               }
             })}
           
  )
  
}


#labs.title=element_blank(),
#labs(title = "Precursor mass to charge frequency plot ") +
#  labs(subtitle = "Plotting frequency of precursor masses for each charge state") +

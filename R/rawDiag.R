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
#' @aliases rawDiag
#' @return a boolean
#' @import tidyverse
#' @author Christian Panse <cp@fgcz.ethz.ch>
#' @importFrom stats na.omit quantile
#' @importFrom magrittr %>%
#' @importFrom utils data packageVersion
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
#' @author Christian Panse <cp@fgcz.ethz.ch>, 2018
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
#' @aliases as.rawDiag.mzR 
#' @return an \code{\link{rawDiag}} object
#' @author Christian Panse <cp@fgcz.ethz.ch>, Witold E.Wolski <wew@fgcz.ethz.ch>, 2018
#' @export as.rawDiag.mzR
#' @examples
#' if(require(mzR)){
#'    mzML <- "04_S174020_5000_5010.mzML"
#'    mzML <- file.path(path.package(package = "rawDiag"), "extdata", mzML)
#'    system.time(RAW <- rawDiag:::as.rawDiag.mzR(openMSfile(mzML)))
#'    summary(RAW)
#'    RAW$scanNumber
#' }
#' 
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
#' @param filename filename if the tdf file
#' @author Witold E. Wolski, Christian Panse <wew,cp@fgcz.ethz.ch>, 2018
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
#' @note this is work in progress
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

#' Extracts XICs of a given mass vector
#'
#' @param rawfile the file name 
#' @param masses a vector of masses 
#' @param tol tolerance in ppm
#' @param mono if the mono enviroment should be used. 
#' @param exe the exe file user by mono
#'
#' @return list of XIC objects
#' @export readXICs 
#' @examples
#' # Example 1: extract iRT peptides
#' iRTpeptide <- c("LGGNEQVTR", "YILAGVENSK", "GTFIIDPGGVIR", "GTFIIDPAAVIR",
#' "GAGSSEPVTGLDAK", "TPVISGGPYEYR", "VEATFGVDESNAK",
#' "TPVITGAPYEYR", "DGLDAASYYAPVR", "ADVTPADFSEWSK",
#' "LFLQFGAQGSPFLK")
#' 
#' library(protViz)
#' # 2Hplus 
#' (mZ <- (parentIonMass(iRTpeptide) + 1.008) / 2)
#' 
#' \dontrun{
#' rawfile <- "/home/cp/Downloads/20180220_14_autoQC01.raw"
#'  X <-readXICs(rawfile, masses=mZ)
#' }
#' 
#' 
#' (rawfile <- file.path(path.package(package = 'rawDiag'), 'extdata', 'sample.raw'))
#' RAW <- read.raw(rawfile)
#' 
#' # not meaning full but proof-of-concept
#' X <-readXICs(rawfile, masses=unique(RAW$PrecursorMass), tol=1000)
#' plot(X)
#' 
readXICs <- function(rawfile, 
                      masses,
                      tol = 10,
                      mono = if(Sys.info()['sysname'] %in% c("Darwin", "Linux")) TRUE else FALSE,
                      exe = file.path(path.package(package = "rawDiag"), "exec", "fgcz_raw.exe")){
  
  # TODO(cp): replace asap we have an R .Net binding similar as Rcpp
  # the current solution writting and reading from a file is pain-of-the-art
  tfi <- tempfile()
  tfo <- tempfile()
  tfstdout <- tempfile()
  
  cat(masses, file=tfi, sep="\n")
  
  cmd <- exe
  
  if (mono){
    rvs <- system2("mono", args = c(shQuote(exe), shQuote(rawfile), "xic", shQuote(tfi), tol, shQuote(tfo)))
  }else{
    rvs <- system2(exe, args = c( shQuote(rawfile), "xic", shQuote(tfi), tol, shQuote(tfo)))
  }
  
  source(tfo)
  unlink(c(tfi, tfo, tfstdout))
  
  rv <- lapply(e$XIC, 
               function(x){
                 class(x) <- c(class(x), 'XIC'); 
                 x$filename <- basename(rawfile); 
                 x})
  
  class(rv) <- c(class(rv), 'XICs')
  return(rv)
}


is.XIC <- function(x){
  if(length(x$times) > 0 && length(x$times) == length(x$intensities)){TRUE}else{FALSE}
}

#' plot extracted ion chromatogram
#' @description the defaukt plot function for an XIC object.
#'
#' @param x an XIC S3 class object
#' @param y will be ignored
#' @param method plot or ggplot 
#' @param ...  passed to the plot function
#'
#' @return plot or ggplot object
#' @export plot.XIC
plot.XIC <- function(x, y, method='plot', zoom=0, ...){
  if(is.XIC(x)){
    t.max <- x$times[x$intensities == max(x$intensities, na.rm=TRUE)][1]
    if(zoom == 0){
      plot(x$times, 
           x$intensities,
           xlab='retention time',
           ylab='intensity',
           type='h',
           
           main=x$filename,
           ...)
    }else{
      plot(x$times, 
           x$intensities,
           xlab='retention time',
           ylab='intensity',
           type='h',
           xlim=c(t.max-zoom,t.max+zoom),
           main=x$filename,
           ...)
      abline(v=t.max, col=rgb(0.5, 0.5, 0.5, alpha = 0.25), lwd=4)
    }
    
    #axis(3, t.max, round(t.max,2))
   
    legend("topleft",
           c(paste("mZ:", x$mass),
             paste("number of peaks:", length(x$times)),
             paste("t max:", t.max)
           )
    )
  }
} 

#' plot extracted ion chromatograms
#'
#' @param x a list of XIC objects
#' @param y will be ignroed
#' @param ... passed to \codeP{plot.XIC}
#'
#' @return 
#' @export plot.XICs
#' @examples
#' # Example 1: extract iRT peptides
#' iRTpeptide <- c("LGGNEQVTR", "YILAGVENSK", "GTFIIDPGGVIR", "GTFIIDPAAVIR",
#' "GAGSSEPVTGLDAK", "TPVISGGPYEYR", "VEATFGVDESNAK",
#' "TPVITGAPYEYR", "DGLDAASYYAPVR", "ADVTPADFSEWSK",
#' "LFLQFGAQGSPFLK")
#' 
#' library(protViz)
#' # 2Hplus 
#' (mZ <- (parentIonMass(iRTpeptide) + 1.008) / 2)
#' 
#' \dontrun{
#'  rawfile <- "/home/cp/Downloads/20180220_14_autoQC01.raw"
#'  X <-readXICs(rawfile, masses=mZ)
#'  plot(X, method='ggplot') + facet_wrap(~mass)
#'  plot(X, method='ggplot') + facet_wrap(~mass, scales = "free_x")
#'  
#' }


plot.XICs <- function(x, y, method='ggplot', ...){
  if(length(x) > 0){
    if (method == 'ggplot'){
      df <- do.call('rbind', lapply(x, function(xx){
        
        if (is.XIC(xx)){
          rv <- data.frame(times=xx$times,
                           intensities=xx$intensities)
          
          rv$filename <- xx$filename
          rv$mass <- as.factor(xx$mass)
          rv}else{NULL}
      }
      ))
      
      
      gp <- ggplot(df, aes_string(x = "times", y = "intensities")) +
        #geom_segment() +
        geom_line(stat='identity', size = 1, aes_string(group = "mass", colour = "mass")) +
        #scale_x_continuous(breaks = scales::pretty_breaks(8)) +
        #scale_y_continuous(breaks = scales::pretty_breaks(8)) +
        labs(title = "XIC plot") +
        labs(subtitle = "Plotting XIC intensity against retention time") +
        labs(x = "Retention Time [min]", y = "Intensity Counts [arb. unit]")
      return(gp)
    }else{
      lapply(x, plot.XIC, ...)
    }
  }
} 



#' read scan of scanids
#'
#' @param rawfile 
#' @param scans 
#' @param mono 
#' @param exe 
#'
#' @return
#' @export readScans
#'
#' @examples
#'  (rawfile <- file.path(path.package(package = 'rawDiag'), 'extdata', 'sample.raw'))
#'  S <- readScans(rawfile, 1:9)
#'  plot(S[[1]])
#'  op <- par(mfrow=c(3, 3))
#'  lapply(S, function(x){plot(x, sub=x$scanType)})
#' 
readScans <- function(rawfile, 
                       scans,  
  mono = if(Sys.info()['sysname'] %in% c("Darwin", "Linux")) TRUE else FALSE,
  exe = file.path(path.package(package = "rawDiag"), "exec", "fgcz_raw.exe")){
  
  # TODO(cp): replace asap we have an R .Net binding similar as Rcpp
  # the current solution writting and reading from a file is pain-of-the-art
  tfi <- tempfile()
  tfo <- tempfile()
  tfstdout <- tempfile()
  
  cat(scans, file = tfi, sep="\n")
  
  cmd <- exe
  
  if (mono){
    rvs <- system2("mono", args = c(shQuote(exe), shQuote(rawfile), "scans", shQuote(tfi), shQuote(tfo)))
  }else{
    rvs <- system2(exe, args = c( shQuote(rawfile), "scans", shQuote(tfi), shQuote(tfo)))
  }
  source(tfo)
  unlink(c(tfi, tfo, tfstdout))
  
  return(lapply(e$PeakList, function(x){class(x) <- c(class(x), 'peaklist'); x}))
}

#' plot a peaklist
#'
#' @param x a peaklist 
#' @param y 
#' @param ... 
#'
#' @return a nested list
#' @export plot.peaklist
#'
#' @examples
#' (rawfile <- file.path(path.package(package = 'rawDiag'), 'extdata', 'sample.raw'))
#' S <- readScans(rawfile, 1:10)
#' plot(S[[1]])
#' 
plot.peaklist <- function(x, y, ...){
  plot(x$mZ, x$intensity, type = 'h', main = x$title, xlab = 'm/Z', ylab = 'intensity', ...)
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
#' @param system2_call calles system2 if TRUE; otherwise it uses a pipe.
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
#' (rawfile <- file.path(path.package(package = 'rawDiag'), 'extdata', 'sample.raw'))
#' system.time(RAW <- read.raw(file = rawfile))
#' dim(RAW)
#' summary(RAW)
#' PlotScanFrequency(RAW)
#' 
#' # read all dimensions
#' dim(RAW)
#' RAW <- read.raw(file = rawfile, rawDiag = FALSE)
#' dim(RAW)
#' \dontrun{
#' library(parallel)
#' library(rawDiag)
#'
#' # consider all raw files of your working dir
#' rawFileNames <- list.files()[grep("raw$", list.files())]
#' 
#' # read all the meta data using 4 cores
#' RAW <- mclapply(rawFileNames, read.raw, mc.cores=4)
#' # as alternative  \code{lapply} instread of \code{mclapply}
#'
#' # concatenate the list data.frames into one single one
#' RAW <- plyr::rbind.fill(RAW)
#' 
#' # have fun
#' PlotMassDistribution(RAW)
#'  }                                       
#' 
read.raw <- function(file, mono = if(Sys.info()['sysname'] %in% c("Darwin", "Linux")) TRUE else FALSE, 
                     exe = file.path(path.package(package = "rawDiag"), "exec", "fgcz_raw.exe"),  
                     mono_path = "",
                     rawDiag = TRUE,
                     argv = "qc",
                     system2_call = if(Sys.info()['sysname'] == "Windows") TRUE else FALSE,
                     method = "thermo",
                     ssh = FALSE){
  
  rv <- NULL
  if (mono_path != ''){
    message(Sys.setenv(MONO_PATH = mono_path))
  }
  
  if (method == "thermo" && ssh){
    return(.read.thermo.raw.ssh(file))
  }
  else if (method == "thermo"){
    
    stopifnot(file.exists(exe))
    stopifnot(file.exists(file))
    
    # message(paste("start", Sys.time(), sep = ":"))
    
    
    if(system2_call){
      tf <- tempfile(fileext = 'tsv')
      tfstdout <- tempfile()
      
      message(paste("system2 is writting to tempfile ", tf, "..."))
      
      if (mono){
        rvs <- system2("mono", args = c(exe, shQuote(file), "qc", shQuote(tf)), stdout = tfstdout)
      }else{
        rvs <- system2(exe, args = c(shQuote(file), "qc", shQuote(tf)), stdout = tfstdout)
      }
      if (rvs == 0){
        rv <- read.csv(tf,  sep = "\t",   stringsAsFactors = FALSE, header = TRUE)
        message(paste("unlinking", tf, "..."))
        unlink(tf)
        unlink(tfstdout)
      }
    }else{
      cmd <- paste(exe, file, argv)
      
      if (mono)  cmd <- paste("mono", cmd)
      
      
      message(paste ("executing", cmd, "..."))
      rv <- read.csv(pipe(cmd), 
                     sep='\t', stringsAsFactors = FALSE, header = TRUE)
      
    }
  }
  
  if (rawDiag) rv <- as.rawDiag(rv)
  class(rv) <- c(class(rv), 'rawDiag')
  rv
}

#' rawDiag Summaries
#'
#' @param ... additional arguments affecting the summary produced.
#' @param object a \code{\link{data.frame}} fullfilling the \code{\link{is.rawDiag}} column naming criteria.
#' 
#' @return a \code{data.frame}
#' @method summary rawDiag
#' @export summary.rawDiag
summary.rawDiag <- function(object, ...){
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

# Calculate Master Scan Number
# calculates the MS1 master scan number of an MS2 scan 
# and populates the MasterScanNumber with it
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

.map.type <- function(x){
  res <- x %>% 
    dplyr::mutate(Type = dplyr::case_when(grepl("FTMS [[:punct:]] c NSI Full ms", .$ScanType) == "TRUE" ~ "FT_Full_ms_c",
                                          grepl("FTMS [[:punct:]] p NSI Full ms", .$ScanType) == "TRUE" ~ "FT_Full_ms_p",
                                          grepl("FTMS [[:punct:]] c NSI Full lock ms", .$ScanType) == "TRUE" ~ "FT_Full_ms_c",
                                          grepl("FTMS [[:punct:]] p NSI Full lock ms", .$ScanType) == "TRUE" ~ "FT_Full_ms_p",
                                          grepl("FTMS [[:punct:]] c NSI d Full ms2", .$ScanType) == "TRUE" ~ "FT_d_Full_ms2_c",
                                          grepl("FTMS [[:punct:]] p NSI d Full ms2", .$ScanType) == "TRUE" ~ "FT_d_Full_ms2_p",
                                          grepl("FTMS [[:punct:]] c NSI SIM ms", .$ScanType) == "TRUE" ~ "FT_SIM_ms_c",
                                          grepl("FTMS [[:punct:]] p NSI SIM ms", .$ScanType) == "TRUE" ~ "FT_SIM_ms_p",
                                          grepl("FTMS [[:punct:]] p NSI SIM msx ms", .$ScanType) == "TRUE" ~ "FT_msxSIM_ms_p",
                                          grepl("FTMS [[:punct:]] c NSI SIM msx ms", .$ScanType) == "TRUE" ~ "FT_msxSIM_ms_c",
                                          grepl("FTMS [[:punct:]] c NSI Full ms2", .$ScanType) == "TRUE" ~ "FT_Full_ms2_c",
                                          grepl("FTMS [[:punct:]] p NSI Full ms2", .$ScanType) == "TRUE" ~ "FT_Full_ms2_p",
                                          grepl("ITMS [[:punct:]] c NSI r d Full ms2", .$ScanType) == "TRUE" ~ "IT_Full_ms2_c",
                                          grepl("ITMS [[:punct:]] p NSI r d Full ms2", .$ScanType) == "TRUE" ~ "IT_Full_ms2_p"
                                          )
    )
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
  df <- x %>% 
    dplyr::filter_at(vars("MSOrder"), any_vars( . == "Ms")) %>% 
    dplyr::select_at(vars("StartTime", "TIC", "BasePeakIntensity", "filename")) %>% 
    dplyr::rename_at(vars("BasePeakIntensity"), funs(as.character("Base_Peak"))) %>% 
    tidyr::gather(key = "Type", value = "Intensity", c("TIC", "Base_Peak"))
  df$Type <- factor(df$Type, levels = c("TIC", "Base_Peak"))
  
  if (method == 'trellis'){
    
    figure <- ggplot(df,aes_string(x = "StartTime", y = "Intensity")) +
      geom_line(size = 0.3) +
      facet_wrap(filename ~ Type, scales = "free", ncol = 2) +
      scale_x_continuous(breaks = scales::pretty_breaks(8)) +
      scale_y_continuous(breaks = scales::pretty_breaks(8)) +
      labs(title = "TIC and Base-Peak plot") +
      labs(subtitle = "Plotting TIC intensity and base peak intensity against retention time") +
      labs(x = "Retention Time [min]", y = "Intensity Counts [arb. unit]") +
      theme_light() 
    return(figure)
    
  }else if(method =='violin'){
    
    figure <- ggplot(df, aes_string(x = "filename", y = "Intensity")) + 
      geom_violin() +
      facet_grid(Type~., scales = "free") +
      #stat_summary(fun.y = mean , geom = "point", colour = "red") +
      scale_y_continuous(trans = scales::log10_trans())+
      labs(title = "TIC and Base-Peak plot") +
      labs(subtitle = "Plotting the TIC and base peak density for all mass spectrometry runs") +
      labs(x = "Filename", y = "Intensity Counts [arb. unit]") +
      theme_light() +
      theme(axis.text.x = element_text(angle = 90))
    return(figure)
    
  }else if (method  == 'overlay'){
    
    figure <- ggplot(df, aes_string(x = "StartTime", y = "Intensity", colour = "filename")) +
      geom_line(size = 0.3) +
      facet_grid(Type~., scales = "free") +
      scale_x_continuous(breaks = scales::pretty_breaks(8)) +
      scale_y_continuous(breaks = scales::pretty_breaks(8)) +
      labs(title = "TIC and Base-Peak plot") +
      labs(subtitle = "Plotting TIC intensity and base peak intensity against retention time") +
      labs(x = "Retention Time [min]", y = "Intensity Counts [arb. unit]") +
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
      scale_x_continuous(breaks = scales::pretty_breaks(8)) +
      scale_y_continuous(breaks = scales::pretty_breaks(8)) +
      labs(title = "Cycle time plot") +
      labs(subtitle = "Plotting the caclulated cycle time of each cycle vs retention time") +
      labs(x = "Retention Time [min]", y = "Cycle Time [sec]") +
      geom_hline(aes_string(yintercept = "quan", group = "filename"), colour = "red3", linetype = "longdash")+
      theme_light() + 
      facet_grid(filename~., scales = "free")
    return(figure)
    
  }else if (method == 'violin'){
    df <- calc.cycle.time(x=x)
    dots <- df %>%
      dplyr::select_at(vars("filename", "quan")) %>% 
      distinct()
    
    figure <- ggplot(df, aes_string(x = "filename", y = "CycleTime")) + 
      geom_violin()  +
      #stat_summary(fun.y = mean , geom = "point", colour = "red") +
      #geom_point(data = dots, aes_string(x = "filename", y = "quan"), colour = "black")+
      scale_y_continuous(breaks = scales::pretty_breaks(8)) +
      labs(title = "Cycle time plot") +
      labs(subtitle = "Plotting the cycle time density of all mass spectrometry runs") +
      labs(x = "Filename", y = "Cycle Time [sec]") +
      theme_light() +
      #theme(axis.text.x=element_blank(), legend.position = "top")
      theme(axis.text.x = element_text(angle = 90))
    return(figure)
    
  } else if(method == 'overlay'){
    df <- calc.cycle.time(x=x)
    
    figure <- ggplot(df, aes_string(x = "StartTime", y = "CycleTime", colour = "filename")) + 
      geom_point(size = 0.5) +
      geom_line(aes_string(group = "filename", colour = "filename"), stat = "smooth", method = "gam", formula = y ~ s(x, bs= "cs"), se = FALSE) +
      scale_x_continuous(breaks = scales::pretty_breaks(8)) +
      scale_y_continuous(breaks = scales::pretty_breaks(8)) +
      labs(title = "Cycle time plot") +
      labs(subtitle = "Plotting the caclulated cycle time of each cycle vs retention time") +
      labs(x = "Retention Time [min]", y = "Cycle Time [sec]") +
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
      geom_line(stat = "smooth", method = "gam", formula = y ~ s(x, bs= "cs"),
                size = 1.1, alpha = 0.6, colour = "cornflowerblue", se = FALSE) +
      scale_x_continuous(breaks = scales::pretty_breaks(8)) +
      scale_y_continuous(breaks = scales::pretty_breaks(8)) +
      labs(title = "Retention Time to m/z correlation plot") +
      labs(subtitle = "Plotting retention time against m/z value of all selected precursors") +
      labs(x = "Retention Time", y = "Presursor m/z value") +
      theme_light()
    return(figure)
    
  }else if (method == 'violin'){
    df <- x %>% 
      dplyr::filter(MSOrder == "Ms2")
    
    figure <- ggplot(df, aes_string(x = "filename", y = "PrecursorMass")) + 
      geom_violin() +
      #stat_summary(fun.y = max , geom = "point", colour = "red") +
      scale_y_continuous(breaks = scales::pretty_breaks(8)) +
      labs(title = "Retention Time to m/z correlation plot") +
      labs(subtitle = "Plotting the precursor m/z value density of all mass spectrometry runs") +
      labs(x = "Filename", y = "Presursor m/z value [Da]") +
      theme_light() +
      theme(axis.text.x = element_text(angle = 90))
    return(figure)
    
  }else if (method == 'overlay'){
    df <- x %>% 
      dplyr::filter(MSOrder == "Ms2")
    
    figure <- ggplot(df, aes_string(x = "StartTime", y = "PrecursorMass", colour = "filename")) + 
      geom_point(size = 0.5, alpha = 0.3) +
      geom_line(stat = "smooth",
                method = "gam",
                formula = y ~ s(x, bs= "cs"),
                size = 1.1,
                se = FALSE) +
      scale_x_continuous(breaks = scales::pretty_breaks(8)) +
      scale_y_continuous(breaks = scales::pretty_breaks(8)) +
      labs(title = "Retention Time to m/z correlation plot") +
      labs(subtitle = "Plotting retention time against m/z value of all selected precursors") +
      labs(x = "Retention Time [min]", y = "Presursor m/z value [Da]") +
      theme_light() +
      theme(legend.position="top")
    return(figure)
    
  }else{NULL}
}

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
  res <- x %>% dplyr::filter_at(vars("MSOrder"), any_vars(. == "Ms2")) %>% 
    dplyr::select_at(vars("ChargeState", "PrecursorMass", "filename"))
  res$deconv <-  round((res$PrecursorMass -1.00782) * res$ChargeState, 0)
  res <- dplyr::mutate_at(res, vars("ChargeState"), funs(factor(.)))
  
  if (method == 'trellis'){

    figure <- ggplot(res, aes_string(x = "deconv", fill = "ChargeState", colour = "ChargeState")) +
      geom_histogram(binwidth = 100, alpha = .3, position = "identity") +
      labs(title = "Precursor mass to charge frequency plot ") +
      labs(subtitle = "Plotting chrage state resolved frequency of precursor masses") +
      labs(x = "Precursor neutral mass [Da]", y = "Frequency [counts]") +
      labs(fill = "Charge State", colour = "Charge State") +
      scale_x_continuous(breaks = scales::pretty_breaks(8)) +
      coord_cartesian(xlim = c(min(res$deconv), 10000)) +
      theme_light() + 
      facet_wrap(~filename)
    return(figure)
    
  }else if (method == 'violin'){ #mz.frequency.violin
    
    figure <- ggplot(res, aes_string(x = "ChargeState", y = "deconv", fill = "filename")) +
      geom_violin() +
      labs(title = "Precursor mass to charge density plot ") +
      labs(subtitle = "Plotting the charge state resolved precursor masse density for each mass spectrometry run") +
      labs(x = "Charge State ", y = "Neutral Mass [Da]") +
      theme_light() +
      theme(legend.position = "top")
    return(figure)
    
  }else if (method == 'overlay'){ #mz.frequency.overlay
    
    figure <- ggplot(res, aes_string(x = "deconv", colour = "filename")) +
      geom_line(stat = "density") +
      labs(title = "Precursor mass density plot ") +
      labs(subtitle = "Plotting the precursor masse density for each mass spectrometry run") +
      labs(x = "Precursor mass [neutral mass]", y = "Density") +
      scale_x_continuous(breaks = scales::pretty_breaks(12)) +
      coord_cartesian(xlim = c(min(res$deconv), 10000)) +
      theme_light() +
      theme(legend.position = "top")
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
      geom_bar(stat = "identity", fill = "cornflowerblue") +
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
    
    figure <- ggplot(res, aes_string(x = "filename", y = "ChargeState")) + #, fill = "filename")) +
      #geom_boxplot(width=.05, outlier.colour = "black", position = position_dodge())+ 
      #geom_violin(position = position_dodge())+
      geom_violin() +
      scale_y_continuous(breaks = scales::pretty_breaks(8)) +
      labs(title = "Charge state plot") +
      labs(subtitle = "Plotting the precursor charge state density for each mass spectrometry run") +
      labs(x = "Filename", y = "Charge State") +
      theme_light() +
      theme(axis.text.x = element_text(angle = 90))
      #theme(axis.text.x=element_blank(), legend.position = "top")
    return(figure)
    
  }else{NULL}
}



#' scan event
#' @description plots time for each scan event
#' @param x a \code{\link{data.frame}} fullfilling the \code{\link{is.rawDiag}} column naming criteria.
#' @param method a character 'trellis', 'violin' or 'overlay'.
#' @aliases scan.time.overlay scan.time.violin
#' @return a ggplot object.
#' @export PlotScanTime
#' @import tidyr
PlotScanTime <- function(x, method='trellis'){
  res <- x %>% 
    dplyr::mutate(ElapsedScanTimesec = ElapsedScanTimesec * 1000) %>% 
    dplyr::select_at(vars("StartTime", "ScanType", "ElapsedScanTimesec", "filename", "MassAnalyzer", "MSOrder", "transient")) %>% 
    na.omit()
  
  res <- .map.type(res)
  
   if(method == 'trellis'){
    
    figure <- ggplot(res, aes_string(x = "StartTime", y = "ElapsedScanTimesec")) +
      geom_point(shape = ".") +
      facet_grid(filename ~ Type) +
      geom_line(stat = "smooth", method = "gam",
                formula = y~s(x), colour = "deepskyblue3", se = FALSE) +
      labs(title = "Scan time plot") +
      labs(subtitle = "Plotting the elapsed scan time for each individual scan") +
      labs(x = "Retentione Time [min]", y = "Elapsed Scan Time [ms]") +
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
      #stat_summary(fun.y = max , geom = "point", colour = "red") +
      scale_y_continuous(breaks = scales::pretty_breaks(8)) +
      labs(title = "Scan time plot") +
      labs(subtitle = "Plotting the retention time resolved elapsed scan time density for each mass spectrometry run") +
      labs(x = "Filename", y = "Elapsed Scan Time [ms]") +
      theme_light() +
      theme(axis.text.x = element_text(angle = 90))
    return(figure)
    
  }else if (method == 'overlay'){
    figure <- ggplot(res, aes_string(x = "StartTime", y = "ElapsedScanTimesec", colour = "filename")) +
      geom_point(size = 0.5) +
      geom_line(aes_string(group = "filename", colour = "filename"), stat = "smooth", method = "gam", formula = y ~ s(x, bs= "cs"), se = FALSE) +
      facet_grid(~ MSOrder + MassAnalyzer, scales = "free") +
      labs(title = "Scan time plot") +
      labs(subtitle = "Plotting the elapsed scan time for each individual scan") +
      labs(x = "Retentione Time [min]", y = "Elapsed Scan Time [ms]") +
      scale_x_continuous(breaks = scales::pretty_breaks((n = 8))) +
      scale_y_continuous(breaks = scales::pretty_breaks((n = 8))) +
      theme_light() +
      theme(legend.position="top")
    return(figure)
    
  }else{NULL}
}




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
      labs(title = "Injection time plot") +
      labs(subtitle = "Plotting injection time against retention time for MS and MSn level") +
      labs(x = "Retentione Time [min]", y = "Injection Time [ms]") +
      theme_light()
    return(figure)
  }
  else if (method == 'violin'){
    figure <- ggplot(x, aes_string(x = "filename", y = "IonInjectionTimems")) +
      geom_violin() +
      facet_grid(MSOrder~.) +
      #stat_summary(fun.y = max , geom = "point", colour = "red") +
      labs(title = "Injection time plot") +
      labs(subtitle = "Plotting retention time resolved injection time density for each mass spectrometry run") +
      labs(x = "Filename", y = "Injection Time [ms]") +
      theme_light() +
      theme(axis.text.x = element_text(angle = 90))
    return(figure)
    
  }else if(method == 'overlay'){
    figure <- ggplot(x, aes_string(x = "StartTime", y = "IonInjectionTimems", colour = "filename")) +
      geom_point(size = 0.5, alpha = 0.1) +
      geom_line(aes_string(group = "filename", colour = "filename"),
                stat = "smooth",
                method = "gam",
                formula = y ~ s(x, bs= "cs"),
                se = FALSE) +
      scale_y_continuous(breaks = scales::pretty_breaks((n = 8))) +
      scale_x_continuous(breaks = scales::pretty_breaks((n = 8))) +
      facet_grid(~ MSOrder, scales = "free") +
      labs(title = "Injection time plot") +
      labs(subtitle = "Plotting injection time against retention time for MS and MSn level") +
      labs(x = "Retentione Time [min]", y = "Injection Time [ms]") +
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
      geom_line(stat = "smooth", method= "gam", 
                formula = y ~ s(x, bs ="cs"), 
                colour = "deepskyblue3", se = FALSE) +
      labs(title = "Lock mass correction plot") +
      labs(subtitle = "Plotting lock mass correction value versus retention time") +
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
      labs(title = "Lock mass correction plot") +
      labs(subtitle = "Plotting the time resolved lock mass correction density for each mass spectrometr run") +
      labs(x = "Filename", y = "Lock Mass Correction [ppm]") +
      theme_light() +
      theme(axis.text.x = element_text(angle = 90))
    return(figure)
    
  }else if (method == 'overlay'){
    res <- x %>% 
      dplyr::filter(MSOrder == "Ms")
    figure <- ggplot(res, aes_string(x = "StartTime" , y = "LMCorrection", colour = "filename")) +
      geom_hline(yintercept = c(-5,5), colour = "red3", linetype = "longdash") +
      geom_line(size = 0.3) +
      geom_line(stat = "smooth", method= "gam", 
                formula = y ~ s(x, bs ="cs"), 
                colour = "deepskyblue3", se = FALSE) +
      labs(title = "Lock mass correction plot") +
      labs(subtitle = "Plotting lock mass correction value over time") +
      labs(x = "Retention Time [min]", y = "Lock Mass Correction [ppm]") +
      scale_x_continuous(breaks = scales::pretty_breaks(8)) +
      scale_y_continuous(breaks = scales::pretty_breaks(8), limits = c(-10,10)) +
      theme_light()
    return(figure)
    
  }else{NULL}
}

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
      labs(title = "Time resolved number of MS2 scans") +
      labs(subtitle = "Plotting the number of MS2 per MS1 scan versus retention time") +
      labs(x = "Retention Time [min]", y = "Number of MS2 per MS1 [counts]") +
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
      labs(title = "Time resolved number of MS2 scans") +
      labs(subtitle = "Plotting the duty cycle resolved MS2 density for each mass spectrometry run") +
      labs(x = "Retention Time [min]", y = "Number of MS2 per MS1 [counts]") +
      theme_light() +
      theme(axis.text.x = element_text(angle = 90))
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
      labs(title = "Time resolved number of MS2 scans") +
      labs(subtitle = "Plotting the number of MS2 per MS1 scan versus retention time") +
      labs(x = "Retention Time [min]", y = "Number of MS2 per MS1 [counts]") +
      theme_light() +
      theme(legend.position = "top")
    return(figure)
  
  }else{NULL}
}


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
      facet_grid(filename ~ Type) +
      scale_x_continuous(breaks = scales::pretty_breaks(8))+
      scale_y_continuous(breaks = scales::pretty_breaks(8))+
      labs(title = "MS2 Scan Frequency Plot") +
      labs(subtitle = "Plotting number of MS2 per second against retention time") +
      labs(x = "Retention Time [min]", y = "MS2 frequency [Hz]") +
      theme_light() 
    return(figure)  
    
  }else if (method == 'violin'){
    res <- x %>% 
      dplyr::ungroup() %>% 
      ScanFrequMovingOver(.) %>% 
      dplyr::ungroup() %>% 
      dplyr::filter_at(vars("Type"), any_vars(. == "ms2"))
    
    figure <- ggplot(res, aes_string(x = "filename", y = "Counts")) +
      geom_violin() +
      labs(title = "MS2 Scan Frequency Plot") +
      labs(subtitle = "Plotting the time resolved MS2 density for each mass spectrometry run") +
      labs(x = "Filename", y = "MS2 frequency [Hz]") +
      theme_light() +
      theme(axis.text.x = element_text(angle = 90))
    return(figure) 
    
  }else if (method =='overlay'){
    res <- x %>% 
      dplyr::ungroup() %>% 
      ScanFrequMovingOver(.)
    
    figure <- ggplot(res, aes_string(x = "Time", y = "Frequency", colour = "filename")) +
      facet_wrap(~Type) +
      geom_line() +
      scale_x_continuous(breaks = scales::pretty_breaks(8))+
      scale_y_continuous(breaks = scales::pretty_breaks(8))+
      labs(title = "MS2 Scan Frequency Plot") +
      labs(subtitle = "Plotting number of MS2 per second against retention time") +
      labs(x = "Retention Time [min]", y = "MS2 frequency [Hz]") +
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

# ----JPR Letter Figures----
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
  
  #stopifnot(require(lattice))
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


# ----ASMS2018 poster Figures----

#' PlotAll applies all available rawDiag plot functions to a given rawDiag object
#' @param x a \code{\link{data.frame}} fullfilling the \code{\link{is.rawDiag}} column naming criteria.
#' @param prefix filename prefix if plots a writting to a png file
#' @param savepng default is TRUE
#' @param resolution number of pixels in png file, default is 240
#' @importFrom grDevices dev.off png rgb
#' @author Christian Panse <cp@fgcz.ethz.ch>, 2018
#' @references Table 1 in \url{https://doi.org/10.1101/304485} 
#' @examples 
#' data(WU163763)
#' WU <- WU163763[WU163763$filename %in% unique(WU163763$filename)[1:2], ]
#' rv <- rawDiag:::PlotAll(x = WU, savepng = FALSE)
PlotAll <- function(x, prefix = "primer", savepng = TRUE, resolution = 240){
  WU <- x
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
                 if(savepng){
                 png(pngFileName, resolution, resolution)
                 print(gp)
                 dev.off()}else{print(gp)}}
             }
           })}
         
  )
}


.ASMS_benchmark_figure_1 <- function(){
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

#
.ASMS_benchmark_figure_2 <- function(){
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
# ----JPR TechNote Figures----
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
        subtitle='A') +
  theme_light() +
  theme(legend.position = "top") +
    theme(title = element_text(size = 24)) +
    theme(axis.title = element_text(size = 14)) +
    theme(legend.text = element_text(size = 9)) +
    theme(legend.title = element_text(size = 9, face = "bold"))
  
  gp
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
         subtitle='B') +
    theme_light() +
    theme(legend.position = "top") +
    theme(title = element_text(size = 24)) +
    theme(axis.title = element_text(size = 14)) +
    theme(legend.text = element_text(size = 9)) +
    theme(legend.title = element_text(size = 9, face = "bold"))
  
  gp
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
  
  res$deconv <-  round((res$PrecursorMass -1.00782) * res$ChargeState, 0)
  
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

# ----Benchmark----
#' benchmark of \code{read.raw} 
#' 
#' @param f set of files
#' @param maxncpu maximal number of cores used for the benchmark
#' @param exe the executable for reading the rawfiles
#' @param rdata file extrention of the tempfile
#' @description benchmarks the \code{read.raw} function's IO. This 
#' function has been used for generating \link{benchmark} data set.
#' @references \url{http://planetorbitrap.com/rawfilereader#.WyThDK3QPmE}
#' @importFrom parallel mclapply
#' @author Christian Panse <cp@fgcz.ethz.ch>, 2017, 2018
#' @return a nested list object.
#' @examples
#' \dontrun{
#'  f <- f[grep("raw$",f<-list.files())]
#'  benchmark_raw(f, maxncpu = 8, exe='~/bin/fgcz_raw.exe', mono=TRUE, rdata='/tmp/b2.RData')
#' }
benchmark_raw <- function(f, maxncpu = c(16, 32, 64), 
                       exe = file.path(path.package(package = "rawDiag"), "exec", "fgcz_raw.exe"), 
                       rdata = tempfile(fileext = ".RData")){
  if(TRUE){
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

benchmark_mzR <- function(f, maxncpu = c(16, 32, 64), 
                           rdata = tempfile(fileext = ".RData")){
  if(TRUE){
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

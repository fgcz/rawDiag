#R 

.writeMgfHeader <- function(filename){
  if(file.exists(filename)){
    message(paste("removing existing file", filename))
    unlink(filename)
    #stopifnot(file.exists(filename))
  }
  FILE <- file(filename, "a")
  writeLines(paste("#", R.version.string), FILE)
  writeLines(as.character(paste("# https://CRAN.R-project.org/package=protViz Version:", packageVersion('protViz'))), FILE)
  writeLines(paste("# ", date(), sep=''), FILE)
  close(FILE)
}

.writeMgfPeaklist <- function(x, filename, charge = TRUE){

  stopifnot(file.exists(filename))
  
  FILE <- file(filename, "a")

  if ("scanType" %in% names(x)){
    writeLines(paste("# ScanType=", x$scanType, sep=''), FILE)
  }

  writeLines("BEGIN IONS", FILE)
  writeLines(paste("TITLE=",as.character(x$title),sep=''), FILE)

  pepmass <- x$pepmass[1]

  tryCatch({
    if (is.na(pepmass) | pepmass <= 0)
      pepmass <- x$monoisotopicMz
  })

  writeLines(paste("PEPMASS=", pepmass, sep=''), FILE)

  if (charge){
    writeLines(paste("CHARGE=", as.character(x$charge), '+', sep=''), FILE)
  }

  writeLines(paste("SCANS=", as.character(x$scan), sep=''), FILE)
  writeLines(paste("RTINSECONDS=", as.character(round(as.numeric(x$rtinseconds))), sep=''), FILE)
  writeLines(as.character(paste(round(x$mZ, 5), round(x$intensity, 2), sep=' ')), FILE)
  writeLines("END IONS", FILE)
  writeLines("", FILE)
  close(FILE)
}


#' writes a Mascot Generic File (mgf)
#'
#' @param rawfilename a Thermo Fisher instrument file.
#' @param mgffilename a Mascot Generic File.
#' @param pattern ScanType regex pattern; default is \code{".+"}.
#' @param FUN a function applied to each MS2 scan.
#' @description The method read the metadata of a given raw filename, applies
#' to filter, and writes every MS2 scan into an mgf file. The technique allows
#' filtering specific scan as EThcD, HCD, or ETD. Potential use is a combination
#' with the comet search engine.
#' 
#' @author Christian Panse <cp@fgcz.ethz.ch>, 2012-2019
#' 
#' @return NULL
#' @seealso \url{https://CRAN.R-project.org/package=protViz}
#' @export mgf
#' @references \url{https://doi.org/10.1002/pmic.201300036}
#' @examples 
#' (rawfilename <- file.path(path.package(package = 'rawDiag'),
#'   'extdata', 'sample.raw'))
#' mgf(rawfilename, tempfile(fileext = '.mgf'))
#' 
#' # extract only top five higest MS2
#' 
#' top5 <- function(x){
#' if(x$mZ > 5){
#'   idx <- rev(order(x$intensity))[1:5]
#'   x$mZ <- x$mZ[idx]
#'   x$intensity <- x$intensity[idx]
#'   }
#'  x
#' }
#' 
#' pattern.EThcD.lowres <- "ITMS.+sa Full ms2.+@etd.+@hcd.+"
#' 
#' mgf(rawfilename, tempfile(fileext = '.EThcD.lowres.mgf'),
#' pattern=pattern.EThcD.lowres, FUN=top5)
#' 
#' 
#' pattern <- list(
#'   EThcD.lowres = "ITMS.+sa Full ms2.+@etd.+@hcd.+",
#'   ETciD.lowres = "ITMS.+sa Full ms2.+@etd.+@cid.+",
#'   CID = "^[^@]+@cid[^@]+$",
#'   HCD = "^[^@]+@hcd[^@]+$",
#'   pattern.EThcD.highres = "FTMS.+sa Full ms2.+@etd.+@hcd.+" 
#' )
#' 
#' lapply(names(pattern), function(x){
#'    mgf(rawfilename,
#'      paste(basename(rawfilename), x, "mgf", sep='.'),
#'      pattern=pattern[[x]])})
#' 
mgf <- function(rawfilename,
                mgffilename = paste(sub('\\.raw$', '', rawfilename), "mgf", sep='.'),
                pattern = ".+",
                FUN=NULL){
 
  stopifnot(file.exists(rawfilename))
  metadata <- read.raw(rawfilename)
  
  scannumberMS2 <- metadata$scanNumber[metadata$MSOrder=='Ms2' & grepl(pattern, metadata$ScanType)]
  
  if (length(scannumberMS2) > 0){
    message(paste("extracting", length(scannumberMS2), "MS2 scans ..."))
    S <- readScans(rawfile = rawfilename, scans = scannumberMS2)
    
    if (!is.null(FUN)){
      S <- lapply(S, FUN)
    }
    
    
    message(paste("writting mgf file to", mgffilename, "..."))
    
    .writeMgfHeader(mgffilename)
    rv <- lapply(S, .writeMgfPeaklist, filename=mgffilename)
    
  }else{
    warning("no MS2 scans found.")
  }
  NULL
}


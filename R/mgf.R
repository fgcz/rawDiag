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
  #if (!is.psm(x)){
  #  warning("object is not a psm.")
   # return()
  #}
  stopifnot(file.exists(filename))
  
  FILE <- file(filename, "a")
  writeLines("BEGIN IONS", FILE)
  writeLines(paste("TITLE=",as.character(x$title),sep=''), FILE)
  writeLines(paste("PEPMASS=",as.character(x$pepmass[1]), sep=''), FILE)
  if (charge)
    writeLines(paste("CHARGE=",as.character(x$charge), '+', sep=''), FILE)
  writeLines(paste("SCANS=",as.character(x$scan), sep=''), FILE)
  writeLines(paste("RTINSECONDS=",as.character(x$rtinseconds),sep=''), FILE)
  writeLines(as.character(paste(x$mZ, x$intensity, sep=' ')), FILE)
  writeLines("END IONS", FILE)
  writeLines("", FILE)
  close(FILE)
}

peaklist.mgf <- function(x, filename, append = TRUE){
  #stopifnot(!is.psmSet(x))
  .writeMgfHeader(filename)
  rv <- lapply(x, function(pl){.writeMgfPeaklist(pl, filename)})
}


#' generate a Mascot Generic File (mgf)
#'
#' @param rawfilename a Thermo Fisher instrument file
#' @param mgffilename a Mascot Generic File
#' @param FUN a function applied to each MS2 scan
#'
#' @author Christian Panse <cp@fgcz.ethz.ch>
#' 
#' @return
#' 
#' @export mgf
#' 
#' @examples 
#' (rawfilename <- file.path(path.package(package = 'rawDiag'),
#'   'extdata', 'sample.raw'))
#' mgf(rawfile, tempfile(fileext = '.mgf'))
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
#' mgf(rawfile, tempfile(fileext = '.mgf'), FUN=top5)
#' 
mgf <- function(rawfilename,
                mgffilename = paste(sub('\\.raw$', '', rawfilename), "mgf", sep='.'),
                FUN=NULL){
 
  stopifnot(file.exists(rawfilename))
  metadata <- read.raw(rawfilename)
  scannumberMS2 <- metadata$scanNumber[metadata$MSOrder=='Ms2']
  
  if (length(scannumberMS2)>0){
    message(paste("extracting", length(scannumberMS2), "MS2 scans ..."))
    S <- readScans(rawfile = rawfilename, scans = scannumberMS2)
    if (!is.null(FUN)){
      S <- lapply(S, FUN)
    }
    peaklist.mgf(S, filename = mgffilename)
  }else{warning("no MS2 scans found.")
    return (NULL)}
}

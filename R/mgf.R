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
  writeLines(paste("PEPMASS=",as.character(x$pepmass),sep=''), FILE)
  if (charge)
    writeLines(paste("CHARGE=",as.character(x$charge),'+',sep=''), FILE)
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


mgf <- function(rawfilename, mgffilename = paste(sub('\\.raw$', '', rawfilename), "mgf", sep='.')){
  #awfilename <- "/Users/cp/__projects/2018/20180602--glyco/data/20180613_10_CM_Native_t_0_2.raw"
  stopifnot(file.exists(rawfilename))
  metadata <- read.raw(rawfilename)
  scannumberMS2 <- metadata$scanNumber[metadata$MSOrder=='Ms2']
  message(paste("extracting", length(scannumberMS2, "MS2 scans ...")))
  S <- readScans(rawfile = rawfilename, scans = scannumberMS2)
  peaklist.mgf(S, filename = mgffilename)
}